unit uart;

{$macro on}

interface

procedure uartInit();
procedure uartTransmit(const data: byte);
// Blocking if data is not available
function uartReceive: byte;

implementation

uses
  intrinsics;

{$ifndef BAUD}
  // If a baud rate wasn't defined elsewhere, default to 115200
  {$define BAUD:=115200}
{$endif}

{$ifdef CPUAVR5}
// Calculate UBRR value for U2X mode
{$define UBRR_:= (F_CPU + 4*BAUD) div (8*BAUD) - 1}
procedure uartInit;
begin
  UBRR0 := UBRR_;
  // Set U2X bit
  UCSR0A := UCSR0A or (1 shl U2X0);
  // Enable receiver and transmitter
  UCSR0B := (1 shl RXEN0) or (1 shl TXEN0);
  // Set frame format: 8data, 1stop bit, no parity
  UCSR0C := (3 shl UCSZ0);
end;

procedure uartTransmit(const data: byte);
begin
  // Wait for empty transmit buffer
  while ((UCSR0A and (1 shl UDRE0)) = 0) do;

  // Put data into buffer, sends the data
  UDR0 := data;
end;

function uartReceive: byte;
begin
  // Wait for data to be received
  while ((UCSR0A and (1 shl RXC0)) = 0) do;

  // Get and return received data from buffer
  result := UDR0;
end;
{$else}
const
  RXBufferSize = 2;
  PB0 = 0;
  PB1 = 1;
  PB2 = 2;
  TXPin = PB2;
  RXPin = PB1;

var
  RXBuffer: array[1..RXBufferSize] of byte;
  RXIndex: byte = 0;

procedure uartInit();
begin
  DDRB := DDRB and not(1 shl RXPin);  // Input
  PORTB := PORTB or (1 shl RXPin);    // Pullup
  {$if defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  GIMSK := GIMSK or (1 shl PCIE);     // Enable pin change interrupt
  PCMSK := PCMSK or (1 shl RXPin);    // Pin change mask = RXPin
  {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  GIMSK := GIMSK or (1 shl 5);        // Enable pin change interrupt1 - PCIE1
  PCMSK1 := PCMSK1 or (1 shl RXPin);
  {$endif}
  DDRB := DDRB and not(1 shl TXPin);  // Input, switch to output only when pushing data
  PORTB := PORTB or (1 shl TXPin);    // Pullup
end;

function uartReceive: byte;
var
  i: byte;
begin
  // Wait for a character
  while(RXIndex = 0) do;

  // Halt receive interrupt for atomic access to buffer
  // Could miss data, but at least data in buffer is protected from corruption
  avr_cli();
  // Return the first character in the buffer
  result := RXBuffer[1];
  dec(RXIndex);
  // Move everything down
  for i := 1 to RXIndex do
    RXBuffer[i] := RXBuffer[i+1];
  avr_sei;
end;

function uartDataAvailable(): boolean;
begin
  Result := RXIndex > 0;
end;

const
  // delays required in CPU clock cycles
  RXDelay1_5 = (((3*F_CPU + (BAUD div 2)) div BAUD) + 1) div 2;  // cycles per bit =
  RXDelay    = (F_CPU + (BAUD div 2)) div BAUD;

  // Loop counter values corrected for overhead & instruction count
  // Switch to word if LoopCount1_5 > 255, or RXDelay1_5 > 3*255 + 30 = 795
{$if RXDelay1_5 > 795}
  {$info 'Switching to word size counter'}
  {$define wordSizeCounter = 1}
  LoopCount1_5 = (RXDelay1_5 - 34 + 2) div 4;
  LoopCount = (RXDelay - 7 + 2) div 4;
{$else}
  {$if RXDelay1_5 < 30}
  {$Error 'Baud rate too high for RX'}
  {$endif}
  LoopCount1_5 = (RXDelay1_5 - 32 + 2) div 3;
  LoopCount = (RXDelay - 6 + 2) div 3;
{$endif}

// Force known storage for a byte value, by declaring it a parameter to ISR
{$if defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
procedure PCINT0; interrupt; public name 'PCINT0_ISR';
{$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
procedure PCINT1; interrupt; public name 'PCINT1_ISR';
{$endif}
label
  RX;
var
  c: byte;
begin
  // Make sure it is our pin and it is 0
  if(PINB and (1 shl RXPin) = 0) then
  begin
    // Start the read (assuming we have the start bit)
    asm
      ldi r24, lo8(LoopCount1_5)
      {$ifdef wordSizeCounter}
      ldi r25, hi8(LoopCount1_5)
      {$endif}
      ldi r20, 0x80          // bit shift counter

      RX:
      {$ifdef wordSizeCounter}
      sbiw r24, 1         // 2
      {$else}
      dec r24             // 1
      {$endif}
      brne RX             // 2 for jmp, 1 to exit

      ldi r24, lo8(LoopCount) // 1
      {$ifdef wordSizeCounter}
      ldi r25, hi8(LoopCount)
      {$endif}
      // Tally from pin change up to here if wordSizeCounter:
      //   8 (ISR) + 22 (until asm) + 3 + 4*LoopCount1_5 - 1 + 2 = 34 + 4*LoopCount1_5;
      // else
      //   8 (ISR) + 22 (until asm) + 2 + 3*LoopCount1_5 - 1 + 1 = 32 + 3*LoopCount1_5;

      sbic PINB+(-32),RXPin; // 2 for skip, else 1 Jump next instruction if RXpin is clear
      sec                    // 1 Set carry flag
      ror r20                // 1 Shift rigth r20 and insert carry flah on left side
      brcc RX             // 2 for jmp, else 1 Repeat until the original bit 7 is rotated into carry flag (= 8 bits read)
      // Tally from RX: up to here if wordSizeCounter
      //   4*LoopCount-1 + 7 (-1, but that is when loop is finished)
      // else
      //   3*LoopCount-1 + 6;

      std c, r20
      // Ignore stop bit for now
      //Stop:
      //dec r18             // 1
      //brne Stop
    end ['r20', 'r24', 'r25'];

    // Now put it in the buffer (if we have room)
    if RXIndex < RXBufferSize then
    begin
      inc(RXIndex);
      RXBuffer[RXIndex] := c;
    end;
  end;
end;

const
  TXDelay = (F_CPU + (BAUD div 2)) div BAUD;
  // If TXDelayCount > 255 then use word size counter
  // (TXDelay - 5) div 3 > 255: TXDelay > 770
{$if TXDelay > 770}
  {$info 'Using word size counter for TX delay'}
  {$define wordSizeTXCounter = 1}
  TXDelayCount = (TXDelay - 8 + 2) div 4;
{$else}
  {$if TXDelay < 5}
  {$Error 'Baud rate too high for TX'}
  {$endif}
  TXDelayCount = (TXDelay - 7 + 2) div 3;
{$endif}

procedure uartTransmit(const data: byte); assembler; nostackframe;
label
  TxLoop, TxDelay;
asm
  // r0 - SREG copy
  // r24 - data
  // r22 (& r23) - Delaycount
  // r21 - stop & idle states
  // r20 - copy of PORTB state
  // Prologue
  in r0, 0x3f
  cli
  // Main code
  sbi DDRB+(-32), TXPin      // Output
  cbi PORTB+(-32), TXPin     // start bit = low
  in r20, PORTB+(-32)        // store state of PORTB
  ldi r21, 3                 // stop bit + idle state
  TxLoop:
  ldi r22, lo8(TXDelayCount)
  {$ifdef wordSizeTXCounter}
  ldi r23, hi8(TXDelayCount)
  {$endif}
  TxDelay:
  {$ifdef wordSizeTXCounter}
  sbiw r22, 1
  {$else}
  dec r22
  {$endif}
  brne TxDelay
  bst r24, 0                  // Store bit 0 in T flag
  bld r20, TXPin              // Load bit from T flag to register
  lsr r21                     // shift stop & idle bits right into carry flag
  ror r24                     // shift stop & idle bits from carry flag, zero flag set of all bits are zero after shift
  // Cycle from after 1st out to after next out below:
  // 8 + 3*TxDelay - 1
  // Word: 9 + 4*TXDelayCount-1
  // Cycles from out to out
  // 8 + 3*TXDelayCount - 1
  // Word: 9 + 4*TXDelayCount-1;
  out PORTB+(-32), r20
  brne TxLoop                 // loop until r24 is zero
  cbi DDRB+(-32), TXPin       // Input
  sbi PORTB+(-32), TXPin       // Pullup
  // Epilogue
  out 0x3f, r0
end;
{$endif}

end.

