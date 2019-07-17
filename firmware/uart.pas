unit uart;

interface

{$ifdef CPUAVR5}
procedure uartInit(const UBRR: word);
{$else}
procedure uartInit();
{$endif}

procedure uartTransmit(const data: byte);
// Blocking if data is not available
function uartReceive: byte;

implementation

uses
  intrinsics;

{$ifdef CPUAVR5}
procedure uartInit(const UBRR: word);
begin
  UBRR0H := UBRR shr 8;
  UBRR0L := byte(UBRR);

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
  BAUDRATE = 115200;
  RXBufferSize = 4;
  PB0 = 0;  // RX
  PB1 = 1;  // TX
  TXPin = PB1;
  RXPin = PB0;

var
  RXBuffer: array[1..RXBufferSize] of byte;
  RXIndex: byte = 0;

procedure uartInit();
begin
  DDRB := DDRB and not(1 shl RXPin);  // Input
  PORTB := PORTB or (1 shl RXPin);    // Pullup
  GIMSK := GIMSK or (1 shl PCIE);     // Enable pin change interrupt
  PCMSK := PCMSK or (1 shl RXPin);    // Pin change mask = RXPin
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
  RXDelay1_5 = (((3*F_CPU + (BAUDRATE div 2)) div BAUDRATE) + 1) div 2;  // cycles per bit =
  RXDelay    = (F_CPU + (BAUDRATE div 2)) div BAUDRATE;

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
procedure PCINT0(b: byte); interrupt; public name 'PCINT0_ISR';
label
  RX;
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

      std b, r20
      // Ignore stop bit for now
      //Stop:
      //dec r18             // 1
      //brne Stop
    end ['r20', 'r24', 'r25'];

    // Now put it in the buffer (if we have room)
    if RXIndex < RXBufferSize then
    begin
      inc(RXIndex);
      RXBuffer[RXIndex] := b;
    end;
  end;
end;

const
  TXDelay = (F_CPU + (BAUDRATE div 2)) div BAUDRATE;
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

procedure uartTransmit(const data: byte);
label
  TxLoop, TxDelay;
begin
  // Switch on ouput - high
  DDRB  := DDRB or (1 shl TXPin);
  PORTB := PORTB or (1 shl TXPin);
  avr_cli;
  asm
    // r24 - data
    // r22 (& r23) - Delaycount
    // r21 - stop & idle states
    // r20 - copy of PORTB state

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

    // Prevent high nibble of SREG to read as $E
    // This ends up in r0 because FPC uses r0 to manipulate SREG
    // When gdb requests target registers (rsp command g), the content of r0 is the first value as response
    // If this response start with E, gdb interprets it as an error
    // Clear T flag - prevents SREG from reading $Ex
    bst r0, 0
  end;
  avr_sei;
  DDRB := DDRB and not(1 shl TXPin);  // Input, switch to output only when pushing data
  PORTB := PORTB or (1 shl TXPin);    // Pullup
end;

{$endif}

end.

