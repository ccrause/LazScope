program sampler;

uses
  intrinsics, commands, uart;

const
  {$include samplesizecalc.inc}

  // ADC ADMUX constants mask - different for atmega/attiny
  ADCVoltageVcc  = {$ifdef CPUAVR5}4{$else}0{$endif} shl 4;
  ADCVoltage1_1  = {$ifdef CPUAVR5}12{$else}8{$endif} shl 4;
  ADCVoltageARev = {$ifdef CPUAVR5}0{$else}4{$endif} shl 4;

  // ADLAR bit in ADCMUX hi nibble
  ADCleftAdjust = {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}2 shl 4
                  {$else}0{$endif};

type
  TTriggerFunc = function(const value1, value2: uint16): boolean;  // pointer to trigger check function
  TDataBuf = array[0..BufferSize-1] of byte;

  TWordAsBytes = packed record
    l, h: byte;
  end;

var
  time: uint32; // Duration of a data frame in 16 microsecond ticks
  ADMUXhiMask: uint8;  // hi nibble of the ADMUX register

  triggerCheck: TTriggerFunc;  // pointer to trigger function
  trigger: byte = cmdTriggerOff;
  triggerlevel: uint16 = 512;  // Trigger threshold

  ADMUXVector: array[0..MaxADCChannels-1] of byte;
  numChannels: uint8;

  {$if defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  // Attiny x5 only have 8 bit timers, so count overflows to extend timer measurement range
  timerOverflow: byte;
  {$endif}

  // Keep at end of global data, serves as a bit of protection agains stack/global data overlap
  databuf: TDataBuf;

// Returns next selected ADC channel, wraps around
procedure nextPortChannel(var nextID: byte); inline;
begin
  inc(nextID);
  if (nextID >= numChannels) then
    nextID := 0;
end;

// v1 is new value, v2 is old value
function checkTriggerRising(const value1, value2: uint16): boolean;
begin
  Result := (triggerlevel <= value1) and (triggerlevel > value2);
end;

// v1 is new value, v2 is old value
function checkTriggerFalling(const value1, value2: uint16): boolean;
begin
  Result := (value1 <= triggerlevel) and (value2 > triggerlevel);
end;

{$if defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
// 2 byte stack (excl. PC)
// 22 bytes code
procedure Timer1Overflow; interrupt; public name 'TIMER1_OVF_ISR'; assembler; nostackframe;
asm
  push r0
  in   r0, 0x3f
  push r0
  lds  r0, timerOverflow;
  inc  r0
  sts  timerOverflow, r0
  pop r0
  out 0x3f, r0
  pop r0
end;

// 8 bytes stack (excl. PC)
// 58 bytes code
//procedure Timer1Overflow; interrupt; public name 'TIMER1_OVF_ISR';
//begin
//  inc(timerOverflow);
//end;
{$endif}

procedure startTimer; inline;
begin
{$if defined(CPUAVR5)}
  TCCR1A := 0;  // Default state anyway, just set to make sure
  TCCR1B := (1 shl 2{CS12}); // 256 prescaler, TCNT1 can run up to ~ 65535 with a freq of 16000000/256 = 625000 Hz, will overflow after ~ 1.048 seconds
{$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  TCCR1A := 0;  // Default state anyway, just set to make sure
  TCCR1B := (3 shl 0); // 64 prescaler, TCNT1 can run up to ~ 65535 with a freq of 8000000/64 = 125000 Hz, will overflow after ~ 0.524 seconds
{$else}
  timerOverflow := 0;
  TCCR1 := (1 shl 3{CS13}); // 128 prescaler, TCNT1 can run up to 255 with a freq of 8000000/128 = 625000 Hz. Data width = 255 / 62500 = 4.08 ms
  TIMSK := 1 shl TOIE1;
{$endif}
  TCNT1 := 0;  // reset counter
end;

function stopGetTimeMicros: uint32; inline;
begin
{$if defined(CPUAVR5)}
  TCCR1B := 0;
  result := uint32(TCNT1) shl 4;  // 256 / F_CPU = 0.000 016 s per tick, or 16 microseconds also 128 / 8 MHz
{$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  TCCR1B := 0;
  result := uint32(TCNT1) shl 3;  // 64 / F_CPU = 0.000 008 s per tick, or 8 microseconds
{$else}
  TCCR1 := 0;
  result := (uint32(TCNT1) + uint32(timerOverflow) shl 8) shl 4;
{$endif}
end;

// Read ADC and pack data buffer
{$if FPC_FLASHSIZE > 2048}
procedure gatherData;
const
  timeout1 = 4*F_CPU div 13;
var
  nextChannelIndex: byte;
  i, j, timeoutcount: word;
  // Keep these variables at end of stack
  // to serve as a bit of protection against stack/global data smashing
  lowbyte, hibyte: byte;
  v1, v2: word;
begin
  // Aim for a timeout of ~ 4 seconds or at most 65535 counts
  timeoutcount := word(uint32(timeout1) shr 16) shr (ADCSRA and $07);
  if timeoutcount > 0 then
    timeoutcount := $FFFE
  else
    timeoutcount := uint32(timeout1) shr (ADCSRA and $07);

  if trigger = cmdTriggerFalling then
    v1 := 0
  else
    v1 := 1023;

  i := 0;
  nextChannelIndex := 0;
  ADMUX := ADMUXVector[0];
  startTimer();

  repeat
    ADCSRA := ADCSRA or (1 shl ADSC);
    TCNT1 := 0;
    v2 := v1;
    while ((ADCSRA and (1 shl ADSC)) > 0) do;
    lowbyte := ADCL;
    hibyte := ADCH;
    TWordAsBytes(v1).h := hibyte;
    TWordAsBytes(v1).l := lowbyte;

    if (trigger = cmdTriggerOff) then
    begin
      TCNT1 := 0;
      Break;
    end
    else if triggerCheck(v1, v2) or (i >= timeoutcount) then
    begin
      databuf[dataOffset] := (TWordAsBytes(v2).h shl 6) or (TWordAsBytes(v2).l shr 2);
      databuf[dataOffset+1] := (TWordAsBytes(v2).l and 3) shl 6;
      i := 1;
      Break;
    end;

    inc(i);
  until false;

  // Start with normal reading
  nextPortChannel(nextChannelIndex);
  while i < NumSamples10bit-1 do
  begin
    ADMUX := ADMUXVector[nextChannelIndex];
    ADCSRA := ADCSRA or (1 shl ADSC); // start the conversion

    nextPortChannel(nextChannelIndex);
    j := i + (i div 2) + dataOffset;
    if (i and 1) = 0 then
    begin
      databuf[j] := (hibyte shl 6) or (lowbyte shr 2);
      databuf[j+1] := (lowbyte and 3) shl 6;
    end
    else
    begin
      databuf[j] := databuf[j] or hibyte;
      databuf[j+1] := lowbyte;
    end;

    inc(i);
    while ((ADCSRA and (1 shl ADSC)) > 0) do; // ADSC is cleared when the conversion finishes
    lowbyte := ADCL;
    hibyte := ADCH;
  end;

  time := stopGetTimeMicros();
  j := i + (i div 2) + dataOffset;
  if (i and 1) = 0 then
  begin
    databuf[j] := (hibyte shl 6) or (lowbyte shr 2);
    databuf[j+1] := (lowbyte and 3) shl 6;
  end
  else
  begin
    databuf[j] := databuf[j] or hibyte;
    databuf[j+1] := lowbyte;
  end;
end;
{$endif}

// Read ADC and pack data buffer
procedure gatherData8;
const
  timeout1 = 4*F_CPU div 13;
var
  nextChannelIndex: byte;
  i, timeoutcount: word;
  // Keep these variables at end of stack
  // to serve as a bit of protection against stack/global data smashing
  v1, v2: byte;
begin
  // Aim for a timeout of ~ 4 seconds or at most 65535 counts
  timeoutcount := word(uint32(timeout1) shr 16) shr (ADCSRA and $07);
  if timeoutcount > 0 then
    timeoutcount := $FFFE
  else
    timeoutcount := uint32(timeout1) shr (ADCSRA and $07);

  if trigger = cmdTriggerFalling then
    v1 := 0
  else
    v1 := 255;

  i := dataOffset;
  nextChannelIndex := 0;
  ADMUX := ADMUXVector[0];
  startTimer();

  repeat
    ADCSRA := ADCSRA or (1 shl ADSC);
    TCNT1 := 0;
    v2 := v1;
    while ((ADCSRA and (1 shl ADSC)) > 0) do;
    v1 := ADCH;

    if trigger = cmdTriggerOff then
    begin
      TCNT1 := 0;
      Break;
    end
    else if triggerCheck(v1, v2) or (i >= timeoutcount) then
    begin
      databuf[dataOffset] := v2;
      i := dataOffset + 1;
      Break;
    end;

    inc(i);
  until false;

  // Start with normal reading
  nextPortChannel(nextChannelIndex);
  while i < NumSamples8bit+1 do
  begin
    ADMUX := ADMUXVector[nextChannelIndex];
    ADCSRA := ADCSRA or (1 shl ADSC); // start the conversion
    nextPortChannel(nextChannelIndex);
    databuf[i] := v1;
    inc(i);
    while ((ADCSRA and (1 shl ADSC)) > 0) do; // ADSC is cleared when the conversion finishes
    v1 := ADCH;
  end;

  time := stopGetTimeMicros();
  databuf[i] := v1;
end;

// Call this to update MUX with new voltage reference
// or if ADC channel selection changed
procedure updateADMUXVector();
var
  i: byte;
begin
  for i := 0 to numChannels-1 do
    ADMUXVector[i] := ADMUXhiMask or (ADMUXVector[i] and $0F);
end;

procedure uartWriteBuffer(constref data: TDataBuf);
var
  i: uint16;
begin
  for i := 0 to BufferSize-1 do
    uartTransmit(data[i]);
end;

procedure init();  inline;
begin
  uartInit();   // Baud rate can be specified with a define, else it defaults to 115200
  avr_sei;

  ADCSRA := $86;
  ADMUXhiMask := ADCVoltageVcc; // Vcc + left adjust

  // Set up A0 as default selected channels
  ADMUXVector[0] := {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
                 0 {$else} 2 {$endif};  // on attiny ADC0 is on reset pin, so generally not used
  numChannels := 1;
  updateADMUXVector; // splice together ADMUXhiMask and channels

  // Disable digital input on all input analog pins to save power
  DIDR0 := {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
           63{$else}(1 shl ADC2D) or (1 shl ADC3D){$endif};
  //triggerCheck := nil;

  // Setup timer2 PWM on PD3 / D3 @ 0.5 kHz
  {$if defined(CPUAVR5) }
  DDRD := (1 shl 3);
  TCCR2A := (1 shl 4) or (1 shl 1);  // Toggle OC2B & timer mode 2 (CTC)
  TCCR2B := 5;   // prescaler = 128
  OCR2A := 124;  // 16000000 / 128 / 125 = 1 kHz
  {$elseif defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  DDRB := DDRB or (1 shl 0);        // PB0 - pin 5
  TCCR0A := (1 shl 6) or (1 shl 1); // toggle OC0A & CTC mode
  TCCR0B := (1 shl 1) or (1 shl 0); // prescaler = 64
  OCR0A := 124; // 8000000 / 64 / 125 = 1 kHz
  {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  DDRA := DDRA or (1 shl 7);        // PA7 - pin 6
  TCCR0A := (1 shl 4) or (1 shl 1); // toggle OC0B & CTC mode
  TCCR0B := (1 shl 1) or (1 shl 0); // prescaler = 64
  OCR0A := 124; // 8000000 / 64 / 125 = 1 kHz
  {$endif}
end;

procedure readSerialCmds; inline;
var
  cmd: byte;
  b: byte;
  i: uint16;
begin
  cmd := uartReceive();

  // Note: all non-data request commands should be echoed
  // This is handled by transmitting the cmd variable at the end of this procedure
  case cmd of
    // ADC pins (PC0..PC5 for atmega328p, PB1..PB3 for attiny}
    cmdADCPins: cmd := {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
                       %00111111{$else}%00001100{$endif};
    // Read the selected ports for ADC
    cmdSelectPorts:
      begin
        uartTransmit(cmd);
        cmd := uartReceive();
        numChannels := 0;
        for b := 0 to 7 do
        begin
          if ((cmd and (1 shl b)) > 0) and (numChannels < MaxADCChannels) then
          begin
            ADMUXVector[numChannels] := ADMUXhiMask or b;
            inc(numChannels);
          end;
        end;
      end;

    // ADC prescaler selection
    cmdADCDiv2  : ADCSRA := $81;
    cmdADCDiv4  : ADCSRA := $82;
    cmdADCDiv8  : ADCSRA := $83;
    cmdADCDiv16 : ADCSRA := $84;
    cmdADCDiv32 : ADCSRA := $85;
    cmdADCDiv64 : ADCSRA := $86;
    cmdADCDiv128: ADCSRA := $87;

   // Reference voltage
    cmdADCVoltage_VCC:
      begin
        ADMUXhiMask := ADCVoltageVcc;  // Vcc
        updateADMUXVector();
      end;
    cmdADCVoltage_1_1:
      begin
        ADMUXhiMask := ADCVoltage1_1;  // 1.1 V
        updateADMUXVector();
      end;
    cmdADCVoltage_AREF:
      begin
        ADMUXhiMask := ADCVoltageARev;// Aref pin
        updateADMUXVector();
      end;

    // Return number of samples in buffer
    cmdBufferSize:
      begin
        uartTransmit(byte(BufferSize and $FF));  // LSB
        cmd := (BufferSize shr 8);
      end;

    cmdSendData:
      begin
        // Voltage reference: 0 = external (Vcc, Aref etc.), 1 = 1.1 V, 2 = 2.56 V
        Databuf[0] := 0;
        {$if defined(CPUAVR5)}
        // The bitmasked comparison triggers an optimization bug,
        // see https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/386
        // Workarond for FPC 3.2.2 and lower
        {$if FPC_FULLVERSION <= 30202}
        b := ADMUXhiMask and $C0;
        if b = $C0 then
        {$else}
        if (ADMUXhiMask and $C0) = $C0 then
        {$endif}
          Databuf[0] := 1;
        {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
        if (ADMUXhiMask and $C0) = $80 then
          Databuf[0] := 1;
        {$elseif defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
        case (ADMUXhiMask and $D0) of
          $80: Databuf[0] := 1;
          $90, $D0: Databuf[0] := 2;
        end;
        {$endif}

        // Trigger?
        if trigger <> cmdTriggerOff then
          Databuf[0] := Databuf[0] or triggerMask;

        // Active channels
        Databuf[1] := 0;
        for b := 0 to numChannels-1 do
        begin
          i := (ADMUXVector[b] and $0F);
          Databuf[1] := Databuf[1] or byte(1 shl byte(i));
        end;

        {$if FPC_FLASHSIZE > 2048}
        // If ADLAR bit is cleared, then in 10 bit data mode
        {$if defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
        if ADCSRB = 0 then
        {$else}
        if (ADMUXhiMask and ADCleftAdjust) = 0 then
        {$endif}
        begin
          Databuf[0] := Databuf[0] or tenBitFlagMask;
          gatherData();
        end
        else
        {$endif}
          gatherData8();

        databuf[BufferSize - 5] := time shr 24;
        databuf[BufferSize - 4] := (time and $00FF0000) shr 16;
        databuf[BufferSize - 3] := (time and $0000FF00) shr 8;
        databuf[BufferSize - 2] := time and $000000FF;

        // XOR data checksum
        b := 0;
        for i := 0 to BufferSize-2 do
          b := b xor databuf[i];
        databuf[BufferSize-1] := b;

        uartWriteBuffer(databuf);
        exit; // Do not echo cmd
      end;

    // Set trigger options
    cmdTriggerOff:
      begin
        trigger := cmdTriggerOff;
        triggerCheck := nil;
      end;

    cmdTriggerRising:
      begin
        uartTransmit(cmd);
        trigger := cmdTriggerRising;
        triggerCheck := @checkTriggerRising;
        cmd := uartReceive();
        triggerlevel := word(cmd) shl 2;
      end;

    cmdTriggerFalling:
      begin
        uartTransmit(cmd);
        trigger := cmdTriggerFalling;
        triggerCheck := @checkTriggerFalling;
        cmd := uartReceive();
        triggerlevel := word(cmd) shl 2;
      end;

    cmdListResolutions:
      begin
        {$if FPC_FLASHSIZE < 4096}
        cmd := 1;
        {$else}
        cmd := 3;
        {$endif}
      end;

    cmdSet8bit:
      begin
        {$if defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
        ADCSRB := 1 shl ADLAR;
        {$else}
        ADMUXhiMask := ADMUXhiMask or ADCleftAdjust;
        updateADMUXVector();
        {$endif}
      end;

    cmdSet10bit:
      begin
        {$if defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
        ADCSRB := 0;
        {$else}
        ADMUXhiMask := ADMUXhiMask and ($FF - ADCleftAdjust);
        updateADMUXVector();
        {$endif}
      end;
  end;

  // Echo command back to show it is completed
  uartTransmit(cmd);
end;

begin
  init();
  while(true) do
    readSerialCmds();
end.

