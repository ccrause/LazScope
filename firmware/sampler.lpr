program sampler;

uses
  intrinsics, commands, uart;

const
  {$ifdef CPUAVR5}
  samples = 1200; // careful, large values will clash with stack!
  {$else}
  samples = 120;
  {$endif}
  // 3 bytes per 2 samples + 2 bytes per odd sample
  BUFSIZE = 3*(samples div 2) + 2*(samples and 1) + 4 + 1;

  // ADC ADMUX constants - different for atmega/attiny
  ADCVoltageVcc = {$ifdef CPUAVR5}4{$else}0{$endif};
  ADCVoltage1_1 = {$ifdef CPUAVR5}12{$else}8{$endif};
  ADCVoltageARev = {$ifdef CPUAVR5}0{$else}4{$endif};

type
  TTriggerFunc = function(const value1, value2: uint16): boolean;  // pointer to trigger check function
  TDataBuf = array[0..BUFSIZE-1] of byte;

  TWordAsBytes = packed record
    l, h: byte;
  end;

var
  databuf: TDataBuf; //array[0..BUFSIZE-1] of byte;  // x samples [word/sample], timedelta in _us [dword], checksum [uint8_t]
  time: uint32; // Duration of a data frame in 16 microsend ticks
  ADMUXhi: uint8;  // hi nibble of the ADMUX register

  triggerCheck: TTriggerFunc;  // pointer to trigger function
  triggerlevel: uint16 = 512;  // Trigger threshold
  rollovercount: uint16 = samples; // continue if trigger didn't fire after so many counts
  triggerInit: word;  // value used to ensure first trigger check is untrue, eliminates a boolean check

  channels: array[0..7] of byte;
  ADMUXVector: array[0..7] of byte;
  numChannels: uint8;

  {$ifndef CPUAVR5}
  timerOverflow: byte;
  {$endif}

// Returns next selected ADC channel, wraps around
procedure nextPortChannel(var nextID: byte); inline;
begin
  inc(nextID);
  if ((numChannels - nextID) = 0) then
    nextID := 0;
end;

// v1 is new value, v2 is old value
function checkTriggerRising(const value1, value2: uint16): boolean;
begin
  Result := (triggerlevel <= value1 {>= triggerlevel}) and (triggerlevel > value2 {< triggerlevel});
end;

// v1 is new value, v2 is old value
function checkTriggerFalling(const value1, value2: uint16): boolean;
begin
  Result := (value1 <= triggerlevel) and (value2 > triggerlevel);
end;

{$ifndef CPUAVR5}
procedure Timer1Overflow; interrupt; public name 'TIMER1_OVF_ISR';
begin
  inc(timerOverflow);
end;
{$endif}

procedure startTimer;
begin
{$ifdef CPUAVR5}
  TCCR1A := 0;  // Default state anyway, just set to make sure
  TCCR1B := (1 shl 2{CS12}); // 256 prescaler, TCNT1 can run up to ~ 65535 with a freq of 16000000/256 = 625000 Hz, will overflow after ~ 1.048 seconds
{$else}
  timerOverflow := 0;
  TCCR1 := (1 shl 3{CS13}); // 128 prescaler, TCNT1 can run up to 255 with a freq of 8000000/128 = 625000 Hz. Data width = 255 / 62500 = 4.08 ms
  TIMSK := 1 shl TOIE1;
{$endif}
  TCNT1 := 0;  // reset counter
end;

function stopGetTimeMicros: uint32;
begin
{$ifdef CPUAVR5}
  TCCR1B := 0;
  result := uint32(TCNT1) shl 4;  // 256 / F_CPU = 0.000 016 s per tick, or 16 microseconds also 128 / 8 MHz
{$else}
  TCCR1 := 0;
  result := (uint32(TCNT1) + uint32(timerOverflow) shl 8) shl 4;
{$endif}
end;

// Read ADC and pack data buffer
procedure gatherData;
var
  nextChannelIndex: byte = 0;
  lowbyte, hibyte: byte;
  v1, v2, i, j: word;
begin
  v1 := triggerInit;
  i := 0;
  repeat
    ADMUX := ADMUXVector[nextChannelIndex];
    ADCSRA := ADCSRA or (1 shl ADSC); // $40;
    inc(i);
    v2 := v1;
    while ((ADCSRA and (1 shl ADSC)) > 0) do; // ADSC is cleared when the conversion finishes
    lowbyte := ADCL;
    hibyte := ADCH;
    //TWordAsBytes(v1).h := hibyte;
    //TWordAsBytes(v1).l := lowbyte;
    v1 := (word(hibyte) shl 8) or lowbyte;   // NOTE - typecast required
    if (triggerCheck = nil) or triggerCheck(v1, v2) then
      Break;
  until (i > rollovercount);

  // Start with normal reading
  nextPortChannel(nextChannelIndex);
  startTimer();
  i := 0;
  while i < samples-1 do
  begin
    ADMUX := ADMUXVector[nextChannelIndex];
    ADCSRA := ADCSRA or (1 shl ADSC); // start the conversion

    nextPortChannel(nextChannelIndex);
    j := i + (i div 2);
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
  j := i + (i div 2);
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

// Call this to update MUX with new voltage reference
// or if ADC channel selection changed
procedure updateADMUXVector();
var
  i: byte;
begin
  for i := 0 to numChannels-1 do
    ADMUXVector[i] := (ADMUXhi shl 4) + channels[i];
end;

procedure uartWriteBuffer(constref data: TDataBuf);
var
  i: uint16;
begin
  for i := 0 to BUFSIZE-1 do
    uartTransmit(data[i]);
end;

procedure init();
begin
  //DDRB := 1 shl 5;  // pin 13 set to output
  //PORTB := 0;  // pin 13 low

  //uart_init1(500000, true);
  {$ifdef CPUAVR5}
  uartInit(3);  // baud rate of 500000 @ 16 MHz
  {$else}
  uartInit();   // Fixed baud rate, see BAUDRATE const in uart.pas
  {$endif}
  avr_sei;

  ADCSRA := $86;
  ADMUXhi := ADCVoltageVcc; // Vcc + left adjust

  // Set up A0 as default selected channels
  channels[0] := {$ifdef CPUAVR5}0{$else}1{$endif};  // on attiny ADC0 is on reset pin, so generally not used
  numChannels := 1;
  updateADMUXVector; // splice together ADMUXhi and channels

  // Disable digital input on all input analog pins to save power
  DIDR0 := {$ifdef CPUAVR5}63{$else}(1 shl ADC1D) or (1 shl ADC2D) or (1 shl ADC3D){$endif};
  triggerCheck := nil;

  // Setup timer2 PWM on PD3 / D3 @ 0.5 kHz
  {$ifdef CPUAVR5}
  DDRD := (1 shl 3);
  TCCR2A := (1 shl 4) or (1 shl 1);  // Toggle OC2B & timer mode 2 (CTC)
  TCCR2B := 5;   // prescaler = 128
  OCR2A := 124;  // 16000000 / 128 / 125 = 1 kHz
  {$endif}
end;

// Mostly handle ADC options
procedure SetADCOptions(cmd: byte);
var
  i: byte;
begin
  case cmd of
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
        ADMUXhi := ADCVoltageVcc;  // Vcc
        updateADMUXVector();
      end;
    cmdADCVoltage_1_1:
      begin
        ADMUXhi := ADCVoltage1_1;  // 1.1 V
        updateADMUXVector();
      end;
    cmdADCVoltage_AREF:
      begin
        ADMUXhi := ADCVoltageARev;// Aref pin
        updateADMUXVector();
      end;

    // Return number of samples in buffer
    cmdSampleCount:
      begin
        uartTransmit(byte(samples and $FF));  // LSB
        cmd := (samples shr 8);
      end;

    // Set trigger options
    cmdTriggerOff: triggerCheck := nil;

    cmdTriggerRising:
      begin
        uartTransmit(cmd);  // Trigger on rising edge
        triggerCheck := @checkTriggerRising;
        cmd := uartReceive();  // trigger value divided by 4
        triggerlevel := word(cmd) shl 2;
        triggerInit := 1023;
      end;

    cmdTriggerFalling:
      begin
        uartTransmit(cmd);  // Trigger of falling edge
        triggerCheck := @checkTriggerFalling;
        cmd := uartReceive();  // trigger value divided by 4
        triggerlevel := word(cmd) shl 2;
        triggerInit := 0;
      end;

    // Read the selected ports for ADC
    cmdSelectPorts:
      begin
        uartTransmit(cmd); // All non-data request commands should be echoed
        cmd := uartReceive();
        numChannels := 0;
        for i := 0 to 7 do
        begin
          ADMUXVector[i] := ADMUXhi shl 4;
          if (cmd and (1 shl i)) > 0 then
          begin
            inc(numChannels);
            channels[numChannels-1] := i;
            ADMUXVector[numChannels-1] := ADMUXVector[numChannels-1] or channels[numChannels-1];
          end;
        end;
      end;
  end;
  // Echo command back to show it is completed
  uartTransmit(cmd);
end;


procedure readSerialCmds;
var
  c: byte;
  checksum: byte = 0;
  i: uint16;
begin
  c := uartReceive();

  if (c = cmdSendData) then
  begin
    gatherData();
    databuf[BUFSIZE - 5] := time shr 24;
    databuf[BUFSIZE - 4] := (time and $00FF0000) shr 16;
    databuf[BUFSIZE - 3] := (time and $0000FF00) shr 8;
    databuf[BUFSIZE - 2] := time and $000000FF;

    checksum := 0;
    for i := 0 to BUFSIZE-2 do
      checksum := checksum xor databuf[i];

    databuf[BUFSIZE-1] := checksum;

    uartWriteBuffer(databuf);
  end
  else // Set ADC division factor
    SetADCOptions(c);
end;

begin
  init();
  while(true) do
    readSerialCmds();
end.

