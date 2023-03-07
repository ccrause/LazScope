{ Total memory required for global variables:
  Databuf      : 3*(samples div 2) + 2*(samples and 1) + 2 + 4 + 1
  time         : 4
  ADMUXhiMask  : 1
  triggerCheck : 2
  triggerlevel : 2
  rollovercount: 2
  triggerInit  : 2
  ADMUXVector  : MaxADCChannels
  numChannels  : 1
  timerOverflow: 1  Only for 8 bit timers on tinyX5
  uart         : RX buffer + index = 3 (tinyX4/X5 only)
  Total        = 19 (20 for tinyX5) + 3*(samples div 2) + 2*(samples and 1) + MaxADCChannels + 3 (tinyX4/X5)

  Stack requirements
  AVR5: 2 + 16 bytes to call gatherData = 18 bytes
  tinyX4/X5: 2 + 14 bytes to call gatherData + 2 + 16 bytes PCINT1 = 34 bytes
}

  // On atmega328p DIP and attinyx4 only ADC0-5 are available
  // On attinyx5 ADC0 is reset and ADC1 is square wave signal, so ADC2-3 are available
  MaxADCChannels = {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
                   5{$else}2{$endif};

  // Global variables excluding data buffer
  {$if defined(CPUAVR5)}
  GlobalVariables = 19;
  {$elseif defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  GlobalVariables = 20;
  {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  GlobalVariables = 19;
  {$else}
    {$error 'Unsupported controller specified, no predefined sample size calculated!'}
  {$endif}

  // Space required for stack
  {$if defined(CPUAVR5)}
  MaxStackSize = 18;
  {$elseif defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  MaxStackSize = 34;
  {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  MaxStackSize = 34;
  {$endif}

  // Space left for data buffer
  BufferSize8bit = FPC_SRAMSIZE - GlobalVariables - MaxStackSize;

  // Note: 7 bytes at end reserved for information
  NumSamples8bit = BufferSize8bit - 7;

// Number of 10 bit samples to fit into buffer
  // 3 bytes per 2x 10 bit samples, plus 1 sample if 2 bytes are left
  NumSamples10bit = (NumSamples8bit div 3) * 2 + ((NumSamples8bit mod 3) shr 1);
  // Actual 10 bit buffer size
  BufferSize10bit = (NumSamples10bit div 2)*3 + (NumSamples10bit and 1)*2 + 7;

