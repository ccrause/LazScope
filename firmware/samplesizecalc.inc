{ Total memory required for global variables:
  Databuf      : 3*(samples div 2) + 2*(samples and 1) + 4 + 1
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
  AVR5: 2 + 14 bytes to call gatherData = 16 bytes
  tinyX4/X5: 2 + 14 bytes to call gatherData + 2 + 16 bytes PCINT1 = 34 bytes
}

  // On atmega328p DIP and attinyx4 only ADC0-5 are available
  // On attinyx5 ADC0 is reset and ADC1 is square wave signal, so ADC2-3 are available
  MaxADCChannels = {$if defined(CPUAVR5) or defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
                   5{$else}2{$endif};

  {$if defined(CPUAVR5)}
  samples = FPC_SRAMSIZE - 19 - MaxADCChannels - 16;
  {$elseif defined(FPC_MCU_ATTINY25) or defined(FPC_MCU_ATTINY45) or defined(FPC_MCU_ATTINY85)}
  samples = FPC_SRAMSIZE - 20 - MaxADCChannels - 3 - 34;
  {$elseif defined(FPC_MCU_ATTINY24) or defined(FPC_MCU_ATTINY44) or defined(FPC_MCU_ATTINY84)}
  samples = FPC_SRAMSIZE - 19 - MaxADCChannels - 3 - 34;
  {$else}
    {$error 'Unsupported controller specified, no predefined sample size calculated!'}
  {$endif}

