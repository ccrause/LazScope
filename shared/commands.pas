unit commands;

{$mode objfpc}{$H+}

interface
{$ifndef CPUAVR}
uses
  Lmessages;
{$endif not CPUAVR}

const
  {$ifndef CPUAVR}
  // Main thread should pull data
  WM_DataWaiting = LM_User + 101;
  {$endif not CPUAVR}

  // Main thread waiting
  cmdWaiting: byte     = 0;

  // ADC pins - bitpacked as byte
  cmdADCPins           = 10;

  // Select frame width option
  cmdADCDiv2           = 20;
  cmdADCDiv4           = 21;
  cmdADCDiv8           = 22;
  cmdADCDiv16          = 23;
  cmdADCDiv32          = 24;
  cmdADCDiv64          = 25;
  cmdADCDiv128         = 26;

  // Select ADC reference voltage
  cmdADCVoltage_VCC    = 32;
  cmdADCVoltage_1_1    = 33;
  cmdADCVoltage_AREF   = 34;

  // Specific command that should be echoed from controller
  cmdEcho              = 85;

  // Request data frame from Arduino
  cmdSendData          = 115; // 's' = start

  // Request number of samples per buffer transfer
  cmdSampleCount       = 127;

  // Trigger commands
  cmdTriggerOff        = 251;
  // Next 2 commands followed by trigger level (0 ... 1020)
  // 2nd value transmitted = trigger level div 4, to fit in 8 bits
  cmdTriggerRising     = 252;
  cmdTriggerFalling    = 253;

  // Specify selected ADC ports to sample
  // Command should be followed by bitmap of ports
  cmdSelectPorts       = 254;

  // Data descriptors offsets in data block
  dataSettingsOffset  = 0;
  dataChannelsOffset  = 1;
  dataOffset          = 2;
  // Masks to indicate specific configuration bits
  tenBitFlagMask      = 1 shl 7;
  triggerMask         = 1 shl 6;

implementation

end.

