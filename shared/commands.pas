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

  // Request ADC pins, return pins as bitpacked byte
  cmdADCPins           = $10;
  // Specify selected ADC ports to sample
  // Command should be followed by bitmap of ports
  cmdSelectPorts       = $11;

  // Select ADC prescaler option
  cmdADCDiv2           = $20; //20;
  cmdADCDiv4           = $21; //21;
  cmdADCDiv8           = $22; //22;
  cmdADCDiv16          = $23; //23;
  cmdADCDiv32          = $24; //24;
  cmdADCDiv64          = $25; //25;
  cmdADCDiv128         = $26; //26;

  // Select ADC reference voltage
  cmdADCVoltage_VCC    = $30; //32;
  cmdADCVoltage_1_1    = $31; //33;
  cmdADCVoltage_AREF   = $32; //34;

  // Request size of data buffer
  cmdBufferSize        = $62; // 'b' = buffer size;
  // Specific command that should be echoed from controller
  cmdEcho              = $65; // 'e' = echo
  // Request data frame from Arduino
  cmdSendData          = $73; // 's' = start

  // Trigger commands
  cmdTriggerOff        = $80;
  // Next 2 commands set trigger type
  // Followed by trigger level (0 ... 255), for 10 bit data this gets multiplied by 4
  cmdTriggerRising     = $81;
  cmdTriggerFalling    = $82;

  // Data resolution
  cmdListResolutions   = $90; // Returns: 1 = 8 bit, 2 = 10 bit, 3 = both 8 and 10 bit
  cmdSet8bit           = $91;
  cmdSet10bit          = $92;

  // Data descriptors offsets in data block
  dataSettingsOffset  = 0;
  dataChannelsOffset  = 1;
  dataOffset          = 2;
  // Masks to indicate specific configuration bits
  tenBitFlagMask      = 1 shl 7;
  triggerMask         = 1 shl 6;

implementation

end.

