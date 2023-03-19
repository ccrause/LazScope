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

  // List available ADC channels, return channels as bitpacked byte
  cmdListADCchannels      = $10;
  // Specify selected ADC channels to sample
  // Command should be followed by bitmap of ports
  cmdSetActiveADCchannels = $11;

  // Select ADC prescaler option
  cmdSetADCDiv2              = $20;
  cmdSetADCDiv4              = $21;
  cmdSetADCDiv8              = $22;
  cmdSetADCDiv16             = $23;
  cmdSetADCDiv32             = $24;
  cmdSetADCDiv64             = $25;
  cmdSetADCDiv128            = $26;

  // Select ADC reference voltage
  cmdGetADCVoltage_2_56   = $30;
  cmdSetADCVoltage_VCC    = $31;
  cmdSetADCVoltage_1_1    = $32;
  cmdSetADCVoltage_AREF   = $33;
  cmdSetADCVoltage_2_56   = $34;
  cmdHasADCVoltage_2_56   = cmdSetADCVoltage_2_56;
  //cmdSetADCVoltage_2_56_cap   = $35;  // attinyx5: Decoupling cap on Aref pin (PB0)

  // Request size of data buffer
  cmdGetBufferSize           = $62; // 'b' = buffer size;
  // Specific command that should be echoed from controller
  cmdEcho                 = $65; // 'e' = echo
  // Request data frame from Arduino
  cmdSendData             = $73; // 's' = start

  // Trigger commands
  cmdTriggerOff           = $80;
  // Next 2 commands set trigger type
  // Followed by trigger level (0 ... 255), for 10 bit data this gets multiplied by 4
  cmdTriggerRising        = $81;
  cmdTriggerFalling       = $82;

  // Data resolution
  cmdListResolutions      = $90; // Returns: 1 = 8 bit, 2 = 10 bit, 3 = both 8 and 10 bit
  cmdSet8bit              = $91;
  cmdSet10bit             = $92;

  // Data descriptors offsets in data block
  dataSettingsOffset      = 0;
  dataChannelsOffset      = 1;
  dataOffset              = 2;
  // Masks to indicate specific configuration bits
  tenBitFlagMask          = 1 shl 7;
  triggerMask             = 1 shl 6;

implementation

end.

