unit SerialThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, serialobject, Types;

type
  TDataBuffer = TByteDynArray;
  TStatusNotify = procedure(const s: string; stopRun: boolean) of object;

  { TSerialInterface }

  TSerialInterface = class(TThread)
  private
    FCmdBuffer: TFPList;
    FSerial: TSerialObj;
    FData: TDataBuffer;
    FErrorMessage: string;
    FAbort: boolean;
    FError: TStatusNotify;
    FWaitingToProceed: PRTLEvent;
    rcSection: TRTLCriticalSection;
    FDone: boolean;

    // Sends a message to Form1 notifying new data
    procedure FlagDataAvailable;

    // Check if an error event handler is attached
    // Then call event handler with error message
    procedure PushError;

    procedure SendCommandWaitForResponse(const cmd: byte);
  protected
    procedure Execute; override;
  public
    SerialReturnValue: integer;

    constructor Create(fSerialPortName: string; fBaudRate: integer);

    // Called by main thread to get new copy of data
    procedure PullData(var data: TDataBuffer);

    // Submits a command to the command buffer
    procedure SetCommand(cmd: byte);

    // Call Terminate, then trigger FWaitingToProceed to run the Execute loop
    procedure WakeUpAndTerminate;

    // Event that will be called to display error messages
    property OnErrorNotify: TStatusNotify read FError write FError;
    property Done: boolean read FDone;
  end;

implementation

uses
  mainGUI, LCLintf, Commands, SysUtils;

{ TSerialInterface }

procedure TSerialInterface.FlagDataAvailable;
begin
  PostMessage(Form1.Handle, WM_DataWaiting, 0, 0);
end;

procedure TSerialInterface.PushError;
begin
  if assigned(FError) then
    FError(FErrorMessage, FAbort);
end;

procedure TSerialInterface.SendCommandWaitForResponse(const cmd: byte);
var
  reply, v1, v2, i: byte;
  recv: integer;
begin
  SerialReturnValue := 0;
  recv := FSerial.Write(cmd);
  // Detect if there was an error
  if recv <= 0 then
    exit;

  if cmd = cmdSendData then
  begin
    recv := FSerial.ReadTimeout(FData[0], bufsize, 3000);
    if recv < bufsize then // retry for remaining data
      recv := recv + FSerial.ReadTimeout(FData[recv], (bufsize - recv), 3000);

    if (recv < bufsize) then
    begin
      FErrorMessage := 'Comms timeout';
      FAbort := true;
      Synchronize(@PushError);
    end
    else
      FlagDataAvailable;
  end
  else if (cmd = cmdGetBufferSize) then // also resize internal data buffer
  begin
    FSerial.ReadByteTimeout(v1, 500);
    FSerial.ReadByteTimeout(v2, 500);

    SerialReturnValue := v1 + (v2 shl 8);
    SetLength(FData, SerialReturnValue);
  end
  else if cmd in [cmdListADCchannels, cmdGetADCVoltage_2_56, cmdListResolutions] then
  begin
    FSerial.ReadByteTimeout(v1, 500);
    SerialReturnValue := v1;
  end
  else if cmd = cmdEcho then
  begin
    // A bootloader may delay startup of firmware
    // so clear input buffer and try to receive an echo cmd
    i := 0;
    repeat
      // First command sent to verify connection and firmware
      // Try to clear garbage from buffer
      FSerial.FlushInput;
      Sleep(250);
      v1 := 0;
      FSerial.ReadByteTimeout(v1, 100);
      if v1 <> cmdEcho then
      begin
        inc(i);
        recv := FSerial.Write(cmd);
        // Detect if there was an error
        if recv <= 0 then
          exit;
      end;
    until (v1 = cmdEcho) or (i > 8);
    SerialReturnValue := v1;
  end
  else
  begin
    reply := 255;
    FSerial.ReadByteTimeout(reply, 500);
    if (reply XOR cmd) <> 0 then   // echo mismatch
    begin
      FErrorMessage := 'Invalid reply echo';
      FAbort := false;
      Synchronize(@PushError);
    end;
  end;
end;

procedure TSerialInterface.Execute;
var
  cmd: byte;
begin
  while not Terminated do
  begin
    FSerial.FlushInput;
    // Execute configuration commands first
    while (FCmdBuffer.Count > 0) and not Terminated do
    begin
      System.EnterCriticalsection(rcSection);
      try
        cmd := lo(integer(FCmdBuffer[0]));
        FCmdBuffer.Delete(0);
      finally
        System.LeaveCriticalsection(rcSection);
      end;

      SendCommandWaitForResponse(cmd);
      if cmd = cmdSendData then break;  // wait for main thread to request more data
    end;

    // Clear the event if there are no commands waiting
    System.EnterCriticalsection(rcSection);
    try
      if FCmdBuffer.Count = 0 then
        RTLeventResetEvent(FWaitingToProceed);
    finally
      System.LeaveCriticalsection(rcSection);
    end;

    // Now wait for next event, but only if not terminated
    if not Terminated then
      RTLeventWaitFor(FWaitingToProceed);
  end;
  FSerial.Free;
  FDone := true;
end;

constructor TSerialInterface.Create(fSerialPortName: string; fBaudRate: integer);
begin
  inherited Create(false);
  FreeOnTerminate := false;  // Requires FreeAndNil call in mainGUI.CloseSerialThread
  System.InitCriticalSection(rcSection);
  FDone := false;
  SerialReturnValue := 0;

  FSerial := TSerialObj.Create;
  FSerial.OpenPort(fSerialPortName, fBaudRate);

  FWaitingToProceed := RTLEventCreate;
  FCmdBuffer := TFPList.Create;
  FCmdBuffer.Capacity := 16;     // should not accumulate this many commands anyway

  // Arduino resets on serial connect, so wait a short while
  Sleep(500);
end;

procedure TSerialInterface.PullData(var data: TDataBuffer);
begin
  data := copy(FData);
end;

procedure TSerialInterface.SetCommand(cmd: byte);
begin
  System.EnterCriticalsection(rcSection);
  try
    FCmdBuffer.Add(pointer(integer(cmd)));
  finally
    System.LeaveCriticalsection(rcSection);
  end;

  // Wake up thread to execute command
  RTLeventSetEvent(FWaitingToProceed);
end;

procedure TSerialInterface.WakeUpAndTerminate;
begin
  Terminate;
  RTLeventSetEvent(FWaitingToProceed);
end;

end.

