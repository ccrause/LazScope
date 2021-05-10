unit SerialThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, serialobject, Types;

type
  TDataBuffer = TByteDynArray;
  TStatusNotify = procedure(const s: string) of object;

  { TSerialInterface }

  TSerialInterface = class(TThread)
  private
    FCmdBuffer: TFPList;
    FSerial: TSerialObj;
    FData: TDataBuffer;
    FErrorMessage: string;
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
    FError(FErrorMessage);
end;

procedure TSerialInterface.SendCommandWaitForResponse(const cmd: byte);
var
  reply, v1, v2: byte;
  recv: integer;
begin
  FSerial.Write(cmd);

  if cmd = cmdSendData then
  begin
    recv := FSerial.ReadTimeout(FData[0], bufsize, 2000);
    if recv < bufsize then // retry for remaining data
      recv := recv + FSerial.ReadTimeout(FData[recv], (bufsize - recv), 2000);

    if (recv < bufsize) then
    begin
      FErrorMessage := 'Comms timeout';
      Synchronize(@PushError);
    end
    else if recv < bufsize then
    begin
      FErrorMessage := 'Data buffer underflow';
      Synchronize(@PushError);
    end
    else
      FlagDataAvailable;
  end
  else if (cmd = cmdSampleCount) then // also resize internal data buffer
  begin
    FSerial.ReadByteTimeout(v1, 500);
    FSerial.ReadByteTimeout(v2, 500);

    SerialReturnValue := v1 + (v2 shl 8);
    SetLength(FData, CalcDataBufferSize(SerialReturnValue));
  end
  else if (cmd = cmdADCPins) or (cmd = cmdEcho) then
  begin
    FSerial.ReadByteTimeout(v1, 500);
    SerialReturnValue := v1;
  end
  else
  begin
    reply := 255;
    FSerial.ReadByteTimeout(reply, 500);
    if (reply XOR cmd) <> 0 then   // echo mismatch
    begin
      FErrorMessage := 'Invalid reply echo';
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
    // Execute configuration commands first
    while FCmdBuffer.Count > 0 do
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

    // Clear the event
    RTLeventResetEvent(FWaitingToProceed);

    // Now wait for next event
    RTLeventWaitFor(FWaitingToProceed);
  end;
  FSerial.Free;
  FDone := true;
end;

constructor TSerialInterface.Create(fSerialPortName: string; fBaudRate: integer);
begin
  inherited Create(false);
  System.InitCriticalSection(rcSection);
  FDone := false;
  SerialReturnValue := 0;

  FSerial := TSerialObj.Create;
  FSerial.OpenPort(fSerialPortName, fBaudRate);
  FSerial.FlushInput;

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

