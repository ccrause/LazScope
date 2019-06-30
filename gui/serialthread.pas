unit SerialThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, Synaser, Types;

type
  TDataBuffer = TByteDynArray;
  TStatusNotify = procedure(const s: string) of object;

  { TSerialInterface }

  TSerialInterface = class(TThread)
  private
    FCmdBuffer: TFPList;
    FSynaSer: TBlockSerial;
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

    //
    procedure SendCommandWaitForResponse(const cmd: byte);
  protected
    procedure Execute; override;
  public
    NumSamples: integer;

    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);

    // Called by main thread to get new copy of data
    procedure PullData(var data: TDataBuffer);

    // Submits a command to the command buffer
    procedure SetCommand(cmd: byte);

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
  reply: byte;
  recv: integer;
begin
  FSynaSer.SendByte(cmd);

  if cmd = cmdSendData then
  begin
    recv := FSynaSer.RecvBufferEx(@FData[0], bufsize, 2000);
    if recv < bufsize then // retry for remaining data
      recv := FSynaSer.RecvBufferEx(@FData[recv], (bufsize - recv), 2000);

    if (FSynaSer.LastError = ErrTimeout) then
    begin
      FErrorMessage := 'Comms timeout';
      Synchronize(@PushError);
    end
    else
      FlagDataAvailable;
  end
  else if (cmd = cmdSampleCount) then // also resize internal data buffer
  begin
    NumSamples := FSynaSer.RecvByte(500);
    NumSamples := NumSamples + (FSynaSer.RecvByte(500) shl 8);
    SetLength(FData, CalcDataBufferSize(NumSamples));
  end
  else
  begin
    reply := FSynaSer.RecvByte(1000);
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
  // Make sure Arduino is running by sending 0, should just echo 0.
  FSynaSer.SendByte(0);
  FSynaSer.RecvByte(500);

  if FSynaSer.LastError <> sOK then
  begin
    FErrorMessage := FSynaSer.LastErrorDesc;
    Synchronize(@PushError);
  end;

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
  FSynaser.Free;
  FDone := true;
end;

constructor TSerialInterface.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  FDone := false;
  NumSamples := 0;

  FSynaSer := TBlockSerial.Create;
  FSynaSer.RaiseExcept := true;
  FSynaSer.LinuxLock := false;  // makes debugging & crash recovery a little easier
  FSynaSer.Connect('/dev/ttyACM0');
  FSynaSer.Config(500000, 8, 'N', 1, false, false);

  // Looks like basic setup is working, disable exceptions
  FSynaSer.RaiseExcept := false;

  FWaitingToProceed := RTLEventCreate;
  FCmdBuffer := TFPList.Create;
  FCmdBuffer.Capacity := 16;     // should not accumulate this many commands anyway

  // Arduino resets on serial connect, so wait a short while
  Sleep(1000);
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

end.

