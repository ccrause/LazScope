unit mainGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, TAGraph, TASeries, TATransformations, TATools,
  SerialThread, commands, LMessages, LCLIntf, Spin, EpikTimer, types;

type
  { TForm1 }

  TForm1 = class(TForm)
    ADCPortsList: TCheckGroup;
    ADCScalerSelector: TComboBox;
    BaudEdit: TEdit;
    Chart1: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    connectButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelX: TLabel;
    LabelXlbl: TLabel;
    LabelY: TLabel;
    LabelYlbl: TLabel;
    Panel1: TPanel;
    ReferenceVoltageSelector: TComboBox;
    RunningCheck: TCheckBox;
    SerialComboBox: TComboBox;
    SingleShotCheck: TCheckBox;
    StatusBar1: TStatusBar;
    TriggerLevelEdit: TSpinEdit;
    TriggerOptionsRadioBox: TRadioGroup;
    procedure ADCPortsListClick(Sender: TObject);
    procedure ADCPortsListItemClick(Sender: TObject; Index: integer);
    procedure ChartToolset1DataPointCrosshairTool1AfterKeyUp(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1DataPointCrosshairTool1AfterMouseMove(
      ATool: TChartTool; APoint: TPoint);
    procedure ChartToolset1DataPointCrosshairTool1Draw(
      ASender: TDataPointCrosshairTool);
    procedure connectButtonClick(Sender: TObject);
    procedure SerialComboBoxDropDown(Sender: TObject);
    procedure SingleShotCheckChange(Sender: TObject);
    procedure RunningCheckChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FrameWidthSelectorClick(Sender: TObject);
    procedure ReferenceVoltageSelectorChange(Sender: TObject);
    procedure TriggerOptionsRadioBoxClick(Sender: TObject);
  private
    SerialThread: TSerialInterface;
    running: boolean;
    TimeFrame: double;
    buf: array of byte;
    data: array of word;
    AskForNewData: boolean;
    PreviousFrameTime: TimerData;
    epTimer: TEpikTimer;
    numPortsSelected: integer;
    connected: boolean;

    procedure Processdata;
    procedure Status(const s: string; stopRun: boolean);
    procedure DataWaiting(var Message: TLMessage); message WM_DataWaiting;
    procedure CheckSelectedADCPorts;
    function CloseSerialThread: boolean;
    procedure doConnected;
    procedure doDisconnected;
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  numsamples: integer = 0;
  bufsize: integer;
  singleShot: boolean = false;

function CalcDataBufferSize(const SampleCount: integer): integer;

implementation

{$R *.lfm}

{ TForm1 }

uses
  TACustomSeries, TAChartUtils, serialobject;

const
  LineColors: array[0..7] of TColor =
    (clLime, clBlue, clRed, clPurple, clYellow, clMaroon, clWhite, clAqua);

function CalcDataBufferSize(const SampleCount: integer): integer;
begin
  result := 3*(SampleCount div 2) + 2*(SampleCount and 1) + 4 + 1;
end;

procedure TForm1.RunningCheckChange(Sender: TObject);
begin
  if RunningCheck.Checked then
  begin
    AskForNewData := true;
    SerialThread.SetCommand(cmdSendData);  // Get thread processing

    epTimer.Clear(PreviousFrameTime);
    epTimer.Start(PreviousFrameTime);
  end
  else
    AskForNewData := false;
end;

procedure TForm1.ADCPortsListItemClick(Sender: TObject; Index: integer);
begin
  CheckSelectedADCPorts;
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1AfterKeyUp(
  ATool: TChartTool; APoint: TPoint);
begin
  TAChartUtils.Unused(ATool, APoint);
  ChartToolset1DataPointCrosshairTool1.Hide;
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1AfterMouseMove(
  ATool: TChartTool; APoint: TPoint);
begin
  TAChartUtils.Unused(ATool, APoint);
  Chart1.SetFocus;
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1Draw(
  ASender: TDataPointCrosshairTool);
const
  R = 20;
var
  p: TPoint;
begin
  p := Chart1.GraphToImage(ASender.Position);

  Chart1.Drawer.Ellipse(p.X - R, p.Y - R, p.X + R, p.Y + R);
  LabelX.Caption := FloatToStrF(ASender.Position.X, ffFixed, 4, 3);
  LabelY.Caption := IntToStr(round(ASender.Position.y));
end;

procedure TForm1.connectButtonClick(Sender: TObject);
var
  cmd: byte;
  baud, i: integer;
  err: boolean;
begin
  if not connected then
  begin
    baud := StrToInt(BaudEdit.Text);
    if (SerialComboBox.Text <> '') and (baud > 0) then
    begin
      if not assigned(SerialThread) then
      begin
        SerialThread := TSerialInterface.Create(SerialComboBox.Text, baud);

        AskForNewData := false;
        SerialThread.OnErrorNotify := @self.Status;
      end;

      err := false;
      SerialThread.SetCommand(cmdEcho);
      i := 0;
      repeat
        Sleep(10);
        Application.ProcessMessages;
        inc(i);
      until (SerialThread.SerialReturnValue > 0) or (i > 100);
      err := (SerialThread.SerialReturnValue <> cmdEcho);

      if not err then
      begin
        SerialThread.SerialReturnValue := 0;
        SerialThread.SetCommand(cmdSampleCount);
        i := 0;
        repeat
          Sleep(10);
          Application.ProcessMessages;
          inc(i);
        until (SerialThread.SerialReturnValue > 0) or (i > 100);
        err := SerialThread.SerialReturnValue = 0;
      end;

      if not err then
      begin
        numsamples := SerialThread.SerialReturnValue;
        bufsize := CalcDataBufferSize(numsamples);
        SetLength(buf, bufsize);
        SetLength(data, numsamples);

        ADCScalerSelector.ItemIndex := 3;
        cmd := cmdADCDiv2 + ADCScalerSelector.ItemIndex;
        SerialThread.SetCommand(cmd);

        SerialThread.SerialReturnValue := 0;
        SerialThread.SetCommand(cmdADCPins);
        i := 0;
        repeat
          Sleep(10);
          Application.ProcessMessages;
          inc(i)
        until (SerialThread.SerialReturnValue > 0) or (i > 100);
        err := SerialThread.SerialReturnValue = 0;
      end;

      if not err then
      begin
        ADCPortsList.Items.Clear;
        cmd := SerialThread.SerialReturnValue;
        for i := 0 to 7 do
        begin
          if (cmd and (1 shl i)) > 0 then
            ADCPortsList.Items.Add(IntToStr(i));
        end;

        // Sync ADC prescaler with Arduino
        ADCPortsList.Checked[0] := true;
        CheckSelectedADCPorts;

        // Sync ADC trigger with GUI
        TriggerOptionsRadioBoxClick(nil);
        doConnected;
        connected := true;
      end
      else
      begin
        CloseSerialThread;
        doDisconnected;
        Status('Error connecting to '+SerialComboBox.Text, true);
        connected := false;
      end;
    end;
  end
  else  // connected, so disconnect now
  begin
    CloseSerialThread;
    doDisconnected;
    connected := false;
  end;
end;

procedure TForm1.SerialComboBoxDropDown(Sender: TObject);
begin
  SerialComboBox.Items.CommaText := GetSerialPortNames;
end;

procedure TForm1.SingleShotCheckChange(Sender: TObject);
begin
  singleShot :=  SingleShotCheck.Checked;
end;

procedure TForm1.ADCPortsListClick(Sender: TObject);
begin
  CheckSelectedADCPorts;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if not CloseSerialThread then
  begin
    Status('Could not close serial thread, calling Halt', false);
    Application.ProcessMessages;
    Sleep(1500);
    Halt(255);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  epTimer := TEpikTimer.Create(self);
  if epTimer.HWCapabilityDataAvailable then
    epTimer.TimebaseSource:= HardwareTimebase;

  Chart1.Extent.YMin := 0;
  Chart1.Extent.YMax := 1024;
  Chart1.Extent.UseYMin := true;
  Chart1.Extent.UseYMax := true;
  Chart1.Extent.XMin := 0;
  Chart1.Extent.XMax := TimeFrame;
  Chart1.Extent.UseXMin := true;
  Chart1.Extent.UseXMax := true;
  Chart1.BottomAxis.Title.Caption := 'Time - ms';
  Chart1.BottomAxis.Title.Visible := true;
end;

procedure TForm1.FrameWidthSelectorClick(Sender: TObject);
var
  cmd: byte;
begin
  cmd := cmdADCDiv2 + ADCScalerSelector.ItemIndex;
  SerialThread.SetCommand(cmd);
end;


procedure TForm1.ReferenceVoltageSelectorChange(Sender: TObject);
var
  cmd: byte;
begin
  cmd := cmdADCVoltage_VCC + byte(ReferenceVoltageSelector.ItemIndex);
  SerialThread.SetCommand(cmd);
end;

procedure TForm1.TriggerOptionsRadioBoxClick(Sender: TObject);
var
  val: byte;
begin
  case TriggerOptionsRadioBox.ItemIndex of
  1: begin
       SerialThread.SetCommand(cmdTriggerRising);
       val := TriggerLevelEdit.Value shr 2;
       SerialThread.SetCommand(val);
     end;
  2: begin
       SerialThread.SetCommand(cmdTriggerFalling);
       val := TriggerLevelEdit.Value shr 2;
       SerialThread.SetCommand(val);
     end;
  else
    SerialThread.SetCommand(cmdTriggerOff);
  end;
end;

procedure TForm1.Processdata;
var
  i, j: integer;
  checksum, l, h: byte;
  t: dword;
  delta: double;
begin
  checksum := 0;

  for i := 0 to length(data)-1 do
  begin
    j := 3*(i shr 1) + (i mod 2);

    h := buf[j];
    l := buf[j+1];
    if (i mod 2) = 0 then  // left adjusted
    begin
      t := h shl 2 + l shr 6;
      data[i] := t;
      checksum := checksum XOR buf[j];
      checksum := checksum XOR buf[j + 1];
    end
    else  // right adjusted
    begin
      t := (h and %00000011);
      t := t shl 8;
      t := t + l;
      data[i] := t;
      checksum := checksum XOR buf[j+1];
    end;
  end;

  // Time frame in microseconds
  j := 3*(numsamples div 2) + (numsamples mod 2);
  t := (buf[j + 0] shl 24);
  t := t + (buf[j + 1] shl 16);
  t := t + (buf[j + 2] shl 8);
  t := t + buf[j + 3];
  checksum := checksum XOR buf[j + 0];
  checksum := checksum XOR buf[j + 1];
  checksum := checksum XOR buf[j + 2];
  checksum := checksum XOR buf[j + 3];

  TimeFrame := t / 1000;  // convert to milliseconds
  StatusBar1.Panels[1].Text := FloatToStrF(TimeFrame, ffFixed, 3, 2);

  // Set Extent to some sensible rounded value
  if TimeFrame < 10 then
    Chart1.Extent.XMax := round((TimeFrame*10) + 0.5) / 10
  else
    Chart1.Extent.XMax := round(TimeFrame + 0.5);

  // Check checksum:
  checksum := checksum XOR buf[j + 4];

  if numPortsSelected = 0 then exit;

  for j := 0 to numPortsSelected-1 do
  begin
    TLineSeries(Chart1.Series[j]).Clear;
    TLineSeries(Chart1.Series[j]).BeginUpdate;
  end;

  delta := TimeFrame / (numsamples-1);
  j := 0;
  for i := 0 to numsamples-1 do
  begin
    TLineSeries(Chart1.Series[j]).AddXY(delta*i, data[i]);
    // Waiting for trigger condition gives two data points for the first channel
    // So add the first two data points to the first series
    if (i > 0) or (TriggerOptionsRadioBox.ItemIndex = 0) or (numPortsSelected = 1) then
      inc(j);

    if j = numPortsSelected then
      j := 0;
  end;

  for j := 0 to numPortsSelected-1 do
    TLineSeries(Chart1.Series[j]).EndUpdate;

  if checksum = 0 then
    Status('OK', false)
  else
  begin
    // Do not stop, could be a temporary problem
    Status('Checksum failed', false);
    Application.ProcessMessages;
    Sleep(100);
  end;
end;

procedure TForm1.Status(const s: string; stopRun: boolean);
begin
  StatusBar1.Panels[6].Text := s;

  // An error (e.g. time-out) interrupted data flow
  if stopRun then
  begin
    RunningCheck.Checked := false;
    AskForNewData := false;
  end;
end;

procedure TForm1.DataWaiting(var Message: TLMessage);
var
  localtimedata: TimerData;
begin
  // Check for race condition if serial thread is terminated while main thread is pulling data
  if Assigned(SerialThread) and AskForNewData then
    SerialThread.PullData(buf)
  else
    exit;

  if AskForNewData and not singleShot then
    SerialThread.SetCommand(cmdSendData)
  else
    RunningCheck.Checked := false;

  epTimer.Stop(PreviousFrameTime);
  StatusBar1.Panels[3].Text := FloatToStrF(epTimer.Elapsed(PreviousFrameTime)*1000, ffGeneral, 4, 3);
  epTimer.Clear(PreviousFrameTime);
  epTimer.Start(PreviousFrameTime);

  epTimer.Clear(localtimedata);
  epTimer.Start(localtimedata);
  Processdata;
  epTimer.Stop(localtimedata);
  StatusBar1.Panels[5].Text := FloatToStrF(epTimer.Elapsed(localtimedata)*1000, ffGeneral, 4, 3);
end;

procedure TForm1.CheckSelectedADCPorts;
var
  i, offset: integer;
  PortsSelected: byte;
  newLineSeries: TLineSeries;
  sl: TStringList;
begin
  PortsSelected := 0;
  numPortsSelected := 0;
  sl := TStringList.Create;

  offset := StrToInt(ADCPortsList.Items[0]);
  for i := 0 to ADCPortsList.Items.Count-1 do
    if ADCPortsList.Checked[i] then
    begin
      PortsSelected := PortsSelected + (1 shl (i+offset));
      inc(numPortsSelected);
      sl.Add(copy(ADCPortsList.Items[i], 1, 2));
    end;

  // Same number of series & ports
  if numPortsSelected > Chart1.SeriesCount then
    while numPortsSelected > Chart1.SeriesCount do
    begin
      newLineSeries := TLineSeries.Create(self);
      Chart1.AddSeries(newLineSeries);
    end
  else if numPortsSelected < Chart1.SeriesCount then
    while numPortsSelected < Chart1.SeriesCount do
      Chart1.DeleteSeries(Chart1.Series[Chart1.SeriesCount-1]);

  // Copy titles of series from saved list
  for i := 0 to sl.Count-1 do
  begin
    TLineSeries(Chart1.Series[i]).Title := sl[i];
    TLineSeries(Chart1.Series[i]).SeriesColor := LineColors[i];
  end;

  sl.free;

  SerialThread.SetCommand(cmdSelectPorts);
  SerialThread.SetCommand(PortsSelected);
end;

function TForm1.CloseSerialThread: boolean;
var
  timeout: integer;
begin
  AskForNewData := false;
  running := false;
  if Assigned(SerialThread) then
  begin
    SerialThread.WakeUpAndTerminate;
    timeout := 10;
    while not SerialThread.Done and (timeout > 0) do
    begin
      Application.ProcessMessages;
      Sleep(100);
      dec(timeout);
    end;
    Result := timeout = 0;
    if not Result then
      FreeAndNil(SerialThread);
  end
  else
    Result := true;
end;

procedure TForm1.doConnected;
begin
  RunningCheck.Enabled := true;
  RunningCheck.Checked := false;
  AskForNewData := false;
  SingleShotCheck.Enabled := true;
  ADCScalerSelector.Enabled := true;
  ADCPortsList.Enabled := true;
  ReferenceVoltageSelector.Enabled := true;
  TriggerOptionsRadioBox.Enabled := true;
  TriggerLevelEdit.Enabled := true;
  SerialComboBox.Enabled := false;
  BaudEdit.Enabled := false;
  connectButton.Caption := 'Disconnect';
  Status('Connected to '+SerialComboBox.Text, false);
end;

procedure TForm1.doDisconnected;
begin
  RunningCheck.Enabled := false;
  SingleShotCheck.Enabled := false;
  ADCScalerSelector.Enabled := false;
  ADCPortsList.Enabled := false;
  ReferenceVoltageSelector.Enabled := false;
  TriggerOptionsRadioBox.Enabled := false;
  TriggerLevelEdit.Enabled := false;
  SerialComboBox.Enabled := true;
  BaudEdit.Enabled := true;
  connectButton.Caption := 'Connect';
  Status('Disconnected', true);
end;

end.
