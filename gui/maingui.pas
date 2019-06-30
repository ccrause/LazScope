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
    Chart1: TChart;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointCrosshairTool1: TDataPointCrosshairTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ADCPortsList: TCheckGroup;
    Label4: TLabel;
    SingleShotCheck: TCheckBox;
    Label2: TLabel;
    TriggerOptionsRadioBox: TRadioGroup;
    RunningCheck: TCheckBox;
    ReferenceVoltageSelector: TComboBox;
    Label3: TLabel;
    ADCScalerSelector: TComboBox;
    Label1: TLabel;
    TriggerLevelEdit: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure ADCPortsListClick(Sender: TObject);
    procedure ADCPortsListItemClick(Sender: TObject; Index: integer);
    procedure ChartToolset1DataPointCrosshairTool1AfterKeyUp(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1DataPointCrosshairTool1AfterMouseMove(
      ATool: TChartTool; APoint: TPoint);
    procedure ChartToolset1DataPointCrosshairTool1Draw(
      ASender: TDataPointCrosshairTool);
    procedure ChartToolset1ReticuleTool1BeforeMouseMove(ATool: TChartTool;
      APoint: TPoint);
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

    procedure Processdata;
    procedure Status(const s: string);

    procedure DataWaiting(var Message: TLMessage); message WM_DataWaiting;

    procedure CheckSelectedADCPorts;
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
  synaser, TACustomSeries, TAChartUtils;

const
  LineColors: array[0..7] of TColor =
    (clLime, clBlue, clRed, clPurple, clYellow, clMaroon, clWhite, clAqua);

  ScanFrames: array[0..6] of double =
    (5.75, 6.68, 10.36, 17.75, 35.3, 66.1, 132);

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
  Unused(ATool, APoint);
  ChartToolset1DataPointCrosshairTool1.Hide;
end;

procedure TForm1.ChartToolset1DataPointCrosshairTool1AfterMouseMove(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
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
  Label4.Caption := format('%0.4f'#13'%d', [ASender.Position.X, round(ASender.Position.Y)]);
end;

procedure TForm1.ChartToolset1ReticuleTool1BeforeMouseMove(ATool: TChartTool;
  APoint: TPoint);
begin
  Label4.Caption := IntToStr(APoint.X) + #13 + IntToStr(APoint.y);
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
  running := false;
  SerialThread.Terminate;
  SerialThread.SetCommand(0);  // Need to wake thread if asleep / waiting for command

  while not SerialThread.Done do
  begin
    Application.ProcessMessages;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  cmd: byte;
begin
  epTimer := TEpikTimer.Create(self);
  if epTimer.HWCapabilityDataAvailable then
    epTimer.TimebaseSource:= HardwareTimebase;

  if not assigned(SerialThread) then
  begin
    SerialThread := TSerialInterface.Create(False);
    AskForNewData := false;
    SerialThread.OnErrorNotify := @self.Status;
  end;

  SerialThread.SetCommand(cmdSampleCount);
  repeat
    Sleep(10);
  until SerialThread.NumSamples > 0;

  numsamples := SerialThread.NumSamples;
  bufsize := CalcDataBufferSize(numsamples);
  SetLength(buf, bufsize);
  SetLength(data, numsamples);

  ADCScalerSelector.ItemIndex := 3;
  cmd := cmdADCDiv2 + ADCScalerSelector.ItemIndex;
  SerialThread.SetCommand(cmd);
  // Estimate of time scale
  TimeFrame := ScanFrames[ADCScalerSelector.ItemIndex] / 1180 * numsamples;

  // Sync ADC prescaler with Arduino
  ADCPortsList.Checked[0] := true;
  CheckSelectedADCPorts;

  // Sync ADC trigger with GUI
  TriggerOptionsRadioBoxClick(nil);

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
  Chart1.Extent.XMax := ScanFrames[ADCScalerSelector.ItemIndex] / 1180 * numsamples;
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

procedure TForm1.ProcessData;
var
  i, j: integer;
  checksum, l, h: byte;
  t: dword;
  delta: double;
begin
  checksum := 0;

  for i := 0 to length(data)-1 do
  begin
    //checksum := checksum XOR buf[i];
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
    inc(j);
    if j = numPortsSelected then
      j := 0;
  end;

  for j := 0 to numPortsSelected-1 do
    TLineSeries(Chart1.Series[j]).EndUpdate;

  if checksum = 0 then
    Status('OK')
  else
  begin
    Status('Checksum failed');
    Application.ProcessMessages;
    Sleep(100);
  end;
end;

procedure TForm1.Status(const s: string);
begin
  StatusBar1.Panels[6].Text := s;
end;

procedure TForm1.DataWaiting(var Message: TLMessage);
var
  localtimedata: TimerData;
begin
  SerialThread.PullData(buf);

  if AskForNewData and not singleShot then
    SerialThread.SetCommand(cmdSendData);

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
  i: integer;
  PortsSelected: byte;
  newLineSeries: TLineSeries;
  sl: TStringList;
begin
  PortsSelected := 0;
  numPortsSelected := 0;
  sl := TStringList.Create;

  for i := 0 to ADCPortsList.Items.Count-1 do
    if ADCPortsList.Checked[i] then
    begin
      PortsSelected := PortsSelected + (1 shl i);
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

end.


