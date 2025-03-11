program LazScope;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} { $ IFDEF UseCThreads}
  cthreads,
  {$ENDIF} { $ ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, mainGUI, SerialThread, EpikTimer, serialobject,
  {$ifdef Windows}
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes,
  {$endif}
  commands;

{$R *.res}

begin
  {$ifdef Windows}
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$endif}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

