program LazScope;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} { $ IFDEF UseCThreads}
  cthreads,
  {$ENDIF} { $ ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, mainGUI, SerialThread, EpikTimer, serialobject,
  commands,
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes;

{$R *.res}

begin
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

