program DU_graphicex_test;
{
  Delphi DUnit Test Project
  -------------------------

  Testing Suite for the GraphicEx units.
}


{$IFNDEF FPC}
{$IFNDEF FASTMM}
  !!!Alert. "FASTMM" required in project conditionals!
{$ENDIF}
{$ENDIF}

uses
  {$IFNDEF FPC}
  FastMM4,
  D6Support,                           // Ignore known memory leaks
  {$ELSE}
  {$IFDEF HEAPTRC}
  SysUtils,
  {$ENDIF}
  Forms, Interfaces,
  fpcunittestrunner,
  {$ENDIF}
  GUITestRunner,
  DU_graphicex_TestFramework,          // Test unit GraphicEx
  DU_GraphicColor_Tests,               // Test unit GraphicColor
  DU_ImageReader_Tests;

{$R *.RES}

begin
  {$IFNDEF FPC}
  GUITestRunner.RunRegisteredTests;
  {$ELSE}
  {$IFDEF HEAPTRC}
  // Set up -gh output for the Leakview package:
  if FileExists('heaptrc.log') then
    DeleteFile('heaptrc.log');
  SetHeapTraceOutput('heaptrc.log');
  {$ENDIF DEBUG}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ENDIF}
end.
