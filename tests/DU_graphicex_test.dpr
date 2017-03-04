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
  {$IFDEF VER140}
  D6Support,                           // Ignore known memory leaks
  {$ENDIF}
  {$ELSE}
  {$IFDEF HEAPTRC_LOG}
  HeaptrcLog,                          // Heaptrc logging should start as early as possible
  {$ENDIF}
  Forms, Interfaces,
  fpcunittestrunner,
  {$ENDIF}
  GUITestRunner,
  DU_graphicex_TestFramework,          // Test unit GraphicEx
  DU_GraphicColor_Tests,               // Test unit GraphicColor
  DU_GraphicCompression_Tests,         // Test unit GraphicCompression
  DU_ImageReader_Tests,
  DU_JpegTests;

{$R *.RES}

begin
  {$IFNDEF FPC}
  GUITestRunner.RunRegisteredTests;
  {$ELSE}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ENDIF}
end.
