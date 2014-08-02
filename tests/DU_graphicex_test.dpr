program DU_graphicex_test;
{
  Delphi DUnit Test Project
  -------------------------

  Testing Suite for the GraphicEx units.
}


{$IFNDEF FASTMM}
  !!!Alert. "FASTMM" required in project conditionals!
{$ENDIF}

uses
  FastMM4,
  D6Support,                           // Ignore known memory leaks
  GUITestRunner,
  DU_graphicex_TestFramework,          // Test unit GraphicEx
   DU_GraphicColor_Tests;               // Test unit GraphicColor

{$R *.RES}

begin
  GUITestRunner.RunRegisteredTests;
end.
