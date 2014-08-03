{ ViewerForm GraphicEx Image Viewer sample program demonstrating some of the
             capabilities of GraphicEx.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2014 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}
program ImageViewer;

uses
  FastMM4,
  Forms,
  ViewerForm in 'ViewerForm.pas' {frmViewer},
  gexThread in 'gexThread.pas',
  gexBlend in 'gexBlend.pas';

{$R *.res}

begin
{$IFDEF VER140}
  // It is Delphi 6 and FASTMM so register its known memory leaks
  RegisterExpectedMemoryLeak(36, 1); // TWinHelpViewer x 1
  RegisterExpectedMemoryLeak(20, 3); // TObjectList x 3
  RegisterExpectedMemoryLeak(20, 3); // Unknown x 3
  RegisterExpectedMemoryLeak(52, 1); // THelpManager x 1

{$IFNDEF FIXED_SHELLCTRLS}
  // ShellCtrls specific apparently: (The fixed version doesn't need these.)
  RegisterExpectedMemoryLeak(36, 1); // String x 1
  RegisterExpectedMemoryLeak(84, 1); // TShellChangeThread x 1
{$ENDIF}
  // Newer ShellCtrls doesn't need the next 2
  // RegisterExpectedMemoryLeak(52, 1); // TStringList x 1
  // RegisterExpectedMemoryLeak(52, 1); // TShellFolder x 1
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TfrmViewer, frmViewer);
  Application.Run;
end.
