// Unit to read (and maybe in the future write) test settings.
// It does its work in the initialization of this unit.
unit TestSettings;

interface

var
  // Root folder where test images can be found.
  ImagesBasePath: string;

implementation

uses SysUtils, IniFiles, Forms, Windows;

const
  // Ini file settings
  C_IniSectionMain  = 'Settings';
  C_IniRootFolder   = 'ImagesBasePath'; // Including "\" at the end.

// Ini file is expected to be in the same folder as the test binary.
// It should have the same name as the binary but with a .ini extension.
procedure ReadSettings;
var IniFile: TIniFile;
  s: string;
begin
  s := ChangeFileExt(Application.ExeName,'.ini');
  if not FileExists(s) then begin
    MessageBox(0, PChar('Test settings file ' + s + ' does not exist!'#13#10+
      'Please create this ini file with a "[Settings] section".'#13#10+
      'Inside that section you should add ImagesBasePath=<location of your folder with test images>.'#13#10+
      'The folder path should end with a "\".'),
      'ImageReader Tests', mb_iconhand + mb_ok);
    Exit;
  end;
  IniFile := TIniFile.Create(s);
  try
    ImagesBasePath := IniFile.ReadString(C_IniSectionMain, C_IniRootFolder, '');
    if (ImagesBasePath = '') or not DirectoryExists(ImagesBasePath) then begin
      MessageBox(0, 'Path to test images does not exist!'#13#10+
        'Please set ImagesBasePath to a correct location with test images.'#13#10+
        'Note that this path should end with a "\".'#13#10+
        'Beware that you also need to initialize the unit-tests.xml files in each image folder!'#13#10+
        'You can do that with the XmlGenerator project in the dev folder.',
        'ImageReader Tests', mb_iconhand + mb_ok);
      ImagesBasePath := '';
    end;
  finally
    IniFile.Free;
  end;
end;

initialization
  ReadSettings;
end.
