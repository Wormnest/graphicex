unit DU_graphicex_TestFramework;

interface

uses
  {$IFNDEF FPC}
  TestFramework;
  {$ELSE}
  fpcunit, testregistry;
  {$ENDIF}

type
  // Tests for graphicex TFileFormatList class
  TTestFileFormatList = class(TTestCase)
  private
    {$IFNDEF FPC}
    MLM : IDUnitMemLeakMonitor;
    {$ENDIF}
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFileFormatListValid;
    procedure TestGraphicFromExtension;
    procedure TestUnregisterFileFormat;
    procedure TestFindExtension;
  end;

  TRegisterFileFormatTests = class(TTestCase)
  published
    procedure CheckNonExistingExtension;
    procedure CheckExistingExtension;
    procedure CheckLongExtension;
    procedure CheckRegisteringExtension;
    procedure CheckReplacingExtension;
  end;

  TRegisterFileFormatExceptionTests = class(TTestCase)
  private
    procedure DoNoExtensionException;
    procedure DoInvalidExtensionException;
    procedure DoTwiceReplaceFalseException;
    procedure DoGraphicClassNilException;
  published
    procedure CheckNoExtensionException;
    procedure CheckInvalidExtensionException;
    procedure CheckTwiceReplaceFalseException;
    procedure CheckGraphicClassNilException;
  end;

implementation

uses SysUtils, Classes, Graphics,
     gexTypes, GraphicStrings, GraphicEx;


procedure TTestFileFormatList.SetUp;
begin
  inherited;
  {$IFNDEF FPC}
  MLM := nil;
  {$ENDIF}

  // TODO: Maybe it would be better to create our own FileFormat object here.

  // Make sure that bmp extension and TBitmap class is always present since
  // it may get removed by some tests.
  if FileFormatList.GraphicFromExtension('bmp') = nil then
    FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TBitmap);
end;

procedure TTestFileFormatList.TearDown;
begin
  inherited;
  {$IFNDEF FPC}
  MLM := nil;
  {$ENDIF}
end;

procedure TTestFileFormatList.TestFileFormatListValid;
begin
  Check(FileFormatList <> nil, 'FileFormatList = nil');
end;

procedure TTestFileFormatList.TestGraphicFromExtension;
begin
  // TODO: Complete filenames, UNC names, extensions with or without '.' are
  // allowed so we need to test for all these cases too.

  // 1. test for extension that shouldn't exist
  Check(FileFormatList.GraphicFromExtension('xyz') = nil, 'Graphic Extension xyz shouldn''t exist!');
  // 2. test for extension that should exist
  Check(FileFormatList.GraphicFromExtension('bmp') <> nil, 'Graphic Extension bmp should exist!');
end;

type
  TFileFormatListAccess = class(TFileFormatList)
  end;

procedure TTestFileFormatList.TestUnregisterFileFormat;
begin
  // 1. Test unregistering known to exist extension
  FileFormatList.UnregisterFileFormat('bmp', TBitmap);
  Check(FileFormatList.GraphicFromExtension('bmp') = nil, 'Graphic Extension bmp shouldn''t be registered!');
  // 2. Test unregistering extension that we know doesn't exist
  FileFormatList.UnregisterFileFormat('qwerty', TBitmap);
  Check(FileFormatList.GraphicFromExtension('qwerty') = nil, 'Graphic Extension qwerty shouldn''t be registered!');
  // 3. Unregistering whole class should work
  FileFormatList.UnregisterFileFormat('', TBitmap);
  Check(TFileFormatListAccess(FileFormatList).FindGraphicClass(TBitmap) = -1, 'GraphicClass TBitmap shouldn''t be registered!');
  // 4. Unregistering whole class should work even if it doesn't exist
  FileFormatList.UnregisterFileFormat('', TBitmap);
  Check(TFileFormatListAccess(FileFormatList).FindGraphicClass(TBitmap) = -1, 'GraphicClass TBitmap shouldn''t be registered!');
  // 5. Check we don't crash on nil class
  FileFormatList.UnregisterFileFormat('', nil);
  Check(TFileFormatListAccess(FileFormatList).FindGraphicClass(nil) = -1, 'GraphicClass nil shouldn''t be registered!');
end;

procedure TTestFileFormatList.TestFindExtension;
begin
  // Check for being able to find an existing extension
  Check(TFileFormatListAccess(FileFormatList).FindExtension('bmp') <> -1, 'Graphic Extension bmp should exist!');
  // Check for not being able to find a non existing extension
  Check(TFileFormatListAccess(FileFormatList).FindExtension('qwerty') = -1, 'Graphic Extension qwerty should not exist!');
  // Check for not being able to find an empty '' extension
  Check(TFileFormatListAccess(FileFormatList).FindExtension('') = -1, 'Graphic Extension '''' should not exist!');
end;

//////////////// RegisterFileFormat - Basic Tests //////////////////////////

procedure TRegisterFileFormatTests.CheckNonExistingExtension;
begin
  // 1. test that extension doesn't exist
  Check(FileFormatList.GraphicFromExtension('xyz') = nil, 'Graphic Extension xyz shouldn''t exist!');
end;

procedure TRegisterFileFormatTests.CheckExistingExtension;
begin
  // 4. test that bmp extension exists
  Check(FileFormatList.GraphicFromExtension('bmp') <> nil, 'Graphic Extension bmp should exist!');
end;

procedure TRegisterFileFormatTests.CheckLongExtension;
var grclass: TGraphicClass;
begin
  // 6. Creating extension longer than 3 chars should work
  FileFormatList.RegisterFileFormat('longextension', gesBitmaps, '', [ftRaster], False, TBitmap);
  grclass := FileFormatList.GraphicFromExtension('longextension');
  Check( grclass <> nil, 'Graphic Extension longextension should exist!');

  // 7. test that extension doesn't exist
  FileFormatList.UnregisterFileFormat('longextension', TBitmap);
  Check( FileFormatList.GraphicFromExtension('longextension') = nil, 'Graphic Extension xyz shouldn''t exist!');
end;

procedure TRegisterFileFormatTests.CheckRegisteringExtension;
var grclass: TGraphicClass;
begin
  // todo: since "" as extension for UNREGISTER is allowed we need to test for that too!

  // Some memory isn't directly freed when registering and unregistering
  // file formats thus we turn FailsOnMemoryLeak off
  {$IFNDEF FPC}
  FailsOnMemoryLeak := False;
  {$ENDIF}

  // 2. test that extension is created and that Graphic Class is TBitmap
  FileFormatList.RegisterFileFormat('xyz', gesBitmaps, '', [ftRaster], False, TBitmap);
  grclass := FileFormatList.GraphicFromExtension('xyz');
  Check( grclass <> nil, 'Graphic Extension xyz should exist!');
  Check( grclass = TBitmap, 'Graphic Class should be TBitmap!');

  // 3. test that extension doesn't exist
  FileFormatList.UnregisterFileFormat('xyz', TBitmap);
  Check( FileFormatList.GraphicFromExtension('xyz') = nil, 'Graphic Extension xyz shouldn''t exist!');
end;

procedure TRegisterFileFormatTests.CheckReplacingExtension;
var grclass: TGraphicClass;
begin
  {$IFNDEF FPC}
  FailsOnMemoryLeak := False;
  {$ENDIF}

  // 1. Test that bmp extension exists
  Check(FileFormatList.GraphicFromExtension('bmp') <> nil, 'Graphic Extension bmp should exist!');

  // 2. test that extension is replaced created and that Graphic Class is TTiffGraphic
  FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], True, TTiffGraphic);
  grclass := FileFormatList.GraphicFromExtension('bmp');
  Check( grclass <> nil, 'Graphic Extension bmp should exist!');
  Check( grclass = TTiffGraphic, 'Graphic Class should be TTiffGraphic!');

  // 3. test that bmp extension is connected to TBitmap again
  FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], True, TBitmap);
  grclass := FileFormatList.GraphicFromExtension('bmp');
  Check( grclass <> nil, 'Graphic Extension bmp should exist!');
  Check( grclass = TBitmap, 'Graphic Class should be TBitmap!');
end;

//////////////// RegisterFileFormat - Exception Tests //////////////////////////

procedure TRegisterFileFormatExceptionTests.DoNoExtensionException;
begin
  // Setting empty extension should fail
  FileFormatList.RegisterFileFormat('', gesBitmaps, '', [ftRaster], False, TBitmap);
end;

procedure TRegisterFileFormatExceptionTests.DoInvalidExtensionException;
begin
  // Setting extension starting with a '.' should fail
  FileFormatList.RegisterFileFormat('.dot', gesBitmaps, '', [ftRaster], False, TBitmap);
end;

procedure TRegisterFileFormatExceptionTests.DoTwiceReplaceFalseException;
begin
  // Setting bmp extension twice should fail when Replace is False
  FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TBitmap);
end;

procedure TRegisterFileFormatExceptionTests.DoGraphicClassNilException;
begin
  // Registering file format with GraphicClass nil should fail
  FileFormatList.RegisterFileFormat('xyz', gesBitmaps, '', [ftRaster], False, nil);
end;

procedure TRegisterFileFormatExceptionTests.CheckNoExtensionException;
begin
  // Setting empty extension should fail
  CheckException({$IFDEF FPC}@{$ENDIF}DoNoExtensionException, EgexInvalidGraphic,
    'Setting empty extension should fail!');
end;

procedure TRegisterFileFormatExceptionTests.CheckInvalidExtensionException;
begin
  // Setting extension starting with a '.' should fail
  CheckException({$IFDEF FPC}@{$ENDIF}DoInvalidExtensionException, EgexInvalidGraphic,
    'Setting extension starting with a ''.''should fail!');

{
  // 5. Test that adding extension starting with '.' fails
  // Since FileFormatList.GraphicFromExtension allows extension (whole filenames)
  // we can't use that function for testing if .dot was added
  // We compare number of extensions in list before and after adding
  ExtList := TStringList.Create;
  try
    FileFormatList.GetExtensionList(ExtList);
    ExtCount1 := ExtList.Count;
    ExtList.Clear();
    FileFormatList.RegisterFileFormat('.dot', gesBitmaps, '', [ftRaster], False, TBitmap);
    FileFormatList.GetExtensionList(ExtList);
    ExtCount2 := ExtList.Count;
    Check( ExtCount2 = ExtCount1, 'Creating Graphic Extension .dot should fail!');
  finally
    ExtList.Free;
  end;
}
end;

procedure TRegisterFileFormatExceptionTests.CheckTwiceReplaceFalseException;
begin
  // Setting bmp extension twice should fail when Replace is False
  CheckException({$IFDEF FPC}@{$ENDIF}DoTwiceReplaceFalseException, EgexInvalidGraphic,
    'Setting bmp extension twice should fail when Replace is False!');
end;

procedure TRegisterFileFormatExceptionTests.CheckGraphicClassNilException;
begin
  // Registering file format with GraphicClass nil should fail
  CheckException({$IFDEF FPC}@{$ENDIF}DoGraphicClassNilException, EgexInvalidGraphic,
    'Registering file format with GraphicClass nil should fail!');
end;


initialization
  RegisterTests('Test GraphicEx.Test Class FileFormatList.Basic Tests',
    [TTestFileFormatList{$IFNDEF FPC}.Suite{$ENDIF}]);
  RegisterTests('Test GraphicEx.Test Class FileFormatList.RegisterFileFormat',
    [TRegisterFileFormatTests{$IFNDEF FPC}.Suite{$ENDIF}]);
  RegisterTests('Test GraphicEx.Test Class FileFormatList.RegisterFileFormat.Exceptions',
    [TRegisterFileFormatExceptionTests{$IFNDEF FPC}.Suite{$ENDIF}]);
end.
