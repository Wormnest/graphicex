unit DU_graphicex_TestFramework;

interface

uses
  TestFramework;

type
  // Tests for graphicex TFileFormatList class
  TTestFileFormatList = class(TTestCase)
  private
    MLM : IDUnitMemLeakMonitor;
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
  published
    procedure CheckNoExtensionException;
    procedure CheckInvalidExtensionException;
    procedure CheckTwiceReplaceFalseException;
    procedure CheckGraphicClassNilException;
  end;

implementation

uses SysUtils, Classes, Graphics,
     GraphicStrings, graphicex;


procedure TTestFileFormatList.SetUp;
begin
  inherited;
  MLM := nil;

  // TODO: Maybe it would be better to create our own FileFormat object here.
end;

procedure TTestFileFormatList.TearDown;
begin
  inherited;
  MLM := nil;
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

procedure TTestFileFormatList.TestUnregisterFileFormat;
begin
  //Check(True);
end;

procedure TTestFileFormatList.TestFindExtension;
begin
  //Check(True);
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
  FailsOnMemoryLeak := False;

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
  FailsOnMemoryLeak := False;

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

procedure TRegisterFileFormatExceptionTests.CheckNoExtensionException;
begin
  // Setting empty extension should fail
  StartExpectingException(EInvalidGraphic);
  FileFormatList.RegisterFileFormat('', gesBitmaps, '', [ftRaster], False, TBitmap);
  StopExpectingException('Setting empty extension should fail!');
end;

procedure TRegisterFileFormatExceptionTests.CheckInvalidExtensionException;
begin
  // Setting extension starting with a '.' should fail
  StartExpectingException(EInvalidGraphic);
  FileFormatList.RegisterFileFormat('.dot', gesBitmaps, '', [ftRaster], False, TBitmap);
  StopExpectingException('Setting extension starting with a ''.''should fail!');

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
  StartExpectingException(EInvalidGraphic);
  FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TBitmap);
  StopExpectingException('Setting bmp extension twice should fail when Replace is False!');
end;

procedure TRegisterFileFormatExceptionTests.CheckGraphicClassNilException;
begin
  // Registering file format with GraphicClass nil should fail
  StartExpectingException(EInvalidGraphic);
  FileFormatList.RegisterFileFormat('xyz', gesBitmaps, '', [ftRaster], False, nil);
  StopExpectingException('Registering file format with GraphicClass nil should fail!');
end;


initialization
  RegisterTests('Test graphicex.Test Class FileFormatList.Basic Tests',
    [TTestFileFormatList.Suite]);
  RegisterTests('Test graphicex.Test Class FileFormatList.RegisterFileFormat',
    [TRegisterFileFormatTests.Suite]);
  RegisterTests('Test graphicex.Test Class FileFormatList.RegisterFileFormat.Exceptions',
    [TRegisterFileFormatExceptionTests.Suite]);
end.
