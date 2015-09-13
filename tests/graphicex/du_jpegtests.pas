unit DU_JpegTests;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Graphics,
  {$IFNDEF FPC}
  TestFramework
  {$ELSE}
  fpcunit, testregistry
  {$ENDIF}
  ;

type

  TgexJpegImageTests = class(TTestCase)
  public
    Graphic: TGraphic;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanLoad;
    procedure TestReadImageProperties;
    procedure TestLoadImage;
  end;

implementation

uses GraphicEx, gexJpeg;

const // For now hardcode some test images
  ImagesBasePath = 'e:\Delphi\Projects\Transcript\test-images\jpg\';
  Test1 = ImagesBasePath + 'nuke-symbol.jpg';      // normal jpg
  Test2 = ImagesBasePath + 'unit-tests.xml';       // not a jpg
  Test3 = ImagesBasePath + 'wrong-extension.bmp';  // jpg with bmp extension

procedure TgexJpegImageTests.SetUp;
var gr: TGraphicClass;
begin
  Graphic := nil;
  //FileFormatList.UnregisterFileFormat('', TBitmap);
  gr := FileFormatList.GraphicFromExtension('jpg');
  if gr = TgexJpegImage then
    Exit // We are already registered
  else if gr <> nil then begin
    FileFormatList.UnregisterFileFormat('jfif', nil);
    FileFormatList.UnregisterFileFormat('jpg', nil);
    FileFormatList.UnregisterFileFormat('jpe', nil);
    FileFormatList.UnregisterFileFormat('jpeg', nil);
  end;
  InitgexJpeg;
end;

procedure TgexJpegImageTests.TearDown;
begin
  Graphic.Free;
end;

procedure TgexJpegImageTests.TestCanLoad;
var GraphicClass: TGraphicExGraphicClass;
begin
  GraphicClass := FileFormatList.GraphicFromContent(Test1);
  Check(GraphicClass = TgexJpegImage, Format('CanLoad didn''t recognize %s as a jpeg image!',[Test1]));
  GraphicClass := FileFormatList.GraphicFromContent(Test2);

  CheckFalse(GraphicClass = TgexJpegImage, Format('CanLoad wrongly recognized %s as a jpeg image!',[Test2]));
  GraphicClass := FileFormatList.GraphicFromContent(Test3);
  Check(GraphicClass = TgexJpegImage, Format('CanLoad didn''t recognize %s as a jpeg image!',[Test3]));
end;

procedure TgexJpegImageTests.TestReadImageProperties;
begin
  // Create correct graphic image format
  Graphic := TgexJpegImage.Create;
  // On purpose no try finally since we are testing
  Check( Graphic <> nil, Format('Failed to create Graphic for %s!', [Test1]));

  // Read image properties
  Check(TGraphicExGraphic(Graphic).ReadImageProperties(Test1,0),
    Format('Failed to read the image properties for %s', [Test1]));
  // Test if the image properties have the expected values
  Check(TGraphicExGraphic(Graphic).ImageProperties.Width = 160,
    Format('Wrong width for %s', [Test1]));
  Check(TGraphicExGraphic(Graphic).ImageProperties.Height = 160,
    Format('Wrong height for %s', [Test1]));
  Check(TGraphicExGraphic(Graphic).ImageProperties.BitsPerPixel = 24,
    Format('Wrong BitsPerPixel (%d instead of 24) for %s',
    [TGraphicExGraphic(Graphic).ImageProperties.BitsPerPixel, Test1]));
end;

procedure TgexJpegImageTests.TestLoadImage;
var AGraphic: TgexJpegImage;
begin
  // Create correct graphic image format
  AGraphic := TgexJpegImage.Create;
  // On purpose no try finally since we are testing
  Check( AGraphic <> nil, Format('Failed to create Graphic for %s!', [Test1]));

  // Load image 1
  TGraphicExGraphic(AGraphic).LoadFromFileByIndex(Test1, 0);
  Check(AGraphic.Empty = False, Format('Failed to load image %s!', [Test1]));

  // Load image 3
  TGraphicExGraphic(AGraphic).LoadFromFileByIndex(Test1, 0);
  Check(AGraphic.Empty = False, Format('Failed to load image %s!', [Test3]));
  AGraphic.Free;
end;

initialization
if not FileExists(Test1) or not FileExists(Test2) or not FileExists(Test3) then
  MessageBox(0, 'At least one of the test images does not exist!'#13#10+
    'Please change the hardcoded test files to something valid for your system.',
    'Jpeg Tests', mb_iconhand + mb_ok);
  RegisterTest(TgexJpegImageTests{$IFNDEF FPC}.Suite{$ENDIF});
end.

