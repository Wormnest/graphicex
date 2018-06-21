unit DU_JpegTests;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Graphics,
  {$IFNDEF FPC}
  TestFramework,
  {$ELSE}
  fpcunit, testregistry,
  {$ENDIF}
  GraphicColor
  ;

type

  TgexJpegImageTests = class(TTestCase)
  public
    Graphic: TGraphic;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestBmp2Jpeg(ABmpName: string);
  published
    procedure TestCanLoad;
    procedure TestReadImageProperties;
    procedure TestLoadImage;
    procedure TestSaveImage;
    procedure TestSaveFromBmp;
  end;

implementation

uses GraphicEx, gexJpeg, TestSettings;

var
  BmpPath,
  BmpImg,
  JpegSavePath,
  Test1,
  Test2,
  Test3: string; // Location of test images.

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

procedure TgexJpegImageTests.TestBmp2Jpeg(ABmpName: string);
var
  ABitmap: TBitmap;
  AGraphic: TgexJpegImage;
begin
  ABitmap := TBitmap.Create;
  AGraphic := TgexJpegImage.Create;
  BmpImg := BmpPath + ABmpName;
  ABitmap.LoadFromFile(BmpImg);
  AGraphic.Assign(ABitmap);
  AGraphic.SaveToFile(JpegSavePath + ABmpName + '.jpg');
  AGraphic.Free;
  ABitmap.Free
end;

procedure TgexJpegImageTests.TestSaveImage;
var AGraphic: TgexJpegImage;
  ABitmap: TBitmap;
  w,h: Integer;
  p: PRGB;
begin
  // Create correct graphic image format
  AGraphic := TgexJpegImage.Create;
  ABitmap := TBitmap.Create;
  with ABitmap do begin
    PixelFormat := pf24Bit;
    Width := 64;
    Height := 64;
    for h := 0 to Height-1 do begin
      p := ABitmap.ScanLine[h];
      for w := 0 to Width-1 do begin
        p^.R := Byte(4*h-1); // Cast to Byte so we don't get a range check error.
        p^.G := 2*w;
        p^.B := w*h div 32;
        Inc(p);
      end;
    end;
  end;

  AGraphic.Assign(ABitmap);
  AGraphic.SaveToFile(JpegSavePath + 'TEMP_GEXJPEG_SAVETOFILE_TEST.jpg');
  AGraphic.CompressionQuality := 25;
  AGraphic.SaveToFile(JpegSavePath + 'TEMP_GEXJPEG_compression_25_TEST.jpg');
  AGraphic.CompressionQuality := 50;
  AGraphic.SaveToFile(JpegSavePath + 'TEMP_GEXJPEG_compression_50_TEST.jpg');
  AGraphic.CompressionQuality := 75;
  AGraphic.SaveToFile(JpegSavePath + 'TEMP_GEXJPEG_compression_75_TEST.jpg');

  ABitmap.Free;
  AGraphic.Free;
end;

procedure TgexJpegImageTests.TestSaveFromBmp;
begin
  // Use some of the images from the bmpsuite.
  // Note this requires that you make a folder called temp below folder g.
  TestBmp2Jpeg('pal1.bmp');
  TestBmp2Jpeg('pal1bg.bmp');
  TestBmp2Jpeg('pal4.bmp');
  TestBmp2Jpeg('pal4gs.bmp');
  TestBmp2Jpeg('pal8.bmp');
  TestBmp2Jpeg('pal8gs.bmp');
  TestBmp2Jpeg('rgb16.bmp');
  TestBmp2Jpeg('rgb16bfdef.bmp');
  TestBmp2Jpeg('rgb24.bmp');
  TestBmp2Jpeg('rgb32.bmp');
  TestBmp2Jpeg('rgb32bf.bmp');
  TestBmp2Jpeg('rgb32bfdef.bmp');
end;

initialization
  // Set the location of our jpg test images.
  Test1 := ImagesBasePath + 'jpg\nuke-symbol.jpg';      // normal jpg
  Test2 := ImagesBasePath + 'jpg\unit-tests.xml';       // not a jpg
  Test3 := ImagesBasePath + 'jpg\wrong-extension.bmp';  // jpg with bmp extension
  BmpPath := ImagesBasePath + 'bmp\bmpsuite-2.5\g\';
  BmpImg := BmpPath + 'rgb24.bmp';
  JpegSavePath := TestDataPath + PathDelim;
  if not FileExists(Test1) or not FileExists(Test2) or not FileExists(Test3) or not FileExists(BmpImg) then
    MessageBox(0, 'At least one of the test images does not exist!'#13#10+
      'Please change the name of the basepath or the test files to something valid for your system.',
      'Jpeg Tests', mb_iconhand + mb_ok)
  else
    RegisterTest(TgexJpegImageTests{$IFNDEF FPC}.Suite{$ENDIF});
end.

