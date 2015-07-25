unit DU_ImageReader_Tests;

interface

uses
  GraphicEx,
  gexBmpWrapper,
  gexJpegWrapper,
  gexXCF,
  Graphics,
  {$IFNDEF FPC}
  TestFramework,
  {$ELSE}
  fpcunit,
  {$ENDIF}
  TestFrameworkXmlConfig;

type
  TImageReadingTestsClass = class of TImageReadingTests;
  TImageReadingTests = class(TImageTestCase)
  private
    Graphic: TGraphic;
  public
    {$IFNDEF FPC}
    class function Suite: ITestSuite; override;
    {$ELSE}
    class function Suite: TTestSuite; virtual;
    {$ENDIF}

    procedure SetUp; override;
    procedure TearDown; override;

    function DetermineImageFormat(AImage: string): TGraphicExGraphicClass;
    procedure ExpectExceptionReadingImage;
  published
    procedure TestReadImage;
  end;

implementation

uses SysUtils,
  GraphicStrings;

const
  ImagesBasePath = 'E:\Delphi\Projects\Transcript\test-images\';
  XmlConfig_FileName = 'unit-tests.xml';

procedure TImageReadingTests.SetUp;
var gc: TGraphicClass;
begin
  // The FileFormat tests may have left the FileFormat list without our bmp wrapper class.
  // Thus make sure it gets initialized to our bmp wrapper class.
  gc := FileFormatList.GraphicFromExtension('.bmp');
  if (FileFormatList.GraphicFromExtension('bmp') = nil) or
     (gc <> TgexBmpGraphic) then begin
    FileFormatList.UnregisterFileFormat('bmp', Graphics.TBitmap);
    FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TgexBmpGraphic);
  end;
end;

procedure TImageReadingTests.TearDown;
begin
  // Make sure Graphic gets freed even if test fails.
  Graphic.Free;
end;

function TImageReadingTests.DetermineImageFormat(AImage: string): TGraphicExGraphicClass;
var GraphicClass: TGraphicExGraphicClass;
  SecondCaseClass: TGraphicClass;
  Ext: string;
begin
  GraphicClass := FileFormatList.GraphicFromContent(AImage);
  if GraphicClass = nil then
  begin
    // Some formats (e.g. Dr. Halo CUT images) cannot be determined from content.
    // Try to guess based on extension.
    // Problem with this is, that above we may have seen an image with our
    // image extension but determined in GraphicFromContent that it's not an
    // image (sub)format we recognize. This image will then be recognized here again
    // even though we won't be able to use it.
    // Since currently the only GraphicEx format we have that can't be determined
    // from content is CUT, we explicitly check for that extension
    Ext := LowerCase(ExtractFileExt(AImage));
    if Ext = '.cut' then begin
      SecondCaseClass := FileFormatList.GraphicFromExtension(AImage);
      if (SecondCaseClass <> nil) and SecondCaseClass.InheritsFrom(TGraphicExGraphic) then
        GraphicClass := TGraphicExGraphicClass(SecondCaseClass);
    end;
  end;
  Result := GraphicClass;
end;

procedure TImageReadingTests.ExpectExceptionReadingImage;
begin
  // Load image for which we are expecting an exception while trying to read it...
  TGraphicExGraphic(Graphic).LoadFromFileByIndex(TestFileName, TestPage);
end;

procedure TImageReadingTests.TestReadImage;
var GraphicClass: TGraphicExGraphicClass;
begin
  // Determine type of image
  GraphicClass := DetermineImageFormat(TestFileName);
  if not Unrecognized then
    Check( GraphicClass <> nil, Format('Failed to determine image format of %s!', [TestFileName]))
  else begin
    // should not be recognized as an image
    Check( GraphicClass = nil, Format('%s should not be recognized as an image!', [TestFileName]));
    Exit;
  end;

  // Create correct graphic image format
  Graphic := GraphicClass.Create;
  // On purpose no try finally since we are testing
  Check( Graphic <> nil, Format('Failed to create Graphic for %s!', [TestFileName]));

  if Readable then begin
    // Load image
    TGraphicExGraphic(Graphic).LoadFromFileByIndex(TestFileName, TestPage);
    if not EmptyImage then
      Check(Graphic.Empty = False, Format('Failed to load image %s!', [TestFileName]))
    else
      Check(Graphic.Empty = True, Format('%s is unexpectedly not empty!', [TestFileName]));
  end
  else
    AssertException('We didn''t get the exception we expected!', ExceptionType,
      {$IFDEF FPC}@{$ENDIF}ExpectExceptionReadingImage, ExceptionMessage);

  Graphic.Free;
  Graphic := nil;
end;

{$IFNDEF FPC}
class function TImageReadingTests.Suite: ITestSuite;
{$ELSE}
class function TImageReadingTests.Suite: TTestSuite;
{$ENDIF}
begin
  Result := TImageReaderTestSuite.Create(self, ImagesBasePath, XmlConfig_FileName, False);
end;


var
  ImageReadingTestsClass: TImageReadingTestsClass;

initialization
  ImageReadingTestsClass := TImageReadingTests;
  AddImageReaderTests('Test Loading Image Formats', ImagesBasePath,
    XmlConfig_FileName, ImageReadingTestsClass);
end.
