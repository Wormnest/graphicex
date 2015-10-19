unit DU_ImageReader_Tests;

interface

uses
  Windows,
  GraphicEx,
  gexBmpWrapper,
  //gexJpegWrapper,
  gexJpeg,
  gexXCF,
  gexMayaIff,
  gexAmigaIff,
  Graphics,
  {$IFNDEF FPC}
  TestFramework,
  pngimage, // PNG loading
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

    {$IFNDEF FPC}
    function CompareImages(TestBitmap: TBitmap; ReferenceBitmap: TBitmap): Boolean;
    {$ELSE}
    function CompareImages(TestBitmap: TFPImageBitmap; ReferenceBitmap: TFPImageBitmap): Boolean;
    {$ENDIF}
    procedure CheckImagePixels;
    function DetermineImageFormat(AImage: string): TGraphicExGraphicClass;
    procedure ExpectExceptionReadingImage;
    procedure DoTestReadImage;
  published
    procedure TestReadImage;
  end;

implementation

uses SysUtils, Classes, Forms,
  {$IFNDEF FPC}
  FastMM4,  // Get memory used
  gexTypes, // NativeUInt
  {$ENDIF}
  {$IFDEF HEAPTRC_LOG}
  heaptrc,
  {$ENDIF}
  GraphicStrings;

const
  ImagesBasePath = 'E:\Delphi\Projects\Transcript\test-images\';
  XmlConfig_FileName = 'unit-tests.xml';

var
  BrushesLoaded: Boolean = False;

{$IFNDEF FPC}
function TImageReadingTests.CompareImages(TestBitmap: TBitmap; ReferenceBitmap: TBitmap): Boolean;
{$ELSE}
function TImageReadingTests.CompareImages(TestBitmap: TFPImageBitmap; ReferenceBitmap: TFPImageBitmap): Boolean;
{$ENDIF}
var
 i, j        : Integer;
 ScanBytes   : Integer;
 CompareResult:Integer;
 Testspp,
 Refspp: Integer;

 function HexBytes(Memory: PAnsiChar; Ofs: Integer; Count: Integer; spp: Byte; SkipCount: Byte = 0): string;
 var i: Integer;
 begin
   Result := '';
   Memory := Memory + Ofs;
   if (SkipCount = 0) then
     for i := 0 to Count-1 do begin
       if i mod spp = 0 then
         Result := Result + '(';
       Result := Result + IntToHex(PByte(Memory)^, 2) + ' ';
       if i mod spp = spp-1 then
         Result := Result + ') ';
       Inc(Memory);
     end
   else begin // Skipcount > 0
     i := 0; j := 0;
     while i < Count do begin
       if (j <> spp) then begin
         if j mod spp = 0 then
           Result := Result + '(';
         Result := Result + IntToHex(PByte(Memory)^, 2) + ' ';
         if j mod spp = spp-1 then
           Result := Result + ') ';
         Inc(Memory);
         Inc(i);
         Inc(j);
       end
       else begin
         // Skip some bytes. Needed for Lazarus where the actual PixelFormat of
         // a ScanLine isn't always the same as what we expect from PixelFormat.
         Inc(Memory, SkipCount);
         j := 0;
       end;
     end
   end;
 end;
 function ComparePixels(ATest, ARef: PByte; BytesPerPixelTest, BytesPerPixelRef, Count: Integer): Integer;
 var CurPos: Integer;
     SkipBytes: Integer;
 begin
   Result := 0;
   CurPos := 0;
   SkipBytes := BytesPerPixelRef - BytesPerPixelTest;
   while CurPos < Count do begin
     if ATest^ <> ARef^ then begin
       Result := CurPos;
       break;
     end;
     Inc(CurPos);
     Inc(ATest);
     Inc(ARef);
     if CurPos mod BytesPerPixelTest = 0 then
       Inc(ARef, SkipBytes);
   end;
 end;

begin
  Check(TestBitmap <> nil, 'TestBitmap is nil!');
  Result := (TestBitmap <> nil) and (ReferenceBitmap <> nil);
  if not Result then exit;

  Check(TestBitmap.Width = ReferenceBitmap.Width,
    Format('TestBitmap width: %d, expected: %d',
    [TestBitmap.Width, ReferenceBitmap.Width]));
  Check(TestBitmap.Height = ReferenceBitmap.Height,
    Format('TestBitmap height: %d, expected: %d',
    [TestBitmap.Height, ReferenceBitmap.Height]));
  Check(TestBitmap.PixelFormat = ReferenceBitmap.PixelFormat,
    'TestBitmap pixelformat not the same as reference image!');
  Result := (TestBitmap.Width = ReferenceBitmap.Width) and
            (TestBitmap.Height = ReferenceBitmap.Height) and
            (TestBitmap.PixelFormat = ReferenceBitmap.PixelFormat);

  if not Result then exit;

  {$IFDEF FPC}
  // In Lazarus the actual PixelFormat of a ScanLine after loading a PNG
  // seems to be pf32Bit even if the PixelFormat returns pf24Bit.
  Refspp := 4;
  {$ENDIF}
  case TestBitmap.PixelFormat of
    pf32Bit: Testspp := 4;
    pf24Bit: Testspp := 3;
  else
    Testspp := 1;
    {$IFDEF FPC}
    Refspp := 1;
    {$ENDIF}
  end;
  {$IFNDEF FPC}
  Refspp := Testspp;
  {$ENDIF}

  {$IFDEF FPC}
  TestBitmap.BeginUpdate(False);
  ReferenceBitmap.BeginUpdate(False);
  {$ENDIF}
  try
    ScanBytes := TestBitmap.Width * TestSpp;
    for i := 0 to TestBitmap.Height-1 do
    Begin
      CompareResult := ComparePixels(TestBitmap.ScanLine[i], ReferenceBitmap.ScanLine[i], Testspp, Refspp, ScanBytes);
      Check(CompareResult = 0,
        Format('TestBitmap (%d x %d) not the same as reference image at scanline %d. Difference at pixel %d.' +
               ' Test Image: %s, Reference: %s' ,
               [TestBitmap.Width, TestBitmap.Height, i, Abs(CompareResult) div Testspp,
               HexBytes(TestBitmap.Scanline[i], Abs(CompareResult) div Testspp * TestSpp, 5*Testspp, Testspp),
               HexBytes(ReferenceBitmap.Scanline[i], Abs(CompareResult) div TestSpp * Refspp, 5*Testspp, Testspp, RefSpp-TestSpp)]));
      if not Result then
        exit;
    End;
  finally
    {$IFDEF FPC}
    TestBitmap.EndUpdate(False);
    ReferenceBitmap.EndUpdate(False);
    {$ENDIF}
  end;
end;

procedure TImageReadingTests.CheckImagePixels;
var
  {$IFDEF FPC}
  refimg: TPortableNetworkGraphic;
  {$ELSE}
  refpng: TPngObject;
  refimg: TBitmap;
  {$ENDIF}
begin
  // First load our reference image
  {$IFDEF FPC}
  refimg := TPortableNetworkGraphic.Create;
  {$ELSE}
  refpng := TPngObject.Create;
  refimg := TBitmap.Create;
  {$ENDIF}
  try
  {$IFDEF FPC}
    refimg.LoadFromFile(CompareFileName);
  {$ELSE}
    refpng.LoadFromFile(CompareFileName);
    refpng.AssignTo(refimg);
  {$ENDIF}

    // Our test image can be found in Graphic
    CompareImages({$IFDEF FPC}TFPImageBitmap{$ELSE}TBitmap{$ENDIF}(Graphic), refimg);
  finally
  {$IFNDEF FPC}
    refpng.Free;
  {$ENDIF}
    refimg.Free;
  end;
end;

procedure TImageReadingTests.SetUp;
var gc: TGraphicClass;
  i: TBitmap;
begin
  // The FileFormat tests may have left the FileFormat list without our bmp wrapper class.
  // Thus make sure it gets initialized to our bmp wrapper class.
  gc := FileFormatList.GraphicFromExtension('.bmp');
  if (FileFormatList.GraphicFromExtension('bmp') = nil) or
     (gc <> TgexBmpGraphic) then begin
    FileFormatList.UnregisterFileFormat('bmp', Graphics.TBitmap);
    FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TgexBmpGraphic);
  end;

  if not BrushesLoaded and (LowerCase(ExtractFileExt(TestFileName)) = '.png') then
  begin
    // Try to remove false positives by selecting brushes that the PNG loading
    // later on may use too thereby causing them to be already loaded in cache.
    i := TBitmap.Create;
    i.Width := 1; i.Height := 1;
    try
      // TODO: Is there a better (faster) way than FillRect to make sure that
      // the brush gets loaded into cache?
      i.Canvas.Brush.Color := TColor($000000); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($ffffff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($fefefe); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00ffff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000ff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00a0ff); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00ff00); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($ff0000); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($80e0e0); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($999999); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000aa); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00000a); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000f5); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000ad); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($0000c2); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($00001f); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      i.Canvas.Brush.Color := TColor($000013); i.Canvas.FillRect(Rect(0, 0, 1, 1));
      BrushesLoaded := True;
    finally
      i.Free;
    end;
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

procedure TImageReadingTests.DoTestReadImage;
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
    if Comparison then
      CheckImagePixels;
  end
  else
    AssertException('We didn''t get the exception we expected!', ExceptionType,
      {$IFDEF FPC}@{$ENDIF}ExpectExceptionReadingImage, ExceptionMessage);

  Graphic.Free;
  Graphic := nil;
end;

{$IFNDEF FPC}
// Adapted from: http://stackoverflow.com/questions/437683/how-to-get-the-memory-used-by-a-delphi-program
function MemoryUsed: NativeUInt;
var
    st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
    i: Integer;
begin
    GetMemoryManagerState(st);
    result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
    for i := 0 to NumSmallBlockTypes - 1 do begin
      sb := st.SmallBlockTypeStates[i];
      result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
    end;
end;
{$ENDIF}

procedure TImageReadingTests.TestReadImage;
var
{$IFDEF FPC}
  fpcHeapStatus : TFPCHeapStatus;
  UsedHeap, FinalUsedHeap: PtrUInt;
{$ELSE}
  UsedHeap, FinalUsedHeap: NativeUInt;
{$ENDIF}
begin
  {$IFDEF FPC}
  // Get initial heap state
  fpcHeapStatus := GetFPCHeapStatus();
  UsedHeap := fpcHeapStatus.CurrHeapUsed;
  {$ELSE}
  //FailsOnMemoryLeak := True; // Disabled: too many false positives
  UsedHeap := MemoryUsed();
  {$ENDIF}

  DoTestReadImage;

  {$IFDEF FPC}
  // Get heap state after tests are done
  fpcHeapStatus := GetFPCHeapStatus();
  FinalUsedHeap := fpcHeapStatus.CurrHeapUsed;
  {$IFDEF HEAPTRC_LOG}
  if UsedHeap <> FinalUsedHeap then
    DumpHeap;
  {$ENDIF}
  {$ELSE}
  FinalUsedHeap := MemoryUsed();
  {$ENDIF}

  // Check if there was a memory leak
  // Note that we currently have some known false positives
  // In Fpc:
  // When reading PNG images a canvas may be used. When a new type of brush is
  // used that brush is cached causing allocation of memory that will not be
  // freed at this point. It will be freed when we close our program.
  // Delphi: many leaks which we assume to be (at least) mostly false positives.
  // Needs more research to find the cause, so for now it's best to disable.
  // Doing a test again manually often removes the leak.

  {.$DEFINE ENABLE_LEAKCHECK}

  {$IF Defined(FPC) OR Defined(ENABLE_LEAKCHECK)}
  Check(UsedHeap = FinalUsedHeap, Format('Memory leak detected: %u bytes',[FinalUsedHeap - UsedHeap]));
  {$IFEND}
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
  if not DirectoryExists(ImagesBasePath) then
    MessageBox(0, 'Path to test images does not exist!'#13#10+
      'Please set ImagesBasePath to a correct location with test images.'#13#10+
      'Beware that you also need to initialize the unit-tests.xml files in each image folder!',
      'ImageReader Tests', mb_iconhand + mb_ok);
  ImageReadingTestsClass := TImageReadingTests;
  AddImageReaderTests('Test Loading Image Formats', ImagesBasePath,
    XmlConfig_FileName, ImageReadingTestsClass);
end.
