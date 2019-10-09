{ gexJpegWrapper A GraphicEx Wrapper around the jpeg unit.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}

unit gexJpegWrapper;

interface

{$I gexdefines.inc}

{$I GraphicConfiguration.inc}

uses Classes,
     {$IFNDEF FPC}
     jpeg,
     {$ELSE}
     Graphics, FPReadJPEG, FPImage, IntfGraphics,
     {$ENDIF}
     GraphicEx;

type
  {$IFDEF FPC}
  // This overrides the fpc default TJpegImage with some enhancements we need.
  TJpegImage = class(Graphics.TJpegImage)
  private
    FScale: TJPEGScale;
    {FIsCMYK: Boolean;
    FAdobeMarkerSeen: Boolean;
    FJpegColorSpace: Integer;}
    // todo: fpc TJpegImage does not take into account the adobe flag (app 14 marker)
    //       If this is on cmyk, cyyk values should be inverted!
  protected
    procedure InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader); override;
    procedure FinalizeReader(AReader: TFPCustomImageReader); override;
  public
    constructor Create; override;
    property Scale: TJPEGScale read FScale write FScale;
    {property AdobeMarkerSeen: Boolean read FAdobeMarkerSeen;
    property IsCMYK: Boolean read FIsCMYK;
    property JpegColorSpace: Integer read FJpegColorSpace;}
  end;
  {$ENDIF}

  // A GraphicEx Wrapper class for jpeg
  // We should also override the other Load ByIndex procedures! However we can't
  // directly use the others. We  probably need to make a temporary intermediate stream.
  // For now we will just ignore that since those are not use a lot.
  TgexJpegGraphic = class(TGraphicExGraphic)
  protected
    procedure AssignJpegToBitmap(JpegImage: TJpegImage);
    procedure CopyJpegProperties(JpegImage: TJpegImage);
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0); override;
    procedure LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;


implementation

uses {$IFNDEF FPC}Graphics,{$ENDIF}
     {$IFDEF FPC}
     JPEGLib,
     {$ENDIF}
     GraphicStrings, GraphicColor;

const cJpegSOIMarker = $d8ff;

type
     TShortFileHeader = record
       case byte of
         0: (Word1, Word2: Word;);
         1: (LongWord1: LongWord;);
         2: (string5: array [0..4] of char);
     end;
     PShortFileHeader = ^TShortFileHeader;

{$IFDEF FPC}
//type
  // We need access to private field of TFPReaderJPEG.
  // ugh fpc also doesn't allow acces to private fields using a Hack!
  {TJpegReaderHack = class(TFPReaderJPEG)
  end;}

  // Seems fpc doesn't allow access to private although Delphi does.
  // Thus we have to keep on using the hack method.
  {TJpegReaderHelper = class helper for TFPReaderJPEG
  private
    function GetInfo: jpeg_decompress_struct;
  protected
    property JpegInfo: jpeg_decompress_struct read GetInfo;
  end;

function TJpegReaderHelper.GetInfo: jpeg_decompress_struct;
begin
  Result := TFPReaderJPEG(Self).FInfo;
end;}

constructor TJpegImage.Create;
begin
  inherited Create;
  FScale := jsFullSize;
  {FIsCMYK := False;
  FAdobeMarkerSeen := False;
  FJpegColorSpace := 0;}
end;

procedure TJpegImage.InitializeReader(AImage: TLazIntfImage; AReader: TFPCustomImageReader);
begin
  inherited InitializeReader(AImage, AReader);
  TFPReaderJPEG(AReader).Scale := FScale;
end;

procedure TJpegImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  //FAdobeMarkerSeen := TJpegReaderHack(AReader).FInfo.saw_Adobe_marker;
  //FIsCMYK := TJpegReaderHack(AReader).FInfo.jpeg_color_space in [JCS_CMYK, JCS_YCCK];
  //FJpegColorSpace := Byte(TJpegReaderHack(AReader).FInfo.jpeg_color_space);
  inherited FinalizeReader(AReader);
end;

{$ENDIF}

class function TgexJpegGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
begin
  // TODO: Find a realistic minimum size for a jpeg image
  if Size > SizeOf(TShortFileHeader) then
    Result := PShortFileHeader(Memory)^.Word1 = cJpegSOIMarker
  else
    Result := False;
end;

procedure TgexJpegGraphic.CopyJpegProperties(JpegImage: TJpegImage);
begin
  // Add some basic info to FImageProperties
  case PixelFormat of
    {$IFNDEF FPC}
    pf24Bit:
    {$ELSE} // fpc uses 32bits for jpegs
    pf32Bit:
    {$ENDIF}
      begin
        FImageProperties.BitsPerPixel := 32;
        FImageProperties.SamplesPerPixel := 4;
        FImageProperties.ColorScheme := csBGR;
      end;
  else // 8 bit
    FImageProperties.BitsPerPixel := 8;
    FImageProperties.SamplesPerPixel := 1;
    FImageProperties.ColorScheme := csG;
  end;
  FImageProperties.BitsPerSample := FImageProperties.BitsPerPixel div FImageProperties.SamplesPerPixel;
  FImageProperties.Compression := ctJPEG;
  FImageProperties.Width := JpegImage.Width;
  FImageProperties.Height := JpegImage.Height;
  FImageProperties.FileGamma := 1;
end;

procedure TgexJpegGraphic.AssignJpegToBitmap(JpegImage: TJpegImage);
begin
  Self.Assign(JpegImage);

  // For CMYK jpeg's (as implemented by Gabriel Corneanu, http://cc.embarcadero.com/Item/19723)
  // we apparently need to explicitly set PixelFormat to pf24Bit since assigning
  // as is results in Pf32Bit PixelFormat.
  // Since we can't be sure that this jpeg version is used we can't use IsCMYK.
  {$IFNDEF FPC}
  if PixelFormat = pf32Bit then
    PixelFormat := pf24Bit;
  {$ELSE}
  // Fpc apparently sets jpegs (except grayscales?, not tested) to 32 bits
  // Setting it to 24bits renders an all black output
  {$ENDIF}
end;

procedure TgexJpegGraphic.LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);
var
  jpgimg: TJpegImage;
begin
  // Since this is just a wrapper we call TJpegImage and let it load the image.
  jpgImg := TJpegImage.Create();
  try
    jpgImg.LoadFromFile(FileName);
    AssignJpegToBitmap(jpgImg);
    CopyJpegProperties(jpgImg);
  finally
    jpgImg.Free;
  end;
end;

procedure TgexJpegGraphic.LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0);
var
  jpgimg: TJpegImage;
begin
  // Since this is just a wrapper we call TJpegImage and let it load the image.
  jpgImg := TJpegImage.Create();
  try
    jpgImg.LoadFromStream(Stream);
    AssignJpegToBitmap(jpgImg);
    CopyJpegProperties(jpgImg);
  finally
    jpgImg.Free;
  end;
end;

function TgexJpegGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;
begin
  // Can only be done after loading image
  Result := FImageProperties.Compression = ctJPEG;
end;

initialization
  // Unregister TJpegImage first (both will just ignore it if TJpegImage isn't registered)
  TPicture.UnregisterGraphicClass(TJpegImage);
  if FileFormatList = nil then
    Exit;
  FileFormatList.UnregisterFileFormat('', {$IFDEF FPC}Graphics.{$ENDIF}TJpegImage);
  // Register Jpeg with our class
  if FileFormatList.GraphicFromExtension('jpg') <> nil then
    Exit; // Something else has already registered jpg
  FileFormatList.RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, TgexJpegGraphic);
end.
