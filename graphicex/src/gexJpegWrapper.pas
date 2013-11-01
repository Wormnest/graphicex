{ gexJpegWrapper A GraphicEx Wrapper around the jpeg unit.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}

unit gexJpegWrapper;

interface

uses Classes, GraphicEx, jpeg;

type
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

uses Graphics, GraphicStrings, GraphicColor;

const cJpegSOIMarker = $d8ff;

type
     TShortFileHeader = record
       case byte of
         0: (Word1, Word2: Word;);
         1: (LongWord1: LongWord;);
         2: (string5: array [0..4] of char);
     end;
     PShortFileHeader = ^TShortFileHeader;

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
    pf24Bit:
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
  if PixelFormat = pf32Bit then
    PixelFormat := pf24Bit;
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
  FileFormatList.UnregisterFileFormat('', TJpegImage);
  // Register Jpeg with our class
  FileFormatList.RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, TgexJpegGraphic);
  FileFormatList.RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, TgexJpegGraphic);
end.
