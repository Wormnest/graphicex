{ gexBmpWrapper A GraphicEx Wrapper around the bmp loading class.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2014 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
}

unit gexBmpWrapper;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses Classes, GraphicEx;

type
  // A GraphicEx wrapper class for bmp images.
  // We should also override the other Load ByIndex procedures! However we can't
  // directly use the others. We  probably need to make a temporary intermediate stream.
  // For now we will just ignore that since those are not used a lot.
  TgexBmpGraphic = class(TGraphicExGraphic)
  protected
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0); override;
    procedure LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;


implementation

uses Graphics, Windows, GraphicStrings, GraphicColor;

const cBmpMarker = $4d42; // 'BM'


class function TgexBmpGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
begin
  // TODO: Find better minimal size of BMP
  // TODO: Also check for correct size in Size field
  if Size > SizeOf(TBitmapFileHeader) then
    Result := PBitmapFileHeader(Memory)^.bfType = cBmpMarker
  else
    Result := False;
end;

procedure TgexBmpGraphic.LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);
begin
  // Since TGraphicExGraphic is an extension of TBitmap (and TgexBmpGraphic is an
  // extension of TGraphicExGraphic) it suffices to call LoadFromFile directly.
  // However we can't use LoadFromFile because inherited GraphicEx LoadFromFile calls
  // LoadFromFileByIndex meaning we would get into an endless loop.
  // To fix that we added LoadBitmapFromFile and LoadBitmapFromStream in our
  // GraphicEx base class.
  LoadBitmapFromFile(FileName);
end;

procedure TgexBmpGraphic.LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0);
begin
  // Call TBitmap's LoadFromStream
  LoadBitmapFromStream(Stream);
end;

function TgexBmpGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;
var
  bmpInfoHeader: PBitmapInfoHeader;
begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
  begin
    // TODO: Unfinished!

    bmpInfoHeader := Pointer(PAnsiChar(Memory) + SizeOf(TBitmapFileHeader));
    FImageProperties.Width := bmpInfoHeader.biWidth;
    FImageProperties.Height := bmpInfoHeader.biHeight;
    FImageProperties.BitsPerPixel := bmpInfoHeader.biBitCount;

    case bmpInfoHeader.biCompression of
      BI_RLE8,
      BI_RLE4:
        begin
          FImageProperties.Compression := ctRLE;
          FImageProperties.ColorScheme := csIndexed;
        end;
      BI_BITFIELDS:
        begin
          // TODO: We currently have no way to identify this compression in GraphicEx
          FImageProperties.Compression := ctNone;
        end;
    else
      // BI_RGB:
      FImageProperties.Compression := ctNone;
      // TODO: BI_JPEG, BI_PNG
    end;

    if bmpInfoHeader.biBitCount > 8 then begin
      if bmpInfoHeader.biBitCount <> 16 then begin
        FImageProperties.BitsPerSample := FImageProperties.BitsPerPixel div 8;
        FImageProperties.SamplesPerPixel := FImageProperties.BitsPerPixel div FImageProperties.BitsPerSample;
      end
      else begin
        FImageProperties.SamplesPerPixel := 3;
        FImageProperties.BitsPerSample := 5;
        if bmpInfoHeader.biCompression = BI_BITFIELDS then begin
          // TODO: Need to determine if it's pf15bit or pf16bit
        end;
      end;
      if bmpInfoHeader.biBitCount = 32 then
        FImageProperties.ColorScheme := csBGRA
      else
        FImageProperties.ColorScheme := csBGR
    end
    else begin
      FImageProperties.BitsPerSample := bmpInfoHeader.biBitCount;
      FImageProperties.SamplesPerPixel := 1;
      FImageProperties.ColorScheme := csIndexed;
    end;
  end;
end;

initialization
  // Unregister TBitmap first (both will just ignore it if TBitmap isn't registered)
  // Qualifying TBitmap because Windows unit also defines TBitmap
  // However for some unknown reason unregistering bmp with TPicture here causes all
  // other GraphicEx registered extensions (like tif etc.) to become unregistered
  // with TPicture. For now we will accept bmp not being unregistered there since
  // it doesn't seem to cause any bad effects.
//  TPicture.UnregisterGraphicClass(Graphics.TBitmap);
  FileFormatList.UnregisterFileFormat('bmp', Graphics.TBitmap);
  // Register Bmp with our class
  FileFormatList.RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TgexBmpGraphic);
end.
