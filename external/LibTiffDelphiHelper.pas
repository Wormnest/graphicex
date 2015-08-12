{ jb Source of this unit found on the libtiff mailing list:
  http://www.asmail.be/msg0055535082.html
  Afterwards slightly adapted.
}
unit LibTiffDelphiHelper;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  LibTiffDelphi;


// Reading TIF into bitmap and image
function ReadTIFFFileIntoBitmap(Filename: String): TBitmap;
procedure VisualizeTIFFFileInImage(Filename: String; Image: TImage);

// Writing bitmap to TIFF
procedure WriteBitmapToTiff(Bitmap: TBitmap; Filename: String);

// Low level
procedure TIFFReadRGBAImageSwapRB(Width,Height: Cardinal; Memory: Pointer);

implementation

procedure TIFFReadRGBAImageSwapRB(Width,Height: Cardinal; Memory: Pointer);
var
  m: PCardinal;
  n: Cardinal;
  o: Cardinal;
begin
  m:=Memory;
  for n:=0 to Width*Height-1 do
  begin
    o:=m^;
    m^:= (o and $FF00FF00) or                {G and A}
        ((o and $00FF0000) shr 16) or        {B}
        ((o and $000000FF) shl 16);          {R}
    Inc(m);
  end;
end;

function ReadTIFFFileIntoBitmap(Filename: String): TBitmap;
var
  OpenTiff: PTIFF;
  FirstPageWidth,FirstPageHeight: Cardinal;
  FirstPageBitmap: TBitmap;
begin
  OpenTiff:=TIFFOpen(Filename,'r');
  if OpenTiff=nil then raise Exception.Create(
           'Unable to open file '''+Filename+'''');
  TIFFGetField(OpenTiff,TIFFTAG_IMAGEWIDTH,@FirstPageWidth);
  TIFFGetField(OpenTiff,TIFFTAG_IMAGELENGTH,@FirstPageHeight);
  FirstPageBitmap:=nil;
  try
    FirstPageBitmap:=TBitmap.Create;
    FirstPageBitmap.PixelFormat:=pf32bit;
    FirstPageBitmap.Width:=FirstPageWidth;
    FirstPageBitmap.Height:=FirstPageHeight;
  except
    if FirstPageBitmap<>nil then FirstPageBitmap.Destroy;
    TIFFClose(OpenTiff);
    raise Exception.Create('Unable to create TBitmap buffer');
  end;
  TIFFReadRGBAImage(OpenTiff,FirstPageWidth,FirstPageHeight,
               FirstPageBitmap.Scanline[FirstPageHeight-1],0);
  TIFFClose(OpenTiff);
  TIFFReadRGBAImageSwapRB(FirstPageWidth,FirstPageheight,
               FirstPageBitmap.Scanline[FirstPageHeight-1]);
  Result:=FirstPageBitmap;
end;

procedure VisualizeTIFFFileInImage(Filename: String; Image: TImage);
var
  m: TBitmap;
begin
  try
    m:=ReadTIFFFileIntoBitmap(Filename);
    Image.Picture.Assign(m);
    m.Destroy;
  except
    Image.Picture.Assign(nil);
  end;
end;

procedure WriteBitmapToTiff(Bitmap: TBitmap; Filename: String);
var
  OpenTiff: PTIFF;
  RowsPerStrip: Longword;
  StripMemory: Pointer;
  StripIndex: Longword;
  StripRowOffset: Longword;
  StripRowCount: Longword;
  ma,mb: PByte;
  nx,ny: Longword;
begin
  if (Bitmap.PixelFormat<>pf24bit) and
     (Bitmap.PixelFormat<>pf32bit) then
    raise Exception.Create('WriteBitmapToTiff is designed for 24bit and 32bit bitmaps only');
  RowsPerStrip:=((256*1024) div (Bitmap.Width*3));
  if RowsPerStrip>Bitmap.Height then
    RowsPerStrip:=Bitmap.Height
  else if RowsPerStrip=0 then
    RowsPerStrip:=1;
  StripMemory:=GetMemory(RowsPerStrip*Bitmap.Width*3);
  OpenTiff:=TIFFOpen(PAnsiChar(Filename),'w');
  if OpenTiff=nil then
  begin
    FreeMemory(StripMemory);
    raise Exception.Create('Unable to create file '''+Filename+'''');
  end;
  TIFFSetField(OpenTiff,TIFFTAG_IMAGEWIDTH,Bitmap.Width);
  TIFFSetField(OpenTiff,TIFFTAG_IMAGELENGTH,Bitmap.Height);
  TIFFSetField(OpenTiff,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
  TIFFSetField(OpenTiff,TIFFTAG_SAMPLESPERPIXEL,3);
  TIFFSetField(OpenTiff,TIFFTAG_BITSPERSAMPLE,8);
  TIFFSetField(OpenTiff,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
  TIFFSetField(OpenTiff,TIFFTAG_COMPRESSION,COMPRESSION_LZW);
  TIFFSetField(OpenTiff,TIFFTAG_PREDICTOR,2);
  TIFFSetField(OpenTiff,TIFFTAG_ROWSPERSTRIP,RowsPerStrip);
  StripIndex:=0;
  StripRowOffset:=0;
  while StripRowOffset<Bitmap.Height do
  begin
    StripRowCount:=RowsPerStrip;
    if StripRowCount>Bitmap.Height-StripRowOffset then
      StripRowCount:=Bitmap.Height-StripRowOffset;
    if Bitmap.PixelFormat=pf24bit then
    begin
      mb:=StripMemory;
      for ny:=StripRowOffset to StripRowOffset+StripRowCount-1 do
      begin
        ma:=Bitmap.ScanLine[ny];
        for nx:=0 to Bitmap.Width-1 do
        begin
          mb^:=PByte(NativeUInt(ma)+2)^;
          Inc(mb);
          mb^:=PByte(NativeUInt(ma)+1)^;
          Inc(mb);
          mb^:=PByte(NativeUInt(ma)+0)^;
          Inc(mb);
          Inc(ma,3);
        end;
      end;
    end
    else
    begin
      mb:=StripMemory;
      for ny:=StripRowOffset to StripRowOffset+StripRowCount-1 do
      begin
        ma:=Bitmap.ScanLine[ny];
        for nx:=0 to Bitmap.Width-1 do
        begin
          mb^:=PByte(NativeUInt(ma)+2)^;
          Inc(mb);
          mb^:=PByte(NativeUInt(ma)+1)^;
          Inc(mb);
          mb^:=PByte(NativeUInt(ma)+0)^;
          Inc(mb);
          Inc(ma,4);
        end;
      end;
    end;
    if TIFFWriteEncodedStrip(OpenTiff,StripIndex,
        StripMemory,StripRowCount*Bitmap.Width*3)=0 then
    begin
      TIFFClose(OpenTiff);
      FreeMemory(StripMemory);
      raise Exception.Create('Failed to write '''+Filename+'''');
    end;
    Inc(StripIndex);
    Inc(StripRowOffset,StripRowCount);
  end;
  TIFFClose(OpenTiff);
  FreeMem(StripMemory);
end;

end.
