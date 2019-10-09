{ gexExif A simple Exif parser class for GraphicEx, mainly to extract Orientation.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2017-2017 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
  Mirror: https://github.com/Wormnest/graphicex
}
unit gexExif;

interface

{$I gexdefines.inc}

{$IFNDEF FPC}
  // Delphi
  {$I Compilers.inc}
{$ENDIF}

{$I GraphicConfiguration.inc}


type
  // Simple Exif reader/parser class. Currently only used to extract orientation.
  TSimpleExifReader = class
  private
    FCorruptExif: Boolean;
    FExif: PByte;
    FExifSize: Cardinal;
    FIntelByteOrder: Boolean;
    FOrientationOnly: Boolean;
    FOrientation: Byte;
    FParsingDone: Boolean;
    function GetOrientation(): Byte;
  protected
    procedure ParseExif;
  public
    // Memory: Location in memory of Exif data. Size: Size of Exif data.
    // Note: Exif data is not copied nor freed here. Memory should stay valid
    // as long as this reader exists!
    constructor Create(Memory: PByte; Size: Cardinal);
    destructor Destroy; override;

    property Orientation: Byte read GetOrientation;
    // Do we stop as soon as we found Orientation tag or do we read all tags.
    property OrientationOnly: Boolean read FOrientationOnly default True;
  end;


implementation

uses gexTypes, gexUtils, gexMemory;

type
  PTIFFHeader = ^TTIFFHeader;
  TTIFFHeader = packed record
    ByteOrder: Word;
    Version: Word;
    FirstIFD: Cardinal;
  end;

const
  // Copied from LibTiffDelphi.
  // TODO: Maybe extra const and type definitions from LibTiffDelphi into a
  // separate unit that can be used without pulling in libtiff itself.
  TIFFTAG_ORIENTATION = 274;

constructor TSimpleExifReader.Create(Memory: PByte; Size: Cardinal);
begin
  FExif := Memory;
  FExifSize := Size;
  FCorruptExif := False;
  FIntelByteOrder := True;
  FOrientationOnly := True;
  FOrientation := 0;
  FParsingDone := False;
end;

destructor TSimpleExifReader.Destroy;
begin
  inherited Destroy;
end;

procedure TSimpleExifReader.ParseExif;
var
  Mem: TMemoryAccess;
  TiffHeader: TTIFFHeader;
  TagCount: Word;
  TagID: Word;
  DataFormat: Word;
  //NumComponents: LongWord;
  Data: LongWord;
  NextIFD: LongWord;
  i: Integer;
  Stop: Boolean;
begin
  FParsingDone := False;
  FOrientation := 0;
  Mem := TMemoryAccess.Create(FExif, FExifSize, 'Exif');
  try
    try
    Stop := False;
    Mem.GetBytes(TiffHeader, 8);
    case TiffHeader.ByteOrder of
      $4949: // II (Intel byte order, little endian)
        begin
          FIntelByteOrder := True;
          Mem.ByteOrder := boLittleEndian;
        end;
      $4d4d: // MM (Motorola byte order, big endian)
        begin
          FIntelByteOrder := False;
          Mem.ByteOrder := boBigEndian;
          TiffHeader.Version := SwapEndian(TiffHeader.Version);
          TiffHeader.FirstIFD := SwapEndian(TiffHeader.FirstIFD);
        end;
    else
      FCorruptExif := True;
      Exit;
    end;
    // Go to first IFD
    NextIFD := TiffHeader.FirstIFD;
    repeat
      Mem.SeekFromBeginning(NextIFD);
      TagCount := Mem.GetWord;
      if TagCount = 0 then
        break;
      for i := 0 to TagCount-1 do begin
        TagID := Mem.GetWord;
        case TagID of
          TIFFTAG_ORIENTATION:
            begin
              // Found orientation tag.
              DataFormat := Mem.GetWord;
              // NumComponents commented out since it's not used currently
              {NumComponents :=} Mem.GetLongWord;
              case DataFormat of
                1: begin Data := Mem.GetByte; Mem.SeekForward(3); end;
                3: begin Data := Mem.GetWord; Mem.SeekForward(2); end;
                4: Data := Mem.GetLongWord;
              else
                // Not a DataFormat we expect for Orientation.
                // Skip tag
                Mem.SeekForward(4);
                Data := 0;
              end;
              FOrientation := Byte(Data);
              if FOrientationOnly then begin
                Stop := True;
                break;
              end;
            end;
        else
          // skip tag info and data: 10 bytes
          Mem.SeekForward(10);
        end;
      end;
      NextIFD := Mem.GetLongWord;
    until (NextIFD = 0) or Stop;
    FParsingDone := True;
    except
      on EgexMemoryAccessException do begin
        FCorruptExif := True;
        FParsingDone := True;
      end;
    end;
  finally
    Mem.Free;
  end;
end;

function TSimpleExifReader.GetOrientation(): Byte;
begin
  if not FParsingDone then begin
    ParseExif;
  end;
  if not FCorruptExif then
    Result := FOrientation
  else
    Result := 0;
end;


end.
