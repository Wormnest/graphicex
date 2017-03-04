unit DU_GraphicCompression_Tests;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  SysUtils,
  {$IFNDEF FPC}
  TestFramework,
  {$ELSE}
  fpcunit, testregistry,
  {$ENDIF}
  GraphicCompression
  ;


type
  TCompressionTestsBase = class(TTestCase)
  private
    FDecompressBuffer: Pointer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {procedure TestCanLoad;
    procedure TestReadImageProperties;
    procedure TestLoadImage;}
  end;

  TCutRLEDecoderTests = class(TCompressionTestsBase)
  private
    FDecoder: TCUTRLEDecoder;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompressedSize0;
    procedure TestDecompressedSize0;
    procedure TestDecompress1Byte0;
    procedure TestDecompress1ByteFF;
    procedure TestDecompress1Byte03;
    procedure TestDecompress2BytesFF64;
    procedure TestDecompress3BytesFF6400;
    procedure TestDecompress2BytesFF64Buffer126;
    procedure TestDecompress4Bytes03xx;
    procedure TestDecompress4Bytes03xx00;
    procedure TestDecompress4Bytes03xxBuffer2;
  end;


implementation


const
  BUFSIZE = 1024; // Size of decompression buffer.

procedure TCompressionTestsBase.SetUp;
begin
  GetMem(FDecompressBuffer, BUFSIZE);
end;

procedure TCompressionTestsBase.TearDown;
begin
  FreeMem(FDecompressBuffer);
end;


procedure TCutRLEDecoderTests.SetUp;
begin
  inherited SetUp;
  FDecoder := TCUTRLEDecoder.Create;
  FDecoder.DecodeInit; // Not really needed here
end;

procedure TCutRLEDecoderTests.TearDown;
begin
  FDecoder.DecodeEnd; // Not really needed here
  FDecoder.Free;
  inherited TearDown;
end;

procedure TCutRLEDecoderTests.TestCompressedSize0;
var InputBuffer: array [0..1] of byte;
  Source: Pointer;
begin
  InputBuffer[0] := 0;
  Source := @InputBuffer;
  FDecoder.Decode(Source, FDecompressBuffer, 0, 0);
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
end;

procedure TCutRLEDecoderTests.TestDecompressedSize0;
var InputBuffer: array [0..1] of byte;
  Source: Pointer;
begin
  InputBuffer[0] := 0;
  Source := @InputBuffer;
  FDecoder.Decode(Source, FDecompressBuffer, 0, 0);
  Check(FDecoder.DecompressedBytes = 0, Format('Decompressed bytes not 0 but %d',
    [FDecoder.DecompressedBytes]));
end;

procedure TCUTRLEDecoderTests.TestDecompress1Byte0;
var InputBuffer: array [0..1] of byte;
  Source: Pointer;
begin
  // 0 means end of input, nothing gets decompressed.
  InputBuffer[0] := 0;
  InputBuffer[1] := 0;
  Source := @InputBuffer;
  // 2 bytes input, max 2 bytes output
  FDecoder.Decode(Source, FDecompressBuffer, 2, 2);
  // There should be 1 bytes available
  Check(FDecoder.CompressedBytesAvailable = 1, Format('Compressed bytes not 1 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 0 bytes decompressed
  Check(FDecoder.DecompressedBytes = 0, Format('Decompressed bytes not 0 but %d',
    [FDecoder.DecompressedBytes]));
end;

procedure TCUTRLEDecoderTests.TestDecompress1ByteFF;
var InputBuffer: array [0..1] of byte;
  Source: Pointer;
begin
  // $ff means copy next byte $7f times
  InputBuffer[0] := $ff;
  Source := @InputBuffer;
  // 1 byte incomplete input should result in 0 bytes output
  FDecoder.Decode(Source, FDecompressBuffer, 1, 128);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 0 bytes decompressed
  Check(FDecoder.DecompressedBytes = 0, Format('Decompressed bytes not 0 but %d',
    [FDecoder.DecompressedBytes]));
end;

procedure TCUTRLEDecoderTests.TestDecompress1Byte03;
var InputBuffer: array [0..3] of byte;
  Source: Pointer;
  i: Integer;
begin
  // $03 means move next $03 bytes to output
  InputBuffer[0] := $03;
  for i := 0 to 2 do
    InputBuffer[i] := i;
  Source := @InputBuffer;
  // 1 byte incomplete input should result in 0 bytes output
  FDecoder.Decode(Source, FDecompressBuffer, 1, 128);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 0 bytes decompressed
  Check(FDecoder.DecompressedBytes = 0, Format('Decompressed bytes not 0 but %d',
    [FDecoder.DecompressedBytes]));
end;

procedure TCUTRLEDecoderTests.TestDecompress2BytesFF64;
var InputBuffer: array [0..1] of byte;
  OutputBuffer: PByte;
  ExpectedValue: Byte;
  Source: Pointer;
  i: Integer;
begin
  // $ff means copy next byte $7f times
  InputBuffer[0] := $ff;
  InputBuffer[1] := $64; // 100
  ExpectedValue := $64;
  Source := @InputBuffer;
  // 2 byte input should result in $7f (127) bytes output (all bytes should be $64)
  FDecoder.Decode(Source, FDecompressBuffer, 2, 128);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 127 bytes decompressed
  Check(FDecoder.DecompressedBytes = 127, Format('Decompressed bytes not 127 but %d',
    [FDecoder.DecompressedBytes]));
  OutputBuffer := FDecompressBuffer;
  for i := 0 to 126 do begin
    Check(OutputBuffer^ = ExpectedValue,
      Format('Decompressed byte at index %d has unexpected value %x instead of %x',
      [i, OutputBuffer^, ExpectedValue]));
    Inc(OutputBuffer);
  end;
end;

procedure TCUTRLEDecoderTests.TestDecompress3BytesFF6400;
var InputBuffer: array [0..2] of byte;
  OutputBuffer: PByte;
  ExpectedValue: Byte;
  Source: Pointer;
  i: Integer;
begin
  // $ff means copy next byte $7f times
  InputBuffer[0] := $ff;
  InputBuffer[1] := $64; // 100
  InputBuffer[2] := $00; // end of input
  ExpectedValue := $64;
  Source := @InputBuffer;
  // 3 byte input should result in $7f (127) bytes output (all bytes should be $64)
  FDecoder.Decode(Source, FDecompressBuffer, 3, 128);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 127 bytes decompressed
  Check(FDecoder.DecompressedBytes = 127, Format('Decompressed bytes not 127 but %d',
    [FDecoder.DecompressedBytes]));
  OutputBuffer := FDecompressBuffer;
  for i := 0 to 126 do begin
    Check(OutputBuffer^ = ExpectedValue,
      Format('Decompressed byte at index %d has unexpected value %x instead of %x',
      [i, OutputBuffer^, ExpectedValue]));
    Inc(OutputBuffer);
  end;
end;

procedure TCUTRLEDecoderTests.TestDecompress2BytesFF64Buffer126;
var InputBuffer: array [0..1] of byte;
  Source: Pointer;
begin
  // $ff means copy next byte $7f times
  InputBuffer[0] := $ff;
  InputBuffer[1] := $64; // 100
  Source := @InputBuffer;
  // 2 byte input should result in $7f (127) bytes output but buffer is too small
  FDecoder.Decode(Source, FDecompressBuffer, 2, 126);
  // There should be 1 byte available since we can't handle the count
  Check(FDecoder.CompressedBytesAvailable = 1, Format('Compressed bytes not 1 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be less than 127 bytes decompressed
  Check(FDecoder.DecompressedBytes < 127, Format('Decompressed bytes not smaller than 127 but %d',
    [FDecoder.DecompressedBytes]));
end;

procedure TCUTRLEDecoderTests.TestDecompress4Bytes03xx;
var InputBuffer: array [0..3] of byte;
  OutputBuffer: PByte;
  Source: Pointer;
  i: Integer;
begin
  // $03 means move next $03 bytes to output
  InputBuffer[0] := $03;
  for i := 1 to 3 do
    InputBuffer[i] := i;
  Source := @InputBuffer;
  // 4 bytes input should result in 3 bytes output
  FDecoder.Decode(Source, FDecompressBuffer, 4, 3);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 3 bytes decompressed
  Check(FDecoder.DecompressedBytes = 3, Format('Decompressed bytes not 3 but %d',
    [FDecoder.DecompressedBytes]));
  OutputBuffer := FDecompressBuffer;
  for i := 1 to 3 do begin
    Check(OutputBuffer^ = i,
      Format('Decompressed byte at index %d has unexpected value %x instead of %x',
      [i, OutputBuffer^, i]));
    Inc(OutputBuffer);
  end;
end;

procedure TCUTRLEDecoderTests.TestDecompress4Bytes03xx00;
var InputBuffer: array [0..4] of byte;
  OutputBuffer: PByte;
  Source: Pointer;
  i: Integer;
begin
  // $03 means move next $03 bytes to output
  InputBuffer[0] := $03;
  for i := 1 to 3 do
    InputBuffer[i] := i;
  InputBuffer[4] := 0;
  Source := @InputBuffer;
  // 5 bytes input should result in 3 bytes output
  FDecoder.Decode(Source, FDecompressBuffer, 5, 3);
  // There should be 0 bytes available
  Check(FDecoder.CompressedBytesAvailable = 0, Format('Compressed bytes not 0 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 3 bytes decompressed
  Check(FDecoder.DecompressedBytes = 3, Format('Decompressed bytes not 3 but %d',
    [FDecoder.DecompressedBytes]));
  OutputBuffer := FDecompressBuffer;
  for i := 1 to 3 do begin
    Check(OutputBuffer^ = i,
      Format('Decompressed byte at index %d has unexpected value %x instead of %x',
      [i, OutputBuffer^, i]));
    Inc(OutputBuffer);
  end;
end;

procedure TCUTRLEDecoderTests.TestDecompress4Bytes03xxBuffer2;
var InputBuffer: array [0..3] of byte;
  Source: Pointer;
  i: Integer;
begin
  // $03 means move next $03 bytes to output
  InputBuffer[0] := $03;
  for i := 1 to 3 do
    InputBuffer[i] := i;
  Source := @InputBuffer;
  // 4 bytes input should result in 3 bytes output, but output buffer is only 2 bytes
  FDecoder.Decode(Source, FDecompressBuffer, 4, 2);
  // There should be 3 bytes available since we couldn't move the data bytes
  Check(FDecoder.CompressedBytesAvailable = 3, Format('Compressed bytes not 3 but %d',
    [FDecoder.CompressedBytesAvailable]));
  // There should be 0 bytes decompressed
  Check(FDecoder.DecompressedBytes = 0, Format('Decompressed bytes not 0 but %d',
    [FDecoder.DecompressedBytes]));
end;

initialization
  RegisterTests('Test GraphicEx.Unit GraphicCompression',
    [
      TCutRLEDecoderTests{$IFNDEF FPC}.Suite{$ENDIF}
    ]);
end.
