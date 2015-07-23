unit DU_GraphicColor_Tests;

interface

// Do this before uses since GraphicColor defines UInt64!
// Not working in Fpc so added IFNDEF FPC.
{$IFNDEF FPC}
{$IF NOT Declared(UInt64)}
  {$DEFINE NO_UINT64}
{$IFEND}
{$ENDIF}

uses
     GraphicColor,
     {$IFNDEF FPC}
     TestFramework;
     {$ELSE}
     fpcunit, testregistry;
     {$ENDIF}

type
  TGetBitsTests = class(TTestCase)
  public
    DoTestCounter: Integer;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoTest(BitOffset,BitsPerSample: byte; Data: array of byte; ExpectedResult: Cardinal);
  published
    procedure Check14BitsAtOffset0;
    procedure Check12BitsAtOffset0;
    procedure Check10BitsAtOffset0;
    procedure Check14BitsAtOffset2;
//    procedure Check12BitsAtOffset2;
    procedure Check10BitsAtOffset2;
  end;

  TCM = class(TColorManager)
  end;

  TScaleConvertTests = class(TTestCase)
  public
    FCM: TCM;
    DoTestCounter: Integer;
    ScaleConvert16: function(Value: Word; BitsPerSampe: Byte): Byte of object;
    ScaleConvert32: function(Value: LongWord; BitsPerSampe: Byte): Byte of object;
    ScaleConvert64: function(Value: UInt64; BitsPerSampe: Byte): Byte of object;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoTestMax16ToMax8(AValue: Word; ABitsPerSample: Byte;
      ExpectedResult: Byte);
    procedure DoTestMax32ToMax8(AValue: LongWord; ABitsPerSample: Byte;
      ExpectedResult: Byte);
    procedure DoTestMax64ToMax8(AValue: UInt64; ABitsPerSample: Byte;
      ExpectedResult: Byte);
  published
    // Conversion to 4 bits per sample
    procedure CheckComponentScaleConvertTo4;
    // Conversion to 8 bits per sample, 2 parameters
    procedure CheckComponentScaleConvert1_15To8;
    procedure CheckComponentScaleConvert6To8;
    procedure CheckComponentScaleConvert10To8;
    procedure CheckComponentScaleConvert12To8;
    procedure CheckComponentScaleConvert14To8;
    procedure CheckComponentScaleConvert32To8;
    procedure CheckComponentScaleConvert64To8;
    procedure CheckComponentScaleConvert17_31To8;
    procedure CheckComponentScaleConvert33_63To8;
    // Conversion to 8 bits per sample, 1 parameter
    procedure CheckComponentScaleConvert16To8;
    procedure CheckComponentScaleConvert32To8_1param;
    procedure CheckComponentScaleConvert64To8_1param;
    // Float conversion to 8 bits per sample
    procedure CheckComponentScaleConvertFloat16To8;
    procedure CheckComponentScaleConvertFloat24To8;
    procedure CheckComponentScaleConvertFloat32To8;
    procedure CheckComponentScaleConvertFloat64To8;
    // Byte swap and scale conversion 16 to 18 bits
    procedure CheckSwapScaleConvert;
  end;

implementation

uses SysUtils, Classes, Graphics,
     GraphicEx;


procedure TGetBitsTests.SetUp;
begin
  inherited;
  DoTestCounter := 0;
end;

procedure TGetBitsTests.TearDown;
begin
  inherited;
end;

const
  cb_array_val_01 : array [0..3] of byte = ( 01, 00, 00, 00);
  cb_array_val_ff : array [0..3] of byte = ( $ff, 00, 00, 00 );
  cb_array_val_00_ff : array [0..3] of byte = ( $00, $ff, 00, 00 );
  cb_array_val_00_00_ff_ff : array [0..3] of byte = ( $00, 00, $ff, $ff );
  cb_array_val_ff_3f : array [0..3] of byte = ( $ff, $3f, 00, 00 );
  cb_array_val_ff_ff : array [0..3] of byte = ( $ff, $ff, 00, 00 );

procedure TGetBitsTests.Check14BitsAtOffset0;
const BitsPerSample = 14;
      BitOffset     =  0;
begin
  DoTestCounter := 0;
  DoTest(BitOffset, BitsPerSample, cb_array_val_01, 1);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff, $ff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_3f, $3fff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_ff, $3fff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_ff, $3f00);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_00_ff_ff, 0);
end;

procedure TGetBitsTests.Check12BitsAtOffset0;
const BitsPerSample = 12;
      BitOffset     =  0;
begin
  DoTestCounter := 0;
  DoTest(BitOffset, BitsPerSample, cb_array_val_01, 1);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff, $ff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_3f, $0fff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_ff, $0fff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_ff, $0f00);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_00_ff_ff, 0);
end;

procedure TGetBitsTests.Check10BitsAtOffset0;
const BitsPerSample = 10;
      BitOffset     =  0;
begin
  DoTestCounter := 0;
  DoTest(BitOffset, BitsPerSample, cb_array_val_01, 1);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff, $ff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_3f, $03ff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_ff_ff, $03ff);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_ff, $0300);
  DoTest(BitOffset, BitsPerSample, cb_array_val_00_00_ff_ff, 0);
end;

const
  test_array_01 : array [0..3] of byte = (  03,  00, $ff, $ff);
  test_array_02 : array [0..3] of byte = ( $fc,  00, $ff, $ff);
  test_array_03 : array [0..3] of byte = ( $fc,  03, $ff, $ff);
  test_array_04 : array [0..3] of byte = ( $ff, $ff, $ff, $ff);
  test_array_05 : array [0..3] of byte = ( 0, 0, 0, 0);
  // results for starting bit offset 2:
  expected_14_01 = 0;
  expected_14_02 = $3f;
  expected_14_03 = $00ff;
  expected_14_04 = $3fff;
  expected_14_05 = 0;
  expected_10_01 = 0;
  expected_10_02 = $3f;
  expected_10_03 = $00ff;
  expected_10_04 = $03ff;
  expected_10_05 = 0;

procedure TGetBitsTests.DoTest(BitOffset,BitsPerSample: byte;
  Data: array of byte; ExpectedResult: Cardinal);
var
  pb : pByte;
  BitsResult: Cardinal;
begin
  pb := @Data;
  BitsResult := GetBits(BitOffset,BitsPerSample,PCardinal(pb));
  Inc(DoTestCounter);
  Check(BitsResult = ExpectedResult, Format('Test %u: BitsResult should be %x but it is %x!',
    [DoTestCounter, ExpectedResult, BitsResult]));
end;

procedure TGetBitsTests.Check14BitsAtOffset2;
const BitsPerSample = 14;
      BitOffset     =  2;
begin
  DoTestCounter := 0;
  DoTest(BitOffset, BitsPerSample, test_array_01, expected_14_01);
  DoTest(BitOffset, BitsPerSample, test_array_02, expected_14_02);
  DoTest(BitOffset, BitsPerSample, test_array_03, expected_14_03);
  DoTest(BitOffset, BitsPerSample, test_array_04, expected_14_04);
  DoTest(BitOffset, BitsPerSample, test_array_05, expected_14_05);
end;

procedure TGetBitsTests.Check10BitsAtOffset2;
const BitsPerSample = 10;
      BitOffset     =  2;
begin
  DoTestCounter := 0;
  DoTest(BitOffset, BitsPerSample, test_array_01, expected_10_01);
  DoTest(BitOffset, BitsPerSample, test_array_02, expected_10_02);
  DoTest(BitOffset, BitsPerSample, test_array_03, expected_10_03);
  DoTest(BitOffset, BitsPerSample, test_array_04, expected_10_04);
  DoTest(BitOffset, BitsPerSample, test_array_05, expected_10_05);
end;

////////////////////////////////////////////////////////////////////////////////
//////////////////// Scale Conversion Tests ////////////////////////////////////

procedure TScaleConvertTests.SetUp;
begin
  inherited;
  FCM := TCM.Create;
  ScaleConvert16 := nil;
  ScaleConvert32 := nil;
  ScaleConvert64 := nil;
end;

procedure TScaleConvertTests.TearDown;
begin
  FCM.Free;
  inherited;
end;

procedure TScaleConvertTests.DoTestMax16ToMax8(AValue: Word; ABitsPerSample: Byte;
  ExpectedResult: Byte);
var
  CvtResult: Byte;
begin
  CvtResult := ScaleConvert16(AValue, ABitsPerSample);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

{$IFDEF FPC}
  {$PUSH}
  {$RANGECHECKS OFF Range checks off, since values used in array of const used by Format is
     always checked against signed integer values even if you used an unsigned value.}
{$ENDIF}
procedure TScaleConvertTests.DoTestMax32ToMax8(AValue: LongWord; ABitsPerSample: Byte;
  ExpectedResult: Byte);
var
  CvtResult: Byte;
begin
  CvtResult := ScaleConvert32(AValue, ABitsPerSample);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;
{$IFDEF FPC}
  {$POP}
{$ENDIF}

procedure TScaleConvertTests.DoTestMax64ToMax8(AValue: UInt64; ABitsPerSample: Byte;
  ExpectedResult: Byte);
var
  CvtResult: Byte;
begin
  CvtResult := ScaleConvert64(AValue, ABitsPerSample);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

const
  Max1Bits  =     1;
  Max2Bits  =     3;
  Max3Bits  =     7;
  Max4Bits  =    15;
  Max5Bits  =    31;
  Max6Bits  =    63;
  Max7Bits  =   127;
  Max8Bits  =   255;
  Max9Bits  =   511;
  Max10Bits =  1023;
  Max11Bits =  2047;
  Max12Bits =  4095;
  Max13Bits =  8191;
  Max14Bits = 16383;
  Max15Bits = 32765;
  Max16Bits = 65535;
  Max32Bits = High(LongWord);
  {$IFNDEF NO_UINT64}
  Max64Bits = High(UInt64);
  {$ELSE}
  // Delphi 6 doesn't know the real UInt64 so we can't use High(UInt64)!
  Max64Bits = Int64(-1);
  {$ENDIF}

  MaxBits17_32: array [17..32] of LongWord = (
    1 shl 17 - 1, 1 shl 18 - 1, 1 shl 19 - 1, 1 shl 20 - 1, 1 shl 21 - 1,
    1 shl 22 - 1, 1 shl 23 - 1, 1 shl 24 - 1, 1 shl 25 - 1, 1 shl 26 - 1,
    1 shl 27 - 1, 1 shl 28 - 1, 1 shl 29 - 1, 1 shl 30 - 1, High(LongInt),
    High(LongWord));

  MaxBits33_64: array [33..64] of UInt64 = (
    UInt64(1) shl 33 - 1, UInt64(1) shl 34 - 1, UInt64(1) shl 35 - 1, UInt64(1) shl 36 - 1, UInt64(1) shl 37 - 1,
    UInt64(1) shl 38 - 1, UInt64(1) shl 39 - 1, UInt64(1) shl 40 - 1, UInt64(1) shl 41 - 1, UInt64(1) shl 42 - 1,
    UInt64(1) shl 43 - 1, UInt64(1) shl 44 - 1, UInt64(1) shl 45 - 1, UInt64(1) shl 46 - 1, UInt64(1) shl 47 - 1,
    UInt64(1) shl 48 - 1, UInt64(1) shl 49 - 1, UInt64(1) shl 50 - 1, UInt64(1) shl 51 - 1, UInt64(1) shl 52 - 1,
    UInt64(1) shl 53 - 1, UInt64(1) shl 54 - 1, UInt64(1) shl 55 - 1, UInt64(1) shl 56 - 1, UInt64(1) shl 57 - 1,
    UInt64(1) shl 58 - 1, UInt64(1) shl 59 - 1, UInt64(1) shl 60 - 1, UInt64(1) shl 61 - 1, UInt64(1) shl 62 - 1,
    {$IFNDEF NO_UINT64}
    High(Int64), High(UInt64));
    {$ELSE}
    // Delphi 6 doesn't know the real UInt64 so we can't use High(UInt64)!
    // -1 represents the same value as High(UInt64)
    High(Int64), -1);
    {$ENDIF}

procedure TScaleConvertTests.CheckComponentScaleConvert1_15To8;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvertUncommonTo8;
  DoTestCounter := 0;
  // Convert 1 bits per sample to 8 bits
  DoTestMax16ToMax8(Max1Bits, 1, Max8Bits); // Max value 1 --> 255
  DoTestMax16ToMax8(0, 1, 0);   // Min value 0 --> 0
  // Convert 2 bits per sample to 8 bits
  DoTestMax16ToMax8(Max2Bits, 2, Max8Bits); // Max value 3 --> 255
  DoTestMax16ToMax8(0, 2, 0);   // Min value 0 --? 0
  // Convert 3 bits per sample to 8 bits
  DoTestMax16ToMax8(Max3Bits, 3, Max8Bits); // Max value 7 --> 255
  DoTestMax16ToMax8(0, 3, 0);   // Min value 0 --? 0
  // Convert 4 bits per sample to 8 bits
  DoTestMax16ToMax8(Max4Bits, 4, Max8Bits); // Max value 15 --> 255
  DoTestMax16ToMax8(0, 4, 0);   // Min value 0 --? 0
  // Convert 5 bits per sample to 8 bits
  DoTestMax16ToMax8(Max5Bits, 5, Max8Bits); // Max value 31 --> 255
  DoTestMax16ToMax8(0, 5, 0);   // Min value 0 --? 0
  // Convert 6 bits per sample to 8 bits
  DoTestMax16ToMax8(Max6Bits, 6, Max8Bits); // Max value 63 --> 255
  DoTestMax16ToMax8(0, 6, 0);   // Min value 0 --? 0
  // Convert 7 bits per sample to 8 bits
  DoTestMax16ToMax8(Max7Bits, 7, Max8Bits); // Max value 127 --> 255
  DoTestMax16ToMax8(0, 7, 0);   // Min value 0 --? 0
  // Convert 8 bits per sample to 8 bits
  DoTestMax16ToMax8(Max8Bits, 8, Max8Bits); // Max value 255 --> 255
  DoTestMax16ToMax8(0, 8, 0);   // Min value 0 --? 0
  // Convert 9 bits per sample to 8 bits
  DoTestMax16ToMax8(Max9Bits, 9, Max8Bits); // Max value 511 --> 255
  DoTestMax16ToMax8(0, 9, 0);   // Min value 0 --? 0
  // Convert 10 bits per sample to 8 bits
  DoTestMax16ToMax8(Max10Bits, 10, Max8Bits); // Max value 1023 --> 255
  DoTestMax16ToMax8(0, 10, 0);   // Min value 0 --? 0
  // Convert 11 bits per sample to 8 bits
  DoTestMax16ToMax8(Max11Bits, 11, Max8Bits); // Max value 2047 --> 255
  DoTestMax16ToMax8(0, 11, 0);   // Min value 0 --? 0
  // Convert 12 bits per sample to 8 bits
  DoTestMax16ToMax8(Max12Bits, 12, Max8Bits); // Max value 4095 --> 255
  DoTestMax16ToMax8(0, 12, 0);   // Min value 0 --? 0
  // Convert 13 bits per sample to 8 bits
  DoTestMax16ToMax8(Max13Bits, 13, Max8Bits); // Max value 8191 --> 255
  DoTestMax16ToMax8(0, 13, 0);   // Min value 0 --? 0
  // Convert 14 bits per sample to 8 bits
  DoTestMax16ToMax8(Max14Bits, 14, Max8Bits); // Max value 16383 --> 255
  DoTestMax16ToMax8(0, 14, 0);   // Min value 0 --? 0
  // Convert 15 bits per sample to 8 bits
  DoTestMax16ToMax8(Max15Bits, 15, Max8Bits); // Max value 32765 --> 255
  DoTestMax16ToMax8(0, 15, 0);   // Min value 0 --? 0
  // Convert 16 bits per sample to 8 bits
  DoTestMax16ToMax8(Max16Bits, 16, Max8Bits); // Max value 65535 --> 255
  DoTestMax16ToMax8(0, 16, 0);   // Min value 0 --? 0
end;

procedure TScaleConvertTests.CheckComponentScaleConvert6To8;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert6To8;
  DoTestCounter := 0;
  // Convert 6 bits per sample to 8 bits
  DoTestMax16ToMax8(Max6Bits, 6, Max8Bits); // Max value 63 --> 255
  DoTestMax16ToMax8(0, 6, 0);   // Min value 0 --? 0
end;

procedure TScaleConvertTests.CheckComponentScaleConvert10To8;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert10To8;
  DoTestCounter := 0;
  // Convert 10 bits per sample to 8 bits
  DoTestMax16ToMax8(Max10Bits, 10, Max8Bits); // Max value
  DoTestMax16ToMax8(0, 10, 0);   // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvert12To8;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert12To8;
  DoTestCounter := 0;
  // Convert 12 bits per sample to 8 bits
  DoTestMax16ToMax8(Max12Bits, 12, Max8Bits); // Max value
  DoTestMax16ToMax8(0, 12, 0);   // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvert14To8;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert14To8;
  DoTestCounter := 0;
  // Convert 14 bits per sample to 8 bits
  DoTestMax16ToMax8(Max14Bits, 14, Max8Bits); // Max value
  DoTestMax16ToMax8(0, 14, 0);   // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvertTo4;
begin
  ScaleConvert16 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvertTo4;
  DoTestCounter := 0;
  // Convert 1 bits per sample to 4 bits
  DoTestMax16ToMax8(Max1Bits, 1, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 1, 0);                 // Min value
  // Convert 2 bits per sample to 4 bits
  DoTestMax16ToMax8(Max2Bits, 2, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 2, 0);                 // Min value
  // Convert 3 bits per sample to 4 bits
  DoTestMax16ToMax8(Max3Bits, 3, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 3, 0);                 // Min value
  // Convert 4 bits per sample to 4 bits
  DoTestMax16ToMax8(Max4Bits, 4, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 4, 0);                 // Min value
  // Convert 5 bits per sample to 4 bits
  DoTestMax16ToMax8(Max5Bits, 5, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 5, 0);                 // Min value
  // Convert 6 bits per sample to 4 bits
  DoTestMax16ToMax8(Max6Bits, 6, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 6, 0);                 // Min value
  // Convert 7 bits per sample to 4 bits
  DoTestMax16ToMax8(Max7Bits, 7, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 7, 0);                 // Min value
  // Convert 8 bits per sample to 4 bits
  DoTestMax16ToMax8(Max8Bits, 8, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 8, 0);                 // Min value
  // Convert 9 bits per sample to 4 bits
  DoTestMax16ToMax8(Max9Bits, 9, Max4Bits);   // Max value
  DoTestMax16ToMax8(0, 9, 0);                 // Min value
  // Convert 10 bits per sample to 4 bits
  DoTestMax16ToMax8(Max10Bits, 10, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 10, 0);                // Min value
  // Convert 11 bits per sample to 4 bits
  DoTestMax16ToMax8(Max11Bits, 11, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 11, 0);                // Min value
  // Convert 12 bits per sample to 4 bits
  DoTestMax16ToMax8(Max12Bits, 12, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 12, 0);                // Min value
  // Convert 13 bits per sample to 4 bits
  DoTestMax16ToMax8(Max13Bits, 13, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 13, 0);                // Min value
  // Convert 14 bits per sample to 4 bits
  DoTestMax16ToMax8(Max14Bits, 14, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 14, 0);                // Min value
  // Convert 15 bits per sample to 4 bits
  DoTestMax16ToMax8(Max15Bits, 15, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 15, 0);                // Min value
  // Convert 16 bits per sample to 4 bits
  DoTestMax16ToMax8(Max16Bits, 16, Max4Bits); // Max value
  DoTestMax16ToMax8(0, 16, 0);                // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvert17_31To8;
var
  BitsPerSample: Word;
begin
  DoTestCounter := 0;

  // Both functions are essentially the same function and should be combined into 1!
  ScaleConvert32 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert17_24To8;
  for BitsPerSample := 17 to 24 do begin
    DoTestMax32ToMax8(MaxBits17_32[BitsPerSample], BitsPerSample, Max8Bits); // Max value
    DoTestMax32ToMax8(0, BitsPerSample, 0);                                  // Min value
  end;
  ScaleConvert32 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert25_31To8;
  for BitsPerSample := 25 to 31 do begin
    DoTestMax32ToMax8(MaxBits17_32[BitsPerSample], BitsPerSample, Max8Bits); // Max value
    DoTestMax32ToMax8(0, BitsPerSample, 0);                                  // Min value
  end;
end;

procedure TScaleConvertTests.CheckComponentScaleConvert32To8;
begin
  ScaleConvert32 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert32To8;
  // Convert 32 bits per sample to 4 bits
  DoTestMax32ToMax8(MaxBits17_32[32], 32, Max8Bits);   // Max value
  DoTestMax32ToMax8(0, 32, 0);                  // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvert64To8;
begin
  ScaleConvert64 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert64To8;
  // Convert 64 bits per sample to 4 bits
  DoTestMax64ToMax8(MaxBits33_64[64], 64, Max8Bits);   // Max value
  DoTestMax64ToMax8(0, 64, 0);                  // Min value
end;

procedure TScaleConvertTests.CheckComponentScaleConvert33_63To8;
var BitsPerSample: Word;
begin
  DoTestCounter := 0;
  ScaleConvert64 := {$IFDEF FPC}@{$ENDIF}FCM.ComponentScaleConvert33_63To8;
  for BitsPerSample := 33 to 63 do begin
    DoTestMax64ToMax8(MaxBits33_64[BitsPerSample], BitsPerSample, Max8Bits); // Max value
    DoTestMax64ToMax8(0, BitsPerSample, 0);                                  // Min value
  end;
end;

procedure TScaleConvertTests.CheckComponentScaleConvert16To8;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: Word;
begin
  DoTestCounter := 0;
  ABitsPerSample := 16;
  AValue := Max16Bits;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvert16To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  AValue := 0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvert16To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  // Extra test to make sure values in the middle of the range are also converted correctly
  AValue := $8000;
  ExpectedResult := 128;
  CvtResult := FCM.ComponentScaleConvert16To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

{$IFDEF FPC}
  {$PUSH}
  {$RANGECHECKS OFF Range checks off, since values used in array of const used by Format is
     always checked against signed integer values even if you used an unsigned value.}
{$ENDIF}
procedure TScaleConvertTests.CheckComponentScaleConvert32To8_1param;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: LongWord;
begin
  DoTestCounter := 0;
  ABitsPerSample := 32;
  AValue := Max32Bits;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvert32To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  AValue := 0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvert32To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  // Extra test to make sure values in the middle of the range are also converted correctly
  AValue := $80000000;
  ExpectedResult := 128;
  CvtResult := FCM.ComponentScaleConvert32To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;
{$IFDEF FPC}
  {$POP}
{$ENDIF}

procedure TScaleConvertTests.CheckComponentScaleConvert64To8_1param;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: UInt64;
begin
  DoTestCounter := 0;
  ABitsPerSample := 64;
  AValue := Max64Bits;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvert64To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  AValue := 0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvert64To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  // Extra test to make sure values in the middle of the range are also converted correctly
  AValue := UInt64($8000000000000000);
  ExpectedResult := 128;
  CvtResult := FCM.ComponentScaleConvert64To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

procedure TScaleConvertTests.CheckComponentScaleConvertFloat16To8;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: Word;
  AHalfFloat: THalfFloat absolute AValue;
begin
  DoTestCounter := 0;
  ABitsPerSample := 16;
  //AValue := Max16Bits;
  AHalfFloat := FloatToHalf(1.0);
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvertFloat16To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  //AValue := 0;
  AHalfFloat := FloatToHalf(0.0);
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvertFloat16To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

procedure TScaleConvertTests.CheckComponentScaleConvertFloat24To8;
{var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: LongWord;
  AFloat24: TFloat24 absolute AValue;}
begin
  {$IFDEF IGNORE_EMPTY_TEST}
  Self.Ignore('Test not Implemented!');
  {$ELSE}
  Check(False, 'Test not implemented!');
  {$ENDIF}

{ Todo: We need a FloatToFloat24 routine first
  DoTestCounter := 0;
  ABitsPerSample := 32;
  //AValue := Max32Bits;
  AFloat24 := FloatToFloat24(1.0);
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvertFloat24To8(AValue, 24);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  //AValue := 0;
  AFloat24 := FloatToFloat24(0.0);
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvertFloat24To8(AValue, 24);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
}
end;

procedure TScaleConvertTests.CheckComponentScaleConvertFloat32To8;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: LongWord;
  ASingle: Single absolute AValue;
begin
  DoTestCounter := 0;
  ABitsPerSample := 32;
  //AValue := Max32Bits;
  ASingle := 1.0;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvertFloat32To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  //AValue := 0;
  ASingle := 0.0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvertFloat32To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

procedure TScaleConvertTests.CheckComponentScaleConvertFloat64To8;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: UInt64;
  AFloat: Double absolute AValue;
begin
  DoTestCounter := 0;
  ABitsPerSample := 64;
  //AValue := Max64Bits;
  AFloat := 1.0;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentScaleConvertFloat64To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  //AValue := 0;
  AFloat := 0.0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentScaleConvertFloat64To8(AValue);
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

procedure TScaleConvertTests.CheckSwapScaleConvert;
var
  CvtResult,
  ExpectedResult,
  ABitsPerSample: Byte;
  AValue: Word;
begin
  DoTestCounter := 0;
  ABitsPerSample := 16;
  // Swap is rtl function so we will trust it to be correct
  AValue := Max16Bits;
  ExpectedResult := Max8Bits;
  CvtResult := FCM.ComponentSwapScaleConvert(Swap(AValue));
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  AValue := 0;
  ExpectedResult := 0;
  CvtResult := FCM.ComponentSwapScaleConvert(Swap(AValue));
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
  // Extra test to make sure values in the middle of the range are also converted correctly
  AValue := $8000;
  ExpectedResult := 128;
  CvtResult := FCM.ComponentSwapScaleConvert(Swap(AValue));
  Inc(DoTestCounter);
  Check( CvtResult = ExpectedResult,
    Format('Conversion test %u: Incorrect conversion result for value %u at %u bits per sample. ' +
    'Result is %u but we expected %u.',
    [DoTestCounter, AValue, ABitsPerSample, CvtResult, ExpectedResult]));
end;

initialization
  RegisterTests('Test GraphicEx.Unit GraphicColor',
    [
      TGetBitsTests{$IFNDEF FPC}.Suite{$ENDIF},
      TScaleConvertTests{$IFNDEF FPC}.Suite{$ENDIF}
    ]);
end.
