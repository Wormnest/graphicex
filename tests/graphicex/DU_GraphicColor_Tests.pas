unit DU_GraphicColor_Tests;

interface

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
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DoTestMax16ToMax8(AValue: Word; ABitsPerSample: Byte;
      ExpectedResult: Byte);
  published
    procedure CheckComponentScaleConvertUncommonTo8;
    procedure CheckComponentScaleConvertTo4;
  end;

implementation

uses SysUtils, Classes, Graphics,
     GraphicStrings, graphicex;


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
  Check(BitsResult = ExpectedResult, Format('Test %d: BitsResult should be %x but it is %x!',
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
    Format('Conversion test %d: Incorrect conversion result for value %d at %d bits per sample. ' +
    'Result is %d but we expected %d.',
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

procedure TScaleConvertTests.CheckComponentScaleConvertUncommonTo8;
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

initialization
  RegisterTests('Test GraphicEx.Unit GraphicColor',
    [
      TGetBitsTests{$IFNDEF FPC}.Suite{$ENDIF}
    ]);
  RegisterTests('Test GraphicEx.Unit GraphicColor',
    [
      TScaleConvertTests{$IFNDEF FPC}.Suite{$ENDIF}
    ]);
end.
