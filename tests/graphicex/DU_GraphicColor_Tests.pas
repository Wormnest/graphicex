unit DU_GraphicColor_Tests;

interface

uses
  TestFramework;

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

implementation

uses SysUtils, Classes, Graphics,
     GraphicColor,
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


initialization
  RegisterTests('Test GraphicEx.Unit GraphicColor.GetBits',
    [TGetBitsTests.Suite]);
end.
