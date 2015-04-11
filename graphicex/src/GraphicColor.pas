{$TYPEDADDRESS OFF}

unit GraphicColor;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicCompression.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Plei�a, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
// Portions Created by Jacob Boerema are Copyright (C) 2013 Jacob Boerema.
// All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/jgb-thirdparty
//
// GraphicColor contains the implementation of the color conversion manager.
// This class is responsible for converting between these color schemes/formats:
//   - RGB(A)
//   - BGR(A)
//   - CMY(K)
//   - CIE L*a*b*
//   - PhotoYCC, standard YCbCr
//   - indexed
//   - grayscale (with alpha, which is ignored currently)
//
// Additional tasks are:
//   - conversions between bit depths (1, 2, 4, 8, 16 bits)
//   - palette creation
//   - gamma tables creation and application
//   - masked pixel transfer for interlaced images
//   - big endian swap
//   - plane (planar) -> interleaved (interlaced) conversion
//
// Notes:
//   - Throughout the entire unit I used the terms BPS and SPP for "bits per sample" and
//     "samples per pixel", respectively. A sample is one component per pixel. For indexed color schemes
//     there's only 1 sample per pixel, for RGB there are 3 (red, green and blue) and so on.
//   - The bit depth of multi sample formats like RGB must be equal for each color component.
//   - Because of the large amount of possible combinations (color schemes, sample depth, gamma, byte swap)
//     I limited the accepted combinations to pratical ones. This leaves currently out:
//       + gamma correction for 16 bit values
//       + conversion to 16 bit (target) grayscale with alpha
//       + samples sizes less than 8 bits for multi-sample schemes (RGB etc.)
//       + indexed schemes with planes (e.g. 16 colors indexed as 4 planes each with one bit per sample)
//   - For now there is no conversion between indexed and non-indexed formats. Also between grayscale
//     and any other scheme is no conversion possible.
//   - jb: The notes above have not been adapted yet to any possible changes and
//         additions that have been done by me.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$Include GraphicConfiguration.inc}

{$IFNDEF FPC}
{$Include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}
{$ELSE}
   // fpc
  {$mode Delphi}
{$ENDIF}

uses
  Windows, Graphics, GraphicStrings, SysUtils;

const
  // This is the value for average CRT monitors, adjust it if your monitor differs.
  DefaultDisplayGamma = 2.2;

type
  // Define UInt64 as Int64 for Delphi versions not having UInt64
  {$IF NOT Declared(UInt64)}
  UInt64 = Int64;
  {$IFEND}
  {$IF NOT Declared(PUInt64)}
  PUint64 = ^UInt64;
  {$IFEND}

  // Color layout records
  // ------------------------- CMYK -------------------------
  PCMYK = ^TCMYK;
  TCMYK = packed record
    C, M, Y, K: Byte;
  end;

  PCMYK16 = ^TCMYK16;
  TCMYK16 = packed record
    C, M, Y, K: Word;
  end;

  PCMY = ^TCMY;
  TCMY = packed record
    C, M, Y: Byte;
  end;

  PCMY16 = ^TCMY16;
  TCMY16 = packed record
    C, M, Y: Word;
  end;

  // ------------------------- RGB(A) -------------------------
  // RGB(A) 8 bit
  TRGBAColor32 = Cardinal; // Define Graphics32 compatible color type

  PRGB = ^TRGB;
  TRGB = packed record
    R, G, B: Byte;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R, G, B, A: Byte;
  end;

  // RGB(A) 16 bit
  PRGB16 = ^TRGB16;
  TRGB16 = packed record
    R, G, B: Word;
  end;

  PRGBA16 = ^TRGBA16;
  TRGBA16 = packed record
    R, G, B, A: Word;
  end;

  // Use TRGBInt only for cases where temp results are stored here that can be negative.
  TRGBInt = record
    R, G, B: Integer;
  end;

  TRGBAInt = record
    R, G, B, A: Integer;
  end;

  // RGB 32 bit
  PRGB32 = ^TRGB32;
  TRGB32 = packed record
    R, G, B: Cardinal;
  end;

  // RGB 32 bit float
  PRGBFloat = ^TRGBFloat;
  TRGBFloat = packed record
    R, G, B: Single;
  end;

  // ------------------------- BGR(A) -------------------------
  // BGR(A) 8 bit
  PBGR = ^TBGR;
  TBGR = packed record
    B, G, R: Byte;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B, G, R, A: Byte;
  end;

  // BGR(A) 16 bit
  PBGR16 = ^TBGR16;
  TBGR16 = packed record
    B, G, R: Word;
  end;

  PBGRA16 = ^TBGRA16;
  TBGRA16 = packed record
    B, G, R, A: Word;
  end;

  // ------------------------- HLS -------------------------
  PHLS = ^THLS;
  THLS = packed record
    H, L, S: Byte;
  end;

  PHLSFloat = ^THLSFloat;
  THLSFloat = packed record
    H, L, S: Single;
  end;

  // Color formats currently known to GraphicEx
  TColorScheme = (
    csUnknown,
    csIndexed,    // Palette format.
    csIndexedA,   // Palette format with alpha channel.
    csG,          // Gray scale.
    csGA,         // Gray scale with alpha channel.
    csRGB,        // Red, green, blue.
    csRGBA,       // RGB with alpha channel
    csBGR,        // RGB in reversed order.
    csBGRA,       // BGR with alpha channel.
    csCMY,        // Cyan, magenta, yellow.
    csCMYK,       // CMY with black.
    csCMYKA,      // CMYK with alpha channel.
    csCIELab,     // CIE color format using luminance and chromaticities.
    csITULab,     // ITU L*a*b*
    csCIELog2L,   // CIE Log2(L)
    csCIELog2Luv, // CIE Log2(L) (u', v')
    csYCbCr,      // Another format using luminance and chromaticities.
    csPhotoYCC    // A modified YCbCr version used for photo CDs.
  );

  // Color Manager conversion settings
  TConvertOptions = set of (
    coAlpha,          // Alpha channel is to be considered (this value is usually automatically
                      // set depending on the color scheme)
    coApplyGamma,     // Target only, gamma correction must take place
    coNeedByteSwap,   // Endian switch needed
    coLabByteRange,   // CIE L*a*b* only, luminance range is from 0..255 instead 0..100
    coLabChromaOffset,// CIE L*a*b* only, chrominance values a and b are given in 0..255 instead -128..127
    coSeparatePlanes, // TIF: PlanarConfig = Separate planes: one color/alpha per plane instead of contigious
    coUnequalSamples, // Signal that bits per sample values for each channel are not equal, e.g. bmp 16bpp 565
    coPaletteBGR      // Interlaced palette data is in BGR order instead of RGB
  );

  // Format of the raw data to create a palette from
  TRawPaletteFormat = (
    pfUnknown,            // Palette format not set
    pfGrayscale,          // Grayscale palette
    pfInterlaced8Triple,  // RGB triple with 8 bits per component
    pfInterlaced8Quad,    // RGB quad with 8 bits per component (fourth entry is reserved as in Windows' logical palette)
    pfPlane8Triple,       // 3 separate planes of data with 8 bits per component (expected order RGB)
    pfPlane8Quad,         // 4 separate planes of data with 8 bits per component (expecter order RGBreserved)
    pfInterlaced16Triple, // RGB triple with 16 bits per component
    pfInterlaced16Quad,   // RGB quad with 16 bits per component (fourth entry reserved)
    pfPlane16Triple,      // 3 separate planes of data with 16 bits per component
    pfPlane16Quad         // 4 separate planes of data with 16 bits per component
  );

  // Data format of samples based on the TIFF definitions
  TSampleDataFormat = (
    sdfUnknown,          // Unknown, use default unsigned integer
    sdfUnsignedInt,      // Unsigned Integer (default)
    sdfSignedInt,        // Signed Integer
    sdfFloat,            // IEEE Floating Point
    sdfUndefined,        // The saving program didn't know data type, try unsigned integer
    sdfComplexInt,       // Complex Integer
    sdfComplexFloat      // Complex IEEE Floating Point
  );

  // TConversionMethod describes the general parameter list to which each implemented conversion method conforms.
  // Note: Source is defined as open array parameter to allow plane and interlaced source data.
  TConversionMethod = procedure(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte) of object;

  TColorManager = class
  private
    FChanged: Boolean;                 // Set if any of the parameters changed
    FSourceBPS,                        // Bits per sample of source data (allowed values are 1, 2, 4, 8, 16)
    FTargetBPS,                        // Bits per sample of target data (allowed values are 1, 2, 4, 8, 16)
    FSourceSPP,                        // Samples per source pixel (allowed values are 1, 3, 4)
    FTargetSPP: Byte;                  // Samples per target pixel (allowed values are 1, 3, 4)
    FSourceExtraBPP: Byte;             // Set to > 0 if there are extra (unused) bits in a source pixel
    FTargetExtraBPP: Byte;             // Set to > 0 if there are extra (unused) bits in a target pixel
    FSourceMultiBPS: array of Byte;    // Source bits per sample for each channel when not all values are the same (e.g. bmp 565)
    FTargetMultiBPS: array of Byte;    // Target bits per sample for each channel when not all values are the same (e.g. bmp 565)
    FMainGamma,                        // Primary gamma value which is usually read from a file (default is 1)
    FDisplayGamma: Single;             // (Constant) gamma value of the current monitor (default is 2.2)
    FGammaTable: array[Byte] of Byte;  // Contains precalculated gamma values for each possible component value
                                       // (range is 0..255)
    FYCbCrCoefficients: array[0..2] of Single;
    FHSubsampling,
    FVSubSampling: Byte;               // Additional parameters used for YCbCr conversion
    FCrToRedTable,                     // Lookup tables used for YCbCr conversion
    FCbToBlueTable,
    FCrToGreenTable,
    FCbToGreenTable: array of Integer;

    FSourcePaletteFormat: TRawPaletteFormat; // Format of palette data
    FSourcePaletteData: array of Pointer;    // Pointer(s) to palette data

    FSourceDataFormat,
    FTargetDataFormat: TSampleDataFormat;
    FSourceScheme,
    FTargetScheme: TColorScheme;
    FRowConversion: TConversionMethod; // Procedure variable for the actual conversion method used
    FSourceOptions,
    FTargetOptions: TConvertOptions;
    procedure SetSourceOptions(const Value: TConvertOptions);   // Options to control conversion
  protected
    // Low level conversion helper used to convert one pixel component.
    function ComponentGammaConvert(Value: Byte): Byte;
    function ComponentNoConvert16(Value: Word): Word;
    function ComponentNoConvert8(Value: Byte): Byte;
    function ComponentScaleConvert16To8(Value: Word): Byte;
    function ComponentScaleConvert6To8(Value: Word; BitsPerSample: Byte = 6): Byte;
    function ComponentScaleConvert10To8(Value: Word; BitsPerSample: Byte = 10): Byte;
    function ComponentScaleConvert12To8(Value: Word; BitsPerSample: Byte = 12): Byte;
    function ComponentScaleConvert14To8(Value: Word; BitsPerSample: Byte = 14): Byte;
    function ComponentScaleConvertUncommonTo8(Value: Word; BitsPerSample: Byte): Byte;
    function ComponentScaleConvertTo4(Value: Word; BitsPerSample: Byte): Byte;
    function ComponentScaleConvert17_24To8(Value: LongWord; BitsPerSample: Byte): Byte;
    function ComponentScaleConvert25_31To8(Value: LongWord; BitsPerSample: Byte): Byte;
    function ComponentScaleConvert32To8(Value: LongWord; BitsPerSample: Byte): Byte; overload;
    function ComponentScaleConvert32To8(Value: LongWord): Byte; overload;
    function ComponentScaleConvert33_63To8(Value: UInt64; BitsPerSample: Byte): Byte;
    function ComponentScaleConvert64To8(Value: UInt64; BitsPerSample: Byte): Byte; overload;
    function ComponentScaleConvert64To8(Value: UInt64): Byte; overload;
    function ComponentScaleConvertFloat16To8(Value: Word): Byte;
    function ComponentScaleConvertFloat24To8(Value: LongWord; BitsPerSample: Byte): Byte;
    function ComponentScaleConvertFloat32To8(Value: LongWord; BitsPerSample: Byte): Byte; overload;
    function ComponentScaleConvertFloat32To8(Value: LongWord): Byte; overload;
    function ComponentScaleConvertFloat64To8(Value: UInt64; BitsPerSample: Byte): Byte; overload;
    function ComponentScaleConvertFloat64To8(Value: UInt64): Byte; overload;
    function ComponentScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleConvert(Value: Word): Byte;
    function ComponentSwapConvert(Value: Word): Word;

    // Row conversion routines
    procedure RowConvertBGR2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertBGR2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertGray(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexed8(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedBoth16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedSource16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedTarget16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexed2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertGray2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

    // Other general routines
    procedure CreateYCbCrLookup;
    function GetPixelFormat(Index: Integer): TPixelFormat;
    procedure PrepareConversion;
    procedure SetSourceBitsPerSample(const Value: Byte);
    procedure SetSourceColorScheme(const Value: TColorScheme);
    procedure SetSourceSamplesPerPixel(const Value: Byte);
    procedure SetSourceDataFormat(const Value: TSampleDataFormat);
    procedure SetTargetBitsPerSample(const Value: Byte);
    procedure SetTargetColorScheme(const Value: TColorScheme);
    procedure SetTargetSamplesPerPixel(const Value: Byte);
    procedure SetTargetDataFormat(const Value: TSampleDataFormat);
  public
    constructor Create;

    procedure ConvertRow(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    function CreateColorPalette(Data: array of Pointer; DataFormat: TRawPaletteFormat; ColorCount: Cardinal;
      RGB: Boolean): HPALETTE;
    function CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;
    procedure SetGamma(MainGamma: Single; DisplayGamma: Single = DefaultDisplayGamma);
    procedure SetYCbCrParameters(Values: array of Single; HSubSampling, VSubSampling: Byte);
    procedure SetSourcePalette( Data: array of Pointer; PaletteFormat: TRawPaletteFormat; RGB: Boolean = True);
    procedure SetSourceUnequalSamples(ASampleCount: Byte; ASamples: array of Byte);
    procedure SetTargetUnequalSamples(ASampleCount: Byte; ASamples: array of Byte);

    property SourceBitsPerSample: Byte read FSourceBPS write SetSourceBitsPerSample;
    property SourceColorScheme: TColorScheme read FSourceScheme write SetSourceColorScheme;
    property SourceOptions: TConvertOptions read FSourceOptions write SetSourceOptions;
    property SourcePixelFormat: TPixelFormat index 0 read GetPixelFormat;
    property SourceSamplesPerPixel: Byte read FSourceSPP write SetSourceSamplesPerPixel;
    property SourceDataFormat: TSampleDataFormat read FSourceDataFormat write SetSourceDataFormat;
    property SourceExtraBPP: Byte read FSourceExtraBPP write FSourceExtraBPP;
    property TargetBitsPerSample: Byte read FTargetBPS write SetTargetBitsPerSample;
    property TargetColorScheme: TColorScheme read FTargetScheme write SetTargetColorScheme;
    property TargetOptions: TConvertOptions read FTargetOptions write FTargetOptions;
    property TargetPixelFormat: TPixelFormat index 1 read GetPixelFormat;
    property TargetSamplesPerPixel: Byte read FTargetSPP write SetTargetSamplesPerPixel;
    property TargetDataFormat: TSampleDataFormat read FTargetDataFormat write SetTargetDataFormat;
    property TargetExtraBPP: Byte read FTargetExtraBPP write FTargetExtraBPP;
  end;

// Common color conversion functions
function HLStoRGB(const HLS: THLSFloat): TRGBFloat;
function RGBToHLS(const RGB: TRGBFloat): THLSFloat;
function HLSInterpolation(const HLS1, HLS2: THLSFloat; Ratio: Extended): THLSFloat;
function RGBInterpolation(const RGB1, RGB2: TRGBFloat; Ratio: Extended): TRGBFloat; overload;
function RGBInterpolation(const RGB1, RGB2: TRGB; Ratio: Extended): TRGB; overload;

// Convert from CIE L*a*b* to CIE XYZ with D50 white point.
procedure CIELabToXYZ(L, a, b: Extended; out X, Y, Z: Extended);
// Convert from CIE XYZ to 8 bits BGR.
procedure XYZToBGR(X, Y, Z: Extended; bgr: PBGR);
// Convert from CIE XYZ to 8 bits RGB.
procedure XYZToRGB(X, Y, Z: Extended; rgb: PRGB);
// Convert from CIE XYZ to 16 bits BGR.
procedure XYZToBGR16(X, Y, Z: Extended; bgr: PBGR16);
// Convert from CIE XYZ to 16 bits RGB.
procedure XYZToRGB16(X, Y, Z: Extended; rgb: PRGB16);

function  HSVToRGB32(const A, H, S, V: Integer): TRGBAColor32;
procedure RGBToHSV32(const ARGB: TRGBAColor32; var H, S, V: Integer);
function  ToRGBAColor32(const R, G, B, A: Byte): TRGBAColor32;

// Color utility functions
function BrightenColor(const Color: TColor; Amount: Extended): TColor; overload;
function BrightenColor(const Color: TRGB; Amount: Extended): TRGB; overload;
function DarkenColor(const Color: TColor; Amount: Extended): TColor; overload;
function DarkenColor(const Color: TRGB; Amount: Extended): TRGB; overload;
function MakeHLS(const H, L, S: Byte): THLS; overload;
function MakeHLS(const H, L, S: Single): THLSFloat; overload;
function MakeRGB(const R, G, B: Byte): TRGB; overload;
function MakeRGB(const R, G, B: Single): TRGBFloat; overload;

// Convert RGBA (e.g. TIFF sources) to Windows BGRA
procedure RGBAToBGRA(Memory: Pointer; Width, Height: Cardinal);

// Alpha channel functions
// Converts PBGRA Array of length Count into premultiplied BGRA
procedure BGRAToPremultipliedAlpha(Source: PBGRA; Count: Integer);

// Sets all alpha values for PBGRA Array of length Count to 255
procedure BGRASetAlpha255(Source: PBGRA; Count: Integer);
// Sets all alpha values of ABitmap (in BGRA format) to 255
procedure BitmapSetAlpha255(ABitmap: TBitmap);

// Apply an 8 bits alpha mask to Source for Count pixels.
// bpp is Bytes per pixel, valid values:
// 4: 32 bits RGBA/BGRA
// 2: 16 bits grayscale with alpha or indexed with alpha
procedure ApplyAlphaMask( Source, AlphaSource: PByte; const Count: Integer;
  bpp: Integer);


// HalfToFloat and FloatToHalf taken from ImagingLib (also in GlScene)
type
  { 16 bit floating-point value. It has 1 sign bit, 5 exponent bits,
    and 10 mantissa bits.}
  THalfFloat = type Word;
  PHalfFloat = ^THalfFloat;

{ Converts 16bit half floating point value to 32bit Single.}
function HalfToFloat(Half: THalfFloat): Single;
{ Converts 32bit Single to 16bit half floating point.}
function FloatToHalf(Float: Single): THalfFloat;

type // 24 bit float as defined in TIFF
  TFloat24 = packed record
    Mantissa: Word;
    ExponentSign: Byte;
  end;
  PFloat24 = ^TFloat24;

{ Convert 24bit floating point value to 32bit Single. }
function Float24ToFloat(Float24: TFloat24): Single;

// general utility functions
function ClampByte(Value: Integer): Byte;
function MulDiv16(Number, Numerator, Denominator: Word): Word;

function GetBitsMSB(BitIndex, NumberOfBits: Cardinal; BitData: PByte): Cardinal;
function GetBits(BitIndex, NumberOfBits: Cardinal; BitData: PCardinal): Cardinal;

//------------------------------------------------------------------------------

// Moved to interface so that we can check in try except on this exception and handle error reporting ourselves
type
  EColorConversionError = class(Exception);

{$IFDEF FPC}
  // Missing in fpc/lazarus
var
  SystemPalette16: HPalette;
{$ENDIF}

implementation

uses
  Math;


//----------------- Helper functions -------------------------------------------

procedure ShowError(const Msg: String);

begin
  raise EColorConversionError.Create(Msg);
end;

//------------------------------------------------------------------------------

function ClampByte(Value: Integer): Byte;

// Ensures Value is in the range 0..255, values < 0 are clamped to 0 and values > 255 are clamped to 255

asm
         OR EAX, EAX
         JNS @@positive
         XOR EAX, EAX
         RET

@@positive:
         CMP EAX, 255
         JBE @@OK
         MOV EAX, 255
@@OK:
end;

//------------------------------------------------------------------------------

function MulDiv16(Number, Numerator, Denominator: Word): Word;

// Faster equivalent to Windows' MulDiv function
// Number is passed via AX
// Numerator is passed via DX
// Denominator is passed via CX
// Result is passed via AX
// Note: no error checking takes place. Denominator must be > 0!

asm
         MUL DX
         DIV CX
end;

//----------------- Common color conversion functions --------------------------

function HLStoRGB(const HLS: THLSFloat): TRGBFloat;

// Converts from HLS (hue, luminance, saturation) to RGB using floating point math
// Input parameters and result values are all in the range 0..1.

  //--------------- Local function --------------------------------------------

  function HueToRGB(m1, m2, hue: Extended): Extended;

  begin
    if hue > 1 then
      hue := hue - 1
    else
      if hue < 0 then
        hue := hue + 1;

    if 6 * hue < 1 then
      Result := m1 + (m2 - m1) * hue * 6
    else
      if 2 * hue < 1 then
        Result := m2
      else
        if 3 * hue < 2 then
          Result := m1 + (m2 - m1) * (2 / 3 - hue) * 6
        else
          Result := m1;
  end;

  //--------------- End local function ----------------------------------------

var
  m1, m2: Single;

begin
  with HLS, Result do
  begin
    if S = 0 then
    begin
      // Achromatic case (no hue)
      R := L;
      G := L;
      B := L
    end
    else
    begin
      if L <= 0.5 then
        m2 := L * (S + 1)
      else
        m2 := L + S - L * S;
      m1 := 2 * L - m2;

      R := HueToRGB(m1, m2, H + 1 / 3);
      G := HueToRGB(m1, m2, H);
      B := HueToRGB(m1, m2, H - 1 / 3)
    end;
  end;
end;

//------------------------------------------------------------------------------

function RGBToHLS(const RGB: TRGBFloat): THLSFloat;

// Converts from RGB to HLS using floating point math
// Input parameters and result values are all in the range 0..1.

var
  Delta,
  Max,
  Min:  Extended;

begin
  with RGB, Result do
  begin
    Max := MaxValue([R, G, B]);
    Min := MinValue([R, G, B]);

    L := (Max + Min) / 2;

    if Max = Min then
    begin
      // Achromatic case
      S := 0;
      H := 0; // undefined
    end
    else
    begin
      Delta := Max - Min;

      if L < 0.5 then
        S := Delta / (Max + Min)
      else
        S := Delta / (2 - (Max + Min));

      if R = Max then
        H := (G - B) / Delta
      else
        if G = Max then
          H := 2 + (B - R) / Delta
        else
          if B = Max then
            H := 4 + (R - G) / Delta;

      H := H / 6;
      if H < 0 then
        H := H + 1;  

    end
  end;
end;

//------------------------------------------------------------------------------

function HLSInterpolation(const HLS1, HLS2: THLSFloat; Ratio: Extended): THLSFloat;

// Interpolates linearly from HLS1 to HLS2 with the given ratio
// Parameters as well as result are in the range 0..1.

begin
  if Ratio <= 0 then
    Result := HLS1
  else
    if Ratio >= 1 then
      Result := HLS2
    else
    begin
      Result.H := HLS1.H + (HLS2.H - HLS1.H) * Ratio;
      Result.L := HLS1.L + (HLS2.L - HLS1.L) * Ratio;
      Result.S := HLS1.S + (HLS2.S - HLS1.S) * Ratio;
    end;
end;

//------------------------------------------------------------------------------

function RGBInterpolation(const RGB1, RGB2: TRGB; Ratio: Extended): TRGB;

// Interpolates linearly from RGB1 to RGB2 with the given ratio using the HLS color space
// which produces more natural results
// Parameters as well as result are in the range 0..255.

var
  HLS1,
  HLS2: THLSFloat;
  RGB: TRGBFloat;

begin
  if Ratio <= 0 then
    Result := RGB1
  else
    if Ratio >= 1 then
      Result := RGB2
    else
    begin
      HLS1 := RGBToHLS(MakeRGB(RGB1.R / 255, RGB1.G / 255, RGB1.B / 255));
      HLS2 := RGBToHLS(MakeRGB(RGB2.R / 255, RGB2.G / 255, RGB2.B / 255));
      HLS2.H := HLS1.H + (HLS2.H - HLS1.H) * Ratio;
      HLS2.L := HLS1.L + (HLS2.L - HLS1.L) * Ratio;
      HLS2.S := HLS1.S + (HLS2.S - HLS1.S) * Ratio;
      RGB := HLSToRGB(HLS2);
      Result.R := Round(RGB.R * 255);
      Result.G := Round(RGB.G * 255);
      Result.B := Round(RGB.B * 255);
    end;
end;

//------------------------------------------------------------------------------

function RGBInterpolation(const RGB1, RGB2: TRGBFloat; Ratio: Extended): TRGBFloat;

// Interpolates linearly from RGB1 to RGB2 with the given ratio using the HLS color space
// which produces more natural results
// Parameters as well as result are in the range 0..1.

var
  HLS1, HLS2: THLSFloat;

begin
  if Ratio <= 0 then
    Result := RGB1
  else
    if Ratio >= 1 then
      Result := RGB2
    else
    begin
      HLS1 := RGBToHLS(RGB1);
      HLS2 := RGBToHLS(RGB2);
      HLS2.H := HLS1.H + (HLS1.H - HLS2.H) * Ratio;
      HLS2.L := HLS1.L + (HLS1.L - HLS2.L) * Ratio;
      HLS2.S := HLS1.S + (HLS1.S - HLS2.S) * Ratio;
      Result := HLSToRGB(HLS2);
    end;
end;

//------------------------------------------------------------------------------

function  ToRGBAColor32(const R, G, B, A: Byte): TRGBAColor32;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

//-- HSV -----------------------------------------------------------------------

// Converted from the GraphicsMagic versions which are based on Earl F. Grey's versions.
// See GraphicsMagic gmColorSpace but also (in externals) ColorLibrary.pas and RealColorLibrary.pas.

// Floating point fractions, 0..1, replaced with integer values, 0..255.
// Use integer conversion ONLY for one-way, or a single final conversions.
// Use floating-point for converting reversibly (see HSVtoRGB above).
//
// H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
//     0 (undefined) for S = 0
// S = 0 (shade of gray) to 255 (pure color)
// V = 0 (black) to 255 (white)
function HSVToRGB32(const A, H, S, V: Integer): TRGBAColor32;
const
  Divisor: Integer = 255 * 60;
var
  f, hTemp, p, q, t, VS: Integer;
begin
  if S = 0 then
  begin
    Result := ToRGBAColor32(V, V, V, A)  // Achromatic:  shades of gray
  end
  else
  begin                                  // Chromatic color
    if H = 360 then
    begin
      hTemp := 0;
    end
    else
    begin
      hTemp := H;
    end;

    f     := hTemp mod 60;     // f is IN [0, 59]
    hTemp := hTemp div 60;     // h is now IN [0,6)

    VS := V * S;
    p  := V - VS div 255;                   // p = v * (1 - s)
    q  := V - (VS * f) div Divisor;         // q = v * (1 - s * f)
    t  := V - (VS * (60 - f)) div Divisor;  // t = v * (1 - s * (1 - f))

    case hTemp of
      0: Result := ToRGBAColor32(V, t, p, A);
      1: Result := ToRGBAColor32(q, V, p, A);
      2: Result := ToRGBAColor32(p, V, t, A);
      3: Result := ToRGBAColor32(p, q, V, A);
      4: Result := ToRGBAColor32(t, p, V, A);
      5: Result := ToRGBAColor32(V, p, q, A);
    else
      Result := ToRGBAColor32(0, 0, 0, A)  // Should never happen. Avoid compiler warning.
    end;
  end;
end;

// RGB, each 0 to 255, to HSV.
// H = 0 to 360 (corresponding to 0..360 degrees around hexcone)
// S = 0 (shade of gray) to 255 (pure color)
// V = 0 (black) to 255 {white)

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 592.  Floating point fractions, 0..1, replaced with
// integer values, 0..255.

procedure RGBToHSV32(const ARGB: TRGBAColor32; {r, g and b IN [0..255]}
                     var   H, S, V: Integer);  {h IN 0..359; s,v IN 0..255}
var
  Delta, Min, R, G, B: Integer;
begin
  R := ARGB shr 16 and $FF;
  G := ARGB shr 8  and $FF;
  B := ARGB        and $FF;

  Min := MinIntValue([R, G, B]);
  V   := MaxIntValue([R, G, B]);

  Delta := V - Min;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if V =  0 then
  begin
    S := 0;
  end
  else
  begin
    S := MulDiv(Delta, 255, V);
  end;

  if S  = 0 then
  begin
    H := 0; // Achromatic:  When s = 0, h is undefined but assigned the value 0
  end
  else
  begin    // Chromatic
    if R = V then
    begin
      H := MulDiv(G - B, 60, Delta); // Degrees -- between yellow and magenta
    end
    else if G = V then
    begin
      H := 120 + MulDiv(B - R, 60, Delta); // Between cyan and yellow
    end
    else if B = V then
    begin
      H := 240 + MulDiv(R - G, 60, Delta); // Between magenta and cyan
    end;

    if H < 0 then
    begin
      H := H + 360;
    end;
  end;
end;

//------------------------------------------------------------------------------

{
  Conversion of 24 bits Float to 32 bits Float (Single) based on specification in TIFFTN3d1.pdf.
  Conversion function based on the HalfToFloat function below.
  * 24 bit floating point values
  * 24 bit floating point numbers have 1 sign bit, 7 exponent bits (biased by 64),
    and 16 mantissa bits.
  * The interpretation of the sign, exponent and mantissa is analogous to IEEE-754
    floating-point numbers. The 24 bit floating point format supports normalized
    and denormalized numbers, infinities and NANs (Not A Number).

  Bit layout of Float24:

    23 (msb)
    |
    | 22    16
    | |     |
    | |     | 15             0 (lsb)
    | |     | |              |
    X XXXXXXX XXXXXXXXXXXXXXXX
    s e       m

  S is the sign-bit, e is the exponent and m is the significand (mantissa).

}

function Float24ToFloat(Float24: TFloat24): Single;
var
  Dst, Sign, Mantissa: LongWord;
  Exp: LongInt;
begin
  Sign := Float24.ExponentSign shr 7;
  Exp := (Float24.ExponentSign and $7F);
  Mantissa := Float24.Mantissa;

  if (Exp > 0) and (Exp < 127) then
  begin
    // Common normalized number
    Exp := Exp + (127 - 63);
    Mantissa := Mantissa shl 7;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // Zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // Denormalized number - renormalize it
    while (Mantissa and $00010000) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00010000;
    // Now assemble normalized number
    Exp := Exp + (127 - 63);
    Mantissa := Mantissa shl 7;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 127) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else //if (Exp = 127) and (Mantisa <> 0) then
  begin
    // Not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 7);
  end;

  // Reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;


//------------------------------------------------------------------------------

{
  Half/Float conversions inspired by half class from OpenEXR library.

  Float (Pascal Single type) is an IEEE 754 single-precision
  floating point number.

  Bit layout of Single:

    31 (msb)
    |
    | 30     23
    | |      |
    | |      | 22                    0 (lsb)
    | |      | |                     |
    X XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXX
    s e        m

  Bit layout of half:

    15 (msb)
    |
    | 14  10
    | |   |
    | |   | 9        0 (lsb)
    | |   | |        |
    X XXXXX XXXXXXXXXX
    s e     m

  S is the sign-bit, e is the exponent and m is the significand (mantissa).
}

function HalfToFloat(Half: THalfFloat): Single;
var
  Dst, Sign, Mantissa: LongWord;
  Exp: LongInt;
begin
  // Extract sign, exponent, and mantissa from half number
  Sign := Half shr 15;
  Exp := (Half and $7C00) shr 10;
  Mantissa := Half and 1023;

  if (Exp > 0) and (Exp < 31) then
  begin
    // Common normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // Zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // Denormalized number - renormalize it
    while (Mantissa and $00000400) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00000400;
    // Now assemble normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (LongWord(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 31) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else //if (Exp = 31) and (Mantisa <> 0) then
  begin
    // Not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
  end;

  // Reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;

function FloatToHalf(Float: Single): THalfFloat;
var
  Src: LongWord;
  Sign, Exp, Mantissa: LongInt;
begin
  Src := PLongWord(@Float)^;
  // Extract sign, exponent, and mantissa from Single number
  Sign := Src shr 31;
  Exp := LongInt((Src and $7F800000) shr 23) - 127 + 15;
  Mantissa := Src and $007FFFFF;

  if (Exp > 0) and (Exp < 30) then
  begin
    // Simple case - round the significand and combine it with the sign and exponent
    Result := (Sign shl 15) or (Exp shl 10) or ((Mantissa + $00001000) shr 13);
  end
  else if Src = 0 then
  begin
    // Input float is zero - return zero
    Result := 0;
  end
  else
  begin
    // Difficult case - lengthy conversion
    if Exp <= 0 then
    begin
      if Exp < -10 then
      begin
        // Input float's value is less than HalfMin, return zero
        Result := 0;
      end
      else
      begin
        // Float is a normalized Single whose magnitude is less than HalfNormMin.
        // We convert it to denormalized half.
        Mantissa := (Mantissa or $00800000) shr (1 - Exp);
        // Round to nearest
        if (Mantissa and $00001000) > 0 then
          Mantissa := Mantissa + $00002000;
        // Assemble Sign and Mantissa (Exp is zero to get denormalized number)
        Result := (Sign shl 15) or (Mantissa shr 13);
      end;
    end
    else if Exp = 255 - 127 + 15 then
    begin
      if Mantissa = 0 then
      begin
        // Input float is infinity, create infinity half with original sign
        Result := (Sign shl 15) or $7C00;
      end
      else
      begin
        // Input float is NaN, create half NaN with original sign and mantissa
        Result := (Sign shl 15) or $7C00 or (Mantissa shr 13);
      end;
    end
    else
    begin
      // Exp is > 0 so input float is normalized Single

      // Round to nearest
      if (Mantissa and $00001000) > 0 then
      begin
        Mantissa := Mantissa + $00002000;
        if (Mantissa and $00800000) > 0 then
        begin
          Mantissa := 0;
          Exp := Exp + 1;
        end;
      end;

      if Exp > 30 then
      begin
        // Exponent overflow - return infinity half
        Result := (Sign shl 15) or $7C00;
      end
      else
        // Assemble normalized half
        Result := (Sign shl 15) or (Exp shl 10) or (Mantissa shr 13);
    end;
  end;
end;

//----------------- Color utility functions ------------------------------------

function BrightenColor(const Color: TColor; Amount: Extended): TColor;

// Brightens the given RGB color by the given amount using the HLS color model (increasing luminance).
// Amount is a percent value (in the range 0..1) which determines by which amount the source color should
// be brightened.

var
  WinColor: COLORREF;
  HLS: THLSFloat;
  RGB: TRGBFloat;

begin
  WinColor := ColorToRGB(Color);
  HLS := RGBToHLS(MakeRGB((WinColor and $FF) / 255, ((WinColor shr 8) and $FF) / 255, ((WinColor shr 16) and $FF) / 255));
  // Brighten means to increase luminance
  HLS.L := (1 + Amount) * HLS.L;

  RGB := HLSToRGB(HLS);
  Result := Windows.RGB(ClampByte(Round(255 * RGB.R)), ClampByte(Round(255 * RGB.G)), ClampByte(Round(255 * RGB.B)));
end;

//------------------------------------------------------------------------------

function BrightenColor(const Color: TRGB; Amount: Extended): TRGB;

var
  HLS: THLSFloat;
  RGB: TRGBFloat;

begin
  HLS := RGBToHLS(MakeRGB(Color.R / 255, Color.G / 255, Color.B / 255));
  HLS.L := (1 + Amount) * HLS.L;

  RGB := HLSToRGB(HLS);
  Result := MakeRGB(ClampByte(Round(255 * RGB.R)), ClampByte(Round(255 * RGB.G)), ClampByte(Round(255 * RGB.B)));
end;

//------------------------------------------------------------------------------

function DarkenColor(const Color: TColor; Amount: Extended): TColor;

// Darkens the given RGB color by the given amount using the HLS color model (decreasing luminance).
// Amount is a percent value (in the range 0..1) which determines by which amount the source color should
// be darkened.

var
  WinColor: COLORREF;
  HLS: THLSFloat;
  RGB: TRGBFloat;

begin
  WinColor := ColorToRGB(Color);
  HLS := RGBToHLS(MakeRGB((WinColor and $FF) / 255, ((WinColor shr 8) and $FF) / 255, ((WinColor shr 16) and $FF) / 255));
  // Darken means to decrease luminance
  HLS.L := (1 - Amount) * HLS.L;

  RGB := HLSToRGB(HLS);
  Result := Windows.RGB(ClampByte(Round(255 * RGB.R)), ClampByte(Round(255 * RGB.G)), ClampByte(Round(255 * RGB.B)));
end;

//------------------------------------------------------------------------------

function DarkenColor(const Color: TRGB; Amount: Extended): TRGB;

var
  HLS: THLSFloat;
  RGB: TRGBFloat;

begin
  HLS := RGBToHLS(MakeRGB(Color.R / 255, Color.G / 255, Color.B / 255));
  // Darken means to decrease luminance
  HLS.L := (1 - Amount) * HLS.L;

  RGB := HLSToRGB(HLS);
  Result := MakeRGB(ClampByte(Round(255 * RGB.R)), ClampByte(Round(255 * RGB.G)), ClampByte(Round(255 * RGB.B)));
end;

//------------------------------------------------------------------------------

function MakeHLS(const H, L, S: Byte): THLS;

begin
  Result.H := H;
  Result.L := L;
  Result.S := S;
end;

//------------------------------------------------------------------------------

function MakeHLS(const H, L, S: Single): THLSFloat;

begin
  Result.H := H;
  Result.L := L;
  Result.S := S;
end;

//------------------------------------------------------------------------------

function MakeRGB(const R, G, B: Byte): TRGB;

begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

//------------------------------------------------------------------------------

function MakeRGB(const R, G, B: Single): TRGBFloat;

begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;

procedure RGBAToBGRA(Memory: Pointer; Width, Height: Cardinal);
var
  m: PCardinal;
  n: Cardinal;
  o: Cardinal;
begin
  m := Memory;
  for n:=0 to Width*Height-1 do
  begin
    o := m^;
    m^ := (o and $FF00FF00) or             {G and A}
          ((o and $00FF0000) shr 16) or    {B}
          ((o and $000000FF) shl 16);      {R}
    Inc(m);
  end;
end;

procedure BitmapSetAlpha255(ABitmap: TBitmap);
var
  i: Cardinal;
  BgraMem: PCardinal;
  TempBgra: Cardinal;
  Row: Cardinal;
begin
  for Row := 0 to ABitmap.Height-1 do begin
    BgraMem := ABitmap.ScanLine[Row];
    for i := 0 to ABitmap.Width - 1 do
    begin
      TempBgra := BgraMem^;
      BgraMem^ := TempBgra or $FF000000;
      Inc(BgraMem);
    end;
  end;
end;

procedure BGRASetAlpha255(Source: PBGRA; Count: Integer);
var
  i: Cardinal;
  BgraMem: PCardinal;
  TempBgra: Cardinal;
begin
  BgraMem := PCardinal(Source);
  for i := 0 to Count - 1 do
  begin
    TempBgra := BgraMem^;
    BgraMem^ := TempBgra or $FF000000;  // Add alpha $ff
    Inc(BgraMem); // Go to next BGRA
  end;
end;

// Converts PBGRA Array of length Count into premultiplied BGRA
procedure BGRAToPremultipliedAlpha(Source: PBGRA; Count: Integer);
begin
  while Count > 0 do
  begin
    with Source^ do
    begin
      R := MulDiv16(R,A,255);
      G := MulDiv16(G,A,255);
      B := MulDiv16(B,A,255);
    end;
    Inc(Source);
    Dec(Count);
  end;
end;

// Apply an 8 bits alpha mask to Source for Count pixels.
// bpp is Bytes per pixel, valid values:
// 4: 32 bits RGBA/BGRA
// 2: 16 bits grayscale with alpha or indexed with alpha
procedure ApplyAlphaMask( Source, AlphaSource: PByte; const Count: Integer;
  bpp: Integer);
var Skip: Integer;
  i: Integer;
begin
  case bpp of
    4: Skip := 4;
    2: Skip := 2;
  else
    Exit;
  end;
  // Move to alpha component
  Inc(Source, Skip-1);
  // Loop over all pixels
  for i := 0 to Count-1 do begin
    Source^ := MulDiv16(Source^, AlphaSource^, 255);
    Inc(Source, Skip);
    Inc(AlphaSource);
  end;
end;

//----------------- TColorManager ----------------------------------------------

constructor TColorManager.Create;

// Set some default values

begin
  FSourceBPS := 8;
  FTargetBPS := 8;
  FSourceSPP := 3; // 24 bit format
  FTargetSPP := 3; // 24 bit format
  FSourceExtraBPP := 0;
  FTargetExtraBPP := 0;
  SetGamma(1, DefaultDisplayGamma);
  FSourceScheme := csRGB;
  FTargetScheme := csBGR;
  FSourcePaletteFormat := pfUnknown;

  // defaults are from CCIR Recommendation 601-1
  FYCbCrCoefficients[0] := 0.299;
  FYCbCrCoefficients[1] := 0.587;
  FYCbCrCoefficients[2] := 0.114;

  FHSubSampling := 1;
  FVSubSampling := 1;

  FChanged := True;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetSourceOptions(const Value: TConvertOptions);

begin
  if FSourceOptions <> Value then
  begin
    FSourceOptions := Value;
    FChanged := True;
  end;
end;

//----------------- Low level conversion routines ------------------------------

// These routines are used for conversions from 16 to 8 bit values, either with gamma correction or byte swap (or both).

function TColorManager.ComponentNoConvert8(Value: Byte): Byte;

begin
  Result := Value;
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentNoConvert16(Value: Word): Word;

begin
  Result := Value;
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentGammaConvert(Value: Byte): Byte;

begin
  Result := FGammaTable[Value];
end;

//------------------------------------------------------------------------------

const
  // MulDiv Divisor for BitsPerSampe = 0 to 15
  // Note that our MulDiv is Word based therefore we can't handle 16 bits 65536 here!
  CBitsDivisor: array [0..15] of Word =
    (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768);

  // Our MulDiv for 17 bits and up is LongWord based, thus we can't handle 2^32!
  C32BitsDivisor: array [0..31] of LongWord =
    (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
    {16:} 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,
    {24:} 16777216, 33554432, 67108864, 134217728, 268435456, 536870912,
    1073741824, 2147483648);

function TColorManager.ComponentScaleConvert16To8(Value: Word): Byte;

begin
  // JGB 2012-04-12 According to http://www.delphipraxis.net/129051-graphicex-tiff-m-256-farben-laden-falsche-farbe.html
  // and http://support.soft-gems.net/forums/viewtopic.php?t=2213
  // Result := MulDiv16(Value, 255, 65535);
  // should be changed to:
  Result := Value shr 8;
  // Note we can't do: Result := MulDiv16(Value, 256, 65536);
  // Because MulDiv16 uses Word parameters (max 65535)!
end;

function TColorManager.ComponentScaleConvert14To8(Value: Word; BitsPerSample: Byte = 14): Byte;
begin
  // Convert/scale down from 14 bits to 8 bits
  // 14 bits 2^14 = 16384 ==> 8 bits = 256
  Result := MulDiv16(Value, 256, 16384);
end;

function TColorManager.ComponentScaleConvert12To8(Value: Word; BitsPerSample: Byte = 12): Byte;
begin
  // Convert/scale down from 12 bits to 8 bits
  // 12 bits 2^12 = 4096 ==> 8 bits = 256
  Result := MulDiv16(Value, 256, 4096);
end;

function TColorManager.ComponentScaleConvert10To8(Value: Word; BitsPerSample: Byte = 10): Byte;
begin
  // Convert/scale down from 10 bits to 8 bits
  // 10 bits 2^10 = 1024 ==> 8 bits = 256
  Result := MulDiv16(Value, 256, 1024);
end;

function TColorManager.ComponentScaleConvert6To8(Value: Word; BitsPerSample: Byte = 6): Byte;
begin
  // Convert/scale up from 6 bits to 8 bits
  // 10 bits 2^10 = 1024 ==> 8 bits = 256
  Result := MulDiv16(Value, 256, 64);
end;

function TColorManager.ComponentScaleConvertUncommonTo8(Value: Word; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from uncommmon n bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  // Todo: split into 2 functions: 1 one for scaling down and one for scaling up
  if Value = 0 then
    Result := 0
  else if BitsPerSample <= 8 then
    // Scale up to 8 bits
    Result := (Value+1) shl (8-BitsPerSample) - 1
  else // Scale down to 8 bits
    Result := Value shr (BitsPerSample-8);
end;

function TColorManager.ComponentScaleConvertTo4(Value: Word; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from uncommmon n bits to 4 bits
  // n bits 2^n = ... ==> 4 bits = 16
  // Todo: split into 2 functions: 1 one for scaling down and one for scaling up
  if Value = 0 then
    Result := 0
  else if BitsPerSample <= 4 then
    // Scale up to 4
    Result := (Value+1) shl (4-BitsPerSample) - 1
  else // Scale down to 4
    Result := Value shr (BitsPerSample-4);
end;

function TColorManager.ComponentScaleConvert17_24To8(Value: LongWord; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from uncommmon n bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  //Result := MulDiv(Value, 256, C32BitsDivisor[BitsPerSample]);
  // This gives a better result, not sure why:
  Result := Value shr (BitsPerSample-8);
end;

function TColorManager.ComponentScaleConvert25_31To8(Value: LongWord; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from 25..31 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  Result := Value shr (BitsPerSample-8);
end;

function TColorManager.ComponentScaleConvert32To8(Value: LongWord; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from 32 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  Result := Value shr 24;
end;

function TColorManager.ComponentScaleConvert32To8(Value: LongWord): Byte;
begin
  // Convert/scale up or down from 32 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  // Note: this version is used for RGB color conversion and this one does
  // need to be shifted 24 bits unlike the version above!
  Result := Value shr 24;
end;

function TColorManager.ComponentScaleConvert33_63To8(Value: UInt64; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from 33..63 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  Result := Value shr (BitsPerSample-8);
end;

function TColorManager.ComponentScaleConvert64To8(Value: UInt64; BitsPerSample: Byte): Byte;
begin
  // Convert/scale up or down from 64 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  Result := Byte(Value shr 56);
end;

function TColorManager.ComponentScaleConvert64To8(Value: UInt64): Byte;
begin
  // Convert/scale up or down from 64 bits to 8 bits
  // n bits 2^n = ... ==> 8 bits = 256
  // Note Delphi 6 has only signed Int64 (which we are hiding here by using our
  // own defined UInt64 = Int64). Shifting for Int64 preserves sign which means
  // we can't use normal shr 56 for 64 bits. However since shifting in this case
  // just returns the highest order byte as result we just grab that byte.
  Result := PByteArray(@Value)^[7];
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat16To8(Value: Word): Byte;
var hf: THalfFloat absolute Value;
  TempVal: Integer;
begin
  TempVal := Trunc(HalfToFloat(hf) * 255);
  if TempVal < 0 then
    TempVal := 0
  else if TempVal > 255 then
    TempVal := 255;
  Result := TempVal;
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat24To8(Value: LongWord; BitsPerSample: Byte): Byte;
var f24: TFloat24;
  TempVal: Integer;
begin
//  f24 := PFloat24(@PByteArray(@Value)^[1])^;
  f24 := PFloat24(@Value)^;
  TempVal := Trunc(Float24ToFloat(f24) * 255);
  if TempVal < 0 then
    TempVal := 0
  else if TempVal > 255 then
    TempVal := 255;
  Result := TempVal;
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat32To8(Value: LongWord; BitsPerSample: Byte): Byte;
var s: Single absolute Value;
  TempVal: Integer;
begin
  TempVal := Trunc(s * 255);
  if TempVal < 0 then
    TempVal := 0
  else if TempVal > 255 then
    TempVal := 255;
  Result := TempVal;
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat32To8(Value: LongWord): Byte;
var s: Single absolute Value;
  TempVal: Integer;
begin
  TempVal := Trunc(s * 255);
  if TempVal < 0 then
    TempVal := 0
  else if TempVal > 255 then
    TempVal := 255;
  Result := TempVal;
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat64To8(Value: UInt64; BitsPerSample: Byte): Byte;
var d: Double absolute Value;
  TempVal: UInt64;
begin
    TempVal := Trunc(d * 255);
    if TempVal < 0 then
      TempVal := 0
    else if TempVal > 255 then
      TempVal := 255;
    Result := TempVal;
end;

// WARNING: Currently assuming values between 0.0 and 1.0!
function TColorManager.ComponentScaleConvertFloat64To8(Value: UInt64): Byte;
var d: Double absolute Value;
  TempVal: UInt64;
begin
    TempVal := Trunc(d * 255);
    if TempVal < 0 then
      TempVal := 0
    else if TempVal > 255 then
      TempVal := 255;
    Result := TempVal;
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentScaleGammaConvert(Value: Word): Byte;

begin
  Result := FGammaTable[MulDiv16(Value, 255, 65535)];
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentSwapScaleGammaConvert(Value: Word): Byte;

begin
  Result := FGammaTable[MulDiv16(Swap(Value), 255, 65535)];
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentSwapScaleConvert(Value: Word): Byte;

begin
  Result := MulDiv16(Swap(Value), 255, 65535);
end;

//------------------------------------------------------------------------------

function TColorManager.ComponentSwapConvert(Value: Word): Word;

begin
  Result := Swap(Value);
end;

//----------------- Row conversion routines ------------------------------------

// Notes: Each method takes parameters for source and target data as well as the count of pixels to work on. This count
//        determines the number of pixels in the target buffer. The actual source count may differ for special color
//        schemes (like YCbCr) or interlaced lines.
//        Mask is a parameter which determines (in a repeative manner) which source pixel should actually be transferred
//        to the target buffer. A 1 in the corresponding bit (MSB is leftmost pixel) causes the transfer to happen.
//        Usually, this parameter is $FF to transfer all pixels, but for interlaced images (e.g. as in PNG format)
//        this will differ to limit pixel transfers. The bit mask only describes which target pixel is to skip. Source
//        pixel must be packed.
//        Windows DIBs are always byte aligned, so we don't need checks for byte alignments (in target).

procedure TColorManager.RowConvertBGR2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// same as ConvertBGR2RGB but for BGR target schemes

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceB8 := Source[0];
          SourceG8 := SourceB8; Inc(SourceG8);
          SourceR8 := SourceG8; Inc(SourceR8);
          SourceA8 := SourceR8; Inc(SourceA8);
        end
        else
        begin
          SourceB8 := Source[0];
          SourceG8 := Source[1];
          SourceR8 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA8 := Source[3]
          else
            SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceB16 := Source[0];
          SourceG16 := SourceB16; Inc(SourceG16);
          SourceR16 := SourceG16; Inc(SourceR16);
          SourceA16 := SourceR16; Inc(SourceA16);
        end
        else
        begin
          SourceB16 := Source[0];
          SourceG16 := Source[1];
          SourceR16 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA16 := Source[3]
          else
            SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleGammaConvert
                else
                  Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleConvert
                else
                  Convert16_8 := ComponentScaleConvert16To8;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then
                Convert16_8Alpha := ComponentSwapScaleConvert
              else
                Convert16_8Alpha := ComponentScaleConvert16To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA16 := Source[3]
                else
                  SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColorManager.RowConvertBGR2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts BGR source schemes to RGB target schemes and takes care for byte swapping, alpha copy/skip and
// gamma correction. 

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // interleaved mode
          SourceB8 := Source[0];
          SourceG8 := SourceB8; Inc(SourceG8);
          SourceR8 := SourceG8; Inc(SourceR8);
          SourceA8 := SourceR8; Inc(SourceA8);
        end
        else
        begin
          SourceB8 := Source[0];
          SourceG8 := Source[1];
          SourceR8 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA8 := Source[3]
          else
            SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceB16 := Source[0];
          SourceG16 := SourceB16; Inc(SourceG16);
          SourceR16 := SourceG16; Inc(SourceR16);
          SourceA16 := SourceR16; Inc(SourceA16);
        end
        else
        begin
          SourceB16 := Source[0];
          SourceG16 := Source[1];
          SourceR16 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA16 := Source[3]
          else
            SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleGammaConvert
                else
                  Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleConvert
                else
                  Convert16_8 := ComponentScaleConvert16To8;
              end;
              // since alpha channels are never gamma corrected we need a separate conversion routine
              if coNeedByteSwap in FSourceOptions then
                Convert16_8Alpha := ComponentSwapScaleConvert
              else
                Convert16_8Alpha := ComponentScaleConvert16To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);
                  
                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // no gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA16 := Source[3]
                else
                  SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    1..7: ;
    9..15: ;
  end;
end;

//------------------------------------------------------------------------------

const
  // From LibTiff tif_aux.c:
  // TIFF 6.0 specification tells that it is no default value for the WhitePoint,
  // but AdobePhotoshop TIFF Technical Note tells that it should be CIE D50.

  // Observer= 2�, Illuminant= D50
  ref_X =  96.422;
  ref_Y = 100.000;
  ref_Z =  82.521;

  // Use a const for 16 / 116 to improve speed.
  _16_116 = 16.0 / 116.0;

// Convert from CIE L*a*b* to CIE XYZ with D50 white point.
// http://www.easyrgb.com/index.php?X=MATH&H=08#text8
// To improve speed only compute Power3 when needed: 0.206893^3 = 0.008856
procedure CIELabToXYZ(L, a, b: Extended; out X, Y, Z: Extended);
var vX, vY, vZ: Extended;
begin
  vY := (L + 16 ) / 116;
  vX := a / 500 + vY;
  vZ := vY - b / 200;

  if vY > 0.206893 then
    vY := Power(vY, 3)
  else
    vY := (vY - _16_116) / 7.787;

  if vX > 0.206893 then
    vX := Power(vX, 3)
  else
    vX := (vX - _16_116) / 7.787;

  if vZ > 0.206893 then
    vZ := Power(vZ, 3)
  else
    vZ := (vZ - _16_116) / 7.787;

  X := ref_X * vX;
  Y := ref_Y * vY;
  Z := ref_Z * vZ;

  { Original GraphicEx conversion (using D65 white point)
  YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
  if L < 7.9996 then
  begin
    Y := L / 903.3;
    X := a / 3893.5 + Y;
    Z := Y - b / 1557.4;
  end
  else
  begin
    T := YYn3 + a / 500;
    X := T * T * T;
    Y := YYn3 * YYn3 * YYn3;
    T := YYn3 - b / 200;
    Z := T * T * T;
  end; }
end;

// Convert from CIE XYZ to BGR (Extended Float, range 0-1).
// http://www.easyrgb.com/index.php?X=MATH&H=01#text1
procedure XYZToBGRExtended(X, Y, Z: Extended; out b, g, r: Extended);
var vr, vg, vb: Extended;
begin
  X := X / 100;
  Y := Y / 100;
  Z := Z / 100;
  vr := X *  3.2406 + Y * -1.5372 + Z * -0.4986;
  vg := X * -0.9689 + Y *  1.8758 + Z *  0.0415;
  vb := X *  0.0557 + Y * -0.2040 + Z *  1.0570;

  if vr > 0.0031308 then
    r := 1.055 * Power(vr, 1/2.4) - 0.055
  else
    r := 12.92 * vr;
  if vg > 0.0031308 then
    g := 1.055 * Power(vg, 1/2.4) - 0.055
  else
    g := 12.92 * vg;
  if vb > 0.0031308 then
    b := 1.055 * Power(vb, 1/2.4) - 0.055
  else
    b := 12.92 * vb;
end;

// Convert from CIE XYZ to 8 bits BGR.
procedure XYZToBGR(X, Y, Z: Extended; bgr: PBGR);
var vr, vg, vb: Extended;
begin
  XYZToBGRExtended(X, Y, Z, vb, vg, vr);

  bgr^.B := ClampByte(Round(255 * vb));
  bgr^.G := ClampByte(Round(255 * vg));
  bgr^.R := ClampByte(Round(255 * vr));

  { Original GraphicEx conversion
  // blue
  bgr^.B := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
  // green
  bgr^.G := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
  // red
  bgr^.R := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
  }
end;

// Convert from CIE XYZ to 8 bits RGB.
procedure XYZToRGB(X, Y, Z: Extended; rgb: PRGB);
var vr, vg, vb: Extended;
begin
  XYZToBGRExtended(X, Y, Z, vb, vg, vr);

  rgb^.R := ClampByte(Round(255 * vr));
  rgb^.G := ClampByte(Round(255 * vg));
  rgb^.B := ClampByte(Round(255 * vb));

  { Original GraphicEx conversion
  // red
  Target8^ := ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
  Inc(Target8);
  // green
  Target8^ := ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
  Inc(Target8);
  // blue
  Target8^ := ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
  }
end;

function ClampWord(w: Integer): Word;
begin
  if w < 0 then
    Result := 0
  else if w > MaxWord then
    Result := MaxWord
  else
    Result := w;
end;

// Convert from CIE XYZ to 16 bits BGR.
procedure XYZToBGR16(X, Y, Z: Extended; bgr: PBGR16);
var vr, vg, vb: Extended;
begin
  XYZToBGRExtended(X, Y, Z, vb, vg, vr);

  bgr^.B := ClampWord(Round(65535 * vb));
  bgr^.G := ClampWord(Round(65535 * vg));
  bgr^.R := ClampWord(Round(65535 * vr));

  { Original GraphicEx conversion
  // blue
  Target16^ := MulDiv16(ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), 65535, 255);
  Inc(Target16);
  // green
  Target16^ := MulDiv16(ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z))), 65535, 255);
  Inc(Target16);
  // red
  Target16^ := MulDiv16(ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), 65535, 255);
  }
end;

// Convert from CIE XYZ to 16 bits RGB.
procedure XYZToRGB16(X, Y, Z: Extended; rgb: PRGB16);
var vr, vg, vb: Extended;
begin
  XYZToBGRExtended(X, Y, Z, vb, vg, vr);

  rgb^.R := ClampWord(Round(65535 * vr));
  rgb^.G := ClampWord(Round(65535 * vg));
  rgb^.B := ClampWord(Round(65535 * vb));

  { Original GraphicEx conversion
  // red
  Target16^ := MulDiv16(ClampByte(Round(255 * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), 65535, 255);
  // green
  Target16^ := MulDiv16(ClampByte(Round(255 * (-0.952 * X + 1.893 * Y + 0.059 * Z))), 65535, 255);
  Inc(Target16);
  // blue
  Target16^ := MulDiv16(ClampByte(Round(255 * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), 65535, 255);
  Inc(Target16);
  }
end;

procedure TColorManager.RowConvertCIELAB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Conversion of the CIE L*a*b* color space to BGR using a two way approach
// first a conversion to CIE XYZ is performed and then from there to RGB

var
  LRun8,
  aRun8,
  bRun8: PByte;
  LRun16,
  aRun16,
  bRun16: PWord;
  L, a, b,
  X, Y, Z: Extended; // Color values in float format
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  // TODO: transfer alpha value.
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          LRun8 := Source[0];
          aRun8 := LRun8; Inc(aRun8);
          bRun8 := aRun8; Inc(bRun8);
          Increment := 3;
        end
        else
        begin
          LRun8 := Source[0];
          aRun8 := Source[1];
          bRun8 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: /// 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun8^ / 2.55
                  else
                    L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToBGR(X, Y, Z, PBGR(Target8));
                  Inc(Target8, 3 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun8^ / 2.55
                  else
                    L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToBGR16(X, Y, Z, PBGR16(Target16));
                  Inc(Target16, 3 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          LRun16 := Source[0];
          aRun16 := LRun16; Inc(aRun16);
          bRun16 := aRun16; Inc(bRun16);
          Increment := 3;
        end
        else
        begin
          LRun16 := Source[0];
          aRun16 := Source[1];
          bRun16 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun16^ / 655.35
                  else
                    L := LRun16^;
                  Inc(LRun16, Increment);

                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ shr 8 - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ shr 8 - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    // Need to convert to ShortInt since it should be in range -128 to 128
                    a := ShortInt(aRun16^ shr 8); // MulDiv(aRun16^, 256, 65536);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^ shr 8); // MulDiv(bRun16^, 256, 65536);
                    Inc(bRun16, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToBGR(X, Y, Z, PBGR(Target8));
                  Inc(Target8, 3 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun16^ / 655.35
                  else
                    L := LRun16^;
                  Inc(LRun16, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ shr 8 - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ shr 8 - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    // Need to convert to ShortInt since it should be in range -128 to 128
                    a := ShortInt(aRun16^ shr 8);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^ shr 8);
                    Inc(bRun16, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToBGR16(X, Y, Z, PBGR16(Target16));
                  Inc(Target16, 3 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertCIELAB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Just like RowConvertCIELAB2BGR but for RGB target schemes

var
  LRun8,
  aRun8,
  bRun8: PByte;
  LRun16,
  aRun16,
  bRun16: PWord;
  L, a, b,
  X, Y, Z: Extended; // Color values in float format
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          LRun8 := Source[0];
          aRun8 := LRun8; Inc(aRun8);
          bRun8 := aRun8; Inc(bRun8);
          Increment := 3;
        end
        else
        begin
          LRun8 := Source[0];
          aRun8 := Source[1];
          bRun8 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: /// 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun8^ / 2.55
                  else
                    L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToRGB(X, Y, Z, PRGB(Target8));
                  Inc(Target8, 3 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun8^ / 2.55
                  else
                    L := LRun8^;
                  Inc(LRun8, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun8^ - 128;
                    Inc(aRun8, Increment);
                    b := bRun8^ - 128;
                    Inc(bRun8, Increment);
                  end
                  else
                  begin
                    a := ShortInt(aRun8^);
                    Inc(aRun8, Increment);
                    b := ShortInt(bRun8^);
                    Inc(bRun8, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToRGB16(X, Y, Z, PRGB16(Target16));
                  Inc(Target16, 3 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          LRun16 := Source[0];
          aRun16 := LRun16; Inc(aRun16);
          bRun16 := aRun16; Inc(bRun16);
          Increment := 3;
        end
        else
        begin
          LRun16 := Source[0];
          aRun16 := Source[1];
          bRun16 := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun16^ / 655.35
                  else
                    L := LRun16^;
                  Inc(LRun16, Increment);

                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ shr 8 - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ shr 8 - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    // Need to convert to ShortInt since it should be in range -128 to 128
                    a := ShortInt(aRun16^ shr 8);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^ shr 8);
                    Inc(bRun16, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToRGB(X, Y, Z, PRGB(Target8));
                  Inc(Target8, 3 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  if coLabByteRange in FSourceOptions then
                    L := LRun16^ / 655.35
                  else
                    L := LRun16^;
                  Inc(LRun16, Increment);
                  if coLabChromaOffset in FSourceOptions then
                  begin
                    a := aRun16^ shr 8 - 128;
                    Inc(aRun16, Increment);
                    b := bRun16^ shr 8 - 128;
                    Inc(bRun16, Increment);
                  end
                  else
                  begin
                    // Need to convert to ShortInt since it should be in range -128 to 128
                    a := ShortInt(aRun16^ shr 8);
                    Inc(aRun16, Increment);
                    b := ShortInt(bRun16^ shr 8);
                    Inc(bRun16, Increment);
                  end;

                  // Convert from CIE L*a*b* to CIE XYZ
                  CIELabToXYZ(L, a, b, X, Y, Z);

                  // Once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
                  XYZToRGB16(X, Y, Z, PRGB16(Target16));
                  Inc(Target16, 3 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertCMYK2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts a stream of Count CMYK values to BGR

var
  C8, M8, Y8, K8, A8: PByte;
  C16, M16, Y16, K16, A16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  if coSeparatePlanes in FSourceOptions then begin
    // Separate planes
    Increment := 1;
  end
  else begin
    // Increment also needs to take into account that we might have unknown
    // extrasamples
    Increment := SourceSamplesPerPixel;
  end;

  case FSourceBPS of
    8:
      begin
        if coSeparatePlanes in FSourceOptions then
        begin
          // Plane mode
          C8 := Source[0];
          M8 := Source[1];
          Y8 := Source[2];
          K8 := Source[3];
          if coAlpha in FSourceOptions then
            A8 := Source[4]
          else
            A8 := nil;
        end
        else
        begin
          // Interleaved mode
          C8 := Source[0];
          M8 := C8; Inc(M8);
          Y8 := M8; Inc(Y8);
          K8 := Y8; Inc(K8);
          if coAlpha in FSourceOptions then begin
            A8 := K8; Inc(A8);
          end
          else begin
            A8 := nil;
          end;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target8^ := ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^));
                  Inc(Target8);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target8^ := A8^;
                      Inc(A8, Increment);
                    end
                    else begin
                      // Add opaque $FF as alpha when source is without alpha
                      Target8^ := $FF;
                    end;
                    Inc(Target8);
                  end;

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target16^ := MulDiv16(ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target16^ := A8^ shr 8; // From max 255 to max 65535
                      Inc(A8, Increment);
                    end
                    else begin
                      // Add opaque $FFFF as alpha when source is without alpha
                      Target16^ := $FFFF;
                    end;
                    Inc(Target16);
                  end;

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if coSeparatePlanes in FSourceOptions then
        begin
          // Plane mode
          C16 := Source[0];
          M16 := Source[1];
          Y16 := Source[2];
          K16 := Source[3];
          if coAlpha in FSourceOptions then
            A16 := Source[4]
          else
            A16 := nil;
        end
        else
        begin
          // Interleaved mode
          C16 := Source[0];
          M16 := C16; Inc(M16);
          Y16 := M16; Inc(Y16);
          K16 := Y16; Inc(K16);
          if coAlpha in FSourceOptions then begin
            A16 := K16; Inc(A16);
          end
          else begin
            A16 := nil;
          end;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target8^ := ClampByte(255 - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - MulDiv16((M16^ - MulDiv16(M16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(255 - MulDiv16((C16^ - MulDiv16(C16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target8^ := A16^ shl 8; // From max 65535 to max 255
                      Inc(A16, Increment);
                    end
                    else begin
                      // Add opaque $FF as alpha when source is without alpha
                      Target8^ := $FF;
                    end;
                    Inc(Target8);
                  end;

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // blue
                  Target16^ := 65535 - (Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // green
                  Target16^ := 65535 - (M16^ - MulDiv16(M16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // blue
                  Target16^ := 65535 - (C16^ - MulDiv16(C16^, K16^, 65535) + K16^);
                  Inc(Target16);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target16^ := A16^;
                      Inc(A16, Increment);
                    end
                    else begin
                      // Add opaque $FFFF as alpha when source is without alpha
                      Target16^ := $FFFF;
                    end;
                    Inc(Target16);
                  end;

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertCMYK2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts a stream of Count CMYK values to RGB,

var
  C8, M8, Y8, K8, A8: PByte;
  C16, M16, Y16, K16, A16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  if coSeparatePlanes in FSourceOptions then begin
    // Separate planes
    Increment := 1;
  end
  else begin
    // Increment also needs to take into account that we might have unknown
    // extrasamples
    Increment := SourceSamplesPerPixel;
  end;

  case FSourceBPS of
    8:
      begin
        if coSeparatePlanes in FSourceOptions then
        begin
          // Plane mode
          C8 := Source[0];
          M8 := Source[1];
          Y8 := Source[2];
          K8 := Source[3];
          if coAlpha in FSourceOptions then
            A8 := Source[4]
          else
            A8 := nil;
        end
        else
        begin
          // Interleaved mode
          C8 := Source[0];
          M8 := C8; Inc(M8);
          Y8 := M8; Inc(Y8);
          K8 := Y8; Inc(K8);
          if coAlpha in FSourceOptions then begin
            A8 := K8; Inc(A8);
          end
          else begin
            A8 := nil;
          end;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target8^ := ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^));
                  Inc(Target8);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target8^ := A8^;
                      Inc(A8, Increment);
                    end
                    else begin
                      // Add opaque $FF as alpha when source is without alpha
                      Target8^ := $FF
                    end;
                    Inc(Target8);
                  end;

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target16^ := MulDiv16(ClampByte(255 - (C8^ - MulDiv16(C8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(255 - (M8^ - MulDiv16(M8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(255 - (Y8^ - MulDiv16(Y8^, K8^, 255) + K8^)), 65535, 255);
                  Inc(Target16);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target16^ := A8^ shr 8; // From max 255 to max 65535
                      Inc(A8, Increment);
                    end
                    else begin
                      // Add opaque $FFFF as alpha when source is without alpha
                      Target16^ := $FFFF;
                    end;
                    Inc(Target16);
                  end;

                  Inc(C8, Increment);
                  Inc(M8, Increment);
                  Inc(Y8, Increment);
                  Inc(K8, Increment);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if coSeparatePlanes in FSourceOptions then
        begin
          // Plane mode
          C16 := Source[0];
          M16 := Source[1];
          Y16 := Source[2];
          K16 := Source[3];
          if coAlpha in FSourceOptions then
            A16 := Source[4]
          else
            A16 := nil;
        end
        else
        begin
          // Interleaved mode
          C16 := Source[0];
          M16 := C16; Inc(M16);
          Y16 := M16; Inc(Y16);
          K16 := Y16; Inc(K16);
          if coAlpha in FSourceOptions then begin
            A16 := K16; Inc(A16);
          end
          else begin
            A16 := nil;
          end;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target8^ := ClampByte(255 - MulDiv16((C16^ - MulDiv16(C16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(255 - MulDiv16((M16^ - MulDiv16(M16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(255 - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^), 255, 65535));
                  Inc(Target8);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target8^ := A16^ shl 8; // From max 65535 to max 255
                      Inc(A16, Increment);
                    end
                    else begin
                      // Add opaque $FF as alpha when source is without alpha
                      Target8^ := $FF;
                    end;
                    Inc(Target8);
                  end;

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  // red
                  Target16^ := 65535 - (C16^ - MulDiv16(C16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // green
                  Target16^ := 65535 - (M16^ - MulDiv16(M16^, K16^, 65535) + K16^);
                  Inc(Target16);
                  // blue
                  Target16^ := 65535 - (Y16^ - MulDiv16(Y16^, K16^, 65535) + K16^);
                  Inc(Target16);

                  if coAlpha in FTargetOptions then begin
                    if coAlpha in FSourceOptions then begin
                      Target16^ := A16^;
                      Inc(A16, Increment);
                    end
                    else begin
                      // Add opaque $FFFF as alpha when source is without alpha
                      Target16^ := $FFFF;
                    end;
                    Inc(Target16);
                  end;

                  Inc(C16, Increment);
                  Inc(M16, Increment);
                  Inc(Y16, Increment);
                  Inc(K16, Increment);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

const
  CBitMask: array [0..17] of Cardinal = (0, $00000001, $00000003, $00000007,
    $0000000F, $0000001F, $0000003F, $0000007F, $000000FF, $000001FF,
    $000003FF, $000007FF, $00000FFF, $00001FFF, $00003FFF, $00007FFF,
    $0000FFFF, $0001FFFF);

// Adapted from GraphicsMagick bit_stream.c and others
// TODO: Maybe change to a bitstream and update the bit and byte pointers
// here and not in the caller
function GetBitsMSB(BitIndex, NumberOfBits: Cardinal; BitData: PByte): Cardinal;
var remaining_quantum_bits,
    octet_bits,
    bits_remaining,
    quantum: Cardinal;
    ShiftBits: Cardinal;
begin
  quantum := 0;
  if (BitIndex = 0) and (NumberOfBits mod 8 = 0) then begin
    // TODO: It would be better for speed to split this case off in a separate
    // function and determine once in the caller what function to use based on
    // the number of bits per sample!
    // Most common and simple case
    ShiftBits := 0;
    while NumberOfBits <> 0 do begin
      quantum := quantum or BitData^ shl ShiftBits;
      Inc(BitData);
      Inc(ShiftBits, 8);
      Dec(NumberOfBits, 8);
    end;
    Result := quantum;
    Exit;
  end;
  remaining_quantum_bits := NumberOfBits;
  bits_remaining := 8-BitIndex;
  while (remaining_quantum_bits <> 0) do begin
    octet_bits := remaining_quantum_bits;
    if octet_bits > bits_remaining then
      octet_bits := bits_remaining;

      remaining_quantum_bits := remaining_quantum_bits - octet_bits;
      bits_remaining := bits_remaining - octet_bits;

      quantum := (quantum shl octet_bits) or
	((BitData^ shr (bits_remaining)) and CBitMask[octet_bits]);

      if (bits_remaining = 0) then begin
	  Inc(BitData);
	  bits_remaining := 8;
      end;
  end;
  Result := quantum;
end;

const
  CBitMask64: array [0..17] of UInt64 = (0, $00000001, $00000003, $00000007,
    $0000000F, $0000001F, $0000003F, $0000007F, $000000FF, $000001FF,
    $000003FF, $000007FF, $00000FFF, $00001FFF, $00003FFF, $00007FFF,
    $0000FFFF, $0001FFFF);

function GetBitsMSB64(BitIndex, NumberOfBits: Cardinal; BitData: PByte): UInt64;
var remaining_quantum_bits,
    octet_bits,
    bits_remaining: Cardinal;
    quantum: UInt64;
    ShiftBits: Cardinal;
begin
  quantum := 0;
  if (BitIndex = 0) and (NumberOfBits mod 8 = 0) then begin
    // TODO: It would be better for speed to split this case off in a separate
    // function and determine once in the caller what function to use based on
    // the number of bits per sample!
    // Most common and simple case
    if NumberOfBits < 64 then begin
      ShiftBits := 0;
      while NumberOfBits <> 0 do begin
        quantum := quantum or BitData^ shl ShiftBits;
        Inc(BitData);
        Inc(ShiftBits, 8);
        Dec(NumberOfBits, 8);
      end;
    end
    else begin
      // Delphi 6 doesn't have unsigned Int64, can't use shifting because it preserves sign
      // Thus we just return the 8 bytes here.
      quantum := PUInt64(BitData)^;
    end;
    Result := quantum;
    Exit;
  end;
  remaining_quantum_bits := NumberOfBits;
  bits_remaining := 8-BitIndex;
  while (remaining_quantum_bits <> 0) do begin
    octet_bits := remaining_quantum_bits;
    if octet_bits > bits_remaining then
      octet_bits := bits_remaining;

      remaining_quantum_bits := remaining_quantum_bits - octet_bits;
      bits_remaining := bits_remaining - octet_bits;

      quantum := (quantum shl octet_bits) or
	((BitData^ shr (bits_remaining)) and CBitMask[octet_bits]);

      if (bits_remaining = 0) then begin
	  Inc(BitData);
	  bits_remaining := 8;
      end;
  end;
  Result := quantum;
end;

// Function to return 64 bits Double as UInt64.
// Since apparently for TIFF float values we don't need to handle BigEndian we
// just return the value where BitData points to without any handling.
function GetBitsDouble(BitIndex, NumberOfBits: Cardinal; BitData: PByte): UInt64;
begin
  Result := PUInt64(BitData)^;
end;

// Function to return 32 bits Single as Cardinal
function GetBitsSingle(BitIndex, NumberOfBits: Cardinal; BitData: PByte): Cardinal;
begin
  Result := PCardinal(BitData)^;
end;

function GetBits(BitIndex, NumberOfBits: Cardinal; BitData: PCardinal): Cardinal;
begin
  // TODO Copy available bytes first so we dont get AV at end of buffer!
  // Since 10 or more bits can be spread over 3 bytes we use a 4 byte Cardinal
  // NOT USED CURRENTLY since we found out we need to do it with MSB first (big endian)
  Result := (BitData^ shr BitIndex) and CBitMask[NumberOfBits];
end;

procedure TColorManager.RowConvertGray(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Conversion from source grayscale (possibly with alpha) to target grayscale
// Note: Currently this also handles 8 bps source/target Indexed with alpha!
// Note: For indexed and grayscale with alpha the alpha channel is skipped/removed!
// Note: Since grayscale is basically handled like indexed mode (palette), there is no need to
//       handle gamma correction here as this happend already during palette creation.

var
  Target8: PByte;
  Target16: PWord;
  Source8: PByte;
  Source16: PWord;
  Source32: PLongWord;
  Source64: PUInt64;
  BitRun: Byte;
  AlphaSkip: Integer;
  Convert16: function(Value: Word): Byte of object;
  ConvertAny: function(Value: Word; BitsPerSampe: Byte): Byte of object;
  Convert32: function(Value: LongWord; BitsPerSampe: Byte): Byte of object;
  Convert64: function(Value: UInt64; BitsPerSampe: Byte): Byte of object;
  BitOffset: Cardinal; // Offset in Source byte where first bit starts
  BitIncrement: Cardinal; // Value to increment bits with, depends on FSourceBPS and AlphaSkip
  Bits, Bits2: Cardinal;
  Bits64: UInt64;
  FirstNibble: Boolean;
  GetBits32: function(BitIndex, NumberOfBits: Cardinal; BitData: PByte): Cardinal;
  GetBits64: function(BitIndex, NumberOfBits: Cardinal; BitData: PByte): UInt64;
begin
  BitRun := $80;
  // When this is an image with alpha and not planar we need to skip the alpha bits
  // Tiff can have extrasamples that is not an alpha channel. That's why testing
  // whether coAlpha is in FSourceOptions may fail in some rare cases.
  // Thus we will check Source samples per pixel and skip anything above 1.
  if (FSourceSPP > 1) and not (coSeparatePlanes in FSourceOptions) then
  begin
    AlphaSkip := FSourceSPP - 1;
    BitIncrement := FSourceSPP * FSourceBPS; // Bits and alpha value (and possibly extra samples)
  end
  else begin
    AlphaSkip := 0;
    BitIncrement := FSourceBPS; // Bits only
  end;

  case FSourceBPS of
    8:
      case FTargetBPS of
        8: // 888 to 888
          begin
            Source8 := Source[0];
            Target8 := Target;

            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target8^ := Source8^;
                Inc(Source8, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target8);
            end;
          end;
        16: // 888 to 161616
          begin
            Source8 := Source[0];
            Target16 := Target;
            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target16^ := MulDiv16(Source8^, 65535, 255);
                Inc(Source8, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target16);
            end;
          end;
      end;
    16:
      case FTargetBPS of
        8: // 161616 to 888
          begin
            Source16 := Source[0];
            Target8 := Target;
            if coNeedByteSwap in FSourceOptions then
              Convert16 := ComponentSwapScaleConvert
            else
              Convert16 := ComponentScaleConvert16To8;
            // Half Float sample data format needs different conversion.
            if FSourceDataFormat = sdfFloat then
              Convert16 := ComponentScaleConvertFloat16To8;

            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                Target8^ := Convert16(Source16^);
                Inc(Source16, 1 + AlphaSkip);
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target8);
            end;
          end;
        16: // 161616 to 161616
          begin
            Source16 := Source[0];
            Target16 := Target;

            if coNeedByteSwap in FSourceOptions then
            begin
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Target16^ := Swap(Source16^);
                  Inc(Source16, 1 + AlphaSkip);
                end;
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
                Inc(Target16);
              end;
            end
            else
            begin
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Target16^ := Source16^;
                  Inc(Source16, 1 + AlphaSkip);
                end;
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
                Inc(Target16);
              end;
            end;
          end;
      end;

    5..7, 9..15:
      // Conversion of uncommon bit formats to 8 bits only for now
      case FTargetBPS of
        8: // Conversion supported for 5..7 bits and 9..15 bits
          begin
            Source16 := Source[0];
            Target8 := Target;
            case FSourceBPS of
               6: ConvertAny := ComponentScaleConvert6To8;
              10: ConvertAny := ComponentScaleConvert10To8;
              12: ConvertAny := ComponentScaleConvert12To8;
              14: ConvertAny := ComponentScaleConvert14To8;
            else
              ConvertAny := ComponentScaleConvertUncommonTo8;
            end;

            BitOffset := 0;
            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                Bits := GetBitsMSB(BitOffset, FSourceBPS,PByte(Source16));
                Target8^ := ConvertAny(Bits,FSourceBPS);
                // Update the bit and byte pointers
                Inc(BitOffset, BitIncrement);
                Inc( PByte(Source16), BitOffset div 8 );
                BitOffset := BitOffset mod 8;
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
              Inc(Target8);
            end;
          end;
      else
      end;
    17..32: // Support for high bits per samples (mainly 24/32 bits)
      case FTargetBPS of
        8:  // Default 8 bits per sample target
          begin
            Source32 := Source[0];
            Target8 := Target;
            GetBits32 := GetBitsMSB;
            case FSourceBPS of
               32:
                 if FSourceDataFormat = sdfFloat then begin
                   Convert32 := ComponentScaleConvertFloat32To8;
                   GetBits32 := GetBitsSingle;
                 end
                 else begin
                   Convert32 := ComponentScaleConvert32To8;
                 end;
               24:
                 if FSourceDataFormat = sdfFloat then begin
                   Convert32 := ComponentScaleConvertFloat24To8;
                   GetBits32 := GetBitsSingle;
                 end
                 else begin
                   Convert32 := ComponentScaleConvert17_24To8;
                 end;
               17..23: Convert32 := ComponentScaleConvert17_24To8;
            else // Use else for 25..31 or else compiler will complain about uninitialized
              {25..31:} Convert32 := ComponentScaleConvert25_31To8;
            end;

            BitOffset := 0;
            while Count > 0 do
            begin
              // For now always assuming that bits are in big endian MSB first order!!! (TIF)
              Bits := GetBits32(BitOffset, FSourceBPS, PByte(Source32));
              Target8^ := Convert32(Bits, FSourceBPS);
              // Update the bit and byte pointers
              Inc(BitOffset, BitIncrement);
              Inc( PByte(Source32), BitOffset div 8 );
              BitOffset := BitOffset mod 8;
              Dec(Count);
              Inc(Target8);
            end;
          end;
      else
        // Other targets unsupported for now
      end;
    33..64: // Support for high bits per samples (mainly 64 bits)
      case FTargetBPS of
        8:  // Default 8 bits per sample target
          begin
            Source64 := Source[0];
            Target8 := Target;
            if FSourceBPS = 64 then begin
              if FSourceDataFormat = sdfFloat then begin
                Convert64 := ComponentScaleConvertFloat64To8;
                GetBits64 := GetBitsDouble; // Apparently not BigEndian
              end
              else begin
                Convert64 := ComponentScaleConvert64To8;
                GetBits64 := GetBitsMSB64; // Always BigEndian (TIFF) is currently assumed
              end;
            end
            else begin
              Convert64 := ComponentScaleConvert33_63To8;
              GetBits64 := GetBitsMSB64; // Always BigEndian (TIFF) is currently assumed
            end;

            BitOffset := 0;
            while Count > 0 do
            begin
              Bits64 := GetBits64(BitOffset, FSourceBPS, PByte(Source64));
              Target8^ := Convert64(Bits64, FSourceBPS);
              // Update the bit and byte pointers
              Inc(BitOffset, BitIncrement);
              Inc( PByte(Source64), BitOffset div 8 );
              BitOffset := BitOffset mod 8;
              Dec(Count);
              Inc(Target8);
            end;
          end;
      else
        // Other targets unsupported for now
      end;
    3: // Conversion of uncommon 3 bits to 4 bits only for now
      case FTargetBPS of
        4: // Convert to 4 bits
          begin
            Source8 := Source[0];
            Target8 := Target;
            BitOffset := 0;
            Bits2 := 0; // Stop Delphi from complaining about Bits2 not being initialized
            FirstNibble := True;
            while Count > 0 do
            begin
              if Boolean(Mask and BitRun) then
              begin
                // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                // Since we are converting to 4 bits we need 2 values to fill a byte!
                Bits := GetBitsMSB(BitOffset, FSourceBPS,Source8);
                if FirstNibble then begin // First 4 bits
                  Bits2 := ComponentScaleConvertTo4(Bits,FSourceBPS) shl 4;
                end
                else begin
                  // Second 4 bits, make them into a byte together with the first 4 bits
                  Target8^ := Bits2 or ComponentScaleConvertTo4(Bits,FSourceBPS);
                  Inc(Target8);  // Increment target buffer position
                end;
                FirstNibble := not FirstNibble;
                // Update the bit and byte pointers
                Inc(BitOffset, BitIncrement);
                if BitOffset >= 8 then begin
                  Inc(Source8);
                  BitOffset := BitOffset mod 8;
                end;
              end;
              asm ROR BYTE PTR [BitRun], 1 end;
              Dec(Count);
            end;
            // We need to check if the last nibble got written
            if not FirstNibble then begin
              // No second nibble at end of line: write only first nibble
              Target8^ := Bits2;
            end;
          end;
      else
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexed8(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats.
// This routine takes care about sample scaling and interlacing.
// Note: 16 bit indexed mode is a bit different (words instead bytes and byte swap) and handled separately.
// Note: 8 bit indexed with alpha is currently handled by RowConvertGray.

var
  SourceRun,
  TargetRun: PByte;
  Value,
  BitRun,
  TargetMask,
  TargetShift,
  MaxOutSample,
  TargetBPS: Byte;  // Local copy to ease assembler access
  Done: Cardinal;
  MaxInSample,      // Supporting up to and including 15 bits per sample for source input
  BitOffset: Word;  // Current start bit in source

begin
  SourceRun := Source[0];
  TargetRun := Target;

  if (FSourceBPS = FTargetBPS) and (Mask = $FF) then
    Move(SourceRun^, TargetRun^, (Count * FSourceBPS + 7) div 8)
  else
  begin
    BitRun := $80;
    // Make a copy of FTargetBPS from private variable to local variable
    // to ease access during assembler parts in the code
    TargetBPS := FTargetBPS;
    MaxInSample := (1 shl FSourceBPS) - 1;
    TargetMask := (1 shl (8 - TargetBPS)) - 1;
    MaxOutSample := (1 shl TargetBPS) - 1;
    TargetShift := 8 - TargetBPS;
    Done := 0;
    BitOffset := 0;
    while Done < Count do
    begin
      if Boolean(Mask and BitRun) then
      begin
        // TODO: Make using MSB big endian bits or not an option.
        // For now we always assume that bits are in big endian MSB first order! (TIF)
        // Reason: currently the only image format that we use and that supports
        // indexed source bits per sample other than 1, 4, 8
        Value := GetBitsMSB(BitOffset, FSourceBPS, SourceRun);
        Inc(BitOffset,FSourceBPS);
        if BitOffset >= 16 then begin
          BitOffset := BitOffset mod 8;
          Inc(SourceRun,2);
        end
        else if BitOffset >= 8 then begin
          BitOffset := BitOffset mod 8;
          Inc(SourceRun);
        end;
        Value := MulDiv16(Value, MaxOutSample, MaxInSample);
        TargetRun^ := (TargetRun^ and TargetMask) or (Value shl TargetShift);
      end;

      asm
        ROR BYTE PTR [BitRun], 1      // adjust test bit mask
        MOV CL, [TargetBPS]
        ROR BYTE PTR [TargetMask], CL // roll target mask with target bit count
      end;
      if TargetShift = 0 then
        TargetShift := 8 - TargetBPS
      else
        Dec(TargetShift, TargetBPS);
      Inc(Done);
      // Advance target pointer every (8 div target bit count)
      if (Done mod (8 div TargetBPS)) = 0 then
        Inc(TargetRun);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedBoth16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per sample values involved
// (special version for source and target resolution being both 16 BPS).

var
  TargetRun,
  SourceRun: PWord;
  BitRun: Byte;

begin
  SourceRun := Source[0];
  TargetRun := Target;
  BitRun := $80;

  if coNeedByteSwap in FSourceOptions then
  begin
    while Count > 0 do
    begin
      if Boolean(Mask and BitRun) then
      begin
        TargetRun^ := Swap(SourceRun^);
        Inc(SourceRun);
      end;
      asm ROR BYTE PTR [BitRun], 1 end;
      Dec(Count);
      Inc(TargetRun);
    end;
  end
  else
  begin
    if Mask = $FF then
      Move(SourceRun^, TargetRun^, 2 * Count)
    else
      while Count > 0 do
      begin
        if Boolean(Mask and BitRun) then
        begin
          TargetRun^ := SourceRun^;
          Inc(SourceRun);
        end;
        asm ROR BYTE PTR [BitRun], 1 end;
        Dec(Count);
        Inc(TargetRun);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedSource16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per source sample values involved.

var
  TargetRun8: PByte;
  SourceRun16: PWord;
  Value,
  BitRun,
  TargetMask,
  TargetShift,
  MaxOutSample,
  TargetBPS: Byte;    // Local copies to ease assembler access

begin
  SourceRun16 := Source[0];
  TargetRun8 := Target;
  BitRun := $80;
  // Make a copy of these both values from private variables to local variables
  // to ease access during assembler parts in the code
  TargetBPS := FTargetBPS;
  TargetMask := (1 shl (8 - TargetBPS)) - 1;
  MaxOutSample := (1 shl TargetBPS) - 1;
  TargetShift := 8 - TargetBPS;
  while Count > 0 do
  begin
    if Boolean(Mask and BitRun) then
    begin
      if coNeedByteSwap in FSourceOptions then
        Value := MulDiv16(Swap(SourceRun16^), MaxOutSample, 65535)
      else
        Value := MulDiv16(SourceRun16^, MaxOutSample, 65535);
      TargetRun8^ := (TargetRun8^ and TargetMask) or (Value shl TargetShift);
      Inc(SourceRun16);
    end;

    asm
      ROR BYTE PTR [BitRun], 1      // Adjust test bit mask
      MOV CL, [TargetBPS]
      ROR BYTE PTR [TargetMask], CL // Roll target mask with target bit count
    end;
    if TargetShift = 0 then
      TargetShift := 8 - TargetBPS
    else
      Dec(TargetShift, TargetBPS);
    Dec(Count);
    // Advance target pointer every (8 div target bit count)
    if (Count mod (8 div TargetBPS)) = 0 then
      Inc(TargetRun8);
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertIndexedTarget16(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// This is the core conversion routine for indexed pixel formats with 16 bits per target sample values involved.

var
  SourceRun8: PByte;
  TargetRun16: PWord;
  Value: Word;
  BitRun,
  SourceMask,
  SourceShift,
  MaxInSample,
  SourceBPS: Byte;

begin
  SourceRun8 := Source[0];
  TargetRun16 := Target;
  BitRun := $80;
  SourceBPS := FSourceBPS;
  SourceMask := Byte(not ((1 shl (8 - SourceBPS)) - 1));
  MaxInSample := (1 shl SourceBPS) - 1;
  SourceShift := 8;
  while Count > 0 do
  begin
    if Boolean(Mask and BitRun) then
    begin
      // Adjust shift value by source bit depth
      Dec(SourceShift, SourceBPS);
      Value := (SourceRun8^ and SourceMask) shr SourceShift;
      Value := MulDiv16(Value, 65535, MaxInSample);
      if coNeedByteSwap in FSourceOptions then
        TargetRun16^ := Swap(Value)
      else
        TargetRun16^ := Value;
      if SourceShift = 0 then
      begin
        SourceShift := 8;
        Inc(SourceRun8);
      end;
      asm
        MOV CL, [SourceBPS]
        ROR BYTE PTR [SourceMask], CL // Roll source bit mask with source bit count
      end;
    end;

    asm
      ROR BYTE PTR [BitRun], 1      // Adjust test bit mask
    end;

    Dec(Count);
    // Advance target pointer every (8 div target bit count)
    Inc(TargetRun16);
  end;
end;

//------------------------------------------------------------------------------

// Convert Indexed to BGR.
// Source Palette data and format should have been set using SetSourcePalette.
// Index 0 in palette data should contain the red channel, 1 = green, 2 = blue.
// Palette channel data as expected from TIF is always 16 bits.
// pfPlane8Triple: PSD indexed
// Mask is currently ignored here.
procedure TColorManager.RowConvertIndexed2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  PalIndex8: Byte;
  PalIndex,
  AlphaVal16: Cardinal;
  SourceRun8: PByte;
  SourceRun16,
  SourceRun16A: PWord;
  SourceRun8A: PByte;
  TargetRun8: PBGRA;
  BitOffset: Cardinal;
  AddAlpha,
  CopyAlpha: Boolean;
  SourceIncrement,
  TargetIncrement: Integer;
begin
  if Length(Source) = 0 then begin
    ShowError(gesSourcePaletteUndefined);
    Exit;
  end;
  // Mainly support for 8 and 16 bits per sample
  if not (FSourceBPS in [1..16]) then
    Exit;

  AddAlpha := False;
  // Check how we need to handle alpha
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := 2; // 1 indexed + 1 alpha
    if coAlpha in FTargetOptions then begin
      TargetIncrement := SizeOf(TBGRA);
      CopyAlpha := True;
    end
    else begin
      TargetIncrement := SizeOf(TBGR);
      CopyAlpha := False;
    end;
  end
  else
  begin
    SourceIncrement := 1; // 1 byte grayscale
    if coAlpha in FTargetOptions then begin
      TargetIncrement := SizeOf(TBGRA);
      AddAlpha := True;
    end
    else
      TargetIncrement := SizeOf(TBGR);
    CopyAlpha := False;
  end;
  // In planar mode source increment is always 1
  if coSeparatePlanes in FSourceOptions then
    SourceIncrement := 1;
  // Convert to number of bytes (Don't worry about FSourceBPS being a multiple of 8:
  // SourceIncrement will not be used in those cases!)
  SourceIncrement := FSourceBPS div 8 * SourceIncrement;

  case FSourcePaletteFormat of
    pfPlane16Triple,
    pfPlane16Quad:
      if ((FSourcePaletteFormat = pfPlane16Triple) and (Length(FSourcePaletteData) <> 3)) or
         ((FSourcePaletteFormat = pfPlane16Quad) and (Length(FSourcePaletteData) <> 4)) then begin
        ShowError(gesIncorrectPaletteDataCount);
      end
      else begin
        case FSourceBPS of
          8:
            begin
              SourceRun8 := Source[0]; // Palette indexes
              if Length(Source) = 1 then begin
                SourceRun8A := SourceRun8; Inc(SourceRun8A);
              end
              else begin
                SourceRun8A := Source[1];
              end;
              TargetRun8 := Target;

              while Count > 0 do
              begin
                // Get palette index
                PalIndex8 := SourceRun8^;

                // Store color info from palette index in Target
                TargetRun8^.B := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[2])^[PalIndex8]);
                TargetRun8^.G := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[1])^[PalIndex8]);
                TargetRun8^.R := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[0])^[PalIndex8]);

                // Handle alpha channel
                if CopyAlpha then begin
                  TargetRun8^.A := SourceRun8A^;
                end
                else if AddAlpha then begin
                  if FSourcePaletteFormat = pfPlane16Quad then begin
                    // Palette also holds alpha, use that for adding alpha.
                    // NB! Untested since I don't have any samples using this!
                    TargetRun8^.A := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[3])^[SourceRun8A^]);
                  end
                  else
                    TargetRun8^.A := $ff;
                end;

                Inc(SourceRun8, SourceIncrement);
                Inc(SourceRun8A, SourceIncrement);
                Inc(PByte(TargetRun8), TargetIncrement);

                Dec(Count);
              end;
            end;
          16:
            begin
              SourceRun16 := Source[0]; // Palette indexes
              if Length(Source) = 1 then begin
                SourceRun16A := SourceRun16; Inc(SourceRun16A);
              end
              else begin
                SourceRun16A := Source[1];
              end;
              TargetRun8 := Target;

              while Count > 0 do
              begin
                // Get palette index
                if coNeedByteSwap in FSourceOptions then
                  PalIndex := Swap(SourceRun16^)
                else
                  PalIndex := SourceRun16^;

                // Store color info from palette index in Target
                TargetRun8^.B := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[2])^[PalIndex]);
                TargetRun8^.G := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[1])^[PalIndex]);
                TargetRun8^.R := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[0])^[PalIndex]);

                // Handle alpha channel
                if CopyAlpha then begin
                  if coNeedByteSwap in FSourceOptions then
                    AlphaVal16 := Swap(SourceRun16A^)
                  else
                    AlphaVal16 := SourceRun16A^;
                  TargetRun8^.A := ComponentScaleConvert16To8(AlphaVal16);
                end
                else if AddAlpha then begin
                  if FSourcePaletteFormat = pfPlane16Quad then begin
                    // Palette also holds alpha, use that for adding alpha.
                    // NB! Untested since I don't have any samples using this!
                    TargetRun8^.A := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[3])^[PalIndex]);
                  end
                  else
                    TargetRun8^.A := $ff;
                end;

                Inc(PByte(SourceRun16), SourceIncrement);
                Inc(PByte(SourceRun16A), SourceIncrement);
                Inc(PByte(TargetRun8), TargetIncrement);

                Dec(Count);
              end;
            end;
          1..7,
          9..15: // See GraphicsMagic test images.
            begin
              SourceRun16 := Source[0]; // Palette indexes
              TargetRun8 := Target;

              BitOffset := 0;
              while Count > 0 do
              begin
                // Get palette index
                PalIndex := GetBitsMSB(BitOffset, FSourceBPS, PByte(SourceRun16));

                // Update the bit and byte pointers
                Inc(BitOffset, FSourceBPS);
                Inc( PByte(SourceRun16), BitOffset div 8 );
                BitOffset := BitOffset mod 8;

                // Store color info from palette index in Target
                TargetRun8^.B := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[2])^[PalIndex]);
                TargetRun8^.G := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[1])^[PalIndex]);
                TargetRun8^.R := ComponentScaleConvert16To8(PWordArray(FSourcePaletteData[0])^[PalIndex]);
                // We are ignoring alpha channels here.
                Inc(PByte(TargetRun8), SizeOf(TBGR));

                Dec(Count);
              end;
            end;
        end;
      end;
    pfPlane8Triple:
      begin
        if Length(FSourcePaletteData) <> 3 then begin
          ShowError(gesIncorrectPaletteDataCount);
          Exit;
        end;
        case FSourceBPS of
          8:
            begin
              SourceRun8 := Source[0]; // Palette indexes
              if Length(Source) = 1 then begin
                SourceRun8A := SourceRun8; Inc(SourceRun8A);
              end
              else begin
                SourceRun8A := Source[1];
              end;
              TargetRun8 := Target;

              while Count > 0 do
              begin
                // Get palette index
                PalIndex8 := SourceRun8^;

                // Store color info from palette index in Target
                TargetRun8^.B := PByteArray(FSourcePaletteData[2])^[PalIndex8];
                TargetRun8^.G := PByteArray(FSourcePaletteData[1])^[PalIndex8];
                TargetRun8^.R := PByteArray(FSourcePaletteData[0])^[PalIndex8];
                // Handle alpha
                if CopyAlpha then
                  TargetRun8^.A := SourceRun8A^
                else if AddAlpha then
                  // Source has no alpha but target needs alpha, set it to $ff
                  TargetRun8^.A := $ff;

                Inc(SourceRun8, SourceIncrement);
                Inc(SourceRun8A, SourceIncrement);
                Inc(PByte(TargetRun8), TargetIncrement);

                Dec(Count);
              end;
            end;
        end;
      end;
  else
    ShowError(gesPaletteFormatConversionUnsupported);
  end;
end;

//------------------------------------------------------------------------------

// Convert Grayscale(A) to BGR(A).
// Source: Currently only 8 bits grayscale with optional 8 bits alpha
// Mask is currently ignored.
// Target BGR(A) = 24/32 bits only at the moment.
procedure TColorManager.RowConvertGray2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceRun8: PByte;
  SourceAlphaRun8: PByte;
  TargetRun8: PBGRA;
  GrayValue8: Byte;
  CopyAlpha,
  AddAlpha: Boolean;
  SourceIncrement,
  TargetIncrement: Integer;
begin
  if Length(Source) = 0 then begin
    ShowError(gesSourcePaletteUndefined);
    Exit;
  end;

  if not (coSeparatePlanes in FSourceOptions) then
    // Since Tiff can have undetermined extra samples we need to use the correct SourceIncrement
    SourceIncrement := FSourceSPP
  else
    // In planar mode source increment is always 1
    SourceIncrement := 1;

  AddAlpha := False;
  // Check how we need to handle alpha
  if coAlpha in FSourceOptions then
  begin
    if coAlpha in FTargetOptions then begin
      TargetIncrement := SizeOf(TBGRA);
      CopyAlpha := True;
    end
    else begin
      TargetIncrement := SizeOf(TBGR);
      CopyAlpha := False;
    end;
  end
  else
  begin
    if coAlpha in FTargetOptions then begin
      TargetIncrement := SizeOf(TBGRA);
      AddAlpha := True;
    end
    else
      TargetIncrement := SizeOf(TBGR);
    CopyAlpha := False;
  end;

  case FSourceBPS of
    8:
      begin
        // Currently only 8BPS target supported.
        if FTargetBPS <> 8 then
          Exit;
        // Set up source and target pointers
        SourceRun8 := Source[0]; // Grayscale source
        if Length(Source) = 1 then begin
          SourceAlphaRun8 := SourceRun8; Inc(SourceAlphaRun8);
        end
        else begin
          // Length of source assumed to be 2. No checking necessary: in case
          // there are more values those pointers will just be ignored.
          SourceAlphaRun8 := Source[1];
        end;
        TargetRun8 := Target;

        while Count > 0 do
        begin
          // Get grayscale index
          GrayValue8 := SourceRun8^;

          // Store grayscale info from palette index in Target
          TargetRun8^.B := GrayValue8;
          TargetRun8^.G := GrayValue8;
          TargetRun8^.R := GrayValue8;
          if CopyAlpha then
            TargetRun8^.A := SourceAlphaRun8^
          else if AddAlpha then
            // Source has no alpha but target needs alpha, set it to $ff
            TargetRun8^.A := $ff;

          Inc(SourceRun8, SourceIncrement);
          Inc(SourceAlphaRun8, SourceIncrement);
          Inc(PByte(TargetRun8), TargetIncrement);

          Dec(Count);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertRGB2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts RGB source schemes to BGR target schemes and takes care for byte swapping, alpha copy/skip and
// gamma correction.

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  Source8,
  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  SourceR32,
  SourceG32,
  SourceB32,
  SourceA32: PLongWord;

  SourceR64,
  SourceG64,
  SourceB64,
  SourceA64: PUint64;

  Target8: PByte;
  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;
  Convert32_8: function(Value: LongWord): Byte of object;
  Convert32_8Alpha: function(Value: LongWord): Byte of object;
  Convert64_8: function(Value: UInt64): Byte of object;
  Convert64_8Alpha: function(Value: UInt64): Byte of object;
  // Convert up to 16 bits to 8 bits
  ConvertAny16To8: function(Value: Word; BitsPerSampe: Byte): Byte of object;
  ConvertAny32To8: function(Value: LongWord; BitsPerSampe: Byte): Byte of object;
  ConvertAny64To8: function(Value: UInt64; BitsPerSampe: Byte): Byte of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

  Bits,
  BitOffset,
  BitIncrement: Cardinal;
  BitOffsetR,
  BitOffsetG,
  BitOffsetB,
  BitOffsetA: Cardinal;
  AlphaSkip: Cardinal;

begin
  BitRun := $80;

  if not (coSeparatePlanes in FSourceOptions) then
    // Since Tiff can have undetermined extra samples we need to use the correct SourceIncrement
    SourceIncrement := FSourceSPP
  else
    // In planar mode source increment is always 1
    SourceIncrement := 1;

  // Determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    // Byte size of components doesn't matter as the increments are applied to
    // pointers whose data types determine the final increment
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
    AlphaSkip := 1;
  end
  else
  begin
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
    AlphaSkip := 0;
  end;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // Interleaved mode
          SourceR8 := Source[0];
          SourceG8 := SourceR8; Inc(SourceG8);
          SourceB8 := SourceG8; Inc(SourceB8);
          SourceA8 := SourceB8; Inc(SourceA8);
        end
        else
        begin
          SourceR8 := Source[0];
          SourceG8 := Source[1];
          SourceB8 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA8 := Source[3]
          else
            SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // Alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceR16 := Source[0];
          SourceG16 := SourceR16; Inc(SourceG16);
          SourceB16 := SourceG16; Inc(SourceB16);
          SourceA16 := SourceB16; Inc(SourceA16);
        end
        else
        begin
          SourceR16 := Source[0];
          SourceG16 := Source[1];
          SourceB16 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA16 := Source[3]
          else
            SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              // Half Float sample data format needs separate conversion.
              if FSourceDataFormat = sdfFloat then
                Convert16_8 := ComponentScaleConvertFloat16To8
              else if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleGammaConvert
                else
                  Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleConvert
                else
                  Convert16_8 := ComponentScaleConvert16To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert16_8Alpha := ComponentScaleConvertFloat16To8
              else if coNeedByteSwap in FSourceOptions then
                Convert16_8Alpha := ComponentSwapScaleConvert
              else
                Convert16_8Alpha := ComponentScaleConvert16To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else if coAlpha in FTargetOptions then
              begin // Need to add Alpha (default = opaque = 255)
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := 255;

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // No gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA16 := Source[3]
                else
                  SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    32: // 32-bits float or 32 bits (un)signed integer.
        // Currently only conversion to 8 bits supported.
      begin
        if Length(Source) = 1 then
        begin
          SourceR32 := Source[0];
          SourceG32 := SourceR32; Inc(SourceG32);
          SourceB32 := SourceG32; Inc(SourceB32);
          SourceA32 := SourceB32; Inc(SourceA32);
        end
        else
        begin
          SourceR32 := Source[0];
          SourceG32 := Source[1];
          SourceB32 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA32 := Source[3]
          else
            SourceA32 := nil;
        end;

        case FTargetBPS of
          8: // 323232 to 888
            begin
              // Float sample data format needs separate conversion.
              if FSourceDataFormat = sdfFloat then
                Convert32_8 := ComponentScaleConvertFloat32To8
              else
              { Gamma conversion not supported here for now.
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert32_8 := ComponentSwapScaleGammaConvert32To8
                else
                  Convert32_8 := ComponentScaleGammaConvert32To8;
              end
              else}
              begin
                { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                  Convert32_8 := ComponentSwapScaleConvert32To8
                else}
                  Convert32_8 := ComponentScaleConvert32To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert32_8Alpha := ComponentScaleConvertFloat32To8
              else { Byte swapping not supported here for now.
              if coNeedByteSwap in FSourceOptions then
                Convert32_8Alpha := ComponentSwapScaleConvert
              else}
                Convert32_8Alpha := ComponentScaleConvert32To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert32_8(SourceR32^);
                    TargetRunA8.G := Convert32_8(SourceG32^);
                    TargetRunA8.B := Convert32_8(SourceB32^);
                    TargetRunA8.A := Convert32_8Alpha(SourceA32^);

                    Inc(SourceB32, SourceIncrement);
                    Inc(SourceG32, SourceIncrement);
                    Inc(SourceR32, SourceIncrement);
                    Inc(SourceA32, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else if coAlpha in FTargetOptions then
              begin // Need to add Alpha (default = opaque = 255)
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert32_8(SourceR32^);
                    TargetRunA8.G := Convert32_8(SourceG32^);
                    TargetRunA8.B := Convert32_8(SourceB32^);
                    TargetRunA8.A := 255;

                    Inc(SourceB32, SourceIncrement);
                    Inc(SourceG32, SourceIncrement);
                    Inc(SourceR32, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert32_8(SourceR32^);
                    TargetRun8.G := Convert32_8(SourceG32^);
                    TargetRun8.B := Convert32_8(SourceB32^);

                    Inc(SourceB32, SourceIncrement);
                    Inc(SourceG32, SourceIncrement);
                    Inc(SourceR32, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    64: // 64-bits float or 64 bits (un)signed integer.
        // Currently only conversion to 8 bits supported.
      begin
        if Length(Source) = 1 then
        begin
          SourceR64 := Source[0];
          SourceG64 := SourceR64; Inc(SourceG64);
          SourceB64 := SourceG64; Inc(SourceB64);
          SourceA64 := SourceB64; Inc(SourceA64);
        end
        else
        begin
          SourceR64 := Source[0];
          SourceG64 := Source[1];
          SourceB64 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA64 := Source[3]
          else
            SourceA64 := nil;
        end;

        case FTargetBPS of
          8: // 646464 to 888
            begin
              // Float sample data format needs separate conversion.
              if FSourceDataFormat = sdfFloat then
                Convert64_8 := ComponentScaleConvertFloat64To8
              else
              { Gamma conversion not supported here for now.
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert64_8 := ComponentSwapScaleGammaConvert64To8
                else
                  Convert64_8 := ComponentScaleGammaConvert64To8;
              end
              else}
              begin
                { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                  Convert64_8 := ComponentSwapScaleConvert64To8
                else}
                  Convert64_8 := ComponentScaleConvert64To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert64_8Alpha := ComponentScaleConvertFloat64To8
              else { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                Convert64_8Alpha := ComponentSwapScaleConvert64To8
              else}
                Convert64_8Alpha := ComponentScaleConvert64To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert64_8(SourceR64^);
                    TargetRunA8.G := Convert64_8(SourceG64^);
                    TargetRunA8.B := Convert64_8(SourceB64^);
                    TargetRunA8.A := Convert64_8Alpha(SourceA64^);

                    Inc(SourceB64, SourceIncrement);
                    Inc(SourceG64, SourceIncrement);
                    Inc(SourceR64, SourceIncrement);
                    Inc(SourceA64, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert64_8(SourceR64^);
                    TargetRun8.G := Convert64_8(SourceG64^);
                    TargetRun8.B := Convert64_8(SourceB64^);

                    Inc(SourceB64, SourceIncrement);
                    Inc(SourceG64, SourceIncrement);
                    Inc(SourceR64, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    1..7,
    9..15:
      begin
        // Currently only supporting conversion to 8 bpp target.
        // Target 1 and 4 bits would need palette handling.
        // Mask parameter not supported here since I'm not sure how to correctly
        // implement it here and no material to test it on.

        BitIncrement := FSourceBPS;
        case FTargetBPS of
          8: // ... to 888
            begin
              Target8 := Target;
              case FSourceBPS of
                 6: ConvertAny16To8 := ComponentScaleConvert6To8;
                10: ConvertAny16To8 := ComponentScaleConvert10To8;
                12: ConvertAny16To8 := ComponentScaleConvert12To8;
                14: ConvertAny16To8 := ComponentScaleConvert14To8;
              else
                ConvertAny16To8 := ComponentScaleConvertUncommonTo8;
              end;

              if Length(Source) = 1 then begin
                 // Interleaved
                Source8 := Source[0];
                BitOffset := 0;
                while Count > 0 do
                begin
                  // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                  // Red
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.R := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  // Green
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.G := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  // Blue
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.B := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  if coAlpha in FSourceOptions then begin
                    Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                    // Update the bit and byte pointers
                    Inc(BitOffset, BitIncrement);
                    Inc( Source8, BitOffset div 8 );
                    BitOffset := BitOffset mod 8;
                    if coAlpha in FTargetOptions then
                      // Only need to use alpha if Target has alpha
                      PBGRA(Target8)^.A := ConvertAny16To8(Bits, FSourceBPS);
                    end
                  else if coAlpha in FTargetOptions then begin
                    // Source no Alpha, Target: add alpha
                    PBGRA(Target8)^.A := $FF; // opaque
                  end;

                  Dec(Count);
                  Inc(Target8, 3 + AlphaSkip );
                end;
              end
              else begin
                // Planar mode
                SourceR8 := Source[0];
                SourceG8 := Source[1];
                SourceB8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
                BitOffsetR := 0;
                BitOffsetG := 0;
                BitOffsetB := 0;
                BitOffsetA := 0;
                while Count > 0 do
                begin
                  // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                  // Red
                  Bits := GetBitsMSB(BitOffsetR, FSourceBPS, SourceR8);
                  PBGR(Target8)^.R := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetR, BitIncrement);
                  Inc( SourceR8, BitOffsetR div 8 );
                  BitOffsetR := BitOffsetR mod 8;

                  // Green
                  Bits := GetBitsMSB(BitOffsetG, FSourceBPS, SourceG8);
                  PBGR(Target8)^.G := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetG, BitIncrement);
                  Inc( SourceG8, BitOffsetG div 8 );
                  BitOffsetG := BitOffsetG mod 8;

                  // Blue
                  Bits := GetBitsMSB(BitOffsetB, FSourceBPS, SourceB8);
                  PBGR(Target8)^.B := ConvertAny16To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetB, BitIncrement);
                  Inc( SourceB8, BitOffsetB div 8 );
                  BitOffsetB := BitOffsetB mod 8;

                  if coAlpha in FSourceOptions then begin
                    Bits := GetBitsMSB(BitOffsetA, FSourceBPS, SourceA8);
                    // Update the bit and byte pointers
                    Inc(BitOffsetA, BitIncrement);
                    Inc( SourceA8, BitOffsetA div 8 );
                    BitOffsetA := BitOffsetA mod 8;
                    if coAlpha in FTargetOptions then
                      // Only need to use alpha if Target has alpha
                      PBGRA(Target8)^.A := ConvertAny16To8(Bits, FSourceBPS);
                    end
                  else if coAlpha in FTargetOptions then begin
                    // Source no Alpha, Target: add alpha
                    PBGRA(Target8)^.A := $FF; // opaque
                  end;

                  Dec(Count);
                  Inc(Target8, 3 + AlphaSkip );
                end;
              end;
            end;
        end; // case
      end;
    17..31:
      begin
        // Currently only supporting conversion to 8 bpp target.
        // Mask parameter not supported here since I'm not sure how to correctly
        // implement it here and no material to test it on.

        BitIncrement := FSourceBPS;
        case FTargetBPS of
          8: // ... to 888
            begin
              Target8 := Target;
              // Float sample data format needs separate conversion.
              if (FSourceDataFormat = sdfFloat) and (FSourceBPS = 24) then
                ConvertAny32To8 := ComponentScaleConvertFloat24To8
              else
                ConvertAny32To8 := ComponentScaleConvert17_24To8;

              if Length(Source) = 1 then begin
                 // Interleaved
                Source8 := Source[0];
                BitOffset := 0;
                while Count > 0 do
                begin
                  // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                  // Red
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.R := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  // Green
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.G := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  // Blue
                  Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                  PBGR(Target8)^.B := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffset, BitIncrement);
                  Inc( Source8, BitOffset div 8 );
                  BitOffset := BitOffset mod 8;

                  if coAlpha in FSourceOptions then begin
                    Bits := GetBitsMSB(BitOffset, FSourceBPS, Source8);
                    // Update the bit and byte pointers
                    Inc(BitOffset, BitIncrement);
                    Inc( Source8, BitOffset div 8 );
                    BitOffset := BitOffset mod 8;
                    if coAlpha in FTargetOptions then
                      // Only need to use alpha if Target has alpha
                      PBGRA(Target8)^.A := ConvertAny32To8(Bits, FSourceBPS);
                    end
                  else if coAlpha in FTargetOptions then begin
                    // Source no Alpha, Target: add alpha
                    PBGRA(Target8)^.A := $FF; // opaque
                  end;

                  Dec(Count);
                  Inc(Target8, 3 + AlphaSkip );
                end;
              end
              else begin
                // Planar mode
                SourceR8 := Source[0];
                SourceG8 := Source[1];
                SourceB8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
                BitOffsetR := 0;
                BitOffsetG := 0;
                BitOffsetB := 0;
                BitOffsetA := 0;
                while Count > 0 do
                begin
                  // For now always assuming that bits are in big endian MSB first order!!! (TIF)
                  // Red
                  Bits := GetBitsMSB(BitOffsetR, FSourceBPS, SourceR8);
                  PBGR(Target8)^.R := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetR, BitIncrement);
                  Inc( SourceR8, BitOffsetR div 8 );
                  BitOffsetR := BitOffsetR mod 8;

                  // Green
                  Bits := GetBitsMSB(BitOffsetG, FSourceBPS, SourceG8);
                  PBGR(Target8)^.G := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetG, BitIncrement);
                  Inc( SourceG8, BitOffsetG div 8 );
                  BitOffsetG := BitOffsetG mod 8;

                  // Blue
                  Bits := GetBitsMSB(BitOffsetB, FSourceBPS, SourceB8);
                  PBGR(Target8)^.B := ConvertAny32To8(Bits, FSourceBPS);
                  // Update the bit and byte pointers
                  Inc(BitOffsetB, BitIncrement);
                  Inc( SourceB8, BitOffsetB div 8 );
                  BitOffsetB := BitOffsetB mod 8;

                  if coAlpha in FSourceOptions then begin
                    Bits := GetBitsMSB(BitOffsetA, FSourceBPS, SourceA8);
                    // Update the bit and byte pointers
                    Inc(BitOffsetA, BitIncrement);
                    Inc( SourceA8, BitOffsetA div 8 );
                    BitOffsetA := BitOffsetA mod 8;
                    if coAlpha in FTargetOptions then
                      // Only need to use alpha if Target has alpha
                      PBGRA(Target8)^.A := ConvertAny32To8(Bits, FSourceBPS);
                    end
                  else if coAlpha in FTargetOptions then begin
                    // Source no Alpha, Target: add alpha
                    PBGRA(Target8)^.A := $FF; // opaque
                  end;

                  Dec(Count);
                  Inc(Target8, 3 + AlphaSkip );
                end;
              end;
            end;
        end; // case
      end;
    33..63: ;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertRGB2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Same as ConvertRGB2BGR but for RGB target schemes

var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  SourceR32,
  SourceG32,
  SourceB32,
  SourceA32: PLongWord;

  SourceR64,
  SourceG64,
  SourceB64,
  SourceA64: PUint64;

  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;
  Convert32_8: function(Value: LongWord): Byte of object;
  Convert32_8Alpha: function(Value: LongWord): Byte of object;
  Convert64_8: function(Value: UInt64): Byte of object;
  Convert64_8Alpha: function(Value: UInt64): Byte of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  // Determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then
  begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else
  begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  // In planar mode source increment is always 1
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          // Interleaved mode
          SourceR8 := Source[0];
          SourceG8 := SourceR8; Inc(SourceG8);
          SourceB8 := SourceG8; Inc(SourceB8);
          SourceA8 := SourceB8; Inc(SourceA8);
        end
        else
        begin
          SourceR8 := Source[0];
          SourceG8 := Source[1];
          SourceB8 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA8 := Source[3]
          else
            SourceA8 := nil;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert8_8(SourceR8^);
                    TargetRunA8.G := Convert8_8(SourceG8^);
                    TargetRunA8.B := Convert8_8(SourceB8^);
                    // Alpha values are never gamma corrected
                    TargetRunA8.A := SourceA8^;
                  
                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert8_8(SourceR8^);
                    TargetRun8.G := Convert8_8(SourceG8^);
                    TargetRun8.B := Convert8_8(SourceB8^);

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 888 to 161616
            begin
              if coApplyGamma in FTargetOptions then
                Convert8_8 := ComponentGammaConvert
              else
                Convert8_8 := ComponentNoConvert8;
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;
              if Length(Source) = 1 then
              begin
                SourceB8 := Source[0];
                SourceG8 := SourceB8; Inc(SourceG8);
                SourceR8 := SourceG8; Inc(SourceR8);
                SourceA8 := SourceR8; Inc(SourceA8);
              end
              else
              begin
                SourceB8 := Source[0];
                SourceG8 := Source[1];
                SourceR8 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA8 := Source[3]
                else
                  SourceA8 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));
                    TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                    Inc(SourceA8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), 65535, 255));
                    TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), 65535, 255));
                    TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), 65535, 255));

                    Inc(SourceB8, SourceIncrement);
                    Inc(SourceG8, SourceIncrement);
                    Inc(SourceR8, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          SourceR16 := Source[0];
          SourceG16 := SourceR16; Inc(SourceG16);
          SourceB16 := SourceG16; Inc(SourceB16);
          SourceA16 := SourceB16; Inc(SourceA16);
        end
        else
        begin
          SourceR16 := Source[0];
          SourceG16 := Source[1];
          SourceB16 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA16 := Source[3]
          else
            SourceA16 := nil;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              if FSourceDataFormat = sdfFloat then
                // Half Float sample data format needs separate conversion.
                Convert16_8 := ComponentScaleConvertFloat16To8
              else if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleGammaConvert
                else
                  Convert16_8 := ComponentScaleGammaConvert;
              end
              else
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert16_8 := ComponentSwapScaleConvert
                else
                  Convert16_8 := ComponentScaleConvert16To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert16_8Alpha := ComponentScaleConvertFloat16To8
              else if coNeedByteSwap in FSourceOptions then
                Convert16_8Alpha := ComponentSwapScaleConvert
              else
                Convert16_8Alpha := ComponentScaleConvert16To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert16_8(SourceR16^);
                    TargetRunA8.G := Convert16_8(SourceG16^);
                    TargetRunA8.B := Convert16_8(SourceB16^);
                    TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert16_8(SourceR16^);
                    TargetRun8.G := Convert16_8(SourceG16^);
                    TargetRun8.B := Convert16_8(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
          16: // 161616 to 161616
            begin
              // No gamma correction for 16 bit samples yet
              if coNeedByteSwap in FSourceOptions then
                Convert16_16 := ComponentSwapConvert
              else
                Convert16_16 := ComponentNoConvert16;

              if Length(Source) = 1 then
              begin
                SourceB16 := Source[0];
                SourceG16 := SourceB16; Inc(SourceG16);
                SourceR16 := SourceG16; Inc(SourceR16);
                SourceA16 := SourceR16; Inc(SourceA16);
              end
              else
              begin
                SourceB16 := Source[0];
                SourceG16 := Source[1];
                SourceR16 := Source[2];
                if coAlpha in FSourceOptions then
                  SourceA16 := Source[3]
                else
                  SourceA16 := nil;
              end;

              if CopyAlpha then
              begin
                TargetRunA16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA16.R := Convert16_16(SourceR16^);
                    TargetRunA16.G := Convert16_16(SourceG16^);
                    TargetRunA16.B := Convert16_16(SourceB16^);
                    TargetRunA16.A := Convert16_16(SourceA16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                    Inc(SourceA16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA16);
                end;
              end
              else
              begin
                TargetRun16 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun16.R := Convert16_16(SourceR16^);
                    TargetRun16.G := Convert16_16(SourceG16^);
                    TargetRun16.B := Convert16_16(SourceB16^);

                    Inc(SourceB16, SourceIncrement);
                    Inc(SourceG16, SourceIncrement);
                    Inc(SourceR16, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PWord(TargetRun16), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    32: // 32-bits float or 32 bits (un)signed integer.
        // Currently only conversion to 8 bits supported.
      begin
        if Length(Source) = 1 then
        begin
          SourceR32 := Source[0];
          SourceG32 := SourceR32; Inc(SourceG32);
          SourceB32 := SourceG32; Inc(SourceB32);
          SourceA32 := SourceB32; Inc(SourceA32);
        end
        else
        begin
          SourceR32 := Source[0];
          SourceG32 := Source[1];
          SourceB32 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA32 := Source[3]
          else
            SourceA32 := nil;
        end;

        case FTargetBPS of
          8: // 323232 to 888
            begin
              // Float sample data format needs separate conversion.
              if FSourceDataFormat = sdfFloat then
                Convert32_8 := ComponentScaleConvertFloat32To8
              else
              { Gamma conversion not supported here for now.
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert32_8 := ComponentSwapScaleGammaConvert32To8
                else
                  Convert32_8 := ComponentScaleGammaConvert32To8;
              end
              else}
              begin
                { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                  Convert32_8 := ComponentSwapScaleConvert
                else}
                  Convert32_8 := ComponentScaleConvert32To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert32_8Alpha := ComponentScaleConvertFloat32To8
              else { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                Convert32_8Alpha := ComponentSwapScaleConvert
              else}
                Convert32_8Alpha := ComponentScaleConvert32To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert32_8(SourceR32^);
                    TargetRunA8.G := Convert32_8(SourceG32^);
                    TargetRunA8.B := Convert32_8(SourceB32^);
                    TargetRunA8.A := Convert32_8Alpha(SourceA32^);

                    Inc(SourceB32, SourceIncrement);
                    Inc(SourceG32, SourceIncrement);
                    Inc(SourceR32, SourceIncrement);
                    Inc(SourceA32, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert32_8(SourceR32^);
                    TargetRun8.G := Convert32_8(SourceG32^);
                    TargetRun8.B := Convert32_8(SourceB32^);

                    Inc(SourceB32, SourceIncrement);
                    Inc(SourceG32, SourceIncrement);
                    Inc(SourceR32, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
    64: // 64-bits float or 64 bits (un)signed integer.
        // Currently only conversion to 8 bits supported.
      begin
        if Length(Source) = 1 then
        begin
          SourceR64 := Source[0];
          SourceG64 := SourceR64; Inc(SourceG64);
          SourceB64 := SourceG64; Inc(SourceB64);
          SourceA64 := SourceB64; Inc(SourceA64);
        end
        else
        begin
          SourceR64 := Source[0];
          SourceG64 := Source[1];
          SourceB64 := Source[2];
          if coAlpha in FSourceOptions then
            SourceA64 := Source[3]
          else
            SourceA64 := nil;
        end;

        case FTargetBPS of
          8: // 646464 to 888
            begin
              // Float sample data format needs separate conversion.
              if FSourceDataFormat = sdfFloat then
                Convert64_8 := ComponentScaleConvertFloat64To8
              else
              { Gamma conversion not supported here for now.
              if coApplyGamma in FTargetOptions then
              begin
                if coNeedByteSwap in FSourceOptions then
                  Convert64_8 := ComponentSwapScaleGammaConvert64To8
                else
                  Convert64_8 := ComponentScaleGammaConvert64To8;
              end
              else}
              begin
                { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                  Convert64_8 := ComponentSwapScaleConvert64To8
                else}
                  Convert64_8 := ComponentScaleConvert64To8;
              end;
              // Since alpha channels are never gamma corrected we need a separate conversion routine
              if FSourceDataFormat = sdfFloat then
                Convert64_8Alpha := ComponentScaleConvertFloat64To8
              else { Byte swapping not supported here for now.
                if coNeedByteSwap in FSourceOptions then
                Convert64_8Alpha := ComponentSwapScaleConvert64To8
              else}
                Convert64_8Alpha := ComponentScaleConvert64To8;

              if CopyAlpha then
              begin
                TargetRunA8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRunA8.R := Convert64_8(SourceR64^);
                    TargetRunA8.G := Convert64_8(SourceG64^);
                    TargetRunA8.B := Convert64_8(SourceB64^);
                    TargetRunA8.A := Convert64_8Alpha(SourceA64^);

                    Inc(SourceB64, SourceIncrement);
                    Inc(SourceG64, SourceIncrement);
                    Inc(SourceR64, SourceIncrement);
                    Inc(SourceA64, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(TargetRunA8);
                end;
              end
              else
              begin
                TargetRun8 := Target;
                while Count > 0 do
                begin
                  if Boolean(Mask and BitRun) then
                  begin
                    TargetRun8.R := Convert64_8(SourceR64^);
                    TargetRun8.G := Convert64_8(SourceG64^);
                    TargetRun8.B := Convert64_8(SourceB64^);

                    Inc(SourceB64, SourceIncrement);
                    Inc(SourceG64, SourceIncrement);
                    Inc(SourceR64, SourceIncrement);
                  end;
                  asm ROR BYTE PTR [BitRun], 1 end;
                  Dec(Count);
                  Inc(PByte(TargetRun8), TargetIncrement);
                end;
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertPhotoYCC2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts from PhotoYCC format to BGR(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // Conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertPhotoYCC2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts from PhotoYCC format to RGB(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // Conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertYCbCr2BGR(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts from standard YCbCr to BGR(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // Conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.RowConvertYCbCr2RGB(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Converts from standard YCbCr to RGB(A)

var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;

begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1

  case FSourceBPS of
    8:
      begin
        if Length(Source) = 1 then
        begin
          Y8Run := Source[0];
          Cb8Run := Y8Run; Inc(Cb8Run);
          Cr8Run := Cb8Run; Inc(Cr8Run);
          Increment := 3;
        end
        else
        begin
          Y8Run := Source[0];
          Cb8Run := Source[1];
          Cr8Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 888 to 888
            begin
              Target8 := Target;
                                             
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);

                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 888 to 161616
            begin
              Target16 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := Y8Run^;
                  Inc(Y8Run, Increment);
                  Cb := Cb8Run^;
                  Inc(Cb8Run, Increment);
                  Cr := Cr8Run^;
                  Inc(Cr8Run, Increment);

                  // red
                  Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), 65535, 255);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), 65535, 255);
                  Inc(Target16);
                  // blue
                  Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), 65535, 255);
                  Inc(Target16);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
    16:
      begin
        if Length(Source) = 1 then
        begin
          Y16Run := Source[0];
          Cb16Run := Y16Run; Inc(Cb16Run);
          Cr16Run := Cb16Run; Inc(Cr16Run);
          Increment := 3;
        end
        else
        begin
          Y16Run := Source[0];
          Cb16Run := Source[1];
          Cr16Run := Source[2];
          Increment := 1;
        end;

        case FTargetBPS of
          8: // 161616 to 888
            begin
              Target8 := Target;

              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Y := MulDiv16(Y16Run^, 255, 65535);
                  Inc(Y16Run, Increment);
                  Cb := MulDiv16(Cb16Run^, 255, 65535);
                  Inc(Cb16Run, Increment);
                  Cr := MulDiv16(Cr16Run^, 255, 65535);
                  Inc(Cr16Run, Increment);

                  // red
                  Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
                  Inc(Target8, 1 + AlphaSkip);
                  // green
                  Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
                  Inc(Target8);
                  // blue
                  Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
                  Inc(Target8);
                end
                else
                  Inc(Target8, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
          16: // 161616 to 161616
            begin
              Target16 := Target;

              // Conversion from 16 to 16 is done with full precision, so there is no
              // loss of information, but the code is slower because the lookup tables
              // cannot be used
              while Count > 0 do
              begin
                if Boolean(Mask and BitRun) then
                begin
                  Yf := 1.3584 * Y16Run^;
                  Inc(Y16Run, Increment);
                  Cbf := Cb16Run^ - 40092; // (156 * 65535) div 255
                  Inc(Cb16Run, Increment);
                  Crf := Cr16Run^ - 35209; // (137 * 65535) div 255
                  Inc(Cr16Run, Increment);

                  // red
                  Target16^ := Round(Yf + 1.8215 * Crf);
                  Inc(Target16, 1 + AlphaSkip);
                  // green
                  Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
                  Inc(Target16);
                  // blue
                  Target16^ := Round(Yf + 2.2179 * Cbf);
                  Inc(Target16);
                end
                else
                  Inc(Target16, 3 + AlphaSkip);
                asm ROR BYTE PTR [BitRun], 1 end;
                Dec(Count);
              end;
            end;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.CreateYCbCrLookup;

// In order to speedup YCbCr conversion lookup tables are used, this methods creates them.
//    R := Y + Cr * (2 - 2 * LumaRed);
//    B := Y + Cb * (2 - 2 * LumaBlue);
//    G := Y - LumaBlue * Cb * (2 - 2 * LumaBlue) / LumaGreen
//           - LumaRed * Cr * (2 - 2 * LumaRed) / LumaGreen;
//
// To avoid floating point arithmetic the fractional constants that come out of the equations are represented
// as fixed point values in the range 0...2^16.  We also eliminate multiplications by // pre-calculating possible
// values indexed by Cb and Cr (this code assumes conversion is being done for 8-bit samples).
//
// Note: the color manager uses dynamic arrays so the memory used here is automatically freed.
//
// Needed settings:
// - YCbCr parameters must be set or default values are used

var
  F1, F2, F3, F4: Single;
  LumaRed,
  LumaGreen,
  LumaBlue: Single;
  I: Integer;
  Offset1, Offset2: Integer;

begin
  LumaRed := FYCbCrCoefficients[0];
  LumaGreen := FYCbCrCoefficients[1];
  LumaBlue := FYCbCrCoefficients[2];

  F1 := 2 - 2 * LumaRed;
  F2 := LumaRed * F1 / LumaGreen;
  F3 := 2 - 2 * LumaBlue;
  F4 := LumaBlue * F3 / LumaGreen;

  SetLength(FCrToRedTable, 256);
  SetLength(FCbToBlueTable, 256);
  SetLength(FCrToGreenTable, 256);
  SetLength(FCbToGreenTable, 256);

  if FSourceScheme = csYCbCr then              
  begin
    // I is the actual input pixel value in the range 0..255, Cb and Cr values are in the range -128..127.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -128;
    for I := 0 to 255 do
    begin
      FCrToRedTable[I] := Round(F1 * Offset1);
      FCbToBlueTable[I] := Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset1);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
    end;
  end
  else
  begin
    // PhotoYCC
    // I is the actual input pixel value in the range 0..255, Cb values are in the range -156..99,
    // Cr values are in the range -137..118.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -156;
    Offset2 := -137;
    for I := 0 to 255 do
    begin
      FCrToRedTable[I] := Round(F1 * Offset2);
      FCbToBlueTable[I] := Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset2);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
      Inc(Offset2);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TColorManager.GetPixelFormat(Index: Integer): TPixelFormat;

// Determines the pixel format from the current sample and pixel sizes
// Note: setting pfCustom as pixel format will raise an exception so check the result from this method first
//       before you actually assign it to a bitmap.
//
// Needed settings:
// - source samples per pixel and bits per sample for Index = 0
// - target samples per pixel and bits per sample for Index = 1

var
  SamplesPerPixel,
  BitsPerSample: Byte;

begin
  case Index of
    0:
      begin
        SamplesPerPixel := FSourceSPP;
        BitsPerSample := FSourceBPS;
      end;
  else
    SamplesPerPixel := FTargetSPP;
    BitsPerSample := FTargetBPS;
  end;

  case SamplesPerPixel of
    1, // One sample per pixel, this is usually a palette format
    2: // 2 samples for grayscale with alpha (extrasamples should be 1)
      case BitsPerSample of
        1:
          Result := pf1Bit;
        2..4: // Values < 4 should be upscaled
          Result := pf4bit;
        6..16: // jb: was 8..16 but seems 6 bit mono is possible too which needs to be upscaled to 8 bits
          // values > 8 bits must be downscaled to 8 bits
          Result := pf8bit;
      else
        Result := pfCustom;
      end;
    3: // Typical case is RGB or CIE L*a*b* (565 and 555 16 bit color formats would also be possible, but aren't handled
       // by the manager).
      case BitsPerSample of
        1..5: // Values < 5 should be upscaled
          Result := pf15Bit;
      else
        // Values > 8 bits should be downscaled
        Result := pf24bit;
      end;
    4: // Typical cases: RGBA and CMYK (with 8 bps, other formats like PCX's
       // 4 planes with 1 bit must be handled elsewhere)
      if BitsPerSample >= 8 then
        Result := pf32Bit
      else
        Result := pfCustom;
  else
    Result := pfCustom;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.PrepareConversion;

// Depending on the source and target pixel and color formats a conversion function must be
// determined which actually carries out the conversion
//
// Needed settings:
// - source and target samples per pixel and bits per sample
// - source and target color scheme

begin
  FRowConversion := nil;

  // Conversion of non indexed to indexed is also not supported.
  if (FTargetScheme in [csIndexed, csIndexedA]) and not (FSourceScheme in [csG, csGA, csIndexed, csIndexedA]) then
    ShowError(gesIndexedNotSupported);

  // Set up special conversion options
  if FSourceScheme in [csGA, csIndexedA, csRGBA, csBGRA, csCMYKA] then
    Include(FSourceOptions, coAlpha)
  else
    Exclude(FSourceOptions, coAlpha);

  if FTargetScheme in [csGA, csIndexedA, csRGBA, csBGRA, csCMYKA] then
    Include(FTargetOptions, coAlpha)
  else
    Exclude(FTargetOptions, coAlpha);

  case FSourceScheme of
    csG:
      case FTargetScheme of
        csBGR,
        csBGRA,
        csRGB,
        csRGBA:
          if (FTargetBPS = 8) and (FSourceBPS = 8) then
            FRowConversion := RowConvertGray2BGR;
      else
        if ((FSourceBPS in [5..64]) and (FTargetBPS in [8, 16])) or
           ((FSourceBPS = 3) and (FTargetBPS = 4)) then
          FRowConversion := RowConvertGray
        else
          FRowConversion := RowConvertIndexed8;
      end;
    csGA:
      case FTargetScheme of
        csBGR,
        csBGRA,
        csRGB,
        csRGBA:
          if (FTargetBPS = 8) and (FSourceBPS = 8) then
            FRowConversion := RowConvertGray2BGR;
      else
        if (FSourceBPS in [5..16]) and (FTargetBPS in [8, 16]) then
          FRowConversion := RowConvertGray;
      end;
    csIndexed:
      begin
        case FTargetScheme of
          csBGR,
          csBGRA,
          csRGB,
          csRGBA:
            if (FTargetBPS = 8) and (FSourceBPS <= 16) then
              FRowConversion := RowConvertIndexed2BGR;
          csIndexed,
          csIndexedA:
            begin
              // Generally use indexed conversions (with various possible bit operations),
              // assign special methods for source only, target only or source and target being 16 bits per sample
              if (FSourceBPS = 16) and (FTargetBPS = 16) then
                FRowConversion := RowConvertIndexedBoth16
              else
                if FSourceBPS = 16 then
                  FRowConversion := RowConvertIndexedSource16
                else
                  if FTargetBPS = 16 then
                    FRowConversion := RowConvertIndexedTarget16
                  else if FSourceSPP = FTargetSPP then
                    FRowConversion := RowConvertIndexed8
                  else
                    // Tiff extrasamples > 0 but apparently not a normal alpha channel
                    FRowConversion := RowConvertGray;
            end;
        end; // case
      end;
    csIndexedA:
      begin
        case FTargetScheme of
          csBGR,
          csBGRA,
          csRGB,
          csRGBA:
            if (FTargetBPS = 8) and (FSourceBPS = 8) then
              FRowConversion := RowConvertIndexed2BGR;
        else
          if (FSourceBPS = 8) and (FTargetBPS = 8) then
            FRowConversion := RowConvertGray;
        end;
      end;
    csRGB,
    csRGBA:
      case FTargetScheme of
        csRGB: FRowConversion := RowConvertRGB2RGB;
        csRGBA: FRowConversion := RowConvertRGB2RGB;
        csBGR: FRowConversion := RowConvertRGB2BGR;
        csBGRA: FRowConversion := RowConvertRGB2BGR;
        csCMY: ;
        csCMYK: ;
        csCMYKA: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csBGRA,
    csBGR:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertBGR2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertBGR2BGR;
        csCMY: ;
        csCMYK: ;
        csCMYKA: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCMY:
      case FTargetScheme of
        csRGB: ;
        csRGBA: ;
        csBGR: ;
        csBGRA: ;
        csCMY: ;
        csCMYK: ;
        csCMYKA: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCMYK,
    csCMYKA:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCMYK2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCMYK2BGR;
        csCMY: ;
        csCMYK: ;
        csCMYKA: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    csCIELab:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCIELab2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCIELab2BGR;
        csCMY: ;
        csCMYK: ;
        csCMYKA: ;
        csCIELab: ;
        csYCbCr: ;
        csG,
        csGA: FRowConversion := RowConvertGray;
      end;
    csYCbCr:
      begin
        // Create lookup tables to speed up conversion
        CreateYCbCrLookup;
        case FTargetScheme of
          csRGB,
          csRGBA: FRowConversion := RowConvertYCbCr2RGB;
          csBGR,
          csBGRA: FRowConversion := RowConvertYCbCr2BGR;
          csCMY: ;
          csCMYK: ;
          csCMYKA: ;
          csCIELab: ;
          csYCbCr: ;
        end;
      end;
    csPhotoYCC:
      begin
        // Create lookup tables to speed up conversion
        CreateYCbCrLookup;
        case FTargetScheme of
          csRGB,
          csRGBA: FRowConversion := RowConvertPhotoYCC2RGB;
          csBGR,
          csBGRA: FRowConversion := RowConvertPhotoYCC2BGR;
          csCMY: ;
          csCMYK: ;
          csCMYKA: ;
          csCIELab: ;
          csYCbCr: ;
        end;
      end;
  end;
  FChanged := False;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetSourceBitsPerSample(const Value: Byte);

begin
  if not (Value in [1..64]) then
    ShowError(gesInvalidSampleDepth);
  if FSourceBPS <> Value then
  begin
    FSourceBPS := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetSourceColorScheme(const Value: TColorScheme);

begin
  if FSourceScheme <> Value then
  begin
    FSourceScheme := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetSourceSamplesPerPixel(const Value: Byte);

begin
  // Since some image formats allow extra samples not directly corresponding to
  // image data we can't check a maximum number of samples per pixel.
  if Value = 0 then
    ShowError(gesInvalidPixelDepth);
  if FSourceSPP <> Value then
  begin
    FSourceSPP := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetTargetBitsPerSample(const Value: Byte);

begin
  if not (Value in [1..16]) then
    ShowError(gesInvalidSampleDepth);
  if FTargetBPS <> Value then
  begin
    FTargetBPS := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetTargetColorScheme(const Value: TColorScheme);

begin
  if FTargetScheme <> Value then
  begin
    FTargetScheme := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetTargetSamplesPerPixel(const Value: Byte);

begin
  if not (Value in [1..4]) then
    ShowError(gesInvalidPixelDepth);
  if FTargetSPP <> Value then
  begin
    FTargetSPP := Value;
    FChanged := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetSourceDataFormat(const Value: TSampleDataFormat);
begin
  FSourceDataFormat := Value;
  if not (Value in [sdfUnknown..sdfUndefined]) then
    ShowError(gesDataFormatNotSupported);
end;

procedure TColorManager.SetTargetDataFormat(const Value: TSampleDataFormat);
begin
  // Not supported currently!
  ShowError(gesDataFormatNotSupported);
end;

//------------------------------------------------------------------------------

procedure TColorManager.ConvertRow(Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);

// Initializes the color conversion method if necessary and calls it to do the actual conversion
//
// Needed settings:
// - source and target BPS and SPP
// - main and display gamma, if gamma correction is wanted
// - conversion options
// - YCbCr parameters if any of the color schemes is csYCbCr

begin
  // If there are pending changes then apply them
  if FChanged then
    PrepareConversion;
  // Check if there's now a conversion method
  if @FRowConversion = nil then
    ShowError(gesConversionUnsupported)
  else
    FRowConversion(Source, Target, Count, Mask);
end;

//------------------------------------------------------------------------------

// Sets flag that the bits per sample values for each channel differ and
// initializes the bits per sample for each channel.
// If ASampleCount is 1 or less then it removes the flag for multiple bps values.
procedure TColorManager.SetSourceUnequalSamples(ASampleCount: Byte; ASamples: array of Byte);
begin
  if ASampleCount > 1 then begin
    FSourceOptions := FSourceOptions + [coUnequalSamples];
    SetLength(FSourceMultiBPS, Length(ASamples));
    Move(ASamples[Low(ASamples)], FSourceMultiBPS[Low(FSourceMultiBPS)],
      Length(ASamples)*SizeOf(Byte));
  end
  else
    FSourceOptions := FSourceOptions - [coUnequalSamples];
end;

// Sets flag that the bits per sample values for each channel differ and
// initializes the bits per sample for each channel.
// If ASampleCount is 1 or less then it removes the flag for multiple bps values.
procedure TColorManager.SetTargetUnequalSamples(ASampleCount: Byte; ASamples: array of Byte);
begin
  if ASampleCount > 1 then begin
    FTargetOptions := FTargetOptions + [coUnequalSamples];
    SetLength(FTargetMultiBPS, Length(ASamples));
    Move(ASamples[Low(ASamples)], FTargetMultiBPS[Low(FTargetMultiBPS)],
      Length(ASamples)*SizeOf(Byte));
  end
  else
    FTargetOptions := FTargetOptions - [coUnequalSamples];
end;

// Set Source Palette needed if we want to convert from indexed to non indexed format.
procedure TColorManager.SetSourcePalette( Data: array of Pointer; PaletteFormat: TRawPaletteFormat; RGB: Boolean = True);
begin
  // No error checking for now.
  FSourcePaletteFormat := PaletteFormat;
  if not RGB then
    Include(FSourceOptions, coPaletteBGR);
  SetLength(FSourcePaletteData, Length(Data));
  Move(Data[Low(Data)], FSourcePaletteData[Low(FSourcePaletteData)], Length(Data)*SizeOf(Pointer));
end;

function TColorManager.CreateColorPalette(Data: array of Pointer; DataFormat: TRawPaletteFormat;
  ColorCount: Cardinal; RGB: Boolean): HPALETTE;

// Creates a color palette from the provided data which can be in various raw formats:
// - either interlaced or plane
// - 8 bits or 16 bits per component
// - in RGB or BGR order
// - with 3 or 4 components per entry (fourth is ignored)
// ColorCount determines the number of color entries to create. If this number does not equal the
// number of palette entries which would result from the given target bits per sample resolution
// then the palette is adjusted accordingly to allow conversion between resolutions.
//
// Notes: For interlaced formats only one pointer needs to be passed in Data (only the first one is used)
//        while for plane data 3 pointers are necessary (for each plane one pointer).
//        The data order is assumed rgb or bgr in interlaced order (depending on RGB). In plane mode the three needed
//        pointers must also be given such that the pointer to red components is in Data[0], the green pointer in
//        Data[1] and the blue one in Data[2]. In this case BGR is not needed.
//
// Needed settings:
// - Main and display gamma, if gamma correction is wanted
// - Options set as needed (gamma, byte swap)
// - Source and target bits per sample resolution

var
  I,
  MaxIn, MaxOut: Integer;
  LogPalette: TMaxLogPalette;
  RunR8,
  RunG8,
  RunB8: PByte;
  RunR16,
  RunG16,
  RunB16: PWord;
  Convert8: function(Value: Byte): Byte of object;
  Convert16: function(Value: Word): Byte of object;
  
begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  if ColorCount > 256 then
    LogPalette.palNumEntries := 256
  else
    LogPalette.palNumEntries := ColorCount;

  case DataFormat of
    pfInterlaced8Triple,
    pfInterlaced8Quad:
      begin
        RunR8 := Data[0];
        if coApplyGamma in FTargetOptions then
          Convert8 := ComponentGammaConvert
        else
          Convert8 := ComponentNoConvert8;

        if RGB then
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peBlue := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peGreen := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
            if DataFormat = pfInterlaced8Quad then
              Inc(RunR8);
          end;
        end
        else
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peGreen := Convert8(RunR8^); Inc(RunR8);
            LogPalette.palPalEntry[I].peBlue := Convert8(RunR8^); Inc(RunR8);
            if DataFormat = pfInterlaced8Quad then
              Inc(RunR8);
          end;
        end;
      end;
    pfPlane8Triple,
    pfPlane8Quad:
      begin
        RunR8 := Data[0];
        RunG8 := Data[1];
        RunB8 := Data[2];
        if coApplyGamma in FTargetOptions then
          Convert8 := ComponentGammaConvert
        else
          Convert8 := ComponentNoConvert8;
        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          LogPalette.palPalEntry[I].peRed := Convert8(RunR8^); Inc(RunR8);
          LogPalette.palPalEntry[I].peGreen := Convert8(RunG8^); Inc(RunG8);
          LogPalette.palPalEntry[I].peBlue := Convert8(RunB8^); Inc(RunB8);
        end;
      end;
    pfInterlaced16Triple,
    pfInterlaced16Quad:
      begin
        RunR16 := Data[0];
        if coApplyGamma in FTargetOptions then
        begin
          if coNeedByteSwap in FSourceOptions then
            Convert16 := ComponentSwapScaleGammaConvert
          else
            Convert16 := ComponentScaleGammaConvert;
        end
        else
        begin
          if coNeedByteSwap in FSourceOptions then
            Convert16 := ComponentSwapScaleConvert
          else
            Convert16 := ComponentScaleConvert16To8;
        end;

        if RGB then
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peBlue := Convert16(RunR16^); Inc(RunR16);
            if DataFormat = pfInterlaced16Quad then
              Inc(RunR16);
          end;
        end
        else
        begin
          for I := 0 to LogPalette.palNumEntries - 1 do
          begin
            LogPalette.palPalEntry[I].peBlue := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
            LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
            if DataFormat = pfInterlaced16Quad then
              Inc(RunR16);
          end;
        end;
      end;
    pfPlane16Triple,
    pfPlane16Quad:
      begin
        RunR16 := Data[0];
        RunG16 := Data[1];
        RunB16 := Data[2];
        if coApplyGamma in FTargetOptions then
        begin
          if coNeedByteSwap in FSourceOptions then
            Convert16 := ComponentSwapScaleGammaConvert
          else
            Convert16 := ComponentScaleGammaConvert;
        end
        else
        begin
          if coNeedByteSwap in FSourceOptions then
            Convert16 := ComponentSwapScaleConvert
          else
            Convert16 := ComponentScaleConvert16To8;
        end;

        for I := 0 to LogPalette.palNumEntries - 1 do
        begin
          LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peGreen := Convert16(RunG16^); Inc(RunG16);
          LogPalette.palPalEntry[I].peBlue := Convert16(RunB16^); Inc(RunB16);
        end;
      end;
  end;

  MaxIn := (1 shl FSourceBPS);
  MaxOut := (1 shl FTargetBPS);
  if (FTargetBPS <= 8) and (MaxIn <> MaxOut) then
  begin
    if FSourceBPS < 16 then begin
      // If target resolution and given color depth differ then the palette needs to be adjusted.
      // Consider the case for 2 bit to 4 bit conversion. Only 4 colors will be given to create
      // the palette but after scaling all values will be up to 15 for which no color is in the palette.
      // This and the reverse case need to be accounted for.
      if MaxIn < MaxOut then
      begin
        // Palette is too small, enhance it
        for I := MaxOut-1 downto 0 do
        begin
          LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peRed;
          LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peGreen;
          LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peBlue;
        end;
      end
      else
      begin
        // Palette contains too many entries, shorten it
        if FTargetBPS < 8 then begin
          for I := 0 to MaxOut-1 do
          begin
            LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxOut, MaxIn)].peRed;
            LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxOut, MaxIn)].peGreen;
            LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxOut, MaxIn)].peBlue;
          end;
        end
        else if FTargetBPS > 8 then begin
          // jb This is far more complicated than implemented above because a palette index
          // is not the same as color where you can just downscale from 16 bit to 8 bit
          // and only loose a little detail since subsequent palette indexes don't need
          // to have subsequent color values.
          // Therefore it's best not to use this function for 16 bit color palettes
          // Instead it should probably be converted to a 24 bit rgb image.
          // For now: just leave the palette as it is (already truncated to 256 indexes above)
        end;
      end;
    end
    else begin
    end;
    LogPalette.palNumEntries := MaxOut;
  end;

  // Finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;

//------------------------------------------------------------------------------

function TColorManager.CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;

// Creates a grayscale palette depending on the target bit depth and returns the handle of it,
// optionally applies gamma to each entry. MinimumIsWhite is True if the palette starts
// with white in index 0 decreasing to black while the index grows otherwise (and this is the usual case)
// a lower palette index has a lower brightness.
//
// Needed settings:
// - Target BPS, 16 bits are handled as 8 bits
// - Target SPP should be set to 1 (indicating an indexed image), but is not evaluated here
// - Main and display gamma, if gamma correction is wanted
// Note: Target bps must not be changed between setting gamma and creating the palette.

var
  LogPalette: TMaxLogPalette;
  I: Integer;
  BPS,
  Upper,
  Factor: Byte;

begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  // The product of BPS and SPP considers planar organizatons correctly
  // (e.g. PCX has a format 4 planes with 1 bit resulting to 16 color image)
  BPS := FTargetBPS * FTargetSPP;
  if BPS > 8 then
    BPS := 8;
  LogPalette.palNumEntries := 1 shl BPS;
  Upper := LogPalette.palNumEntries - 1;
  Factor := 255 div Upper;
  if MinimumIsWhite then
  begin
    if not (coApplyGamma in FTargetOptions) then
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[Upper - I].peBlue := I * Factor;
        LogPalette.palPalEntry[Upper - I].peGreen := I * Factor;
        LogPalette.palPalEntry[Upper - I].peRed := I * Factor;
      end;
    end
    else
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[Upper - I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peRed := FGammaTable[I * Factor];
      end;
    end;
  end
  else
  begin
    if not (coApplyGamma in FTargetOptions) then
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[I].peBlue := I * Factor;
        LogPalette.palPalEntry[I].peGreen := I * Factor;
        LogPalette.palPalEntry[I].peRed := I * Factor;
      end;
    end
    else
    begin
      for I := 0 to Upper do
      begin
        LogPalette.palPalEntry[I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peRed := FGammaTable[I * Factor];
      end;
    end;
  end;
  // Finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetGamma(MainGamma, DisplayGamma: Single);

// Sets the current gamma values and creates the gamma lookup table
//
// Needed settings:
// - Source bits per samples must be set
// - Target bits per samples must be set

var
  I,
  SourceHighBound,
  TargetHighBound: Integer;
  Gamma: Single;

begin
  if MainGamma <= 0 then
    FMainGamma := 1
  else
    FMainGamma := MainGamma;
  if DisplayGamma <= 0 then
    FDisplayGamma := 2.2 // Default value for a usual CRT
  else
    FDisplayGamma := DisplayGamma;

  Gamma := 1 / (FMainGamma * FDisplayGamma);

  // Source high bound is the maximum possible source value which can appear (0..255)
  if FSourceBPS >= 8 then
    SourceHighBound := 255
  else
    SourceHighBound := (1 shl FTargetBPS) - 1;
  // Target high bound is the target value which corresponds to a target sample value of 1 (0..255)
  if FTargetBPS >= 8 then
    TargetHighBound := 255
  else
    TargetHighBound := (1 shl FTargetBPS) - 1;
  for I := 0 to SourceHighBound  do
    FGammaTable[I] := Round(Power((I / SourceHighBound), Gamma) * TargetHighBound);
end;

//------------------------------------------------------------------------------

procedure TColorManager.SetYCbCrParameters(Values: array of Single; HSubSampling, VSubSampling: Byte);

// Sets coefficients needed to convert from YCbCr color scheme

begin
  // There must always be at least one value in an open array
  FYCbCrCoefficients[0] := Values[0];
  if High(Values) > 0 then
  begin
    FYCbCrCoefficients[1] := Values[1];
    if High(Values) > 1 then
      FYCbCrCoefficients[2] := Values[2];
  end;

  // Subsampling can be 1, 2 or 4 and vertical subsampling must always be <= horizontal subsampling
  if not (HSubSampling in [1, 2, 4]) then
    ShowError(gesInvalidSubSampling);
  if not (VSubSampling in [1, 2, 4]) then
    ShowError(gesInvalidSubSampling);
  if VSubSampling > HSubSampling then
    ShowError(gesVerticalSubSamplingError);
  FHSubSampling := HSubSampling;
  FVSubSampling := VSubSampling;
end;

//------------------------------------------------------------------------------

{$IFDEF FPC}
initialization
  // Since it's a stock object, it doesn't have to be deleted on finalization
  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
{$ENDIF}
end.
