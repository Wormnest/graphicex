{
 This unit adds most popular blendmodes + some new ones to GR32.
 Some are already available in other GR32 units, but are for the
 completeness listed here.

 Some blendmodes have been created from the descriptions Adobe Photoshop
 gives in the help file.
 Others come from Jens Gruschel & Francesco Savastano(and others) from
 various newsgroup discussions...

 I provide the Reflection, Custom & Bright Light modes.
 The custom mode is a 0..255,0..255 LUT where you can load your
 own blendmaps to - The bad thing in this implementation is that
 if several layers uses custom mode with different LUTs, you
 have to take care of the temporary loading yourself.

 For descriptions and other stuff see Jens Gruschels page at:

 http://www.pegtop.net/delphi/blendmodes/

 If you have coded some interesting modes & want them added to this unit,
 pls send me the code along with a description of purpose and use
 (i.e. "Good for adding bright objects" or so).

 If you find any lines or structures that may be optimized or if
 you're an shark with asm and want to rewrite the procs - please
 contact me - lots of the rewritting is just copy/paste stuff so ... :)


 Michael Hansen.
}
{
  Original filename: GR32_Add_BlendModes.
  The version used is that from GraphicsMagicPro v. 1.4.7 dated 2013-06-30.
  Adapted for use in combination with GraphicEx.
  To keep open the option of implementing support for GR32 we will use a define.
  Added: support for grayscale with alpha and indexed with alpha blending.
  This unit splits off the basic blend modes: the ones used in Gimp and not
  needing any extra tables initialized.
  Also removes the class object to call the blend procedures and adds calling
  the blend functions line by line instead of pixel by pixel.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex

  Jacob Boerema
}

unit gexBasicBlendModes;

{.$DEFINE USE_GR32}
{$IFDEF USE_GR32}
  This is not yet implemented!
{$ENDIF}

{.DEFINE USE_BRIGHTLIGHT} // Bright Light needs a large sqrt table to be initialized
                          // at program start. Only define this when needed.
{.$DEFINE USE_INTERPOLATION} // Optional since it initializes a table at program start

{ For the blending algorithm, you could find it at:
  http://en.wikipedia.org/wiki/Alpha_compositing
  http://www.answers.com/topic/alpha-compositing

  Quote to Michael Hansen:
  ------------------------
  A correct alpha compositing solution should use the merge formula to blend
  the RGB result of the blendmode with the background ARGB, using foreground
  alpha (eventually multiplied with masteralpha variable):

  F: Foreground
  B: Background
  C: Combined
  O: Output

  Crgb = blendmode(Frgb, Brgb)

  optional master alpha:
  Fa = Fa * Ma

  Oa = Fa + Ba * (1 - Fa)
  Orgb = (Crgb * Fa + (Brgb * Ba) * (1 - Fa)) / Oa
  ------------------------------------------------

  So, according to the above formula, the blending code should be:

  rA := fA + bA - bA * fA div 255;
  rR := ( BlendR * fA + bR * bA - bR * bA * fA div 255 ) div rA;
  rG := ( BlendG * fA + bG * bA - bG * bA * fA div 255 ) div rA;
  rB := ( BlendB * fA + bB * bA - bB * bA * fA div 255 ) div rA;



  Quote to Anders Melander:
  ------------------------
  The compositing formula used by PhotoShop is [drumroll]:

  rAlpa := fAlpha + bAlpha * (1 - fAlpha);

  rColor := (1 - fAlpha / rAlpha) * bColor +
  (fAlpha / rAlpha) * ((1 - bAlpha) * fColor + bAlpha * Blend(fColor, bColor));
  ------------------------------------------------------------------------------

  So, according to the above formula, the blending code should be:

  rA := fA + bA - ba * fa div 255;
  rR := bR - bR * fA div rA + (fR - fR * bA div 255 + BlendR * bA div 255) * fA div rA;
  rG := bG - bG * fA div rA + (fG - fG * bA div 255 + BlendG * bA div 255) * fA div rA;
  rB := bB - bB * fA div rA + (fB - fB * bA div 255 + BlendB * bA div 255) * fA div rA;

  After Optimization:
  rA := fA + bA - ba * fa div 255;
  rR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  rG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  rB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  We apply the Photoshop's formula to all the code of blending methods.


  NOTE:

  Michael Hansen has correct the first formula to:

  Oa = Fa + Ba * (1 - Fa)
  Crgb = Frgb + (blendmode(Frgb, Brgb) - Frgb) * Ba
  Orgb = (Crgb * Fa + (Brgb * Ba) * (1 - Fa)) / Oa

  This formula has the same result as Anders Melander's formula, proven by
  Anders Melander.

  Quote to Anders Melander
  ------------------------

  As far as I can tell the foreground & blend terms are exactly the same (once
  expanded), but at first look the background term appears different:

  1) Foreground/blend term

  1.1) Mine:

    (fAlpha/rAlpha)(fColor*(1 - bAlpha) + bAlpha*Blend)		[1a]


  1.2) Yours:

    Fa*(Frgb + Ba*(Blend - Frgb))/Oa =				[2a]

    (Fa/Oa)(Frgb + Ba*Blend - Ba*Frgb) =				[2b]

    (Fa/Oa)(Frgb*(1 - Ba) + Ba*Blend) =				[2c]

    (fAlpha/rAlpha)(fColor*(1 - bAlpha) + bAlpha*Blend)		[2d]

  [1a] = [2d] => Our foreground and blend terms are identical.


  2) Background term

  2.1) Mine:

    bColor*(1 - fAlpha/rAlpha)					[3a]

  Expanding rAlpha we get:

    bColor*(1 - fAlpha/(fAlpha + bAlpha*(1 - fAlpha))) =		[3b]

  (handwave)

    bColor*bAlpha*(fAlpha-1)/(bAlpha*(fAlpha-1)-fAlpha)		[3c]

  2.2) Yours:

    (Brgb*Ba)(1 - Fa)/Oa =					[4a]

    Brgb*(1 - Fa)(Ba/Oa) =					[4b]

    bColor*(1 - fAlpha)(bAlpha/rAlpha)				[4c]

  Expanding rAlpha we get:

    bColor*(1 - fAlpha)(bAlpha/(fAlpha + bAlpha*(1 - fAlpha))) =	[4d]

  (handwave)

    bColor*bAlpha*(fAlpha-1)/(bAlpha*(fAlpha-1)-fAlpha))		[4e]

  [3c] = [4e] => Our background terms are identical.

  So both our compositing formulas are in fact identical.


  3) Isolating the terms we can verify that the composition formula is correct:

  Alpha:
    rAlpha := fAlpha + bAlpha*(1 - fAlpha)			[5a]
  Foreground term:
    nS := (fAlpha/rAlpha)(fColor*(1 - bAlpha))			[5b]
  Background term:
    nD := bColor*(1 - fAlpha/rAlpha)				[5c]
  Blend term:
    nB := (fAlpha/rAlpha)(bAlpha*Blend)				[5d]
  Result:
    rColor := nS + nD + nB;					[5e]

  3.1) For Normal blend mode, Blend(fColor, bColor) = fColor, the blend term
  reduces to:

    nB := (fAlpha/rAlpha)(bAlpha*fColor)				[5f]

  3.1a) Solving for (fAlpha = bAlpha = 1) we get:

    rAlpha = 1 + 1*(1-1) = 1
    nS = (1/1)(fColor*(1-1)) = 0
    nD = bColor*(1-1/1) = 0
    nB = (1/1)(1*fColor) = fColor
    rColor = 0+0+fColor = fColor

  	Correct.

  3.1b) Solving for (fAlpha = 1, bAlpha = 0) we get:

    rAlpha = 1 + 0*(1-1) = 1
    nS = (1/1)(fColor*(1-0)) = fColor
    nD = bColor*(1-1/1) = 0
    nB = (1/1)(0*fColor) = 0
    rColor = 0+fColor+0 = fColor

	  Correct.

  3.1c) Solving for (fAlpha = 0, bAlpha = 1) we get:

    rAlpha = 0 + 1*(1-0) = 1
    nS = (0/1)(fColor*(1-1)) = 0
    nD = bColor*(1-0/1) = bColor
    nB = (0/1)(1*fColor) = 0
    rColor = 0+bColor+0 = 0

	  Correct.

  3.1d) Solving for (fAlpha = 0.5, bAlpha = 0.5) we get:

    rAlpha = 0.5 + 0.5*(1-0.5) = 0.75
    nS = (0.5/0.75)(fColor*(1-0.5)) = fColor/3
    nD = bColor*(1-0.5/0.75) = bColor/3
    nB = (0.5/0.75)(0.5*fColor) = fColor/3
    rColor = fColor/3+bColor/3+fColor/3 = 2/3*fColor + 1/3*bColor

	  Correct.

  3.1e) Solving for (fAlpha = 0.5, bAlpha = 1) we get:

    rAlpha = 0.5 + 1*(1-0.5) = 1
    nS = (0.5/1)(fColor*(1-1)) = 0
    nD = bColor*(1-0.5/1) = bColor/2
    nB = (0.5/1)(1*fColor) = fColor/2
    rColor = 0+bColor/2+fColor/2 = 1/2*fColor + 1/2*bColor

	  Correct.

  3.2) For other blend modes, substitute Blend in [5d] with the blend result,
  rinse and repeat.


  I have already verified that the output of my implementation of the above
  matches that of PhotoShop.

  -----------------------------

  Commented by: Ma Xiaoguang and Ma Xiaoming
}

{ Updates:

  2013-06-06
     Added Dissolve blend mode by Ma Xiaoguang and Ma Xiaoming.
}

interface

{$I gexdefines.inc}

uses
{$IFDEF USE_GR32}
  GR32,
{$ELSE}
  gexGR32Replacement,
{$ENDIF}
  Sysutils, Classes;

type
  // Define basic blend modes modelled on Gimp's layer blend modes.
  TgexBasicBlendModes = (
    gbmNormal,          // Normal"
    gbmDissolve,        // Dissolve"
    gbmBehind,          // Behind"        (not selectable in the GIMP UI)
    gbmMultiply,        // Multiply"
    gbmScreen,          // Screen"
    gbmOverlay,         // Overlay"
    gbmDifference,      // Difference"
    gbmAddition,        // Addition"
    gbmSubtract,        // Subtract"
    gbmDarkenOnly,      // Darken only"
    gbmLightenOnly,     // Lighten only"
    gbmHue,             // Hue"
    gbmSaturation,      // Saturation"
    gbmColor,           // Color"
    gbmValue,           // Value"
    gbmDivide,          // Divide"
    gbmDodge,           // Dodge"
    gbmBurn,            // Burn"
    gbmHardLight,       // Hard light"
    gbmSoftLight,       // Soft light"    (XCF version >= 2 only)
    gbmGrainExtract,    // Grain extract" (XCF version >= 2 only)
    gbmGrainMerge,      // Grain merge"   (XCF version >= 2 only)
    gbmColorErase,      // Color erase"   (not selectable in the GIMP UI)
    gbmErase,           // Erase"         (not selectable in the GIMP UI)
    gbmReplace,         // Replace"       (not selectable in the GIMP UI)
    gbmAntiErase        // Anti erase"    (not selectable in the GIMP UI)
  );

  // Definition of 8 bit grayscale with alpha type
  TGrayAlpha = Word;
  PGrayAlpha = ^TGrayAlpha;

  // Definition of 8 bit indexed color with alpha type
  TIndexedAlpha = Word;
  PIndexedAlpha = ^TIndexedAlpha;

  // Blend procedure type definitions
  TRGBABlendProc    = procedure(F: TColor32; var B: TColor32; M: TColor32);
  TGrayBlendProc    = procedure(F: TGrayAlpha; var B: TGrayAlpha; MasterAlpha: Byte);
  TIndexedBlendProc = procedure(F: TIndexedAlpha; var B: TIndexedAlpha; MasterAlpha: Byte);


// RGBA Color blending functions
procedure NormalBlend           (F: TColor32; var B: TColor32; M: TColor32);
procedure DissolveBlend         (F: TColor32; var B: TColor32; M: TColor32);
// Missing: Behind Blend, need to investigate if we need it since it's not available in Gimp's UI
procedure MultiplyBlend         (F: TColor32; var B: TColor32; M: TColor32);
procedure ScreenBlend           (F: TColor32; var B: TColor32; M: TColor32);
procedure OverlayBlend          (F: TColor32; var B: TColor32; M: TColor32);
procedure DifferenceBlend       (F: TColor32; var B: TColor32; M: TColor32);
procedure AdditiveBlend         (F: TColor32; var B: TColor32; M: TColor32);
procedure SubtractiveBlend      (F: TColor32; var B: TColor32; M: TColor32);
procedure DarkenBlend           (F: TColor32; var B: TColor32; M: TColor32);
procedure LightenBlend          (F: TColor32; var B: TColor32; M: TColor32);
procedure HueBlend              (F: TColor32; var B: TColor32; M: TColor32);
procedure SaturationBlend       (F: TColor32; var B: TColor32; M: TColor32);
procedure ColorBlend            (F: TColor32; var B: TColor32; M: TColor32);
procedure ValueBlend            (F: TColor32; var B: TColor32; M: TColor32);
procedure DivideBlend           (F: TColor32; var B: TColor32; M: TColor32);
procedure ColorDodgeBlend       (F: TColor32; var B: TColor32; M: TColor32);
procedure ColorBurnBlend        (F: TColor32; var B: TColor32; M: TColor32);
procedure HardLightBlend        (F: TColor32; var B: TColor32; M: TColor32);
procedure SoftLightBlend        (F: TColor32; var B: TColor32; M: TColor32);
procedure GrainExtractBlend     (F: TColor32; var B: TColor32; M: TColor32);
procedure GrainMergeBlend       (F: TColor32; var B: TColor32; M: TColor32);
// Missing: Color Erase Blend, need to investigate if we need it since it's not available in Gimp's UI
// Missing: Erase Blend, need to investigate if we need it since it's not available in Gimp's UI
// Missing: Replace Blend, need to investigate if we need it since it's not available in Gimp's UI
// Missing: Anti Erase Blend, need to investigate if we need it since it's not available in Gimp's UI

// Blend modes not defined in Gimp:

{$IFDEF USE_BRIGHTLIGHT} // Optional since it initializes a large table at program start
procedure BrightLightBlend      (F: TColor32; var B: TColor32; M: TColor32);
{$ENDIF}
{$IFDEF USE_INTERPOLATION} // Optional since it initializes a table at program start
procedure InterpolationBlend    (F: TColor32; var B: TColor32; M: TColor32);
{$ENDIF}

procedure NegationBlend         (F: TColor32; var B: TColor32; M: TColor32);
procedure ExclusionBlend        (F: TColor32; var B: TColor32; M: TColor32);
procedure LuminosityBlend       (F: TColor32; var B: TColor32; M: TColor32);
procedure AverageBlend          (F: TColor32; var B: TColor32; M: TColor32);
procedure InverseColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
procedure InverseColorBurnBlend (F: TColor32; var B: TColor32; M: TColor32);
procedure SoftColorDodgeBlend   (F: TColor32; var B: TColor32; M: TColor32);
procedure SoftColorBurnBlend    (F: TColor32; var B: TColor32; M: TColor32);
procedure ReflectBlend          (F: TColor32; var B: TColor32; M: TColor32);
procedure GlowBlend             (F: TColor32; var B: TColor32; M: TColor32);
procedure FreezeBlend           (F: TColor32; var B: TColor32; M: TColor32);
procedure HeatBlend             (F: TColor32; var B: TColor32; M: TColor32);
procedure StampBlend            (F: TColor32; var B: TColor32; M: TColor32);
procedure xorBlend              (F: TColor32; var B: TColor32; M: TColor32);
procedure andBlend              (F: TColor32; var B: TColor32; M: TColor32);
procedure orBlend               (F: TColor32; var B: TColor32; M: TColor32);
procedure RedBlend              (F: TColor32; var B: TColor32; M: TColor32);
procedure GreenBlend            (F: TColor32; var B: TColor32; M: TColor32);
procedure BlueBlend             (F: TColor32; var B: TColor32; M: TColor32);


// Grayscale (8 bit) with alpha blending functions.
// Note we can't use overload and use the same function names since the grayscale and indexed
// versions have the same parameters (TGrayAlpha and TIndexedAlpha both are Word).

procedure GrayNormalBlend(F: TGrayAlpha; var B: TGrayAlpha; MasterAlpha: Byte); overload;

// Indexed (8 bit) with alpha blending functions
procedure IndexedNormalBlend(F: TIndexedAlpha; var B: TIndexedAlpha; MasterAlpha: Byte); overload;


implementation

{$IFNDEF FPC}
{$IF CompilerVersion >= 7}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
{$IFEND}
{$ENDIF}

uses
  Math, GraphicColor;

var
{$IFDEF USE_BRIGHTLIGHT} // Optional since it initializes a large table at program start
  SqrtTable: array [0 .. 65535] of Byte;
{$ENDIF}
{$IFDEF USE_INTERPOLATION} // Optional since it initializes a table at program start
  CosineTab: array [0 .. 255] of Integer;
{$ENDIF}
  ProbTable: array [0..100, 0..99] of Boolean;


//--- Blendmodes ---------------------------------------------------------------

procedure NormalBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  ba, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure MultiplyBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR * fR div 255;
  BlendG := bG * fG div 255;
  BlendB := bB * fB div 255;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure ScreenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := 255 - (255 - fR) * (255 - bR) div 255;
  BlendG := 255 - (255 - fG) * (255 - bG) div 255;
  BlendB := 255 - (255 - fB) * (255 - bB) div 255;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure OverlayBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if bR < 128 then
  begin
    BlendR := bR * fR div 128;
  end
  else
  begin
    BlendR := 255 - (255 - bR) * (255 - fR) div 128;
  end;

  if bG < 128 then
  begin
    BlendG := bG * fG div 128;
  end
  else
  begin
    BlendG := 255 - (255 - bG) * (255 - fG) div 128;
  end;

  if bB < 128 then
  begin
    BlendB := bB * fB div 128;
  end
  else
  begin
    BlendB := 255 - (255 - bB) * (255 - fB) div 128;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{ Soft Light - formula by Jens Gruschel }
procedure SoftLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM, C                 : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;
  
  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  C      := bR * fR div 255;
  BlendR := C + bR * (  255 - ( (255 - bR) * (255 - fR) div 255 ) - C  ) div 255;

  C      := bG * fG div 255;
  BlendG := C + bG * (  255 - ( (255 - bG) * (255 - fG) div 255 ) - C  ) div 255;

  C      := bB * fB div 255;
  BlendB := C + bB * (  255 - ( (255 - bB) * (255 - fB) div 255 ) - C  ) div 255;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure HardLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if fR < 128 then
  begin
    BlendR := bR * fR div 128;
  end
  else
  begin
    BlendR := 255 - (255 - bR) * (255 - fR) div 128;
  end;

  if fG < 128 then
  begin
    BlendG := bG * fG div 128;
  end
  else
  begin
    BlendG := 255 - (255 - bG) * (255 - fG) div 128;
  end;

  if fB < 128 then
  begin
    BlendB := bB * fB div 128;
  end
  else
  begin
    BlendB := 255 - (255 - bB) * (255 - fB) div 128;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{$IFDEF USE_BRIGHTLIGHT} // Optional since it initializes a large table at program start
{ Bright Light - Introduced by Michael Hansen -  much like average }
procedure BrightLightBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := SqrtTable[fR * bR];
  BlendG := SqrtTable[fG * bG];
  BlendB := SqrtTable[fB * bB];

  {Blend}
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;
{$ENDIF}

{ Gimp: Divide:        f(x1,x2, 15) = CLAMP(x1/x2)
  In the "Divide", "Dodge", and "Burn" modes, division by zero should
  be considered to produce a number so large that CLAMP(x/0) = 1 unless
  x=0, in which case CLAMP(0/0) = 0.
}
procedure DivideBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  {BlendR := fR;
  BlendG := fG;
  BlendB := fB;}

  BlendR := bR * 256 div (1 + fR);
  if BlendR > 255 then
  begin
    BlendR := 255;
  end;

  BlendG := bG * 256 div (1 + fG);
  if BlendG > 255 then
  begin
    BlendG := 255;
  end;

  BlendB := bB * 256 div (1 + fB);
  if BlendB > 255 then
  begin
    BlendB := 255;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure ColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR; 
  BlendG := fG;
  BlendB := fB;

  if fR < 255 then
  begin
    BlendR := bR * 255 div (255 - fR);

    if BlendR > 255 then
    begin
      BlendR := 255;
    end;
  end;

  if fG < 255 then
  begin
    BlendG := bG * 255 div (255 - fG);

    if BlendG > 255 then
    begin
      BlendG := 255;
    end;
  end;

  if fB < 255 then
  begin
    BlendB := bB * 255 div (255 - fB);

    if BlendB > 255 then
    begin
      BlendB := 255;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure ColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if fR > 0 then
  begin
    Temp := 255 - ( (255 - bR) * 255 div fR );

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if fG > 0 then
  begin
    Temp := 255 - ( (255 - bG) * 255 div fG );

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if fB > 0 then
  begin
    Temp := 255 - ( (255 - bB) * 255 div fB );

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure DarkenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if fR > bR then
  begin
    BlendR := bR;
  end;

  if fG > bG then
  begin
    BlendG := bG;
  end;

  if fB > bB then
  begin
    BlendB := bB;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure LightenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if fR < bR then
  begin
    BlendR := bR;
  end;

  if fG < bG then
  begin
    BlendG := bG;
  end;

  if fB < bB then
  begin
    BlendB := bB;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA; 

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure DifferenceBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := Abs(bR - fR);
  BlendG := Abs(bG - fG);
  BlendB := Abs(bB - fB);

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{ Negation - introduced by Jens Gruschel }
procedure NegationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := 255 - Abs(255 - bR - fR);
  BlendG := 255 - Abs(255 - bG - fG);
  BlendB := 255 - Abs(255 - bB - fB);

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure ExclusionBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR + fR - bR * fR div 128;
  BlendG := bG + fG - bG * fG div 128;
  BlendB := bB + fB - bB * fB div 128;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure HueBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  fH, fS, fL            : Single;
  bH, bS, bL            : Single;
  NewHSL                : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }

  RGBToHSL(F, fH, fS, fL);        // Invert Channel To HSL
  RGBToHSL(B, bH, bS, BL);        // Invert Channel To HSL

  NewHSL := HSLToRGB(fH, bS, bL); // Combine HSL and invert it to RGB

  BlendR := NewHSL shr 16 and $FF;
  BlendG := NewHSL shr  8 and $FF;
  BlendB := NewHSL        and $FF;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure SaturationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  fH, fS, fL            : Single;
  bH, bS, bL            : Single;
  NewHSL                : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  RGBToHSL(F, fH, fS, fL);        // Invert Channel To HSL
  RGBToHSL(B, bH, bS, BL);        // Invert Channel To HSL

  NewHSL := HSLToRGB(bH, fS, bL); // Combine HSL and invert it to RGB

  BlendR := NewHSL shr 16 and $FF;
  BlendG := NewHSL shr  8 and $FF;
  BlendB := NewHSL        and $FF;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure ColorBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  fH, fS, fL            : Single;
  bH, bS, bL            : Single;
  NewHSL                : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  RGBToHSL(F, fH, fS, fL);        // Invert channel to HLS
  RGBToHSL(B, bH, bS, BL);        // Invert channel to HLS

  NewHSL := HSLToRGB(fH, fS, bL); // Combine HSL and invert it to RGB

  BlendR := NewHSL shr 16 and $FF;
  BlendG := NewHSL shr  8 and $FF;
  BlendB := NewHSL        and $FF;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{
 Gimp: Mode 14: Value (V of HSV)

  h(r1,g1,b1, r2,g2,b2, 14) is the color that has
    the hue of (r1,g1,b1)
    the value of (r2,g2,b2)
    the HSV-saturation of (r1,g1,b1)
}
procedure ValueBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  fH, fS, fV            : Integer; // For now, maybe later change to Single.
  bH, bS, bV            : Integer; // For now, maybe later change to Single.
  NewHSV                : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  RGBToHSV32(F, fH, fS, fV);        // Invert channel to HSV
  RGBToHSV32(B, bH, bS, bV);        // Invert channel to HSV

  NewHSV := HSVToRGB32($FF, bH, bS, fV); // Combine HSV and invert it to RGB

  BlendR := NewHSV shr 16 and $FF;
  BlendG := NewHSV shr  8 and $FF;
  BlendB := NewHSV        and $FF;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure LuminosityBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  fH, fS, fL            : Single;
  bH, bS, bL            : Single;
  NewHSL                : TColor32;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  RGBToHSL(F, fH, fS, fL);        // Invert channel To HSL
  RGBToHSL(B, bH, bS, BL);        // Invert channel To HSL

  NewHSL := HSLToRGB(bH, bS, fL); // Combine HSL and invert it to RGB

  { Channel separation }
  BlendR := NewHSL shr 16 and $FF;
  BlendG := NewHSL shr  8 and $FF;
  BlendB := NewHSL        and $FF;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{ Average - useful in some cases - but the same as Normal with MasterAlpha = 128 }
procedure AverageBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := (fR + bR) div 2;
  BlendG := (fg + bG) div 2;
  BlendB := (fB + bB) div 2;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure InverseColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if bR = 255 then
  begin
    BlendR := 255;
  end
  else
  begin
    BlendR := fR * 255 div (255 - bR);

    if BlendR > 255 then
    begin
      BlendR := 255;
    end;
  end;

  if bG = 255 then
  begin
    BlendG := 255;
  end
  else
  begin
    BlendG := fG * 255 div (255 - bG);

    if BlendG > 255 then
    begin
      BlendG := 255;
    end;
  end;

  if bB = 255 then
  begin
    BlendB := 255;
  end
  else
  begin
    BlendB := fB * 255 div (255 - bB);

    if BlendB > 255 then
    begin
      BlendB := 255;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure InverseColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if bR = 0 then
  begin
    BlendR := 0;
  end
  else
  begin
    Temp := 255 - (255 - fR) * 255 div bR;

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if bG = 0 then
  begin
    BlendG := 0;
  end
  else
  begin
    Temp := 255 - (255 - fG) * 255 div bG;

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if bB = 0 then
  begin
    BlendB := 0;
  end
  else
  begin
    Temp := 255 - (255 - fB) * 255 div bB;

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure SoftColorDodgeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if (bR + fR) < 256 then
  begin
    if fR <> 255 then
    begin
      BlendR := bR * 128 div (255 - fR);

      if BlendR > 255 then
      begin
        BlendR := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - fR) * 128 div bR;

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if (bG + fG) < 256 then
  begin
    if fG <> 255 then
    begin
      BlendG := bG * 128 div (255 - fG);

      if BlendG > 255 then
      begin
        BlendG := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - fG) * 128 div bG;

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if (bB + fB) < 256 then
  begin
    if fB <> 255 then
    begin
      BlendB := bB * 128 div (255 - fB);

      if BlendB > 255 then
      begin
        BlendB := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - fB) * 128 div bB;

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure SoftColorBurnBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if (bR + fR) < 256 then
  begin
    if bR = 255 then
    begin
      BlendR := 255;
    end
    else
    begin
      BlendR := fR * 128 div (255 - bR);

      if BlendR > 255 then
      begin
        BlendR := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - bR) * 128  div fR;

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if (bG + fG) < 256 then
  begin
    if bG = 255 then
    begin
      BlendG := 255;
    end
    else
    begin
      BlendG := fG * 128 div (255 - bG);

      if BlendG > 255 then
      begin
        BlendG := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - bG) * 128 div fG;

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if (bB + fB) < 256 then
  begin
    if bB = 255 then
    begin
      BlendB := 255;
    end
    else
    begin
      BlendB := fB * 128 div (255 - bB);

      if BlendB > 255 then
      begin
        BlendB := 255;
      end;
    end;
  end
  else
  begin
    Temp := 255 - (255 - bB) * 128 div fB;

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{ Reflect - introduced by Michael Hansen }
procedure ReflectBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if fR <> 255 then
  begin
    BlendR := Sqr(bR) div (255 - fR);

    if BlendR > 255 then
    begin
      BlendR := 255;
    end;
  end;

  if fG <> 255 then
  begin
    BlendG := Sqr(bG) div (255 - fG);

    if BlendG > 255 then
    begin
      BlendG := 255;
    end;
  end;

  if fB <> 255 then
  begin
    BlendB := Sqr(bB) div (255 - fB);

    if BlendB > 255 then
    begin
      BlendB := 255;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure GlowBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  if bR < 255 then
  begin
    BlendR := sqr(fR) div (255 - bR);

    if BlendR > 255 then
    begin
      BlendR := 255;
    end;
  end
  else
  begin
    BlendR := 255;
  end;

  if bG < 255 then
  begin
    BlendG := sqr(fG) div (255 - bG);

    if BlendG > 255 then
    begin
      BlendG := 255;
    end;
  end
  else
  begin
    BlendG := 255;
  end;

  if bB < 255 then
  begin
    BlendB := sqr(fB) div (255 - bB);

    if BlendB > 255 then
    begin
      BlendB := 255;
    end;
  end
  else
  begin
    BlendB := 255;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure FreezeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := fG;
  BlendB := fB;

  if fR > 0 then
  begin
    Temp := 255 - Sqr(255 - bR) div fR;

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if fG > 0 then
  begin
    Temp := 255 - Sqr(255 - bG) div fG;

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if fB > 0 then
  begin
    Temp := 255 - Sqr(255 - bB) div fB;

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure HeatBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr 8  and $FF;
  bB := B        and $FF;

  { Combine }
  if bR = 0 then
  begin
    BlendR := 0;
  end
  else
  begin
    Temp := 255 - Sqr(255 - fR) div bR;

    if Temp < 0 then
    begin
      BlendR := 0;
    end
    else
    begin
      BlendR := Temp;
    end;
  end;

  if bG = 0 then
  begin
    BlendG := 0;
  end
  else
  begin
    Temp := 255 - Sqr(255 - fG) div bG;

    if Temp < 0 then
    begin
      BlendG := 0;
    end
    else
    begin
      BlendG := Temp;
    end;
  end;

  if bB = 0 then
  begin
    BlendB := 0;
  end
  else
  begin
    Temp := 255 - Sqr(255 - fB) div bB;

    if Temp < 0 then
    begin
      BlendB := 0;
    end
    else
    begin
      BlendB := Temp;
    end;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure AdditiveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR + bR;
  BlendG := fG + bG;
  BlendB := fB + bB;

  if BlendR > 255 then
  begin
    BlendR := 255;
  end;

  if BlendG > 255 then
  begin
    BlendG := 255;
  end;

  if BlendB > 255 then
  begin
    BlendB := 255;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure SubtractiveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  Temp := bR + fR - 256;
  
  if Temp < 0 then
  begin
    BlendR := 0;
  end
  else
  begin
    BlendR := Temp;
  end;

  Temp := bG + fG - 256;

  if Temp < 0 then
  begin
    BlendG := 0;
  end
  else
  begin
    BlendG := Temp;
  end;

  Temp := bB + fB - 256;

  if Temp < 0 then
  begin
    BlendB := 0;
  end
  else
  begin
    BlendB := Temp;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure GrainMergeBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR + bR - 128;
  BlendG := fG + bG - 128;
  BlendB := fB + bB - 128;

  if BlendR > 255 then
  begin
    BlendR := 255;
  end
  else if BlendR < 0 then
    BlendR := 0;

  if BlendG > 255 then
  begin
    BlendG := 255;
  end
  else if BlendG < 0 then
    BlendG := 0;

  if BlendB > 255 then
  begin
    BlendB := 255;
  end
  else if BlendB < 0 then
    BlendB := 0;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure GrainExtractBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  Temp := bR + fR - 128;

  if Temp < 0 then
  begin
    BlendR := 0;
  end
  else
  begin
    BlendR := Temp;
  end;

  Temp := bG + fG - 128;

  if Temp < 0 then
  begin
    BlendG := 0;
  end
  else
  begin
    BlendG := Temp;
  end;

  Temp := bB + fB - 128;

  if Temp < 0 then
  begin
    BlendB := 0;
  end
  else
  begin
    BlendB := Temp;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

{$IFDEF USE_INTERPOLATIONBLEND} // Optional since it initializes a large table at program start
procedure InterpolationBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;
  
  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := CosineTab[fR] + CosineTab[bR];
  BlendG := CosineTab[fG] + CosineTab[bG];
  BlendB := CosineTab[fB] + CosineTab[bB];

  if BlendR > 255 then
  begin
    BlendR := 255;
  end;

  if BlendG > 255 then
  begin
    BlendG := 255;
  end;

  if BlendB > 255 then
  begin
    BlendB := 255;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;
{$ENDIF}

procedure StampBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
  Temp                  : Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  Temp := bR + fR * 2 - 255; //256;

  if Temp < 0 then
  begin
    BlendR := 0;
  end
  else if Temp > 255 then
  begin
    BlendR := 255;
  end
  else
  begin
    BlendR := Temp;
  end;

  Temp := bG + fG * 2 - 255; //256;

  if Temp < 0 then
  begin
    BlendG := 0;
  end
  else if Temp > 255 then
  begin
    BlendG := 255;
  end
  else
  begin
    BlendG := Temp;
  end;

  Temp := bB + fB * 2 - 255; //256;

  if Temp < 0 then
  begin
    BlendB := 0;
  end
  else if Temp > 255 then
  begin
    BlendB := 255;
  end
  else
  begin
    BlendB := Temp;
  end;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure xorBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR xor fR;
  BlendG := bG xor fG;
  BlendB := bB xor fB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure andBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR and fR;
  BlendG := bG and fG;
  BlendB := bB and fB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure orBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR or fR;
  BlendG := bG or fG;
  BlendB := bB or fB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure RedBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := fR;
  BlendG := bG;
  BlendB := bB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure GreenBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR;
  BlendG := fG;
  BlendB := bB;

  {Blend}
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure BlueBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  aM                    : Byte;
  fA, fR, fG, fB        : Byte;
  bA, bR, bG, bB        : Byte;
  rA, rR, rG, rB        : Byte;
  tR, tG, tB            : Integer;
  BlendR, BlendG, BlendB: Integer;
begin
  {Foreground Alpha and Master Alpha combined}
  aM := M and $FF;
  fA := F shr 24 and $FF;
  fA := fA * aM div 255;

  if fA = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  { Channel separation }
  fR := F shr 16 and $FF;
  fG := F shr  8 and $FF;
  fB := F        and $FF;

  bA := B shr 24 and $FF;
  bR := B shr 16 and $FF;
  bG := B shr  8 and $FF;
  bB := B        and $FF;

  { Combine }
  BlendR := bR;
  BlendG := bG;
  BlendB := fB;

  { Blend }
  rA := fA + bA - ba * fa div 255;

  tR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
  tG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
  tB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

  if tR < 0 then
  begin
    rR := 0;
  end
  else if tR > 255 then
  begin
    rR := 255;
  end
  else
  begin
    rR := tR;
  end;

  if tG < 0 then
  begin
    rG := 0;
  end
  else if tG > 255 then
  begin
    rG := 255;
  end
  else
  begin
    rG := tG;
  end;

  if tB < 0 then
  begin
    rB := 0;
  end
  else if tB > 255 then
  begin
    rB := 255;
  end
  else
  begin
    rB := tB;
  end;

  B := (rA shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;

procedure DissolveBlend(F: TColor32; var B: TColor32; M: TColor32);
var
  LProbIndex  : Cardinal;
  LRandomIndex: Integer;
begin
  LProbIndex   := Round( (M and $FF) / 255 * 100 );
  LRandomIndex := Random(100);

  if not ProbTable[LProbIndex, LRandomIndex] then
  begin
    F := F and $00FFFFFF;
  end;

  NormalBlend(F, B, 255);
end;

//-- Grayscale blending functions  ---------------------------------------------

procedure GrayNormalBlend(F: TGrayAlpha; var B: TGrayAlpha; MasterAlpha: Byte);
var
  aMaster        : Byte;
  fAlpha, bAlpha : Byte;
  fGray, bGray   : Byte;
  rAlpha, rGray  : Byte;
//  BlendGray      : Word;
  tGray          : SmallInt;

begin
  {Foreground Alpha and Master Alpha combined}
  aMaster := MasterAlpha and $FF;
  fAlpha := F shr 8      and $FF;
  fAlpha := fAlpha * aMaster div 255;

  if fAlpha = 0 then
  begin
    Exit;  //exit if nothing changes ...
  end;

  fGray  := F       and $FF;
  bAlpha := B shr 8 and $FF;
  bGray  := B       and $FF;

{ not actually used thus commented out
  BlendGray := fGray;
}
  { Blend }
  rAlpha := fAlpha + bAlpha - bAlpha * fAlpha div 255;

  // Since (fGray - BlendGray) always resolves to 0 we comment it out
  tGray := bGray - bGray * fAlpha div rAlpha + (fGray - {(fGray - BlendGray)}0 * bAlpha div 255) * fAlpha div rAlpha;

  if tGray < 0 then
  begin
    rGray := 0;
  end
  else if tGray > 255 then
  begin
    rGray := 255;
  end
  else
  begin
    rGray := tGray;
  end;

  B := (rAlpha shl 8) or rGray;
end;

//-- Indexed blending functions  -----------------------------------------------

procedure IndexedNormalBlend(F: TIndexedAlpha; var B: TIndexedAlpha; MasterAlpha: Byte);
var
  aMaster        : Byte;
  fAlpha         : Byte;
begin
  // For indexed according to the Gimp specification Normal mode is:
  // COMPOSITE(a1,i1, a2,i2,Normal) = if a2 > 0.5 then (1.0,i2) else (a1,i1)

  {Foreground Alpha and Master Alpha combined}
  aMaster := MasterAlpha and $FF;
  fAlpha := F shr 8      and $FF;
  fAlpha := fAlpha * aMaster div 255;

  if fAlpha >= 128 then
    B := $FF00 or F;
end;

//-- Initialization part -------------------------------------------------------

procedure InitTables;
var
  i, j: Integer;
{$IFDEF USE_BRIGHTLIGHT} // Optional since it initializes a large table at program start
  x: Integer;
  LTmp: Integer;
{$ENDIF}
begin
{$IFDEF USE_BRIGHTLIGHT} // Optional since it initializes a large table at program start
  { Init SqrtTable }
  for x := 0 to 65535 do
  begin
    LTmp := Round(Sqrt(x));

    if LTmp <= 255 then
    begin
      SqrtTable[x] := LTmp;
    end
    else
    begin
      SqrtTable[x] := 255;
    end;
  end;
{$ENDIF}

(**
  { Init Custom Blendmap - like normal blend }
  for x := 0 to 255 do
  begin
    for y := 0 to 255 do
    begin
      FillChar(BlendMap[y + x shl 8], 3, x);
    end;
  end;
**)

{$IFDEF USE_INTERPOLATION} // Optional since it initializes a table at program start
  { Init CosineTable }
  for i := 0 to 255 do
  begin
    CosineTab[i] := Round( 64 - Cos(i * Pi / 255) * 64 );
  end;
{$ENDIF}

  { Init ProbTable -- Probability Table }
  for i := 0 to 100 do
  begin
    for j := 0 to 99 do
    begin
      if j < i then
      begin
        ProbTable[i, j] := True;
      end
      else
      begin
        ProbTable[i, j] := False;
      end;
    end;
  end;
end;

initialization
  InitTables;
  Randomize;
end.

