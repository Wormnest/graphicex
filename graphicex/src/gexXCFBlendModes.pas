{ gexXCFBlendModes Gimp XCF layer blend modes
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex

}
unit gexXCFBlendModes;

interface

{$I gexdefines.inc}

uses
{$IFDEF USE_GR32}
  GR32,  Warning: not yet fully implemented!
{$ELSE}
  gexGR32Replacement,
{$ENDIF}
  gexXCFTypes,
  gexBasicBlendModes;

type
  TgexBlendFunction = procedure( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
      const Count: Integer);
  TgexRGBABlendFunction = procedure (F: TColor32; var B: TColor32; M: TColor32);

function SelectBlendFunction(const ColorMode: TGimpImageType;
  const LayerMode: TGimpLayerModeEffects): TgexBlendFunction;

// General note on the blend functions: For parameters FG and BGLayer:
// The RGBA functions expect a PColor32
// The Grayscale functions expect a PGrayAlpha
// The Indexed functions expect a PIndexedAlpha
// The reason to use PByte as actual parameter type is to be able to define
// a common function prototype that can be called for all color formats.

// --------------- RGBA Blend functions ----------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure BlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);

// --------------- GrayA Blend functions ---------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure GrayBlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);

// --------------- IndexedA Blend functions ------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure IndexedBlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);


implementation


// --------------- General functions -------------------------------------------

const
  // table of procedures corresponding to Gimp's layer modes.
  // Layer modes with a * don't have an equivalent mode yet.
  // Testing has to be done to make sure all layer modes do exactly what they're
  // supposed to do in Gimp.
  RGBABlendFunctions: array [TGimpLayerModeEffects] of TRGBABlendProc = (
    NormalBlend,        // GIMP_NORMAL_MODE
    DissolveBlend,      // GIMP_DISSOLVE_MODE
    NormalBlend,        // GIMP_BEHIND_MODE           * (not selectable in the GIMP UI)
    MultiplyBlend,      // GIMP_MULTIPLY_MODE
    ScreenBlend,        // GIMP_SCREEN_MODE
    OverlayBlend,       // GIMP_OVERLAY_MODE
    DifferenceBlend,    // GIMP_DIFFERENCE_MODE
    AdditiveBlend,      // GIMP_ADDITION_MODE
    SubtractiveBlend,   // GIMP_SUBTRACT_MODE
    DarkenBlend,        // GIMP_DARKEN_ONLY_MODE
    LightenBlend,       // GIMP_LIGHTEN_ONLY_MODE
    HueBlend,           // GIMP_HUE_MODE
    SaturationBlend,    // GIMP_SATURATION_MODE
    ColorBlend,         // GIMP_COLOR_MODE
    ValueBlend,         // GIMP_VALUE_MODE
    DivideBlend,        // GIMP_DIVIDE_MODE           
    ColorDodgeBlend,    // GIMP_DODGE_MODE
    ColorBurnBlend,     // GIMP_BURN_MODE
    HardLightBlend,     // GIMP_HARDLIGHT_MODE
    SoftLightBlend,     // GIMP_SOFTLIGHT_MODE
    GrainExtractBlend,  // GIMP_GRAIN_EXTRACT_MODE
    GrainMergeBlend,    // GIMP_GRAIN_MERGE_MODE      
    NormalBlend,        // GIMP_COLOR_ERASE_MODE      * (not selectable in the GIMP UI)
    NormalBlend,        // GIMP_ERASE_MODE            * (not selectable in the GIMP UI)
    NormalBlend,        // GIMP_REPLACE_MODE          * (not selectable in the GIMP UI)
    NormalBlend         // GIMP_ANTI_ERASE_MODE       * (not selectable in the GIMP UI)
  );

const RGBABlendFunction: TRGBABlendProc = nil;

function SelectBlendFunction(const ColorMode: TGimpImageType;
  const LayerMode: TGimpLayerModeEffects): TgexBlendFunction;
begin
  case ColorMode of
    GIMP_RGB_IMAGE,
    GIMP_RGBA_IMAGE:
      begin
        RGBABlendFunction := RGBABlendFunctions[LayerMode];
        Result := BlendLayersNormal;
      end;
    GIMP_GRAY_IMAGE,
    GIMP_GRAYA_IMAGE:
      begin
        Result := GrayBlendLayersNormal;
      end;
    GIMP_INDEXED_IMAGE,
    GIMP_INDEXEDA_IMAGE:
      begin
        Result := IndexedBlendLayersNormal;
      end;
  else
    Result := nil;
  end;
end;

// --------------- RGBA Blend functions ----------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure BlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);
var i: Integer;
  Master: TColor32;
  FG, BG: PColor32;
begin
  // WARNING: Although Master Alpha is defined as a TColor32, it is expected to
  // contain an alpha value in the range 0..255!
  Master := TColor32(MasterAlpha); // Set Master Alpha
  // Set FG and BG pointers
  FG := PColor32(FGLayer);
  BG := PColor32(BGLayer);
  // TODO: Compare speed of the current version to that when using an
  // array construction: FG[i] BG[i]
  // EVEN BETTER: Add a Count parameter to the blend mode functions and do the
  // loop inside the blend function.
  for i := 0 to Count-1 do begin
    //BlendMode.NormalBlend(FG^, BG^, Master);
    RGBABlendFunction(FG^, BG^, Master);
    Inc(FG); Inc(BG);
  end;
end;


// --------------- GrayA Blend functions ---------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure GrayBlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);
var i: Integer;
  FG, BG: PGrayAlpha;
begin
  // Set FG and BG pointers
  FG := PGrayAlpha(FGLayer);
  BG := PGrayAlpha(BGLayer);
  // TODO: Compare speed of the current version to that when using an
  // array construction: FG[i] BG[i]
  // EVEN BETTER: Add a Count parameter to the blend mode functions and do the
  // loop inside the blend function.
  for i := 0 to Count-1 do begin
    GrayNormalBlend(FG^, BG^, MasterAlpha);
    Inc(FG); Inc(BG);
  end;
end;


// --------------- IndexedA Blend functions ------------------------------------

// Blend a line from FG Layer onto BGLayer using MasterAlpha
procedure IndexedBlendLayersNormal( const FGLayer, BGLayer: PByte; const MasterAlpha: Byte;
  const Count: Integer);
var i: Integer;
  FG, BG: PIndexedAlpha;
begin
  // Set FG and BG pointers
  FG := PIndexedAlpha(FGLayer);
  BG := PIndexedAlpha(BGLayer);
  // TODO: Compare speed of the current version to that when using an
  // array construction: FG[i] BG[i]
  // EVEN BETTER: Add a Count parameter to the blend mode functions and do the
  // loop inside the blend function.
  for i := 0 to Count-1 do begin
    IndexedNormalBlend(FG^, BG^, MasterAlpha);
    Inc(FG); Inc(BG);
  end;
end;

end.
