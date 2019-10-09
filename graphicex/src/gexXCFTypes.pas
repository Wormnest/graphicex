{ gexXCF Type definitions for XCF image format.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  Based on xcftools and gimp sources.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}
unit gexXCFTypes;

interface

{$I gexdefines.inc}

type
////////////////////////////////////////////////////////////////////////////////
// From: libgimpbase/gimpbaseenums.h
////////////////////////////////////////////////////////////////////////////////
  // GimpImageBaseType Enum:
  TGimpImageBaseType = (
    GIMP_RGB,             // RGB color
    GIMP_GRAY,            // Grayscale
    GIMP_INDEXED);        // Indexed color

  // GimpImageType Enum: (defined for all layers)
  TGimpImageType = (
    GIMP_RGB_IMAGE,         // RGB
    GIMP_RGBA_IMAGE,        // RGB-alpha
    GIMP_GRAY_IMAGE,        // Grayscale
    GIMP_GRAYA_IMAGE,       // Grayscale-alpha
    GIMP_INDEXED_IMAGE,     // Indexed
    GIMP_INDEXEDA_IMAGE     // Indexed-alpha
  );


////////////////////////////////////////////////////////////////////////////////
// From: app/xcf/xcf-private.h
////////////////////////////////////////////////////////////////////////////////
const
  // PropType Enum:
  PROP_END                =  0;
  PROP_COLORMAP           =  1;
  PROP_ACTIVE_LAYER       =  2;
  PROP_ACTIVE_CHANNEL     =  3;
  PROP_SELECTION          =  4;
  PROP_FLOATING_SELECTION =  5;
  PROP_OPACITY            =  6;
  PROP_MODE               =  7;
  PROP_VISIBLE            =  8;
  PROP_LINKED             =  9;
  PROP_LOCK_ALPHA         = 10;
  PROP_APPLY_MASK         = 11;
  PROP_EDIT_MASK          = 12;
  PROP_SHOW_MASK          = 13;
  PROP_SHOW_MASKED        = 14;
  PROP_OFFSETS            = 15;
  PROP_COLOR              = 16;
  PROP_COMPRESSION        = 17;
  PROP_GUIDES             = 18;
  PROP_RESOLUTION         = 19;
  PROP_TATTOO             = 20;
  PROP_PARASITES          = 21;
  PROP_UNIT               = 22;
  PROP_PATHS              = 23;
  PROP_USER_UNIT          = 24;
  PROP_VECTORS            = 25;
  PROP_TEXT_LAYER_FLAGS   = 26;
  PROP_SAMPLE_POINTS      = 27;
  PROP_LOCK_CONTENT       = 28;
  PROP_GROUP_ITEM         = 29;
  PROP_ITEM_PATH          = 30;
  PROP_GROUP_ITEM_FLAGS   = 31;

  // XcfCompressionType Enum:
  COMPRESS_NONE              =  0;
  COMPRESS_RLE               =  1;
  COMPRESS_ZLIB              =  2;  // unused
  COMPRESS_FRACTAL           =  3;  // unused

  // XcfOrientationType Enum:
  XCF_ORIENTATION_HORIZONTAL = 1;
  XCF_ORIENTATION_VERTICAL   = 2;

  // XcfStrokeType Enum:
  XCF_STROKETYPE_STROKE        = 0;
  XCF_STROKETYPE_BEZIER_STROKE = 1;

  // XcfGroupItemFlagsType Enum:
  XCF_GROUP_ITEM_EXPANDED      = 1;

////////////////////////////////////////////////////////////////////////////////
// From: app/base/base-enums.h
////////////////////////////////////////////////////////////////////////////////

// GimpLayerModeEffects: Layer mode effects Enum
type TGimpLayerModeEffects = (
  GIMP_NORMAL_MODE,          // Normal"
  GIMP_DISSOLVE_MODE,        // Dissolve"
  GIMP_BEHIND_MODE,          // Behind"        (not selectable in the GIMP UI)
  GIMP_MULTIPLY_MODE,        // Multiply"
  GIMP_SCREEN_MODE,          // Screen"
  GIMP_OVERLAY_MODE,         // Overlay"
  GIMP_DIFFERENCE_MODE,      // Difference"
  GIMP_ADDITION_MODE,        // Addition"
  GIMP_SUBTRACT_MODE,        // Subtract"
  GIMP_DARKEN_ONLY_MODE,     // Darken only"
  GIMP_LIGHTEN_ONLY_MODE,    // Lighten only"
  GIMP_HUE_MODE,             // Hue"
  GIMP_SATURATION_MODE,      // Saturation"
  GIMP_COLOR_MODE,           // Color"
  GIMP_VALUE_MODE,           // Value"
  GIMP_DIVIDE_MODE,          // Divide"
  GIMP_DODGE_MODE,           // Dodge"
  GIMP_BURN_MODE,            // Burn"
  GIMP_HARDLIGHT_MODE,       // Hard light"
  GIMP_SOFTLIGHT_MODE,       // Soft light"    (XCF version >= 2 only)
  GIMP_GRAIN_EXTRACT_MODE,   // Grain extract" (XCF version >= 2 only)
  GIMP_GRAIN_MERGE_MODE,     // Grain merge"   (XCF version >= 2 only)
  GIMP_COLOR_ERASE_MODE,     // Color erase"   (not selectable in the GIMP UI)
  GIMP_ERASE_MODE,           // Erase"         (not selectable in the GIMP UI)
  GIMP_REPLACE_MODE,         // Replace"       (not selectable in the GIMP UI)
  GIMP_ANTI_ERASE_MODE       // Anti erase"    (not selectable in the GIMP UI)
  );


////////////////////////////////////////////////////////////////////////////////
// From: app/base/tile.h
////////////////////////////////////////////////////////////////////////////////

const
  TILE_WIDTH   = 64;
  TILE_HEIGHT  = 64;

////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// From: app/core/gimpimage-colormap.h
////////////////////////////////////////////////////////////////////////////////

GIMP_IMAGE_COLORMAP_SIZE = 768;

////////////////////////////////////////////////////////////////////////////////

implementation

end.
