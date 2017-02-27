// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicConfiguration.inc, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (Pleiﬂa, Germany, www.delphi-gems.com),
//
// Portions created by Mike Lischke are
// Copyright (C) 1999-2005 Mike Lischke. All Rights Reserved.
//
// Portions created by Jacob Boerema are
// Copyright (C) 2013-2017 Jacob Boerema. All Rights Reserved.
//------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// In this configuration file you can determine which parts of the library are to be compiled.
// Other configuration options include to limit the number of available image formats to
// save memory, if necessary.
//
// 2013-06-21 jb To make it easier to have different configurations we
// rename the default configuration file to ExampleGraphicConfiguration.inc.
// If you are sure that you will always use the same configuration then you
// can change the name back to GraphicConfiguration.inc. To be able to
// support multiple configurations copy this file to your project's folder
// and then change the name to GraphicConfiguration.inc.
//
// 2017-02-27 jb Added defines for using ICC profiles with lcms2. When enabled
// you will have to make sure to add lcms2.dll to the folder of your executable.
//
// IMPORTANT: a change in this include file always requires a REBUILD of your project!
//
//------------------------------------------------------------------------------

// Enable the image formats that you need below. They are ordered in several
// categories from most important to least important. Within a category they
// are ordered alphabetically on the name of the define.
// By disabling certain image formats you may be able to reduce your filesize.

////////////////////////////////////////////////////////////////////////////////
//      Common general purpose image formats
////////////////////////////////////////////////////////////////////////////////

{$define GIFGraphic}             // *.gif images

{$define JpegGraphic}            // *.jpg, *.jpeg, *.jpe, *.jfif

{$IFDEF JpegGraphic}
  // When using JpegGraphic you have several choices:

  // 1. Use the standard TJpegImage supplied by Delphi/Fpc.
  {.$DEFINE USE_TJPEGIMAGE}

  // 2. Use our JpegWrapper around the standard TJpegImage
  //    Note: Currently you will still have to add unit gexJpegWrapper to your uses!
  {.$DEFINE USE_JPEGWRAPPER}
  {$IFDEF USE_JPEGWRAPPER}
    {$UNDEF USE_TJPEGIMAGE}
  {$ENDIF}

  // 3. Use our TJpegImage replacement gexJpeg.
  //    Note: Currently you will still have to add unit gexJpeg to your uses!
  //    Note: currently only supports reading of Jpegs.
  {$DEFINE USE_GEXJPEG}

  {$IFDEF USE_GEXJPEG}
    // If you also need saving support either use one of the above defines, or
    // add the below define which will also pull in TJpegImage.
    {$DEFINE NEED_TJPEGIMAGE_SAVING}

    {$UNDEF USE_TJPEGIMAGE}
    {$UNDEF USE_JPEGWRAPPER}
  {$ENDIF}

{$ENDIF}

{$define PortableMapGraphic}     // *.ppm, *.pgm, *.pbm images

{$define PortableNetworkGraphic} // *.png images

{$define TIFFGraphic}            // *.tif, *.tiff images


////////////////////////////////////////////////////////////////////////////////
//      Special purpose image formats
////////////////////////////////////////////////////////////////////////////////

// The format used by PhotoShop image editor.
{$define PhotoshopGraphic}       // *.psd, *.pdd images

// The format used by PaintShopPro image editor.
{$define PaintshopProGraphic}    // *.psp images

// Image format used in 3D or image design.
{$define TargaGraphic}           // *.tga, *.vst, *.icb, *.vda, *.win images

// XCF. The image format used by GIMP image editor.
// This format can not be set by using a define.
// To enable it include unit gexXCF in your uses.

////////////////////////////////////////////////////////////////////////////////
//      Uncommon image formats that you may encounter once in a while.
////////////////////////////////////////////////////////////////////////////////

// We can read only certain kinds of EPS images that have a TIFF image inside.
{$define EPSGraphic}             // *.eps images

// Photo CD images, a format designed by KODAK. Not much used anymore.
{$define PCDGraphic}             // *.pcd images

// In the past a very often encountered image format, nowadays rarely used.
{$define PCXGraphic}             // *.pcx, *.pcc, *.scr images

// These two image formats are sometimes seen in 3D design.
{$define RLAGraphic}             // *.rla, *.rpf images

{$define SGIGraphic}             // *.bw, *.rgb, *.rgba, *.sgi images


////////////////////////////////////////////////////////////////////////////////
//      The image formats below are very old and hardly ever encountered.
//      Consider them deprecated and only use them if you really need them.
////////////////////////////////////////////////////////////////////////////////

// Arts & Letters Draw images, supposedly version 4 and possibly newer.
// I haven't been able to find any samples thus no way to test if it still works.
// Consider it very deprecated.
{.$define ArtsAndLettersGraphic}  // *.ged images

// The old (first) Autodesk Animator image format.
// Does not work with the slightly newer Autodesk Animator Pro images.
{.$define AutodeskGraphic}        // *.cel, *.pic images

// Dr Halo images. Not used anymore and very rarely seen.
{.$define CUTGraphic}             // *.cut (+ *.pal) images


////////////////////////////////////////////////////////////////////////////////
//
//      Adjust definitions that depend on other definitions.
////////////////////////////////////////////////////////////////////////////////

// EPSGraphic depends on TIFFGraphic so undefine it if the latter is not defined.
{$ifndef TIFFGraphic} {$undef EPSGraphic} {$endif}


////////////////////////////////////////////////////////////////////////////////
//
//      Support for ICC profiles. Disabled by default.
////////////////////////////////////////////////////////////////////////////////
{.$define LCMS}                   // Enable use of LCMS. This requires availability of lcms2.dll.
{$ifdef LCMS}
  {$define LCMS_CONVERSION}      // Enable by default conversion based on ICC profiles for images that have an ICC profile.
{$endif}

