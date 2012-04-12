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
// Copyright (C) 1999-2004 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
//
// In this configuration file you can determine which parts of the library are to be compiled.
// This is in particular important for LZW compression code for which Unisys owns a patent.
// If you want to use LZW decoding then you have to make a deal with Unisys. Don't make me
// responsible for using the code.
// Other configuration options include to limit the number of available image formats to
// save memory, if necessary.
//
//----------------------------------------------------------------------------------------------------------------------

// Pick your preferred image formats here or leave them all enabled. By disabling
// certain formats which are not of interest you can save some memory.

// JGB 15-5-2006: EEN AANTAL DOOR MIJ UITGESCHAKELD DOOR EEN . ERVOOR TE ZETTEN
// 2012-04-12 temporarily turned on tga for testing

{.$define SGIGraphic}             // *.bw, *.rgb, *.rgba, *.sgi images are supported
{.$define AutodeskGraphic}        // *.cel, *.pic images
{$define TIFFGraphic}            // *.tif, *.tiff images
{$define TargaGraphic}           // *.tga, *.vst, *.icb, *.vda, *.win images
{.$define PCXGraphic}             // *.pcx, *.pcc, *.scr images
{$define PCDGraphic}             // *.pcd images
{$define PortableMapGraphic}     // *.ppm, *.pgm, *.pbm images
{.$define CUTGraphic}             // *.cut (+ *.pal) images
{.$define GIFGraphic}             // *.gif images
{.$define RLAGraphic}             // *.rla, *.rpf images
{$define PhotoshopGraphic}       // *.psd, *.pdd images
{$define PaintshopProGraphic}    // *.psp images
// JGB: png doen we d.n.v Gustavo Daud's unit so we can also save the png!
{.$define PortableNetworkGraphic} // *.png images
{.$define EPSGraphic}             // *.eps images
{.$define ArtsAndLettersGraphic}  // *.ged images

// adjust dependent definitions
{$ifndef TIFFGraphic} {$undef EPSGraphic} {$endif}

