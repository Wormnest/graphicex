unit GraphicStrings;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicStrings.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//
// Portions Created by Jacob Boerema are Copyright (C) 2013-2017 Jacob Boerema.
// All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// GraphicStrings contains the English version of the strings used in GraphicEx, which can be localized.
// Translation done by Mike Lischke (public@delphi-gems.com).
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

{$I gexdefines.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'All images';

  gesBitmaps = 'Windows bitmaps';
  gesRLEBitmaps = 'Run length encoded Windows bitmaps';
  gesDIBs = 'Device independant Windows bitmaps';
  gesEPS = 'Encapsulated Postscript images';
  gesIcons = 'Windows icons';
  gesMetaFiles = 'Windows metafiles';
  gesEnhancedMetaFiles = 'Windows enhanced meta files';
  gesJPGImages = 'JPG images';
  gesJPEGImages = 'JPEG images';
  gesJPEImages = 'JPE images';
  gesJFIFImages = 'JFIF images';
  gesTruevision = 'Truevision images';
  gesTIFF = 'Tagged image file format images';
  gesMacTIFF =  'Macintosh TIFF images';
  gesPCTIF = 'PC TIF images';
  gesGFIFax = 'GFI fax images';
  gesSGI = 'SGI images';
  gesSGITrueColor = 'SGI true color images';
  gesZSoft = 'ZSoft Paintbrush images';
  gesZSoftWord = 'Word 5.x screen capture images';
  gesAliasWaveFront = 'Alias/Wavefront images';
  gesSGITrueColorAlpha = 'SGI true color images with alpha';
  gesSGIMono = 'SGI black/white images';
  gesPhotoshop = 'Photoshop images';
  gesPortable = 'Portable map images';
  gesPortableAny = 'Portable any map images';
  gesPortablePixel = 'Portable pixel map images';
  gesPortableGray = 'Portable gray map images';
  gesPortableMono = 'Portable bitmap images';
  gesAutoDesk = 'Autodesk images';
  gesKodakPhotoCD = 'Kodak Photo-CD images';
  gesGIF = 'GIF images';
  gesHalo = 'Dr. Halo images';
  gesPaintshopPro = 'Paintshop Pro images';
  gesPaintshopProFrames = 'Paintshop Pro frames';
  gesPaintshopProTubes = 'Paintshop Pro tubes';
  gesPortableNetworkGraphic = 'Portable network graphic images';
  gesArtsAndLettersGraphic = 'Arts & Letters thumbnail images';

  // File format registration error messages
  gesRegistration = 'Attempt to register %s twice.';
  gesInvalidGraphicClass = 'Attempt to register image format %s with invalid GraphicClass.';
  gesNoExtension = 'Attempt to register image format without extension.';
  gesInvalidExtension = 'Attempt to register extension starting with ''.''';

  // image specific error messages
  gesInvalidImage = 'Cannot load image. Invalid or unexpected %s image format.';
  gesInvalidImageEx = 'Cannot load %s image. %s';
  gesInvalidColorFormat = 'Invalid color format in %s file.';
  gesStreamReadError = 'Stream read error in %s file.';
  gesUnsupportedImage = 'Cannot load image. Unsupported %s image format.';
  gesUnsupportedFeature = 'Cannot load image. %s not supported for %s files.';
  gesInvalidCRC = 'Cannot load image. CRC error found in %s file.';
  gesDecompression = 'Cannot load image. Error during decompression of %s file.';
  gesExtraCompressedData = 'Cannot load image. Extra compressed data found in %s file.';
  gesUnsupportedCompression = 'Cannot load %s image. Unsupported compression scheme.';
  gesColorScheme = 'Cannot load %s image. Color scheme missing or not supported.';
  gesInvalidPalette = 'Cannot load image. Palette in %s file is invalid.';
  gesUnknownCriticalChunk = 'Cannot load PNG image. Unexpected but critical chunk detected.';
  gesInvalidPSDLayerData = 'Image is invalid. Layer data is corrupt.';
  gesInvalidPSDResourceData = 'Image is invalid. Resource data is corrupt.';
  gesInvalidDimensions = 'Cannot load %s image. Invalid or unsupported dimensions %d x %d.';
  gesInvalidBitsPerSample = 'Cannot load %s image. Invalid value %d for bits per sample.';

  // Reasons for image loading failures
  gerInvalidDimensions = 'Invalid image dimensions (w x h): %d x %d.';
  gerInvalidColorScheme = 'Invalid or unsupported color format.';
  gerInvalidCompression = 'Invalid or unsupported compression type.';
  gerInvalidBitsPerSample = 'Invalid or unsupported bits per sample value: %d.';
  gerInvalidSamplesPerPixel = 'Invalid or unsupported samples per pixel value: %d.';
  gerUnsupportedDimensions = 'Unsupported image dimensions (w x h): %d x %d.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'The compression scheme is';
  gesRLAPixelFormat = 'Image formats other than RGB and RGBA are';
  gesPSPFileType = 'File versions other than 3 or 4 are';

  // color manager error messages
  gesIndexedNotSupported = 'Conversion from non-indexed to indexed pixel formats is not supported.';
  gesConversionUnsupported = 'Color conversion failed. Could not find a proper method.';
  gesInvalidSampleDepth = 'Color depth has invalid or unsupported bits per sample.';
  gesInvalidPixelDepth = 'Sample count per pixel does not correspond to the given color scheme.';
  gesInvalidSubSampling = 'Subsampling value is invalid. Allowed are 1, 2 and 4.';
  gesVerticalSubSamplingError = 'Vertical subsampling value must be <= horizontal subsampling value.';
  gesDataFormatNotSupported = 'Unsupported or unknown pixel data format';
  gesPaletteFormatConversionUnsupported = 'Conversion is not supported for this palette format.';
  gesIncorrectPaletteDataCount = 'Incorrect amount of palette data for this palette format.';
  gesSourceUndefined = 'Source data not assigned.';
  gesTargetUndefined = 'Target data not assigned.';
  gesGammaTableNotInitialized = 'Gamma table not initialized!';

  // progress strings
  gesPreparing = 'Preparing...';
  gesLoadingData = 'Loading data...';
  gesUpsampling = 'Upsampling...';
  gesTransfering = 'Transferring...';

  // compression errors
  gesLZ77Error = 'LZ77 decompression error.';
  gesJPEGEOI = 'JPEG decompression error. Unexpected end of input.';
  gesJPEGStripSize = 'Improper JPEG strip/tile size.';
  gesJPEGComponentCount = 'Improper JPEG component count.';
  gesJPEGDataPrecision = 'Improper JPEG data precision.';
  gesJPEGSamplingFactors = 'Improper JPEG sampling factors.';
  gesJPEGBogusTableField = 'Bogus JPEG tables field.';
  gesJPEGFractionalLine = 'Fractional JPEG scanline unsupported.';
  gesOutputBufferOverflow = 'Output buffer overflow in %s.'#10'Please report this bug. It is a serious flaw!';
  gesInputBufferOverflow = 'Input buffer overflow in %s.'#10'Please report this bug. It is a serious flaw!';

  // miscellaneous
  gesWarning = 'Warning';

  // memory access errors
  gesMemoryAccess = 'We tried to access a memory location outside the %s image buffer.'#10'Most likely this is caused by a corrupt image.';

  // Image saving errors
  gesStreamWriteError = 'Error writing to stream. Maybe drive is full or write protected.';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
