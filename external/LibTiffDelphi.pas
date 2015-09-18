{ LibTiffDelphi
  This version is based on the one found in ImagingLib which
  is based on libtiff 3.9.1 and 3.9.4.
  The version here was first updated to libtiff 3.9.7 and further enhanced
  and adapted by me Jacob Boerema.
}

unit LibTiffDelphi;

{$IFDEF FPC}
  {$mode delphi}
  // We need cvar enabled here which is on in objfpc but off by default in delphi mode.
  // In the cvar enabled mode we can use named public variables which is not
  // possible by default in delphi mode.
  {$modeswitch cvar}
{$ENDIF}

{$ALIGN 8}
{$MINENUMSIZE 1}

interface

uses
  Windows, SysUtils, Classes, C_Types;

const
  CPrecompiled    = #13#10'Pre-compiled LibTiff for ';
  CRepository     = #13#10'https://bitbucket.org/jacobb/jgb-thirdparty'#13#10;
  CLibTiffDelphi  = 'LibTiffDelphi ';
  {$IFDEF LIBTIFF4}
  CLibTiffVersion = '4.0.4';
  {$ELSE}
  CLibTiffVersion = '3.9.7';
  {$ENDIF}
  {$IFNDEF FPC}
  CCompiler       = 'Delphi';
  {$ELSE}
  CCompiler       = 'Fpc/Lazarus';
  {$ENDIF}
  // Define this as a typed const; that way it will always get included.
  LibTiffDelphiVersionString: AnsiString = CLibTiffDelphi + CLibTiffVersion +
    CPrecompiled + CCompiler + CRepository;


  // tiff.h

  {$IFDEF LIBTIFF4}
  TIFF_VERSION_CLASSIC  = 42;
  TIFF_VERSION_BIG      = 43;
  {$ELSE}
  TIFF_VERSION          = 42;
  TIFF_BIGTIFF_VERSION  = 43;
  {$ENDIF}
  TIFF_BIGENDIAN     = $4D4D;
  TIFF_LITTLEENDIAN  = $4949;
  MDI_LITTLEENDIAN   = $5045;
  MDI_BIGENDIAN      = $4550;

type

{*
 * Intrinsic data types required by the file format:
 *
 * 8-bit quantities     int8/uint8
 * 16-bit quantities    int16/uint16
 * 32-bit quantities    int32/uint32
 * 64-bit quantities    int64/uint64
 * strings              unsigned char*
 *}
  int8   = ShortInt;
  uint8  = Byte;
  int16  = SmallInt;
  uint16 = Word;
  int32  = Integer;
  uint32 = LongWord;
  //int64 - Already defined

  puint8 = ^uint8;
  puint16 = ^uint16;
  pint32 = ^int32;
  puint32 = ^uint32;
  ppuint32 = ^puint32;

{*
 * Some types as promoted in a variable argument list
 * We use uint16_vap rather then directly using int, because this way
 * we document the type we actually want to pass through, conceptually,
 * rather then confusing the issue by merely stating the type it gets
 * promoted to
 *}

  uint16_vap = Integer; // Dependant on 32/64 bits compiler? Needs to be checked!!!

  // TIFF header.

  {$IFNDEF LIBTIFF4}
  TTIFFHeader = record
    tiff_magic: Word;      // magic number (defines byte order)
    tiff_version: Word;    // TIFF version number
    tiff_diroff: Cardinal; // byte offset to first directory
  end;
  {$ELSE}
  TTIFFHeaderCommon = record
    tiff_magic: uint16;      // magic number (defines byte order)
    tiff_version: uint16;    // TIFF version number
  end;

  TTIFFHeaderClassic = record
    tiff_magic: uint16;      // magic number (defines byte order)
    tiff_version: uint16;    // TIFF version number
    tiff_diroff: uint32;     // byte offset to first directory
  end;

  TTIFFHeaderBig = record
    tiff_magic: uint16;      // magic number (defines byte order)
    tiff_version: uint16;    // TIFF version number
    tiff_offsetsize: uint16; // size of offsets, should be 8
    tiff_unused: uint16;     // unused word, should be 0
    tiff_diroff: uint64;     // byte offset to first directory
  end;
  {$ENDIF}

const
  {  NB: In the comments below,
     - items marked with a + are obsoleted by revision 5.0,
     - items marked with a ! are introduced in revision 6.0.
     - items marked with a % are introduced post revision 6.0.
     - items marked with a $ are obsoleted by revision 6.0.
     - items marked with a & are introduced by Adobe DNG specification.
  }

  // Tag data type information.
  // Note: RATIONALs are the ratio of two 32-bit integer values.
  TIFF_NOTYPE                           = 0;
  TIFF_BYTE                             = 1;       { 8-bit unsigned integer }
  TIFF_ASCII                            = 2;       { 8-bit bytes w/ last byte null }
  TIFF_SHORT                            = 3;       { 16-bit unsigned integer }
  TIFF_LONG                             = 4;       { 32-bit unsigned integer }
  TIFF_RATIONAL                         = 5;       { 64-bit unsigned fraction }
  TIFF_SBYTE                            = 6;       { !8-bit signed integer }
  TIFF_UNDEFINED                        = 7;       { !8-bit untyped data }
  TIFF_SSHORT                           = 8;       { !16-bit signed integer }
  TIFF_SLONG                            = 9;       { !32-bit signed integer }
  TIFF_SRATIONAL                        = 10;      { !64-bit signed fraction }
  TIFF_FLOAT                            = 11;      { !32-bit IEEE floating point }
  TIFF_DOUBLE                           = 12;      { !64-bit IEEE floating point }
  TIFF_IFD                              = 13;      { %32-bit unsigned integer (offset) }
  TIFF_UNICODE                          = 14;      // Not defined in tiff 3.9.7 and 4.0.4
  TIFF_COMPLEX                          = 15;      // Not defined in tiff 3.9.7 and 4.0.4
  TIFF_LONG8                            = 16;      // BigTIFF 64-bit unsigned integer
  TIFF_SLONG8                           = 17;      // BigTIFF 64-bit signed integer
  TIFF_IFD8                             = 18;      // BigTIFF 64-bit unsigned integer (offset)

  // TIFF Tag Definitions.
  TIFFTAG_SUBFILETYPE                   = 254;     { subfile data descriptor }
    FILETYPE_REDUCEDIMAGE               = $1;      { reduced resolution version }
    FILETYPE_PAGE                       = $2;      { one page of many }
    FILETYPE_MASK                       = $4;      { transparency mask }
  TIFFTAG_OSUBFILETYPE                  = 255;     { +kind of data in subfile }
    OFILETYPE_IMAGE                     = 1;       { full resolution image data }
    OFILETYPE_REDUCEDIMAGE              = 2;       { reduced size image data }
    OFILETYPE_PAGE                      = 3;       { one page of many }
  TIFFTAG_IMAGEWIDTH                    = 256;     { image width in pixels }
  TIFFTAG_IMAGELENGTH                   = 257;     { image height in pixels }
  TIFFTAG_BITSPERSAMPLE                 = 258;     { bits per channel (sample) }
  TIFFTAG_COMPRESSION                   = 259;     { data compression technique }
    COMPRESSION_NONE                    = 1;       { dump mode }
    COMPRESSION_CCITTRLE                = 2;       { CCITT modified Huffman RLE }
    COMPRESSION_CCITTFAX3               = 3;       { CCITT Group 3 fax encoding }
    COMPRESSION_CCITT_T4                = 3;       { CCITT T.4 (TIFF 6 name) }
    COMPRESSION_CCITTFAX4               = 4;       { CCITT Group 4 fax encoding }
    COMPRESSION_CCITT_T6                = 4;       { CCITT T.6 (TIFF 6 name) }
    COMPRESSION_LZW                     = 5;       { Lempel-Ziv  & Welch }
    COMPRESSION_OJPEG                   = 6;       { !6.0 JPEG }
    COMPRESSION_JPEG                    = 7;       { %JPEG DCT compression }
    COMPRESSION_T85                     = 9;       { !TIFF/FX T.85 JBIG compression }
    COMPRESSION_T43                     = 10;      { !TIFF/FX T.43 colour by layered JBIG compression }
    COMPRESSION_NEXT                    = 32766;   { NeXT 2-bit RLE }
    COMPRESSION_CCITTRLEW               = 32771;   { #1 w/ word alignment }
    COMPRESSION_PACKBITS                = 32773;   { Macintosh RLE }
    COMPRESSION_THUNDERSCAN             = 32809;   { ThunderScan RLE }
    { codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
    COMPRESSION_IT8CTPAD                = 32895;   { IT8 CT w/padding }
    COMPRESSION_IT8LW                   = 32896;   { IT8 Linework RLE }
    COMPRESSION_IT8MP                   = 32897;   { IT8 Monochrome picture }
    COMPRESSION_IT8BL                   = 32898;   { IT8 Binary line art }
    { compression codes 32908-32911 are reserved for Pixar }
    COMPRESSION_PIXARFILM               = 32908;   { Pixar companded 10bit LZW }
    COMPRESSION_PIXARLOG                = 32909;   { Pixar companded 11bit ZIP }
    COMPRESSION_DEFLATE                 = 32946;   { Deflate compression }
    COMPRESSION_ADOBE_DEFLATE           = 8;       { Deflate compression, as recognized by Adobe }
    { compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> }
    COMPRESSION_DCS                     = 32947;   { Kodak DCS encoding }
    COMPRESSION_JBIG                    = 34661;   { ISO JBIG }
    COMPRESSION_SGILOG                  = 34676;   { SGI Log Luminance RLE }
    COMPRESSION_SGILOG24                = 34677;   { SGI Log 24-bit packed }

    // jb Not documented in tiff.h but encountered compression scheme(s):
    // jb Note that Lead Tools apparently has more proprietary TIFF formats for
    //    which we don't know the values but most likely around 34710.
    COMPRESSION_LEADTOOLS_CMP           = 34709;    { LeadTools Proprietary "FILE_TIF_CMP".}

    COMPRESSION_JP2000                  = 34712;   { Leadtools JPEG2000 }
    COMPRESSION_LZMA                    = 34925;   { LZMA2 }
  TIFFTAG_PHOTOMETRIC                   = 262;     { photometric interpretation }
    PHOTOMETRIC_MINISWHITE              = 0;       { min value is white }
    PHOTOMETRIC_MINISBLACK              = 1;       { min value is black }
    PHOTOMETRIC_RGB                     = 2;       { RGB color model }
    PHOTOMETRIC_PALETTE                 = 3;       { color map indexed }
    PHOTOMETRIC_MASK                    = 4;       { $holdout mask }
    PHOTOMETRIC_SEPARATED               = 5;       { !color separations }
    PHOTOMETRIC_YCBCR                   = 6;       { !CCIR 601 }
    PHOTOMETRIC_CIELAB                  = 8;       { !1976 CIE L*a*b* }
    PHOTOMETRIC_ICCLAB                  = 9;       { ICC L*a*b* [Adobe TIFF Technote 4] }
    PHOTOMETRIC_ITULAB                  = 10;      { ITU L*a*b* }
    PHOTOMETRIC_CFA                     = 32803;   { color filter array }
    PHOTOMETRIC_LOGL                    = 32844;   { CIE Log2(L) }
    PHOTOMETRIC_LOGLUV                  = 32845;   { CIE Log2(L) (u',v') }
  TIFFTAG_THRESHHOLDING                 = 263;     { thresholding used on data }
    THRESHHOLD_BILEVEL                  = 1;       { b&w art scan }
    THRESHHOLD_HALFTONE                 = 2;       { or dithered scan }
    THRESHHOLD_ERRORDIFFUSE             = 3;       { usually floyd-steinberg }
  TIFFTAG_CELLWIDTH                     = 264;     { +dithering matrix width }
  TIFFTAG_CELLLENGTH                    = 265;     { +dithering matrix height }
  TIFFTAG_FILLORDER                     = 266;     { data order within a byte }
    FILLORDER_MSB2LSB                   = 1;       { most significant -> least }
    FILLORDER_LSB2MSB                   = 2;       { least significant -> most }
  TIFFTAG_DOCUMENTNAME                  = 269;     { name of doc. image is from }
  TIFFTAG_IMAGEDESCRIPTION              = 270;     { info about image }
  TIFFTAG_MAKE                          = 271;     { scanner manufacturer name }
  TIFFTAG_MODEL                         = 272;     { scanner model name/number }
  TIFFTAG_STRIPOFFSETS                  = 273;     { offsets to data strips }
  TIFFTAG_ORIENTATION                   = 274;     { +image orientation }
    ORIENTATION_TOPLEFT                 = 1;       { row 0 top, col 0 lhs }
    ORIENTATION_TOPRIGHT                = 2;       { row 0 top, col 0 rhs }
    ORIENTATION_BOTRIGHT                = 3;       { row 0 bottom, col 0 rhs }
    ORIENTATION_BOTLEFT                 = 4;       { row 0 bottom, col 0 lhs }
    ORIENTATION_LEFTTOP                 = 5;       { row 0 lhs, col 0 top }
    ORIENTATION_RIGHTTOP                = 6;       { row 0 rhs, col 0 top }
    ORIENTATION_RIGHTBOT                = 7;       { row 0 rhs, col 0 bottom }
    ORIENTATION_LEFTBOT                 = 8;       { row 0 lhs, col 0 bottom }
  TIFFTAG_SAMPLESPERPIXEL               = 277;     { samples per pixel }
  TIFFTAG_ROWSPERSTRIP                  = 278;     { rows per strip of data }
  TIFFTAG_STRIPBYTECOUNTS               = 279;     { bytes counts for strips }
  TIFFTAG_MINSAMPLEVALUE                = 280;     { +minimum sample value }
  TIFFTAG_MAXSAMPLEVALUE                = 281;     { +maximum sample value }
  TIFFTAG_XRESOLUTION                   = 282;     { pixels/resolution in x }
  TIFFTAG_YRESOLUTION                   = 283;     { pixels/resolution in y }
  TIFFTAG_PLANARCONFIG                  = 284;     { storage organization }
    PLANARCONFIG_CONTIG                 = 1;       { single image plane }
    PLANARCONFIG_SEPARATE               = 2;       { separate planes of data }
  TIFFTAG_PAGENAME                      = 285;     { page name image is from }
  TIFFTAG_XPOSITION                     = 286;     { x page offset of image lhs }
  TIFFTAG_YPOSITION                     = 287;     { y page offset of image lhs }
  TIFFTAG_FREEOFFSETS                   = 288;     { +byte offset to free block }
  TIFFTAG_FREEBYTECOUNTS                = 289;     { +sizes of free blocks }
  TIFFTAG_GRAYRESPONSEUNIT              = 290;     { $gray scale curve accuracy }
    GRAYRESPONSEUNIT_10S                = 1;       { tenths of a unit }
    GRAYRESPONSEUNIT_100S               = 2;       { hundredths of a unit }
    GRAYRESPONSEUNIT_1000S              = 3;       { thousandths of a unit }
    GRAYRESPONSEUNIT_10000S             = 4;       { ten-thousandths of a unit }
    GRAYRESPONSEUNIT_100000S            = 5;       { hundred-thousandths }
  TIFFTAG_GRAYRESPONSECURVE             = 291;     { $gray scale response curve }
  TIFFTAG_GROUP3OPTIONS                 = 292;     { 32 flag bits }
  TIFFTAG_T4OPTIONS                     = 292;     { TIFF 6.0 proper name alias }
    GROUP3OPT_2DENCODING                = $1;      { 2-dimensional coding }
    GROUP3OPT_UNCOMPRESSED              = $2;      { data not compressed }
    GROUP3OPT_FILLBITS                  = $4;      { fill to byte boundary }
  TIFFTAG_GROUP4OPTIONS                 = 293;     { 32 flag bits }
  TIFFTAG_T6OPTIONS                     = 293;     { TIFF 6.0 proper name }
    GROUP4OPT_UNCOMPRESSED              = $2;      { data not compressed }
  TIFFTAG_RESOLUTIONUNIT                = 296;     { units of resolutions }
    RESUNIT_NONE                        = 1;       { no meaningful units }
    RESUNIT_INCH                        = 2;       { english }
    RESUNIT_CENTIMETER                  = 3;       { metric }
  TIFFTAG_PAGENUMBER                    = 297;     { page numbers of multi-page }
  TIFFTAG_COLORRESPONSEUNIT             = 300;     { $color curve accuracy }
    COLORRESPONSEUNIT_10S               = 1;       { tenths of a unit }
    COLORRESPONSEUNIT_100S              = 2;       { hundredths of a unit }
    COLORRESPONSEUNIT_1000S             = 3;       { thousandths of a unit }
    COLORRESPONSEUNIT_10000S            = 4;       { ten-thousandths of a unit }
    COLORRESPONSEUNIT_100000S           = 5;       { hundred-thousandths }
  TIFFTAG_TRANSFERFUNCTION              = 301;     { !colorimetry info }
  TIFFTAG_SOFTWARE                      = 305;     { name & release }
  TIFFTAG_DATETIME                      = 306;     { creation date and time }
  TIFFTAG_ARTIST                        = 315;     { creator of image }
  TIFFTAG_HOSTCOMPUTER                  = 316;     { machine where created }
  TIFFTAG_PREDICTOR                     = 317;     { prediction scheme w/ LZW }
    PREDICTOR_NONE                      = 1;       { no prediction scheme used }
    PREDICTOR_HORIZONTAL                = 2;       { horizontal differencing }
    PREDICTOR_FLOATINGPOINT             = 3;       { floating point predictor }
  TIFFTAG_WHITEPOINT                    = 318;     { image white point }
  TIFFTAG_PRIMARYCHROMATICITIES         = 319;     { !primary chromaticities }
  TIFFTAG_COLORMAP                      = 320;     { RGB map for pallette image }
  TIFFTAG_HALFTONEHINTS                 = 321;     { !highlight+shadow info }
  TIFFTAG_TILEWIDTH                     = 322;     { !rows/data tile }
  TIFFTAG_TILELENGTH                    = 323;     { !cols/data tile }
  TIFFTAG_TILEOFFSETS                   = 324;     { !offsets to data tiles }
  TIFFTAG_TILEBYTECOUNTS                = 325;     { !byte counts for tiles }
  TIFFTAG_BADFAXLINES                   = 326;     { lines w/ wrong pixel count }
  TIFFTAG_CLEANFAXDATA                  = 327;     { regenerated line info }
    CLEANFAXDATA_CLEAN                  = 0;       { no errors detected }
    CLEANFAXDATA_REGENERATED            = 1;       { receiver regenerated lines }
    CLEANFAXDATA_UNCLEAN                = 2;       { uncorrected errors exist }
  TIFFTAG_CONSECUTIVEBADFAXLINES        = 328;     { max consecutive bad lines }
  TIFFTAG_SUBIFD                        = 330;     { subimage descriptors }
  TIFFTAG_INKSET                        = 332;     { !inks in separated image }
    INKSET_CMYK                         = 1;       { !cyan-magenta-yellow-black color }
    INKSET_MULTIINK                     = 2;       { !multi-ink or hi-fi color }
  TIFFTAG_INKNAMES                      = 333;     { !ascii names of inks }
  TIFFTAG_NUMBEROFINKS                  = 334;     { !number of inks }
  TIFFTAG_DOTRANGE                      = 336;     { !0% and 100% dot codes }
  TIFFTAG_TARGETPRINTER                 = 337;     { !separation target }
  TIFFTAG_EXTRASAMPLES                  = 338;     { !info about extra samples }
    EXTRASAMPLE_UNSPECIFIED             = 0;       { !unspecified data }
    EXTRASAMPLE_ASSOCALPHA              = 1;       { !associated alpha data }
    EXTRASAMPLE_UNASSALPHA              = 2;       { !unassociated alpha data }
  TIFFTAG_SAMPLEFORMAT                  = 339;     { !data sample format }
    SAMPLEFORMAT_UINT                   = 1;       { !unsigned integer data }
    SAMPLEFORMAT_INT                    = 2;       { !signed integer data }
    SAMPLEFORMAT_IEEEFP                 = 3;       { !IEEE floating point data }
    SAMPLEFORMAT_VOID                   = 4;       { !untyped data }
    SAMPLEFORMAT_COMPLEXINT             = 5;       { !complex signed int }
    SAMPLEFORMAT_COMPLEXIEEEFP          = 6;       { !complex ieee floating }
  TIFFTAG_SMINSAMPLEVALUE               = 340;     { !variable MinSampleValue }
  TIFFTAG_SMAXSAMPLEVALUE               = 341;     { !variable MaxSampleValue }
  TIFFTAG_CLIPPATH                      = 343;     { %ClipPath [Adobe TIFF technote 2] }
  TIFFTAG_XCLIPPATHUNITS                = 344;     { %XClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_YCLIPPATHUNITS                = 345;     { %YClipPathUnits [Adobe TIFF technote 2] }
  TIFFTAG_INDEXED                       = 346;     { %Indexed [Adobe TIFF Technote 3] }
  TIFFTAG_JPEGTABLES                    = 347;     { %JPEG table stream }
  TIFFTAG_OPIPROXY                      = 351;     { %OPI Proxy [Adobe TIFF technote] }
  // Tags 400-435 are from the TIFF/FX spec.
  TIFFTAG_GLOBALPARAMETERSIFD           = 400;     { ! }
  TIFFTAG_PROFILETYPE                   = 401;     { ! }
    PROFILETYPE_UNSPECIFIED             = 0;       { ! }
    PROFILETYPE_G3_FAX                  = 1;       { ! }
  TIFFTAG_FAXPROFILE                    = 402;     { ! }
    FAXPROFILE_S                        = 1;       { !TIFF/FX FAX profile S }
    FAXPROFILE_F                        = 2;       { !TIFF/FX FAX profile F }
    FAXPROFILE_J                        = 3;       { !TIFF/FX FAX profile J }
    FAXPROFILE_C                        = 4;       { !TIFF/FX FAX profile C }
    FAXPROFILE_L                        = 5;       { !TIFF/FX FAX profile L }
    FAXPROFILE_M                        = 6;       { !TIFF/FX FAX profile LM }
  TIFFTAG_CODINGMETHODS                 = 403;     { !TIFF/FX coding methods }
    CODINGMETHODS_T4_1D                 = (1 shl 1); { !T.4 1D }
    CODINGMETHODS_T4_2D                 = (1 shl 2); { !T.4 2D }
    CODINGMETHODS_T6                    = (1 shl 3); { !T.6 }
    CODINGMETHODS_T85                   = (1 shl 4); { !T.85 JBIG }
    CODINGMETHODS_T42                   = (1 shl 5); { !T.42 JPEG }
    CODINGMETHODS_T43                   = (1 shl 6); { !T.43 colour by layered JBIG }
  TIFFTAG_VERSIONYEAR                   = 404;     { !TIFF/FX version year }
  TIFFTAG_MODENUMBER                    = 405;     { !TIFF/FX mode number }
  TIFFTAG_DECODE                        = 433;     { !TIFF/FX decode }
  TIFFTAG_IMAGEBASECOLOR                = 434;     { !TIFF/FX image base colour }
  TIFFTAG_T82OPTIONS                    = 435;     { !TIFF/FX T.82 options }
  { Tags 512-521 are obsoleted by Technical Note #2
    which specifies a revised JPEG-in-TIFF scheme. }
  TIFFTAG_JPEGPROC                      = 512;     { !JPEG processing algorithm }
    JPEGPROC_BASELINE                   = 1;       { !baseline sequential }
    JPEGPROC_LOSSLESS                   = 14;      { !Huffman coded lossless }
  TIFFTAG_JPEGIFOFFSET                  = 513;     { !pointer to SOI marker }
  TIFFTAG_JPEGIFBYTECOUNT               = 514;     { !JFIF stream length }
  TIFFTAG_JPEGRESTARTINTERVAL           = 515;     { !restart interval length }
  TIFFTAG_JPEGLOSSLESSPREDICTORS        = 517;     { !lossless proc predictor }
  TIFFTAG_JPEGPOINTTRANSFORM            = 518;     { !lossless point transform }
  TIFFTAG_JPEGQTABLES                   = 519;     { !Q matrice offsets }
  TIFFTAG_JPEGDCTABLES                  = 520;     { !DCT table offsets }
  TIFFTAG_JPEGACTABLES                  = 521;     { !AC coefficient offsets }
  TIFFTAG_YCBCRCOEFFICIENTS             = 529;     { !RGB -> YCbCr transform }
  TIFFTAG_YCBCRSUBSAMPLING              = 530;     { !YCbCr subsampling factors }
  TIFFTAG_YCBCRPOSITIONING              = 531;     { !subsample positioning }
    YCBCRPOSITION_CENTERED              = 1;       { !as in PostScript Level 2 }
    YCBCRPOSITION_COSITED               = 2;       { !as in CCIR 601-1 }
  TIFFTAG_REFERENCEBLACKWHITE           = 532;     { !colorimetry info }
  TIFFTAG_STRIPROWCOUNTS                = 559;     { !TIFF/FX strip row counts }
  TIFFTAG_XMLPACKET                     = 700;     { %XML packet [Adobe XMP technote 9-14-02] (dkelly@apago.com) }
  TIFFTAG_OPIIMAGEID                    = 32781;   { %OPI ImageID [Adobe TIFF technote] }
  { tags 32952-32956 are private tags registered to Island Graphics }
  TIFFTAG_REFPTS                        = 32953;   { image reference points }
  TIFFTAG_REGIONTACKPOINT               = 32954;   { region-xform tack point }
  TIFFTAG_REGIONWARPCORNERS             = 32955;   { warp quadrilateral }
  TIFFTAG_REGIONAFFINE                  = 32956;   { affine transformation mat }
  { tags 32995-32999 are private tags registered to SGI }
  TIFFTAG_MATTEING                      = 32995;   { $use ExtraSamples }
  TIFFTAG_DATATYPE                      = 32996;   { $use SampleFormat }
  TIFFTAG_IMAGEDEPTH                    = 32997;   { z depth of image }
  TIFFTAG_TILEDEPTH                     = 32998;   { z depth/data tile }
  { tags 33300-33309 are private tags registered to Pixar }
  { TIFFTAG_PIXAR_IMAGEFULLWIDTH and TIFFTAG_PIXAR_IMAGEFULLLENGTH are set when
    an image has been cropped out of a larger image.
    They reflect the size of the original uncropped image.
    The TIFFTAG_XPOSITION and TIFFTAG_YPOSITION can be used to determine the
    position of the smaller image in the larger one. }
  TIFFTAG_PIXAR_IMAGEFULLWIDTH          = 33300;   { full image size in x }
  TIFFTAG_PIXAR_IMAGEFULLLENGTH         = 33301;   { full image size in y }
  { Tags 33302-33306 are used to identify special image modes and data used by Pixar's texture formats. }
  TIFFTAG_PIXAR_TEXTUREFORMAT           = 33302;   { texture map format }
  TIFFTAG_PIXAR_WRAPMODES               = 33303;   { s & t wrap modes }
  TIFFTAG_PIXAR_FOVCOT                  = 33304;   { cotan(fov) for env. maps }
  TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN    = 33305;
  TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA    = 33306;
  { tag 33405 is a private tag registered to Eastman Kodak }
  TIFFTAG_WRITERSERIALNUMBER            = 33405;   { device serial number }
  TIFFTAG_CFAREPEATPATTERNDIM           = 33421;   { dimensions of CFA pattern }
  TIFFTAG_CFAPATTERN                    = 33422;   { color filter array pattern }
  { tag 33432 is listed in the 6.0 spec w/ unknown ownership }
  TIFFTAG_COPYRIGHT                     = 33432;   { copyright string }
  { IPTC TAG from RichTIFF specifications }
  TIFFTAG_RICHTIFFIPTC                  = 33723;
  { 34016-34029 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
  TIFFTAG_IT8SITE                       = 34016;   { site name }
  TIFFTAG_IT8COLORSEQUENCE              = 34017;   { color seq. [RGB,CMYK,etc] }
  TIFFTAG_IT8HEADER                     = 34018;   { DDES Header }
  TIFFTAG_IT8RASTERPADDING              = 34019;   { raster scanline padding }
  TIFFTAG_IT8BITSPERRUNLENGTH           = 34020;   { # of bits in short run }
  TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH   = 34021;   { # of bits in long run }
  TIFFTAG_IT8COLORTABLE                 = 34022;   { LW colortable }
  TIFFTAG_IT8IMAGECOLORINDICATOR        = 34023;   { BP/BL image color switch }
  TIFFTAG_IT8BKGCOLORINDICATOR          = 34024;   { BP/BL bg color switch }
  TIFFTAG_IT8IMAGECOLORVALUE            = 34025;   { BP/BL image color value }
  TIFFTAG_IT8BKGCOLORVALUE              = 34026;   { BP/BL bg color value }
  TIFFTAG_IT8PIXELINTENSITYRANGE        = 34027;   { MP pixel intensity value }
  TIFFTAG_IT8TRANSPARENCYINDICATOR      = 34028;   { HC transparency switch }
  TIFFTAG_IT8COLORCHARACTERIZATION      = 34029;   { color character. table }
  TIFFTAG_IT8HCUSAGE                    = 34030;   { HC usage indicator }
  TIFFTAG_IT8TRAPINDICATOR              = 34031;   { Trapping indicator (untrapped=0, trapped=1) }
  TIFFTAG_IT8CMYKEQUIVALENT             = 34032;   { CMYK color equivalents }
  { tags 34232-34236 are private tags registered to Texas Instruments }
  TIFFTAG_FRAMECOUNT                    = 34232;   { Sequence Frame Count }
  { tag 34377 is private tag registered to Adobe for PhotoShop }
  TIFFTAG_PHOTOSHOP                     = 34377;
  { tags 34665, 34853 and 40965 are documented in EXIF specification }
  TIFFTAG_EXIFIFD                       = 34665;   { Pointer to EXIF private directory }
  { tag 34750 is a private tag registered to Adobe? }
  TIFFTAG_ICCPROFILE                    = 34675;   { ICC profile data }
  // jb Next one from 4.0.3
  TIFFTAG_IMAGELAYER                    = 34732;   { !TIFF/FX image layer information }
  { tag 34750 is a private tag registered to Pixel Magic }
  TIFFTAG_JBIGOPTIONS                   = 34750;   { JBIG options }
  TIFFTAG_GPSIFD                        = 34853;   { Pointer to GPS private directory }
  { tags 34908-34914 are private tags registered to SGI }
  TIFFTAG_FAXRECVPARAMS                 = 34908;   { encoded Class 2 ses. parms }
  TIFFTAG_FAXSUBADDRESS                 = 34909;   { received SubAddr string }
  TIFFTAG_FAXRECVTIME                   = 34910;   { receive time (secs) }
  TIFFTAG_FAXDCS                        = 34911;   { encoded fax ses. params, Table 2/T.30 }
  { tags 37439-37443 are registered to SGI <gregl@sgi.com> }
  TIFFTAG_STONITS                       = 37439;   { Sample value to Nits }
  { tag 34929 is a private tag registered to FedEx }
  TIFFTAG_FEDEX_EDR                     = 34929;   { unknown use }
  TIFFTAG_INTEROPERABILITYIFD           = 40965;   { Pointer to Interoperability private directory }
  { Adobe Digital Negative (DNG) format tags }
  TIFFTAG_DNGVERSION                    = 50706;   { &DNG version number }
  TIFFTAG_DNGBACKWARDVERSION            = 50707;   { &DNG compatibility version }
  TIFFTAG_UNIQUECAMERAMODEL             = 50708;   { &name for the camera model }
  TIFFTAG_LOCALIZEDCAMERAMODEL          = 50709;   { &localized camera model name }
  TIFFTAG_CFAPLANECOLOR                 = 50710;   { &CFAPattern->LinearRaw space mapping }
  TIFFTAG_CFALAYOUT                     = 50711;   { &spatial layout of the CFA }
  TIFFTAG_LINEARIZATIONTABLE            = 50712;   { &lookup table description }
  TIFFTAG_BLACKLEVELREPEATDIM           = 50713;   { &repeat pattern size for the BlackLevel tag }
  TIFFTAG_BLACKLEVEL                    = 50714;   { &zero light encoding level }
  TIFFTAG_BLACKLEVELDELTAH              = 50715;   { &zero light encoding level differences (columns) }
  TIFFTAG_BLACKLEVELDELTAV              = 50716;   { &zero light encoding level differences (rows) }
  TIFFTAG_WHITELEVEL                    = 50717;   { &fully saturated encoding level }
  TIFFTAG_DEFAULTSCALE                  = 50718;   { &default scale factors }
  TIFFTAG_DEFAULTCROPORIGIN             = 50719;   { &origin of the final image area }
  TIFFTAG_DEFAULTCROPSIZE               = 50720;   { &size of the final image area }
  TIFFTAG_COLORMATRIX1                  = 50721;   { &XYZ->reference color space transformation matrix 1 }
  TIFFTAG_COLORMATRIX2                  = 50722;   { &XYZ->reference color space transformation matrix 2 }
  TIFFTAG_CAMERACALIBRATION1            = 50723;   { &calibration matrix 1 }
  TIFFTAG_CAMERACALIBRATION2            = 50724;   { &calibration matrix 2 }
  TIFFTAG_REDUCTIONMATRIX1              = 50725;   { &dimensionality reduction matrix 1 }
  TIFFTAG_REDUCTIONMATRIX2              = 50726;   { &dimensionality reduction matrix 2 }
  TIFFTAG_ANALOGBALANCE                 = 50727;   { &gain applied the stored raw values}
  TIFFTAG_ASSHOTNEUTRAL                 = 50728;   { &selected white balance in linear reference space }
  TIFFTAG_ASSHOTWHITEXY                 = 50729;   { &selected white balance in x-y chromaticity coordinates }
  TIFFTAG_BASELINEEXPOSURE              = 50730;   { &how much to move the zero point }
  TIFFTAG_BASELINENOISE                 = 50731;   { &relative noise level }
  TIFFTAG_BASELINESHARPNESS             = 50732;   { &relative amount of sharpening }
  TIFFTAG_BAYERGREENSPLIT               = 50733;   { &how closely the values of the green pixels in the
                                                     blue/green rows track the values of the green pixels
                                                     in the red/green rows }
  TIFFTAG_LINEARRESPONSELIMIT           = 50734;   { &non-linear encoding range }
  TIFFTAG_CAMERASERIALNUMBER            = 50735;   { &camera's serial number }
  TIFFTAG_LENSINFO                      = 50736;   { info about the lens }
  TIFFTAG_CHROMABLURRADIUS              = 50737;   { &chroma blur radius }
  TIFFTAG_ANTIALIASSTRENGTH             = 50738;   { &relative strength of the camera's anti-alias filter }
  TIFFTAG_SHADOWSCALE                   = 50739;   { &used by Adobe Camera Raw }
  TIFFTAG_DNGPRIVATEDATA                = 50740;   { &manufacturer's private data }
  TIFFTAG_MAKERNOTESAFETY               = 50741;   { &whether the EXIF MakerNote tag is safe to preserve
                                                     along with the rest of the EXIF data }
  TIFFTAG_CALIBRATIONILLUMINANT1        = 50778;   { &illuminant 1 }
  TIFFTAG_CALIBRATIONILLUMINANT2        = 50779;   { &illuminant 2 }
  TIFFTAG_BESTQUALITYSCALE              = 50780;   { &best quality multiplier }
  TIFFTAG_RAWDATAUNIQUEID               = 50781;   { &unique identifier for the raw image data }
  TIFFTAG_ORIGINALRAWFILENAME           = 50827;   { &file name of the original raw file }
  TIFFTAG_ORIGINALRAWFILEDATA           = 50828;   { &contents of the original raw file }
  TIFFTAG_ACTIVEAREA                    = 50829;   { &active (non-masked) pixels of the sensor }
  TIFFTAG_MASKEDAREAS                   = 50830;   { &list of coordinates of fully masked pixels }
  TIFFTAG_ASSHOTICCPROFILE              = 50831;   { &these two tags used to }
  TIFFTAG_ASSHOTPREPROFILEMATRIX        = 50832;   { map cameras's color space into ICC profile space }
  TIFFTAG_CURRENTICCPROFILE             = 50833;   { & }
  TIFFTAG_CURRENTPREPROFILEMATRIX       = 50834;   { & }
  { tag 65535 is an undefined tag used by Eastman Kodak }
  TIFFTAG_DCSHUESHIFTVALUES             = 65535;   { hue shift correction data }

  { The following are ``pseudo tags'' that can be used to control codec-specific functionality.
    These tags are not written to file.
    Note that these values start at 0xffff+1 so that they'll never collide with Aldus-assigned tags. }
  TIFFTAG_FAXMODE                       = 65536;   { Group 3/4 format control }
    FAXMODE_CLASSIC                     = $0;      { default, include RTC }
    FAXMODE_NORTC                       = $1;      { no RTC at end of data }
    FAXMODE_NOEOL                       = $2;      { no EOL code at end of row }
    FAXMODE_BYTEALIGN                   = $4;      { byte align row }
    FAXMODE_WORDALIGN                   = $8;      { word align row }
    FAXMODE_CLASSF                      = FAXMODE_NORTC;        { TIFF Class F }
  TIFFTAG_JPEGQUALITY                   = 65537;   { Compression quality level }
  { Note: quality level is on the IJG 0-100 scale.  Default value is 75 }
  TIFFTAG_JPEGCOLORMODE                 = 65538;   { Auto RGB<=>YCbCr convert? }
    JPEGCOLORMODE_RAW                   = $0;      { no conversion (default) }
    JPEGCOLORMODE_RGB                   = $1;      { do auto conversion }
  TIFFTAG_JPEGTABLESMODE                = 65539;   { What to put in JPEGTables }
    JPEGTABLESMODE_QUANT                = $1;      { include quantization tbls }
    JPEGTABLESMODE_HUFF                 = $2;      { include Huffman tbls }
  { Note: default is JPEGTABLESMODE_QUANT | JPEGTABLESMODE_HUFF }
  TIFFTAG_FAXFILLFUNC                   = 65540;   { G3/G4 fill function }
  TIFFTAG_PIXARLOGDATAFMT               = 65549;   { PixarLogCodec I/O data sz }
    PIXARLOGDATAFMT_8BIT                = 0;       { regular u_char samples }
    PIXARLOGDATAFMT_8BITABGR            = 1;       { ABGR-order u_chars }
    PIXARLOGDATAFMT_11BITLOG            = 2;       { 11-bit log-encoded (raw) }
    PIXARLOGDATAFMT_12BITPICIO          = 3;       { as per PICIO (1.0==2048) }
    PIXARLOGDATAFMT_16BIT               = 4;       { signed short samples }
    PIXARLOGDATAFMT_FLOAT               = 5;       { IEEE float samples }
  { 65550-65556 are allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSIMAGERTYPE                 = 65550;   { imager model & filter }
    DCSIMAGERMODEL_M3                   = 0;       { M3 chip (1280 x 1024) }
    DCSIMAGERMODEL_M5                   = 1;       { M5 chip (1536 x 1024) }
    DCSIMAGERMODEL_M6                   = 2;       { M6 chip (3072 x 2048) }
    DCSIMAGERFILTER_IR                  = 0;       { infrared filter }
    DCSIMAGERFILTER_MONO                = 1;       { monochrome filter }
    DCSIMAGERFILTER_CFA                 = 2;       { color filter array }
    DCSIMAGERFILTER_OTHER               = 3;       { other filter }
  TIFFTAG_DCSINTERPMODE                 = 65551;   { interpolation mode }
    DCSINTERPMODE_NORMAL                = 0;       { whole image, default }
    DCSINTERPMODE_PREVIEW               = 1;       { preview of image (384x256) }
  TIFFTAG_DCSBALANCEARRAY               = 65552;   { color balance values }
  TIFFTAG_DCSCORRECTMATRIX              = 65553;   { color correction values }
  TIFFTAG_DCSGAMMA                      = 65554;   { gamma value }
  TIFFTAG_DCSTOESHOULDERPTS             = 65555;   { toe & shoulder points }
  TIFFTAG_DCSCALIBRATIONFD              = 65556;   { calibration file desc }
  { Note: quality level is on the ZLIB 1-9 scale. Default value is -1 }
  TIFFTAG_ZIPQUALITY                    = 65557;   { compression quality level }
  TIFFTAG_PIXARLOGQUALITY               = 65558;   { PixarLog uses same scale }
  { 65559 is allocated to Oceana Matrix <dev@oceana.com> }
  TIFFTAG_DCSCLIPRECTANGLE              = 65559;   { area of image to acquire }
  TIFFTAG_SGILOGDATAFMT                 = 65560;   { SGILog user data format }
    SGILOGDATAFMT_FLOAT                 = 0;       { IEEE float samples }
    SGILOGDATAFMT_16BIT                 = 1;       { 16-bit samples }
    SGILOGDATAFMT_RAW                   = 2;       { uninterpreted data }
    SGILOGDATAFMT_8BIT                  = 3;       { 8-bit RGB monitor values }
  TIFFTAG_SGILOGENCODE                  = 65561;   { SGILog data encoding control }
    SGILOGENCODE_NODITHER               = 0;       { do not dither encoded values }
    SGILOGENCODE_RANDITHER              = 1;       { randomly dither encd values }
  TIFFTAG_LZMAPRESET                    = 65562;   { LZMA2 preset (compression level) }
  TIFFTAG_PERSAMPLE                     = 65563;   { interface for per sample tags }
    PERSAMPLE_MERGED                    = 0;           { present as a single value }
    PERSAMPLE_MULTI                     = 1;           { present as multiple values }

  { EXIF tags }
  EXIFTAG_EXPOSURETIME                  = 33434;   { Exposure time, given in seconds }
  EXIFTAG_FNUMBER                       = 33437;   { F number }
  EXIFTAG_EXPOSUREPROGRAM               = 34850;   { Exposure program }
  EXIFTAG_SPECTRALSENSITIVITY           = 34852;   { Spectral sensitivity of each channel of the camera used }
  EXIFTAG_ISOSPEEDRATINGS               = 34855;   { ISO Speed and ISO Latitude }
  EXIFTAG_OECF                          = 34856;   { Opto-Electric Conversion factor }
  EXIFTAG_EXIFVERSION                   = 36864;   { Exif version }
  EXIFTAG_DATETIMEORIGINAL              = 36867;   { Date and time of original data generation }
  EXIFTAG_DATETIMEDIGITIZED             = 36868;   { Date and time of digital data generation }
  EXIFTAG_COMPONENTSCONFIGURATION       = 37121;   { Meaning of each component }
  EXIFTAG_COMPRESSEDBITSPERPIXEL        = 37122;   { Image compression mode }
  EXIFTAG_SHUTTERSPEEDVALUE             = 37377;   { Shutter speed }
  EXIFTAG_APERTUREVALUE                 = 37378;   { Aperture }
  EXIFTAG_BRIGHTNESSVALUE               = 37379;   { Brightness }
  EXIFTAG_EXPOSUREBIASVALUE             = 37380;   { Exposure bias }
  EXIFTAG_MAXAPERTUREVALUE              = 37381;   { Maximum lens aperture }
  EXIFTAG_SUBJECTDISTANCE               = 37382;   { Subject distance in meters }
  EXIFTAG_METERINGMODE                  = 37383;   { Metering mode }
  EXIFTAG_LIGHTSOURCE                   = 37384;   { Light source }
  EXIFTAG_FLASH                         = 37385;   { Flash }
  EXIFTAG_FOCALLENGTH                   = 37386;   { Lens focal length }
  EXIFTAG_SUBJECTAREA                   = 37396;   { Subject area (in exif ver.2.2) }
  EXIFTAG_MAKERNOTE                     = 37500;   { Manufacturer notes }
  EXIFTAG_USERCOMMENT                   = 37510;   { User comments }
  EXIFTAG_SUBSECTIME                    = 37520;   { DateTime subseconds }
  EXIFTAG_SUBSECTIMEORIGINAL            = 37521;   { DateTimeOriginal subseconds }
  EXIFTAG_SUBSECTIMEDIGITIZED           = 37522;   { DateTimeDigitized subseconds }
  EXIFTAG_FLASHPIXVERSION               = 40960;   { Supported FlashPix version }
  EXIFTAG_COLORSPACE                    = 40961;   { Color space information }
  EXIFTAG_PIXELXDIMENSION               = 40962;   { Valid image width }
  EXIFTAG_PIXELYDIMENSION               = 40963;   { Valid image height }
  EXIFTAG_RELATEDSOUNDFILE              = 40964;   { Related audio file }
  EXIFTAG_FLASHENERGY                   = 41483;   { Flash energy }
  EXIFTAG_SPATIALFREQUENCYRESPONSE      = 41484;   { Spatial frequency response }
  EXIFTAG_FOCALPLANEXRESOLUTION         = 41486;   { Focal plane X resolution }
  EXIFTAG_FOCALPLANEYRESOLUTION         = 41487;   { Focal plane Y resolution }
  EXIFTAG_FOCALPLANERESOLUTIONUNIT      = 41488;   { Focal plane resolution unit }
  EXIFTAG_SUBJECTLOCATION               = 41492;   { Subject location }
  EXIFTAG_EXPOSUREINDEX                 = 41493;   { Exposure index }
  EXIFTAG_SENSINGMETHOD                 = 41495;   { Sensing method }
  EXIFTAG_FILESOURCE                    = 41728;   { File source }
  EXIFTAG_SCENETYPE                     = 41729;   { Scene type }
  EXIFTAG_CFAPATTERN                    = 41730;   { CFA pattern }
  EXIFTAG_CUSTOMRENDERED                = 41985;   { Custom image processing (in exif ver.2.2) }
  EXIFTAG_EXPOSUREMODE                  = 41986;   { Exposure mode (in exif ver.2.2) }
  EXIFTAG_WHITEBALANCE                  = 41987;   { White balance (in exif ver.2.2) }
  EXIFTAG_DIGITALZOOMRATIO              = 41988;   { Digital zoom ratio (in exif ver.2.2) }
  EXIFTAG_FOCALLENGTHIN35MMFILM         = 41989;   { Focal length in 35mm film camera, in mm }
  EXIFTAG_SCENECAPTURETYPE              = 41990;   { Scene capture type (in exif ver.2.2) }
  EXIFTAG_GAINCONTROL                   = 41991;   { Gain control (in exif ver.2.2) }
  EXIFTAG_CONTRAST                      = 41992;   { Contrast (in exif ver.2.2) }
  EXIFTAG_SATURATION                    = 41993;   { Saturation (in exif ver.2.2) }
  EXIFTAG_SHARPNESS                     = 41994;   { Sharpness (in exif ver.2.2) }
  EXIFTAG_DEVICESETTINGDESCRIPTION      = 41995;   { Device settings description (in exif ver.2.2) }
  EXIFTAG_SUBJECTDISTANCERANGE          = 41996;   { Subject distance range (in exif ver.2.2) }
  EXIFTAG_IMAGEUNIQUEID                 = 42016;   { Unique image ID (in exif ver.2.2) }

  // End of tiff.h
  // Start of tiffio.h

type
  PTIFF = Pointer;

  {
   * The following typedefs define the intrinsic size of
   * data types used in the *exported* interfaces.  These
   * definitions depend on the proper definition of types
   * in tiff.h.  Note also that the varargs interface used
   * to pass tag types and values uses the types defined in
   * tiff.h directly.
   *
   * NB: ttag_t is unsigned int and not unsigned short because
   *     ANSI C requires that the type before the ellipsis be a
   *     promoted type (i.e. one of int, unsigned int, pointer,
   *     or double) and because we defined pseudo-tags that are
   *     outside the range of legal Aldus-assigned tags.
   * NB: tsize_t is int32 and not uint32 because some functions
   *     return -1.
   * NB: toff_t is not off_t for many reasons; TIFFs max out at
   *     32-bit file offsets, and BigTIFF maxes out at 64-bit
   *     offsets being the most important, and to ensure use of
   *     a consistently unsigned type across architectures.
   *     Prior to libtiff 4.0, this was an unsigned 32 bit type.
   *}
  {
    this is the machine addressing size type, only it's signed, so make it
    int32 on 32bit machines, int64 on 64bit machines
  }
  tmsize_t = NativeInt;
  {$IFDEF LIBTIFF4}
  toff_t   = uint64;
  {$ELSE} // LIBTIFF 3.x
  toff_t   = uint32;
  {$ENDIF}
  ptoff_t = ^toff_t;
  thandle_t = THANDLE;

const
  { Flags to pass to TIFFPrintDirectory to control printing of data structures that are potentially very large. Bit-or these flags to
    enable printing multiple items. }
  TIFFPRINT_NONE                        = $0;      { no extra info }
  TIFFPRINT_STRIPS                      = $1;      { strips/tiles info }
  TIFFPRINT_CURVES                      = $2;      { color/gray response curves }
  TIFFPRINT_COLORMAP                    = $4;      { colormap }
  TIFFPRINT_JPEGQTABLES                 = $100;    { JPEG Q matrices }
  TIFFPRINT_JPEGACTABLES                = $200;    { JPEG AC tables }
  TIFFPRINT_JPEGDCTABLES                = $200;    { JPEG DC tables }

  // Colour conversion stuff

  // reference white
  D65_X0 = 95.0470;
  D65_Y0 = 100.0;
  D65_Z0 = 108.8827;

  D50_X0 = 96.4250;
  D50_Y0 = 100.0;
  D50_Z0 = 82.4680;

type
  // Structure for holding information about a display device.
  TIFFRGBValue = Byte;                 { 8-bit samples }
  PTIFFRGBValue = ^TIFFRGBValue;

  TIFFDisplay = record
      d_mat: array [0..2, 0..2] of float;  { XYZ -> luminance matrix }
      d_YCR: float;                        { Light o/p for reference white }
      d_YCG: float;
      d_YCB: float;
      d_Vrwr: uint32;                      { Pixel values for ref. white }
      d_Vrwg: uint32;
      d_Vrwb: uint32;
      d_Y0R: float;                        { Residual light for black pixel }
      d_Y0G: float;
      d_Y0B: float;
      d_gammaR: float;                     { Gamma values for the three guns }
      d_gammaG: float;
      d_gammaB: float;
  end;
  PTIFFDisplay = ^TIFFDisplay;

  // YCbCr->RGB support
  RecTIFFYCbCrToRGB = record
    clamptab: PTIFFRGBValue;               { range clamping table }
    Cr_r_tab: PInteger;
    Cb_b_tab: PInteger;
    Cr_g_tab: pint32;
    Cb_g_tab: pint32;
    Y_tab: pint32;
  end;
  PTIFFYCbCrToRGB = ^RecTIFFYCbCrToRGB;

const
  CIELABTORGB_TABLE_RANGE = 1500;

type
  // CIE Lab 1976->RGB support
  RecTIFFCIELabToRGB = record
    range: Integer;                          { Size of conversion table }
    rstep, gstep, bstep: float;
    X0, Y0, Z0: float;                       { Reference white point }
    display: TIFFDisplay;
    Yr2r: array [0..CIELABTORGB_TABLE_RANGE] of float;  { Conversion of Yr to r }
    Yg2g: array [0..CIELABTORGB_TABLE_RANGE] of float;  { Conversion of Yg to g }
    Yb2b: array [0..CIELABTORGB_TABLE_RANGE] of float;  { Conversion of Yb to b }
  end;
  PTIFFCIELabToRGB = ^RecTIFFCIELabToRGB;

  // RGBA-style image support.
  PTIFFRGBAImage = ^_TIFFRGBAImage;  //Pointer;

{*
 * The image reading and conversion routines invoke
 * ``put routines'' to copy/image/whatever tiles of
 * raw image data.  A default set of routines are
 * provided to convert/copy raw image data to 8-bit
 * packed ABGR format rasters.  Applications can supply
 * alternate routines that unpack the data into a
 * different format or, for example, unpack the data
 * and draw the unpacked raster on the display.
 *}

  tileContigRoutine = procedure(img: PTIFFRGBAImage; cp: puint32;
    x, y, w, h, fromskew, toskew: uint32; pp: PByte);
  tileSeparateRoutine = procedure(img: PTIFFRGBAImage; cp: puint32;
    x, y, w, h, fromskew, toskew: uint32; r, g, b, a: PByte);
  getRoutine = function(img: PTIFFRGBAImage; raster: puint32; w, h: uint32): PInteger;

  put_rec = record
    case boolean of
      False: (contig: tileContigRoutine);
      True: (separate: tileSeparateRoutine);
  end;

  _TIFFRGBAImage = record
        tif: PTIFF;                             // image handle
        stoponerr: Integer;                     // stop on read error
        isContig: Integer;                      // data is packed/separate
        alpha: Integer;                         // type of alpha data present
        width: uint32;                          // image width
        height: uint32;                         // image height
        bitspersample: uint16;                  // image bits/sample
        samplesperpixel: uint16;                // image samples/pixel
        orientation: uint16;                    // image orientation
        req_orientation: uint16;                // requested orientation
        photometric: uint16;                    // image photometric interp
        redcmap: puint16;                       // colormap pallete
        greencmap: puint16;
        bluecmap: puint16;
        {* get image data routine *}
        get: getRoutine;
        {* put decoded strip/tile *}
        put: put_rec;
        Map: PTIFFRGBValue;                     // sample mapping array
        BWmap: ppuint32;                        // black&white map
        PALmap: ppuint32;                       // palette image map
        ycbcr: PTIFFYCbCrToRGB;                 // YCbCr conversion state
        cielab: PTIFFCIELabToRGB;               // CIE L*a*b conversion state

        UaToAa: puint8;                         // Unassociated alpha to associated alpha convertion LUT
        Bitdepth16To8: puint8;                  // LUT for conversion from 16bit to 8bit values

        row_offset: Integer;
        col_offset: Integer;
  end;

  {
   * A CODEC is a software package that implements decoding,
   * encoding, or decoding+encoding of a compression algorithm.
   * The library provides a collection of builtin codecs.
   * More codecs may be registered through calls to the library
   * and/or the builtin implementations may be overridden.
  }
  TIFFInitMethod = function(Handle: PTIFF; Scheme: Integer): Integer; cdecl;

  PTIFFCodec = ^TIFFCodec;
  TIFFCodec = record
    name: PAnsiChar;
    scheme: Word;
    init: TIFFInitMethod;
  end;

  {* share internal LogLuv conversion routines? *}
  {$IFNDEF LOGLUV_PUBLIC}
    {$DEFINE LOGLUV_PUBLIC 1}
  {$ENDIF}

  TIFFErrorHandler = procedure(const Module: PAnsiChar; const Fmt: PAnsiChar; Args: Pointer); cdecl;
  TIFFErrorHandlerExt = procedure(Fd: thandle_t; const Module: PAnsiChar; const Fmt: PAnsiChar; Args: Pointer); cdecl;
  LibTiffDelphiErrorHandler = procedure(const Module, Error: AnsiString);
  TIFFReadWriteProc = function(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
  TIFFSeekProc = function(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl;
  TIFFCloseProc = function(Fd: thandle_t): Integer; cdecl;
  TIFFSizeProc = function(Fd: thandle_t): toff_t; cdecl;
  TIFFMapFileProc = function(Fd: thandle_t; PBase: PPointer; PSize: ptoff_t): Integer; cdecl;
  TIFFUnmapFileProc = procedure(Fd: thandle_t; Base: Pointer; Size: toff_t); cdecl;
  TIFFExtendProc = procedure(Handle: PTIFF); cdecl;

function  TIFFGetVersion: PAnsiChar; cdecl; external;
function  LibTiffDelphiVersion: AnsiString;
function  LibTiffDelphiGetVersionNumber: AnsiString;

// Codec handling.
function  TIFFFindCODEC(Scheme: uint16): PTIFFCodec; cdecl; external;
function  TIFFRegisterCODEC(Scheme: uint16; Name: PAnsiChar; InitMethod: TIFFInitMethod): PTIFFCodec; cdecl; external;
procedure TIFFUnRegisterCODEC(c: PTIFFCodec); cdecl; external;
function  TIFFIsCODECConfigured(Scheme: uint16): Integer; cdecl; external;
function  TIFFGetConfiguredCODECs: PTIFFCodec; cdecl; external;

// Stuff, related to tag handling and creating custom tags.
// See also: TIFFFieldInfo, TIFFTagValue and TIFFGetTagListEntry
function  TIFFGetTagListCount(Handle: PTIFF): Integer; cdecl; external;
function  TIFFGetTagListEntry(Handle: PTIFF; TagIndex: Integer): uint32; cdecl; external;

const
  TIFF_ANY       = TIFF_NOTYPE; { for field descriptor searching }
  TIFF_VARIABLE  = -1;          { marker for variable length tags }
  TIFF_SPP       = -2;          { marker for SamplesPerPixel tags }
  TIFF_VARIABLE2 = -3;          { marker for uint32 var-length tags }

  FIELD_CUSTOM   = 65;

type
  {$IFNDEF LIBTIFF4}

  // Stuff, related to tag handling and creating custom tags.
  PTIFFFieldInfo = ^TIFFFieldInfo;
  TIFFFieldInfo = record
    FieldTag: Cardinal;              { field's tag }
    FieldReadCount: Smallint;        { read count/TIFF_VARIABLE/TIFF_SPP }
    FieldWriteCount: Smallint;       { write count/TIFF_VARIABLE }
    FieldType: Integer;              { type of associated data }
    FieldBit: Word;                  { bit in fieldsset bit vector }
    FieldOkToChange: Byte;           { if true, can change while writing }
    FieldPassCount: Byte;            { if true, pass dir count on set }
    FieldName: PAnsiChar;            { ASCII name }
  end;

  PTIFFTagValue = ^TIFFTagValue;
  TIFFTagValue = record
    Info: PTIFFFieldInfo;
    Count: Integer;
    Value: Pointer;
  end;

function  TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFindFieldInfoByName(Handle: PTIFF; FieldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external;

{$ELSE} // LIBTIFF 4

  PTIFFField = Pointer;
  PTIFFFieldArray = Pointer;

function  TIFFFindField(Handle: PTIFF; Tag: uint32; DataType: uint32): PTIFFField; cdecl; external;
function  TIFFFieldWithTag(Handle: PTIFF; Tag: uint32): PTIFFField; cdecl; external;
function  TIFFFieldWithName(Handle: PTIFF; const FieldName: PAnsiChar): PTIFFField; cdecl; external;

function  TIFFFieldTag(const Fip: PTIFFField): uint32; cdecl; external;
function  TIFFFieldName(const Fip: PTIFFField): PAnsiChar; cdecl; external;
function  TIFFFieldDataType(const Fip: PTIFFField): uint32; cdecl; external;
function  TIFFFieldPassCount(const Fip: PTIFFField): Integer; cdecl; external;
function  TIFFFieldReadCount(const Fip: PTIFFField): Integer; cdecl; external;
function  TIFFFieldWriteCount(const Fip: PTIFFField): Integer; cdecl; external;

{$ENDIF ~LIBTIFF4}

type
  TIFFVSetMethod = function(Handle: PTIFF; Tag: uint32; args: Pointer): Integer; cdecl;
  TIFFVGetMethod = function(Handle: PTIFF; Tag: uint32; args: Pointer): Integer; cdecl;
  TIFFPrintMethod = procedure(Handle: PTIFF; FileHandle: Pointer; Value: LongWord); cdecl;

  PTIFFTagMethods = ^TIFFTagMethods;
  TIFFTagMethods = record
    vsetfield: TIFFVSetMethod;  { tag set routine }
    vgetfield: TIFFVGetMethod;  { tag get routine }
    printdir:  TIFFPrintMethod; { directory print routine }
  end;

function  TIFFAccessTagMethods(Handle: PTIFF): PTIFFTagMethods; cdecl; external;
function  TIFFGetClientInfo(Handle: PTIFF; const Name : PAnsiChar): Pointer; cdecl; external;
procedure TIFFSetClientInfo(Handle: PTIFF; Data: Pointer; const Name : PAnsiChar); cdecl; external;

procedure TIFFCleanup(Handle: PTIFF); cdecl; external;
procedure TIFFClose(Handle: PTIFF); cdecl; external;
function  TIFFFlush(Handle: PTIFF): Integer; cdecl; external;
function  TIFFFlushData(Handle: PTIFF): Integer; cdecl; external;
function  TIFFGetField(Handle: PTIFF; Tag: uint32): Integer; cdecl; external; varargs;
function  TIFFVGetField(Handle: PTIFF; Tag: uint32; Ap: Pointer): Integer; cdecl; external;
function  TIFFGetFieldDefaulted(Handle: PTIFF; Tag: uint32): Integer; cdecl; external; varargs;
function  TIFFVGetFieldDefaulted(tif: Pointer; Tag: uint32; Ap: Pointer): Integer; cdecl; external;

function  TIFFReadDirectory(Handle: PTIFF): Integer; cdecl; external;
{$IFNDEF LIBTIFF4}
function  TIFFReadCustomDirectory(Handle: PTIFF; DirOff: toff_t; Info: PTIFFFieldInfo; N: Integer): Integer; cdecl; external;
{$ELSE} // LIBTIFF 4
function  TIFFReadCustomDirectory(Handle: PTIFF; DirOff: toff_t; InfoArray: PTIFFFieldArray): Integer; cdecl; external;
{$ENDIF ~LIBTIFF4}
function  TIFFReadEXIFDirectory(Handle: PTIFF; DirOff: toff_t): Integer; cdecl; external;

function  TIFFScanlineSize(Handle: PTIFF): tmsize_t; cdecl; external;
function  TIFFRasterScanlineSize(Handle: PTIFF): tmsize_t; cdecl; external;
function  TIFFStripSize(Handle: PTIFF): tmsize_t; cdecl; external;
function  TIFFRawStripSize(Handle: PTIFF; Strip: uint32): tmsize_t; cdecl; external;
function  TIFFVStripSize(Handle: PTIFF; NRows: uint32): tmsize_t; cdecl; external;
function  TIFFTileRowSize(Handle: PTIFF): tmsize_t; cdecl; external;
function  TIFFTileSize(Handle: PTIFF): tmsize_t; cdecl; external;
function  TIFFVTileSize(Handle: PTIFF; NRows: uint32): tmsize_t; cdecl; external;

{$IFDEF LIBTIFF4}
function  TIFFScanlineSize64(Handle: PTIFF): uint64; cdecl; external;
function  TIFFRasterScanlineSize64(Handle: PTIFF): uint64; cdecl; external;
function  TIFFStripSize64(Handle: PTIFF): uint64; cdecl; external;
function  TIFFRawStripSize64(Handle: PTIFF; Strip: uint32): uint64; cdecl; external;
function  TIFFVStripSize64(Handle: PTIFF; NRows: uint32): uint64; cdecl; external;
function  TIFFTileRowSize64(Handle: PTIFF): uint64; cdecl; external;
function  TIFFTileSize64(Handle: PTIFF): uint64; cdecl; external;
function  TIFFVTileSize64(Handle: PTIFF; NRows: uint32): uint64; cdecl; external;
{$ENDIF LIBTIFF4}

function  TIFFDefaultStripSize(Handle: PTIFF; Request: uint32): uint32; cdecl; external;
procedure TIFFDefaultTileSize(Handle: PTIFF; Tw: puint32; Th: puint32); cdecl; external;
function  TIFFFileno(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetFileno(Handle: PTIFF; Newvalue: Integer): Integer; cdecl; external;
function  TIFFClientdata(Handle: PTIFF): thandle_t; cdecl; external;
function  TIFFSetClientdata(Handle: PTIFF; Newvalue: thandle_t): thandle_t; cdecl; external;
function  TIFFGetMode(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetMode(Handle: PTIFF; Mode: Integer): Integer; cdecl; external;
function  TIFFIsTiled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsByteSwapped(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsUpSampled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsMSB2LSB(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsBigEndian(Handle: PTIFF): Integer; cdecl; external;
function  TIFFGetReadProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetWriteProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetSeekProc(Handle: PTIFF): TIFFSeekProc; cdecl; external;
function  TIFFGetCloseProc(Handle: PTIFF): TIFFCloseProc; cdecl; external;
function  TIFFGetSizeProc(Handle: PTIFF): TIFFSizeProc; cdecl; external;
function  TIFFGetMapFileProc(Handle: PTIFF): TIFFMapFileProc; cdecl; external;
function  TIFFGetUnmapFileProc(Handle: PTIFF): TIFFUnmapFileProc; cdecl; external;
function  TIFFCurrentRow(Handle: PTIFF): uint32; cdecl; external;
function  TIFFCurrentDirectory(Handle: PTIFF): uint16; cdecl; external;
function  TIFFNumberOfDirectories(Handle: PTIFF): uint16; cdecl; external;
{$IFNDEF LIBTIFF4}
function  TIFFCurrentDirOffset(Handle: PTIFF): uint32; cdecl; external;
{$ELSE}
function  TIFFCurrentDirOffset(Handle: PTIFF): uint64; cdecl; external;
{$ENDIF ~LIBTIFF4}
function  TIFFCurrentStrip(Handle: PTIFF): uint32; cdecl; external;
function  TIFFCurrentTile(Handle: PTIFF): uint32; cdecl; external;
function  TIFFReadBufferSetup(Handle: PTIFF; Buf: Pointer; Size: tmsize_t): Integer; cdecl; external;
function  TIFFWriteBufferSetup(Handle: PTIFF; Buf: Pointer; Size: tmsize_t): Integer; cdecl; external;
function  TIFFSetupStrips(Handle: PTIFF): Integer; cdecl; external;
function  TIFFWriteCheck(Handle: PTIFF; Tiles: Integer; const Module: PAnsiChar): Integer; cdecl; external;
procedure TIFFFreeDirectory(Handle: PTIFF); cdecl; external;
function  TIFFCreateDirectory(Handle: PTIFF): Integer; cdecl; external;
{$IFDEF LIBTIFF4}
function  TIFFCreateCustomDirectory(Handle: PTIFF; const InfoArray: PTIFFFieldArray): Integer; cdecl; external;
function  TIFFCreateEXIFDirectory(Handle: PTIFF): Integer; cdecl; external;
{$ENDIF LIBTIFF4}
function  TIFFLastDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetDirectory(Handle: PTIFF; Dirn: uint16): Integer; cdecl; external;
{$IFNDEF LIBTIFF4}
function  TIFFSetSubDirectory(Handle: PTIFF; DirOff: uint32): Integer; cdecl; external;
{$ELSE}
function  TIFFSetSubDirectory(Handle: PTIFF; DirOff: uint64): Integer; cdecl; external;
{$ENDIF ~LIBTIFF4}
function  TIFFUnlinkDirectory(handle: PTIFF; Dirn: uint16): Integer; cdecl; external;
function  TIFFSetField(Handle: PTIFF; Tag: uint32): Integer; cdecl; external; varargs;
function  TIFFVSetField(Handle: PTIFF; Tag: uint32; Ap: Pointer): Integer; cdecl; external;
{$IFDEF LIBTIFF4}
function  TIFFUnsetField(Handle: PTIFF; Tag: uint32): Integer; cdecl; external;
function  TIFFWriteCustomDirectory(Handle: PTIFF; PDirOff: puint64): Integer; cdecl; external;
{$ENDIF LIBTIFF4}
function  TIFFWriteDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFCheckpointDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFRewriteDirectory(Handle: PTIFF): Integer; cdecl; external;

procedure TIFFPrintDirectory(Handle: PTIFF; Fd: Pointer; Flags: Integer); cdecl; external;
function  TIFFReadScanline(Handle: PTIFF; Buf: Pointer; Row: uint32; Sample: uint16): Integer; cdecl; external;
function  TIFFWriteScanline(Handle: PTIFF; Buf: Pointer; Row: uint32; Sample: uint16): Integer; cdecl; external;
function  TIFFReadRGBAImage(Handle: PTIFF; RWidth, RHeight: uint32; Raster: puint32; Stop: Integer): Integer; cdecl; external;
function  TIFFReadRGBAImageOriented(Handle: PTIFF; RWidth, RHeight: uint32; Raster: puint32; Orientation: Integer; Stop: Integer): Integer; cdecl; external;

function  TIFFReadRGBAStrip(Handle: PTIFF; Row: uint32; Raster: puint32): Integer; cdecl; external;
function  TIFFReadRGBATile(Handle: PTIFF; Col, Row: uint32; Raster: puint32): Integer; cdecl; external;
// jb is the following correct (twice)? Original param Emsg in c is "char [1024]"
function  TIFFRGBAImageOk(Handle: PTIFF; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageBegin(Img: PTIFFRGBAImage; Handle: PTIFF; Stop: Integer; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageGet(Img: PTIFFRGBAImage; Raster: puint32; W, H: uint32): Integer; cdecl; external;
procedure TIFFRGBAImageEnd(Img: PTIFFRGBAImage); cdecl; external;

{$IFDEF MSWINDOWS}
function  TIFFOpen(const Name: AnsiString; const Mode: AnsiString): PTIFF;
{$IFNDEF LIBTIFF4}
// TODO implement Wide version of open
//function  TIFFOpenW(const Name: WideString; const Mode: AnsiString): PTIFF;
{$ENDIF ~LIBTIFF4}
function  TIFFOpenStream(const Stream: TStream; const Mode: AnsiString): PTIFF;
{$ENDIF}

function  TIFFClientOpen(const Name: PAnsiChar; const Mode: PAnsiChar; ClientData: thandle_t;
  ReadProc: TIFFReadWriteProc; WriteProc: TIFFReadWriteProc; SeekProc: TIFFSeekProc;
  CloseProc: TIFFCloseProc; SizeProc: TIFFSizeProc;
  MapProc: TIFFMapFileProc; UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl; external;

function  TIFFFileName(Handle: PTIFF): PAnsiChar; cdecl; external;
function  TIFFSetFileName(Handle: PTIFF; Name: PAnsiChar): PAnsiChar; cdecl; external;
procedure TIFFError(const Module: PAnsiChar; const Fmt: PAnsiChar); cdecl; external; varargs;
procedure TIFFErrorExt(Fd: Pointer; const Module: PAnsiChar; const Fmt: PAnsiChar); cdecl; external; varargs;
procedure TIFFWarning(const Module: PAnsiChar; const Fmt: PAnsiChar); cdecl; external; varargs;
procedure TIFFWarningExt(Fd: Pointer; const Module: PAnsiChar; const Fmt: PAnsiChar); cdecl; external; varargs;
function  TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  TIFFSetErrorHandlerExt(Handler: TIFFErrorHandlerExt): TIFFErrorHandlerExt; cdecl; external;
function  TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  TIFFSetWarningHandlerExt(Handler: TIFFErrorHandlerExt): TIFFErrorHandlerExt; cdecl; external;

function  LibTiffDelphiGetErrorHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetErrorHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
function  LibTiffDelphiGetWarningHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetWarningHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;

function  TIFFSetTagExtender(Extender: TIFFExtendProc): TIFFExtendProc; cdecl; external;
function  TIFFComputeTile(Handle: PTIFF; x, y, z: uint32; S: uint16): uint32; cdecl; external;
function  TIFFCheckTile(Handle: PTIFF; x, y, z: uint32; s: uint16): Integer; cdecl; external;
function  TIFFNumberOfTiles(Handle: PTIFF): uint32; cdecl; external;
function  TIFFReadTile(Handle: PTIFF; Buf: Pointer; x, y, z: uint32; s: uint16): tmsize_t; cdecl; external;
function  TIFFWriteTile(Handle: PTIFF; Buf: Pointer; x, y, z: uint32; s: uint16): tmsize_t; cdecl; external;
function  TIFFComputeStrip(Handle: PTIFF; Row: uint32; Sample: uint16): uint32; cdecl; external;
function  TIFFNumberOfStrips(Handle: PTIFF): uint32; cdecl; external;
function  TIFFReadEncodedStrip(Handle: PTIFF; Strip: uint32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external;
function  TIFFReadRawStrip(Handle: PTIFF; Strip: uint32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external;
function  TIFFReadEncodedTile(Handle: PTIFF; Tile: uint32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external;
function  TIFFReadRawTile(Handle: PTIFF; Tile: uint32; Buf: Pointer; Size: tmsize_t): tmsize_t; cdecl; external;
function  TIFFWriteEncodedStrip(Handle: PTIFF; Strip: uint32; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external;
function  TIFFWriteRawStrip(Handle: PTIFF; Strip: uint32; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external;
function  TIFFWriteEncodedTile(Handle: PTIFF; Tile: uint32; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external;
function  TIFFWriteRawTile(Handle: PTIFF; Tile: uint32; Data: Pointer; Cc: tmsize_t): tmsize_t; cdecl; external;
function  TIFFDataWidth(DataType: uint32): Integer; cdecl; external;  // table of tag datatype widths
procedure TIFFSetWriteOffset(Handle: PTIFF; Off: toff_t); cdecl; external;

procedure TIFFSwabShort(Wp: puint16); cdecl; external;
procedure TIFFSwabLong(Lp: puint32); cdecl; external;
procedure TIFFSwabDouble(Dp: PDouble); cdecl; external;
procedure TIFFSwabArrayOfShort(Wp: puint16; N: tmsize_t); cdecl; external;
procedure TIFFSwabArrayOfTriples(Tp: puint8; N: tmsize_t); cdecl; external;
procedure TIFFSwabArrayOfLong(Lp: puint32; N: tmsize_t); cdecl; external;
procedure TIFFSwabArrayOfDouble(Dp: PDouble; N: tmsize_t); cdecl; external;
procedure TIFFReverseBits(Cp: puint8; N: tmsize_t); cdecl; external;
function  TIFFGetBitRevTable(Reversed: Integer): Pointer; cdecl; external;

{$IFDEF LIBTIFF4}
procedure TIFFSwabLong8(Lp: puint64); cdecl; external;
procedure TIFFSwabFloat(Fp: pfloat); cdecl; external;
procedure TIFFSwabArrayOfLong8(Lp: puint64; N: tmsize_t); cdecl; external;
procedure TIFFSwabArrayOfFloat(Fp: pfloat; N: tmsize_t); cdecl; external;

{$IFDEF LOGLUV_PUBLIC}
const
  U_NEU   = 0.210526316;
  V_NEU   = 0.473684211;
  UVSCALE = 410.0;

function  LogL16toY(p16: Integer): Double; cdecl; external;
function  LogL10toY(p10: Integer): Double; cdecl; external;
procedure XYZtoRGB24(xyz: pfloat; rgb: puint8); cdecl; external;
function  uv_decode(up: pdouble; vp: pdouble; c: Integer): Integer; cdecl; external;
procedure LogLuv24toXYZ(p: uint32; XYZ: pfloat); cdecl; external;
procedure LogLuv32toXYZ(p: uint32; XYZ: pfloat); cdecl; external;

function  LogL16fromY(Y: double; Em: Integer): Integer; cdecl; external;
function  LogL10fromY(Y: double; Em: Integer): Integer; cdecl; external;
function  uv_encode(u: double;v: double; Em: Integer): Integer; cdecl; external;
function  LogLuv24fromXYZ(XYZ: pfloat; Em: Integer): uint32; cdecl; external;
function  LogLuv32fromXYZ(XYZ: pfloat; Em: Integer): uint32; cdecl; external;
{$ENDIF LOGLUV_PUBLIC}

{$ENDIF LIBTIFF4}

function  TIFFCIELabToRGBInit(cielab: PTIFFCIELabToRGB; display: PTIFFDisplay;
  refWhite: pfloat): Integer; cdecl; external;
procedure TIFFCIELabToXYZ(cielab: PTIFFCIELabToRGB; l: uint32; a: int32; b: int32;
  X: pfloat; Y: pfloat; Z: pfloat); cdecl; external;
procedure TIFFXYZToRGB(cielab: PTIFFCIELabToRGB; X: float; Y: float; Z: float;
  r: puint32; g: puint32; b: puint32); cdecl; external;

function  TIFFYCbCrToRGBInit(ycbcr: PTIFFYCbCrToRGB; luma: pfloat;
  refBlackWhite: pfloat): Integer; cdecl; external;
procedure TIFFYCbCrtoRGB(ycbcr: PTIFFYCbCrToRGB; Y: uint32; Cb: int32; Cr: int32;
  r: puint32; g: puint32; b: puint32); cdecl; external;


{$IFNDEF LIBTIFF4}
  {
    TIFF Image File Directories are comprised of a table of field descriptors of the form shown
    below.  The table is sorted in ascending order by tag.  The values associated with each entry
    are disjoint and may appear anywhere in the file (so long as they are placed on a word boundary).

    If the value is 4 bytes or less, then it is placed in the offset field to save space.  If the value
    is less than 4 bytes, it is left-justified in the offset field.
  }
type
  TTIFFDirEntry = record
    tdir_tag: Word;        // see below
    tdir_type: Word;       // data type; see below
    tdir_count: Cardinal;  // number of items; length in spec
    tdir_offset: Cardinal; // byte offset to field data
  end;

function  TIFFDefaultDirectory(Handle: PTIFF): Integer; cdecl; external;
procedure TIFFMergeFieldInfo(Handle: PTIFF; Info: PTIFFFieldInfo; N: Integer); cdecl; external;
{$ENDIF ~LIBTIFF4}


implementation

uses
  LibDelphi, {$IFNDEF FPC}LibStub,{$ENDIF} LibJpeg, ZLibDelphi, StrUtils;

{$IFDEF FPC}
  // fpc: link libtiff
  {$LINKLIB libtiff.a} // Todo: should be last to be loaded
                       // However: since it's a hassle to figure out a
                       // working loading order we leave it like this for now.
  {$IFDEF LIBTIFF4}
    {$IFDEF MSWINDOWS}
      {$IFNDEF CPU64}
        {$LINKLIB libgcc.a} // __udivdi3
        // Also missing _snprint. Can't figure out what's wrong atm so we will use
        // snprintf from LibDelphi for now.
      {$ELSE}
        {$LINKLIB libmingwex.a}
        {$LINKLIB libmingw32.a}
        {$LINKLIB libmsvcrt.a}
        {$LINKLIB libmingw32.a}
        {$LINKLIB libkernel32.a}
        {$LINKLIB libuser32.a}
        {$LINKLIB libmsvcrt.a}

        {.$LINKLIB liblzma.a}   // Only needed if you link libtiff with lzma enabled
      {$ENDIF}
    {$ENDIF}
    {$IFDEF UNIX}
      Todo...
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

var
  {$IFNDEF FPC}
  _TIFFwarningHandler: TIFFErrorHandler;
  _TIFFerrorHandler: TIFFErrorHandler;
  {$ELSE}
  {$IFNDEF CPU64}
  __TIFFwarningHandler: TIFFErrorHandler; public name '__TIFFwarningHandler';
  __TIFFerrorHandler: TIFFErrorHandler; public name  '__TIFFerrorHandler';
  {$ELSE}
  __TIFFwarningHandler: TIFFErrorHandler;
  __TIFFerrorHandler: TIFFErrorHandler;
  {$ENDIF}
  {$ENDIF}
  FLibTiffDelphiWarningHandler: LibTiffDelphiErrorHandler;
  FLibTiffDelphiErrorHandler: LibTiffDelphiErrorHandler;

// -----  Required TIFF memory handling functions. -----------------------------

{$IFNDEF FPC}
function _TIFFmalloc(s: tmsize_t): Pointer; cdecl;
{$ELSE}
function _TIFFmalloc(s: tmsize_t): Pointer; cdecl; public name '__TIFFmalloc';
{$ENDIF}
begin
  Result := AllocMem(s);
end;

{$IFNDEF FPC}
function _TIFFrealloc(p: Pointer; s: tmsize_t): Pointer; cdecl;
{$ELSE}
function _TIFFrealloc(p: Pointer; s: tmsize_t): Pointer; cdecl; public name '__TIFFrealloc';
//[public,alias:'__TIFFrealloc'];
{$ENDIF}
begin
  if p = nil then
    Result := AllocMem(s)
  else
    Result := ReallocMemory(p,s);
end;

{$IFNDEF FPC}
procedure _TIFFmemset(p: Pointer; v: Integer; c: tmsize_t); cdecl;
{$ELSE}
procedure _TIFFmemset(p: Pointer; v: Integer; c: tmsize_t); cdecl; public name '__TIFFmemset';
{$ENDIF}
begin
  FillChar(p^, c, v);
end;

{$IFNDEF FPC}
procedure _TIFFmemcpy(d: Pointer; s: Pointer; c: tmsize_t); cdecl;
{$ELSE}
procedure _TIFFmemcpy(d: Pointer; s: Pointer; c: tmsize_t); cdecl; public name '__TIFFmemcpy';
{$ENDIF}
begin
  Move(s^, d^, c);
end;

{$IFNDEF FPC}
function _TIFFmemcmp(buf1: Pointer; buf2: Pointer; count: tmsize_t): Integer; cdecl;
{$ELSE}
function _TIFFmemcmp(buf1: Pointer; buf2: Pointer; count: tmsize_t): Integer; cdecl; public name '__TIFFmemcmp';
{$ENDIF}
var
  ma, mb: PByte;
  n: Integer;
begin
  ma := buf1;
  mb := buf2;
  n := 0;
  while n < Count do
  begin
    if ma^ <> mb^ then
    begin
      if ma^ < mb^ then
        Result := -1
      else
        Result := 1;
      exit;
    end;
    Inc(ma);
    Inc(mb);
    Inc(n);
  end;
  Result := 0;
end;

{$IFNDEF FPC}
procedure _TIFFfree(p: Pointer); cdecl;
{$ELSE}
procedure _TIFFfree(p: Pointer); cdecl; public name '__TIFFfree';
{$ENDIF}
begin
  FreeMem(p);
end;


// -----  LibTiffDelphi Version and Error handlers. ----------------------------

function  LibTiffDelphiVersion: AnsiString;
var
  m: AnsiString;
  na, nb: Integer;
begin
  Result := '';
  m := TIFFGetVersion;
  na := 1;
  while True do
  begin
    nb := na;
    while nb <= Length(m) do
    begin
      if m[nb] = #10 then break;
      Inc(nb);
    end;
    Result := Result + System.Copy(m, na, nb - na);
    if nb > Length(m) then break;
    Result := Result + #13#10;
    na := nb + 1;
  end;
  Result := Result + #13#10 + LibTiffDelphiVersionString;
end;

function  LibTiffDelphiGetVersionNumber: AnsiString;
const CLibTiffStart = 'LIBTIFF, Version ';
var TiffVer: AnsiString;
begin
  TiffVer := TIFFGetVersion;
  if AnsiStartsStr(CLibTiffStart, TiffVer) and (Length(TiffVer) >= Length(CLibTiffStart)+5) then
    Result := Copy(TiffVer, Length(CLibTiffStart)+1, 5)
  else
    Result := '0.0.0';
end;

procedure LibTiffDelphiWarningThrp(const Module: PAnsiChar; const Fmt: PAnsiChar; Args: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiWarningHandler <> nil then
  begin
    m := sprintfsec( nil, Fmt, Args);
    SetLength(n, m);
    sprintfsec(Pointer(n), Fmt, Args);
    FLibTiffDelphiWarningHandler(Module, n);
  end;
end;

{$DEFINE ADAPT_TIFF_ERRORS}
{$IFDEF ADAPT_TIFF_ERRORS}
const
  // Wang has made an error writing their own private tag which causes errors
  // in libtif 4.x which I think in this case should just be a warning
  // therefore we check for this error and send it further as a warning
  ErrorStart = ': Null count for "Tag ';
  //WangTagError = ': Null count for "Tag 32934" (type 4, writecount -3, passcount 1)';
  // See also: http://www.imagemagick.org/discourse-server/viewtopic.php?t=27173
  // Other tags I've encountered that may do this: 34022 (type 1), 62630
  // And according to the link: 34025, 34026, 34031

// We assume here that AString starts with ErrorStart
function CanWeIgnoreError(AString: AnsiString): boolean;
var TagCode: Integer;
begin
  // Assume the 5 chars following ErrorStart are a tag number
  TagCode := StrToIntDef(Copy(AString, Length(ErrorStart)+1, 5), 0);
  case TagCode of
    34022, 34025, 34026, 34031, 32934, 62630: Result := True;
  else
    Result := False;
  end;
end;
{$ENDIF}

procedure LibTiffDelphiErrorThrp(const Module: PAnsiChar; const Fmt: PAnsiChar; Args: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiErrorHandler <> nil then
  begin
    m := sprintfsec(nil, Fmt, Args);
    SetLength(n, m);
    sprintfsec(Pointer(n), Fmt, Args);
    // The length set is including the #0 at the end of the string.
    // Since in some cases having the #0 included in the string can lead to
    // problems (writing the #0 in xml unittest results), we will change the
    // length here
    if (m > 0) and (n[m] = #0) then
      SetLength(n, m-1);
{$IFDEF ADAPT_TIFF_ERRORS}
    if AnsiStartsStr(ErrorStart, n) and CanWeIgnoreError(n) then begin
      if @FLibTiffDelphiWarningHandler <> nil then
        FLibTiffDelphiWarningHandler(Module, n);
    end
    else
{$ENDIF}
    FLibTiffDelphiErrorHandler(Module, n);
  end;
end;

function LibTiffDelphiGetWarningHandler: LibTiffDelphiErrorHandler;
begin
  Result := FLibTiffDelphiWarningHandler;
end;

function LibTiffDelphiSetWarningHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
begin
  Result := FLibTiffDelphiWarningHandler;
  FLibTiffDelphiWarningHandler := Handler;
end;

function LibTiffDelphiGetErrorHandler: LibTiffDelphiErrorHandler;
begin
  Result := FLibTiffDelphiErrorHandler;
end;

function LibTiffDelphiSetErrorHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
begin
  Result := FLibTiffDelphiErrorHandler;
  FLibTiffDelphiErrorHandler := Handler;
end;

{$IFDEF LIBTIFF4}
// The tiff .obj files need access to private extern functions declared in
// tiffiop.h etc.
// Since they just need to be referenced we are not going to add the
// complete parameter lists.

// From tif_dir.h
procedure _TIFFGetFields(); external;
procedure _TIFFGetExifFields(); external;
procedure _TIFFSetupFields(); external;
procedure _TIFFPrintFieldInfo(); external;
procedure _TIFFFillStriles(); external;
procedure _TIFFMergeFields(); external;
procedure _TIFFFindOrRegisterField(); external;
procedure _TIFFCreateAnonField(); external;


// From tiffiop.h
// _TIFFGetMode is used in our TIFFOpen therefore we do add the exact
// parameter declaration for this one even though it should stay private.
// N.B.: Lazarus requires the following declaration to be the same case:
// thus no uppercase G for GetMode!
function  _TIFFgetMode(Mode: PAnsiChar; Module: PAnsiChar): Integer; cdecl; external;
procedure _TIFFNoRowEncode(); external;
procedure _TIFFNoStripEncode(); external;
procedure _TIFFNoTileEncode(); external;
procedure _TIFFNoRowDecode(); external;
procedure _TIFFNoStripDecode(); external;
procedure _TIFFNoTileDecode(); external;
procedure _TIFFNoPostDecode(); external;
procedure _TIFFNoPreCode(); external;
procedure _TIFFNoSeek(); external;
procedure _TIFFSwab16BitData(); external;
procedure _TIFFSwab24BitData(); external;
procedure _TIFFSwab32BitData(); external;
procedure _TIFFSwab64BitData(); external;
procedure TIFFFlushData1(); external;
procedure TIFFDefaultDirectory(); external;
procedure _TIFFSetDefaultCompressionState(); external;
procedure _TIFFRewriteField(); external;
procedure TIFFSetCompressionScheme(); external;
//procedure TIFFSetDefaultCompressionState(); external; - not present anywhere but tiffiop.h?
procedure _TIFFDefaultStripSize(); external;
procedure _TIFFDefaultTileSize(); external;
procedure _TIFFDataSize(); external;

procedure _TIFFsetByteArray(); external;
procedure _TIFFsetString(); external;
procedure _TIFFsetShortArray(); external;
procedure _TIFFsetLongArray(); external;
procedure _TIFFsetFloatArray(); external;
procedure _TIFFsetDoubleArray(); external;

procedure _TIFFprintAscii(); external;
procedure _TIFFprintAsciiTag(); external;

//procedure _TIFFwarningHandler(); external;
//procedure _TIFFerrorHandler(); external;
//procedure _TIFFwarningHandlerExt(); external;
//procedure _TIFFerrorHandlerExt(); external;

procedure _TIFFMultiply32(); external;
procedure _TIFFMultiply64(); external;
procedure _TIFFCheckMalloc(); external;
procedure _TIFFCheckRealloc(); external;

procedure _TIFFUInt64ToDouble(); external;
procedure _TIFFUInt64ToFloat(); external;

procedure TIFFInitDumpMode(); external;

// From tif_predict.h
procedure TIFFPredictorInit(); external;
procedure TIFFPredictorCleanup(); external;
{$ENDIF LIBTIFF4}


// -----  LINK libtiff obj files and -------------------------------------------

{$IFNDEF FPC}

{$LINK tif_read.obj}
{$LINK tif_dirinfo.obj}
{$LINK tif_dirwrite.obj}
{$LINK tif_flush.obj}
{$LINK tif_write.obj}
{$LINK tif_dumpmode.obj}
{$LINK tif_compress.obj}
{$LINK tif_dirread.obj}
{$LINK tif_dir.obj}
{$LINK tif_aux.obj}
{$LINK tif_color.obj}
{$LINK tif_close.obj}
{$LINK tif_extension.obj}
{$LINK tif_open.obj}
{$LINK tif_getimage.obj}
{$LINK tif_predict.obj}
{$LINK tif_print.obj}
{$LINK tif_error.obj}
{$LINK tif_strip.obj}
{$LINK tif_swab.obj}
{$LINK tif_tile.obj}
{$LINK tif_warning.obj}
{$LINK tif_version.obj}
{$LINK tif_codec.obj}
{$LINK tif_fax3.obj}
{$LINK tif_fax3sm.obj}
{$LINK tif_jpeg.obj}
{$LINK tif_luv.obj}
{$LINK tif_lzw.obj}
{$LINK tif_next.obj}
{$LINK tif_packbits.obj}
{$LINK tif_pixarlog.obj}
{$LINK tif_thunder.obj}
{$LINK tif_zip.obj}
{$LINK tif_ojpeg.obj}

{$ENDIF ~FPC}

{$IFNDEF LIBTIFF4}

// Note: Commented out functions starting with (I) are defined in the interface,
//       those starting with (R) have redefined names.
//       To make it easier to find back where they are defined in the source
//       we also mention them in the implemention with the linked obj file.

// -----  tif_read -------------------------------------------------------------
// Scanline-oriented Read Support

// (I) function  TIFFReadScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;
// (I) function  TIFFReadEncodedStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
// (I) function  TIFFReadRawStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFFillStrip(Handle: PTIFF; Strip : Cardinal): Integer; cdecl; external;

// Tile-oriented Read Support
// (I) function  TIFFReadTile(Handle: Pointer; Buf: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
// (I) function  TIFFReadEncodedTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
// (I) function  TIFFReadRawTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFFillTile(Handle: PTIFF; Tile: Cardinal): Integer; cdecl; external;
// (I) function  TIFFReadBufferSetup(Handle: PTIFF; bp: Pointer; Size: Integer): Integer; cdecl; external;

procedure _TIFFNoPostDecode(Handle: PTIFF; Buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab16BitData(Handle: PTIFF; Buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab24BitData(Handle: PTIFF; Buf: pointer; cc: integer); cdecl; external;
procedure _TIFFSwab32BitData(Handle: PTIFF; Buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab64BitData(Handle: PTIFF; Buf: Pointer; cc: Integer); cdecl; external;


// -----  tiff_dir.h -----------------------------------------------------------
// Defines the following as names for the functions with an underscore in tif_dirinfo

// (I) function  TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external;
// (I) function  TIFFFindFieldInfoByName(Handle: PTIFF; FieldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external;
// (I) function  TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external;
// (I) function  TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external;

// -----  tif_dirinfo ----------------------------------------------------------
// Core Directory Tag Support.

function  _TIFFGetFieldInfo(Size : PLongInt): Pointer; cdecl; external;
function  _TIFFGetExifFieldInfo(Size : PLongInt): Pointer; cdecl; external;
procedure _TIFFSetupFieldInfo(Handle: PTIFF); cdecl; external;
// (I) procedure TIFFMergeFieldInfo(Handle: PTIFF; Info: PTIFFFieldInfo; N: Integer); cdecl; external;
function  _TIFFMergeFieldInfo(Handle: PTIFF; FieldInfo : PTIFFFieldInfo; N : Integer):Integer; cdecl; external;
function  _TIFFPrintFieldInfo(Handle: PTIFF; Fd: Pointer):Integer; cdecl; external;
// (I) function  TIFFDataWidth(DataType: Integer): Integer; cdecl; external;
function  _TIFFDataSize(DataType : Integer): Integer; cdecl; external;
function  _TIFFSampleToTagType(Handle: PTIFF): Integer; cdecl; external;
// (R) function  _TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external;
// (R) function  _TIFFFindFieldInfoByName(Handle: PTIFF; FieldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external;
// (R) function  _TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external;
// (R) function  _TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external;
function  _TIFFFindOrRegisterFieldInfo( Handle: PTIFF; Tag: Cardinal; Dt: Integer ): PTIFFFieldInfo; cdecl; external;
function  _TIFFCreateAnonFieldInfo(Handle: PTIFF; Tag: Cardinal; field_type: Integer): PTIFFFieldInfo; cdecl; external;


// ----- tif_dirwrite ----------------------------------------------------------
// Directory Write Support Routines.
// (I) function  TIFFWriteDirectory(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFCheckpointDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFWriteCustomDirectory(Handle: PTIFF; pdiroff: PCardinal): Integer; cdecl; external;
// (I) function  TIFFRewriteDirectory(Handle: PTIFF): Integer; cdecl; external;


// -----  tif_flush ------------------------------------------------------------

// (I) function  TIFFFlush(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFFlushData(Handle: PTIFF): Integer; cdecl; external;


// -----  tif_write ------------------------------------------------------------
// Scanline-oriented Write Support

// (I) function  TIFFWriteScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;
// (I) function  TIFFWriteEncodedStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
// (I) function  TIFFWriteRawStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
// (I) function  TIFFWriteTile(Handle: Pointer; Buf: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
// (I) function  TIFFWriteEncodedTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
// (I) function  TIFFWriteRawTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
// (I) function  TIFFSetupStrips(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFWriteCheck(Handle: PTIFF; Tiles: Integer; Module: PAnsiChar): Integer; cdecl; external;
// (I) function  TIFFWriteBufferSetup(Handle: PTIFF; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFFlushData1(Handle: PTIFF): Integer; cdecl; external;
// (I) procedure TIFFSetWriteOffset(Handle: PTIFF; Off: Cardinal); cdecl; external;


// -----  tif_dumpmode ---------------------------------------------------------
// "Null" Compression Algorithm Support.

function  TIFFInitDumpMode(Handle: PTIFF; Scheme: Integer): Integer; cdecl; external;


// -----  tif_compress ---------------------------------------------------------
// Compression Scheme Configuration Support.

procedure _TIFFSetDefaultCompressionState(Handle: PTIFF); cdecl; external;
function  TIFFSetCompressionScheme(Handle: PTIFF; Scheme: Integer): Integer; cdecl; external;
// (I) function  TIFFFindCODEC(Scheme: Word): PTIFFCodec; cdecl; external;
// (I) function  TIFFRegisterCODEC(Scheme: Word; Name: PAnsiChar; InitMethod: TIFFInitMethod): PTIFFCodec; cdecl; external;
// (I) procedure TIFFUnRegisterCODEC(c: PTIFFCodec); cdecl; external;
// (I) function  TIFFGetConfiguredCODECs: PTIFFCodec; cdecl; external;


// -----  tif_dirread ----------------------------------------------------------
// Directory Read Support Routines.

// (I) function  TIFFReadDirectory(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFReadCustomDirectory(Handle: PTIFF; DirOff: Cardinal; Info: PTIFFFieldInfo; N: Integer): Integer; cdecl; external;
// (I) function  TIFFReadEXIFDirectory(Handle: PTIFF; DirOff: Cardinal): Integer; cdecl; external;


// -----  tif_dir --------------------------------------------------------------
// Directory Tag Get & Set Routines.  (and also some miscellaneous stuff)

procedure _TIFFsetByteArray(vpp: Pointer; vp: Pointer; n: Integer); cdecl; external;
procedure _TIFFsetString(cpp: Pointer; cp: Pointer); cdecl; external;
// More similar functions as the two above not mentioned here.
// (I) function  TIFFSetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
// (I) function  TIFFVSetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
// (I) function  TIFFGetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
// (I) function  TIFFVGetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
// (I) procedure TIFFFreeDirectory(Handle: PTIFF); cdecl; external;
// (I) function  TIFFSetTagExtender(Extender: TIFFExtendProc): TIFFExtendProc; cdecl; external;
// (I) function  TIFFCreateDirectory(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFDefaultDirectory(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFNumberOfDirectories(Handle: PTIFF): Word; cdecl; external;
// (I) function  TIFFSetDirectory(Handle: PTIFF; Dirn: Word): Integer; cdecl; external;
// (I) function  TIFFSetSubDirectory(Handle: PTIFF; DirOff: Cardinal): Integer; cdecl; external;
// (I) function  TIFFCurrentDirOffset(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFLastDirectory(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFUnlinkDirectory(handle: PTIFF; Dirn: Word): Integer; cdecl; external;
// TIFFReassignTagToIgnore FIXME: this is never used properly. Should be removed in the future.
function  TIFFReassignTagToIgnore(Task: Integer; TIFFtagID: Integer): Integer; cdecl; external;


// -----  tif_aux --------------------------------------------------------------
// Auxiliary Support Routines.

// (I) function  TIFFVGetFieldDefaulted(tif: Pointer; tag: Cardinal; ap: Pointer): Integer; cdecl; external;
// (I) function  TIFFGetFieldDefaulted(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;


// -----  tif_color ------------------------------------------------------------
// Color space conversion routines.

// (I) procedure TIFFCIELabToXYZ(cielab: PTIFFCIELabToRGB; l: Cardinal; a: Integer; b: Integer; X: PSingle; Y: PSingle; Z: PSingle); cdecl; external;
// (I) procedure TIFFXYZToRGB(cielab: PTIFFCIELabToRGB; X: Single; Y: Single; Z: Single; r: PCardinal; g: PCardinal; b: PCardinal); cdecl; external;
// (I) function  TIFFCIELabToRGBInit(cielab: PTIFFCIELabToRGB; display: PTIFFDisplay; refWhite: PSingle): Integer; cdecl; external;
// (I) procedure TIFFYCbCrtoRGB(ycbcr: PTIFFYCbCrToRGB; Y: Cardinal; Cb: Integer; Cr: Integer; r: PCardinal; g: PCardinal; b: PCardinal); cdecl; external;
// (I) function  TIFFYCbCrToRGBInit(ycbcr: PTIFFYCbCrToRGB; luma: PSingle; refBlackWhite: PSingle): Integer; cdecl; external;


// -----  tif_close ------------------------------------------------------------
// (I) procedure TIFFCleanup(Handle: PTIFF); cdecl; external;
// (I) procedure TIFFClose(Handle: PTIFF); cdecl; external;


// -----  tif_extension --------------------------------------------------------
// Various routines support external extension of the tag set, and other
// application extension capabilities.

// (I) function  TIFFGetTagListCount(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFGetTagListEntry(Handle: PTIFF; TagIndex: Integer): Cardinal; cdecl; external;
// (I) function  TIFFAccessTagMethods(Handle: PTIFF): PTIFFTagMethods; cdecl; external;
// (I) function  TIFFGetClientInfo( Handle: PTIFF; const Name : PAnsiChar): Pointer; cdecl; external;
// (I) procedure TIFFSetClientInfo( Handle: PTIFF; Data: Pointer; const Name : PAnsiChar); cdecl; external;


// -----  tif_open -------------------------------------------------------------

function  _TIFFgetMode(Mode: PAnsiChar; Module: PAnsiChar): Integer; cdecl; external;
// (I) function  TIFFClientOpen(Name: PAnsiChar; Mode: PAnsiChar; ClientData: Cardinal;
//          ReadProc: TIFFReadWriteProc;
//          WriteProc: TIFFReadWriteProc;
//          SeekProc: TIFFSeekProc;
//          CloseProc: TIFFCloseProc;
//          SizeProc: TIFFSizeProc;
//          MapProc: TIFFMapFileProc;
//          UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl; external;
// (I) function  TIFFFileName(Handle: PTIFF): PAnsiChar; cdecl; external;
// (I) function  TIFFSetFileName(Handle: PTIFF; Name: PAnsiChar): PAnsiChar; cdecl; external;
// (I) function  TIFFFileno(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFSetFileno(Handle: PTIFF; Newvalue: Integer): Integer; cdecl; external;
// (I) function  TIFFClientdata(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFSetClientdata(Handle: PTIFF; Newvalue: Cardinal): Cardinal; cdecl; external;
// (I) function  TIFFGetMode(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFSetMode(Handle: PTIFF; Mode: Integer): Integer; cdecl; external;
// (I) function  TIFFIsTiled(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFCurrentRow(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFCurrentDirectory(Handle: PTIFF): Word; cdecl; external;
// (I) function  TIFFCurrentStrip(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFCurrentTile(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFIsByteSwapped(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFIsUpSampled(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFIsMSB2LSB(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFIsBigEndian(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFGetReadProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
// (I) function  TIFFGetWriteProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
// (I) function  TIFFGetSeekProc(Handle: PTIFF): TIFFSeekProc; cdecl; external;
// (I) function  TIFFGetCloseProc(Handle: PTIFF): TIFFCloseProc; cdecl; external;
// (I) function  TIFFGetSizeProc(Handle: PTIFF): TIFFSizeProc; cdecl; external;
// (I) function  TIFFGetMapFileProc(Handle: PTIFF): TIFFMapFileProc; cdecl; external;
// (I) function  TIFFGetUnmapFileProc(Handle: PTIFF): TIFFUnmapFileProc; cdecl; external;


// -----  tif_getimage ---------------------------------------------------------
// Read and return a packed RGBA image.

// (I) function  TIFFRGBAImageOk(Handle: PTIFF; Emsg: PAnsiChar): Integer; cdecl; external;
// (I) procedure TIFFRGBAImageEnd(Img: PTIFFRGBAImage); cdecl; external;
// (I) function  TIFFRGBAImageBegin(Img: PTIFFRGBAImage; Handle: PTIFF; Stop: Integer; Emsg: PAnsiChar): Integer; cdecl; external;
// (I) function  TIFFRGBAImageGet(Img: PTIFFRGBAImage; Raster: Pointer; W, H: Cardinal): Integer; cdecl; external;
// (I) function  TIFFReadRGBAImageOriented(Handle: PTIFF; RWidth, RHeight: Cardinal; Raster: Pointer; Orientation: Integer; Stop: Integer): Integer; cdecl; external;
// (I) function  TIFFReadRGBAImage(Handle: PTIFF; RWidth, RHeight: Cardinal; Raster: Pointer; Stop: Integer): Integer; cdecl; external;
// (I) function  TIFFReadRGBAStrip(Handle: PTIFF; Row: Cardinal; Raster: Pointer): Integer; cdecl; external;
// (I) function  TIFFReadRGBATile(Handle: PTIFF; Col, Row: Cardinal; Raster: Pointer): Integer; cdecl; external;


// -----  tif_predict ----------------------------------------------------------

function  TIFFPredictorInit(Handle: PTIFF): Integer; cdecl; external;
function  TIFFPredictorCleanup(Handle: PTIFF):integer; cdecl; external;


// -----  tif_print ------------------------------------------------------------
// Directory Printing Support.

// (I) procedure TIFFPrintDirectory(Handle: PTIFF; Fd: Pointer; Flags: Integer); cdecl; external;


// -----  tif_error ------------------------------------------------------------

// (I) function  TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
// (I) function  TIFFSetErrorHandlerExt(Handler: TIFFErrorHandlerExt): TIFFErrorHandlerExt; cdecl; external;
// (I) procedure TIFFError(Module: PAnsiChar; Fmt: Pointer); cdecl; external; varargs;
// (I) procedure TIFFErrorExt(Fd: Pointer; Module: PAnsiChar; Fmt: Pointer); cdecl; external; varargs;


// -----  tif_strip ------------------------------------------------------------
// Strip-organized Image Support Routines.

// (I) function  TIFFComputeStrip(Handle: PTIFF; Row: Cardinal; Sample: Word): Cardinal; cdecl; external;
// (I) function  TIFFNumberOfStrips(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFVStripSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
// (I) function  TIFFRawStripSize(Handle: PTIFF; Strip: Cardinal): Integer; cdecl; external;
// (I) function  TIFFStripSize(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFDefaultStripSize(Handle: PTIFF; Request: Cardinal): Cardinal; cdecl; external;
function  _TIFFDefaultStripSize(Handle: PTIFF; s: Cardinal): Cardinal; cdecl; external;
// (I) function  TIFFScanlineSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFOldScanlineSize(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFNewScanlineSize(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFRasterScanlineSize(Handle: PTIFF): Integer; cdecl; external;


// -----  tif_swab -------------------------------------------------------------
// TIFF Library Bit & Byte Swapping Support.
// XXX We assume short = 16-bits and long = 32-bits XXX

// (I) procedure TIFFSwabShort(Wp: PWord); cdecl; external;
// (I) procedure TIFFSwabLong(Lp: PCardinal); cdecl; external;
// (I) procedure TIFFSwabArrayOfShort(Wp: PWord; N: Cardinal); cdecl; external;
// (I) procedure TIFFSwabArrayOfTriples(Tp: PByte; N: Cardinal); cdecl; external;
// (I) procedure TIFFSwabArrayOfLong(Lp: PCardinal; N: Cardinal); cdecl; external;
// (I) procedure TIFFSwabDouble(Dp: PDouble); cdecl; external;
// (I) procedure TIFFSwabArrayOfDouble(Dp: PDouble; N: Cardinal); cdecl; external;
// (I) function  TIFFGetBitRevTable(Reversed: Integer): Pointer; cdecl; external;
// (I) procedure TIFFReverseBits(Cp: Pointer; N: Cardinal); cdecl; external;


// -----  tif_tile -------------------------------------------------------------
// Tiled Image Support Routines.

// (I) function  TIFFComputeTile(Handle: PTIFF; X, Y, Z: Cardinal; S: Word): Cardinal; cdecl; external;
// (I) function  TIFFCheckTile(Handle: PTIFF; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
// (I) function  TIFFNumberOfTiles(Handle: PTIFF): Cardinal; cdecl; external;
// (I) function  TIFFTileRowSize(Handle: PTIFF): Integer; cdecl; external;
// (I) function  TIFFVTileSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
// (I) function  TIFFTileSize(Handle: PTIFF): Integer; cdecl; external;
// (I) procedure TIFFDefaultTileSize(Handle: PTIFF; Tw: PCardinal; Th: PCardinal); cdecl; external;
procedure _TIFFDefaultTileSize(Handle: PTIFF; tw: Pointer; th: Pointer); cdecl; external;


// -----  tif_warning ----------------------------------------------------------

// (I) function  TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
// (I) function  TIFFSetWarningHandlerExt(Handler: TIFFErrorHandlerExt): TIFFErrorHandlerExt; cdecl; external;
// (I) procedure TIFFWarning(Module: Pointer; Fmt: Pointer); cdecl; external; varargs;
// (I) procedure TIFFWarningExt(Fd: Pointer; Module: PAnsiChar; Fmt: Pointer); cdecl; external; varargs;


// -----  tif_version ----------------------------------------------------------

// (I) function  TIFFGetVersion: PAnsiChar; cdecl; external;


// -----  tif_codec ------------------------------------------------------------

// (I) function  TIFFIsCODECConfigured(Scheme: Word): Integer; cdecl; external;


// -----  TIFF Codecs used -----------------------------------------------------

// -----  tif_fax3 -------------------------------------------------------------

function  TIFFInitCCITTFax4(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTRLE(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTRLEW(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTFax3(tif: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_fax3sm -----------------------------------------------------------

{$ENDIF ~LIBTIFF4}

// -----  tif_jpeg -------------------------------------------------------------

procedure TIFFjpeg_error_exit_raise; cdecl; {$IFDEF FPC} public name '_TIFFjpeg_error_exit_raise'; {$ENDIF}
begin
  raise ELibJpegError.Create('LibTiff: Error in jpeg!');
end;

function TIFFcallvjpeg_jpeg_CreateCompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_CreateCompress'; {$ENDIF}
begin
  try
    jpeg_CreateCompress(cinfo, version, structsize);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_CreateDecompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_CreateDecompress'; {$ENDIF}
begin
  try
    jpeg_CreateDecompress(cinfo, version, structsize);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_defaults(cinfo: Pointer): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_set_defaults'; {$ENDIF}
begin
  try
    jpeg_set_defaults(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_colorspace(cinfo: Pointer; colorspace: J_COLOR_SPACE): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_set_colorspace'; {$ENDIF}
begin
  try
    jpeg_set_colorspace(cinfo, colorspace);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_quality(cinfo: Pointer; quality: Integer; force_baseline: LongBool): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_set_quality'; {$ENDIF}
begin
  try
    jpeg_set_quality(cinfo, quality, force_baseline);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_suppress_tables(cinfo: j_compress_ptr; suppress: LongBool): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_suppress_tables'; {$ENDIF}
begin
  try
    jpeg_suppress_tables(cinfo, suppress);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_start_compress(cinfo: j_compress_ptr; write_all_tables: LongBool): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_start_compress'; {$ENDIF}
begin
  try
    jpeg_start_compress(cinfo, write_all_tables);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_write_scanlines(errreturn: Integer; cinfo: j_compress_ptr; scanlines: Pointer; num_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_write_scanlines'; {$ENDIF}
begin
  try
    Result := jpeg_write_scanlines(cinfo, scanlines, num_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_write_raw_data(errreturn: Integer; cinfo: j_compress_ptr; data: Pointer; num_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_write_raw_data'; {$ENDIF}
begin
  try
    Result := jpeg_write_raw_data(cinfo, data, num_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_finish_compress(cinfo: j_compress_ptr): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_finish_compress'; {$ENDIF}
begin
  try
    jpeg_finish_compress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_write_tables(cinfo: j_compress_ptr): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_write_tables'; {$ENDIF}
begin
  try
    jpeg_write_tables(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_read_header(errreturn: Integer; cinfo: j_decompress_ptr; require_image: Byte): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_read_header'; {$ENDIF}
begin
  try
    Result := jpeg_read_header(cinfo, Boolean(require_image));
  except
    Result := errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_start_decompress(cinfo: j_decompress_ptr): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_start_decompress'; {$ENDIF}
begin
  try
    jpeg_start_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_read_scanlines(errreturn: Integer; cinfo: j_decompress_ptr; scanlines: Pointer; max_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_read_scanlines'; {$ENDIF}
begin
  try
    Result := jpeg_read_scanlines(cinfo, scanlines, max_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_read_raw_data(errreturn: Integer; cinfo: j_decompress_ptr; data: Pointer; max_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_read_raw_data'; {$ENDIF}
begin
  try
    Result := jpeg_read_raw_data(cinfo, data, max_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_finish_decompress(errreturn: Integer; cinfo: j_decompress_ptr): Boolean; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_jpeg_finish_decompress'; {$ENDIF}
begin
  try
    Result := jpeg_finish_decompress(cinfo);
  except
    Result := Boolean(errreturn);
  end;
end;

function TIFFcallvjpeg_jpeg_abort(cinfo: j_common_ptr): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_abort'; {$ENDIF}
begin
  try
    jpeg_abort(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_destroy(cinfo: j_common_ptr): Integer; cdecl;
{$IFDEF FPC} public name '_TIFFcallvjpeg_jpeg_destroy'; {$ENDIF}
begin
  try
    jpeg_destroy(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

type
  jpeg_alloc_sarray = function(cinfo: j_common_ptr; pool_id: Integer; samplesperrow: Cardinal; numrows: Cardinal): Pointer; cdecl;

function TIFFcalljpeg_alloc_sarray(alloc_sarray: jpeg_alloc_sarray; cinfo: j_common_ptr; pool_id: Integer; samplesperrow: Cardinal;
                    numrows: Cardinal): Pointer; cdecl;
{$IFDEF FPC} public name '_TIFFcalljpeg_alloc_sarray'; {$ENDIF}
begin
  try
    Result := alloc_sarray(cinfo, pool_id, samplesperrow, numrows);
  except
    Result := nil;
  end;
end;

{$IFNDEF LIBTIFF4}

function  TIFFInitJPEG(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_luv --------------------------------------------------------------

function  TIFFInitSGILog(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_lzw --------------------------------------------------------------

function  TIFFInitLZW(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_next -------------------------------------------------------------

function  TIFFInitNeXT(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_packbits ---------------------------------------------------------

function  TIFFInitPackBits(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_pixarlog ---------------------------------------------------------

function  TIFFInitPixarLog(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_thunder ----------------------------------------------------------

function  TIFFInitThunderScan(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;


// -----  tif_zip --------------------------------------------------------------

function  TIFFInitZIP(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;

{$ENDIF ~LIBTIFF4}

// ----- tif_ojpeg -------------------------------------------------------------

function jpeg_create_decompress_encap({%H-}sp: Pointer; cinfo: Pointer): Integer; cdecl;
{$IFDEF FPC} public name '_jpeg_create_decompress_encap'; {$ENDIF}
begin
  try
    jpeg_create_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function jpeg_read_header_encap({%H-}sp: Pointer; cinfo: Pointer; require_image: Byte): Integer; cdecl;
{$IFDEF FPC} public name '_jpeg_read_header_encap'; {$ENDIF}
begin
  try
    Result := jpeg_read_header(cinfo, Boolean(require_image));
  except
    Result := 0;
  end;
end;

function jpeg_start_decompress_encap({%H-}sp: Pointer; cinfo: Pointer): Integer; cdecl;
{$IFDEF FPC} public name '_jpeg_start_decompress_encap'; {$ENDIF}
begin
  try
    jpeg_start_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function jpeg_read_scanlines_encap({%H-}sp: Pointer; cinfo: Pointer; scanlines: Pointer;
  max_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_jpeg_read_scanlines_encap'; {$ENDIF}
begin
  try
    Result := jpeg_read_scanlines(cinfo, scanlines, max_lines);
  except
    Result := 0;
  end;
end;

function jpeg_read_raw_data_encap({%H-}sp: Pointer; cinfo: Pointer; data: Pointer;
  max_lines: Cardinal): Integer; cdecl;
{$IFDEF FPC} public name '_jpeg_read_raw_data_encap'; {$ENDIF}
begin
  try
    Result := jpeg_read_raw_data(cinfo, data, max_lines);
  except
    Result := 0;
  end;
end;

procedure jpeg_encap_unwind(Handle: PTIFF); cdecl;
{$IFDEF FPC} public name '_jpeg_encap_unwind'; {$ENDIF}
begin
  // TODO! Not sure what we need to do here.
end;

{$IFNDEF LIBTIFF4}

function  TIFFInitOJPEG(Handle: PTIFF; scheme: Integer): Integer; cdecl; external;

{$ENDIF ~LIBTIFF4}

// TODO: Add LZMA and JBIG codec (needs downloading and compiling separate libraries).

// -----  LibTiffDelphi --------------------------------------------------------

{$IFDEF MSWINDOWS} // TODO: Change to crossplatform implementation
function  TIFFFileReadProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl; forward;
function  TIFFFileWriteProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl; forward;
function  TIFFFileSizeProc(Fd: thandle_t): toff_t; cdecl; forward;
function  TIFFFileSeekProc(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl; forward;
function  TIFFFileCloseProc(Fd: thandle_t): Integer; cdecl; forward;

function  TIFFStreamReadProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl; forward;
function  TIFFStreamWriteProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl; forward;
function  TIFFStreamSizeProc(Fd: thandle_t): toff_t; cdecl; forward;
function  TIFFStreamSeekProc(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl; forward;
function  TIFFStreamCloseProc(Fd: thandle_t): Integer; cdecl; forward;

function  TIFFNoMapProc(Fd: thandle_t; PBase: PPointer; PSize: ptoff_t): Integer; cdecl; forward;
procedure TIFFNoUnmapProc(Fd: thandle_t; Base: Pointer; Size: toff_t); cdecl; forward;

function TIFFFileCloseProc(Fd: thandle_t): Integer; cdecl;
begin
  if CloseHandle(Fd) = True then
    Result := 0
  else
    Result := -1;
end;

function TIFFFileSizeProc(Fd: thandle_t): toff_t; cdecl;
begin
  Result := GetFileSize(Fd, nil);
end;

function TIFFFileSeekProc(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  MoveMethod: Cardinal;
begin
  if Off = $ffffffff then
  begin
    Result := $ffffffff;
    exit;
  end;
  case Whence of
    SEEK_SET: MoveMethod := FILE_BEGIN;
    SEEK_CUR: MoveMethod := FILE_CURRENT;
    SEEK_END: MoveMethod := FILE_END;
  else
    MoveMethod := FILE_BEGIN;
  end;
  Result := SetFilePointer(Fd, Off, nil, MoveMethod);
end;

function TIFFFileReadProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
var
  m: Cardinal;
begin
  if ReadFile(Fd, Buffer^, Cardinal(Size), m, nil) = False then
    Result := 0
  else
    Result := m;
end;

function TIFFFileWriteProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
var
  m: Cardinal;
begin
  if WriteFile(Fd, Buffer^, Cardinal(Size), m, nil) = False then
    Result := 0
  else
    Result := m;
end;

function TIFFStreamCloseProc(Fd: thandle_t): Integer; cdecl;
begin
  Result := 0;
end;

function TIFFStreamSizeProc(Fd: thandle_t): toff_t; cdecl;
begin
  try
    Result := TStream(Fd).Size;
  except
    Result := 0;
  end;
end;

function TIFFStreamSeekProc(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
var
  MoveMethod: Word;
begin
  if Off = $ffffffff then
  begin
    Result := $ffffffff;
    exit;
  end;
  case Whence of
    SEEK_SET: MoveMethod := soFromBeginning;
    SEEK_CUR: MoveMethod := soFromCurrent;
    SEEK_END: MoveMethod := soFromEnd;
  else
    MoveMethod := soFromBeginning;
  end;
  try
    Result := TStream(Fd).Seek(Off, MoveMethod);
  except
    Result := 0;
  end;
end;

function TIFFStreamReadProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
begin
  try
    Result := TStream(Fd).Read(Buffer^, Size);
  except
    Result := 0;
  end;
end;

function TIFFStreamWriteProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
begin
  try
    Result := TStream(Fd).Write(Buffer^, Size);
  except
    Result := 0;
  end;
end;

function TIFFNoMapProc(Fd: thandle_t; PBase: PPointer; PSize: ptoff_t): Integer; cdecl;
begin
  Result := 0;
end;

procedure TIFFNoUnmapProc(Fd: thandle_t; Base: Pointer; Size: toff_t); cdecl;
begin
end;

function TIFFOpen(const Name: AnsiString; const Mode: AnsiString): PTIFF;
const
  Module: AnsiString = 'TIFFOpen';
  O_RDONLY = 0;
  O_WRONLY = 1;
  O_RDWR = 2;
  O_CREAT = $0100;
  O_TRUNC = $0200;
var
  m: Integer;
  DesiredAccess: Cardinal;
  CreateDisposition: Cardinal;
  FlagsAndAttributes: Cardinal;
  fd: THandle;
begin
  m :=_TIFFgetMode(PAnsiChar(Mode), PAnsiChar(Module));
  if m = o_RDONLY then
    DesiredAccess := GENERIC_READ
  else
    DesiredAccess := (GENERIC_READ or GENERIC_WRITE);
  case m of
    O_RDONLY: CreateDisposition := OPEN_EXISTING;
    O_RDWR: CreateDisposition := OPEN_ALWAYS;
    (O_RDWR or O_CREAT): CreateDisposition := OPEN_ALWAYS;
    (O_RDWR or O_TRUNC): CreateDisposition := CREATE_ALWAYS;
    (O_RDWR or O_CREAT or O_TRUNC): CreateDisposition := CREATE_ALWAYS;
  else
    Result := nil;
    exit;
  end;
  if m = O_RDONLY then
    FlagsAndAttributes := FILE_ATTRIBUTE_READONLY
  else
    FlagsAndAttributes := FILE_ATTRIBUTE_NORMAL;
  fd := CreateFileA(PAnsiChar(Name), DesiredAccess, FILE_SHARE_READ, nil,
    CreateDisposition, FlagsAndAttributes, 0);
  if fd = INVALID_HANDLE_VALUE then
  begin
    TiffError(PAnsiChar(Module), PAnsiChar('%s: Cannot open'), PAnsiChar(Name));
    Result := nil;
    exit;
  end;
  Result := TIFFClientOpen(PAnsiChar(Name), PAnsiChar(Mode), fd, @TIFFFileReadProc,
    @TIFFFileWriteProc, @TIFFFileSeekProc, @TIFFFileCloseProc,
    @TIFFFileSizeProc, @TIFFNoMapProc, @TIFFNoUnmapProc);
  if Result <> nil then
    TIFFSetFileno(Result, fd)
  else
    CloseHandle(fd);
end;

function TIFFOpenStream(const Stream: TStream; const Mode: AnsiString): PTIFF;
var
  m: AnsiString;
begin
  m := 'Stream';
  Result := TIFFClientOpen(PAnsiChar(m), PAnsiChar(Mode), Cardinal(Stream),
    @TIFFStreamReadProc, @TIFFStreamWriteProc, @TIFFStreamSeekProc, @TIFFStreamCloseProc,
    @TIFFStreamSizeProc, @TIFFNoMapProc, @TIFFNoUnmapProc);
  if Result <> nil then
    TIFFSetFileno(Result,Cardinal(Stream));
end;
{$ENDIF}

initialization
  {$IFNDEF FPC}
  _TIFFwarningHandler := LibTiffDelphiWarningThrp;
  _TIFFerrorHandler := LibTiffDelphiErrorThrp;
  {$ELSE}
  {$IFNDEF CPU64}
  __TIFFwarningHandler := @LibTiffDelphiWarningThrp;
  __TIFFerrorHandler := @LibTiffDelphiErrorThrp;
  {$ELSE}
  // We apparently can't replace the external error handlers here which use
  // a single underscore. Thus we just define our own with two underscores
  // and use the SetErrorHandler functions to assign them.
  __TIFFerrorHandler := TIFFSetErrorHandler(@LibTiffDelphiErrorThrp);
  __TIFFwarningHandler := TIFFSetWarningHandler(@LibTiffDelphiWarningThrp);
  {$ENDIF}
  {$ENDIF}
end.

