{ Version found in ImagingLib based on libtiff 3.9.4.
  jb: Now updated to libtiff 3.9.7.
  jb: Since we want to be able to test difference with the original LibTiffDelphi
  that was based on libtiff 3.7.0 and possible future changes we are going
  to use defines to set the expected libtiff version. You will still have
  to make sure yourself that the linked obj files correspond to the version
  you are defining.
}
{.$DEFINE LIBTIFF_3_7_0}
{$DEFINE LIBTIFF_3_9_7}

{$IFDEF LIBTIFF_3_9_7}
    {$UNDEF LIBTIFF_3_7_0}
{$ELSE}
  {$IFNDEF LIBTIFF_3_7_0}
  Error: Either LIBTIFF_3_9_7 or LIBTIFF_3_7_0 should be defined!
  {$ENDIF}
{$ENDIF}

unit LibTiffDelphi;

{$ALIGN 8}
{$MINENUMSIZE 1}

interface

uses
  Classes;

const
  LibTiffDelphiVersionString = 'LibTiffDelphi 3.9.7'#13#10'Pre-compiled LibTiff for Delphi'#13#10'https://bitbucket.org/jacobb/jgb-thirdparty'#13#10;

  // Defines taken from tiff.h
  TIFF_VERSION          = 42;
  TIFF_BIGTIFF_VERSION  = 43;
  TIFF_BIGENDIAN     = $4D4D;
  TIFF_LITTLEENDIAN  = $4949;
  MDI_LITTLEENDIAN   = $5045;
  MDI_BIGENDIAN      = $4550;

  // TIFFDataType enum
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
  // jb The values below are not defined in tiff.h 3.9.7
  TIFF_UNICODE                          = 14;
  TIFF_COMPLEX                          = 15;
  TIFF_LONG8                            = 16;
  TIFF_SLONG8                           = 17;
  TIFF_IFD8                             = 18;

  // TIFF Tag Definitions.
  TIFFTAG_SUBFILETYPE                   = 254;     { subfile data descriptor }
    FILETYPE_REDUCEDIMAGE               = $1;      { reduced resolution version }
    FILETYPE_PAGE                       = $2;      { one page of many }
    FILETYPE_MASK                       = $4;      { transparency mask }
  TIFFTAG_OSUBFILETYPE                  = 255;     { kind of data in subfile }
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
    COMPRESSION_CCITTFAX4	        = 4;	   { CCITT Group 4 fax encoding }
    COMPRESSION_CCITT_T6                = 4;       { CCITT T.6 (TIFF 6 name) }
    COMPRESSION_LZW		        = 5;       { Lempel-Ziv  & Welch }
    COMPRESSION_OJPEG		        = 6;	   { !6.0 JPEG }
    COMPRESSION_JPEG		        = 7;	   { %JPEG DCT compression }
    // jb Next 2 values defined in 4.0.3
    COMPRESSION_T85			= 9;	   {* !TIFF/FX T.85 JBIG compression }
    COMPRESSION_T43			= 10;	   { !TIFF/FX T.43 colour by layered JBIG compression }
    COMPRESSION_NEXT		        = 32766;   { NeXT 2-bit RLE }
    COMPRESSION_CCITTRLEW	        = 32771;   { #1 w/ word alignment }
    COMPRESSION_PACKBITS	        = 32773;   { Macintosh RLE }
    COMPRESSION_THUNDERSCAN	        = 32809;   { ThunderScan RLE }
    { codes 32895-32898 are reserved for ANSI IT8 TIFF/IT <dkelly@apago.com) }
    COMPRESSION_IT8CTPAD	        = 32895;   { IT8 CT w/padding }
    COMPRESSION_IT8LW		        = 32896;   { IT8 Linework RLE }
    COMPRESSION_IT8MP		        = 32897;   { IT8 Monochrome picture }
    COMPRESSION_IT8BL		        = 32898;   { IT8 Binary line art }
    { compression codes 32908-32911 are reserved for Pixar }
    COMPRESSION_PIXARFILM	        = 32908;   { Pixar companded 10bit LZW }
    COMPRESSION_PIXARLOG	        = 32909;   { Pixar companded 11bit ZIP }
    COMPRESSION_DEFLATE		        = 32946;   { Deflate compression }
    COMPRESSION_ADOBE_DEFLATE           = 8;       { Deflate compression, as recognized by Adobe }
    { compression code 32947 is reserved for Oceana Matrix <dev@oceana.com> }
    COMPRESSION_DCS                     = 32947;   { Kodak DCS encoding }
    COMPRESSION_JBIG		        = 34661;   { ISO JBIG }
    COMPRESSION_SGILOG		        = 34676;   { SGI Log Luminance RLE }
    COMPRESSION_SGILOG24	        = 34677;   { SGI Log 24-bit packed }
    COMPRESSION_JP2000                  = 34712;   { Leadtools JPEG2000 }
    // jb Not documented in tiff.h but encountered compression scheme(s):
    // jb Note that Lead Tools apparently has more proprietary TIFF formats for
    //    which we don't know the values but most likely around 34710.
    COMPRESSION_LEADTOOLS_CMP           = 34709;    { LeadTools Proprietary "FILE_TIF_CMP".}
    // Defined in 4.0.3
    COMPRESSION_LZMA		        = 34925;   { LZMA2 }
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
    PREDICTOR_NONE		        = 1;	   { no prediction scheme used }
    PREDICTOR_HORIZONTAL	        = 2;	   { horizontal differencing }
    PREDICTOR_FLOATINGPOINT	        = 3;	   { floating point predictor }
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

  // Tags 400-435 taken from tiff.h 4.0.3
  // Tags 400-435 are from the TIFF/FX spec.
  TIFFTAG_GLOBALPARAMETERSIFD	        = 400;	   { ! }
  TIFFTAG_PROFILETYPE			= 401;	   { ! }
    PROFILETYPE_UNSPECIFIED	        = 0;	   { ! }
    PROFILETYPE_G3_FAX		        = 1;	   { ! }
  TIFFTAG_FAXPROFILE			= 402;	   { ! }
    FAXPROFILE_S			= 1;	   { !TIFF/FX FAX profile S }
    FAXPROFILE_F			= 2;	   { !TIFF/FX FAX profile F }
    FAXPROFILE_J			= 3;	   { !TIFF/FX FAX profile J }
    FAXPROFILE_C			= 4;	   { !TIFF/FX FAX profile C }
    FAXPROFILE_L			= 5;	   { !TIFF/FX FAX profile L }
    FAXPROFILE_M			= 6;	   { !TIFF/FX FAX profile LM }
  TIFFTAG_CODINGMETHODS		        = 403;	   { !TIFF/FX coding methods }
    CODINGMETHODS_T4_1D		        = (1 shl 1); { !T.4 1D }
    CODINGMETHODS_T4_2D		        = (1 shl 2); { !T.4 2D }
    CODINGMETHODS_T6		        = (1 shl 3); { !T.6 }
    CODINGMETHODS_T85 		        = (1 shl 4); { !T.85 JBIG }
    CODINGMETHODS_T42 		        = (1 shl 5); { !T.42 JPEG }
    CODINGMETHODS_T43		        = (1 shl 6); { !T.43 colour by layered JBIG }
  TIFFTAG_VERSIONYEAR			= 404;	   { !TIFF/FX version year }
  TIFFTAG_MODENUMBER			= 405;	   { !TIFF/FX mode number }
  TIFFTAG_DECODE			= 433;	   { !TIFF/FX decode }
  TIFFTAG_IMAGEBASECOLOR		= 434;	   { !TIFF/FX image base colour }
  TIFFTAG_T82OPTIONS			= 435;	   { !TIFF/FX T.82 options }

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
  TIFFTAG_EXIFIFD			= 34665;   { Pointer to EXIF private directory }
  { tag 34750 is a private tag registered to Adobe? }
  TIFFTAG_ICCPROFILE                    = 34675;   { ICC profile data }
  // jb Next one from 4.0.3
  TIFFTAG_IMAGELAYER		        = 34732;   { !TIFF/FX image layer information }
  { tag 34750 is a private tag registered to Pixel Magic }
  TIFFTAG_JBIGOPTIONS                   = 34750;   { JBIG options }
  TIFFTAG_GPSIFD			= 34853;   { Pointer to GPS private directory }
  { tags 34908-34914 are private tags registered to SGI }
  TIFFTAG_FAXRECVPARAMS                 = 34908;   { encoded Class 2 ses. parms }
  TIFFTAG_FAXSUBADDRESS                 = 34909;   { received SubAddr string }
  TIFFTAG_FAXRECVTIME                   = 34910;   { receive time (secs) }
  TIFFTAG_FAXDCS			= 34911;   { encoded fax ses. params, Table 2/T.30 }
  { tags 37439-37443 are registered to SGI <gregl@sgi.com> }
  TIFFTAG_STONITS                       = 37439;   { Sample value to Nits }
  { tag 34929 is a private tag registered to FedEx }
  TIFFTAG_FEDEX_EDR                     = 34929;   { unknown use }
  TIFFTAG_INTEROPERABILITYIFD	        = 40965;   { Pointer to Interoperability private directory }
  { Adobe Digital Negative (DNG) format tags }
  TIFFTAG_DNGVERSION		        = 50706;   { &DNG version number }
  TIFFTAG_DNGBACKWARDVERSION	        = 50707;   { &DNG compatibility version }
  TIFFTAG_UNIQUECAMERAMODEL	        = 50708;   { &name for the camera model }
  TIFFTAG_LOCALIZEDCAMERAMODEL	        = 50709;   { &localized camera model name }
  TIFFTAG_CFAPLANECOLOR		        = 50710;   { &CFAPattern->LinearRaw space mapping }
  TIFFTAG_CFALAYOUT		        = 50711;   { &spatial layout of the CFA }
  TIFFTAG_LINEARIZATIONTABLE	        = 50712;   { &lookup table description }
  TIFFTAG_BLACKLEVELREPEATDIM	        = 50713;   { &repeat pattern size for the BlackLevel tag }
  TIFFTAG_BLACKLEVEL		        = 50714;   { &zero light encoding level }
  TIFFTAG_BLACKLEVELDELTAH	        = 50715;   { &zero light encoding level differences (columns) }
  TIFFTAG_BLACKLEVELDELTAV	        = 50716;   { &zero light encoding level differences (rows) }
  TIFFTAG_WHITELEVEL		        = 50717;   { &fully saturated encoding level }
  TIFFTAG_DEFAULTSCALE		        = 50718;   { &default scale factors }
  TIFFTAG_DEFAULTCROPORIGIN	        = 50719;   { &origin of the final image area }
  TIFFTAG_DEFAULTCROPSIZE	        = 50720;   { &size of the final image area }
  TIFFTAG_COLORMATRIX1		        = 50721;   { &XYZ->reference color space transformation matrix 1 }
  TIFFTAG_COLORMATRIX2		        = 50722;   { &XYZ->reference color space transformation matrix 2 }
  TIFFTAG_CAMERACALIBRATION1	        = 50723;   { &calibration matrix 1 }
  TIFFTAG_CAMERACALIBRATION2	        = 50724;   { &calibration matrix 2 }
  TIFFTAG_REDUCTIONMATRIX1	        = 50725;   { &dimensionality reduction matrix 1 }
  TIFFTAG_REDUCTIONMATRIX2	        = 50726;   { &dimensionality reduction matrix 2 }
  TIFFTAG_ANALOGBALANCE		        = 50727;   { &gain applied the stored raw values}
  TIFFTAG_ASSHOTNEUTRAL		        = 50728;   { &selected white balance in linear reference space }
  TIFFTAG_ASSHOTWHITEXY		        = 50729;   { &selected white balance in x-y chromaticity coordinates }
  TIFFTAG_BASELINEEXPOSURE	        = 50730;   { &how much to move the zero point }
  TIFFTAG_BASELINENOISE		        = 50731;   { &relative noise level }
  TIFFTAG_BASELINESHARPNESS	        = 50732;   { &relative amount of sharpening }
  TIFFTAG_BAYERGREENSPLIT	        = 50733;   { &how closely the values of the green pixels in the
                                                     blue/green rows track the values of the green pixels
                                                     in the red/green rows }
  TIFFTAG_LINEARRESPONSELIMIT	        = 50734;   { &non-linear encoding range }
  TIFFTAG_CAMERASERIALNUMBER	        = 50735;   { &camera's serial number }
  TIFFTAG_LENSINFO		        = 50736;   { info about the lens }
  TIFFTAG_CHROMABLURRADIUS	        = 50737;   { &chroma blur radius }
  TIFFTAG_ANTIALIASSTRENGTH	        = 50738;   { &relative strength of the camera's anti-alias filter }
  TIFFTAG_SHADOWSCALE		        = 50739;   { &used by Adobe Camera Raw }
  TIFFTAG_DNGPRIVATEDATA	        = 50740;   { &manufacturer's private data }
  TIFFTAG_MAKERNOTESAFETY	        = 50741;   { &whether the EXIF MakerNote tag is safe to preserve
                                                     along with the rest of the EXIF data }
  TIFFTAG_CALIBRATIONILLUMINANT1        = 50778;   { &illuminant 1 }
  TIFFTAG_CALIBRATIONILLUMINANT2        = 50779;   { &illuminant 2 }
  TIFFTAG_BESTQUALITYSCALE	        = 50780;   { &best quality multiplier }
  TIFFTAG_RAWDATAUNIQUEID		= 50781;   { &unique identifier for the raw image data }
  TIFFTAG_ORIGINALRAWFILENAME	        = 50827;   { &file name of the original raw file }
  TIFFTAG_ORIGINALRAWFILEDATA	        = 50828;   { &contents of the original raw file }
  TIFFTAG_ACTIVEAREA		        = 50829;   { &active (non-masked) pixels of the sensor }
  TIFFTAG_MASKEDAREAS		        = 50830;   { &list of coordinates of fully masked pixels }
  TIFFTAG_ASSHOTICCPROFILE	        = 50831;   { &these two tags used to }
  TIFFTAG_ASSHOTPREPROFILEMATRIX	= 50832;   { map cameras's color space into ICC profile space }
  TIFFTAG_CURRENTICCPROFILE	        = 50833;   { & }
  TIFFTAG_CURRENTPREPROFILEMATRIX	= 50834;   { & }
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
  TIFFTAG_LZMAPRESET		        = 65562;   { LZMA2 preset (compression level) }
  TIFFTAG_PERSAMPLE                     = 65563;   { interface for per sample tags }
    PERSAMPLE_MERGED                    = 0;	   { present as a single value }
    PERSAMPLE_MULTI                     = 1;	   { present as multiple values }

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

  { Flags to pass to TIFFPrintDirectory to control printing of data structures that are potentially very large. Bit-or these flags to
    enable printing multiple items. }
  TIFFPRINT_NONE                        = $0;      { no extra info }
  TIFFPRINT_STRIPS                      = $1;      { strips/tiles info }
  TIFFPRINT_CURVES                      = $2;      { color/gray response curves }
  TIFFPRINT_COLORMAP                    = $4;      { colormap }
  TIFFPRINT_JPEGQTABLES                 = $100;    { JPEG Q matrices }
  TIFFPRINT_JPEGACTABLES                = $200;    { JPEG AC tables }
  TIFFPRINT_JPEGDCTABLES                = $200;    { JPEG DC tables }


  TIFF_ANY                              = TIFF_NOTYPE;   { for field descriptor searching }
  TIFF_VARIABLE                         = -1;      { marker for variable length tags }
  TIFF_SPP                              = -2;      { marker for SamplesPerPixel tags }
  TIFF_VARIABLE2                        = -3;      { marker for uint32 var-length tags }

  FIELD_CUSTOM                          = 65;

type

  PTIFF = Pointer;
  PTIFFRGBAImage = Pointer;

  TIFFErrorHandler = procedure(a: Pointer; b: Pointer; c: Pointer); cdecl;
  LibTiffDelphiErrorHandler = procedure(const a,b: AnsiString);
  TIFFReadWriteProc = function(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
  TIFFSeekProc = function(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
  TIFFCloseProc = function(Fd: Cardinal): Integer; cdecl;
  TIFFSizeProc = function(Fd: Cardinal): Cardinal; cdecl;
  TIFFMapFileProc = function(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl;
  TIFFUnmapFileProc = procedure(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl;
  TIFFExtendProc = procedure(Handle: PTIFF); cdecl;

  TIFFInitMethod = function(Handle: PTIFF; Scheme: Integer): Integer; cdecl;

  // From tiff.h
  TTIFFHeader = record
    tiff_magic: Word;      // magic number (defines byte order)
    tiff_version: Word;    // TIFF version number
    tiff_diroff: Cardinal; // byte offset to first directory
  end;

  {
    TIFF Image File Directories are comprised of a table of field descriptors of the form shown
    below.  The table is sorted in ascending order by tag.  The values associated with each entry
    are disjoint and may appear anywhere in the file (so long as they are placed on a word boundary).

    If the value is 4 bytes or less, then it is placed in the offset field to save space.  If the value
    is less than 4 bytes, it is left-justified in the offset field.
  }

  TTIFFDirEntry = record
    tdir_tag: Word;        // see below
    tdir_type: Word;       // data type; see below
    tdir_count: Cardinal;  // number of items; length in spec
    tdir_offset: Cardinal; // byte offset to field data
  end;

  PTIFFCodec = ^TIFFCodec;
  TIFFCodec = record
    Name: PAnsiChar;
    Scheme: Word;
    Init: TIFFInitMethod;
  end;

  PTIFFFieldInfo = ^TIFFFieldInfo;
  TIFFFieldInfo = record
    FieldTag: Cardinal;              { field's tag }
    FieldReadCount: Smallint;        { read count/TIFF_VARIABLE/TIFF_SPP }
    FieldWriteCount: Smallint;       { write count/TIFF_VARIABLE }
    FieldType: Integer;              { type of associated data }
    FieldBit: Word;                  { bit in fieldsset bit vector }
    FieldOkToChange: Byte;           { if true, can change while writing }
    FieldPassCount: Byte;            { if true, pass dir count on set }
    FieldName: PAnsiChar;                { ASCII name }
  end;

  // tif_dir.h
  PTIFFTagValue = ^TIFFTagValue;
  TIFFTagValue = record
    Info: PTIFFFieldInfo;
    Count: Integer;
    Value: Pointer;
  end;

function  LibTiffDelphiVersion: AnsiString;
function  TIFFGetVersion: PAnsiChar; cdecl; external;
function  TIFFFindCODEC(Scheme: Word): PTIFFCodec; cdecl; external;
function  TIFFRegisterCODEC(Scheme: Word; Name: PAnsiChar; InitMethod: TIFFInitMethod): PTIFFCodec; cdecl; external;
procedure TIFFUnRegisterCODEC(c: PTIFFCodec); cdecl; external;
function  TIFFIsCODECConfigured(Scheme: Word): Integer; cdecl; external;
function  TIFFGetConfiguredCODECs: PTIFFCodec; cdecl; external;

function  TIFFOpen(const Name: AnsiString; const Mode: AnsiString): PTIFF;
function  TIFFOpenStream(const Stream: TStream; const Mode: AnsiString): PTIFF;
function  TIFFClientOpen(Name: PAnsiChar; Mode: PAnsiChar; ClientData: Cardinal;
          ReadProc: TIFFReadWriteProc;
          WriteProc: TIFFReadWriteProc;
          SeekProc: TIFFSeekProc;
          CloseProc: TIFFCloseProc;
          SizeProc: TIFFSizeProc;
          MapProc: TIFFMapFileProc;
          UnmapProc: TIFFUnmapFileProc): PTIFF; cdecl; external;
procedure TIFFCleanup(Handle: PTIFF); cdecl; external;
procedure TIFFClose(Handle: PTIFF); cdecl; external;
function  TIFFFileno(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetFileno(Handle: PTIFF; Newvalue: Integer): Integer; cdecl; external;
function  TIFFClientdata(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFSetClientdata(Handle: PTIFF; Newvalue: Cardinal): Cardinal; cdecl; external;
function  TIFFGetMode(Handle: PTIFF): Integer; cdecl; external;
function  TIFFSetMode(Handle: PTIFF; Mode: Integer): Integer; cdecl; external;
function  TIFFFileName(Handle: PTIFF): Pointer; cdecl; external;
function  TIFFSetFileName(Handle: PTIFF; Name: PAnsiChar): PAnsiChar; cdecl; external;
function  TIFFGetReadProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetWriteProc(Handle: PTIFF): TIFFReadWriteProc; cdecl; external;
function  TIFFGetSeekProc(Handle: PTIFF): TIFFSeekProc; cdecl; external;
function  TIFFGetCloseProc(Handle: PTIFF): TIFFCloseProc; cdecl; external;
function  TIFFGetSizeProc(Handle: PTIFF): TIFFSizeProc; cdecl; external;
procedure TIFFError(Module: Pointer; Fmt: Pointer); cdecl; external; varargs;
function  TIFFSetErrorHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  LibTiffDelphiGetErrorHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetErrorHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
procedure TIFFWarning(Module: Pointer; Fmt: Pointer); cdecl; external; varargs;
function  TIFFSetWarningHandler(Handler: TIFFErrorHandler): TIFFErrorHandler; cdecl; external;
function  LibTiffDelphiGetWarningHandler: LibTiffDelphiErrorHandler;
function  LibTiffDelphiSetWarningHandler(Handler: LibTiffDelphiErrorHandler): LibTiffDelphiErrorHandler;
function  TIFFSetTagExtender(Extender: TIFFExtendProc): TIFFExtendProc; cdecl; external;


function  TIFFFlush(Handle: PTIFF): Integer; cdecl; external;
function  TIFFFlushData(Handle: PTIFF): Integer; cdecl; external;

{$IFDEF LIBTIFF_3_9_7}
function  TIFFReadEXIFDirectory(Handle: PTIFF; Diroff: Cardinal): Integer; cdecl; external;
{$ENDIF}

function  TIFFReadDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFCurrentDirectory(Handle: PTIFF): Word; cdecl; external;
function  TIFFCurrentDirOffset(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFLastDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFNumberOfDirectories(Handle: PTIFF): Word; cdecl; external;
function  TIFFSetDirectory(Handle: PTIFF; Dirn: Word): Integer; cdecl; external;
function  TIFFSetSubDirectory(Handle: PTIFF; Diroff: Cardinal): Integer; cdecl; external;
function  TIFFCreateDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFWriteDirectory(Handle: PTIFF): Integer; cdecl; external;
function  TIFFUnlinkDirectory(handle: PTIFF; Dirn: Word): Integer; cdecl; external;
procedure TIFFPrintDirectory(Handle: PTIFF; Fd: Pointer; Flags: Integer); cdecl; external;

function  TIFFGetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFGetFieldDefaulted(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFVGetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
function  TIFFVGetFieldDefaulted(tif: Pointer; tag: Cardinal; ap: Pointer): Integer; cdecl; external;
function  TIFFSetField(Handle: PTIFF; Tag: Cardinal): Integer; cdecl; external; varargs;
function  TIFFVSetField(Handle: PTIFF; Tag: Cardinal; Ap: Pointer): Integer; cdecl; external;
function  TIFFIsBigEndian(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsTiled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsByteSwapped(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsUpSampled(Handle: PTIFF): Integer; cdecl; external;
function  TIFFIsMSB2LSB(Handle: PTIFF): Integer; cdecl; external;

function  TIFFGetTagListCount(Handle: PTIFF): Integer; cdecl; external;
function  TIFFGetTagListEntry(Handle: PTIFF; TagIndex: Integer): Cardinal; cdecl; external;
procedure TIFFMergeFieldInfo(Handle: PTIFF; Info: PTIFFFieldInfo; N: Integer); cdecl; external;
function  TIFFFindFieldInfo(Handle: PTIFF; Tag: Cardinal; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFindFieldInfoByName(Handle: PTIFF; FIeldName: PAnsiChar; Dt: Integer): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithTag(Handle: PTIFF; Tag: Cardinal): PTIFFFieldInfo; cdecl; external;
function  TIFFFieldWithName(Handle: PTIFF; FieldName: PAnsiChar): PTIFFFieldInfo; cdecl; external;
function  TIFFDataWidth(DataType: Integer): Integer; cdecl; external;

function  TIFFReadRGBAImage(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Stop: Integer): Integer; cdecl; external;
function  TIFFReadRGBAImageOriented(Handle: PTIFF; RWidth,RHeight: Cardinal; Raster: Pointer; Orientation: Integer; Stop: Integer): Integer; cdecl; external;
function  TIFFReadRGBAStrip(Handle: PTIFF; Row: Cardinal; Raster: Pointer): Integer; cdecl; external;
function  TIFFReadRGBATile(Handle: PTIFF; Col,Row: Cardinal; Raster: Pointer): Integer; cdecl; external;
function  TIFFRGBAImageOk(Handle: PTIFF; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageBegin(Img: PTIFFRGBAImage; Handle: PTIFF; Stop: Integer; Emsg: PAnsiChar): Integer; cdecl; external;
function  TIFFRGBAImageGet(Img: PTIFFRGBAImage; Raster: Pointer; W,H: Cardinal): Integer; cdecl; external;
procedure TIFFRGBAImageEnd(Img: PTIFFRGBAImage); cdecl; external;

function  TIFFCurrentRow(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFStripSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFRawStripSize(Handle: PTIFF; Strip: Cardinal): Integer; cdecl; external;
function  TIFFVStripSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
function  TIFFDefaultStripSize(Handle: PTIFF; Request: Cardinal): Cardinal; cdecl; external;
function  TIFFNumberOfStrips(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFComputeStrip(Handle: PTIFF; Row: Cardinal; Sample: Word): Cardinal; cdecl; external;
function  TIFFReadRawStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFReadEncodedStrip(Handle: PTIFF; Strip: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFWriteRawStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFWriteEncodedStrip(Handle: PTIFF; Strip: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFCurrentStrip(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFTileSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFTileRowSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFVTileSize(Handle: PTIFF; NRows: Cardinal): Integer; cdecl; external;
procedure TIFFDefaultTileSize(Handle: PTIFF; Tw: PCardinal; Th: PCardinal); cdecl; external;
function  TIFFNumberOfTiles(Handle: PTIFF): Cardinal; cdecl; external;
function  TIFFComputeTile(Handle: PTIFF; X,Y,Z: Cardinal; S: Word): Cardinal; cdecl; external;
function  TIFFReadRawTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFReadEncodedTile(Handle: PTIFF; Tile: Cardinal; Buf: Pointer; Size: Integer): Integer; cdecl; external;
function  TIFFWriteRawTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFWriteEncodedTile(Handle: PTIFF; Tile: Cardinal; Data: Pointer; Cc: Integer): Integer; cdecl; external;
function  TIFFCurrentTile(Handle: PTIFF): Cardinal; cdecl; external;

function  TIFFScanlineSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFRasterScanlineSize(Handle: PTIFF): Integer; cdecl; external;
function  TIFFReadScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;
function  TIFFWriteScanline(Handle: PTIFF; Buf: Pointer; Row: Cardinal; Sample: Word): Integer; cdecl; external;

procedure TIFFSetWriteOffset(Handle: PTIFF; Off: Cardinal); cdecl; external;

procedure TIFFSwabShort(Wp: PWord); cdecl; external;
procedure TIFFSwabLong(Lp: PCardinal); cdecl; external;
procedure TIFFSwabDouble(Dp: PDouble); cdecl; external;
procedure TIFFSwabArrayOfShort(Wp: PWord; N: Cardinal); cdecl; external;
procedure TIFFSwabArrayOfLong(Lp: PCardinal; N: Cardinal); cdecl; external;
procedure TIFFSwabArrayOfDouble(Dp: PDouble; N: Cardinal); cdecl; external;
procedure TIFFReverseBits(Cp: Pointer; N: Cardinal); cdecl; external;
function  TIFFGetBitRevTable(Reversed: Integer): Pointer; cdecl; external;


implementation

uses
  Windows, SysUtils, LibDelphi, LibStub, LibJpegDelphi, ZLibDelphi;

var
  _TIFFwarningHandler: TIFFErrorHandler;
  _TIFFerrorHandler: TIFFErrorHandler;
  FLibTiffDelphiWarningHandler: LibTiffDelphiErrorHandler;
  FLibTiffDelphiErrorHandler: LibTiffDelphiErrorHandler;

function _TIFFmemcmp(buf1: Pointer; buf2: Pointer; count: Cardinal): Integer; cdecl;
var
  ma, mb: PByte;
  n: Integer;
begin
  ma := buf1;
  mb := buf2;
  n := 0;
  while Cardinal(n) < Count do
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

procedure _TIFFmemset(p: Pointer; v: Integer; c: Longint); cdecl;
begin
  FillMemory(p, c, v);
end;

function _TIFFrealloc(p: Pointer; s: Longint): Pointer; cdecl;
begin
  if p = nil then
    Result := AllocMem(s)
  else
    Result := ReallocMemory(p,s);
end;

procedure _TIFFfree(p: Pointer); cdecl;
begin
  FreeMem(p);
end;

procedure _TIFFmemcpy(d: Pointer; s: Pointer; c: Longint); cdecl;
begin
  CopyMemory(d, s, c);
end;

function _TIFFmalloc(s: Longint): Pointer; cdecl;
begin
  Result := AllocMem(s);
end;

{LibTiffDelphi}

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

procedure LibTiffDelphiWarningThrp(a: Pointer; b: Pointer; c: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiWarningHandler <> nil then
  begin
    m := sprintfsec( nil, b, c);
    SetLength(n, m);
    sprintfsec(Pointer(n), b, c);
    FLibTiffDelphiWarningHandler(PAnsiChar(a), n);
  end;
end;

procedure LibTiffDelphiErrorThrp(a: Pointer; b: Pointer; c: Pointer); cdecl;
var
  m: Integer;
  n: AnsiString;
begin
  if @FLibTiffDelphiErrorHandler <> nil then
  begin
    m := sprintfsec(nil, b, c);
    SetLength(n, m);
    sprintfsec(Pointer(n), b, c);
    FLibTiffDelphiErrorHandler(PAnsiChar(a), n);
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

{tif_read}

procedure _TIFFSwab16BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
{$IFDEF LIBTIFF_3_9_7}
procedure _TIFFSwab24BitData(tif: pointer; buf: pointer; cc: integer); cdecl; external;
{$ENDIF}
procedure _TIFFSwab32BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFSwab64BitData(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
procedure _TIFFNoPostDecode(tif: Pointer; buf: Pointer; cc: Integer); cdecl; external;
function  TIFFReadTile(tif: Pointer; buf: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
function TIFFFillTile(tif: Pointer; tile: longword):integer; cdecl; external;

{$L tif_read.obj}

{tif_dirinfo}

function  _TIFFSampleToTagType(tif: Pointer): Integer; cdecl; external;
procedure _TIFFSetupFieldInfo(tif: Pointer); cdecl; external;
function  _TIFFCreateAnonFieldInfo(tif: Pointer; tag: Cardinal; field_type: Integer): Pointer; cdecl; external;

{$IFDEF LIBTIFF_3_9_7}
function _TIFFGetExifFieldInfo(size : plongint):pointer; cdecl; external;
function _TIFFDataSize(TIFFDataType : longint):longint; cdecl; external;
function _TIFFGetFieldInfo(size : plongint):pointer; cdecl; external;
function _TIFFMergeFieldInfo(tif: Pointer; fieldinfo : Pointer; n : Integer):Integer; cdecl; external;
{$ENDIF}

{$L tif_dirinfo.obj}

{tif_dirwrite}

{$L tif_dirwrite.obj}

{tif_flush}

{$L tif_flush.obj}

{tif_write}

function  TIFFFlushData1(tif: Pointer): Integer; cdecl; external;
function  TIFFSetupStrips(tif: Pointer): Integer; cdecl; external;

{$L tif_write.obj}

{tif_dumpmode}

function  TIFFInitDumpMode(tif: Pointer; scheme: Integer): Integer; cdecl; external;

{$L tif_dumpmode.obj}

{tif_compress}

function  TIFFSetCompressionScheme(tif: Pointer; scheme: Integer): Integer; cdecl; external;
procedure _TIFFSetDefaultCompressionState(tif: Pointer); cdecl; external;

{$L tif_compress.obj}

{tif_dirread}

{$L tif_dirread.obj}

{tif_dir}

procedure TIFFFreeDirectory(tif: Pointer); cdecl; external;
function  TIFFDefaultDirectory(tif: Pointer): Integer; cdecl; external;
function  TIFFReassignTagToIgnore(task: Integer; TIFFtagID: Integer): Integer; cdecl; external;
procedure _TIFFsetString(cpp: Pointer; cp: Pointer); cdecl; external;
procedure _TIFFsetByteArray(vpp: Pointer; vp: Pointer; n: Integer); cdecl; external;

{$L tif_dir.obj}

{tif_aux}
// jb Moved to interface since we need it.
//function  TIFFVGetFieldDefaulted(tif: Pointer; tag: Cardinal; ap: Pointer): Integer; cdecl; external;

{$L tif_aux.obj}

{tif_color}

procedure TIFFCIELabToXYZ(cielab: Pointer; l: Cardinal; a: Integer; b: Integer; X: Pointer; Y: Pointer; Z: Pointer); cdecl; external;
procedure TIFFXYZToRGB(cielab: Pointer; X: Single; Y: Single; Z: Single; r: Pointer; g: Pointer; b: Pointer); cdecl; external;
procedure TIFFYCbCrtoRGB(ycbcr: Pointer; Y: Cardinal; Cb: Integer; Cr: Integer; r: Pointer; g: Pointer; b: Pointer); cdecl; external;
function  TIFFYCbCrToRGBInit(ycbcr: Pointer; luma: Pointer; refBlackWhite: Pointer): Integer; cdecl; external;
function  TIFFCIELabToRGBInit(cielab: Pointer; display: Pointer; refWhite: Pointer): Integer; cdecl; external;

{$L tif_color.obj}

{tif_close}

{$L tif_close.obj}

{tif_extension}

{$L tif_extension.obj}

{tif_open}

function  _TIFFgetMode(mode: PAnsiChar; module: PAnsiChar): Integer; cdecl; external;

{$L tif_open.obj}

{tif_getimage}

{$L tif_getimage.obj}

{tif_predict}

function  TIFFPredictorInit(tif: PTIFF): Integer; cdecl; external;
{$IFDEF LIBTIFF_3_9_7}
function  TIFFPredictorCleanup(tif: PTIFF):integer; cdecl; external;
{$ENDIF}

{$L tif_predict.obj}

{tif_print}

{$L tif_print.obj}

{tif_error}

{$L tif_error.obj}

{tif_strip}

function  _TIFFDefaultStripSize(tif: Pointer; s: Cardinal): Cardinal; cdecl; external;
{$IFDEF LIBTIFF_3_9_7}
function TIFFOldScanlineSize(tif: Pointer):Cardinal; cdecl; external;
{$ENDIF}

{$L tif_strip.obj}

{tif_swab}

{$L tif_swab.obj}

{tif_tile}

function  TIFFCheckTile(tif: Pointer; x: Cardinal; y: Cardinal; z: Cardinal; s: Word): Integer; cdecl; external;
procedure _TIFFDefaultTileSize(tif: Pointer; tw: Pointer; th: Pointer); cdecl; external;

{$L tif_tile.obj}

{tif_warning}

{$L tif_warning.obj}

{tif_fax3}

function  TIFFInitCCITTRLE(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTRLEW(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTFax3(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFInitCCITTFax4(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_fax3.obj}

{tif_fax3sm}

{$L tif_fax3sm.obj}

{tif_jpeg}

procedure TIFFjpeg_error_exit_raise; cdecl;
begin
  raise Exception.Create('TIFFjpeg_error_exit');
end;

function TIFFcallvjpeg_jpeg_CreateCompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
begin
  try
    jpeg_CreateCompress(cinfo, version, structsize);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_CreateDecompress(cinfo: Pointer; version: Integer; structsize: Cardinal): Integer; cdecl;
begin
  try
    jpeg_CreateDecompress(cinfo, version, structsize);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_defaults(cinfo: Pointer): Integer; cdecl;
begin
  try
    jpeg_set_defaults(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_colorspace(cinfo: Pointer; colorspace: Integer): Integer; cdecl;
begin
  try
    jpeg_set_colorspace(cinfo, colorspace);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_set_quality(cinfo: Pointer; quality: Integer; force_baseline: Byte): Integer; cdecl;
begin
  try
    jpeg_set_quality(cinfo, quality, force_baseline);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_suppress_tables(cinfo: PRJpegCompressStruct; suppress: Byte): Integer; cdecl;
begin
  try
    jpeg_suppress_tables(cinfo, suppress);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_start_compress(cinfo: PRJpegCompressStruct; write_all_tables: Byte): Integer; cdecl;
begin
  try
    jpeg_start_compress(cinfo, write_all_tables);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_write_scanlines(errreturn: Integer; cinfo: PRJpegCompressStruct; scanlines: Pointer; num_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_write_scanlines(cinfo, scanlines, num_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_write_raw_data(errreturn: Integer; cinfo: PRJpegCompressStruct; data: Pointer; num_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_write_raw_data(cinfo, data, num_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_finish_compress(cinfo: PRJpegCompressStruct): Integer; cdecl;
begin
  try
    jpeg_finish_compress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_write_tables(cinfo: PRJpegCompressStruct): Integer; cdecl;
begin
  try
    jpeg_write_tables(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_read_header(errreturn: Integer; cinfo: PRJpegDecompressStruct; require_image: Byte): Integer; cdecl;
begin
  try
    Result := jpeg_read_header(cinfo, Boolean(require_image));
  except
    Result := errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_start_decompress(cinfo: PRJpegDecompressStruct): Integer; cdecl;
begin
  try
    jpeg_start_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcalljpeg_jpeg_read_scanlines(errreturn: Integer; cinfo: PRJpegDecompressStruct; scanlines: Pointer; max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_read_scanlines(cinfo, scanlines, max_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_read_raw_data(errreturn: Integer; cinfo: PRJpegDecompressStruct; data: Pointer; max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_read_raw_data(cinfo, data, max_lines);
  except
    Result := errreturn;
  end;
end;

function TIFFcalljpeg_jpeg_finish_decompress(errreturn: Integer; cinfo: PRJpegDecompressStruct): Integer; cdecl;
begin
  try
    Result := jpeg_finish_decompress(cinfo);
  except
    Result := errreturn;
  end;
end;

function TIFFcallvjpeg_jpeg_abort(cinfo: PRJpegCommonStruct): Integer; cdecl;
begin
  try
    jpeg_abort(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function TIFFcallvjpeg_jpeg_destroy(cinfo: PRJpegCommonStruct): Integer; cdecl;
begin
  try
    jpeg_destroy(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

type
  jpeg_alloc_sarray = function(cinfo: PRJpegCommonStruct; pool_id: Integer; samplesperrow: Cardinal; numrows: Cardinal): Pointer; cdecl;

function TIFFcalljpeg_alloc_sarray(alloc_sarray: jpeg_alloc_sarray; cinfo: PRJpegCommonStruct; pool_id: Integer; samplesperrow: Cardinal;
                    numrows: Cardinal): Pointer; cdecl;
begin
  try
    Result := alloc_sarray(cinfo, pool_id, samplesperrow, numrows);
  except
    Result := nil;
  end;
end;

function  TIFFInitJPEG(tif: PTIFF; scheme: Integer): Integer; cdecl; external;
function  TIFFFillStrip(tif : PTIFF; Len : longword): integer; cdecl; external;

{$L tif_jpeg.obj}

{tif_luv}

function  TIFFInitSGILog(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_luv.obj}

{tif_lzw}

function  TIFFInitLZW(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_lzw.obj}

{tif_next}

function  TIFFInitNeXT(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_next.obj}

{tif_packbits}

function  TIFFInitPackBits(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_packbits.obj}

{tif_pixarlog}

function  TIFFInitPixarLog(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_pixarlog.obj}

{tif_thunder}

function  TIFFInitThunderScan(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_thunder.obj}

{tif_version}

{$L tif_version.obj}

{tif_zip}

function  TIFFInitZIP(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

{$L tif_zip.obj}

{tif_codec}


{$IFDEF LIBTIFF_3_7_0}
// Only need NotConfigured here. Beware that you need to remove static from the
// definition of NotConfigured in the C source code or Delphi won't be able to find it.
function NotConfigured(tif: PTIFF; scheme: Integer): Integer; cdecl; external;

const
  _TIFFBuiltinCODECS: array[0..17] of TIFFCodec = (
       (name:'None'; scheme: COMPRESSION_NONE; init: TIFFInitDumpMode),
       (name:'LZW'; scheme: COMPRESSION_LZW; init: TIFFInitLZW),
       (name:'PackBits'; scheme: COMPRESSION_PACKBITS; init: TIFFInitPackBits),
       (name:'ThunderScan'; scheme: COMPRESSION_THUNDERSCAN; init: TIFFInitThunderScan),
       (name:'NeXT'; scheme: COMPRESSION_NEXT; init: TIFFInitNeXT),
       (name:'JPEG'; scheme: COMPRESSION_JPEG; init: TIFFInitJPEG),
       (name:'Old-style JPEG'; scheme: COMPRESSION_OJPEG; init: NotConfigured),
       (name:'CCITT RLE'; scheme: COMPRESSION_CCITTRLE; init: TIFFInitCCITTRLE),
       (name:'CCITT RLE/W'; scheme: COMPRESSION_CCITTRLEW; init: TIFFInitCCITTRLEW),
       (name:'CCITT Group 3'; scheme: COMPRESSION_CCITTFAX3; init: TIFFInitCCITTFax3),
       (name:'CCITT Group 4'; scheme: COMPRESSION_CCITTFAX4; init: TIFFInitCCITTFax4),
       (name:'ISO JBIG'; scheme: COMPRESSION_JBIG; init: NotConfigured),
       (name:'Deflate'; scheme: COMPRESSION_DEFLATE; init: TIFFInitZIP),
       (name:'AdobeDeflate'; scheme: COMPRESSION_ADOBE_DEFLATE; init: TIFFInitZIP),
       (name:'PixarLog'; scheme: COMPRESSION_PIXARLOG; init: TIFFInitPixarLog),
       (name:'SGILog'; scheme: COMPRESSION_SGILOG; init: TIFFInitSGILog),
       (name:'SGILog24'; scheme: COMPRESSION_SGILOG24; init: TIFFInitSGILog),
       (name:nil; scheme:0; init:nil));
{$ENDIF}

{$L tif_codec.obj}

{$L tif_ojpeg.obj}
function jpeg_create_decompress_encap(sp: Pointer; cinfo: Pointer): Integer; cdecl;
begin
  try
    jpeg_create_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function jpeg_read_header_encap(sp: Pointer; cinfo: Pointer; require_image: Byte): Integer; cdecl;
begin
  try
    Result := jpeg_read_header(cinfo, Boolean(require_image));
  except
    Result := 0;
  end;
end;

function jpeg_start_decompress_encap(sp: Pointer; cinfo: Pointer): Integer; cdecl;
begin
  try
    jpeg_start_decompress(cinfo);
    Result := 1;
  except
    Result := 0;
  end;
end;

function jpeg_read_scanlines_encap(sp: Pointer; cinfo: Pointer; scanlines: Pointer;
  max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_read_scanlines(cinfo, scanlines, max_lines);
  except
    Result := 0;
  end;
end;

function jpeg_read_raw_data_encap(sp: Pointer; cinfo: Pointer; data: Pointer;
  max_lines: Cardinal): Integer; cdecl;
begin
  try
    Result := jpeg_read_raw_data(cinfo, data, max_lines);
  except
    Result := 0;
  end;
end;

procedure jpeg_encap_unwind(Handle: PTIFF); cdecl;
begin
  // TODO! Not sure what we need to do here.
end;


{LibTiffDelphi}

function  TIFFFileReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFFileWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFFileSizeProc(Fd: Cardinal): Cardinal; cdecl; forward;
function  TIFFFileSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl; forward;
function  TIFFFileCloseProc(Fd: Cardinal): Integer; cdecl; forward;

function  TIFFStreamReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFStreamWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl; forward;
function  TIFFStreamSizeProc(Fd: Cardinal): Cardinal; cdecl; forward;
function  TIFFStreamSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl; forward;
function  TIFFStreamCloseProc(Fd: Cardinal): Integer; cdecl; forward;

function  TIFFNoMapProc(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl; forward;
procedure TIFFNoUnmapProc(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl; forward;

function TIFFFileCloseProc(Fd: Cardinal): Integer; cdecl;
begin
  if CloseHandle(Fd) = True then
    Result := 0
  else
    Result := -1;
end;

function TIFFFileSizeProc(Fd: Cardinal): Cardinal; cdecl;
begin
  Result := GetFileSize(Fd, nil);
end;

function TIFFFileSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
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

function TIFFFileReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  m: Cardinal;
begin
  if ReadFile(Fd, Buffer^, Cardinal(Size), m, nil) = False then
    Result := 0
  else
    Result := m;
end;

function TIFFFileWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
var
  m: Cardinal;
begin
  if WriteFile(Fd, Buffer^, Cardinal(Size), m, nil) = False then
    Result := 0
  else
    Result := m;
end;

function TIFFStreamCloseProc(Fd: Cardinal): Integer; cdecl;
begin
  Result := 0;
end;

function TIFFStreamSizeProc(Fd: Cardinal): Cardinal; cdecl;
begin
  try
    Result := TStream(Fd).Size;
  except
    Result := 0;
  end;
end;

function TIFFStreamSeekProc(Fd: Cardinal; Off: Cardinal; Whence: Integer): Cardinal; cdecl;
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

function TIFFStreamReadProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
begin
  try
    Result := TStream(Fd).Read(Buffer^, Size);
  except
    Result := 0;
  end;
end;

function TIFFStreamWriteProc(Fd: Cardinal; Buffer: Pointer; Size: Integer): Integer; cdecl;
begin
  try
    Result := TStream(Fd).Write(Buffer^, Size);
  except
    Result := 0;
  end;
end;

function TIFFNoMapProc(Fd: Cardinal; PBase: PPointer; PSize: PCardinal): Integer; cdecl;
begin
  Result := 0;
end;

procedure TIFFNoUnmapProc(Fd: Cardinal; Base: Pointer; Size: Cardinal); cdecl;
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


initialization
  _TIFFwarningHandler:=LibTiffDelphiWarningThrp;
  _TIFFerrorHandler:=LibTiffDelphiErrorThrp;
end.

