{$TYPEDADDRESS OFF}

unit GraphicEx;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is GraphicEx.pas, released November 1, 1999.
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Portions created by Mike Lischke are
// Copyright (C) 1999, 2008 Mike Lischke. All Rights Reserved.
//
// Portions created by Jacob Boerema are
// Copyright (C) 2012-2015 Jacob Boerema. All Rights Reserved.
//
// Credits:
//   Haukur K. Bragason, Ingo Neumann, Craig Peterson
//----------------------------------------------------------------------------------------------------------------------
//
// See help file for a description of supported image formats.
//
// Version: see gexVersion.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
//
// Note: This library can be compiled with Delphi 5 or newer versions.
//
//----------------------------------------------------------------------------------------------------------------------
//
// September 2008
//   - Bug fix: size computations in component retrieval for SGI images.
// October 2006
//   - Bug fix: 16 bpp SGI images loading failed
// August 2005
//   - Bug fix: added exceptions for PCX and PCD images in case they cannot be read.
// December 2005
//   - Bug fix: The filter string returned for open dialogs was incorrect, which caused missing files in the dialog.
// November 2005
//   - Bug fix: correct handling of 256 colors in PPM files.
// October 2005
//   - Bug fix: Passing dynamic arrays of zero size to functions using the @ operator fails.
// February 2005
//   - Bug fix: Line offset in TIFF did not consider partly used source bytes (BPP 4 and lower).
//
// January 2005:
//   - Bug fix: color manager must be used for new TIFF reader.
//   - Bug fix: CompBuffer in PSD loader must be set to nil initially.
//   - Bug fix: DoStretch working bitmap is not thread safe, needs Canvas.Lock/Unlock.
//   - Improvement: New standalone function ReadImageProperties.
//
// See help file for a full development history.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$I GraphicConfiguration.inc}

{$IFDEF JpegGraphic}
  {$IF NOT Defined(USE_TJPEGIMAGE) AND NOT Defined(USE_JPEGWRAPPER) AND NOT Defined(USE_GEXJPEG)}
  The configuration for using jpeg images has changed. Please check
  ExampleGraphicConfiguration.inc on how to change your configuration file.
  {$IFEND}
{$ENDIF}


{$IFNDEF FPC}
{$I Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  // We switch off warnings about that fact. We know it and we accept it.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}
{$ELSE}
  // fpc
  {$mode delphi}
{$ENDIF}

uses
  Windows, Classes, ExtCtrls, Graphics, SysUtils, Contnrs,
  {$ifdef TIFFGraphic}
  LibTiffDelphi,
  {$endif}
  {$ifdef JpegGraphic}
    {$IF Defined(USE_TJPEGIMAGE) OR Defined(NEED_TJPEGIMAGE_SAVING)}
    {$IFNDEF FPC}jpeg,{$ENDIF}    // This will pull in the C object files.
    {$IFEND}
  {$endif ~JpegGraphic}
  {$IFDEF FPC}
  FPImage, // Progress stage defines
  {$ENDIF}
  {$IFDEF LCMS}
  gexICC, // ICC profile manager
  {$ENDIF}
  GraphicCompression, GraphicStrings, GraphicColor, gexMemory;

type
  TCardinalArray = array of Cardinal;
  TByteArray = array of Byte;
  TFloatArray = array of Single;

  TImageOptions = set of (
    ioTiled,       // image consists of tiles not strips (TIF)
    ioBigEndian,   // byte order in values >= words is reversed (TIF, RLA, SGI)
    ioMinIsWhite,  // minimum value in grayscale palette is white not black (TIF)
    ioReversed,    // bit order in bytes is reveresed (TIF)
    ioUseGamma,    // gamma correction is used
    ioSeparatePlanes // Use separate planes instead of contigious (TIF)
  );

  // describes the compression used in the image file
  TCompressionType = (
    ctUnknown,          // Compression type is unknown.
    ctNone,             // No compression.
    ctRLE,              // Run length encoding.
    ctPackedBits,       // Macintosh packed bits.
    ctLZW,              // Lempel-Zif-Welch.
    ctFax3,             // CCITT T.4 (1D), also known as fax group 3.
    ct2DFax3,           // CCITT T.4 (2D).
    ctFaxRLE,           // Modified Huffman (CCITT T.4 derivative).
    ctFax4,             // CCITT T.6, also known as fax group 4.
    ctFaxRLEW,          // CCITT T.4 with word alignment.
    ctLZ77,             // Hufman inflate/deflate.
    ctJPEG,             // TIF JPEG compression (new version)
    ctOJPEG,            // TIF JPEG compression (old version)
    ctThunderscan,      // TIF thunderscan compression
    ctNext,
    ctIT8CTPAD,
    ctIT8LW,
    ctIT8MP,
    ctIT8BL,
    ctPixarFilm,
    ctPixarLog,
    ctDCS,
    ctJBIG,
    ctPCDHuffmann,      // PhotoCD Hufman compression
    ctPlainZip,         // ZIP compression without prediction
    ctPredictedZip,     // ZIP comression with prediction
    ctSGILog,           // SGI Log Luminance RLE
    ctSGILog24,         // SGI Log 24-bit packed
    ctJpeg2000,         // Jpeg2000
    ctLZMA              // LZMA2
  );

  // Image orientation, enumeration based on the TIFF Orientation tag
  TgexOrientation = (
    gexoUnknown,
    gexoTopLeft,
    gexoTopRight,
    gexoBottomRight,
    gexoBottomLeft,
    // Rows and columns switched:
    gexoLeftTop,
    gexoRightTop,
    gexoRightBottom,
    gexoLeftBottom
  );

  // properties of a particular image which are set while loading an image or when
  // they are explicitly requested via ReadImageProperties
  PImageProperties = ^TImageProperties;
  TImageProperties = record
    Version: Cardinal;                 // TIF, PSP, GIF
    Options: TImageOptions;            // all images
    Width,                             // all images
    Height: Integer;                   // all images
    ColorScheme: TColorScheme;         // all images
    BitsPerSample,                     // all Images
    SamplesPerPixel,                   // all images
    BitsPerPixel: Byte;                // all images
    ExtraBits: Byte;                   // TGA, BMP extra bits in a pixel (e.g. bmp 555 uses 16 bits total)
    Compression: TCompressionType;     // all images
    FileGamma: Single;                 // RLA, PNG
    XResolution,
    YResolution: Single;               // given in dpi (TIF, PCX, PSP)
    Interlaced,                        // GIF, PNG
    HasAlpha: Boolean;                 // TIF, PNG
    ImageCount: Cardinal;              // Number of subimages (PCD, TIF, GIF, MNG).
    Comment: WideString;               // Implemented for PNG and GIF.
    Orientation: TgexOrientation;      // Image orientation (TIFF, Targa, RLA, ...)

    // TODO: Internal info should be moved to the specific imageformat classes.
    // Informational data, used internally and/or by decoders
    // PCD
    Overview: Boolean;                 // true if image is an overview image
    Rotate: Byte;                      // describes how the image is rotated (aka landscape vs. portrait image)

    // PNG
    FilterMode: Byte;

    // TIFF
    SampleFormat: Byte;                // DataType of samples (default = 1 = unsigned int)
  end;

  // This mode is used when creating a file mapping. See TFileMapping.
  TFileMappingMode = (
    fmmCreateNew,       // Always create a new file (overwrite any existing). Implicitely gives read/write access.
    fmmOpenOrCreate,    // Open if file exists (implicitely gives read/write access) or create if it does not.
    fmmReadOnly,        // Open existing file read only.
    fmmReadWrite        // Open existing file with read and write access.
  );

  // This class is used to provide direct (mapped) memory access to a file.
  // It is optimized for use in GraphicEx (sequential access).
  TFileMapping = class
  private
    FFileName: string;
    FFileHandle,
    FFileMapping: THandle;
    FFileSize: Int64;
    FMemory: Pointer;
  public
    constructor Create(const FileName: string; Mode: TFileMappingMode); overload;
    constructor Create(Stream: THandleStream); overload;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property Memory: Pointer read FMemory;
    property Size: Int64 read FFileSize;
  end;

  // This is the base class for all image types implemented in GraphicEx.
  // It contains some generally used stuff.
  TGraphicExGraphic = class(TBitmap)
  private
    FColorManager: TColorManager;
    {$IFDEF LCMS}
    FICCManager: TICCProfileManager;
    FICCTransformEnabled: Boolean;
    {$ENDIF}

    // Advanced progress display support.
    FProgressStack: TStack;       // Used to manage nested progress sections.
    FProgressRect: TRect;
    FPercentDone: Single;         // Progress over all parts of the load process.
  protected
    FImageProperties: TImageProperties; // Can't be private because we need access from other units
    Decoder: TDecoder;            // The decoder used to decompress the image data.

    procedure AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);
    procedure ClearProgressStack;
    procedure FinishProgressSection(DoRedraw: Boolean);
    procedure InitProgress(AWidth, AHeight: Integer);
    procedure StartProgressSection(Size: Single; const S: string);

    // We need access to the original Bitmap file/stream loading routines for our
    // bmp wrapper class.
    // Since I don't know a better way to access them I add loading routines here
    // that access the inherited LoadFromFile/LoadFromStream.
    procedure LoadBitmapFromFile(const FileName: string);
    procedure LoadBitmapFromStream(Stream: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    class function CanLoad(const FileName: string): Boolean; overload;
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; overload; virtual;
    class function CanLoad(Stream: TStream): Boolean; overload;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0); virtual;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ImageIndex: Cardinal = 0); virtual;
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ImageIndex: Cardinal = 0); virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0); virtual;

    function ReadImageProperties(const Name: string; ImageIndex: Cardinal): Boolean; overload; virtual;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; overload; virtual;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; overload; virtual;

    property ColorManager: TColorManager read FColorManager;
    property ImageProperties: TImageProperties read FImageProperties;
    {$IFDEF LCMS}
    property ICCManager: TICCProfileManager read FICCManager;
    property ICCTransformEnabled: Boolean read FICCTransformEnabled write FICCTransformEnabled
      default {$IFDEF LCMS_CONVERSION}true{$ELSE}false{$ENDIF};
    {$ENDIF}
  end;

  TGraphicExGraphicClass = class of TGraphicExGraphic;

  {$ifdef AutodeskGraphic}
  // *.cel, *.pic images
  TAutodeskGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif AutodeskGraphic}

  {$ifdef SGIGraphic}
  // *.bw, *.rgb, *.rgba, *.sgi images
  TSGIGraphic = class(TGraphicExGraphic)
  private
    FRowStart,
    FRowSize: TCardinalArray;    // Start and compressed length of the lines if the image is compressed.
    procedure GetComponents(const Memory: Pointer; var Red, Green, Blue, Alpha: Pointer; Row: Integer);
    procedure ReadAndDecode(const Memory: Pointer; Red, Green, Blue, Alpha: Pointer; Row: Integer; BPC: Cardinal);
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif SGIGraphic}

  {$ifdef TIFFGraphic}
  // *.tif, *.tiff images
  // Record to store some of the actual Tiff tag data
  TActualTiffData = record
    TiffCompression: Cardinal;
    TiffPhotometric: Cardinal;
  end;

  // YCbCr luma handling helper
  TLuma = record
    LumaRed,
    LumaGreen,
    LumaBlue: Single;
  end;
  PLuma = ^TLuma;
  TTIFFGraphic = class(TGraphicExGraphic)
  private
    FMemory: PByte;
    FCurrentPointer: PByte;
    FSize: Int64;
    FMinFloatSample,
    FMaxFloatSample: Double; // min/max values when floating point sample format is used.
    // YCbCr values
    FHorSubSampling,
    FVertSubSampling: Byte;
    FYcbCrPositioning: Byte;
    FLuma: TLuma;
    FActualTiffData: TActualTiffData;
  protected
    procedure ReadContiguous(tif: PTIFF);
    procedure ReadTiled(tif: PTIFF);
    function SetOrientation(tif: PTIFF; H: Cardinal): Cardinal;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size:Int64; ImageIndex: Cardinal): Boolean; override;
    property ActualTiffData: TActualTiffData read FActualTiffData;
  end;

    {$ifdef EPSGraphic}
    TEPSGraphic = class(TTIFFGraphic)
    public
      class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
      procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
      function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean; override;
    end;
    {$endif EPSGraphic}
  {$endif TIFFGraphic}

  {$ifdef TargaGraphic}
type
  PTargaHeader = ^TTargaHeader;
  TTargaHeader = packed record
    IDLength,
    ColorMapType,
    ImageType: Byte;
    ColorMapOrigin,
    ColorMapSize: Word;
    ColorMapEntrySize: Byte;
    XOrigin,
    YOrigin,
    Width,
    Height: Word;
    PixelSize: Byte;
    ImageDescriptor: Byte;
  end;

  TTargaV2Footer = packed record
    ExtAreaOffset: Cardinal;
    DevDirOffset:  Cardinal;
    Signature: array [0..17] of AnsiChar;
  end;

  TTargaDate = packed record
    Month: Word;
    Day: Word;
    Year: Word;
  end;
  TTargaTime = packed record
    Hour: Word;
    Minute: Word;
    Second: Word;
  end;

  TTargaAlphaAttributes = ( NoAlphaData, UndefinedAlphaCanBeIgnored,
    UndefinedAlphaButKeep, AlphaDataPresent, PreMultipliedAlpha );

  TExtensionArea = packed record
    Size: Word;                           // Should always be 495 for Version 2
    Author: array [0..40] of AnsiChar;    // Null terminated Author name
    Comments: array [0..3, 0..80] of AnsiChar; // Four lines of 80 characters each followed by a null terminator
    SaveDate: TTargaDate;
    SaveTime: TTargaTime;
    JobName: array [0..40] of AnsiChar;   // Null terminated job name or id
    JobTime: TTargaTime;
    Software: array [0..40] of AnsiChar;  // Null terminated name of the Software used to create this image
    SoftwareVersionNumber: Word;
    SoftwareVersionLetter: AnsiChar;
    KeyColor: TBGRA;                      // Background or transparent color at the time of saving
    PixelRatioNumerator: Word;
    PixelRatioDenominator: Word;
    GammaRatioNumerator: Word;            // The resulting value should be in the range of 0.0 to 10.0,
    GammaRatioDenominator: Word;          // with only one decimal place of precision necessary.
    ColorCorrectionOffset: Cardinal;      // This is an offset from the beginning of the file
                                          // to the start of the Color Correction table.
    PostageStampOffset: Cardinal;         // This is an offset from the beginning of the file
                                          // to the start of the Postage Stamp Image.
                                          // (i.e. a Thumbnail, same format as full image)
    ScanLineOffset: Cardinal;             // This is an offset from the beginning of the file
                                          // to the start of the Scan Line Table.
    Attributes: TTargaAlphaAttributes;
  end;
  PExtensionArea = ^TExtensionArea;

  // *.tga; *.vst; *.icb; *.vda; *.win images
  TTargaGraphic = class(TGraphicExGraphic)
   private
     FTargaHeader: TTargaHeader;
     FTargaFooter: TTargaV2Footer;
     FExtensionArea: PExtensionArea;
   public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
    procedure SaveToStream(Stream: TStream); overload; override;
    procedure SaveToStream(Stream: TStream; Compressed: Boolean); reintroduce; overload;

    property TargaHeader: TTargaHeader read FTargaHeader;
    property TargaFooter: TTargaV2Footer read FTargaFooter;
    property ExtensionArea: PExtensionArea read FExtensionArea;
  end;
  {$endif TargaGraphic}

  {$ifdef PCXGraphic}
  // *.pcx; *.pcc; *.scr images
  // Note: Due to the badly designed format a PCX/SCR file cannot be part in a larger stream because the position of the
  //       color palette as well as the decoding size can only be determined by the size of the image.
  //       Hence the image must be the only one in the stream or the last one.
  TPCXGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PCXGraphic}

  {$ifdef PCDGraphic}
  // *.pcd images
  // Note: By default the BASE resolution of a PCD image is loaded with LoadFromStream.
  TPCDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 2); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PCDGraphic}

  {$ifdef PortableMapGraphic}
  // *.ppm, *.pgm, *.pbm images

  TGetByteMethod = function(): Byte of object;
  TPPMGraphic = class(TGraphicExGraphic)
  private
    FSource: PAnsiChar;
    FRemainingSize: Int64;
    FGetByte: TGetByteMethod;
    function GetByteFromChar: Byte;
    function GetByteFromNumber: Byte;
    function GetChar: AnsiChar;
    function GetNumber: Cardinal;
    function ReadLine: AnsiString;
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PortableMapGraphic}

  {$ifdef CUTGraphic}
  // *.cut (+ *.pal) images
  // Note: Also this format should not be used in a stream unless it is the only image or the last one!
  TCUTGraphic = class(TGraphicExGraphic)
  private
    FPaletteFile: string;
  protected
    function LoadPalette: TMaxLogPalette;
    procedure SetDefaultPaletteFile(const FileName: string);
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0); override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property PaletteFile: string read FPaletteFile write FPaletteFile;
  end;
  {$endif CUTGraphic}

  {$ifdef GIFGraphic}
  // *.gif images
  TGifFlag = (gfHasGlobalColorTable, gfHasLocalColorTable,
    gfGlobalSorted, gfLocalSorted, gfInterlaced,
    gfHasTransparentColor, gfUserInput);
  TGifDisposalFlag = (gdfNoDisposal, gdfDoNotDispose, gdfRestoreBackgroundColor, gdfRestorePrevious);
  TGifFlags = set of TGifFlag;
  TGifInfo = record
    CanvasWidth, CanvasHeight: Word;
    FrameLeft, FrameTop,
    FrameWidth, FrameHeight: Word;
    Flags: TGifFlags;
    BackgroundColorIndex: Byte;  // According to specs only valid if global colortable present
    TransparentColorIndex: Byte;
    AspectRatio: Byte;
    DelayTime: Word;
    Disposal: TGifDisposalFlag;
  end;

  TGIFGraphic = class(TGraphicExGraphic)
  private
    // Offset of the image/frame we want to view, set by ReadImageInfo based on ImageIndex
    FImageOffset: UInt64;
    FTransparentIndex: Byte;
    FApplicationExtensions: TStringList;
    FGifInformation: TGifInfo;
    FMem: TMemoryAccess;

    function SkipExtensions(IsTargetImage: Boolean = True): Byte;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property ApplicationExtensions: TStringList read FApplicationExtensions;
    property GifInformation: TGifInfo read FGifInformation;
  end;
  {$endif GIFGraphic}

  {$ifdef RLAGraphic}
  // *.rla, *.rpf images
  // Implementation based on code from Dipl. Ing. Ingo Neumann (ingo@delphingo.com).
  TRLAGraphic = class(TGraphicExGraphic)
  private
    procedure SwapHeader(var Header); // start position of the image header in the stream
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif RLAGraphic}

  {$ifdef PhotoshopGraphic}
const
  // color modes
  // Several PSD libraries list extra modes although the official specification
  // does not mention them. I have added these here with a remark.
  PSD_BITMAP = 0;
  PSD_GRAYSCALE = 1;
  PSD_INDEXED = 2;
  PSD_RGB = 3;
  PSD_CMYK = 4;
  PSD_HSL = 5;               // Not in official specification
  PSD_HSB = 6;               // Not in official specification
  PSD_MULTICHANNEL = 7;
  PSD_DUOTONE = 8;
  PSD_LAB = 9;
  PSD_GRAYSCALE16 = 10;      // Not in official specification
  PSD_RGB48 = 11;            // Not in official specification
  PSD_LAB48 = 12;            // Not in official specification
  PSD_CMYK64 = 13;           // Not in official specification
  PSD_DEEPMULTICHANNEL = 14; // Not in official specification
  PSD_DUOTONE16 = 15;        // Not in official specification

type
  // *.psd, *.pdd images
  TPSDLayerBlendMode = (
    lbmNormal,
    lbmDarken,
    lbmLighten,
    lbmHue,
    lbmSaturation,
    lbmColor,
    lbmLuminosity,
    lbmMultiply,
    lbmScreen,
    lbmDissolve,
    lbmOverlay,
    lbmHardLight,
    lbmSoftLight,
    lbmDifference,
    lbmExclusion,
    lbmColorDodge,
    lbmColorBurn
  );

  TPSDLayerClipping = (
    lcBase,
    lcNonBase
  );

  TPSDLayerOptions = set of (
    loTransparencyProtected,
    loVisible,      // According to current specs it's visible not hidden
    loObsolete,     // Obsolete according to specs
    loPhotoshop5,   // for Photoshop 5.0 and later, tells if bit 4 has useful information
                    // If it's on then the next bit is valid
    loPixelDataIrrelevant // If on: pixel data irrelevant to appearance of document
  );

  TPSDLayerType = (
    ltBitmap,
    ltText,
    ltMask
  );

  // Flags used for mask data in a Photoshop layer.
  TPSDLayerMaskFlags = set of (
    lmfRelativePosition,     // Position of mask is relative to layer.
    lmfMaskDisabled,         // The layer mask is disabled.
    lmfInvertMask,           // Invert layer mask when blending. (obsolete)
    lmfUserMaskRendered,     // Indicates that the user mask actually came from rendering other data
    lmfMaskWithParameters    // Indicates that the user and/or vector masks have parameters applied to them
  );
  // Flags in case lmfMaskWithParameters bit was set (see above)
  // Flags that are set below specify how many bytes are following
  TPSDMaskParameters = set of (
    mpUserMaskDensity,       // user mask density, 1 byte
    mpUserMaskFeather,       // user mask feather, 8 byte, double
    mpVectorMaskDensity,     // vector mask density, 1 byte
    mpVectorMaskFeather      // vector mask feather, 8 bytes, double
  );

  TPSDLayerMaskData = record
    Bounds: TRect;
    DefaultColor: Byte;
    Flags: TPSDLayerMaskFlags;
    UserMaskBackground: Byte;
    MaskParameters: TPSDMaskParameters;
  end;

  // Currently no info is available for data in this block.
  TPSDCompositeGrayBlend = record
    Black1,
    Black2,
    White1,
    White2: Byte;
  end;

  // Data specific to one channel in a layer.
  // Pixel data is not stored separately for each channel but the layer as a whole.
  TPSDChannel = record
    ChannelID: SmallInt;
    Size: Cardinal;               // Size of channel data when loading or storing.
    BlendSourceRange,
    BlendTargetRange: TPSDCompositeGrayBlend;
    Data: Pointer;                // Temporary storage for the channel's pixel data.
  end;

  // Each layer has a collection of channel data.
  TPSDChannels = array of TPSDChannel;

  // Indirect type declaration here to allow recursive item data structure.
  PPSDItemList = ^TPSDItemList;
  PPSDDescriptor = ^TPSDDescriptor;

  TPSDItemData = record
    ItemType: Cardinal;           // Type of the item. See ReadDescriptor for a list of possible values.
    ClassID: WideString;          // Only valid if property or class item.
    KeyID: WideString;            // Only valid if property or string item.
    Name: WideString;             // Only valid if name or identifier item.
    Units: Integer;               // Only valid if Unit float item.
    Value: Double;                // Only valid if Unit float or double item.
    TypeID: WideString;           // Only valid if enumeration item.
    EnumValue: WideString;        // Only valid if enumeration item.
    Offset: Cardinal;             // Only valid if offset item.
    IntValue: Integer;            // Only valid if integer item.
    BoolValue: Boolean;           // Only valid if boolean item.
    List: PPSDItemList;           // Only valid if (reference) list or item.
    DataSize: Cardinal;           // Only valid if raw data.
    Data: Pointer;                // Only valid if raw data.
    Descriptor: PPSDDescriptor;   // Only valid if the item is again a PSD descriptor.
  end;
  TPSDItemList = array of TPSDItemData;

  // One entry in a PSD descriptor stored as part of e.g. the type tool adjustment layer.
  TPSDDescriptorItem = record
    Key: AnsiString;                  // Item name.
    Data: TPSDItemData;           // The value of the item.
  end;

  TPSDDescriptor = record
    ClassID,
    ClassID2: WideString;
    Items: array of TPSDDescriptorItem;
  end;

  TDoubleRect = record
    Left, Top, Right, Bottom: Double;
  end;

  TTypeTransform = record
    XX, XY, YX, YY, TX, TY: Double;
  end;

  TPSDTypeToolInfo = record
    Transform: TTypeTransform;
    TextDescriptor,
    WarpDescriptor: TPSDDescriptor;
    WarpRectangle: TDoubleRect;
  end;

  TPSDGraphic = class;

  TPhotoshopLayer = class
  private
    FGraphic: TPSDGraphic;
    FBounds: TRect;
    FBlendMode: TPSDLayerBlendMode;
    FOpacity: Byte;                    // 0 = transparent ... 255 = opaque
    FClipping: TPSDLayerClipping;
    FOptions: TPSDLayerOptions;
    FMaskData: TPSDLayerMaskData;
    FCompositeGrayBlendSource,
    FCompositeGrayBlendDestination: TPSDCompositeGrayBlend;
    FChannels: TPSDChannels;
    FName: WideString;
    FImage: TBitmap;
    FType: TPSDLayerType;
    FTypeToolInfo: TPSDTypeToolInfo;   // Only valid if layer is a text layer.
    procedure SetImage(const Value: TBitmap);
  public
    constructor Create(Graphic: TPSDGraphic);
    destructor Destroy; override;

    property BlendMode: TPSDLayerBlendMode read FBlendMode write FBlendMode;
    property Bounds: TRect read FBounds write FBounds;
    property Channels: TPSDChannels read FChannels write FChannels;
    property Clipping: TPSDLayerClipping read FClipping write FClipping;
    property CompositeGrayBlendDestination: TPSDCompositeGrayBlend read FCompositeGrayBlendDestination
      write FCompositeGrayBlendDestination;
    property CompositeGrayBlendSource: TPSDCompositeGrayBlend read FCompositeGrayBlendSource
      write FCompositeGrayBlendSource;
    property Image: TBitmap read FImage write SetImage;
    property LayerType: TPSDLayerType read FType;
    property MaskData: TPSDLayerMaskData read FMaskData write FMaskData;
    property Name: WideString read FName write FName;
    property Opacity: Byte read FOpacity write FOpacity;
    property Options: TPSDLayerOptions read FOptions write FOptions;
  end;

  TPhotoshopLayers = class(TList)
  private
    FGraphic: TPSDGraphic;
    // The following fields are read from the global layer mask info data but
    // their meaning is not well documented or at least obvious from their names.
    FOverlayColorSpace: Word;               // undocumented
    FColorComponents: array[0..3] of Word;  // undocumented
    FLayerMaskOpacity: Word;                // 0 = transparent, 100 = opaque
    FKind: Byte;                            // 0 = Color selected, 1 = Color protected, 128 = use value stored per layer.
                                            // The last one is preferred. The others are for backward compatibility.
  protected
    function Get(Index: Integer): TPhotoshopLayer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Put(Index: Integer; Layer: TPhotoshopLayer);
  public
    constructor Create(Graphic: TPSDGraphic);

    function Add(Layer: TPhotoshopLayer): Integer;
    function AddNewLayer: TPhotoshopLayer;
    function Extract(Layer: TPhotoshopLayer): TPhotoshopLayer;
    function First: TPhotoshopLayer;
    function IndexOf(Layer: TPhotoshopLayer): Integer;
    procedure Insert(Index: Integer; Layer: TPhotoshopLayer);
    function Last: TPhotoshopLayer;
    function Remove(Layer: TPhotoshopLayer): Integer;

    property Items[Index: Integer]: TPhotoshopLayer read Get write Put; default;
  end;

  TPSDGuide = record
    Location: Single;        // Either X or Y coordinate of the guide depending on IsHorizontal.
    IsHorizontal: Boolean;   // True if it is a horizontal guide, otherwise False.
  end;

  TPSDGridSettings = record
    HorizontalCycle,         // Number of dots per cycle relative to 72 dpi.
    VerticalCycle: Single;
    Guides: array of TPSDGuide;
  end;

  TPSDGraphic = class(TGraphicExGraphic)
  private
    FChannels,     // Original channel count of the image (1..56).
    FMode: Word;   // Original color mode of the image (PSD_*).
    FLayerCount: Cardinal; // ReadImageProperties doesn't read all layers so we can't use FLayers.Count.
    FMergedTransparencyPresent: Boolean; // If True: first alpha channel contains the transparency data for the merged result.
    FLayers: TPhotoshopLayers;
    FGridSettings: TPSDGridSettings;
    FICCUntagged: Boolean; // True if ICC profile is intentionally untagged (disabled).
  protected
    procedure CombineChannels(Layer: TPhotoshopLayer);
    function ConvertCompression(Value: Word): TCompressionType;
    function DetermineColorScheme(ChannelCount: Integer): TColorScheme;
    procedure LoadAdjustmentLayer(var Run: PByte; Layer: TPhotoshopLayer);
    procedure ReadChannelData(var Run: PByte; var Channel: TPSDChannel; AWidth, AHeight: Integer; IsIrrelevant: Boolean);
    procedure ReadDescriptor(var Run: PByte; var Descriptor: TPSDDescriptor);
    procedure ReadMergedImage(var Source: PByte; Layer: TPhotoshopLayer; Compression: TCompressionType; Channels: Byte);
    procedure ReadLayers(Run: PByte);
    procedure ReadResources(Run: PByte);
    function SetupColorManager(Channels: Integer): TPixelFormat;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property GridSettings: TPSDGridSettings read FGridSettings;
    property Layers: TPhotoshopLayers read FLayers;
    property ChannelCount: Word read FChannels;
    property Mode: Word read FMode;
    property LayerCount: Cardinal read FLayerCount;
    property MergedTransparencyPresent: Boolean read FMergedTransparencyPresent;
    property ICCUntagged: Boolean read FICCUntagged;
  end;
  {$endif PhotoshopGraphic}

  {$ifdef PaintshopProGraphic}
  // *.psp images (file version 3 and 4)
  TPSPGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif PaintshopProGraphic}

  {$ifdef PortableNetworkGraphic}
  // *.png images
  TChunkType = array [0..3] of AnsiChar;

  // This header is followed by a variable number of data bytes, which are followed by the CRC for this data.
  // The actual size of this data is given by field length in the chunk header.
  // CRC is Cardinal (4 byte unsigned integer).
  TPNGChunkHeader = packed record
    Length: Cardinal;  // size of data (entire chunk excluding itself, CRC and type)
    case Integer of
      0: (ChunkType: TChunkType);
      1: (ChunkMask: DWORD);
  end;

  TPNGGraphic = class(TGraphicExGraphic)
  private
    FIDATSize: Integer;        // remaining bytes in the current IDAT chunk
    FRawBuffer,                // buffer to load raw chunk data and to check CRC
    FCurrentSource: Pointer;   // points into FRawBuffer for current position of decoding
    FHeader: TPNGChunkHeader;  // header of the current chunk
    FCurrentCRC: Cardinal;     // running CRC for the current chunk
    FSourceBPP: Integer;       // bits per pixel used in the file
    FPalette: HPALETTE;        // used to hold the palette handle until we can set it finally after the pixel format
                               // has been set too (as this destroys the current palette)
    FTransparency: PByteArray; // If the image is indexed then this array might contain alpha values (depends on file)
                               // each entry corresponding to the same palette index as the index in this array.
                               // For grayscale and RGB images FTransparentColor contains the (only) transparent
                               // color.
    FTransparentColor: TColor; // transparent color for gray and RGB
    FBackgroundColor: TColor;  // index or color ref
    FEOF: Pointer;             // End of File in memory buffer: position AFTER the last byte in the file
    procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    function IsChunk(ChunkType: TChunkType): Boolean;
    function LoadAndSwapHeader(var Source: PByte): Cardinal;
    procedure LoadBackgroundColor(var Source: PByte; const Description);
    procedure LoadIDAT(var Source: PByte; const Description);
    procedure LoadText(var Source: PByte);
    procedure LoadTransparency(var Source: PByte; const Description);
    procedure LoadICCProfile(var Source: PByte);
    procedure DecompressToBuffer(Source: PByte; CompressedSize: Cardinal;
      out DecompressBuf: PByte; out DecompressedSize: Cardinal);
    procedure ReadDataAndCheckCRC(var Source: PByte);
    procedure ReadRow(var Source: PByte; RowBuffer: Pointer; BytesPerRow: Integer);
    function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
    procedure ValidateMemoryPosition(const CurPos: Pointer; const AOffset: Cardinal);
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;

    property BackgroundColor: TColor read FBackgroundColor;
  end;
  {$endif PortableNetworkGraphic}

  {$ifdef ArtsAndLettersGraphic}
  // *.ged images (Arts & Letters images)
  TGEDGraphic = class(TGraphicExGraphic)
  public
    class function CanLoad(const Memory: Pointer; Size: Int64): Boolean; override;
    procedure LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); override;
    function ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean; override;
  end;
  {$endif ArtsAndLettersGraphic}

  // ---------- file format management stuff
  TFormatType = (
    ftAnimation,   // format contains an animation (like GIF or AVI)
    ftLayered,     // format supports multiple layers (like PSP, PSD)
    ftMultiImage,  // format can contain more than one image (like TIF or GIF)
    ftRaster,      // format is contains raster data (this is mainly used)
    ftVector       // format contains vector data (like DXF or PSP file version 4)
  );
  TFormatTypes = set of TFormatType;

  TFilterSortType = (
    fstNone,        // do not sort entries, list them as they are registered
    fstBoth,        // sort entries first by description then by extension
    fstDescription, // sort entries by description only
    fstExtension    // sort entries by extension only
  );

  TFilterOption = (
    foCompact,          // use the compact form in filter strings instead listing each extension on a separate line
    foIncludeAll,       // include the 'All image files' filter string
    foIncludeExtension  // add the extension to the description
  );
  TFilterOptions = set of TFilterOption;

  // The file format list is an alternative to Delphi's own poor implementation which does neither allow to filter
  // graphic formats nor to build common entries in filter strings nor does it care for duplicate entries or
  // alphabetic ordering. Additionally, some properties are maintained for each format to do searches, filter particular
  // formats for a certain case etc.
  TFileFormatList = class
  private
    FClassList,
    FExtensionList: TList;
  protected
    function FindExtension(const Extension: string): Integer;
    function FindGraphicClass(GraphicClass: TGraphicClass): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetDescription(Graphic: TGraphicClass): string;
    procedure GetExtensionList(List: TStrings);
    function GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType; Options: TFilterOptions;
      GraphicClass: TGraphicClass): string;
    function GraphicFromExtension(S: string): TGraphicClass;
    function GraphicFromContent(const FileName: string): TGraphicExGraphicClass; overload;
    function GraphicFromContent(const Memory: Pointer; Size: Int64): TGraphicExGraphicClass; overload;
    function GraphicFromContent(Stream: TStream): TGraphicExGraphicClass; overload;
    procedure RegisterFileFormat(const Extension, Common, Individual: string; FormatTypes: TFormatTypes;
      Replace: Boolean; GraphicClass: TGraphicClass);
    procedure UnregisterFileFormat(const Extension: string; GraphicClass: TGraphicClass);
  end;

procedure GraphicExError(ErrorString: string); overload;
procedure GraphicExError(ErrorString: string; Args: array of const); overload;

function ReadImageProperties(const FileName: string; var Properties: TImageProperties): Boolean;

var
  FileFormatList: TFileFormatList;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  {$IFNDEF FPC}Consts,{$ENDIF}
  Math, ZLibDelphi,
  {$IFDEF PaintshopProGraphic}
  {$IFDEF USE_GEXJPEG}
  // PSP images can have a JPEG encoded composite image. We don't want to pull in
  // gexJpeg unnecessarily so we only will use it when that unit is already being used.
  // If it is not used we will try to use the layers to merge the image from.
  // Note pulling this in here is not ideal since it will cause a circular
  // reference that may cause problems.
  // TODO: Move PSP image handling to a separate unit.
  // 2017-01-28 Disabled since it causes initialization of gexJpeg to be called before
  // initialization of GraphicEx meaning FileFormatList is still nil when gexJpeg is
  // initialized meaning it can't register its image format.
  {$UNDEF USE_GEXJPEG}
  //gexJpeg,
  {$ENDIF}
  {$ENDIF}
  gexTypes, gexVersion, gexUtils;

type
  {$ifndef COMPILER_6_UP}
  {$IFNDEF FPC}
  PCardinal = ^Cardinal;
  {$ENDIF}
  {$endif COMPILER_6_UP}

  // An entry of the progress stack for nested progress sections.
  PProgressSection = ^TProgressSection;
  TProgressSection = record
    Position,                     // Current position in percent.
    ParentSize,                   // Size of this section in the context of the parent section (in %).
    TransformFactor: Single;      // Accumulated factor to transform a step in this section to an overall value.
    Message: string;              // Message to display for this section.
  end;

//----------------------------------------------------------------------------------------------------------------------

{$ifndef COMPILER_6_UP}
{$IFNDEF FPC}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}
{$endif}

//------------------------------------------------------------------------------

// For "at ReturnAddress" syntax see: http://stackoverflow.com/questions/8950513/what-does-at-returnaddress-mean-in-delphi
// Apparently Fpc doesn't have ReturnAddress, see: http://www.freepascal.org/docs-html/ref/refse101.html
procedure GraphicExError(ErrorString: string); overload;
begin
  {$IFNDEF FPC}
  raise EgexInvalidGraphic.Create(ErrorString) at ReturnAddress;
  {$ELSE}
  raise EgexInvalidGraphic.Create(ErrorString) at get_caller_addr(get_frame), get_caller_frame(get_frame);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GraphicExError(ErrorString: string; Args: array of const); overload;
begin
  {$IFNDEF FPC}
  raise EgexInvalidGraphic.CreateFmt(ErrorString, Args) at ReturnAddress;
  {$ELSE}
  raise EgexInvalidGraphic.CreateFmt(ErrorString, Args) at get_caller_addr(get_frame), get_caller_frame(get_frame);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Upsample(Width, Height, ScaledWidth: Cardinal; Pixels: PAnsiChar);

// Creates a new image that is a integral size greater than an existing one.

var
  X, Y: Cardinal;
  P, Q, R: PAnsiChar;

begin
  for Y := 0 to Height - 1 do
  begin
    P := Pixels + (Height - 1 - Y) * ScaledWidth + (Width - 1);
    Q := Pixels + ((Height - 1 - Y) shl 1) * ScaledWidth + ((Width - 1) shl 1);
    Q^ := P^;
    (Q + 1)^ := P^;
    for X := 1 to Width - 1 do
    begin
      Dec(P);
      Dec(Q, 2);
      Q^ := P^;
      (Q + 1)^ := AnsiChar((Word(P^) + Word((P + 1)^) + 1) shr 1);
    end;
  end;

  for Y := 0 to Height - 2 do
  begin
    P := Pixels + (Y shl 1) * ScaledWidth;
    Q := P + ScaledWidth;
    R := Q + ScaledWidth;
    for X := 0 to Width - 2 do
    begin
      Q^ := AnsiChar((Word(P^) + Word(R^) + 1) shr 1);
      (Q + 1)^ := AnsiChar((Word(P^) + Word((P + 2)^) + Word(R^) + Word((R + 2)^) + 2) shr 2);
      Inc(Q, 2);
      Inc(P, 2);
      Inc(R, 2);
    end;
    Q^ := AnsiChar((Word(P^) + Word(R^) + 1) shr 1);
    Inc(P);
    Inc(Q);
    Q^ := AnsiChar((Word(P^) + Word(R^) + 1) shr 1);
  end;
  P := Pixels + (2 * Height - 2) * ScaledWidth;
  Q := Pixels + (2 * Height - 1) * ScaledWidth;
  Move(P^, Q^, 2 * Width);
end;


function ReadImageProperties(const FileName: string; var Properties: TImageProperties): Boolean;

// Reads the properties of an image given by FileName. It just simplifies to find a proper loader class.
// Reading image properties is a light weight task. Only a small part of the image must be accessed.
// It is not loaded into memory.
// True is returned if the properties could be read. False appears in case on a problem (e.g. read error). 

var
  Extension: string;
  GraphicClass: TGraphicClass;
  NewGraphic: TGraphic;

begin
  Result := False;

  try
    Extension := ExtractFileExt(FileName);
    GraphicClass := FileFormatList.GraphicFromExtension(Extension);

    if (GraphicClass <> nil) and (GraphicClass.ClassParent = TGraphicExGraphic) then
    begin
      NewGraphic := GraphicClass.Create;
      try
        with TGraphicExGraphic(NewGraphic) do
        begin
          ReadImageProperties(FileName, 0);
          Properties := ImageProperties;
          Result := True;
        end;
      finally
        NewGraphic.Free;
      end;
    end;
  except
    // Silent exception, we return False for any error.
  end;
end;

//----------------- TFileMapping ---------------------------------------------------------------------------------------

constructor TFileMapping.Create(const FileName: string; Mode: TFileMappingMode);

var
  AccessFlags,
  CreationFlag: Cardinal;
  SizeLow,
  SizeHigh: Cardinal;

begin
  FFileName := FileName;

  AccessFlags := GENERIC_READ;
  if Mode <> fmmReadOnly then
    AccessFlags := AccessFlags or GENERIC_WRITE;
  case Mode of
    fmmCreateNew:
      CreationFlag := CREATE_ALWAYS;
    fmmOpenOrCreate:
      CreationFlag := OPEN_ALWAYS;
  else
    // fmmReadOnly, fmmReadWrite
    CreationFlag := OPEN_EXISTING;
  end;
  FFileHandle := CreateFile(PChar(FileName), AccessFLags, FILE_SHARE_READ, nil, CreationFlag, FILE_ATTRIBUTE_NORMAL
    or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError
  else
  begin
    SizeLow := GetFileSize(FFileHandle, @SizeHigh);
    FFileSize := Int64(SizeHigh) shl 32 + SizeLow;
    if FFileSize = 0 then
      Exit; // Empty file should not give an error here. We will handle it in our graphics type detection
    FFileMapping := CreateFileMapping(FFileHandle, nil, PAGE_READONLY	, 0, 0, nil);
    if FFileMapping = 0 then
      RaiseLastOSError;
    FMemory := MapViewOfFile(FFileMapping, FILE_MAP_READ, 0, 0, 0);
    if FMemory = nil then
      RaiseLastOSError;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TFileMapping.Create(Stream: THandleStream);

// Alternative constructor to create the mapping from a handle stream which is usually a wrapper for a normal file.
// NOTE: you must not change the file content using the stream as long as the mapping exists otherwise inconsitencies
//       will appear! However you can write into the stream using the memory pointer from the mapping.

var
  SizeLow,
  SizeHigh: Cardinal;

begin
  // Set the file handle to invalid so it does not get freed in the destructor.
  FFileHandle := INVALID_HANDLE_VALUE;

  SizeLow := GetFileSize(Stream.Handle, @SizeHigh);
  FFileSize := Int64(SizeHigh) shl 32 + SizeLow;
    if FFileSize = 0 then
      Exit; // Empty file should not give an error here. We will handle it in our graphics type detection
  FFileMapping := CreateFileMapping(Stream.Handle, nil, PAGE_READONLY, 0, 0, nil);
  if FFileMapping = 0 then
    RaiseLastOSError;
  FMemory := MapViewOfFile(FFileMapping, FILE_MAP_READ, 0, 0, 0);
  if FMemory = nil then
    RaiseLastOSError;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileMapping.Destroy;

begin
  if Assigned(FMemory) then
    UnmapViewOfFile(Memory);
  if FFileMapping <> 0 then
    CloseHandle(FFileMapping);
  if FFileHandle <> INVALID_HANDLE_VALUE then
    FileClose(FFileHandle);

  inherited;
end;

//----------------- TGraphicExGraphic ----------------------------------------------------------------------------------

constructor TGraphicExGraphic.Create;

begin
  inherited;
  FColorManager := TColorManager.Create;
  Decoder := nil;
  {$IFDEF LCMS}
  {$IFDEF LCMS_CONVERSION}
  FICCTransformEnabled := True;
  {$ELSE}
  FICCTransformEnabled := False;
  {$ENDIF}
  {$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGraphicExGraphic.Destroy;

begin
  ClearProgressStack;
  {$IFDEF LCMS}
  FICCManager.Free;
  {$ENDIF}
  FColorManager.Free;
  Decoder.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

// Since loading an image involves often a lot of processing it is diffcult to provide the user with
// usefull progress information. This is mainly due to the impossibility to tell in advance how
// much overall percent a particular part needs and/or has finished.
// TGraphicExGraphic implements an advanced management which takes socalled sections as base interval.
// A section is the amount of percents a process will take up in the whole range of 0..100% relative to its
// "parent section".
// Stepping up the progress always means here to count "locally" (i.e. in the current section).
// This way a particular process can always step from 0 to 100% and the steps are automatically transformed to an
// overall value depending on the section sizes.
//

procedure TGraphicExGraphic.AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);

// Steps the current progress section up by Amount percent (0..100%).
// The meaning of the parameters in the method is:
//   Amount   - Value which is used to increase the section's current progress position (0..100%)
//   OffsetX,
//   OffsetY  - Values to offset the progress rectangle with
//   DoRedraw - Tells the application to update its display.

var
  CurrentSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');

  // Advance the top section.
  CurrentSection := FProgressStack.Peek;
  Amount := Amount / 100;
  // Ensure that we never exceed the 100% limit.
  if CurrentSection.Position + Amount > 1 then
  begin
    Amount := 1 - CurrentSection.Position;
    CurrentSection.Position := 1;
  end
  else
    CurrentSection.Position := CurrentSection.Position + Amount;

  // Sum up the section's percents under consideration of the section size.
  FPercentDone := FPercentDone + CurrentSection.TransformFactor * Amount;
  OffsetRect(FProgressRect, OffsetX, OffsetY);
  Progress(Self, psRunning, Round(100 * FPercentDone), DoRedraw, FProgressRect, CurrentSection.Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.ClearProgressStack;

// Empties the current progress stack and frees it afterwards.

var
  CurrentSection: PProgressSection;

begin
  if Assigned(FProgressStack) then
  begin
    while FProgressStack.Count > 0 do
    begin
      CurrentSection := FProgressStack.Pop;
      Dispose(CurrentSection);
    end;
    FreeAndNil(FProgressStack);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.FinishProgressSection(DoRedraw: Boolean);

// Finishes the current section and removes it from the progress stack.
// The parent section is updated assuming this section has exactly used 100% (regardless of the actual amount).

var
  Percent: Single;
  CurrentSection,
  ParentSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');

  CurrentSection := FProgressStack.Pop;
  if FProgressStack.Count = 0 then
    FreeAndNil(FProgressStack)
  else
  begin
    // Update position of the parent section.
    ParentSection := FProgressStack.Peek;
    if ParentSection.Position + CurrentSection.ParentSize > 1 then
      ParentSection.Position := 1
    else
      ParentSection.Position :=  ParentSection.Position + CurrentSection.ParentSize;
  end;

  // Update the overall percent value.
  Percent := 1 - CurrentSection.Position;
  if Percent > 0 then
    FPercentDone := FPercentDone + CurrentSection.TransformFactor * Percent;
  Dispose(CurrentSection);

  if FProgressStack = nil then
    Progress(Self, psEnding, Round(100 * FPercentDone), DoRedraw, FProgressRect, '')
  else
    Progress(Self, psRunning, Round(100 * FPercentDone), DoRedraw, FProgressRect, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.InitProgress(AWidth, AHeight: Integer);

// Initializes all progress related variables.

begin
  ClearProgressStack;
  FProgressStack := TStack.Create;

  FProgressRect := Rect(0, 0, AWidth, AHeight);
  FPercentDone := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.StartProgressSection(Size: Single; const S: string);

// Starts a new progress section within the current section.
// Size determines the amount the new section will take up in the current section and must be given in
// percent (0..100%). If Size is 0 then the full rest of the current section is taken.
// S is the message string to use for the progress event.

var
  CurrentSection,
  NewSection: PProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');

  New(NewSection);
  if FProgressStack.Count = 0 then
  begin
    // This is the first (root) section.
    NewSection.ParentSize := 1;
    NewSection.TransformFactor := 1;
  end
  else
  begin
    CurrentSection := FProgressStack.Peek;
    if Size = 0 then
      NewSection.ParentSize := 1 - CurrentSection.Position
    else
      NewSection.ParentSize := Size / 100;
    NewSection.TransformFactor := CurrentSection.TransformFactor * NewSection.ParentSize;
  end;

  NewSection.Position := 0;
  NewSection.Message := S;

  FProgressStack.Push(NewSection);
  if FProgressStack.Count = 1 then
    Progress(Self, psStarting, Round(100 * FPercentDone), False, FProgressRect, S)
  else
    Progress(Self, psRunning, Round(100 * FPercentDone), False, FProgressRect, S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.Assign(Source: TPersistent);

begin
  if Source is TGraphicExGraphic then
    FImageProperties := TGraphicExGraphic(Source).FImageProperties;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(const FileName: string): Boolean;

var
  Stream: TFileStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CanLoad(Stream);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGraphicExGraphic.CanLoad(Stream: TStream): Boolean;

begin
  // We can optimize load operations by using direct memory access if possible.
  // For file streams a file mapping is created, memory streams can directly be accessed and
  // other streams (e.g. blob streams) are converted into a memory stream first.
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := CanLoad(Memory, Size);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := CanLoad(Memory, Size);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := CanLoad(Memory, Size);
      finally
        Free;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromFile(const FileName: string);

begin
  LoadFromFileByIndex(FileName, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);

begin
  // Create a file mapping for the file to access the data without intermediate buffering.
  with TFileMapping.Create(FileName, fmmReadOnly) do
  try
    LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

begin
  FreeAndNil(Decoder);
  Handle := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceID(Instance: THandle; ResID: Integer; ImageIndex: Cardinal = 0);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_RCDATA);
  try
    // Resource streams are memory streams, so we can directly access their data.
    with Stream do
      LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromResourceName(Instance: THandle; const ResName: string; ImageIndex: Cardinal = 0);

var
  Stream: TResourceStream;

begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    // Resource streams are memory streams, so we can directly access their data.
    with Stream do
      LoadFromMemory(Memory, Size, ImageIndex);
  finally
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromStream(Stream: TStream);

begin
  LoadFromStreamByIndex(Stream, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGraphicExGraphic.LoadFromStreamByIndex(Stream: TStream; ImageIndex: Cardinal = 0);

begin
  // We can optimize load operations by using direct memory access if possible.
  // For file streams a file mapping is created, memory streams can directly be accessed and
  // other streams (e.g. blob streams) are converted into a memory stream first.
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      LoadFromMemory(Memory, Size, ImageIndex);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        LoadFromMemory(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        LoadFromMemory(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

// We need access to the original Bitmap file/stream loading routines for our
// bmp wrapper class.
// Since I don't know a better way to access them I add loading routines here
// that access the inherited LoadFromFile/LoadFromStream.

procedure TGraphicExGraphic.LoadBitmapFromFile(const FileName: string);
begin
  inherited LoadFromFile(FileName);
end;

procedure TGraphicExGraphic.LoadBitmapFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(const Name: string; ImageIndex: Cardinal): Boolean;

begin
  // Create a file mapping for the file to access the data without intermediate buffering.
  with TFileMapping.Create(Name, fmmReadOnly) do
  try
    Result := ReadImageProperties(Memory, Size, ImageIndex);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// This is the overloaded variant for streams.

var
  LastPos: Int64;

begin
  LastPos := Stream.Position;
  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := ReadImageProperties(Memory, Size, ImageIndex);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := ReadImageProperties(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := ReadImageProperties(Memory, Size, ImageIndex);
      finally
        Free;
      end;
    end;

  Stream.Position := LastPos;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGraphicExGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

// Initializes the internal image properties structure.
// Descentants must override this method to fill in the actual values.

begin
  ZeroMemory(@FImageProperties, SizeOf(FImageProperties));
  Result := True;
end;

//----------------- TAutodeskGraphic -----------------------------------------------------------------------------------

{$ifdef AutodeskGraphic}

type
  PAutodeskHeader = ^TAutodeskHeader;
  TAutodeskHeader = packed record
    Width,
    Height,
    XCoord,
    YCoord: Word;
    Depth,
    Compression: Byte;
    DataSize: Cardinal;
    Reserved: array[0..15] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TAutodeskGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  Run: PByte;

begin
  Run := Memory;
  Result := Size > SizeOf(Word) + SizeOf(TAutodeskHeader);
  if Result then
  begin
    // Check file ID.
    Result := PWord(Run)^ = $9119;
    if Result then
    begin
      // Read image dimensions.
      Inc(Run, SizeOf(Word));
      with PAutodeskHeader(Run)^ do
        Result := (Depth = 8) and (Compression = 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAutodeskGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PAnsiChar;
  LogPalette: TMaxLogPalette;
  I: Integer;

begin
  inherited;

  Run := Memory;
  
  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

    // Skip file ID and header.
    Inc(Run, 2 + SizeOf(TAutodeskHeader));

    // Read palette entries and create a palette.
    ZeroMemory(@LogPalette, SizeOf(LogPalette));
    LogPalette.palVersion := $300;
    LogPalette.palNumEntries := 256;
    for I := 0 to 255 do
    begin
      with PPaletteEntry(Run)^ do
      begin
        LogPalette.palPalEntry[I].peBlue := Byte(peBlue shl 2);
        LogPalette.palPalEntry[I].peGreen := Byte(peGreen shl 2);
        LogPalette.palPalEntry[I].peRed := Byte(peRed shl 2);
      end;
      Inc(Run, 3);
    end;

    // Setup bitmap properties.
    {$IFNDEF FPC}
    PixelFormat := pf8Bit;
    Palette := CreatePalette(PLogPalette(@LogPalette)^);
    {$ELSE}
    PixelFormat := pf24Bit;
    Palette := CreatePalette(PLogPalette(@LogPalette)^);
    ColorManager.SetSourcePalette([@LogPalette.palPalEntry], pfInterlaced8Quad);
    ColorManager.SourceBitsPerSample := 8;
    ColorManager.SourceSamplesPerPixel := 1;
    ColorManager.SourceColorScheme := csIndexed;
    ColorManager.TargetBitsPerSample := 8;
    ColorManager.TargetSamplesPerPixel := 3;
    ColorManager.TargetColorScheme := csBGR;
    {$ENDIF}
    Width := FImageProperties.Width;
    Height := FImageProperties.Height;
    // Finally read image data.
    for I := 0 to Height - 1 do
    begin
      {$IFNDEF FPC}
      Move(Run^, Scanline[I]^, Width);
      {$ELSE}
      ColorManager.ConvertRow([Run], Scanline[I], Width, $ff);
      {$ENDIF}
      Inc(Run, Width);

      Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
      OffsetRect(FProgressRect, 0, 1);
    end;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end
  else
    GraphicExError(gesInvalidImage, ['Autodesk']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAutodeskGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PAnsiChar;
  Header: PAutodeskHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);
  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      // Skip file ID. This has been check in the inherited call.
      Header := Pointer(Run + 2);
      ColorScheme := csIndexed;
      Width := Header.Width;
      Height := Header.Height;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := 8;
      Compression := ctNone;
    end;
end;

{$endif AutodeskGraphic}

//----------------- TSGIGraphic ----------------------------------------------------------------------------------------

{$ifdef SGIGraphic}

const
  SGIMagic = 474;

  SGI_COMPRESSION_VERBATIM = 0;
  SGI_COMPRESSION_RLE = 1;

type
  PSGIHeader = ^TSGIHeader;
  TSGIHeader = packed record
    Magic: SmallInt;         // IRIS image file magic number
    Storage,                 // Storage format
    BPC: Byte;               // Number of bytes per pixel channel (1 or 2)
    Dimension: Word;         // Number of dimensions
                             //   1 - one single scanline (and one channel) of length XSize
                             //   2 - two dimensional (one channel) of size XSize x YSize
                             //   3 - three dimensional (ZSize channels) of size XSize x YSize
    XSize,                   // width of image
    YSize,                   // height of image
    ZSize: Word;             // number of channels/planes in image (3 for RGB, 4 for RGBA etc.)
    PixMin,                  // Minimum pixel value
    PixMax: Cardinal;        // Maximum pixel value
    Dummy: Cardinal;         // ignored
    ImageName: array[0..79] of AnsiChar;
    ColorMap: Integer;       // Colormap ID
                             //  0 - default, almost all images are stored with this flag
                             //  1 - dithered, only one channel of data (pixels are packed), obsolete
                             //  2 - screen (palette) image, obsolete
                             //  3 - no image data, palette only, not displayable
    Dummy2: array[0..403] of Byte; // ignored
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.GetComponents(const Memory: Pointer; var Red, Green, Blue, Alpha: Pointer; Row: Integer);

var
 RowWidth: Integer;
 PlaneSize: Integer;

begin
 RowWidth := Row * Width * FImageProperties.BitsPerSample div 8;
 PlaneSize := Width * Height * FImageProperties.BitsPerSample div 8;

 Red := PAnsiChar(Memory) + 512 + RowWidth;
 Green := PAnsiChar(Memory) + 512 + RowWidth + PlaneSize;
 Blue := PAnsiChar(Memory) + 512 + RowWidth + 2 * PlaneSize;
 Alpha := PAnsiChar(Memory) + 512 + RowWidth + 3 * PlaneSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.ReadAndDecode(const Memory: Pointer; Red, Green, Blue, Alpha: Pointer; Row: Integer; BPC: Cardinal);

var
  Count: Cardinal;
  Run: PAnsiChar;

begin
  if Assigned(Red) then
  begin
    Run := PAnsiChar(Memory) + FRowStart[Row + 0 * Height];
    Count := BPC * FRowSize[Row + 0 * Height];
    Decoder.Decode(Pointer(Run), Red, Count, Width);
  end;

  if Assigned(Green) then
  begin
    Run := PAnsiChar(Memory) + FRowStart[Row + 1 * Height];
    Count := BPC * FRowSize[Row + 1 * Height];
    Decoder.Decode(Pointer(Run), Green, Count, Width);
  end;

  if Assigned(Blue) then
  begin
    Run := PAnsiChar(Memory) + FRowStart[Row + 2 * Height];
    Count := BPC * FRowSize[Row + 2 * Height];
    Decoder.Decode(Pointer(Run), Blue, Count, Width);
  end;

  if Assigned(Alpha) then
  begin
    Run := PAnsiChar(Memory) + FRowStart[Row + 3 * Height];
    Count := BPC * FRowSize[Row + 3 * Height];
    Decoder.Decode(Pointer(Run), Alpha, Count, Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TSGIGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TSGIHeader);
  if Result then
    with PSGIHeader(Memory)^ do
    begin
      // There are not many unique fields which can be used for identification, so
      // we do some simple plausibility checks too.
      Result := (SwapEndian(Magic) = SGIMagic) and (BPC in [1, 2]) and (SwapEndian(Dimension) in [1..3]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSGIGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PAnsiChar;
  Y: Integer;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Header: TSGIHeader;
  Count: Cardinal;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      // Read header again. We need some additional information.
      Move(Run^, Header, SizeOf(TSGIHeader));
      Inc(Run, SizeOf(TSGIHeader));

      // SGI images are always stored in big endian style
      ColorManager.SourceOptions := [coNeedByteSwap];
      with Header do
        ColorMap := SwapEndian(ColorMap);

      if Compression = ctRLE then
      begin
        Count := Height * SamplesPerPixel;
        SetLength(FRowStart, Count);
        SetLength(FRowSize, Count);
        // Convert line starts and sizes.
        Move(Run^, Pointer(FRowStart)^, Count * SizeOf(Cardinal));
        SwapCardinalArrayEndian(PCardinal(FRowStart), Count);
        Move(Run^, Pointer(FRowSize)^, Count * SizeOf(Cardinal));
        SwapCardinalArrayEndian(PCardinal(FRowSize), Count);
        Decoder := TSGIRLEDecoder.Create(BitsPerSample);
      end
      else
        Decoder := nil;

      // Set pixel format before size to avoid possibly large conversion operation.
      with ColorManager do
      begin
        SourceBitsPerSample := BitsPerSample;
        TargetBitsPerSample := 8;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        case ColorScheme of
          csRGBA:
            TargetColorScheme := csBGRA;
          csRGB:
            TargetColorScheme := csBGR;
        else
          {$IFNDEF FPC}
          TargetColorScheme := csIndexed;
          {$ELSE}
          SourceColorScheme := csG; // Has a handler for grayscale/indexed while csIndexed doesn't have one (yet)
          TargetColorScheme := csBGR;
          TargetSamplesPerPixel := 3
          {$ENDIF}
        end;
        PixelFormat := TargetPixelFormat;
        // Uses separate channels thus we need to set that in source options.
        // Grayscale will only be 1 channel but it's not using the ColorManger for conversion.
        ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
      end;
      Self.Width := Width;
      Self.Height := Height;

      Progress(Self, psEnding, 100, True, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      try
        Count := (BitsPerPixel div 8) * Width;
        // read lines and put them into the bitmap
        case ColorScheme of
          csRGBA:
            if Decoder = nil then
            begin
              // Uncompressed storage.
              for  Y := 0 to Height - 1 do
              begin
                GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], ScanLine[Height - Y - 1],
                  Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end
            else
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              GetMem(AlphaBuffer, Count);
              try
                for  Y := 0 to Height - 1 do
                begin
                  ReadAndDecode(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y, Header.BPC);
                  ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], ScanLine[Height - Y - 1],
                    Width, $FF);
                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              finally
                FreeMem(RedBuffer);
                FreeMem(GreenBuffer);
                FreeMem(BlueBuffer);
                FreeMem(AlphaBuffer);
              end;
            end;
          csRGB:
            if Decoder = nil then
            begin
              // Uncompressed storage.
              for  Y := 0 to Height - 1 do
              begin
                GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
                ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end
            else
            begin
              GetMem(RedBuffer, Count);
              GetMem(GreenBuffer, Count);
              GetMem(BlueBuffer, Count);
              try
                for  Y := 0 to Height - 1 do
                begin
                  ReadAndDecode(Memory, RedBuffer, GreenBuffer, BlueBuffer, nil, Y, Header.BPC);
                  ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], ScanLine[Height - Y - 1], Width, $FF);
                  Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              finally
                FreeMem(RedBuffer);
                FreeMem(GreenBuffer);
                FreeMem(BlueBuffer);
              end;
            end;
        else
          // Any other format is interpreted as being 256 gray scales.
          Palette := ColorManager.CreateGrayscalePalette(False);
          if Decoder = nil then
          begin
            // Uncompressed storage.
            for  Y := 0 to Height - 1 do
            begin
              GetComponents(Memory, RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer, Y);
              {$IFNDEF FPC}
              Move(RedBuffer^, ScanLine[Height - Y - 1]^, Width);
              {$ELSE}
              ColorManager.ConvertRow(RedBuffer, ScanLine[Height - Y - 1], Width, $FF);
              {$ENDIF}
              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end
          else
          begin
            {$IFNDEF FPC}
            for  Y := 0 to Height - 1 do
            begin
              ReadAndDecode(Memory, ScanLine[Height - Y - 1], nil, nil, nil, Y, Header.BPC);
              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
            {$ELSE}
            GetMem(RedBuffer, Count);
            try
              for  Y := 0 to Height - 1 do
              begin
                ReadAndDecode(Memory, RedBuffer, nil, nil, nil, Y, Header.BPC);
                ColorManager.ConvertRow(RedBuffer, ScanLine[Height - Y - 1], Width, $FF);
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            finally
              FreeMem(RedBuffer);
            end;
            {$ENDIF}
          end;
        end;
      finally
        Progress(Self, psEnding, 100, True, FProgressRect, '');
        FreeAndNil(Decoder);
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['sgi, bw or rgb(a)']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSGIGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TSGIHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);
  if Result then
    with FImageProperties do
    begin
      Move(Memory^, Header, SizeOf(TSGIHeader));
      if SwapEndian(Header.Magic) = SGIMagic then
      begin
        Options := [ioBigEndian];
        BitsPerSample := Header.BPC * 8;
        Width := SwapEndian(Header.XSize);
        Height := SwapEndian(Header.YSize);
        SamplesPerPixel := SwapEndian(Header.ZSize);
        case SamplesPerPixel of
          4:
            ColorScheme := csRGBA;
          3:
            ColorScheme := csRGB;
        else
          // All other is considered as being 8 bit gray scale.
          ColorScheme := csIndexed;
        end;

        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if Header.Storage = SGI_COMPRESSION_RLE then
          Compression := ctRLE
        else
          Compression := ctNone;
      end
      else
        Result := False;
    end;
end;

{$endif SGIGraphic}

//----------------- TTIFFGraphic ---------------------------------------------------------------------------------------

{$ifdef TIFFGraphic}

const TIFF_STOP_ON_ERROR_TRUE = 1;
type
  PTIFFHeader = ^TTIFFHeader;
  TTIFFHeader = packed record
    ByteOrder: Word;
    Version: Word;
    case boolean of
      False: (FirstIFD: Cardinal); // Classic TIFF
      True: (OffsetSize, Unused: Word;
             FirstIFD64: UInt64); // Big TIFF
  end;

//----------------------------------------------------------------------------------------------------------------------

// For the libtiff library we need global functions to do the data retrieval. The setup is so that the currently
// loading TIFF instance is given in the fd parameter.

function TIFFReadProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
var
  Graphic: TTIFFGraphic;
  MaxLocation: UInt64;
  UsableSize: UInt64;
begin
  Graphic := TTIFFGraphic(Fd);
  // Make sure we have a valid location (can happen with invalid or hacked tiff files)
  MaxLocation := UInt64(PAnsiChar(Graphic.FMemory) + Graphic.FSize);
  if (UInt64(Graphic.FCurrentPointer) + UInt64(Size) > MaxLocation) then begin
    if (UInt64(Graphic.FCurrentPointer) > MaxLocation) then begin
      // Current position is beyond eof
      Result := 0;
      Exit;
    end
    else // We can still read a part of the requested data
      UsableSize := MaxLocation - UInt64(Graphic.FCurrentPointer);
  end
  else
    UsableSize := Size;
  Move(Graphic.FCurrentPointer^, Buffer^, UsableSize);
  Inc(Graphic.FCurrentPointer, UsableSize);
  Result := UsableSize;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFWriteProc(Fd: thandle_t; Buffer: Pointer; Size: tmsize_t): tmsize_t; cdecl;
begin
  Result := 0; // Writing is not supported yet.
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFSeekProc(Fd: thandle_t; Off: toff_t; Whence: Integer): toff_t; cdecl;
const
  SEEK_SET = 0; // seek to an absolute position
  SEEK_CUR = 1; // seek relative to current position
  SEEK_END = 2; // seek relative to end of file
var
  Graphic: TTIFFGraphic;
begin
  Graphic := TTIFFGraphic(Fd);

  case Whence of
    SEEK_CUR:
      Inc(Graphic.FCurrentPointer, Off);
    SEEK_END:
      Graphic.FCurrentPointer := Pointer(PAnsiChar(Graphic.FMemory) + Graphic.FSize - Off);
  else
    Graphic.FCurrentPointer := Pointer(PAnsiChar(Graphic.FMemory) + Off);
  end;
  // Make sure we have a valid location (can happen with invalid or hacked tiff files)
  {$IFNDEF FPC}
  if (Graphic.FCurrentPointer >= PAnsiChar(Graphic.FMemory)+Graphic.FSize) or
  {$ELSE}
  if (Graphic.FCurrentPointer >= Graphic.FMemory+Graphic.FSize) or
  {$ENDIF}
     (UInt64(Graphic.FCurrentPointer) < UInt64(Graphic.FMemory)) then
    Result := 0
  else
    Result := UInt64(PAnsiChar(Graphic.FCurrentPointer) - PAnsiChar(Graphic.FMemory));
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFCloseProc(Fd: thandle_t): Integer; cdecl;
var
  Graphic: TTIFFGraphic;
begin
  Graphic := TTIFFGraphic(Fd);
  Graphic.FCurrentPointer := nil;
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFSizeProc(Fd: thandle_t): toff_t; cdecl;
var
  Graphic: TTIFFGraphic;
begin
  Graphic := TTIFFGraphic(Fd);
  Result := Graphic.FSize;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIFFMapProc(Fd: thandle_t; PBase: PPointer; PSize: ptoff_t): Integer; cdecl;
begin
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIFFUnmapProc(Fd: thandle_t; Base: Pointer; Size: toff_t); cdecl;
begin
end;

//----------------------------------------------------------------------------------------------------------------------

// LibTiffDelphi TIFF Error handler proc
procedure TiffError(const Module, ErrorString: AnsiString);
begin
  if Length(Module) > 0 then
    if (Length(ErrorString) > 0) and (ErrorString[1] <> ':') then
      GraphicExError(Module + ': ' + ErrorString)
    else
      GraphicExError(Module + ErrorString)
  else
    GraphicExError(ErrorString);
end;

procedure TTIFFGraphic.ReadContiguous(tif: PTIFF);

var
  Row, Y,
  RowsToRead: Integer;
  Pos: Integer;
  Buffer: Pointer;
  FromSkew: Integer;
  RowCount,
  LineSize: Integer;
  RowsPerStrip: Integer;
  RowInc: Integer;
  LineOffset: Integer;
  iPlane: Integer;
  nPlanes: Integer;
  nStripSize: Integer;
  PtrArray: array of pointer;
  BufPtr: Pointer;
  // YCbCr handling
  RowSubCount: Integer;

begin
  nStripSize := TIFFStripSize(tif);
  if ioSeparatePlanes in FImageProperties.Options then begin
    GetMem(Buffer, nStripSize * FImageProperties.SamplesPerPixel);
    nPlanes := FImageProperties.SamplesPerPixel;
    SetLength(PtrArray, FImageProperties.SamplesPerPixel);
  end
  else begin
    GetMem(Buffer, nStripSize);
    nPlanes := 1;
    SetLength(PtrArray, 1);
  end;
  with FImageProperties do
  try
    Y := SetOrientation(tif, Height);

    TIFFGetFieldDefaulted(tif, TIFFTAG_ROWSPERSTRIP, @RowsPerStrip);
    if RowsPerStrip = -1 then
      RowsPerStrip := Height;

    LineSize := TIFFRasterScanlineSize(tif);  // Take planar into account
    if (BitsPerPixel = 1) and ((Width mod 8) <> 0) then
      FromSkew := ((Width + 7) and not 7) - Width
    else
      FromSkew := 0;

    if Ord(FImageProperties.Orientation) = ORIENTATION_TOPLEFT then
      RowInc := 1
    else
      RowInc := -1;

    Row := 0;
    while Row < Height do
    begin
      RowsToRead := RowsPerStrip - Row mod RowsPerStrip;
      if Row + RowsToRead > Height then
        RowCount := Height - Row
      else
        RowCount := RowsToRead;

      // Extra handling needed for YCbCr
      RowSubCount := RowCount;
      if RowSubCount mod FVertSubSampling <> 0 then
        Inc(RowSubCount, FVertSubSampling - RowSubCount mod FVertSubSampling);

      Pos := (Row mod RowsPerStrip) * LineSize;

      if ioSeparatePlanes in Options then begin
        // Image data is arrange in separate planes: We need to read a strip
        // for each plane and thus use BitsPerSample for computing Offset/Increment.
        for iPlane := 0 to nPlanes-1 do begin
          BufPtr := PAnsiChar(Buffer)+iPlane*nStripSize;
          TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, Row, iPlane), BufPtr,
            (Row mod RowsPerStrip + RowCount) * LineSize);
          PtrArray[iPlane] := PAnsiChar(BufPtr) + Pos;
        end;
        LineOffset := Ceil(BitsPerSample * (Width + FromSkew) / 8);
      end
      else begin
        TIFFReadEncodedStrip(tif, TIFFComputeStrip(tif, Row, 0), Buffer,
          (Row mod RowsPerStrip + RowSubCount{RowCount}) * LineSize);
        PtrArray[0] := PAnsiChar(Buffer) + Pos;
        LineOffset := Ceil(BitsPerPixel * (Width + FromSkew) / 8);
      end;

      Inc(Row, RowCount);
      while RowCount > 0 do
      begin
        ColorManager.ConvertRow(PtrArray, Scanline[Y], Width, $FF);
        for iPlane := 0 to nPlanes-1 do
          Inc(PAnsiChar(PtrArray[iPlane]), LineOffset);
        Inc(Y, RowInc);
        Dec(RowCount);
      end;
      AdvanceProgress(100 * RowCount / Height, 0, 1, True);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.ReadTiled(tif: PTIFF);

var
  Column, Row, Y,
  RowsToRead: Cardinal;
  Pos: Cardinal;
  Counter: Integer;
  TileWidth, TileHeight: Cardinal;
  Buffer: Pointer;
  FromSkew: Cardinal;
  RowCount: Cardinal;
  PixelCount: Cardinal;
  Line: PAnsiChar;
  RowInc: Integer;
  ColumnOffset: Cardinal;
  TileSize: Integer;
  TileRowSize: Cardinal;
  iPlane,
  nPlanes: Integer;
  PtrArray: array of pointer;
  BufPtr: Pointer;
  TileOffset: Cardinal;

begin
  TileSize := TIFFTileSize(tif);
  if ioSeparatePlanes in FImageProperties.Options then begin
    GetMem(Buffer, TileSize * FImageProperties.SamplesPerPixel);
    nPlanes := FImageProperties.SamplesPerPixel;
    SetLength(PtrArray, FImageProperties.SamplesPerPixel);
  end
  else begin
    GetMem(Buffer, TileSize);
    nPlanes := 1;
    SetLength(PtrArray, 1);
  end;
  with FImageProperties do
  try
    TIFFGetField(tif, TIFFTAG_TILEWIDTH, @TileWidth);
    TIFFGetField(tif, TIFFTAG_TILELENGTH, @TileHeight);
    if Ord(Orientation) = ORIENTATION_TOPLEFT then
      RowInc := 1
    else
      RowInc := -1;

    TileRowSize := TIFFTileRowSize(tif);
    Row := 0;
    while Row < Cardinal(Height) do
    begin
      RowsToRead := TileHeight - (Row mod TileHeight);
      if Row + RowsToRead > Cardinal(Height) then
        RowCount := Cardinal(Height) - Row
      else
        RowCount := RowsToRead;

      Column := 0;
      while Column < Cardinal(Width) do
      begin
        Pos := (Row mod TileHeight) * TileRowSize;
        if ioSeparatePlanes in Options then begin
          // Image data is arrange in separate planes: We need to read a tile
          // for each plane and thus use BitsPerSample for computing Offset/Increment.
          for iPlane := 0 to nPlanes-1 do begin
            BufPtr := PAnsiChar(Buffer)+iPlane*TileSize;
            TIFFReadEncodedTile(tif, TIFFComputeTile(tif, Column, Row, 0, iPlane), BufPtr, TileSize);
            PtrArray[iPlane] := PAnsiChar(BufPtr) + Pos;
          end;
          TileOffset := Ceil(BitsPerSample * TileWidth / 8);
        end
        else begin
          TIFFReadEncodedTile(tif, TIFFComputeTile(tif, Column, Row, 0, 0), Buffer, TileSize);
          PtrArray[0] := PAnsiChar(Buffer) + Pos;
          TileOffset := Ceil(BitsPerPixel * TileWidth / 8);
        end;

        Y := Row;
        Counter := RowCount;
        ColumnOffset := ColorManager.TargetBitsPerSample * ColorManager.TargetSamplesPerPixel * Column div 8;
        if Column + TileWidth > Cardinal(Width) then
        begin
          // Tile is clipped horizontally.  Calculate visible portion and skewing factors.
          PixelCount := Cardinal(Width) - Column;
          FromSkew := TileWidth - PixelCount;
          if ioSeparatePlanes in Options then
            TileOffset := Ceil(BitsPerSample * (PixelCount + FromSkew) / 8)
          else
            TileOffset := Ceil(BitsPerPixel * (PixelCount + FromSkew) / 8);

          while Counter > 0 do
          begin
            Line := Scanline[Y];
            Inc(Line, ColumnOffset);
            ColorManager.ConvertRow(PtrArray, Line, PixelCount, $FF);
            for iPlane := 0 to nPlanes-1 do
              Inc(PAnsiChar(PtrArray[iPlane]), TileOffset);
            Inc(Y, RowInc);
            Dec(Counter);
          end;
        end
        else
        begin
          while Counter > 0 do
          begin
            Line := Scanline[Y];
            Inc(Line, ColumnOffset);
            ColorManager.ConvertRow(PtrArray, Line, TileWidth, $FF);
            for iPlane := 0 to nPlanes-1 do
              Inc(PAnsiChar(PtrArray[iPlane]), TileOffset);
            Inc(Y, RowInc);
            Dec(Counter);
          end;
        end;
        Inc(Column, TileWidth);
      end;

      Inc(Row, RowCount);
      AdvanceProgress(100 * RowCount / Height, 0, 1, True);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.SetOrientation(tif: PTIFF; H: Cardinal): Cardinal;

begin
  case Ord(FImageProperties.Orientation) of
    ORIENTATION_BOTRIGHT,
    ORIENTATION_RIGHTBOT,
    ORIENTATION_LEFTBOT,
    ORIENTATION_BOTLEFT:
        Result := H - 1;
  else
    // ORIENTATION_TOPRIGHT
    // ORIENTATION_RIGHTTOP
    // ORIENTATION_LEFTTOP etc.
    FImageProperties.Orientation := TgexOrientation(ORIENTATION_TOPLEFT);
    Result := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TTIFFGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  Run: PByte;
  Header: TTIFFHeader;

begin
  Run := Memory;
  Result := Size > SizeOf(TTIFFHeader);
  if Result then
  begin
    Move(Run^, Header, SizeOf(Header));
    with Header do
    begin
      Result := (ByteOrder = TIFF_BIGENDIAN) or (ByteOrder = TIFF_LITTLEENDIAN);
      if Result then
      begin
        if ByteOrder = TIFF_BIGENDIAN then
        begin
          Version := SwapEndian(Header.Version);
          if Version = TIFF_VERSION_CLASSIC then begin
            FirstIFD := SwapEndian(Header.FirstIFD);
          end
          else if Version = TIFF_VERSION_BIG then begin
            FirstIFD64 := SwapEndian(Header.FirstIFD64);
          end
        end;

        case Version of
          TIFF_VERSION_CLASSIC: Result := Int64(FirstIFD) < Size;
          TIFF_VERSION_BIG: Result := Int64(FirstIFD64) < Size;
        else
          Result := False;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTIFFGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  TIFFImage: PTIFF;
  Run: PAnsiChar;
  Count: Cardinal;
  Pixels: Pointer;
  I: Integer;
  Line: Pointer;

  {$ifndef DELPHI_6_UP}
    // Structure used to build a va_list array.
    ExtraInfo: record
      Value1: Pointer;
      Value2: Pointer;
      Value3: Pointer;
    end;
  {$endif DELPHI_6_UP}
  RedMap,
  GreenMap,
  BlueMap: PWord;
  GotPalette: Integer;
  Luma: PLuma;

begin
  inherited;
  FVertSubSampling := 1;
  FHorSubSampling := 1;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    try
      // Initialize outermost progress display.
      InitProgress(Width, 1);
      StartProgressSection(0, '');

      // Initialize sub section for image preparation. We give it a (guessed) value of 1%.
      StartProgressSection(1, gesPreparing);

      // First some checks to see if we are able to handle the image
      // Do this after InitProgress since in finally we will finalize the progress
      // and if it hasn't been initialized first it will crash.
      if Compression = ctUnknown then
        GraphicExError(gesUnsupportedCompression, ['TIFF']);
      if ColorScheme = csUnknown then
        GraphicExError(gesColorScheme, ['TIFF']);
      if (Width <= 0) or (Height <= 0) then
        GraphicExError(gesInvalidDimensions, ['TIFF', Width, Height]);

      FMemory := Memory;
      FCurrentPointer := Memory;
      FSize := Size;

      // OpenMode: r - readmode, (lowercase) m - Don't use memory mapped file
      // Since we are already using a memory mapped file ourselves it is not
      // necessary to let libtif also use a memory mapped file.
      TIFFImage := TIFFClientOpen('', 'rm', NativeUInt(Self), TIFFReadProc, TIFFWriteProc, TIFFSeekProc, TIFFCloseProc,
        TIFFSizeProc, TIFFMapProc, TIFFUnmapProc);
      try
        // The preparation part is finished. Finish also progress section (which will step the main progress).
        FinishProgressSection(False);

        if Assigned(TIFFImage) then
        begin
          TIFFSetDirectory(TIFFImage, ImageIndex);

          ColorManager.SourceColorScheme := ColorScheme;
          ColorManager.SourceDataFormat := TSampleDataFormat(SampleFormat);
          // If Tiff uses separate planes we need to add that to our source options
          if ioSeparatePlanes in Options then
            ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];

          {$DEFINE YCBCR} // Use this as long as we haven't finished handling YCbCr ourselves
          // Generic RGBA image loading. Only needed when we can't handle the
          // image format ourselves. Currently that seems to be the case
          // with the LOGLUV and YCbCr formats.
          // Since we can't read these formats anyway there's no need to test
          // for additional limits (BitsPerSample, SamplesPerPixel, ...) to
          // see whether TIFFReadRGBAImage can handle it.
          if (ColorScheme in [csCIELog2L, csCIELog2Luv{$IFDEF YCBCR}, csYCbCr{$ENDIF}]) then begin
             // Generic RGBA reading interface
            if (Height > 0) and (Width > 0) then
            begin
              // 3 or more samples per pixel are used for RGB(A), CMYK, L*a*b*, YCbCr etc.
              // All of these will be converted to RGBA.
              if HasAlpha then
                PixelFormat := pf32Bit
              else
                PixelFormat := pf24Bit;
              Self.Width := Width;
              Self.Height := Height;
              // We will always receive RGBA thus we need to reserve space for
              // Width * Height * SizeOf(RGBA)
              Count := Width * Height;
              GetMem(Pixels, Count * SizeOf(TRGBA));
              try
                StartProgressSection(70, gesLoadingData);
                if TIFFReadRGBAImage(TIFFImage, Width, Height, Pixels, TIFF_STOP_ON_ERROR_TRUE) = 1 then
                begin
                  FinishProgressSection(False);

                  StartProgressSection(30, gesTransfering);
                  Run := Pointer(Pixels);
                  for I := Height - 1 downto 0 do
                  begin
                    Line := Scanline[I];
                    if HasAlpha then
                      // Change RGBA to BGRA, 1 line at a time
                      RGBAToBGRA(Run, Width, 1)
                    else
                      RGBAToBGR(Run, Width, 1);
                    Move(Run^, Line^, Width * (3+Ord(HasAlpha)));
                    Inc(Run, Width * 4);
                    AdvanceProgress(100 / Height, 0, 1, True);
                  end;
                end;
                FinishProgressSection(False);
              finally
                FreeMem(Pixels);
              end;
            end;
          end
          else begin
            if (Width = 0) or (Height = 0) then
              // We can't show broken images where either width or height is 0.
              Exit;
            // Monochrome and indexed with 1-64 bits per pixel including floating point
            // RGB(A) 16, 32, 64 bits including floating point
            // Strip, Tiles, contiguous and planar are all supported
            ColorManager.SourceBitsPerSample := BitsPerSample;
            ColorManager.SourceSamplesPerPixel := SamplesPerPixel;

            if ColorScheme in [csG, csGA, csIndexed, csIndexedA] then begin
              // Monochrome images are handled just like indexed images (a gray scale palette is used).

              // TargetBitsPerSample needs to correspond to the TargetPixelFormat
              // or else the image will not be painted correctly.
              {$IFNDEF FPC}
              if (BitsPerSample >= 5) and (BitsPerSample <= 64) then
                ColorManager.TargetBitsPerSample := 8
              else if BitsPerSample in [2, 3, 4] then
                ColorManager.TargetBitsPerSample := 4
              else // 1 BitsPerSample, or values > 64 which we don't support and will throw an error
                ColorManager.TargetBitsPerSample := BitsPerSample;

              ColorManager.TargetSamplesPerPixel := SamplesPerPixel;
              if (SamplesPerPixel > 1) and not HasAlpha then begin
                // There are extra samples but apparently not a normal alpha channel.
                // We need to make sure these extra samples get skipped.
                ColorManager.TargetSamplesPerPixel := 1;
              end;

              if (ColorScheme = csGA) and (BitsPerSample = 8) then begin
                // Need to convert to BGRA to be able to show alpha
                ColorManager.TargetColorScheme := csBGRA;
                ColorManager.TargetSamplesPerPixel := 4;
              end
              else
                ColorManager.TargetColorScheme := csIndexed;
              {$ELSE}
              ColorManager.TargetBitsPerSample := 8;
              if HasAlpha then begin
                ColorManager.TargetSamplesPerPixel := 4;
                ColorManager.TargetColorScheme := csBGRA;
              end
              else begin
                ColorManager.TargetSamplesPerPixel := 3;
                ColorManager.TargetColorScheme := csBGR;
              end;
              {$ENDIF}
              if ioSeparatePlanes in Options then begin
                // Only possible for Grayscale or Indexed with alpha.
                ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
              end;
              if ioMinIsWhite in Options then
                ColorManager.SourceOptions := ColorManager.SourceOptions + [coMinIsWhite];
            end
            else begin
              // Assume we want BGR(A) for everything else

              // TargetBitsPerSample needs to correspond to the TargetPixelFormat
              // or else the image will not be painted correctly.
              // For BGR/RGB  we are always converting to 8 bits
              // since target 1, 4 bits would need palette handling.
              ColorManager.TargetBitsPerSample := 8;

              if HasAlpha then
                // Note that if we wanted to add alpha where the source doesn't
                // have alpha we would need to add the next line:
                // ColorManager.TargetSamplesPerPixel := 4;
                ColorManager.TargetColorScheme := csBGRA
              else
                ColorManager.TargetColorScheme := csBGR;

              case ColorScheme of
                csYCbCr:
                  begin
                    TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_YCBCRSUBSAMPLING, @FHorSubSampling, @FVertSubSampling);
                    TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_YCBCRPOSITIONING, @FYcbCrPositioning);
                    TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_YCBCRCOEFFICIENTS, @Luma);
                    // Copy luma
                    Move(Luma^,FLuma,SizeOf(TLuma));
                    ColorManager.SetYCbCrParameters([FLuma.LumaRed, FLuma.LumaGreen, FLuma.LumaBlue], FHorSubSampling, FVertSubSampling);
                    if (Compression = ctJPEG) and not (ioTiled in Options) then begin
                      // Let the Jpeg Lib do the conversion from YCbCr to RGB for us
                      TIFFSetField(TIFFImage, TIFFTAG_JPEGCOLORMODE, JPEGCOLORMODE_RGB);
                      ColorManager.SourceColorScheme := csRGB;
                    end;
                  end;
                csCIELab,
                csICCLab,
                csITULab:
                  begin
                    if SamplesPerPixel >= 3 then begin
                      ColorManager.TargetSamplesPerPixel := 3 + Ord(HasAlpha);
                      if ColorScheme = csCIELab then // Not sure about this.
                        ColorManager.SourceOptions := ColorManager.SourceOptions +
                          [coLabByteRange]
                      else if ColorScheme = csICCLab then
                        ColorManager.SourceOptions := ColorManager.SourceOptions +
                          [coLabByteRange, coLabChromaOffset]
                    end
                    else begin
                      {$IFNDEF FPC}
                      ColorManager.TargetSamplesPerPixel := 1;
                      ColorManager.TargetColorScheme := csG;
                      {$ELSE}
                      // Fpc: convert CIELAB gray to BGR and pretend that source is grayscale
                      ColorManager.SourceColorScheme := csG;
                      ColorManager.TargetSamplesPerPixel := 3;
                      {$ENDIF}
                      // The one example I have has a range from light=1 to dark= 254
                      // It has an extra TIFF tag: Halftone Hints: light 1 dark 254
                      Include(Options, ioMinIsWhite);
                      ColorManager.SourceOptions := ColorManager.SourceOptions + [coMinIsWhite];
                    end;
                  end;
                csUnknown: // Do a simple guess what color scheme it could be.
                  begin
                    if BitsPerSample in [8, 16] then
                      case SamplesPerPixel of
                        1:
                          begin
                            ColorManager.SourceColorScheme := csG;
                            ColorManager.TargetColorScheme := csG;
                          end;
                        3:
                          begin
                            ColorManager.SourceColorScheme := csRGB;
                            ColorManager.TargetColorScheme := csBGR;
                          end;
                        4:
                          begin
                            ColorManager.SourceColorScheme := csRGBA;
                            ColorManager.TargetColorScheme := csBGRA;
                          end;
                      end;
                  end;
              else
                // Tiff can have extra samples that are not normal alpha channels.
                // Catch those so we can interpret it at least partially.
                // That is the reason that we use explicit numbers here and not SamplesPerPixel.
                if HasAlpha then
                  ColorManager.TargetSamplesPerPixel := 4
                else
                  ColorManager.TargetSamplesPerPixel := 3;
              end;
            end;

            PixelFormat := ColorManager.TargetPixelFormat;
            // TIFF can handle sizes larger than Max(Integer) on 32 bits
            // We probably won't be able to handle the amount of memory needed
            // but we will limit the loading to Max(Integer) in these cases.
            if Width >= 0 then
              Self.Width := Width
            else
              Self.Width := MaxInt;
            if Height >= 0 then
              Self.Height := Height
            else
              Self.Height := MaxInt;

            if ColorScheme in [csIndexed, csIndexedA] then
            begin
              {$ifndef DELPHI_6_UP}
                ExtraInfo.Value1 := @RedMap;
                ExtraInfo.Value2 := @GreenMap;
                ExtraInfo.Value3 := @BlueMap;
                GotPalette := TIFFVGetField(TIFFImage, TIFFTAG_COLORMAP, @ExtraInfo);
              {$else}
                GotPalette := TIFFGetField(TIFFImage, TIFFTAG_COLORMAP, @RedMap, @GreenMap, @BlueMap);
              {$endif DELPHI_6_UP}

              if GotPalette > 0 then
              begin
                {$IFNDEF FPC}
                if BitsPerSample in [9..16] then begin
                {$ENDIF}
                  // Palette images with more than 8 bits per sample are converted
                  // to RGB since Windows palette can have a maximum of 8 bits (256) entries
                  // and downscaling a palette is very complicated.
                  ColorManager.SetSourcePalette([RedMap, GreenMap, Bluemap], pfPlane16Triple);
                  if not HasAlpha then begin
                    if ColorManager.TargetColorScheme <> csBGR then begin
                      // Only change if needed since changing PixelFormat might be slow
                      ColorManager.TargetColorScheme := csBGR;
                      ColorManager.TargetBitsPerSample := 8;
                      ColorManager.TargetSamplesPerPixel := 3;
                      PixelFormat := ColorManager.TargetPixelFormat;
                    end
                  end
                  else begin
                    // Extra alpha channel present
                    if ColorManager.TargetColorScheme <> csBGRA then begin
                      // Only change if needed since changing PixelFormat might be slow
                      ColorManager.TargetColorScheme := csBGRA;
                      ColorManager.TargetBitsPerSample := 8;
                      ColorManager.TargetSamplesPerPixel := 4;
                      PixelFormat := ColorManager.TargetPixelFormat;
                    end
                  end
                {$IFNDEF FPC}
                end
                else
                  // Create the palette from the three maps.
                  Palette := ColorManager.CreateColorPalette([RedMap, GreenMap, Bluemap], pfPlane16Triple, 1 shl BitsPerPixel, True);
                {$ENDIF}
              end
              else // If there was no palette then use a grayscale palette.
                Palette := ColorManager.CreateGrayscalePalette(False);
            end
            else if (ColorScheme in [csG, csGA]) or (ColorManager.TargetColorScheme in [csG, csGA]) then
            begin
              // Gray scale image data.
              if ColorManager.TargetColorScheme in [csG, csGA, csIndexed, csIndexedA] then
                Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite in Options);
            end;

            StartProgressSection(0, gesLoadingData);
            if ioTiled in Options then
              ReadTiled(TIFFImage)
            else
              ReadContiguous(TIFFImage);
            FinishProgressSection(False);
          end
        end;
      finally
        TIFFClose(TIFFImage);
      end;
    finally
      FinishProgressSection(False);

      if Assigned(Decoder) then
        Decoder.DecodeEnd;
      FreeAndNil(Decoder);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['TIF/TIFF']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TTIFFGraphic.ReadImageProperties(const Memory: Pointer; Size:Int64; ImageIndex: Cardinal): Boolean;

// Reads all relevant TIF properties of the image of index ImageIndex (zero based).
type
  TFloatArray = array [0..1] of Single;
  PFloatArray = ^TFloatArray;
var
  TIFFImage: PTIFF;
  PhotometricInterpretation: Word;
  ExtraSamples: Word;
  SampleInfo: PWordArray;
  TIFFValue: Word;
  TIFFCompression: Word;
  ResUnit: Word;
  FillOrder: Word;
  TiffStringValue: array [0..0] of PAnsiChar;
  TiffRationals: PFloatArray;
  RefWhiteX: Single;

  {$ifndef DELPHI_6_UP}
    // Structure used to build a va_list array.
    ExtraInfo: record
      Value1: Pointer;
      Value2: Pointer;
    end;
  {$endif DELPHI_6_UP}
  
begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
  begin
    with FImageProperties do
    begin
      FMemory := Memory;
      FCurrentPointer := Memory;
      FSize := Size;

      // OpenMode: r - readmode, (lowercase) m - Don't use memory mapped file
      // Since we are already using a memory mapped file ourselves it is not
      // necessary to let libtif also use a memory mapped file.
      TIFFImage := TIFFClientOpen('', 'rm', NativeUInt(Self), TIFFReadProc, TIFFWriteProc, TIFFSeekProc, TIFFCloseProc,
        TIFFSizeProc, TIFFMapProc, TIFFUnmapProc);
      if Assigned(TIFFImage) then
      try
        // This version is actually a magic number.
        Version := pTIFFHEADER(FMemory).Version;
        if pTIFFHEADER(FMemory).ByteOrder = TIFF_BIGENDIAN then
          Version := SwapEndian(Version);
        try
          // Account for invalid files.
          ImageCount := TIFFNumberOfDirectories(TIFFImage);
        except
          ImageCount := 1;
        end;

        TIFFSetDirectory(TIFFImage, ImageIndex);
        TIFFGetField(TIFFImage, TIFFTAG_IMAGEWIDTH, @Width);
        TIFFGetField(TIFFImage, TIFFTAG_IMAGELENGTH, @Height);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_ORIENTATION, @TIFFValue);
        Orientation := TgexOrientation(TIFFValue);

        // Number of color components per pixel (1 for b&w, 16 and 256 colors, 3 for RGB, 4 for CMYK etc.).
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_SAMPLESPERPIXEL, @TIFFValue);
        SamplesPerPixel := TIFFValue;

        // Number of bits per color component.
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_BITSPERSAMPLE, @TIFFValue);
        BitsPerSample := TIFFValue;

        // Determine whether image is tiled.
        if TIFFIsTiled(TIFFImage) > 0 then
          Include(Options, ioTiled);

        // Photometric interpretation determines the color space.
        TIFFGetField(TIFFImage, TIFFTAG_PHOTOMETRIC, @PhotometricInterpretation);
        FActualTiffData.TiffPhotometric := PhotometricInterpretation;
        // Type of extra information for additional samples per pixel.
        {$ifndef DELPHI_6_UP}
          ExtraInfo.Value1 := @ExtraSamples;
          ExtraInfo.Value2 := @SampleInfo;
          TIFFVGetFieldDefaulted(TIFFImage, TIFFTAG_EXTRASAMPLES, @ExtraInfo);
        {$else}
          TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_EXTRASAMPLES, @ExtraSamples, @SampleInfo);
        {$endif DELPHI_6_UP}

        // Determine whether extra samples must be considered.
        HasAlpha := (ExtraSamples >= 1) and
          (SampleInfo^[0] in [EXTRASAMPLE_ASSOCALPHA, EXTRASAMPLE_UNASSALPHA]);

        // SampleFormat determines DataType of samples (default = unsigned int)
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_SAMPLEFORMAT, @TIFFValue);
        SampleFormat := TIFFValue;
        if SampleFormat in [SAMPLEFORMAT_IEEEFP, SAMPLEFORMAT_COMPLEXIEEEFP] then begin
          // Get min and max pixel values for floating point pixel data
          // TODO: Proabably we should be prepared to read min/max values for each sample
          // thus 1 for grayscale, 3 for rgb
          TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_SMINSAMPLEVALUE, @FMinFloatSample);
          TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_SMAXSAMPLEVALUE, @FMaxFloatSample);
        end;

        // PlanarConfig needed to determine BitsPerPixel in case its Separate
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_PLANARCONFIG, @TIFFValue);

        // Compute Bits per Pixel
        if TIFFVALUE = PLANARCONFIG_SEPARATE then begin
          // separate planes
          Include(Options, ioSeparatePlanes);
          BitsPerPixel := BitsPerSample * (SamplesPerPixel-ExtraSamples);
        end
        else // bits are contigious
          BitsPerPixel := BitsPerSample * SamplesPerPixel;

        // Convert compression identifier.
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_COMPRESSION, @TIFFCompression);
        FActualTiffData.TiffCompression :=  TIFFCompression;
        case TIFFCompression of
          COMPRESSION_NONE:
            Compression := ctNone;
          COMPRESSION_CCITTRLE:
            Compression := ctFaxRLE;
          COMPRESSION_CCITTFAX3:
            begin
              TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_T4OPTIONS, @TIFFValue);
              if (TIFFValue and GROUP3OPT_2DENCODING) <> 0 then
                Compression := ct2DFax3
              else
                Compression := ctFax3;
            end;
          COMPRESSION_CCITTFAX4:
            Compression := ctFax4;
          COMPRESSION_LZW:
            Compression := ctLZW;
          COMPRESSION_OJPEG:
            Compression := ctOJPEG;
          COMPRESSION_JPEG:
            Compression := ctJPEG;
          {COMPRESSION_T85, - not implemented
          COMPRESSION_T43   - not implemented}
          COMPRESSION_NEXT:
            Compression := ctNext;
          COMPRESSION_CCITTRLEW:
            Compression := ctFaxRLEW;
          COMPRESSION_PACKBITS:
            Compression := ctPackedBits;
          COMPRESSION_THUNDERSCAN:
            Compression := ctThunderscan;
          COMPRESSION_IT8CTPAD:
            Compression := ctIT8CTPAD;
          COMPRESSION_IT8LW:
            Compression := ctIT8LW;
          COMPRESSION_IT8MP:
            Compression := ctIT8MP;
          COMPRESSION_IT8BL:
            Compression := ctIT8BL;
          COMPRESSION_PIXARFILM:
            Compression := ctPixarFilm;
          COMPRESSION_PIXARLOG: // also a LZ77 clone
            Compression := ctPixarLog;
          COMPRESSION_ADOBE_DEFLATE,
          COMPRESSION_DEFLATE: 
            Compression := ctLZ77;
          COMPRESSION_DCS:
            Compression := ctDCS;
          COMPRESSION_JBIG:
            Compression := ctJBIG;
          COMPRESSION_SGILOG:
            Compression := ctSGILog;
          COMPRESSION_SGILOG24:
            Compression := ctSGILog24;
          // COMPRESSION_LEADTOOLS_CMP (34709, LeadTools undocumented)
          COMPRESSION_JP2000: // LeadTools Jpeg2000
            Compression := ctJpeg2000;
          COMPRESSION_LZMA:   // LZMA2
            Compression := ctLZMA;
        else
          Compression := ctUnknown;
        end;

        case PhotometricInterpretation of
          PHOTOMETRIC_MINISWHITE:
            begin
              if HasAlpha then
                ColorScheme := csGA
              else
                ColorScheme := csG;
              Include(Options, ioMinIsWhite);
            end;
          PHOTOMETRIC_MINISBLACK,
          PHOTOMETRIC_MASK:      // Mask is long deprecated, try to interpret as grayscale
            if HasAlpha then
              ColorScheme := csGA
            else
              ColorScheme := csG;
          PHOTOMETRIC_RGB,
          // These 2 supposedly are DNG specification color schemes, try to interpret as RGB for now
          PHOTOMETRIC_CFA,
          PHOTOMETRIC_LINEAR_RAW:
            begin
              if (SamplesPerPixel < 4) then
                ColorScheme := csRGB
              else
                ColorScheme := csRGBA;
            end;
          PHOTOMETRIC_PALETTE:
            if HasAlpha then
              ColorScheme := csIndexedA
            else
              ColorScheme := csIndexed;
          PHOTOMETRIC_SEPARATED:
            if HasAlpha then
              ColorScheme := csCMYKA
            else
              ColorScheme := csCMYK;
          PHOTOMETRIC_YCBCR:
            ColorScheme := csYCbCr;
          PHOTOMETRIC_CIELAB:
            ColorScheme := csCIELab;
          PHOTOMETRIC_ICCLAB:
            ColorScheme := csICCLab;
          PHOTOMETRIC_ITULAB:
            ColorScheme := csITULab;
          PHOTOMETRIC_LOGL:
            ColorScheme := csCIELog2L;
          PHOTOMETRIC_LOGLUV:
            ColorScheme := csCIELog2Luv;
        else
          ColorScheme := csUnknown;
        end;

        if ColorScheme in [csCIELAB, csICCLab, csITULAB] then begin
          TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_WHITEPOINT, @TiffRationals);
          RefWhiteX := TiffRationals^[0]/TiffRationals^[1] * 100.0;
          ColorManager.SetWhitePoint(
            RefWhiteX, 100.0,
            (1.0 - TiffRationals^[0] - TiffRationals^[1]) / TiffRationals^[1] * RefWhiteX );
        end;

        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_XRESOLUTION, @XResolution);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_YRESOLUTION, @YResolution);
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_RESOLUTIONUNIT, @ResUnit);
        if ResUnit = RESUNIT_CENTIMETER then
        begin
          // Resolution is given in centimeters -> convert to inches.
          XResolution := XResolution * 2.54;
          YResolution := YResolution * 2.54;
        end;

        // Determine fill order in bytes
        TIFFGetFieldDefaulted(TIFFImage, TIFFTAG_FILLORDER, @FillOrder);
        if FillOrder = FILLORDER_LSB2MSB then
          Include(Options, ioReversed);

        // Get TIFF Image Description (if present)
        if (TIFFGetField(TIFFImage, TIFFTAG_IMAGEDESCRIPTION, @TiffStringValue) = 1) and
           (TiffStringValue[0] <> nil) then
          Comment := TiffStringValue[0];
      finally
        TIFFClose(TIFFImage);
      end
      else
        Result := False;
    end;
  end;
end;

//----------------- TEPSGraphic ----------------------------------------------------------------------------------------

{$ifdef EPSGraphic}

// Note: This EPS implementation does only read embedded pixel graphics in TIF format (preview).
// Credits to:
//   Olaf Stieleke
//   Torsten Pohlmeyer
//   CPS Krohn GmbH
// for providing the base information about how to read the preview image.

type
  PEPSHeader = ^TEPSHeader;
  TEPSHeader = packed record
    Code: Cardinal;   // alway $C6D3D0C5, if not there then this is not an EPS or it is not a binary EPS
    PSStart,          // Offset PostScript-Code
    PSLen,            // length of PostScript-Code
    MetaPos,          // position of a WMF
    MetaLen,          // length of a WMF 
    TiffPos,          // position of TIFF (preview images should be either WMF or TIF but not both)
    TiffLen: Integer; // length of the TIFF
    Checksum: SmallInt;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TEPSGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TEPSHeader);
  if Result then
    with PEPSHeader(Memory)^ do
    begin
      Result := (Code = $C6D3D0C5) and (TiffPos >= SizeOf(TEPSHeader)) and (TiffLen > 0);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEPSGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

begin
  with PEPSHeader(Memory)^ do
  begin
    if Code = $C6D3D0C5 then
      inherited LoadFromMemory(PAnsiChar(Memory) + TiffPos, TiffLen)
    else
      GraphicExError(gesInvalidImage, ['EPS']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEPSGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;

begin
  Result := inherited ReadImageProperties(Stream, ImageIndex);
end;

{$endif EPSGraphic}

{$endif TIFFGraphic}

//----------------- TTargaGraphic ----------------------------------------------

{$ifdef TargaGraphic}

//  FILE STRUCTURE FOR THE ORIGINAL TRUEVISION TGA FILE
//    FIELD 1: NUMBER OF CHARACTERS IN ID FIELD (1 BYTES)
//    FIELD 2: COLOR MAP TYPE (1 BYTES)
//    FIELD 3: IMAGE TYPE CODE (1 BYTES)
//      = 0  NO IMAGE DATA INCLUDED
//      = 1  UNCOMPRESSED, COLOR-MAPPED IMAGE
//      = 2  UNCOMPRESSED, TRUE-COLOR IMAGE
//      = 3  UNCOMPRESSED, BLACK AND WHITE IMAGE (black and white is actually grayscale)
//      = 9  RUN-LENGTH ENCODED COLOR-MAPPED IMAGE
//      = 10 RUN-LENGTH ENCODED TRUE-COLOR IMAGE
//      = 11 RUN-LENGTH ENCODED BLACK AND WHITE IMAGE
//    FIELD 4: COLOR MAP SPECIFICATION (5 BYTES)
//      4.1: COLOR MAP ORIGIN (2 BYTES)
//      4.2: COLOR MAP LENGTH (2 BYTES)
//      4.3: COLOR MAP ENTRY SIZE (1 BYTES)
//    FIELD 5:IMAGE SPECIFICATION (10 BYTES)
//      5.1: X-ORIGIN OF IMAGE (2 BYTES)
//      5.2: Y-ORIGIN OF IMAGE (2 BYTES)
//      5.3: WIDTH OF IMAGE (2 BYTES)
//      5.4: HEIGHT OF IMAGE (2 BYTES)
//      5.5: IMAGE PIXEL SIZE (1 BYTE)
//      5.6: IMAGE DESCRIPTOR BYTE (1 BYTE)
//        bit 0..3: attribute bits per pixel
//        bit 4..5: image orientation:
//          0: bottom left
//          1: bottom right
//          2: top left
//          3: top right
//        bit 6..7: interleaved flag
//          0: two way (even-odd) interleave (e.g. IBM Graphics Card Adapter), obsolete
//          1: four way interleave (e.g. AT&T 6300 High Resolution), obsolete
//    FIELD 6: IMAGE ID FIELD (LENGTH SPECIFIED BY FIELD 1)
//    FIELD 7: COLOR MAP DATA (BIT WIDTH SPECIFIED BY FIELD 4.3 AND
//             NUMBER OF COLOR MAP ENTRIES SPECIFIED IN FIELD 4.2)
//    FIELD 8: IMAGE DATA FIELD (WIDTH AND HEIGHT SPECIFIED IN FIELD 5.3 AND 5.4)

const
  // ColorMap presence (indexed images)
  TARGA_NO_COLORMAP         = 0;
  TARGA_COLORMAP            = 1;

  // Targa image types
  TARGA_EMPTY_IMAGE         = 0;
  TARGA_INDEXED_IMAGE       = 1;
  TARGA_TRUECOLOR_IMAGE     = 2;
  TARGA_BW_IMAGE            = 3;
  TARGA_INDEXED_RLE_IMAGE   = 9;
  TARGA_TRUECOLOR_RLE_IMAGE = 10;
  TARGA_BW_RLE_IMAGE        = 11;

  // Targa version 2 signature and extension area size
  TARGA_SIGNATURE: array [0..17] of AnsiChar = 'TRUEVISION-XFILE.'+#0;
  TARGA_V2_EXTENSION_AREA_SIZE = 495;

//------------------------------------------------------------------------------

constructor TTargaGraphic.Create;
begin
  inherited Create;
  FExtensionArea := nil;
end;

destructor TTargaGraphic.Destroy;
begin
  if Assigned(FExtensionArea) then
    FreeMem(FExtensionArea);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

class function TTargaGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TTargaHeader);
  if Result then
    with PTargaHeader(Memory)^ do
    begin
      // Targa version 1 images are hard to determine because there is no magic id or something like that.
      // Hence all we can do is to check if all values from the header are within correct limits.
      Result := (ImageType in [TARGA_EMPTY_IMAGE, TARGA_INDEXED_IMAGE, TARGA_TRUECOLOR_IMAGE, TARGA_BW_IMAGE,
        TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE, TARGA_BW_RLE_IMAGE]) and
        (ColorMapType in [TARGA_NO_COLORMAP, TARGA_COLORMAP]) and
        (ColorMapEntrySize in [0, 15, 16, 24, 32]) and
        (PixelSize in [8, 15, 16, 24, 32]);
    end;
end;

//------------------------------------------------------------------------------

procedure TTargaGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run,
  Source,
  Buffer: PByte;
  I: Integer;
  LineSize: Integer;
  LineBuffer: Pointer;
  LogPalette: TMaxLogPalette;
  FlipV: Boolean;
  Decoder: TTargaRLEDecoder;
  ColorMapBufSize: Integer;
  ColorMapBuffer: Pointer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FlipV := Orientation = gexoTopLeft;

      // skip image ID
      Source := Pointer(PAnsiChar(Memory) + SizeOf(TTargaHeader) + FTargaHeader.IDLength);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        SourceBitsPerSample := BitsPerSample;
        {$IFNDEF FPC}
        TargetBitsPerSample := BitsPerSample;
        TargetSamplesPerPixel := SamplesPerPixel;
        // To be able to set alpha to opaque we need to check target color scheme
        // which by default is csBGR. Since for Delphi we don't convert but just
        // move bytes for 32 bits this is the only reason to set target color scheme for.
        if TargetSamplesPerPixel = 4 then begin
          TargetColorScheme := csBGRA;
        end;
        {$ELSE}
        TargetBitsPerSample := 8;
        if BitsPerSample = 5 then
          SourceExtraBPP := 1; // 1 extra bit per pixel
        if HasAlpha then begin
          TargetSamplesPerPixel := 4;
          TargetColorScheme := csBGRA;
        end
        else begin
          TargetSamplesPerPixel := 3;
          TargetColorScheme := csBGR;
        end;
        {$ENDIF}
        if ioUseGamma in Options then begin
          SetGamma(FileGamma);
          ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
        end;
        PixelFormat := TargetPixelFormat;
      end;

      if (FTargaHeader.ColorMapType = TARGA_COLORMAP) or
         (FTargaHeader.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE]) then
      begin
        if FTargaHeader.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
          Palette := ColorManager.CreateGrayscalePalette(False)
        else
        begin
          // Note that ColorMapBufSize and ColorMapBuffer are currently not used
          // by 15/16 bits color map entries. However since it is planned to move
          // that code to the ColorManager to we will leave this as is since it
          // will be needed there too after the move.
          ColorMapBufSize := ((FTargaHeader.ColorMapEntrySize + 7) div 8) * FTargaHeader.ColorMapSize;
          GetMem(ColorMapBuffer, ColorMapBufSize);
          try
            Move(Source^, ColorMapBuffer^, ColorMapBufSize);
            case FTargaHeader.ColorMapEntrySize of
              32:
                begin
                  {$IFDEF FPC}
                  ColorManager.SetSourcePalette([Source], pfInterlaced8Quad, False {BGR order});
                  {$ENDIF}
                  Palette := ColorManager.CreateColorPalette([ColorMapBuffer],
                    pfInterlaced8Quad, FTargaHeader.ColorMapSize, False {BGR order});
                  Inc(Source, ColorMapBufSize);
                end;
              24:
                begin
                  {$IFDEF FPC}
                  ColorManager.SetSourcePalette([Source], pfInterlaced8Triple, False {BGR order});
                  {$ENDIF}
                  Palette := ColorManager.CreateColorPalette([ColorMapBuffer],
                    pfInterlaced8Triple, FTargaHeader.ColorMapSize, False {BGR order});
                  Inc(Source, ColorMapBufSize);
                end;
              15, 16:
                with LogPalette do
                begin
                  // read palette entries and create a palette
                  ZeroMemory(@LogPalette, SizeOf(LogPalette));
                  palVersion := $300;
                  palNumEntries := FTargaHeader.ColorMapSize;

                  // TODO: This Color Palette creation algorithm should be moved to
                  // ColorManager.CreateColorPalette!
                  // 15 and 16 bits per color map entry (handle both like 555 color format
                  // but make 8 bit from 5 bit per color component)
                  for I := 0 to FTargaHeader.ColorMapSize - 1 do
                  begin
                    palPalEntry[I].peBlue := Byte((PWord(Source)^ and $1F) shl 3);
                    palPalEntry[I].peGreen := Byte((PWord(Source)^ and $3E0) shr 2);
                    palPalEntry[I].peRed := Byte((PWord(Source)^ and $7C00) shr 7);
                    Inc(PWord(Source));
                  end;
                  Palette := CreatePalette(PLogPalette(@LogPalette)^);
                  {$IFDEF FPC}
                  ColorManager.SetSourcePalette([@LogPalette.palPalEntry], pfInterlaced8Quad);
                  {$ENDIF}
                end;
            else
              // Other color map entry sizes are not supported
              GraphicExError(gesInvalidImage, ['TGA']);
            end;
          finally
            if Assigned(ColorMapBuffer) then
              FreeMem(ColorMapBuffer);
          end;
        end;
      end;

      Self.Width := FTargaHeader.Width;
      Self.Height := FTargaHeader.Height;

      // Compute size in bytes of one line of the image.
      LineSize := Width * ((FTargaHeader.PixelSize+7) div 8);
      Progress(Self, psEnding, 0, False, FProgressRect, '');

      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
      case FTargaHeader.ImageType of
        TARGA_EMPTY_IMAGE: // nothing to do here
          ;
        TARGA_BW_IMAGE,
        TARGA_INDEXED_IMAGE,
        TARGA_TRUECOLOR_IMAGE:
          begin
            for I := 0 to Height - 1 do
            begin
              if FlipV then
                LineBuffer := ScanLine[I]
              else
                LineBuffer := ScanLine[FTargaHeader.Height - (I + 1)];
              {$IFNDEF FPC}
              Move(Source^, LineBuffer^, LineSize);
              {$ELSE}
              ColorManager.ConvertRow([Source], LineBuffer, Width, $FF);
              {$ENDIF}
              Inc(Source, LineSize);
              Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        TARGA_BW_RLE_IMAGE,
        TARGA_INDEXED_RLE_IMAGE,
        TARGA_TRUECOLOR_RLE_IMAGE:
          begin
            Buffer := nil;
            Decoder := TTargaRLEDecoder.Create(FTargaHeader.PixelSize);
            try
              // Targa RLE is not line oriented. Convert all the RLE data in one rush.
              GetMem(Buffer, Height * LineSize);
              Run := Buffer;
              // Problematic is that we don't know in advance the size of the compressed data
              // Only thing we can do is make sure it doesn't go beyond the size of the image
              Decoder.Decode(Pointer(Source), Pointer(Buffer),
                Size - (NativeUInt(Source)-NativeUInt(Memory)),
                Height * LineSize);

              // Finally put data into the image.
              for I := 0 to Height - 1 do
              begin
                if FlipV then
                  LineBuffer := ScanLine[I]
                else
                  LineBuffer := ScanLine[FTargaHeader.Height - (I + 1)];
                {$IFNDEF FPC}
                Move(Run^, LineBuffer^, LineSize);
                {$ELSE}
                  ColorManager.ConvertRow([Run], LineBuffer, Width, $FF);
                {$ENDIF}
                Inc(Run, LineSize);
                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            finally
              if Assigned(Buffer) then
                FreeMem(Buffer);
              FreeAndNil(Decoder);
            end;
          end;
      else
        GraphicExError(gesInvalidImage, ['TGA']);
      end;

      // 32 bit TGA images may not be using the alpha channel, in that case we
      // replace it by Alpha is 255 or else the image will be invisible
      if (FTargaHeader.PixelSize = 32) and (ColorManager.TargetColorScheme = csBGRA) then begin
        if not HasAlpha then
          for i := 0 to Height-1 do
            BGRASetAlpha255(ScanLine[i], Width);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
end;

//------------------------------------------------------------------------------

function TTargaGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Move(Memory^, FTargaHeader, SizeOf(TTargaHeader));

      Width := FTargaHeader.Width;
      Height := FTargaHeader.Height;
      BitsPerSample := 8;

      case FTargaHeader.PixelSize of
        8:
          begin
            if FTargaHeader.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
              ColorScheme := csG
            else
              ColorScheme := csIndexed;
            SamplesPerPixel := 1;
          end;
        15,
        16: // actually, 16 bit are meant being 15 bit
          begin
            ColorScheme := csBGR;
            BitsPerSample := 5;
            SamplesPerPixel := 3;
            ExtraBits := 1;
          end;
        24:
          begin
            ColorScheme := csBGR;
            SamplesPerPixel := 3;
          end;
        32:
          begin
            ColorScheme := csBGRA;
            SamplesPerPixel := 4;
          end;
      end;

      BitsPerPixel := SamplesPerPixel * BitsPerSample;
      if FTargaHeader.ImageType in [TARGA_BW_RLE_IMAGE, TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE] then
        Compression := ctRLE
      else
        Compression := ctNone;

      // Get image Orientation
      case ((FTargaHeader.ImageDescriptor and $30) shr 4) of
        0: Orientation := gexoBottomLeft;
        1: Orientation := gexoBottomRight;
        2: Orientation := gexoTopLeft;
      else // 3
        Orientation := gexoTopRight;
      end;

      // Check for Targa version 1 id field, if present use it as comment
      if FTargaHeader.IDLength > 0 then begin
        Run := Memory;
        Inc(Run, SizeOf(TTargaHeader));
        SetString(FImageProperties.Comment, PAnsiChar(Run), FTargaHeader.IDLength);
      end;

      FImageProperties.Version := 1;
      // Check if Targa version 2 Footer is present
      if (SizeOf(TTargaHeader) + SizeOf(TTargaV2Footer) < Size) then begin
        Run := Memory;
        Inc(Run, Size-SizeOf(TTargaV2Footer));
        Move(Run^, FTargaFooter, SizeOf(TTargaV2Footer));

        // Does it have the version 2 signature?
        if CompareStr(FTargaFooter.Signature, TARGA_SIGNATURE) = 0 then begin
          FImageProperties.Version := 2; // Yes, it is version 2.

          // Does it have the optional ExtensionArea?
          if FTargaFooter.ExtAreaOffset > 0 then begin
            Run := Memory;
            Inc(Run, FTargaFooter.ExtAreaOffset);

            // Does the ExtensionArea have the correct size?
            if PWord(Run)^ = TARGA_V2_EXTENSION_AREA_SIZE then begin // The expected size of ExtensionArea
              if not Assigned(FExtensionArea) then
                GetMem(FExtensionArea, SizeOf(TExtensionArea));
              Move(Run^, FExtensionArea^, SizeOf(TExtensionArea));
              if FExtensionArea.Comments[0][0] <> '' then begin
                // Comment present, for now we only copy the first line.
                FImageProperties.Comment := FExtensionArea.Comments[0];
              end;
              if (FExtensionArea.GammaRatioDenominator > 0) and
                (FExtensionArea.GammaRatioNumerator > 0) then begin
                // Todo: TGA gamma is in range 0.0 - 10.0, do we need to convert this range?
                // I don't have any examples where gamma is defined
                FileGamma := FExtensionArea.GammaRatioDenominator +
                  FExtensionArea.GammaRatioNumerator / 100;
                Include(Options, ioUseGamma);
              end;
            end
            else // Unexpected size don't know how to handle.
              FTargaFooter.ExtAreaOffset := 0;
          end;
        end;
      end;

      HasAlpha := (FTargaHeader.ImageDescriptor and $F > 0);
      if (FImageProperties.Version = 2) and (FTargaFooter.ExtAreaOffset > 0) then
        HasAlpha := FExtensionArea.Attributes in [AlphaDataPresent, PreMultipliedAlpha];
      // Although 16 bits per pixel targa has in theory an alpha channel,
      // the examples I have seen have all alpha values set to 0 (invisible).
      // As such it doesn't seem useful to set this until we encounter
      // a 16 bit tga image that does set non zero values.
      {if (FTargaHeader.PixelSize = 16) and HasAlpha then begin
        ColorScheme := csBGRA;
        // Not sure if we should set SamplesPerPixel to 4 or just use
        // ColorScheme is csBGRA in combination with ExtrBits = 1 to define this.
        SamplesPerPixel := 4;
      end;}
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream);

begin
  SaveToStream(Stream, True);
end;

//------------------------------------------------------------------------------

procedure TTargaGraphic.SaveToStream(Stream: TStream; Compressed: Boolean);

// The format of the image to be saved depends on the current properties of the bitmap not
// on the values which may be set in the header during a former load.

var
  RLEBuffer: Pointer;
  I: Integer;
  LineSize: Integer;
  WriteLength: Cardinal;
  LogPalette: TMaxLogPalette;
  BPP: Byte;
  Header: TTargaHeader;
  Encoder: TTargaRLEDecoder;

begin
  FProgressRect := Rect(0, 0, Width, 1);
  Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
  // prepare color depth
  case PixelFormat of
    pf1Bit,
    pf4Bit: // Note: 1 bit and 4 bits per pixel are not supported in the Targa format, an image
            //       with one of these pixel formats is implicitly converted to 256 colors.
      begin
        PixelFormat := pf8Bit;
        BPP := 1;
      end;
    pf8Bit:
      BPP := 1;
    pf15Bit,
    pf16Bit:
      BPP := 2;
    pf24Bit:
      BPP := 3;
    pf32Bit:
      BPP := 4;
  else
    BPP := GetDeviceCaps(Canvas.Handle, BITSPIXEL) div 8;
  end;

  if not Empty then
  begin
    with Header do
    begin
      IDLength := 0;
      if BPP = 1 then
        ColorMapType := 1
      else
        ColorMapType := 0;
      if not Compressed then
        // can't distinct between a B&W and an color indexed image here, so I use always the latter
        if BPP = 1 then
          ImageType := TARGA_INDEXED_IMAGE
        else
          ImageType := TARGA_TRUECOLOR_IMAGE
      else
        if BPP = 1 then
          ImageType := TARGA_INDEXED_RLE_IMAGE
        else
          ImageType := TARGA_TRUECOLOR_RLE_IMAGE;

      ColorMapOrigin := 0;
      XOrigin := 0;
      YOrigin := 0;
      Width := Self.Width;
      Height := Self.Height;
      PixelSize := 8 * BPP;
      // if the image is a bottom-up DIB then indicate this in the image descriptor
      if NativeUInt(Scanline[0]) > NativeUInt(Scanline[1]) then
        ImageDescriptor := $20
      else
        ImageDescriptor := 0;

      Stream.Write(Header, SizeOf(Header));

      // store color palette if necessary
      if ColorMapType = 1 then
      begin
        with LogPalette do
        begin
          // read palette entries
          GetPaletteEntries(Palette, 0, 256, palPalEntry);
          for I := 0 to 255 do
          begin
            Stream.Write(palPalEntry[I].peBlue, 1);
            Stream.Write(palPalEntry[I].peGreen, 1);
            Stream.Write(palPalEntry[I].peRed, 1);
          end;
        end;
        ColorMapSize := 256;
        ColorMapEntrySize := 24;
      end
      else
      begin
        ColorMapSize := 0;
        ColorMapEntrySize := 0;
      end;
    end;

    LineSize := Width * ((Header.PixelSize + 7) div 8);
    Progress(Self, psEnding, 0, False, FProgressRect, '');

    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
    // finally write image data
    if Compressed then
    begin
      RLEBuffer := nil;
      Encoder := TTargaRLEDecoder.Create(Header.PixelSize);
      try
        GetMem(RLEBuffer, 2 * LineSize);
        for I := 0 to Height - 1 do
        begin
          Encoder.Encode(ScanLine[I], RLEBuffer, Width, WriteLength);
          Stream.WriteBuffer(RLEBuffer^, WriteLength);

          Progress(Self, psRunning, 0, False, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RLEBuffer) then
         FreeMem(RLEBuffer);
        Encoder.Free;
      end;
    end
    else
    begin
      for I := 0 to Height - 1 do
      begin
        Stream.WriteBuffer(ScanLine[I]^, LineSize);

        Progress(Self, psRunning, 0, False, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
    end;

    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end;
end;

{$endif TargaGraphic}

//----------------- TPCXGraphic ----------------------------------------------------------------------------------------

{$ifdef PCXGraphic}

type
  PPCXHeader = ^TPCXHeader;
  TPCXHeader = record
    FileID: Byte;                      // $0A for PCX files, $CD for SCR files
    Version: Byte;                     // 0: version 2.5; 2: 2.8 with palette; 3: 2.8 w/o palette; 5: version 3;
                                       // 4: PC Paintbrush for Windows; 5: PC Paintbrush +, Publisher's Paintbrush
    Encoding: Byte;                    // 0: uncompressed; 1: RLE encoded
    BitsPerPixel: Byte;                // Number of bits to represent a pixel (per Plane) - 1, 2, 4, or 8
    XMin,
    YMin,
    XMax,
    YMax,                              // coordinates of the corners of the image
    HRes,                              // horizontal resolution in dpi
    VRes: Word;                        // vertical resolution in dpi
    ColorMap: array[0..15] of TRGB;    // color table
    Reserved,
    ColorPlanes: Byte;                 // color planes (1, 3 or 4)
    BytesPerLine,                      // number of bytes of one line of one plane (must be an even number)
    PaletteType: Word;                 // 1: color or b&w; 2: gray scale (ignored in PB IV/ IV +)
    HscreenSize: Word;                 // Horizontal screen size in pixels. New field found only in PB IV/IV Plus
    VscreenSize: Word;                 // Vertical screen size in pixels. New field found only in PB IV/IV Plus
    Fill: array[0..53] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPCXGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TPCXHeader);
  if Result then
    with PPCXHeader(Memory)^ do
    begin
      Result := (FileID in [$0A, $CD]) and (Version in [0, 2..5]) and
        (Encoding in [0, 1]) and (BitsPerPixel in [1, 2, 4, 8]);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCXGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TPCXHeader;
  Run: PByte;

  //--------------- local functions -------------------------------------------

  procedure MakePalette(APixelFormat: TPixelFormat);

  var
    PaletteData: PByte;
    bgIndex: Byte;
    UseColor: Boolean;
    UseGreenRedBrown: Boolean;
    IsLight: Boolean;
  begin
    if (Header.Version <> 3) or (APixelFormat = pf1Bit) then
    begin
      case APixelFormat of
        pf1Bit:
          Palette := ColorManager.CreateGrayScalePalette(False);
        pf4Bit:
          if Header.paletteType = 2 then
            Palette := ColorManager.CreateGrayScalePalette(False)
          else if (Header.BitsPerPixel = 2) and
            // Just a guess why CGA_FSD.PCX has a palette and not CGA mode byte and background byte
            not ((Header.Version = 5) and (Header.VscreenSize = 1)) then begin
            // CGA Palette
            // Get the CGA background color (0-15)
            bgIndex := Header.ColorMap[0].R shr 4;
            if ((Header.Version = 5) and (Header.VscreenSize = 201)) then begin
              // Settings that seem to work for CGA_TST1.PCX and CGA_RGBI.PCX
              // No documentation found for this nor any other pcx images to test.
              UseColor := Header.ColorMap[1].R and $08 = 0;
              UseGreenRedBrown := Header.ColorMap[1].R and $10 = 0;
              IsLight := Header.ColorMap[1].R and $04 <> 0;
            end
            else begin
              // Bits according to the PCX specs checked below.
              // However according to the text file accompanying CGA_TST1.PCX and CGA_RGBI.PCX
              // These seem to be wrong.
              UseColor := Header.ColorMap[1].R and $80 = 0;
              UseGreenRedBrown := Header.ColorMap[1].R and $40 = 0;
              IsLight := Header.ColorMap[1].R and $20 <> 0;
            end;
            Palette := ColorManager.CreateCGAColorPalette(bgIndex, UseColor, UseGreenRedBrown, IsLight);
          end
          else begin
            Palette := ColorManager.CreateColorPalette([@Header.ColorMap], pfInterlaced8Triple, 16);
            {$IFDEF FPC}
            ColorManager.SetSourcePalette([@Header.ColorMap], pfInterlaced8Triple);
            {$ENDIF}
          end;
        pf8Bit:
          begin
            // 256 colors with 3 components plus one marker byte
            PaletteData := Pointer(PAnsiChar(Memory) + Size - 769);
            if PaletteData^ <> $0C then
            begin
              // palette ID is wrong, perhaps gray scale?
              if Header.PaletteType = 2 then
                Palette := ColorManager.CreateGrayScalePalette(False);
              // else ignore palette
            end
            else
            begin
              Inc(PaletteData);
              Palette := ColorManager.CreateColorPalette([PaletteData], pfInterlaced8Triple, 256);
              {$IFDEF FPC}
              ColorManager.SetSourcePalette([PaletteData], pfInterlaced8Triple);
              {$ENDIF}
            end;
          end;
      end;
    end
    else
    begin
      // version 2.8 without palette information, just use the system palette
      // 256 colors will not be correct with this assignment...
      Palette := SystemPalette16;
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  PCXSize,
  DataSize: Integer;
  DecodeBuffer: Pointer;
  {$IFDEF FPC}
  LineBuf: PByte;
  {$ENDIF}
  Plane1,
  Plane2,
  Plane3,
  Plane4: PByte;
  Value,
  Mask: Byte;
  I, J: Integer;
  Line: PByte;
  Increment: Integer;
  TempPixelFormat: TPixelFormat;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FProgressRect := Rect(0, 0, Width, 1);
    Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

    Run := Memory;
    Move(Run^, Header, SizeOf(Header));
    Inc(Run, SizeOf(Header));
    if not (Header.FileID in [$0A, $CD]) then
      GraphicExError(gesInvalidImage, ['PCX, PCC or SCR']);

    ColorManager.SourceColorScheme := FImageProperties.ColorScheme;
    ColorManager.SourceBitsPerSample := FImageProperties.BitsPerSample;
    ColorManager.SourceSamplesPerPixel := FImageProperties.SamplesPerPixel;
    ColorManager.TargetSamplesPerPixel := FImageProperties.SamplesPerPixel;
    if FImageProperties.ColorScheme = csIndexed then
      {$IFNDEF FPC}
      ColorManager.TargetColorScheme := csIndexed
      {$ELSE}
      if FImageProperties.BitsPerSample > 1 then begin
        ColorManager.TargetColorScheme := csBGR;
        ColorManager.TargetSamplesPerPixel := 3;
      end
      else begin
        ColorManager.TargetColorScheme := csIndexed;
        ColorManager.TargetSamplesPerPixel := 1;
      end
      {$ENDIF}
    else begin
      if ColorManager.SourceSamplesPerPixel = 3 then
        ColorManager.TargetColorScheme := csBGR
      else
        ColorManager.TargetColorScheme := csBGRA;
      ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
    end;
    if (ColorManager.SourceSamplesPerPixel in [3, 4]) then
      if FImageProperties.ColorScheme = csIndexed then begin
        // Should be 1 bits per pixel x 4 planes special PCX case
        {$IFNDEF FPC}
        ColorManager.TargetBitsPerSample := 4;
        ColorManager.TargetSamplesPerPixel := 1;
        {$ELSE}
        ColorManager.TargetBitsPerSample := 8;
        ColorManager.TargetSamplesPerPixel := 3;
        ColorManager.TargetColorScheme := csBGR;
        {$ENDIF}
        // To be able to get a correct palette source bits per sample also needs to be 4.
        ColorManager.SourceBitsPerSample := 4;
      end
      else begin
        // Use 8 bits per samples since we don't have a converter yet to 5 bits in ColorManager.
        ColorManager.TargetBitsPerSample := 8;
        // Separate channels thus we need to set that in source options.
        ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
      end
    else if FImageProperties.BitsPerPixel = 2 then begin
      {$IFNDEF FPC}
      ColorManager.TargetBitsPerSample := 4;
      {$ELSE}
      ColorManager.TargetBitsPerSample := 8;
      ColorManager.TargetSamplesPerPixel := 3;
      ColorManager.TargetColorScheme := csBGR;
      {$ENDIF}
    end
    else
      ColorManager.TargetBitsPerSample := FImageProperties.BitsPerSample;

    // Set image pixel format
    PixelFormat := ColorManager.TargetPixelFormat;

    // 256 colors palette is appended to the actual PCX data.
    PCXSize := Size;
    // Since TBitmap can change PixelFormat internally to what it accepts,
    // we cannot use it since we need source format to determine if we need
    // to add palette data.
    TempPixelFormat := ColorManager.SourcePixelFormat;
    // Since pcx special case 4 samples 1 bit returns pfCustom, we need to fix that
    if (TempPixelFormat = pfCustom) and (FImageProperties.BitsPerSample = 1) and
       (FImageProperties.SamplesPerPixel = 4) then
      TempPixelFormat := pf4Bit;

    if TempPixelFormat = pf8Bit then
      Dec(PCXSize, 769);
    if TempPixelFormat in [pf1Bit, pf4Bit, pf8Bit] then
      MakePalette(TempPixelFormat);

    Self.Width := FImageProperties.Width;
    Self.Height := FImageProperties.Height;

    // adjust alignment of line
    Increment := FImageProperties.SamplesPerPixel * Header.BytesPerLine;

    // allocate pixel data buffer and decode data if necessary
    if FImageProperties.Compression = ctRLE then
    begin
      DataSize := Increment * Height;
      GetMem(DecodeBuffer, DataSize);

      with TPCXRLEDecoder.Create do
      try
        Decode(Pointer(Run), DecodeBuffer, PCXSize, DataSize);
      finally
        Free;
      end;
    end
    else
    begin
      GetMem(DecodeBuffer, PCXSize);
      Move(Run^, DecodeBuffer^, PCXSize);
    end;
    Progress(Self, psEnding, 0, False, FProgressRect, '');

    Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
    try
      Run := DecodeBuffer;

      if (FImageProperties.SamplesPerPixel = 4) and (FImageProperties.BitsPerPixel = 4) then
      begin
        // 4 planes with one bit

        {$IFDEF FPC}
        DataSize := (Width * BitsPerPixel + 7) div 8;
        GetMem(LineBuf, DataSize);
        try
        {$ENDIF}
          for I := 0 to Height - 1 do
          begin
            Plane1 := Run;
            Plane2 := PByte(PAnsiChar(Run) + Header.BytesPerLine);
            Plane3 := PByte(PAnsiChar(Run) + 2 * Header.BytesPerLine);
            Plane4 := PByte(PAnsiChar(Run) + 3 * Header.BytesPerLine);

            {$IFNDEF FPC}
            Line := ScanLine[I];
            {$ELSE}
            Line := LineBuf;
            {$ENDIF}
            // number of bytes to write
            DataSize := (Width * FImageProperties.BitsPerPixel + 7) div 8;
            Mask := 0;
            while DataSize > 0 do
            begin
              Value := 0;
              for J := 0 to 1 do
              {$IFNDEF CPU64}
              asm
                MOV AL, [Value]

                MOV EDX, [Plane4]             // take the 4 MSBs from the 4 runs and build a nibble
                SHL BYTE PTR [EDX], 1         // read MSB and prepare next run at the same time
                RCL AL, 1                     // MSB from previous shift is in CF -> move it to AL

                MOV EDX, [Plane3]             // now do the same with the other three runs
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane2]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV EDX, [Plane1]
                SHL BYTE PTR [EDX], 1
                RCL AL, 1

                MOV [Value], AL
              end;
              {$ELSE}
              begin
                Value := Value shl 1; // No effect the first time since Value will be 0
                if Plane4^ and $80 <> 0 then Value := Value or $01;
                Value := Value shl 1;
                Plane4^ := Plane4^ shl 1;
                if Plane3^ and $80 <> 0 then Value := Value or $01;
                Value := Value shl 1;
                Plane3^ := Plane3^ shl 1;
                if Plane2^ and $80 <> 0 then Value := Value or $01;
                Value := Value shl 1;
                Plane2^ := Plane2^ shl 1;
                if Plane1^ and $80 <> 0 then Value := Value or $01;
                Plane1^ := Plane1^ shl 1;
              end;
              {$ENDIF}
              Line^ := Value;
              Inc(Line);
              Dec(DataSize);

              // two runs above (to construct two nibbles -> one byte), now update marker
              // to know when to switch to next byte in the planes
              Mask := (Mask + 2) mod 8;
              if Mask = 0 then
              begin
                Inc(Plane1);
                Inc(Plane2);
                Inc(Plane3);
                Inc(Plane4);
              end;
            end;
            {$IFDEF FPC}
            ColorManager.SourceBitsPerSample := 4;
            ColorManager.SourceSamplesPerPixel := 1;
            ColorManager.ConvertRow([LineBuf], ScanLine[I], Width, $FF);
            {$ENDIF}
            Inc(Run, Increment);

            Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        {$IFDEF FPC}
        finally
          FreeMem(LineBuf);
        end;
        {$ENDIF}
      end
      else
        case FImageProperties.SamplesPerPixel of
          3:  // RGB 3 planes
            begin
              if FImageProperties.BitsPerPixel >= 8 then begin
                Plane1 := Run;
                Plane2 := PByte(PAnsiChar(Run) + Header.BytesPerLine);
                Plane3 := PByte(PAnsiChar(Run) + 2 * Header.BytesPerLine);
              end
              else begin
                // For some reason 3 planes x 1 pixel has different order of rgb.
                Plane3 := Run;
                Plane2 := PByte(PAnsiChar(Run) + Header.BytesPerLine);
                Plane1 := PByte(PAnsiChar(Run) + 2 * Header.BytesPerLine);
              end;
              for I := 0 to Height - 1 do
              begin
                Line := ScanLine[I];
                ColorManager.ConvertRow([Plane1, Plane2, Plane3], Line, Width, $FF);
                Inc(Plane1, Increment);
                Inc(Plane2, Increment);
                Inc(Plane3, Increment);

                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
          4:  // RGBA 4 planes (most likely never used in PCX)
            begin
              Plane1 := Run;
              Plane2 := PByte(PAnsiChar(Run) + Header.BytesPerLine);
              Plane3 := PByte(PAnsiChar(Run) + 2 * Header.BytesPerLine);
              Plane4 := PByte(PAnsiChar(Run) + 3 * Header.BytesPerLine);
              for I := 0 to Height - 1 do
              begin
                Line := ScanLine[I];
                ColorManager.ConvertRow([Plane1, Plane2, Plane3, Plane4], Line, Width, $FF);
                Inc(Plane1, Increment);
                Inc(Plane2, Increment);
                Inc(Plane3, Increment);
                Inc(Plane4, Increment);

                Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
        else // indexed formats
          for I := 0 to Height - 1 do
          begin
            Line := ScanLine[I];
            ColorManager.ConvertRow([Run], Line, Width, $FF);
            Inc(Run, Increment);

            Progress(Self, psRunning, MulDiv(I, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end;
    finally
      if Assigned(DecodeBuffer) then
        FreeMem(DecodeBuffer);
    end;
    Progress(Self, psEnding, 0, False, FProgressRect, '');
  end
  else
    GraphicExError(gesInvalidImage, ['PCX, PCC or SCR']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCXGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: PPCXHeader;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
  begin
    Header := Memory;
    if Header.FileID in [$0A, $CD] then
    begin
      FImageProperties.Version := Header.Version;
      FImageProperties.Width := Header.XMax - Header.XMin + 1;
      FImageProperties.Height := Header.YMax - Header.YMin + 1;

      FImageProperties.SamplesPerPixel := Header.ColorPlanes;
      FImageProperties.BitsPerSample := Header.BitsPerPixel;
      FImageProperties.BitsPerPixel := FImageProperties.BitsPerSample * FImageProperties.SamplesPerPixel;

      case Header.ColorPlanes of
        1: FImageProperties.ColorScheme := csIndexed;
        3: FImageProperties.ColorScheme := csRGB;
        4: if Header.BitsPerPixel = 1 then
             // Special PCX case
             FImageProperties.ColorScheme := csIndexed
           else
             FImageProperties.ColorScheme := csRGBA;
      else
        FImageProperties.ColorScheme := csUnknown;
      end;

      if Header.Encoding = 1 then
        FImageProperties.Compression := ctRLE
      else
        FImageProperties.Compression := ctNone;
      FImageProperties.XResolution := Header.HRes;
      FImageProperties.YResolution := Header.VRes;

      Result := True;
    end
    else
      Result := False;
  end;
end;

{$endif PCXGraphic}

//----------------- TPCDGraphic ----------------------------------------------------------------------------------------

{$ifdef PCDGraphic}

const
  PCD_BEGIN_BASE16 = 8192;
  PCD_BEGIN_BASE4 = 47104;
  PCD_BEGIN_BASE = 196608;
  PCD_BEGIN_ORIENTATION = 194635;
  PCD_BEGIN = 2048;

  PCD_MAGIC = 'PCD_IPI';

//----------------------------------------------------------------------------------------------------------------------

class function TPCDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

var
  ID1, ID2 : PAnsiChar;

begin
  Result := Size > 3 * $800;
  if Result then
  begin
    ID1 := Memory;
    ID2 := ID1 + $800;
    Result := (StrLComp(ID1, 'PCD_OPA', 7) = 0) or (StrLComp(ID2, 'PCD', 3) = 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPCDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 2);

var
  C1, C2, YY: PAnsiChar;
  YCbCrData: array[0..2] of PAnsiChar;
  {SourceDummy,
  DestDummy: Pointer;}

  Offset, I,
  X, Y,
  Rows: Integer;
  Columns: Cardinal;
  ScanLines: array of Pointer;

  LineBuffer: Pointer;
  Line,
  Run: PBGR;
  Decoder: TPCDDecoder;

  Source: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Source := Memory;
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
      Columns := 192 shl Min(ImageIndex, 2);
      Rows := 128 shl Min(ImageIndex, 2);

      // since row and columns might be swapped because of rotated images
      // we determine the final dimensions once more
      Width := 192 shl ImageIndex;
      Height := 128 shl ImageIndex;

      ZeroMemory(@YCbCrData, SizeOf(YCbCrData));
      try
        GetMem(YCbCrData[0], Width * Height);
        GetMem(YCbCrData[1], Width * Height);
        GetMem(YCbCrData[2], Width * Height);

        // advance to image data 
        Offset := 96;
        if Overview then
          Offset := 5
        else
          if ImageIndex = 1 then
            Offset := 23
          else
            if ImageIndex = 0 then
              Offset := 4;
        Inc(Source, Offset * $800);

        // color conversion setup
        with ColorManager do
        begin
          SourceColorScheme := csPhotoYCC;
          SourceBitsPerSample := 8;
          SourceSamplesPerPixel := 3;
          TargetColorScheme := csBGR;
          TargetBitsPerSample := 8;
          TargetSamplesPerPixel := 3;
        end;
        PixelFormat := pf24Bit;
        // PhotoYCC format uses CCIR Recommendation 709 coefficients and is subsampled
        // by factor 2 vertically and horizontally
        ColorManager.SetYCbCrParameters([0.2125, 0.7154, 0.0721], 2, 2);

        Progress(Self, psEnding, 0, False, FProgressRect, '');

        if Overview then
        begin
          // if Overview then ... no info yet about overview image structure
        end
        else
        begin
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];
          I := 0;
          Progress(Self, psStarting, 0, False, FProgressRect, gesLoadingData);
          while I < Rows do
          begin
            Progress(Self, psRunning, MulDiv(I, 100, Rows), False, FProgressRect, '');

            Move(Source^, YY^, Columns);
            Inc(YY, Width);
            Inc(Source, Columns);

            Move(Source^, YY^, Columns);
            Inc(YY, Width);
            Inc(Source, Columns);

            Move(Source^, C1^, Columns shr 1);
            Inc(C1, Width);
            Inc(Source, Columns shr 1);

            Move(Source^, C2^, Columns shr 1);
            Inc(C2, Width);
            Inc(Source, Columns shr 1);

            Inc(I, 2);
          end;
          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesUpsampling);
          // Y stands here for maximum number of upsample calls.
          Y := 5;
          if ImageIndex >= 3 then
          begin
            Inc(Y, 3 * (ImageIndex - 3));

            Decoder := TPCDDecoder.Create(Source);
            //SourceDummy := @YCbCrData;
            //DestDummy := nil;
            try
              // Recover luminance deltas for 1536 x 1024 image.
              Progress(Self, psRunning, MulDiv(0, 100, Y), False, FProgressRect, '');
              Upsample(768, 512, Width, YCbCrData[0]);
              Progress(Self, psRunning, MulDiv(1, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[1]);
              Progress(Self, psRunning, MulDiv(2, 100, Y), False, FProgressRect, '');
              Upsample(384, 256, Width, YCbCrData[2]);

              // The decoder does not work as expected. Larger resolutions are not loaded but created by scaling.
              //Decoder.Decode(SourceDummy, DestDummy, Width, 1024);
              if ImageIndex >= 4 then
              begin
                // recover luminance deltas for 3072 x 2048 image
                Progress(Self, psRunning, MulDiv(3, 100, Y), False, FProgressRect, '');
                Upsample(1536, 1024, Width, YCbCrData[0]);
                Progress(Self, psRunning, MulDiv(4, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[1]);
                Progress(Self, psRunning, MulDiv(5, 100, Y), False, FProgressRect, '');
                Upsample(768, 512, Width, YCbCrData[2]);

                //Decoder.Decode(SourceDummy, DestDummy, Width, 2048);
                if ImageIndex = 5 then
                begin
                  // recover luminance deltas for 6144 x 4096 image (vaporware)
                  Progress(Self, psRunning, MulDiv(6, 100, Y), False, FProgressRect, '');
                  Upsample(3072, 2048, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(7, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[1]);
                  Progress(Self, psRunning, MulDiv(8, 100, Y), False, FProgressRect, '');
                  Upsample(1536, 1024, Width, YCbCrData[2]);
                end;
              end;
            finally
              FreeAndNil(Decoder);
            end;
          end;

          Progress(Self, psRunning, MulDiv(Y - 1, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[1]);
          Progress(Self, psRunning, MulDiv(Y, 100, Y), False, FProgressRect, '');
          Upsample(Width shr 1, Height shr 1, Width, YCbCrData[2]);

          Progress(Self, psEnding, 0, False, FProgressRect, '');

          Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
          // transfer luminance and chrominance channels
          YY := YCbCrData[0];
          C1 := YCbCrData[1];
          C2 := YCbCrData[2];

          // For the rotated mode where we need to turn the image by 90°. We can speed up loading
          // the image by factor 2 by using a local copy of the Scanline pointers.
          if Rotate in [1, 3] then
          begin
            Self.Width := Height;
            Self.Height := Width;
            FProgressRect.Right := Height;
            
            SetLength(ScanLines, Width);
            for Y := 0 to Width - 1 do
              ScanLines[Y] := ScanLine[Y];
            GetMem(LineBuffer, 3 * Width);
          end
          else
          begin
            ScanLines := nil;
            Self.Width := Width;
            Self.Height := Height;
            LineBuffer := nil;
          end;

          try
            case Rotate of
              1: // rotate -90° 
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PByte(Line) := PByte(PAnsiChar(ScanLines[Width - X - 1]) + Y * 3);
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
              3: // rotate 90°
                begin
                  for Y := 0 to Height - 1 do
                  begin
                    ColorManager.ConvertRow([YY, C1, C2], LineBuffer, Width, $FF);
                    Inc(YY, Width);
                    Inc(C1, Width);
                    Inc(C2, Width);

                    Run := LineBuffer;
                    for X := 0 to Width - 1 do
                    begin
                      PByte(Line) := PByte(PAnsiChar(ScanLines[X]) + (Height - Y - 1) * 3);
                      Line^ := Run^;
                      Inc(Run);
                    end;

                    Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                    OffsetRect(FProgressRect, 0, 1);
                  end;
                end;
            else
              for Y := 0 to Height - 1 do
              begin
                ColorManager.ConvertRow([YY, C1, C2], ScanLine[Y], Width, $FF);
                Inc(YY, Width);
                Inc(C1, Width);
                Inc(C2, Width);

                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            end;
            Progress(Self, psEnding, 0, False, FProgressRect, '');
          finally
            ScanLines := nil;
            if Assigned(LineBuffer) then
              FreeMem(LineBuffer);
          end;
        end;

      finally
        if Assigned(YCbCrData[2]) then
          FreeMem(YCbCrData[2]);
        if Assigned(YCbCrData[1]) then
          FreeMem(YCbCrData[1]);
        if Assigned(YCbCrData[0]) then
          FreeMem(YCbCrData[0]);
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PCD']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPCDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: PAnsiChar;
  Temp: Cardinal;

begin
  if ImageIndex > 5 then
    ImageIndex := 5;
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex) and (Size > 3 * $800);

  if Result then
    with FImageProperties do
    begin
      Header := Memory;

      Overview := StrLComp(Header, 'PCD_OPA', 7) = 0;
      // determine if image is a PhotoCD image
      if Overview or (StrLComp(Header + $800, 'PCD', 3) = 0) then
      begin
        Rotate := Byte(Header[$0E02]) and 3;

        // image sizes are fixed, depending on the given image index
        if Overview then
          ImageIndex := 0;
        Width := 192 shl ImageIndex;
        Height := 128 shl ImageIndex;
        if (Rotate = 1) or (Rotate = 3) then
        begin
          Temp := Width;
          Width := Height;
          Height := Temp;
        end;
        ColorScheme := csPhotoYCC;
        BitsPerSample := 8;
        SamplesPerPixel := 3;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;
        if ImageIndex > 2 then
          Compression := ctPCDHuffmann
        else
          Compression := ctNone;

        if Overview then
          ImageCount := (Byte(Header[10]) shl 8) or Byte(Header[11])
        else
          ImageCount := 5; // These are the always present image resolutions.

        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif PCDGraphic}

//----------------- TPPMGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableMapGraphic}

class function TPPMGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > 10;
  if Result then
  begin
    // These are weak criteria here, but there is nothing more to test for this image format.
    Result := (PAnsiChar(Memory)^ = 'P') and (PAnsiChar(Memory)[1] in ['1'..'6']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetChar: AnsiChar;

begin
  if FRemainingSize = 0 then
    GraphicExError(gesStreamReadError, ['PPM']);
  Result := FSource^;
  Inc(FSource);
  Dec(FRemainingSize);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.GetNumber: Cardinal;

// reads the next number from the stream (and skips all characters which are not in 0..9)

var
  Ch: AnsiChar;

begin
  // skip all non-numbers
  repeat
    Ch := GetChar;
    // skip comments
    if Ch = '#' then
    begin
      ReadLine;
      Ch := GetChar;
    end;
  until Ch in ['0'..'9'];

  // read the number characters and convert meanwhile
  Result := 0;
  repeat
    Result := 10 * Result + Ord(Ch) - $30;
    Ch := GetChar;
  until not (Ch in ['0'..'9']);
end;

//------------------------------------------------------------------------------

function TPPMGraphic.GetByteFromChar: Byte;
begin
  Result := Byte(GetChar());
end;

//------------------------------------------------------------------------------

function TPPMGraphic.GetByteFromNumber: Byte;
begin
  Result := Byte(GetNumber());
end;

//------------------------------------------------------------------------------

function TPPMGraphic.ReadLine: AnsiString;

// reads one text line from stream and skips comments

var
  Ch: AnsiChar;
  I: Integer;

begin
  Result := '';
  repeat
    Ch := GetChar;
    if Ch in [#13, #10] then
      Break
    else
      Result := Result + Ch;
  until False;
  // eat #13#10 combination
  if (Ch = #13) and (FSource = #10) then
    GetChar;

  // delete comments
  I := Pos(AnsiString('#'), Result);
  if I > 0 then
    Delete(Result, I, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPPMGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Line24: PBGR;
  Line8: PByte;
  {$IFDEF FPC}
  LineBuf: PByte;
  {$ENDIF}
  X, Y: Integer;
  Pixel: Byte;
  MaxVal: Word;
  PpmType: Integer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    FSource := Memory;
    FRemainingSize := Size;
    with FImageProperties do
    begin
      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      if GetChar <> 'P' then
        GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);

      PpmType := StrToInt(String(GetChar));
      if PpmType in [1..3] then
        // ASCII format
        FGetByte := GetByteFromNumber
      else
        // Binary format
        FGetByte := GetByteFromChar;
      case PpmType of
        1: // PBM ASCII format (black & white)
          begin
            PixelFormat := pf1Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 1;
            Palette := ColorManager.CreateGrayScalePalette(True);
            {$IFDEF FPC}
            // Fpc seems to not use the palette for deciding which value is black or white.
            // This means we will have to convert the color scheme.
            // We could just swap all 0's and 1's but instead we go for BGR.

            // Needs to be done after creating palette since it uses the values
            // of TargetBitsPerSample and TargetSamplesPerPixel
            ColorManager.TargetSamplesPerPixel := 3;
            ColorManager.TargetBitsPerSample := 8;
            ColorManager.TargetColorScheme := csBGR;
            ColorManager.SourceBitsPerSample := 1;
            ColorManager.SourceSamplesPerPixel := 1;
            ColorManager.SourceColorScheme := csG;
            ColorManager.SourceOptions := ColorManager.SourceOptions + [coMinIsWhite];
            PixelFormat := pf24Bit;

            GetMem(LineBuf, Width div 8 + 1);
            try
            {$ENDIF}

              // read image data
              for Y := 0 to Height - 1 do
              begin
                {$IFNDEF FPC}
                Line8 := ScanLine[Y];
                {$ELSE}
                Line8 := LineBuf;
                {$ENDIF}
                Pixel := 0;
                for X := 1 to Width do
                begin
                  Pixel := (Pixel shl 1) or (GetNumber and 1);
                  if (X mod 8) = 0 then
                  begin
                    Line8^ := Pixel;
                    Inc(Line8);
                    Pixel := 0;
                  end;
                  if (Width mod 8) <> 0 then
                    Line8^ := Pixel shl (8 - (Width mod 8));
                end;

                {$IFDEF FPC}
                ColorManager.ConvertRow([LineBuf], ScanLine[Y], Width, $FF);
                {$ENDIF}
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            {$IFDEF FPC}
            finally
              FreeMem(LineBuf);
            end;
            {$ENDIF}
          end;
        4: // PBM binary format (black & white)
          begin
            PixelFormat := pf1Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 1;
            Palette := ColorManager.CreateGrayScalePalette(True);
            {$IFDEF FPC}
            // Fpc seems to not use the palette for deciding which value is black or white.
            // This means we will have to convert the color scheme.
            // We could just swap all 0's and 1's but instead we go for BGR.

            // Needs to be done after creating palette since it uses the values
            // of TargetBitsPerSample and TargetSamplesPerPixel
            ColorManager.TargetSamplesPerPixel := 3;
            ColorManager.TargetBitsPerSample := 8;
            ColorManager.TargetColorScheme := csBGR;
            ColorManager.SourceBitsPerSample := 1;
            ColorManager.SourceSamplesPerPixel := 1;
            ColorManager.SourceColorScheme := csG;
            ColorManager.SourceOptions := ColorManager.SourceOptions + [coMinIsWhite];
            PixelFormat := pf24Bit;

            GetMem(LineBuf, Width div 8 + 1);
            try
            {$ENDIF}

              // read image data
              for Y := 0 to Height - 1 do
              begin
                {$IFNDEF FPC}
                Line8 := ScanLine[Y];
                {$ELSE}
                Line8 := LineBuf;
                {$ENDIF}
                for X := 0 to (Width div 8) - 1 do
                begin
                  Line8^ := Byte(GetChar);
                  Inc(Line8);
                end;
                if (Width mod 8) <> 0 then
                  Line8^ := Byte(GetChar);

                {$IFDEF FPC}
                ColorManager.ConvertRow([LineBuf], ScanLine[Y], Width, $FF);
                {$ENDIF}
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            {$IFDEF FPC}
            finally
              FreeMem(LineBuf);
            end;
            {$ENDIF}
          end;
        2, // PGM ASCII form (gray scale)
        5: // PGM binary form (gray scale)
          begin
            {$IFNDEF FPC}
            PixelFormat := pf8Bit;
            {$ENDIF}
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            // skip maximum color value
            GetNumber;
            ColorManager.TargetSamplesPerPixel := 1;
            ColorManager.TargetBitsPerSample := 8;
            Palette := ColorManager.CreateGrayScalePalette(False);
            {$IFDEF FPC}
            // Needs to be done after creating palette since it uses the values
            // of TargetBitsPerSample and TargetSamplesPerPixel
            ColorManager.TargetSamplesPerPixel := 3;
            ColorManager.TargetColorScheme := csBGR;
            ColorManager.SourceSamplesPerPixel := SamplesPerPixel;
            ColorManager.SourceBitsPerSample := BitsPerSample;
            ColorManager.SourceColorScheme := csG;
            PixelFormat := pf24Bit;

            GetMem(LineBuf, Width);
            try
            {$ENDIF}

              // read image data
              for Y := 0 to Height - 1 do
              begin
                {$IFNDEF FPC}
                Line8 := ScanLine[Y];
                {$ELSE}
                Line8 := LineBuf;
                {$ENDIF}
                for X := 0 to Width - 1 do
                begin
                  Line8^ := FGetByte();
                  Inc(Line8);
                end;

                {$IFDEF FPC}
                ColorManager.ConvertRow([LineBuf], ScanLine[Y], Width, $FF);
                {$ENDIF}
                Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
                OffsetRect(FProgressRect, 0, 1);
              end;
            {$IFDEF FPC}
            finally
              FreeMem(LineBuf);
            end;
            {$ENDIF}
          end;
        3, // PPM ASCII form (true color)
        6: // PPM binary form (true color)
          begin
            PixelFormat := pf24Bit;
            Self.Width := GetNumber;
            Self.Height := GetNumber;
            MaxVal := GetNumber;

            // Pixel values are store linearly (but RGB instead BGR).
            // There's one allowed white space which will automatically be skipped by the first
            // GetChar call below
            // now read the pixels
            for Y := 0 to Height - 1 do
            begin
              Line24 := ScanLine[Y];
              if MaxVal = 255 then
                for X := 0 to Width - 1 do
                begin
                  Line24.R := FGetByte();
                  Line24.G := FGetByte();
                  Line24.B := FGetByte();
                  Inc(Line24);
                end
              else if MaxVal < 255 then
                for X := 0 to Width - 1 do
                begin
                  // These floating point calculations are the same as the GIMP's PPM scaling.
                  // Precomputing 255/MaxVal or using MulDiv both give slightly different results
                  // due to precision differences.
                  // Paint Shop Pro's calculations for MaxVal < 255 are screwed up, and I couldn't
                  // figure out the exact algorithm they're using.
                  Line24.R := Trunc(FGetByte() * 255 / MaxVal);
                  Line24.G := Trunc(FGetByte() * 255 / MaxVal);
                  Line24.B := Trunc(FGetByte() * 255 / MaxVal);
                  Inc(Line24);
                end
              else
                GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
                // TODO: PPM does support a MaxVal up to 65535, but I don't have any sample files to test
                // JB: If we want to implement this we will have to add a FGetWord function variable
//                for X := 0 to Width - 1 do
//                begin
//                  Line24.R := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Line24.G := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Line24.B := Trunc(Byte(GetChar) shl 8 + Byte(GetChar), 255, MaxVal);
//                  Inc(Line24);
//                end;

              Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end;
        else
          GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PBM, PGM or PPM']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPPMGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      FSource := Memory;
      FRemainingSize := Size;

      Compression := ctNone;

      if GetChar = 'P' then
      begin
        case StrToInt(String(GetChar)) of
          1: // PBM ASCII format (black & white)
            begin
              Width := GetNumber;
              Height := GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 1;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
              Include(Options, ioMinIsWhite);
            end;
          2: // PGM ASCII form (gray scale)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 8;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          3: // PPM ASCII form (true color)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 3;
              BitsPerSample := 8;
              ColorScheme := csRGB;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          4: // PBM binary format (black & white)
            begin
              Width := GetNumber;
              Height := GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 1;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
              Include(Options, ioMinIsWhite);
            end;
          5: // PGM binary form (gray scale)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 1;
              BitsPerSample := 8;
              ColorScheme := csIndexed;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
          6: // PPM binary form (true color)
            begin
              Width := GetNumber;
              Height := GetNumber;
              // skip maximum color value
              GetNumber;

              SamplesPerPixel := 3;
              BitsPerSample := 8;
              ColorScheme := csRGB;
              BitsPerPixel := SamplesPerPixel * BitsPerSample;
            end;
        else
          Result := False;
        end;
      end
      else
        Result := False;
    end;
end;

{$endif PortableMapGraphic}

//----------------- TCUTGraphic ----------------------------------------------------------------------------------------

{$ifdef CUTGraphic}

class function TCUTGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

// Note: cut files cannot be determined from stream because the only information
//       is width and height of the image at stream/image start which is by no means
//       enough to identify a cut (or any other) image.

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

// Set Default name of Palette file unless FPaletteFile already has a name
procedure TCUTGraphic.SetDefaultPaletteFile(const FileName: string);
begin
  if FPaletteFile = '' then
    FPaletteFile := ChangeFileExt(FileName, '.pal');
end;

procedure TCUTGraphic.LoadFromFile(const FileName: string);

// Overridden to extract an implicit palette file name.

begin
  SetDefaultPaletteFile(FileName);
  inherited;
end;

procedure TCUTGraphic.LoadFromFileByIndex(const FileName: string; ImageIndex: Cardinal = 0);
begin
  SetDefaultPaletteFile(FileName);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCUTGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Source: PByte;
  Line: Pointer;
  Decoder: TCUTRLEDecoder;
  Y: Integer;
  {$IFDEF FPC}
  LineBuf: PByte;
  {$ENDIF}
  LogPalette: TMaxLogPalette;
  CompressedSize: Word;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Source := Pointer(PAnsiChar(Memory) + 6);

      FProgressRect := Rect(0, 0, Width, 0);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      {$IFNDEF FPC}
      PixelFormat := pf8Bit;
      {$ELSE}
      PixelFormat := pf24Bit;
      ColorManager.SourceBitsPerSample := BitsPerSample;
      ColorManager.SourceSamplesPerPixel := SamplesPerPixel;
      ColorManager.SourceColorScheme := csIndexed;
      ColorManager.TargetBitsPerSample := 8;
      ColorManager.TargetSamplesPerPixel := 3;
      ColorManager.TargetColorScheme := csBGR;
      {$ENDIF}
      Self.Width := Width;
      Self.Height := Height;
      // TODO: We should first decode the image data and depending on the
      // amount of palette indexes used determine the palette size and what
      // to use for what indexes. e.g. I have examples using probably only 2
      // colors black/white that now get 2 very similar blacks instead of
      // 1 index black and 1 white.
      LogPalette := LoadPalette;

      {$IFDEF FPC}
      ColorManager.SetSourcePalette([@LogPalette.palPalEntry], pfInterlaced8Quad);
      GetMem(LineBuf, Width);
      {$ENDIF}
      Decoder := TCUTRLEDecoder.Create;
      try
        for Y := 0 to Height - 1 do
        begin
          {$IFNDEF FPC}
          Line := ScanLine[Y];
          {$ELSE}
          Line := LineBuf;
          {$ENDIF}
          // Length in bytes of compressed data.
          CompressedSize := PWord(Source)^;
          Inc(Source, 2);
          // Decode one line.
          Decoder.Decode(Pointer(Source), Line, CompressedSize, Width);
          // Check that the correct amount of data got decompressed.
          if (Decoder.CompressedBytesAvailable <> 0) or (Decoder.DecompressedBytes <> Width) then
            GraphicExError(gesDecompression, ['CUT']);

          {$IFDEF FPC}
          ColorManager.ConvertRow([LineBuf], ScanLine[Y], Width, $FF);
          {$ENDIF}
          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        FreeAndNil(Decoder);
        {$IFDEF FPC}
        FreeMem(LineBuf);
        {$ENDIF}
      end;

      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCUTGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PWord;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      PixelFormat := pf8Bit;
      Run := Memory;
      Width := Run^;
      Inc(Run);
      Height := Run^;

      ColorScheme := csIndexed;
      BitsPerSample := 8;
      SamplesPerPixel := 1;
      BitsPerPixel := BitsPerSample * SamplesPerPixel;

      Compression := ctRLE;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  // the palette file header is actually more complex than the
  // image file's header, funny...
  PHaloPaletteHeader = ^THaloPaletteHeader;
  THaloPaletteHeader = packed record
    ID: array[0..1] of AnsiChar;  // should be 'AH'
    Version,
    Size: Word;
    FileType,
    SubType: Byte;
    BrdID,
    GrMode: Word;
    MaxIndex,
    MaxRed,
    MaxGreen,
    MaxBlue: Word; // colors = MaxIndex + 1
    Signature: array[0..7] of AnsiChar; // 'Dr. Halo'
    Filler: array[0..11] of Byte;
  end;

//----------------------------------------------------------------------------------------------------------------------

function TCUTGraphic.LoadPalette: TMaxLogPalette;

var
  Header: PHaloPaletteHeader;
  LogPalette: TMaxLogPalette;
  I: Integer;
  Buffer: array[0..511] of Byte;
  Run: PWord;

begin
  LogPalette.palVersion := $300;
  if FileExists(FPaletteFile) then
  begin
    with TFileStream.Create(FPaletteFile, fmOpenRead or fmShareDenyNone) do
    try
      // quite strange file organization here, we need always to load 512 bytes blocks
      // and skip occasionally some bytes
      ReadBuffer(Buffer, SizeOf(Buffer));
      Header := @Buffer;
      LogPalette.palNumEntries := Header.MaxIndex + 1;
      Run := @Buffer;
      Inc(PByte(Run), SizeOf(Header^));
      for I := 0 to LogPalette.palNumEntries - 1 do
      begin
        // load next 512 bytes buffer if necessary
        if (NativeInt(Run) - NativeInt(@Buffer)) > 506 then
        begin
          ReadBuffer(Buffer, SizeOf(Buffer));
          Run := @Buffer;
        end;
        LogPalette.palPalEntry[I].peRed := Byte(Run^);
        Inc(Run);
        LogPalette.palPalEntry[I].peGreen := Byte(Run^);
        Inc(Run);
        LogPalette.palPalEntry[I].peBlue := Byte(Run^);
        Inc(Run);
      end;
    finally
      Free;
    end;
  end
  else
  begin
    LogPalette.palNumEntries := 256;
    // no external palette so use gray scale
    for I := 0 to 255 do
    begin
      LogPalette.palPalEntry[I].peBlue := I;
      LogPalette.palPalEntry[I].peGreen := I;
      LogPalette.palPalEntry[I].peRed := I;
    end;
  end;

  Result := LogPalette;
  // finally create palette
  Palette := CreatePalette(PLogPalette(@LogPalette)^);
end;

{$endif CUTGraphic}

//----------------- TGIFGraphic ----------------------------------------------------------------------------------------

{$ifdef GIFGraphic}

const
  // logical screen descriptor packed field masks
  GIF_GLOBALCOLORTABLE = $80;
  GIF_COLORRESOLUTION = $70;
  GIF_GLOBALCOLORTABLESORTED = $08;
  GIF_COLORTABLESIZE = $07;

  // image flags
  GIF_LOCALCOLORTABLE = $80;
  GIF_INTERLACED = $40;
  GIF_LOCALCOLORTABLESORTED= $20;

  // block identifiers
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = Ord(',');
  GIF_EXTENSIONINTRODUCER = Ord('!');
  GIF_TRAILER = Ord(';');

  // Graphic Control Extension - Disposal method values etc.
  GIF_NO_DISPOSAL              = 0;  // 0
  GIF_DO_NOT_DISPOSE           = 4;  // 1
  GIF_RESTORE_BACKGROUND_COLOR = 8;  // 2
  GIF_RESTORE_PREVIOUS         = 12; // 3
  GIF_DISPOSAL_ALL             = 28; // bits 2-4 ($1C)
  GIF_USER_INPUT_FLAG          = 2;
  GIF_TRANSPARENT_COLOR_FLAG   = 1;

type
  PGIFHeader = ^TGIFHeader;
  TGIFHeader = packed record
    Signature: array[0..2] of AnsiChar; // magic ID 'GIF'
    Version: array[0..2] of AnsiChar;   // '87a' or '89a'
  end;

  TLogicalScreenDescriptor = packed record
    ScreenWidth: Word;
    ScreenHeight: Word;
    PackedFields,
    BackgroundColorIndex, // index into global color table
    AspectRatio: Byte;    // actual ratio = (AspectRatio + 15) / 64
  end;

  TImageDescriptor = packed record
    //Separator: Byte; // leave that out since we always read one bye ahead
    Left: Word;		 // X position of image with respect to logical screen
    Top: Word;		 // Y position
    Width: Word;
    Height: Word;
    PackedFields: Byte;
  end;

  TGraphicControlExtension = packed record
    PackedFields: Byte;
    DelayTime: Word;
    TransparentColorIndex: Byte;
  end;
  PGraphicControlExtension = ^TGraphicControlExtension;

  TAppExtensionDescriptor = packed record
    AppID: array [0..7] of AnsiChar;
    AppAuthenticationCode: array [0..2] of Byte;
  end;
  PAppExtensionDescriptor = ^TAppExtensionDescriptor;

constructor TGIFGraphic.Create;
begin
  inherited Create;
  FApplicationExtensions := TStringList.Create;
end;

destructor TGIFGraphic.Destroy;
begin
  FApplicationExtensions.Free;
  FMem.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TGIFGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := (Size > (SizeOf(TGIFHeader) + SizeOf(TLogicalScreenDescriptor) + SizeOf(TImageDescriptor))) and
    (StrLIComp(PAnsiChar(Memory), 'GIF', 3) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.SkipExtensions(IsTargetImage: Boolean = True): Byte;

// Skips all blocks until an image block has been found in the data stream.
// Result is the image block ID if an image block could be found.

var
  Increment: Byte;
  Content : array[0..255] of AnsiChar; // Gif comment sub-block has a maximum size of 255 bytes
  GotApp, NeedApp: Boolean;
  GraphicExtension: TGraphicControlExtension;
  AppExtensionDescriptor: TAppExtensionDescriptor;
begin
  FImageProperties.Comment := '';
  // Flag to check that we don't read app extensions twice because LoadFromMemory call ReadImageProperties
  NeedApp := FApplicationExtensions.Count = 0;

  // Iterate through the blocks until first image is found.
  repeat
    Result := FMem.GetByte();
    if Result = GIF_EXTENSIONINTRODUCER then
    begin
      // Read the block control label and act accordingly.
      Result := FMem.GetByte();
      case Result of
        GIF_PLAINTEXT:
          begin
            // Block size of text grid data.
            Increment := FMem.GetByte();
            FMem.SeekForward(Increment);
            // Skip variable length text block.
            repeat
              // Block size.
              Increment := FMem.GetByte();
              if Increment = 0 then
                Break;
              FMem.SeekForward(Increment);
            until False;
          end;
        GIF_GRAPHICCONTROLEXTENSION:
          begin
            // Block size.
            Increment := FMem.GetByte();
            if Increment > 0 then
            begin
              // Size should always be 4 so this is just a failsafe
              if Increment = 4 then begin
                FMem.GetBytes(GraphicExtension, SizeOf(TGraphicControlExtension));
                // The graphic control extension includes the transparency flag.
                // Read this and the transparency color index.
                if (GraphicExtension.PackedFields and GIF_TRANSPARENT_COLOR_FLAG) <> 0 then
                begin
                  // Image is transparent, read index.
                  Transparent := True;
                  FTransparentIndex := GraphicExtension.TransparentColorIndex;
                  if IsTargetImage then begin
                    FGifInformation.TransparentColorIndex := FTransparentIndex;
                    Include(FGifInformation.Flags, gfHasTransparentColor);
                  end;
                end;
                if IsTargetImage then begin
                  FGifInformation.DelayTime := GraphicExtension.DelayTime;
                  if (GraphicExtension.PackedFields and GIF_USER_INPUT_FLAG) <> 0 then
                    Include(FGifInformation.Flags, gfUserInput);
                  FGifInformation.Disposal := TGifDisposalFlag(Byte(GraphicExtension.PackedFields
                    and GIF_DISPOSAL_ALL) shr 2);
                end;
              end
              else
                FMem.SeekForward(Increment);
            end;
            // Finally skip terminator.
            FMem.SeekForward(1);
          end;
        GIF_COMMENTEXTENSION:
          repeat
            // block size
            Increment := FMem.GetByte();
            if Increment = 0 then
              Break;
            FMem.GetBytes(Content, Increment);
            Content[Increment] := #0;
            FImageProperties.Comment := FImageProperties.Comment + Content;
          until False;
        GIF_APPLICATIONEXTENSION:
          begin
            // application id and authentication code plus potential application data
            GotApp := not NeedApp;
            repeat
              Increment := FMem.GetByte();
              if Increment = 0 then
                Break;
              if not GotApp and (Increment = SizeOf(TAppExtensionDescriptor)) then begin
                FMem.GetBytes(AppExtensionDescriptor, SizeOf(TAppExtensionDescriptor));
                FApplicationExtensions.Add(AppExtensionDescriptor.AppID);
                GotApp := True;
              end
              else
                FMem.SeekForward(Increment);
            until False;
          end;
      end;
    end;
  until (Result = GIF_IMAGEDESCRIPTOR) or (Result = GIF_TRAILER);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGIFGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  LogPalette: TMaxLogPalette;
  I: Integer;
  BlockID: Byte;
  InitCodeSize: Byte;
  RawData,
  Run: PByte;
  TargetBuffer,
  TargetRun,
  Line: Pointer;
  Pass,
  Increment: Integer;
  SavedPosition: UInt64;
  TransValid: Boolean;
  TransColor: TColor; // TransparentColor is already taken by TBitmap

  // Global and Local color table have the same layout and the PackedFields too
  // thus we can make a function that handles both.
  procedure ReadPalette(APackedFields: Byte);
  var I: Integer;
    {$IFDEF FPC}
    FPalettePtr: Pointer;
    {$ENDIF}
  begin
    // Read color table if given.
    // Note we can use GIF_GLOBALCOLORTABLE als for the local color table
    // because the same values are used for both.
    if (APackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
    begin
      LogPalette.palNumEntries := 2 shl (APackedFields and GIF_COLORTABLESIZE);
      {$IFDEF FPC}
      // TODO: This should be changed so that the actual palette info will be copied!
      FPalettePtr := FMem.GetAccessToMemory(3*LogPalette.palNumEntries);
      ColorManager.SetSourcePalette([FPalettePtr], pfInterlaced8Triple);
      {$ENDIF}
      for I := 0 to LogPalette.palNumEntries - 1 do
      begin
        LogPalette.palPalEntry[I].peRed := FMem.GetByte();
        LogPalette.palPalEntry[I].peGreen := FMem.GetByte();
        LogPalette.palPalEntry[I].peBlue := FMem.GetByte();
      end;
      // Finally create the palette.
      Palette := CreatePalette(PLogPalette(@LogPalette)^);
    end;
  end;

begin
  inherited;

  FProgressRect := Rect(0, 0, FImageProperties.Width, 1);
  Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);
  if ReadImageProperties(Memory, Size, ImageIndex) then begin
    Transparent := False;

    // ReadImageProperties will have set our Memory Access handler
    if not Assigned(FMem) then // However it can't hurt to make sure
      Exit;
    // Since ReadImageProperties moved the current position reset it.
    FMem.SeekFromBeginning(0);
    FMem.GetBytes(Header, SizeOf(Header));

    PixelFormat := pf8Bit;
    {$IFDEF FPC}
    ColorManager.SourceColorScheme := FImageProperties.ColorScheme;
    // Source bits per sampel should always be 8 since apparently always a
    // whole byte is used even if bps is less than 8 (and 8 is the maximum).
    ColorManager.SourceBitsPerSample := 8;
    ColorManager.SourceSamplesPerPixel := FImageProperties.SamplesPerPixel;
    // fpc doesn't support indexed pf8Bit so we will have to convert
    // it to 24bits BGR
    ColorManager.TargetColorScheme := csBGR;
    ColorManager.TargetBitsPerSample := 8;
    ColorManager.TargetSamplesPerPixel := 3;
    PixelFormat := ColorManager.TargetPixelFormat;
    {$ENDIF}

    // Read general information.
    FMem.GetBytes(ScreenDescriptor, SizeOf(ScreenDescriptor));

    ZeroMemory(@LogPalette, SizeOf(LogPalette));
    LogPalette.palVersion := $300;
    // Read global color table if given.
    ReadPalette(ScreenDescriptor.PackedFields);

    // Now skip to the correct image we want to show
    if FImageOffset <> 0 then
      FMem.SeekFromBeginning(FImageOffset);

    // Read or skip extension info
    BlockID := SkipExtensions();

    TransValid := False;
    // SkipExtensions might have set the transparent property.
    if Transparent then
      // If transparent color index is valid then get transparent color.
      if FTransparentIndex < LogPalette.palNumEntries then begin
        TransValid := True;
        // We are not setting TBitmap's TransparentColor here since it seems to
        // cause some problems when reading in a thread.
        with LogPalette.palPalEntry[FTransparentIndex] do
          TransColor := RGB(peRed, peGreen, peBlue);
      end;

    Progress(Self, psEnding, 1, False, FProgressRect, '');

    // image found?
    if BlockID = GIF_IMAGEDESCRIPTOR then
    begin
      Progress(Self, psStarting, 1, False, FProgressRect, gesLoadingData);
      FMem.GetBytes(ImageDescriptor, SizeOf(TImageDescriptor));
      Self.Width := FImageProperties.Width;
      Self.Height := FImageProperties.Height;

      // if there is a local color table then override the already set one
      ReadPalette(ImageDescriptor.PackedFields);

      InitCodeSize := FMem.GetByte();
      // decompress data in one step
      // 1) count data
      SavedPosition := FMem.CurrentPosition;
      Pass := 0;
      repeat
        Increment := FMem.GetByte();
        Inc(Pass, Increment);
        FMem.SeekForward(Increment);
      until Increment = 0;

      // 2) Allocate memory for decompressed image
      GetMem(TargetBuffer, Width * Height);

      try
        // 3) Allocate memory for decompress buffer
        GetMem(RawData, Pass);
        try
          // 4) read and decode data
          FMem.SeekFromBeginning(SavedPosition);
          Run := RawData;
          repeat
            Increment := FMem.GetByte();
            FMem.GetBytes(Run^, Increment);
            Inc(Run, Increment);
          until Increment = 0;

          Decoder := TGIFLZWDecoder.Create(InitCodeSize);
          try
            Run := RawData;
            Decoder.Decode(Pointer(Run), TargetBuffer, Pass, Width * Height);
            if Decoder.DecoderStatus <> dsOK then begin
              // Corrupt image. Since all errors get caught we could in principle
              // still show the image (in case part of it did get decoded), however
              // for safety it's probably better to always stop with an error.
              GraphicExError(gesDecompression, ['GIF']);
            end;
          finally
            FreeAndNil(Decoder);
          end;
        finally
          FreeMem(RawData);
        end;
        Progress(Self, psEnding, 50, True, FProgressRect, '');

        // finally transfer image data
        Progress(Self, psStarting, 25, False, FProgressRect, gesTransfering);
        Self.Canvas.Lock;
        try
          if TransValid then
            TransparentColor := TransColor;
          if (ImageDescriptor.PackedFields and GIF_INTERLACED) = 0 then
          begin
            TargetRun := TargetBuffer;
            for I := 0 to Height - 1 do
            begin
              Line := Scanline[I];
              {$IFNDEF FPC}
              Move(TargetRun^, Line^, Width);
              {$ELSE}
              ColorManager.ConvertRow(TargetRun, Line, Width, $FF);
              {$ENDIF}
              Inc(PByte(TargetRun), Width);

              Progress(Self, psRunning, 25 + MulDiv(I, 50, Height), True, FProgressRect, '');
              OffsetRect(FProgressRect, 0, 1);
            end;
          end
          else
          begin
            TargetRun := TargetBuffer;
            // interlaced image, need to move in four passes
            for Pass := 0 to 3 do
            begin
              // determine start line and increment of the pass
              case Pass of
                0:
                  begin
                    I := 0;
                    Increment := 8;
                  end;
                1:
                  begin
                    I := 4;
                    Increment := 8;
                  end;
                2:
                  begin
                    I := 2;
                    Increment := 4;
                  end;
              else
                I := 1;
                Increment := 2;
              end;

              while I < Height do
              begin
                Line := Scanline[I];
                {$IFNDEF FPC}
                Move(TargetRun^, Line^, Width);
                {$ELSE}
                ColorManager.ConvertRow(TargetRun, Line, Width, $FF);
                {$ENDIF}
                Inc(PByte(TargetRun), Width);
                Inc(I, Increment);

                if Pass = 3 then
                begin
                  // progress events only for last (and most expensive) run
                  Progress(Self, psRunning, 25 + MulDiv(I, 50, Height), True, FProgressRect, '');
                  OffsetRect(FProgressRect, 0, 1);
                end;
              end;
            end;
          end;
        finally
          Self.Canvas.Unlock;
        end;
      finally
        Progress(Self, psEnding, 0, False, FProgressRect, '');
        if Assigned(TargetBuffer) then
          FreeMem(TargetBuffer);
      end;
    end;
  end
  else begin
    Progress(Self, psEnding, 0, False, FProgressRect, '');
    GraphicExError(gesInvalidImage, ['GIF']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGIFGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TGIFHeader;
  ScreenDescriptor: TLogicalScreenDescriptor;
  ImageDescriptor: TImageDescriptor;
  BlockID: Integer;

  // Skip image contents and return next block ID
  function SkipImage(NeedDescriptor: Boolean): Byte;
  var
    ctSize, Increment: Cardinal;
    IDesc: TImageDescriptor;
    Flags: Byte;
  begin
    if NeedDescriptor then begin
      // Skip Image descriptor
      FMem.GetBytes(IDesc, SizeOf(TImageDescriptor));
      Flags := IDesc.PackedFields;
    end
    else
      Flags := ImageDescriptor.PackedFields;

    // Skip local color table if present
    if (Flags and GIF_LOCALCOLORTABLE) <> 0 then
    begin
      // Get size of color table and skip memory
      ctSize := 2 shl (Flags and GIF_COLORTABLESIZE);
      FMem.SeekForward(3*ctSize);
    end;
    // Skip "InitCodeSize" compression marker
    FMem.SeekForward(1);
    // Skip decompression data
    repeat
      Increment := FMem.GetByte();
      FMem.SeekForward(Increment);
    until Increment = 0;
    Result := FMem.GetByte();
  end;

begin
  FImageOffset := 0;
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then begin
    if not Assigned(FMem) then
      FMem := TMemoryAccess.Create(Memory, Size, 'GIF');
    FMem.GetBytes(Header, SizeOf(Header));
    if UpperCase(Header.Signature) = 'GIF' then
    begin
      FImageProperties.Version := StrToInt(Copy(Header.Version, 1, 2));
      FImageProperties.ColorScheme := csIndexed;
      FImageProperties.SamplesPerPixel := 1;
      // might be overwritten
      FImageProperties.BitsPerSample := 8;
      FImageProperties.Compression := ctLZW;

      // general information
      FMem.GetBytes(ScreenDescriptor, SizeOf(ScreenDescriptor));
      // Copy info to our public GifInformation record
      FGifInformation.CanvasWidth := ScreenDescriptor.ScreenWidth;
      FGifInformation.CanvasHeight := ScreenDescriptor.ScreenHeight;
      // Background color index only valid if global color table present
      FGifINformation.BackgroundColorIndex := ScreenDescriptor.BackgroundColorIndex;
      FGifInformation.AspectRatio := ScreenDescriptor.AspectRatio;

      // Skip global color table if given.
      if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLE) <> 0 then
      begin
        Include(FGifInformation.Flags, gfHasGlobalColorTable);
        if (ScreenDescriptor.PackedFields and GIF_GLOBALCOLORTABLESORTED) <> 0 then
          Include(FGifInformation.Flags, gfGlobalSorted);
        FImageProperties.BitsPerSample := (ScreenDescriptor.PackedFields and GIF_COLORTABLESIZE) + 1;
        // The global color table immediately follows the screen descriptor.
        FMem.SeekForward(3 * (1 shl FImageProperties.BitsPerSample));
      end;

      if ImageIndex = 0 then
        FImageOffset := FMem.CurrentPosition;
      BlockID := SkipExtensions(ImageIndex = 0);

      // We want to know the number of images/frames so we will skip and count them
      repeat
        if BlockID = GIF_IMAGEDESCRIPTOR then begin
          if FImageProperties.ImageCount = ImageIndex then begin
            // Found the requested image
            FMem.GetBytes(ImageDescriptor, SizeOf(TImageDescriptor));

            FImageProperties.Width := ImageDescriptor.Width;
            if FImageProperties.Width = 0 then
              FImageProperties.Width := ScreenDescriptor.ScreenWidth;
            FImageProperties.Height := ImageDescriptor.Height;
            if FImageProperties.Height = 0 then
              FImageProperties.Height := ScreenDescriptor.ScreenHeight;

            FGifInformation.FrameLeft := ImageDescriptor.Left;
            FGifInformation.FrameTop := ImageDescriptor.Top;
            FGifInformation.FrameWidth := ImageDescriptor.Width;
            FGifInformation.FrameHeight := ImageDescriptor.Height;

            // if there is a local color table then override the already set one
            if (ImageDescriptor.PackedFields and GIF_LOCALCOLORTABLE) <> 0 then begin
              Include(FGifInformation.Flags, gfHasLocalColorTable);
              if (ScreenDescriptor.PackedFields and GIF_LOCALCOLORTABLESORTED) <> 0 then
                Include(FGifInformation.Flags, gfLocalSorted);
              FImageProperties.BitsPerSample := (ImageDescriptor.PackedFields and GIF_COLORTABLESIZE) + 1;
            end;
            FImageProperties.Interlaced := (ImageDescriptor.PackedFields and GIF_INTERLACED) <> 0;
            if FImageProperties.Interlaced then
              Include(FGifInformation.Flags, gfInterlaced);
          end;
          BlockID := SkipImage(FImageProperties.ImageCount <> ImageIndex);
          Inc(FImageProperties.ImageCount);
        end
        else if BlockID = GIF_EXTENSIONINTRODUCER then begin
          // Since SkipExtensions expects to read the ID byte we need to back up our position.
          FMem.SeekBackward(1); // TODO: Should be changed to not need this!
          // Set memory location of the image we want to show
          if FImageProperties.ImageCount = ImageIndex then
            FImageOffset := FMem.CurrentPosition;
          BlockID := SkipExtensions(FImageProperties.ImageCount = ImageIndex);
        end
        else begin
          // Since we don't know what to do here we will stop.
          // "Images" that only have a text block and no real image will arrive
          // here with BlockID = GIF_TRAILER.
          // All other cases shouldn't happen but might be caused by a broken image.
          BlockID := GIF_TRAILER; // Dummy to be able to set a breakpoint here.
        end;
      until BlockID = GIF_TRAILER;

      // Make sure Bits per Sample is valid
      if (FImageProperties.BitsPerSample < 1) or (FImageProperties.BitsPerSample > 8) then
        GraphicExError(gesInvalidBitsPerSample, ['GIF', FImageProperties.BitsPerSample]);
      FImageProperties.BitsPerPixel := FImageProperties.SamplesPerPixel * FImageProperties.BitsPerSample;
      Result := True;
    end
    else
      Result := False;
  end;
end;

{$endif GIFGraphic}

//----------------- TRLAGraphic ----------------------------------------------------------------------------------------

{$ifdef RLAGraphic}

// This implementation is based on code from Dipl. Ing. Ingo Neumann (ingo (AT) upstart.de, ingo_n (AT) dialup.nacamar.de).

type
  TRLAWindow = packed record
    Left,
    Right,
    Bottom,
    Top: SmallInt;
  end;

  PRLAHeader = ^TRLAHeader;
  TRLAHeader = packed record
    Window,                            // overall image size
    Active_window: TRLAWindow;         // size of non-zero portion of image (we use this as actual image size)
    Frame,                             // frame number if part of a sequence
    Storage_type,                      // type of image channels (0 - integer data, 1 - float data)
    Num_chan,                          // samples per pixel (usually 3: r, g, b)
    Num_matte,                         // number of matte channels (usually only 1)
    Num_aux,                           // number of auxiliary channels, usually 0
    Revision: SmallInt;                // always $FFFE
    Gamma: array[0..15] of AnsiChar;       // gamma single value used when writing the image
    Red_pri: array[0..23] of AnsiChar;     // used chromaticity for red channel (typical format: "%7.4f %7.4f")
    Green_pri: array[0..23] of AnsiChar;   // used chromaticity for green channel
    Blue_pri: array[0..23] of AnsiChar;    // used chromaticity for blue channel
    White_pt: array[0..23] of AnsiChar;    // used chromaticity for white point
    Job_num: Integer;                      // rendering speciifc
    Name: array[0..127] of AnsiChar;       // original file name
    Desc: array[0..127] of AnsiChar;       // a file description
    ProgramName: array[0..63] of AnsiChar; // name of program which created the image
    Machine: array[0..31] of AnsiChar;     // name of computer on which the image was rendered
    User: array[0..31] of AnsiChar;        // user who ran the creation program of the image
    Date: array[0..19] of AnsiChar;        // creation data of image (ex: Sep 30 12:29 1993)
    Aspect: array[0..23] of AnsiChar;      // aspect format of the file (external resource)
    Aspect_ratio: array[0..7] of AnsiChar; // float number Width /Height
    Chan: array[0..31] of AnsiChar;        // color space (can be: rgb, xyz, sampled or raw)
    Field: SmallInt;                       // 0 - non-field rendered data, 1 - field rendered data
    Time: array[0..11] of AnsiChar;        // time needed to create the image (used when rendering)
    Filter: array[0..31] of AnsiChar;      // filter name to post-process image data
    Chan_bits,                         // bits per sample
    Matte_type,                        // type of matte channel (see aux_type)
    Matte_bits,                        // precision of a pixel's matte channel (1..32)
    Aux_type,                          // type of aux channel (0 - integer data; 4 - single (float) data
    Aux_bits: SmallInt;                // bits precision of the pixel's aux channel (1..32 bits)
    Aux: array[0..31] of AnsiChar;     // auxiliary channel as either range or depth
    Space: array[0..35] of Byte;       // unused
    Next: Integer;                     // offset for next header if multi-frame image
  end;
  
//------------------------------------------------------------------------------

class function TRLAGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TRLAHeader);
  if Result then
    with PRLAHeader(Memory)^ do
      Result := ((Word(Revision) = $FEFF) or (Word(Revision) = $FDFF)) and
        ((StrLIComp(Chan, 'rgb', 3) = 0) or (StrLIComp(Chan, 'xyz', 3) = 0));
end;

//------------------------------------------------------------------------------

procedure TRLAGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0); 

var
  Offsets: TCardinalArray;
  RLELength: Word;
  Line: Pointer;
  Y: Integer;

  // RLE buffers
  RawBuffer,
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  AlphaBuffer: Pointer;
  Decoder: TRLADecoder;

  Run: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Run := Memory;

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);

      with ColorManager do
      begin
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;

        if SampleFormat = 3 then // Floating point
          ColorManager.SourceDataFormat := TSampleDataFormat(SampleFormat);

        SourceBitsPerSample := BitsPerSample;
        if BitsPerSample > 8 then
          TargetBitsPerSample := 8
        else
          TargetBitsPerSample := BitsPerSample;
        SourceColorScheme := ColorScheme;
        case ColorScheme of
          csRGBA, csGA: TargetColorScheme := csBGRA;
          csRGB, csXYZ: TargetColorScheme := csBGR;
          csG: TargetColorScheme := csBGR;
        else
        end;

        PixelFormat := TargetPixelFormat;

        if Abs(FileGamma) >= 0.01 then
        begin
          // Gamma is apparently already applied to the image in rla, meaning
          // we don't need to set it in TargetOptions.
          Include(Options, ioUseGamma);
        end;
        // Uses separate channels thus we need to set that in source options.
        ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
      end;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Self.Width := Width;
      Self.Height := Height;

      // Each scanline is organized in RLE compressed strips whose location in the stream
      // is determined by the offsets table.
      SetLength(Offsets, Height);
      Inc(Run, SizeOf(TRLAHeader)); // Offsets are located right after the header
      Move(Run^, Offsets[0], Height * SizeOf(Cardinal));
      Inc(Run, Height * SizeOf(Cardinal));
      SwapCardinalArrayEndian(PCardinal(Offsets), Height);

      // Setup intermediate storage.
      Decoder := TRLADecoder.Create;
      RawBuffer := nil;
      RedBuffer := nil;
      GreenBuffer := nil;
      BlueBuffer := nil;
      AlphaBuffer := nil;
      try
        GetMem(RedBuffer, Width);
        GetMem(GreenBuffer, Width);
        GetMem(BlueBuffer, Width);
        GetMem(AlphaBuffer, Width);

        // no go for each scanline
        for Y := 0 to Height - 1 do
        begin
          Run := Pointer(PAnsiChar(Memory) + Offsets[Y]);
          if Orientation = gexoBottomLeft then
            Line := ScanLine[Height - Y - 1]
          else // TopLeft
            Line := ScanLine[Y];
          // read channel data to decode
          // red
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := SwapEndian(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, RedBuffer, RLELength, Width);
          // green
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := SwapEndian(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, GreenBuffer, RLELength, Width);
          // blue
          Move(Run^, RLELength, SizeOf(RLELength));
          Inc(Run, SizeOf(RLELength));
          RLELength := SwapEndian(RLELength);
          RawBuffer := Run;
          Inc(Run, RLELength);
          Decoder.Decode(RawBuffer, BlueBuffer, RLELength, Width);

          if ColorManager.TargetColorScheme = csBGR then
          begin
            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer], Line, Width, $FF);
          end
          else
          begin
            // alpha
            Move(Run^, RLELength, SizeOf(RLELength));
            Inc(Run, SizeOf(RLELength));
            RLELength := SwapEndian(RLELength);
            Decoder.Decode(Pointer(Run), AlphaBuffer, RLELength, Width);

            ColorManager.ConvertRow([RedBuffer, GreenBuffer, BlueBuffer, AlphaBuffer], Line, Width, $FF);
          end;

          Progress(Self, psRunning, MulDiv(Y, 100, Height), True, FProgressRect, '');
          OffsetRect(FProgressRect, 0, 1);
        end;
      finally
        if Assigned(RedBuffer) then
          FreeMem(RedBuffer);
        if Assigned(GreenBuffer) then
          FreeMem(GreenBuffer);
        if Assigned(BlueBuffer) then
          FreeMem(BlueBuffer);
        if Assigned(AlphaBuffer) then
          FreeMem(AlphaBuffer);
        FreeAndNil(Decoder);
      end;
      Progress(Self, psEnding, 0, False, FProgressRect, '');
    end;
  end;
end;

//------------------------------------------------------------------------------

// RLA data can have leading spaces that we need to remove and we also need to
// convert the '.' decimal separator to the system default decimal separator.
function ConvertAnsiFloatToString(const s: string): string;
var i, j: Integer;
begin
  i := 1;
  j := 1;
  SetLength(Result, Length(s));
  while i <= Length(s) do begin
    if s[i] <> ' ' then begin
      if s[i] = '.' then begin
        {$IF Declared(CompilerVersion) AND (CompilerVersion >= 22)}
        // Note that we should use Declared here not Defined since that doesn't work.
        // Starting with Delphi VER220 = CompilerVersion 22 = XE use of FormatSettings is required.
        Result[j] := FormatSettings.DecimalSeparator;
        {$ELSE}
        Result[j] := DecimalSeparator;
        {$IFEND}
      end
      else
        Result[j] := s[i];
      Inc(j);
    end;
    Inc(i);
  end;
  SetLength(Result, j-1);
end;

function TRLAGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TRLAHeader;
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Header, SizeOf(Header));

      // data is always given in big endian order, so swap data which needs this
      SwapHeader(Header);
      Options := [ioBigEndian];

      SampleFormat := Header.Storage_type;

      SamplesPerPixel := Header.num_chan;
      if Header.num_matte = 1 then
        Inc(SamplesPerPixel);
      BitsPerSample := Header.Chan_bits;
      BitsPerPixel := SamplesPerPixel * BitsPerSample;

      if LowerCase(AnsiString(Header.Chan)) = 'rgb' then
      begin
        if Header.num_chan = 3 then begin
          if Header.num_matte > 0 then
            ColorScheme := csRGBA
          else
            ColorScheme := csRGB;
        end
        else if Header.num_chan = 1 then begin
          if Header.num_matte > 0 then
            ColorScheme := csGA
          else
            ColorScheme := csG;
        end
      end
      else if LowerCase(AnsiString(Header.Chan)) = 'xyz' then
        ColorScheme := csXYZ
      else
        ColorScheme := csUnknown;

      // The description of fileformat.info about gamma says:
      // Gamma contains an ASCII floating-point number representing the gamma
      // correction factor applied to the image before it was stored. A value of
      // 2.2 is considered typical. A value of 0.0 indicates no gamma setting.
      if Header.Gamma[0] <> #0 then begin
        FileGamma := StrToFloatDef(ConvertAnsiFloatToString(AnsiString(Header.Gamma)), 1) / 2.2;
        if Abs(FileGamma) >= 0.01 then
          Include(Options, ioUseGamma);
      end;

      Compression := ctRLE;

      // dimension of image, top might be larger than bottom denoting a bottom up image
      Width := Header.Active_window.Right - Header.Active_window.Left + 1;
      Height := Abs(Header.Active_window.Bottom - Header.Active_window.Top) + 1;
      if (Header.Active_window.Bottom - Header.Active_window.Top) < 0 then
        Orientation := gexoBottomLeft
      else
        Orientation := gexoTopLeft;

      Comment := AnsiString(Header.Desc);

      Result := True;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRLAGraphic.SwapHeader(var Header);

// Separate swap method to ease reading the main flow of the LoadFromMemory method.

begin
  with TRLAHeader(Header) do
  begin
    SwapWordArrayEndian(@Window, 4);
    SwapWordArrayEndian(@Active_window, 4);
    Frame := SwapEndian(Frame);
    Storage_type := SwapEndian(Storage_type);
    Num_chan := SwapEndian(Num_chan);
    Num_matte := SwapEndian(Num_matte);
    Num_aux := SwapEndian(Num_aux);
    Revision := SwapEndian(Revision);
    Job_num  := SwapEndian(Job_num);
    Field := SwapEndian(Field);
    Chan_bits := SwapEndian(Chan_bits);
    Matte_type := SwapEndian(Matte_type);
    Matte_bits := SwapEndian(Matte_bits);
    Aux_type := SwapEndian(Aux_type);
    Aux_bits := SwapEndian(Aux_bits);
    Next := SwapEndian(Next);
  end;
end;

{$endif RLAGraphic}

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

{$ifdef PhotoshopGraphic}

const
  PSD_COMPRESSION_NONE = 0;
  PSD_COMPRESSION_RLE = 1; // RLE compression (same as TIFF packed bits)
  PSD_PLAIN_ZIP = 2;
  PSD_PREDICTED_ZIP = 3;

  PSD_MAX_CHANNELS = 56; // Maximum number of channels allowed according to the specs.

  PSDBlendModeMapping: array[TPSDLayerBlendMode] of PAnsiChar = (
    'norm', // lbmNormal
    'dark', // lbmDarken
    'lite', // lbmLighten
    'hue ', // lbmHue
    'sat ', // lbmSaturation
    'colr', // lbmColor
    'lum ', // lbmLuminosity
    'mul ', // lbmMultiply
    'scrn', // lbmScreen
    'diss', // lbmDissolve
    'over', // lbmOverlay
    'hLit', // lbmHardLight
    'sLit', // lbmSoftLight
    'diff', // lbmDifference
    'smud', // lbmExclusion
    'div ', // lbmColorDodge
    'idiv'  // lbmColorBur
  );

  // Resource IDs for records in the resource block.
  Obsolete1 = $03E8;                        // Obsolete—Photoshop 2.0 only. Contains five 2 byte values: number of
                                            // channels, rows, columns, depth, and mode.
  MacPrintManInfo = $03E9;                  // Optional. Macintosh print manager print info record.
  MacPageFormatInfo = $03EA;                // Macintosh page format information. No longer read by Photoshop. (Obsolete)
  Obsolete2 = $03EB;                        // Obsolete—Photoshop 2.0 only. Contains the indexed color table.
  ResInfo = $03ED;                          // ResolutionInfo structure. See Appendix A in Photoshop SDK Guide.pdf.
  AlphaChannelNames = $03EE;                // Names of the alpha channels as a series of Pascal strings.
  DisplayInfoOld = $03EF;                   // Obsolete. See id $0435. DisplayInfo structure. See Appendix A in Photoshop SDK Guide.pdf.
  Caption = $03F0;                          // Optional. The caption as a Pascal string.
  Border = $03F1;                           // Border information. Contains a fixed number (2 bytes real, 2 bytes fraction)
                                            // for the border width, and 2 bytes for border units (1 = inches, 2 = cm, 3 = points, 4 = picas, 5 = columns).
  BackgroundColor = $03F2;                  // Background color. See the Colors additional file information.
  PrintFlags = $03F3;                       // Print flags. A series of one byte boolean values (see Page Setup dialog):
                                            // labels, crop marks, color bars, registration marks, negative, flip,
                                            // interpolate, caption, print flags.
  GrayMultichannelHalftoningInfo = $03F4;   // Grayscale and multichannel halftoning information.
  ColorHalftoningInfo = $03F5;              // Color halftoning information.
  DuotoneHalftoningInfo = $03F6;            // Duotone halftoning information.
  GrayMultiChannelTransferInfo = $03F7;     // Grayscale and multichannel transfer function.
  ColorTransferInfo = $03F8;                // Color transfer functions.
  DuotoneTransferInfo = $03F9;              // Duotone transfer functions.
  DuotoneImageInfo = $03FA;                 // Duotone image information.
  DotRange = $03FB;                         // Two bytes for the effective black and white values for the dot range.
  Obsolete3 = $03FC;                        // Obsolete.
  EPSOptions = $03FD;                       // EPS options.
  QuickMaskInfo = $03FE;                    // Quick Mask information. 2 bytes containing Quick Mask channel ID, 1 byte
                                            // boolean indicating whether the mask was initially empty.
  Obsolete4 = $03FF;                        // Obsolete.
  LayerStateInfo = $0400;                   // Layer state information. 2 bytes containing the index of target layer.
                                            // 0 = bottom layer.
  WorkingPath = $0401;                      // Working path (not saved). See path resource in Photoshop File Formats Spec.
  LayersGroupInfo = $0402;                  // Layers group information. 2 bytes per layer containing a group ID for the
                                            // dragging groups. Layers in a group have the same group ID.
  Obsolete5 = $0403;                        // Obsolete.
  IPTC_NAARecord = $0404;                   // IPTC-NAA record. This contains the File Info... information. See the
                                            // documentation in the IPTC folder of the Documentation folder.
  RawImageMode = $0405;                     // Image mode for raw format files.
  JPEGQuality = $0406;                      // JPEG quality. Private.
  GridAndGuides = $0408;                    // Grid and guides information.
  ThumbnailResource = $0409;                // New since Photoshop 4.0. Thumbnail resource for Photoshop 4.0 only. See thumbnail resource in Photoshop File Formats Spec.
  CopyrightFlg = $040A;                     // New since Photoshop 4.0. Copyright flag. Boolean indicating whether image is copyrighted. Can be
                                            // set via Property suite or by user in File Info...
  URL = $040B;                              // New since Photoshop 4.0. URL. Handle of a text string with uniform resource locator. Can be set
                                            // via Property suite or by user in File Info...
  ThumbnailResource2 = $040C;               // New since Photoshop 5.0. Thumbnail resource (supersedes $0409). See thumbnail resource in Photoshop File Formats Spec.
  GlobalAngle = $040D;                      // New since Photoshop 5.0. Global Angle. 4 bytes that contain an integer between 0..359 which is the
                                            // global lighting angle for effects layer. If not present assumed to be 30.
  ColorSamplersResource = $040E;            // Obsolete. New since Photoshop 5.0. Color samplers resource. See See Color samplers resource format.
  ICC_Profile = $040F;                      // New since Photoshop 5.0. ICC Profile. The raw bytes of an  ICC (International Color Consortium) format profile.
                                            // See ICC1v42_2006-05.pdf in the Documentation folder and icProfileHeader.h in Sample Code\Common\Includes .
  Watermark = $0410;                        // New since Photoshop 5.0. One byte for Watermark.
  ICC_Untagged = $0411;                     // New since Photoshop 5.0. ICC Untagged. 1 byte that disables any assumed profile handling when
                                            // opening the file. 1 = intentionally untagged.
  EffectsVisible = $0412;                   // New since Photoshop 5.0. Effects visible. 1 byte global flag to show/hide all the effects layer.
                                            // Only present when they are hidden.
  SpotHalftone = $0413;                     // New since Photoshop 5.0. New since Photoshop 5.0. Spot Halftone. 4 bytes for version, 4 bytes for length,
                                            // and the variable length data.
  DocumentSpecificIDs = $0414;              // New since Photoshop 5.0. Document specific IDs seed number: 4 bytes:
                                            // Base value starting at which layer IDs will be generated (or a greater value if existing IDs already exceed it)
                                            // It’s purpose is to avoid the case where we add layers, flatten, save,
                                            // open, and then add more layers that end up with the same IDs as the first set.
  AlphaNames = $0415;                       // New since Photoshop 5.0. Unicode Alpha Names. 4 bytes for length and the string as a unicode string.
  ColorTableCount = $0416;                  // New since Photoshop 6.0. Indexed Color Table Count. 2 bytes for the number
                                            // of colors in table that are actually defined.
  TransparentIndex = $0417;                 // New since version 6.0 of Adobe Photoshop: Tansparency Index. 2 bytes for
                                            // the index of transparent color, if any.
  GlobalAltitude = $0419;                   // New since version 6.0 of Adobe Photoshop: Global Altitude. 4 byte entry
                                            // for altitude.
  Slices = $041A;                           // New since version 6.0 of Adobe Photoshop: Slices. See Slices resource format.
  WorkflowURL = $041B;                      // New since version 6.0 of Adobe Photoshop: Workflow URL. Unicode string.
  XPEP = $041C;                             // New since version 6.0 of Adobe Photoshop: Jump To XPEP. 2 bytes major
                                            // version, 2 bytes minor version, 4 bytes count. Following is repeated for
                                            // count: 4 bytes block size, 4 bytes key, if key = 'jtDd' then next is a
                                            // Boolean for the dirty flag otherwise it’s a 4 byte entry for the mod date.
  AlphaIdentifiers = $041D;                 // New since version 6.0 of Adobe Photoshop: Alpha Identifiers. 4 bytes of
                                            // length, followed by 4 bytes each for every alpha identifier.
  URLList = $041E;                          // New since version 6.0 of Adobe Photoshop: URL List. 4 byte count of URLs,
                                            // followed by 4 byte long, 4 byte ID, and unicode string for each count.
  VersionInfo = $0421;                      // New since version 6.0 of Adobe Photoshop: Version Info. 4 byte version,
                                            // 1 byte HasRealMergedData, unicode string of writer name, unicode string
                                            // of reader name, 4 bytes of file version.
  ExifData1 = $0422;                        // (Photoshop 7.0) EXIF data 1. See http://www.kodak.com/global/plugins/acrobat/en/service/digCam/exifStandard2.pdf
  ExifData3 = $0423;                        // (Photoshop 7.0) EXIF data 1. See http://www.kodak.com/global/plugins/acrobat/en/service/digCam/exifStandard2.pdf
  XMPMetaData = $0424;                      // (Photoshop 7.0) XMP metadata. File info as XML description. See http://www.adobe.com/devnet/xmp/
  CaptionDigest = $0425;                    // (Photoshop 7.0) Caption digest. 16 bytes: RSA Data Security, MD5 message-digest algorithm.
  PrintScale = $0426;                       // (Photoshop 7.0) Print scale. 2 bytes style (0 = centered, 1 = size to fit, 2 = user defined).
                                            // 4 bytes x location (floating point). 4 bytes y location (floating point). 4 bytes scale (floating point)
  PixelAspectRatio = $0428;                 // (Photoshop CS) Pixel Aspect Ratio. 4 bytes (version = 1 or 2), 8 bytes double, x / y of a pixel.
                                            // Version 2, attempting to correct values for NTSC and PAL, previously off by a factor of approx. 5%.
  LayerComps = $0429;                       // (Photoshop CS) Layer Comps. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure)
  AlternateDuotoneColors = $042A;           // (Photoshop CS) Alternate Duotone Colors. 2 bytes (version = 1), 2 bytes count, following is repeated for
                                            // each count: [ Color: 2 bytes for space followed by 4 * 2 byte color component ], following this is
                                            // another 2 byte count, usually 256, followed by Lab colors one byte each for L, a, b. This resource
                                            // is not read or used by Photoshop.
  AlternateSpotColors = $42B;               // (Photoshop CS) Alternate Spot Colors. 2 bytes (version = 1), 2 bytes channel count, following is
                                            // repeated for each count: 4 bytes channel ID, Color: 2 bytes for space followed by 4 * 2 byte color
                                            // component. This resource is not read or used by Photoshop.
  LayerSectionIDs = $42D;                   // (Photoshop CS2) Layer Selection ID(s). 2 bytes count, following is repeated for each count: 4 bytes layer ID.
  HDRToningInfo = $042E;                    // (Photoshop CS2) HDR Toning information.
  PrintInfo1 = $042F;                       // (Photoshop CS2) Print info.
  LayerGroupsEnabled = $0430;               // (Photoshop CS2) Layer Group(s) Enabled ID. 1 byte for each layer in the document, repeated
                                            // by length of the resource. NOTE: Layer groups have start and end markers.
  ColorSamplers = $0431;                    // (Photoshop CS3) Color samplers resource. Also see ID 1038 ($040E) for old
                                            // format. See See Color samplers resource format.
  MeasurementScale = $0432;                 // (Photoshop CS3) Measurement Scale. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
  TimelineInfo = $0433;                     // (Photoshop CS3) Timeline Information. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
  SheetDisclosure = $0434;                  // (Photoshop CS3) Sheet Disclosure. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
  DisplayInfo = $0435;                      // (Photoshop CS3) DisplayInfo structure to support floating point clors. Also see ID 1007 ($03EF). See Appendix A in Photoshop API Guide.pdf.
  OnionSkins = $0436;                       // (Photoshop CS3) Onion Skins. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
  CountInfo = $0438;                        // (Photoshop CS4) Count Information. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
                                            // Information about the count in the document. See the Count Tool.
  PrintInfo2 = $043A;                       // (Photoshop CS5) Print Information. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
                                            // Information about the current print settings in the document. The color management options.
  PrintStyle = $043B;                       // (Photoshop CS5) Print Style. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
                                            // Information about the current print style in the document. The printing marks, labels, ornaments, etc.
  MacNSPrintInfo = $043C;                   // (Photoshop CS5) Macintosh NSPrintInfo. Variable OS specific info for Macintosh. NSPrintInfo.
                                            // It is recommened that you do not interpret or use this data.
  WinDevMode = $043D;                       // (Photoshop CS5) Windows DEVMODE. Variable OS specific info for Windows. DEVMODE.
                                            // It is recommened that you do not interpret or use this data.
  AutoSaveFilePath = $043E;                 // (Photoshop CS6) Auto Save File Path. Unicode string. It is recommened that you do not interpret or use this data.
  AutoSaveFormat = $043F;                   // (Photoshop CS6) Auto Save Format. Unicode string. It is recommened that you do not interpret or use this data.
  PathSelectionState = $0440;               // (Photoshop CC)Path Selection State. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
                                            // Information about the current path selection state.
  // $07D0 - $0BB6 Path Information (saved paths). See path resource format in Photoshop File Formats Spec.
  ClippingPathName = $0BB7;                 // Name of clipping path. See path resource format.
  OriginPathInfo = $0BB8;                   // (Photoshop CC) Origin Path Info. 4 bytes (descriptor version = 16), Descriptor (see See Descriptor structure).
                                            // Information about the origin path data.
  // $0FA0 - $1387 - Plug-In resource(s). Resources added by a plug-in. See the plug-in API found in the SDK documentation.
  ImageReadyVariables = $1B58;              // Image Ready variables. XML representation of variables definition
  ImageReadyDataSets = $1B59;               // Image Ready data sets
  ImageReadyDefaultSelectStat = $1B5A;      // Image Ready default selected state
  ImageReady7RolloverExpanded = $1B5B;      // Image Ready 7 rollover expanded state
  ImageReadyRolloverExpanded = $1B5C;       // Image Ready rollover expanded state
  ImageReadySaveLayerSettings = $1B5D;      // Image Ready save layer settings
  ImageReadyVersion = $1B5E;                // Image Ready version
  LightroomWorkflow = $1F40;                // (Photoshop CS3) Lightroom workflow, if present the document is in the middle of a Lightroom workflow.
  PrintFlagsInfo = $2710;                   // Print flags information. 2 bytes version (= 1), 1 byte center crop marks,
                                            // 1 byte (= 0), 4 bytes bleed width value, 2 bytes bleed width scale.

type
  PPSDHeader = ^TPSDHeader;
  TPSDHeader = packed record
    Signature: array[0..3] of AnsiChar; // always '8BPS'
    Version: Word;                  // always 1 (2 for PSB)
    Reserved: array[0..5] of Byte;  // reserved, always 0
    Channels: Word;                 // 1..56, number of channels in the image (including alpha)
    Rows,
    Columns: Cardinal;              // 1..30000, size of image (**PSB** max of 300,000.)
    Depth: Word;                    // 1, 8, 16, 32 bits per channel
    Mode: Word;                     // color mode (see constants above)
  end;

  // Description of a channel in a layer.
  TPSDChannelLengthInfo = packed record
    ChannelID: SmallInt;            // 0 = red, 1 = green etc. -1 = transparency mask, -2 = user supplied mask
    Size: Cardinal;                 // Size of channel data.
  end;

//----------------- TPhotoshopLayer ------------------------------------------------------------------------------------

constructor TPhotoshopLayer.Create(Graphic: TPSDGraphic);

begin
  FGraphic := Graphic;
  FImage := TBitmap.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TPhotoshopLayer.Destroy;

  //---------------------------------------------------------------------------

  procedure FreeItem(var Data: TPSDItemData);

  // Releases any dynamically allocated memory in the item.
  // Works also recursive if necessary.

  var
    I: Integer;
    
  begin
    with Data do
    begin
      // List.
      if Assigned(List) then
      begin
        for I := 0 to High(List^) do
          FreeItem(List^[I]);
        Dispose(List);
      end;

      // Descriptor or global object.
      if Assigned(Descriptor) then
      begin
        for I := 0 to High(Descriptor.Items) do
          FreeItem(Descriptor.Items[I].Data);
        Descriptor.Items := nil;
        Dispose(Descriptor);
      end;

      // Raw data.
      if Assigned(Data) then
        FreeMem(Data);
    end;
  end;

  //---------------------------------------------------------------------------

var
  I: Integer;

begin
  FImage.Free;

  with FTypeToolInfo do
  begin
    with TextDescriptor do
    begin
      for I := 0 to High(Items) do
        FreeItem(Items[I].Data);
      Items := nil;
    end;
    with WarpDescriptor do
    begin
      for I := 0 to High(Items) do
        FreeItem(Items[I].Data);
      Items := nil;
    end;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayer.SetImage(const Value: TBitmap);

begin
  FImage.Assign(Value);
end;

//----------------- TPhotoshopLayers -----------------------------------------------------------------------------------

constructor TPhotoshopLayers.Create(Graphic: TPSDGraphic);

begin
  {$IFDEF FPC}
  inherited Create;
  {$ENDIF}
  FGraphic := Graphic;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Get(Index: Integer): TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited Get(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Notify(Ptr: Pointer; Action: TListNotification);

begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    TPhotoShopLayer(Ptr).Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Put(Index: Integer; Layer: TPhotoshopLayer);

begin
  inherited Put(Index, Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Add(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited Add(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.AddNewLayer: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer.Create(FGraphic);
  inherited Add(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Extract(Layer: TPhotoshopLayer): TPhotoshopLayer;

begin
  Result := inherited Extract(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.First: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited First);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.IndexOf(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited IndexOf(Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPhotoshopLayers.Insert(Index: Integer; Layer: TPhotoshopLayer);

begin
  inherited Insert(Index, Layer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Last: TPhotoshopLayer;

begin
  Result := TPhotoshopLayer(inherited Last);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPhotoshopLayers.Remove(Layer: TPhotoshopLayer): Integer;

begin
  Result := inherited Remove(Layer);
  Layer.Free;
end;

//----------------- TPSDGraphic ----------------------------------------------------------------------------------------

constructor TPSDGraphic.Create;

begin
  inherited;
  FLayers := TPhotoshopLayers.Create(Self);
  FICCUntagged := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TPSDGraphic.Destroy;

begin
  FLayers.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.CombineChannels(Layer: TPhotoshopLayer);

// Combines all separate channels of the given layer into the layer bitmap.
// Previously allocated memory is freed here too.

  //---------------------------------------------------------------------------

  function GetChannel(ID: Integer): Pointer;

  // Returns the reference of the channel with the given ID where ID means:
  // -2 = user data mask
  // -1 = alpha channel
  //  0 = red (or gray, or cyan etc.)
  //  1 = green (or magenta etc.)
  //  2 = blue (or yellow etc.)
  //  3 = black (for CMYK images)

  var
    I: Integer;

  begin
    Result := nil;
    I := 0;
    while I < Length(Layer.FChannels) do
    begin
      if Layer.FChannels[I].ChannelID = ID then
      begin
        Result := Layer.FChannels[I].Data;
        Break;
      end;
      Inc(I);
    end;
  end;

  //---------------------------------------------------------------------------

var
  RunR, RunG, RunB, RunA, RunCMYKA: PByte;
  RunChannels: array of Pointer;
  Y, I: Integer;
  ChannelSize: Integer;

begin
  with Layer.FImage do
  begin
    ChannelSize := Width * Height;
    case PixelFormat of
      pf8Bit:
        begin
          RunR := GetChannel(0);
          for Y := 0 to Height - 1 do
          begin
            {$IFNDEF FPC}
            Move(RunR^, ScanLine[Y]^, Width);
            {$ELSE}
            ColorManager.ConvertRow([RunR], ScanLine[Y], Width, $FF);
            {$ENDIF}
            Inc(RunR, Width);
          end;
        end;
      pf24Bit: // RGB, CMYK or Lab
        begin
          // We need to add planar to our source options
          ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
          if FMode = PSD_CMYK then
          begin
            // Photoshop CMYK values are given with 0 for maximum values, but the
            // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
            // all entries in the buffer.
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
            for Y := 1 to ChannelSize do
            begin
              RunR^ := 255 - RunR^;
              Inc(RunR);
              RunG^ := 255 - RunG^;
              Inc(RunG);
              RunB^ := 255 - RunB^;
              Inc(RunB);
              RunA^ := 255 - RunA^;
              Inc(RunA);
            end;
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
          end
          else
          begin
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := nil;
          end;
          for Y := 0 to Height - 1 do
          begin
            ColorManager.ConvertRow([RunR, RunG, RunB, RunA], ScanLine[Y], Width, $FF);
            Inc(RunR, Width);
            Inc(RunG, Width);
            Inc(RunB, Width);
          end;
        end;
      pf32Bit:
        begin
          // TODO: A lot of the conversion stuff below can probably be handled in ColorManager
          // or is already present there! Remove it here to simplify things.
          // We should also check for the determined color mode csXXX instead of using FMode.
          // TODO: Support 16 bit per channel versions. Doing the above will probably solve that.
          // We need to add planar to our source options
          ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
          if FMode = PSD_CMYK then
          begin
            // Photoshop CMYK values are given with 0 for maximum values, but the
            // (general) CMYK conversion works with 255 as maxium value. Hence we must reverse
            // all entries in the buffer.
            RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);
            for Y := 1 to ChannelSize do
            begin
              RunR^ := 255 - RunR^;
              Inc(RunR);
              RunG^ := 255 - RunG^;
              Inc(RunG);
              RunB^ := 255 - RunB^;
              Inc(RunB);
              RunA^ := 255 - RunA^;
              Inc(RunA);
            end;
            // Getting the pointers to the start of the channels back.
            {RunR := GetChannel(0);
            RunG := GetChannel(1);
            RunB := GetChannel(2);
            RunA := GetChannel(3);}
            RunCMYKA := GetChannel(-1);
            if RunCMYKA = nil then
              SetLength(RunChannels, 4)
            else begin // CMYKA
              SetLength(RunChannels, 5);
              RunChannels[4] := RunCMYKA;  // A
            end;
            RunChannels[0] := GetChannel(0);  // C
            RunChannels[1] := GetChannel(1);  // M
            RunChannels[2] := GetChannel(2);  // Y
            RunChannels[3] := GetChannel(3);  // K
          end
          else if FMode in [PSD_GRAYSCALE, PSD_INDEXED] then begin
            // Gray or Indexed with alpha
            SetLength(RunChannels, 2);
            RunChannels[0] := GetChannel(0);  // G/I
            RunChannels[1] := GetChannel(-1); // A
          end
          else
          begin
            // Either RGBA or Lab with alpha.
            SetLength(RunChannels, 4);
            RunChannels[0] := GetChannel(0);  // R
            RunChannels[1] := GetChannel(1);  // G
            RunChannels[2] := GetChannel(2);  // B
            RunChannels[3] := GetChannel(-1); // A
          end;
          for Y := 0 to Height - 1 do
          begin
            ColorManager.ConvertRow(RunChannels, ScanLine[Y], Width, $FF);
            for i := 0 to High(RunChannels) do
              Inc(PByte(RunChannels[i]), Width);
            {Inc(RunR, Width);
            Inc(RunG, Width);
            Inc(RunB, Width);
            Inc(RunA, Width);}
          end;
        end;
    end;

    // Finally free all channel data.
    for Y := 0 to High(Layer.FChannels) do
      FreeMem(Layer.FChannels[Y].Data);
    Layer.FChannels := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.ConvertCompression(Value: Word): TCompressionType;

begin
  case Value of
    0:
      Result := ctNone;
    1:
      Result := ctPackedBits;
    2: // not yet supported
      Result := ctPlainZIP;
    3: // not yet supported
      Result := ctPredictedZIP;
  else
    Result := ctUnknown;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.DetermineColorScheme(ChannelCount: Integer): TColorScheme;

begin
  // To be able to read images with extra channels we will set any image with
  // more than the expected number as channels as an image with alpha since
  // at this moment we don't know exactly how to determine at this point if
  // the image is with/without alpha channel.
  case FMode of
    PSD_DUOTONE, // duo tone should be handled as grayscale
    PSD_GRAYSCALE, PSD_GRAYSCALE16:
      case ChannelCount of
        1:
          Result := csG;
        2:
          Result := csGA;
      else
        Result := csGA;
      end;
    PSD_BITMAP:  // B&W
        Result := csG;
    PSD_INDEXED: // 8 bits only are assumed because 16 bit wouldn't make sense here
      case ChannelCount of
        1:
          Result := csIndexed;
        2:
          Result := csIndexedA;
      else
        Result := csIndexedA;
      end;
    PSD_MULTICHANNEL, PSD_DEEPMULTICHANNEL,
    PSD_RGB, PSD_RGB48:
      case ChannelCount of
        3:
          Result := csRGB;
        4:
          Result := csRGBA;
      else
        Result := csRGBA;
      end;
    PSD_CMYK, PSD_CMYK64:
      if ChannelCount = 4 then
        Result := csCMYK
      else if ChannelCount = 5 then
        Result := csCMYKA
      else
        Result := csCMYKA;
    PSD_LAB, PSD_LAB48:
      if ChannelCount = 3 then
        Result := csCIELab
      else
        Result := csCIELab;
  else
    Result := csUnknown;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.LoadAdjustmentLayer(var Run: PByte; Layer: TPhotoshopLayer);

// Reads an adjustment layer whose identification is given by the first 4 bytes pointed to by Run.
// An adjustment layer is kind of a sub layer for the current layer.

const
  KeyCount = 36;
  AdjustmentKey: array[0..KeyCount - 1] of PAnsiChar = (
    'levl', //  0, Levels
    'curv', //  1, Curves
    'brit', //  2, Brightness/Contrast
    'blnc', //  3, Color balance
    'hue ', //  4, Old hue/saturation, Photoshop 4.0
    'hue2', //  5, New hue/saturation, Photoshop 5.0
    'selc', //  6, Selective color
    'thrs', //  7, Threshold
    'nvrt', //  8, Invert
    'post', //  9, Posterize
    'lrFX', // 10, Effects layer
    'tySh', // 11, Type tool info
    'luni', // 12, Unicode layer name
    'lyid', // 13, LayerID
    'lfx2', // 14, Object based effects layer info
    'Patt', // 15, Patterns
    'Anno', // 16, Annotations
    'clbl', // 17, Blend clipping elements
    'infx', // 18, Blend interior elements
    'knko', // 19, Knockout settings
    'lspf', // 20, Protected setting
    'lclr', // 21, Sheet color setting
    'fxrp', // 22, Reference point
    'grdm', // 23, Gradient settings
    'lsct', // 24, Section divider setting
    'brst', // 25, Channel blending restriction setting
    'SoCo', // 26, Solid color sheet setting
    'PtFl', // 27, Pattern fill setting
    'GdFl', // 28, Gradient fill setting
    'vmsk', // 29, Vector mask setting
    'TySh', // 30, Type tool object setting
    'ffxi', // 31, Foreign effect ID
    'lnsr', // 32, Layer name source setting
    'shpa', // 33, Pattern data
    'shmd', // 34, Meta data setting
    'Layr'  // 35, Layer data
  );

  // Signatures used in an effects adjustment layer.
  EffectSignature: array[0..5] of PAnsiChar = (
    'cmnS', // 0, common state
    'dsdw', // 1, drop shadow
    'isdw', // 2, inner shadow
    'oglw', // 3, outer glow
    'iglw', // 4, inner glow
    'bevl'  // 5, bevel
  );

var
  I: Integer;
  Size: Cardinal;
  Temp: PAnsiChar;

begin
  // Find out which data there is.
  I := 0;
  while I < KeyCount do
  begin
    if StrLComp(PAnsiChar(Run), AdjustmentKey[I], 4) = 0 then
      Break;
    Inc(I);
  end;
  Inc(Run, 4);

  // Prepare read address after the adjustment layer, regardless whether we read the data or not.
  Size := ReadBigEndianCardinal(Run);
  Temp := PAnsiChar(Run); Inc(Temp, Size);
  // What type is it?
  case I of
    12: // Unicode layer name.
      Layer.FName := ReadBigEndianString(Run);
    30: // Type tool object settings (text layer).
      begin
        Layer.FType := ltText;
        // Skip version number (1 = Photoshop 6).
        Inc(Run, SizeOf(Word));
        with Layer.FTypeToolInfo do
        begin
          Transform.XX := ReadBigEndianDouble(Run);
          Transform.XY := ReadBigEndianDouble(Run);
          Transform.YX := ReadBigEndianDouble(Run);
          Transform.YY := ReadBigEndianDouble(Run);
          Transform.TX := ReadBigEndianDouble(Run);
          Transform.TY := ReadBigEndianDouble(Run);
          
          // Skip text descriptor version (= 50 for PS 6) and descriptor version (= 16 for PS 6) fields.
          Inc(Run, 6);
          // Read text descriptor.
          ReadDescriptor(Run, TextDescriptor);
          // Skip warp descriptor version (= 1 for PS 6) and descriptor version (= 16 for PS 6) fields.
          Inc(Run, 6);
          // Read warp descriptor.
          ReadDescriptor(Run, WarpDescriptor);

          // Finally read the warp rectangle. It is supposed to be four double values but
          // often is some other size. We can compensate for this by the overall size, but the
          // values might be wrong then.
          WarpRectangle.Left := ReadBigEndianDouble(Run);
          WarpRectangle.Top := ReadBigEndianDouble(Run);
          WarpRectangle.Right := ReadBigEndianDouble(Run);
          WarpRectangle.Bottom := ReadBigEndianDouble(Run);
        end;
      end;
  end;
  Run := PByte(Temp);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadChannelData(var Run: PByte; var Channel: TPSDChannel; AWidth, AHeight: Integer;
  IsIrrelevant: Boolean);

// Reads and optionally decompresses image data for one channel.

var
  Y: Integer;
  Count: Integer;
  RLELength: array of Word;
  Compression: TCompressionType;
  Target: PByte;
  RemainingSize: Integer;

begin
  RemainingSize := Channel.Size;
  if RemainingSize > 0 then
  begin
    Compression := ConvertCompression(ReadBigEndianWord(Run));
    Dec(RemainingSize, 2);
    if (RemainingSize > 0) and not IsIrrelevant then
    begin
      // Allocate temporary storage for the channel data. This memory is freed in CombineChannels.
      // A channel is always 8 bit per pixel.
      GetMem(Channel.Data, AWidth * AHeight);

      case Compression of
        ctNone: // Simple case, just move the data to our storage.
          Move(Run^, Channel.Data^, RemainingSize);
        ctPackedBits:
          begin
            Decoder := TPackbitsRLEDecoder.Create;
            try
              Decoder.DecodeInit;
              SetLength(RLELength, AHeight);
              Count := 2 * AHeight;
              Move(Run^, Pointer(RLELength)^, Count); // RLE lengths are word values.
              SwapWordArrayEndian(Pointer(RLELength), AHeight);
              Dec(RemainingSize, Count);
              // Advance the running pointer to after the RLE lenghts.
              Inc(Run, Count);

              Target := Channel.Data;
              for Y := 0 to AHeight - 1 do
              begin
                Decoder.Decode(Pointer(Run), Pointer(Target), RLELength[Y], AWidth);
                Inc(Run, RLELength[Y]);
                Inc(Target, AWidth);
                Dec(RemainingSize, RLELength[Y]);
                if RemainingSize <= 0 then
                  Break;
              end;
            finally
              FreeAndNil(Decoder);
            end;
          end;
        ctPlainZIP, ctPredictedZIP: // Supposedly these are only used for layer data not composited data
          begin
            // We try with Z_FINISH first if that doesn't work we can try Z_PARTIAL_FLUSH
            // but that will need extra code in a loop.
            Decoder := TLZ77Decoder.Create(Z_FINISH, False);
            Decoder.DecodeInit;
            Decoder.Decode(Pointer(Run), Channel.Data, RemainingSize, AWidth);
            if Compression = ctPredictedZIP then begin
              // Extra work needs to be done here for prediction.
              // See: https://github.com/psd-tools/psd-tools/blob/master/src/psd_tools/compression.py
            end;
          end;
      else
        FreeMem(Channel.Data);
        GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PSD/PDD']);
      end;
    end;
    Inc(Run, RemainingSize);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadDescriptor(var Run: PByte; var Descriptor: TPSDDescriptor);

const
  // Identifiers used in the descriptor structures.
  KeyCount = 20;
  OSTypeKey: array[0..KeyCount - 1] of PAnsiChar = (
    'obj ', // Reference
    'Objc', // Descriptor
    'VlLs', // List
    'doub', // Double
    'UntF', // Unit float
    'TEXT', // String
    'Enmr', // Enumerated
    'long', // Integer
    'bool', // Boolean
    'GlbO', // GlobalObject same as Descriptor
    'Clss', // Class
    'GlbC', // Gobal class
    'alis', // Alias

    // Additional keys for reference type.
    'prop', // Property
    'rele', // Offset
    'Idnt', // Identifier
    'indx', // Index
    'name', // Name

    // Other keys not mentioned in the docs.
    'enum', // enumeration
    'tdta'  // raw data
  );

  //--------------- local functions -------------------------------------------

  function ReadUnicodeID: WideString;

  // Reads an ID which is either a Unicode string or a 4 byte ANSI string.

  var
    I: Cardinal;

  begin
    I := ReadBigEndianCardinal(Run);
    if I = 0 then
    begin
      SetString(Result, PAnsiChar(Run), 4);
      Inc(Run, 4);
    end
    else
      Result := ReadBigEndianString(Run, I);
  end;

  //---------------------------------------------------------------------------

  function ReadANSIID: AnsiString;

  // Reads an ID which is an ANSI string.

  var
    I: Cardinal;

  begin
    I := ReadBigEndianCardinal(Run);
    if I = 0 then
      I := 4;
    SetString(Result, PAnsiChar(Run), I);
    Inc(Run, I);
  end;

  //---------------------------------------------------------------------------

  function ReadOSTypeKey: Cardinal;

  begin
    Result := 0;
    while (Result < KeyCount) and (StrLComp(PAnsiChar(Run), OSTypeKey[Result], 4) <> 0) do
      Inc(Result);
    Inc(Run, 4);
  end;

  //---------------------------------------------------------------------------

  procedure ReadItem(var Data: TPSDItemData);

  // Reads a single descriptor item. Since an item might be a list which again contains
  // descriptor items this code might be called recursively.
  // Note: There are a few types whose structure are not documented. They are loaded by
  //       guessing what could be there.

  var
    RefItemCount: Cardinal;
    I: Integer;

  begin
    with Data do
    begin
      case ItemType of
        0, 2: // Reference or List
          begin
            RefItemCount := ReadBigEndianCardinal(Run);
            New(List);
            SetLength(List^, RefItemCount);
            for I := 0 to RefItemCount - 1 do
            begin
              List^[I].ItemType := ReadOSTypeKey;
              // Recurse down and read a new PSD data item.
              ReadItem(List^[I]);
            end;
          end;
        1, 9: // Descriptor or global object
          begin
            New(Descriptor);
            ReadDescriptor(Run, Descriptor^);
          end;
        3: // Double
          Value := ReadBigEndianDouble(Run);
        4: // Unit float
          begin
            Units := ReadBigEndianCardinal(Run);
            Value := ReadBigEndianDouble(Run);
          end;
        5: // String
          KeyID := ReadBigEndianString(Run);
        6: // Enumerated
          begin
            ClassID := ReadUnicodeID;
            KeyID := ReadANSIID;
            TypeID := ReadANSIID;
            EnumValue := ReadANSIID;
          end;
        7: // Integer (undocumented)
          IntValue := Integer(ReadBigEndianCardinal(Run));
        8: // Boolean
          begin
            BoolValue := Run^ <> 0;
            Inc(Run);
          end;
        10, 11: // Class or global class (undocumented)
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
          end;
        12: // Alias
          begin
            // Alias data is OS specific. It could contain Macintosh FSSpec data, which cannot be
            // handled on Windows.
            // On Windows it might contain a handle to a string of the full path, whatever that means.
            // There is no further information available, so we skip that data.
            I := ReadBigEndianCardinal(Run);
            Inc(Run, I);
          end;
        13: // Property
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
            KeyID := ReadANSIID;
          end;
        14: // Offset
          begin
            ClassID := ReadUnicodeID;
            // Skip ANSI form of the string.
            ReadANSIID;
            Offset := ReadBigEndianCardinal(Run);
          end;
        15, 17: // Identifier (undocumented) or Name (undocumented)
          Name := ReadBigEndianString(Run);
        16: // Index (undocumented)
          IntValue := Integer(ReadBigEndianCardinal(Run));
        18: // 'enum' (undocumented)
          begin
            TypeID := ReadANSIID;
            EnumValue := ReadANSIID;
          end;
        19: // 'tdta' (undocumented)
          begin
            DataSize := ReadBigEndianCardinal(Run);
            GetMem(Data, DataSize);
            Move(Run^, Data^, DataSize);
            Inc(Run, DataSize);
          end;
      else
        // There is no error recovery in the file for the case the item key is unknown.
        // So we cannot skip it and have to throw an exception here to stop processing and to avoid
        // an exception showing up in a totally wrong place.
        GraphicExError(gesInvalidPSDLayerData);
      end;
    end;
  end;
  
  //--------------- end local functions ---------------------------------------

var
  I: Cardinal;
  ItemCount: Cardinal;

begin
  with Descriptor do
  begin
    // Class ID as Unicode string.
    ClassID := ReadBigEndianString(Run);
    ClassID2 := ReadANSIID;

    // Now read the items in the descriptor.
    ItemCount := ReadBigEndianCardinal(Run);
    SetLength(Descriptor.Items, ItemCount);
    for I := 0 to ItemCount - 1 do
    begin
      Descriptor.Items[I].Key := ReadANSIID;
      Descriptor.Items[I].Data.ItemType := ReadOSTypeKey;
      ReadItem(Descriptor.Items[I].Data);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadMergedImage(var Source: PByte; Layer: TPhotoshopLayer; Compression: TCompressionType;
  Channels: Byte);

// Reads the image data of the composite image (if Layer = nil) or the given layer.

var
  Count: Cardinal;
  RLELength: array of Word;

  Y: Integer;
  BPS: Integer;         // bytes per sample either 1 or 2 for 8 bits per channel and 16 bits per channel respectively
  ChannelSize: Integer; // size of one channel (taking BPS into account)
  Increment: Integer;   // pointer increment from one line to next

  // RLE buffers
  Line,
  Buffer: Pointer;     // all iamge data uncompressed
  Run1: PByte;         // running pointer in Buffer 1
  PtrArray: array of pointer; // Array of channel buffers
  iChannel: Word;

  W, H: Integer;       // Width and height of the layer or composite image.

begin
  if Layer = nil then
  begin
    W := Width;
    H := Height;
  end
  else
  begin
    W := Layer.Image.Width;
    H := Layer.Image.Height;
  end;

  // Reading the merged image takes up the rest of the entire loading process.
  StartProgressSection(0, gesTransfering);

  Decoder := nil;
  case Compression of
    ctNone: ;
    ctPackedBits:
      begin
        Decoder := TPackbitsRLEDecoder.Create;
        Decoder.DecodeInit;
        Count := H * Channels;
        // We have to swap the byte order but must not modify the data pointed to by Run (might be a file mapping).
        // Hence we have to make a copy of the RLE lengths.
        SetLength(RLELength, Count);
        Move(Source^, Pointer(RLELength)^, 2 * Count);
        SwapWordArrayEndian(Pointer(RLELength), Count);
        // Advance the running pointer to after the RLE lenghts.
        Inc(Source, 2 * Count);
      end;
  else
    GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PSD/PDD']);
  end;

  try
    case FImageProperties.ColorScheme of
      csG,
      csIndexed:
        begin
          {$IFDEF LCMS}
          if FICCManager = nil then
            // This will allow us to call Transform that directly returns (does nothing)
            // that way we will not have to check for nil inside the loop.
            FICCManager := TICCProfileManager.Create
          else if FICCTransformEnabled and (FImageProperties.ColorScheme = csG) then
            FICCManager.CreateTransformTosRGB_Gray8();
          // else for Indexed no transform should be done here, instead the palette entries should be transformed
          {$ENDIF}
          // Very simple format here, we don't need the color conversion manager.
          if Assigned(Decoder) then
          begin
            {$IFDEF FPC}
            GetMem(Buffer, W);
            try
            {$ENDIF}
              for Y := 0 to H - 1 do
              begin
                Count := RLELength[Y];
                {$IFNDEF FPC}
                Line := ScanLine[Y];
                {$ELSE}
                Line := Buffer;
                {$ENDIF}
                Decoder.Decode(Pointer(Source), Line, Count, W);
                Inc(Source, Count);

                {$IFDEF FPC}
                ColorManager.ConvertRow([Buffer], ScanLine[Y], W, $FF);
                {$ENDIF}
                {$IFDEF LCMS}
                FICCManager.ExecuteTransform(ScanLine[Y], W);
                {$ENDIF}
                AdvanceProgress(100 / H, 0, 1, True);
              end;
            {$IFDEF FPC}
            finally
              FreeMem(Buffer);
            end;
            {$ENDIF}
          end
          else // uncompressed data
            for Y := 0 to H - 1 do
            begin
              {$IFNDEF FPC}
              Move(Source^, ScanLine[Y]^, W);
              {$ELSE}
              ColorManager.ConvertRow([Source], ScanLine[Y], W, $FF);
              {$ENDIF}
              {$IFDEF LCMS}
              FICCManager.ExecuteTransform(ScanLine[Y], W);
              {$ENDIF}
              Inc(Source, W);

              AdvanceProgress(100 / H, 0, 1, True);
            end;
          {$IFDEF LCMS}
          FICCManager.DestroyTransform();
          {$ENDIF}
        end;
      csGA,
      csIndexedA,
      csRGB,
      csRGBA,
      csCMYK, csCMYKA,
      csCIELab:
        begin
          // Data is organized in planes. This means first all red rows, then
          // all green and finally all blue rows.
          // We need to add that to our source options
          ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
          BPS := FImageProperties.BitsPerSample div 8;
          ChannelSize := BPS * W * H;

          GetMem(Buffer, Channels * ChannelSize);
          try
            // first run: load image data and decompress it if necessary
            if Assigned(Decoder) then
            begin
              // determine whole compressed size
              Count := 0;
              for Y := 0 to High(RLELength) do
                Inc(Count, RLELength[Y]);
              Decoder.Decode(Pointer(Source), Buffer, Count * Cardinal(BPS), Channels * ChannelSize);
              Inc(Source, Count * Cardinal(BPS));
            end
            else
            begin
              Move(Source^, Buffer^, Channels * ChannelSize);
              Inc(Source, Channels * ChannelSize);
            end;

            if FImageProperties.ColorScheme in [csCMYK, csCMYKA] then
              ColorManager.SourceOptions := ColorManager.SourceOptions + [coInvertedCMYK];

            Increment := BPS * W;

            // second run: put data into image (convert color space if necessary)
            Run1 := Buffer;
            // Since we can have a variable number of channels we use a
            // dynamic array for the channel pointers.
            SetLength(PtrArray, Channels);
            for iChannel := 0 to Channels-1 do begin
              PtrArray[iChannel] := Run1;
              Inc(Run1, ChannelSize);
            end;
            {$IFDEF LCMS}
            if FICCManager = nil then
              // This will allow us to call Transform that directly returns (does nothing)
              // that way we will not have to check for nil inside the loop.
              FICCManager := TICCProfileManager.Create
            else if FICCTransformEnabled then
              FICCManager.CreateTransformTosRGB(Channels > 3);
            // else dummy Transform will be called.
            {$ENDIF}
            for Y := 0 to H - 1 do
            begin
              ColorManager.ConvertRow(PtrArray, ScanLine[Y], W, $FF);
              {$IFDEF LCMS}
              FICCManager.ExecuteTransform(ScanLine[Y], W);
              {$ENDIF}
              for iChannel := 0 to Channels-1 do begin
                Inc(PAnsiChar(PtrArray[iChannel]), Increment);
              end;

              AdvanceProgress(100 / H, 0, 1, True);
            end;
            {$IFDEF LCMS}
            FICCManager.DestroyTransform();
            {$ENDIF}
          finally
            if Assigned(Buffer) then
              FreeMem(Buffer);
          end;
        end;
    end;
    FinishProgressSection(False);
  finally
    FreeAndNil(Decoder);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadLayers(Run: PByte);

// Recreates the layer structure given in the file. Run points to the layer section size.

  //--------------- local functions -------------------------------------------

  procedure ReadBlendRanges(var Data: TPSDCompositeGrayBlend);

  begin
    with Data do
    begin
      Black1 := Byte(Run^);
      Inc(Run);
      Black2 := Byte(Run^);
      Inc(Run);
      White1 := Byte(Run^);
      Inc(Run);
      White2 := Byte(Run^);
      Inc(Run);
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  LayerCount: SmallInt;
  Layer: TPhotoshopLayer;
  I, LayerIndex: Integer;
  R: TRect;
  Channels: Integer;
  BlendMode: TPSDLayerBlendMode;
  Dummy: Byte;
  BlockSize: Cardinal;
  S: AnsiString;
  BlockStart: PByte;
  // Required for newer Delphi (cannot assign to class property records)
  TempMaskData: TPSDLayerMaskData;
  HasIrrelevantData: Boolean;

begin
  // Skip the layer section size. We are going to read the full section.
  Inc(Run, SizeOf(Cardinal));

  LayerCount := SwapEndian(PSmallInt(Run)^);
  // If LayerCount is < 0 then it means the first alpha channel contains the transparency data for the
  // composite image (the merged result). I'm not sure what to do with that info.
  LayerCount := Abs(LayerCount);
  Inc(Run, SizeOf(SmallInt));

  // Once we know how many layers to load we can setup a progress section for them.
  // The size is the layer count relative to itself plus one image (the composite/merged image) less 1% we
  // already spent for preparation.
  if LayerCount > 0 then
  begin
    StartProgressSection(100 * LayerCount / (LayerCount + 1) - 1, gesLoadingData);

    // Now retrieve the actual layers.
    LayerIndex := 0;

    // Start an own progress section for loading the layer info and assign it 5% (difficult to say in advance
    // how much it really is, since this depends on layer sizes and amount of info).
    StartProgressSection(5, gesLoadingData);

    while LayerIndex < LayerCount do
    begin
      Layer := FLayers.AddNewLayer;

      // bounds rectangle
      R.Top := Integer(ReadBigEndianCardinal(Run));
      R.Left := Integer(ReadBigEndianCardinal(Run));
      R.Bottom := Integer(ReadBigEndianCardinal(Run));
      R.Right := Integer(ReadBigEndianCardinal(Run));
      Layer.Bounds := R;
      Channels := ReadBigEndianWord(Run);
      // Extra safety check: current specs say 56 is max.
      // Note that apparently it is possible and allowed that Channels > FChannels.
      if (Channels > PSD_MAX_CHANNELS) then // FChannels from PSD header
        GraphicExError(gesInvalidPSDLayerData);

      // Keep the channel data for later pixel data retrieval.
      SetLength(Layer.FChannels, Channels);
      for I := 0 to Channels - 1 do
      begin
        with Layer.Channels[I] do
        begin
          ChannelID := SwapEndian(PSmallInt(Run)^);
          Inc(Run, SizeOf(Word));
          Size := ReadBigEndianCardinal(Run);
        end;
      end;

      // Next comes the blend mode signature which is always '8BIM'. We can use this for error checking.
      if StrLIComp(PAnsiChar(Run), '8BIM', 4) <> 0 then
        GraphicExError(gesInvalidPSDLayerData);
      Inc(Run, 4);
      // Determine the blend mode from the four character ID.
      for BlendMode := Low(TPSDLayerBlendMode) to High(TPSDLayerBlendMode) do
        if StrLIComp(PAnsiChar(Run), PSDBlendModeMapping[BlendMode], 4) = 0 then
        begin
          Layer.BlendMode := BlendMode;
          Break;
        end;
      Inc(Run, 4);
      Layer.Opacity := Byte(Run^);
      Inc(Run);
      if Byte(Run^) = 0 then
        Layer.Clipping := lcBase
      else
        Layer.Clipping := lcNonBase;
      Inc(Run);
      Dummy := Byte(Run^); // 5 valid bits
      {$IFNDEF FPC}
      Layer.Options := TPSDLayerOptions(Dummy and 31);
      {$ELSE}
      Layer.Options := TPSDLayerOptions(Dummy and Byte(31));
      {$ENDIF}
      HasIrrelevantData := [loPhotoshop5, loPixelDataIrrelevant] * Layer.Options =
        [loPhotoshop5, loPixelDataIrrelevant];
      // There is a filler byte after the flags/options.
      Inc(Run, 2);

      // Skip extra data size value.
      Inc(Run, SizeOf(Cardinal));

      // Read out mask data.
      // The size depends on LayerMaskFlags and MaskParameters
      BlockSize := ReadBigEndianCardinal(Run);
      if BlockSize > 0 then
      begin
        BlockStart := Run;
        // Newer Delphi requires us to use a local variable to assign to a TRect as part of a class
        // See: http://stackoverflow.com/questions/12352563/why-do-i-get-left-side-cannot-be-assigned-to-for-trect-after-upgrading-delphi
        TempMaskData := Layer.MaskData;
        R.Top := ReadBigEndianInteger(Run);
        R.Left := ReadBigEndianInteger(Run);
        R.Bottom := ReadBigEndianInteger(Run);
        R.Right := ReadBigEndianInteger(Run);
        TempMaskData.Bounds := R;
        TempMaskData.DefaultColor := Byte(Run^);
        Inc(Run);
        TempMaskData.Flags := TPSDLayerMaskFlags(Run^);
        Inc(Run);
        // If only 20 bytes mask data is present then we are finished here with it (except 2 padding bytes).
        // Otherwise read additional data.
        if BlockSize = 20 then
          Inc(Run, 2)
        else
        begin
          if lmfMaskWithParameters in TempMaskData.Flags then begin
            // 1 extra byte with "Mask Parameters". Extra size depends on the bits that are set.
            TempMaskData.MaskParameters := TPSDMaskParameters(Run^);
            Inc(Run);
            // Not reading the extra bytes here. Rest of the block will be skipped.
          end
          else begin
            // Skip "real flags" field, which is just a duplication of the flags.
            Inc(Run);
            TempMaskData.UserMaskBackground := Byte(Run^);
            // Advance after the mask background value and skip the copy of the enclosing rectangle too, which follows.
            Inc(Run, 1 + 4 * SizeOf(Cardinal));
          end;
        end;
        Layer.MaskData := TempMaskData;
        // To be better able to handle unexpected information (future changes etc)
        // we will use the BlockSize to compute the next block offset.
        Inc(BlockStart, BlockSize);
        Run := BlockStart;
      end;

      // Next are the layer blending ranges. In opposition to the docs the size seems not to depend on the number of
      // channels in the layer. It is always 10 range entries large, even for gray scale images.
      BlockSize := ReadBigEndianCardinal(Run);
      if BlockSize > 0 then
      begin
        BlockStart := Run;
        // Take the first two entries for gray blending.
        ReadBlendRanges(Layer.FCompositeGrayBlendSource);
        ReadBlendRanges(Layer.FCompositeGrayBlendDestination);
        // Read as many entries as there are channels, but not more than 8.
        for I := 0 to Min(High(Layer.Channels), 4) do
        begin
          ReadBlendRanges(Layer.Channels[I].BlendSourceRange);
          ReadBlendRanges(Layer.Channels[I].BlendTargetRange);
        end;
        // Skip whatever left over.
        Inc(BlockStart, BlockSize);
        Run := BlockStart;
      end;

      // Read the pascal style (ANSI) layer name. This might get overwritten by the Unicode name.
      I := Byte(Run^);
      SetString(S, PAnsiChar(Run) + 1, I);
      Layer.Name := S;
      // The name is padded to a 4 byte boundary.
      Inc(Run, (I + 4) and not 3);

      // From Photoshop version 4 on there might be additional data here. This data is organized in blocks
      // all starting with '8BIM' as tag and is referred to as "adjustment layers" (e.g. Unicode name, effects etc.).
      while StrLIComp(PAnsiChar(Run), '8BIM', 4) = 0 do
      begin
        Inc(Run, 4);
        LoadAdjustmentLayer(Run, Layer);
      end;

      // Advance to next layer.
      Inc(LayerIndex);
    end;
    // Finish progress for layer info retrieval.
    FinishProgressSection(False);

    // Start progress section for the rest of the entire layer loading process.
    StartProgressSection(0, gesLoadingData);
    // Here we reached the image data. This block contains all channel data for all layers.
    for LayerIndex := 0 to FLayers.Count - 1 do
    begin
      StartProgressSection(100 / LayerCount, gesLoadingData);

      Layer := FLayers[LayerIndex];
      // Each channel might have an individual compression scheme.
      // If the layer contains irrelevant data then tell it the reader method so it skips the data accordingly.
      for I := 0 to High(Layer.FChannels) do
        with Layer.Bounds do
          ReadChannelData(Run, Layer.FChannels[I], Right - Left, Bottom - Top, HasIrrelevantData);

      if not HasIrrelevantData then
      begin
        // Extra layer channels always follow the actual image data so we can limit the maximum
        // number of channels to use to 5 (CMYKA) without harm.
        Layer.Image.PixelFormat := SetupColorManager(Min(5, Length(Layer.FChannels)));
        with Layer, Bounds do
        begin
          Image.Width := Right - Left;
          Image.Height := Bottom - Top;
          Image.Palette := CopyPalette(Palette);
        end;
        CombineChannels(Layer);
      end;

      FinishProgressSection(True);
    end;
    // Finish the decompression and combining process.
    FinishProgressSection(False);
    // Finish the layer loading progress. The global layer mask info does not produce progress events.
    FinishProgressSection(False);
  end;

  // The last step after all layers have been read is to read the global layer mask info.
  BlockSize := ReadBigEndianCardinal(Run);
  if BlockSize > 0 then
  begin
    BlockStart := Run;

    FLayers.FOverlayColorSpace := ReadBigEndianWord(Run);
    for I := 0 to 3 do
      FLayers.FColorComponents[I] := ReadBigEndianWord(Run);
    FLayers.FLayerMaskOpacity := ReadBigEndianWord(Run);
    FLayers.FKind := Byte(Run^);

    Inc(BlockStart, BlockSize);
    Run := BlockStart;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.ReadResources(Run: PByte);

var
  ID: Word;
  I: Cardinal;
  Name: AnsiString;
  Size: Cardinal;

begin
  while StrLIComp(PAnsiChar(Run), '8BIM', 4) = 0 do
  begin
    // Skip signature.
    Inc(Run, 4);
    // Resource ID.
    ID := ReadBigEndianWord(Run);
    // Resource name (pascal short string style).
    I := Byte(Run^);
    Inc(Run);
    SetString(Name, PAnsiChar(Run), I);
    Inc(Run, I);
    Inc(Run, NativeInt(Run) and 1); // Padded to even size.

    // Resource size.
    Size := ReadBigEndianCardinal(Run);
    case ID of
      GridAndGuides:
        with FGridSettings do
        begin
          // Skip version number (= 1 for Photoshop 4.0).
          Inc(Run, 4);
          // Numbers here are in 16.16 fix point format.
          HorizontalCycle := ReadBigEndianCardinal(Run) / 32;
          VerticalCycle := ReadBigEndianCardinal(Run) / 32;
          // Number of guides.
          Size := ReadBigEndianCardinal(Run);
          if Size > 0 then
          begin
            SetLength(Guides, Size);
            for I := 0 to Size - 1 do
            begin
              Guides[I].Location := ReadBigEndianCardinal(Run) / 32;
              Guides[I].IsHorizontal := Boolean(Run^);
              Inc(Run);
            end;
          end;
        end;
      ICC_Profile:
        begin
          {$IFDEF LCMS}
          if not Assigned(FICCManager) then
            FICCManager := TICCProfileManager.Create;
          FICCManager.LoadSourceProfileFromMemory(Run, Size);
          {$ENDIF}
          Inc(Run, Size);
        end;
      ICC_Untagged:
        begin
          FICCUntagged := Run^ = 1;
          Inc(Run, Size);
        end;
    else
      // Simply skip any unknown entries.
      Inc(Run, Size);
    end;
    Inc(Run, NativeInt(Run) and 1); // Padded to even size.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.SetupColorManager(Channels: Integer): TPixelFormat;

// Determines source and target color schemes and initializes the color manager options for the
// following load of image data.
// There are only a couple of scenarios which are allowed in layers in a PSD file:
//   - only grayscale, RGB, CMYK and Lab
//   - only 8 bits per sample.
// Returns the necessary pixel format.

var
  CurrentColorScheme: TColorScheme;

begin
  with FImageProperties, ColorManager do
  begin
    // PSD always uses bigendian (even for float values!).
    SourceOptions := [coNeedByteSwap];
    SourceBitsPerSample := BitsPerSample;
    case BitsPerSample of
      1, 8:
      begin
        TargetBitsPerSample := BitsPerSample;
      end;
      16:
      begin
        TargetBitsPerSample := 8;
      end;
      32:
      begin
        TargetBitsPerSample := 8;
        SourceDataFormat := sdfFloat;
      end;
    else
      // On purpose nothing is set here.
      // Unknown BitsPerSample will raise a bitdepth error.
    end;
    SourceSamplesPerPixel := Channels;

    CurrentColorScheme := DetermineColorScheme(Channels);
    SourceColorScheme := CurrentColorScheme;
    // Always explicitly set TargetSamplesPerPixel because source might have
    // extra non color channels that we need to ignore if possible.
    case CurrentColorScheme of
      csG,
      csIndexed:
        begin
          if ioMinIsWhite in Options then
            SourceOptions := SourceOptions + [coMinIsWhite];
          {$IFNDEF FPC}
          TargetColorScheme := CurrentColorScheme;
          TargetSamplesPerPixel := 1;
          {$ELSE}
          TargetColorScheme := csBGR;
          TargetSamplesPerPixel := 3;
          TargetBitsPerSample := 8; // Necessary since it might be different
          PixelFormat := pf24Bit;
          {$ENDIF}
        end;
      csGA,
      csIndexedA:
        begin
          TargetColorScheme := csBGRA;
          TargetSamplesPerPixel := 4;
        end;
      csRGB:
        if Channels = 3 then
        begin
          TargetColorScheme := csBGR;
          TargetSamplesPerPixel := 3;
        end
        else // 4 or more
        begin
          SourceColorScheme := csRGBA;
          TargetColorScheme := csBGRA;
          TargetSamplesPerPixel := 4;
        end;
      csRGBA:
        begin
          TargetColorScheme := csBGRA;
          TargetSamplesPerPixel := 4;
        end;
      csCMYK:
        begin
          TargetColorScheme := csBGR;
          TargetSamplesPerPixel := 3;
        end;
      csCMYKA:
        begin
          TargetColorScheme := csBGRA;
          TargetSamplesPerPixel := 4;
        end;
      csCIELab:
        begin
          SourceColorScheme := CurrentColorScheme;
          // PSD uses 0..255 for a and b so we need to convert them to -128..127
          SourceOptions := SourceOptions + [coLabByteRange, coLabChromaOffset];
          if Channels = 3 then begin
            TargetColorScheme := csBGR;
            TargetSamplesPerPixel := 3;
          end
          else begin // 4 or more
            TargetColorScheme := csBGRA;
            TargetSamplesPerPixel := 4;
          end;
        end;
    end;
    Result := TargetPixelFormat;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := Size > SizeOf(TPSDHeader);
  if Result then
    with PPSDHeader(Memory)^ do
      Result := (StrLIComp(Signature, '8BPS', 4) = 0) and (SwapEndian(Version) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Run: PByte;      // Pointer to the current position in the given memory.
  Count: Cardinal;

begin
  inherited;

  FLayers.Clear;
  Run := Memory;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      // Initialize outermost progress display.
      InitProgress(Width, 1);
      StartProgressSection(0, '');

      // Initialize sub section for image preparation. We give it a (guessed) value of 1%.
      StartProgressSection(1, gesPreparing);

      // Skip the header, image info is already read.
      Inc(Run, SizeOf(TPSDHeader));

      PixelFormat := SetupColorManager(SamplesPerPixel);
      Self.Width := Width;
      Self.Height := Height;

      // Read color mode data if present.
      // Size of palette.
      Count := ReadBigEndianCardinal(Run);
      // Setup the palette if necessary.
      case ColorScheme of
        csG: // For csGA we don't need to create a palette since we're converting it to BGRA
          Palette := ColorManager.CreateGrayscalePalette(ioMinIsWhite in Options);
        csIndexed:
          begin
            {$IFDEF LCMS}
            // TODO: Figure out how to handle interlaced palette color transform
            {$ENDIF}
            Palette := ColorManager.CreateColorPalette([Run, PAnsiChar(Run) + Count div 3,
              PAnsiChar(Run) + 2 * Count div 3], pfPlane8Triple, Count);
            ColorManager.SetSourcePalette([Run, PAnsiChar(Run) + Count div 3,
              PAnsiChar(Run) + 2 * Count div 3], pfPlane8Triple);
          end;
        csIndexedA:
          ColorManager.SetSourcePalette([Run, PAnsiChar(Run) + Count div 3,
            PAnsiChar(Run) + 2 * Count div 3], pfPlane8Triple);

      end;
      Inc(Run, Count);

      // The preparation part is finished. Finish also progress section (which will step the main progress).
      FinishProgressSection(False);

      // Read resource section.
      Count := ReadBigEndianCardinal(Run);
      if Count > 0 then
        ReadResources(Run);
      Inc(Run, Count);

      // Read layers section.
      Count := ReadBigEndianCardinal(Run);
      if Count > 0 then
        ReadLayers(Run);

      // Use +2 in order to skip the following compression value (which we already know).
      Inc(Run, Count + 2);
      // Setup the color manager again. It might be changed by the layer loading stuff.
      if FLayers.Count > 0 then
        SetupColorManager(SamplesPerPixel);
      ReadMergedImage(Run, nil, Compression, FChannels);

      FinishProgressSection(False);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PSD or PDD']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Run: PByte;
  Header: TPSDHeader;
  Count: Cardinal;
  TempRun: PByte;
  Temp: SmallInt;
begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Inc(Run, SizeOf(TPSDHeader));

      Move(Memory^, Header, SizeOf(TPSDHeader));
      if Header.Signature = '8BPS' then
      begin
        with Header do
        begin
          // PSD files are big endian only.
          Channels := SwapEndian(Channels);
          Rows := SwapEndian(Rows);
          Columns := SwapEndian(Columns);
          Depth := SwapEndian(Depth);
          Mode := SwapEndian(Mode);
        end;

        Options := [ioBigEndian];
        // Initialize color manager.
        BitsPerSample := Header.Depth;
        FChannels := Header.Channels;
        // 1..24 channels are supported in PSD files.
        // 2017-02-21 Current specs say 1..56 channels are supported: https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/
        // The documentation states that main image data (rgb(a), cmyk etc.) is always
        // written with the first channels in their component order.
        // We accept extra channels but will ignore them unless we know what to do with them.
        SamplesPerPixel := FChannels;

        BitsPerPixel := SamplesPerPixel * BitsPerSample;

        // color space
        FMode := Header.Mode;
        ColorScheme := DetermineColorScheme(SamplesPerPixel);
        if FMode = PSD_BITMAP then
          Include(Options, ioMinIsWhite);

        Width := Header.Columns;
        Height := Header.Rows;

        // Read the size of the palette.
        Count := ReadBigEndianCardinal(Run);
        // Skip palette (count is always given, might be 0 however, e.g. for RGB).
        Inc(Run, Count);

        // Skip resourcesection.
        Count := ReadBigEndianCardinal(Run);
        Inc(Run, Count);
        // Layer section: We want to know the number of layers.
        Count := ReadBigEndianCardinal(Run);
        if Count > 0 then begin
          TempRun := Run;
          Inc(TempRun,4); // Skip Layer Section Size
          Temp := SwapEndian(PSmallInt(TempRun)^);
          FLayerCount := Abs(Temp);
          FMergedTransparencyPresent := Temp < 0;
        end;
        // Skip layer section
        Inc(Run, Count);

        Compression := ConvertCompression(ReadBigEndianWord(Run));
        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif PhotoshopGraphic}

//----------------- TPSPGraphic ----------------------------------------------------------------------------------------

{$ifdef PaintshopProGraphic}

const
  // block identifiers
  PSP_IMAGE_BLOCK = 0;                      // General Image Attributes Block (main)
  PSP_CREATOR_BLOCK = 1;                    // Creator Data Block (main)
  PSP_COLOR_BLOCK = 2;                      // Color Palette Block (main and sub)
  PSP_LAYER_START_BLOCK = 3;                // Layer Bank Block (main)
    PSP_LAYER_BLOCK = 4;                    // Layer Block (sub)
    PSP_CHANNEL_BLOCK = 5;                  // Channel Block (sub)
  PSP_SELECTION_BLOCK = 6;                  // Selection Block (main)
  PSP_ALPHA_BANK_BLOCK = 7;                 // Alpha Bank Block (main)
    PSP_ALPHA_CHANNEL_BLOCK = 8;            // Alpha Channel Block (sub)
  PSP_THUMBNAIL_BLOCK = 9;                  // Thumbnail Block (main) [version 5]
  PSP_COMPOSITE_IMAGE_BLOCK = 9;            // Composite Image Block (sub) [version 6 and up]
  PSP_EXTENDED_DATA_BLOCK = 10;             // Extended Data Block (main)
  PSP_TUBE_BLOCK = 11;                      // Picture Tube Data Block (main)
    PSP_ADJUSTMENT_EXTENSION_BLOCK = 12;    // Adjustment Layer Extension Block (sub)
    PSP_VECTOR_EXTENSION_BLOCK = 13;        // Vector Layer Extension Block (sub)
    PSP_SHAPE_BLOCK = 14;                   // Vector Shape Block (sub)
    PSP_PAINTSTYLE_BLOCK = 15;              // Paint Style Block (sub)
  PSP_COMPOSITE_IMAGE_BANK_BLOCK = 16;      // Composite Image Bank (main)
    PSP_COMPOSITE_ATTRIBUTES_BLOCK = 17;    // Composite Image Attributes (sub)
    PSP_JPEG_BLOCK = 18;                    // JPEG Image Block (sub)

  // bitmap types
	PSP_DIB_IMAGE = 0;            // Layer color bitmap
	PSP_DIB_TRANS_MASK = 1;       // Layer transparency mask bitmap
	PSP_DIB_USER_MASK = 2;        // Layer user mask bitmap
	PSP_DIB_SELECTION= 3;         // Selection mask bitmap
	PSP_DIB_ALPHA_MASK = 4;       // Alpha channel mask bitmap
	PSP_DIB_THUMBNAIL = 5;        // Thumbnail bitmap
  PSP_DIB_THUMBNAIL_TRANS_MASK = 6; // Thumbnail transparency mask
  PSP_DIB_ADJUSTMENT_LAYER = 7; // Adjustment layer bitmap
  PSP_DIB_COMPOSITE = 8;        // Composite image bitmap
  PSP_DIB_COMPOSITE_TRANS_MASK = 9; // Composite image transparency

  // composite image type
  PSP_IMAGE_COMPOSITE = 0;      // Composite Image
  PSP_IMAGE_THUMBNAIL = 1;      // Thumbnail Image

  // graphic contents flags
  PSP_GC_RASTERLAYERS = 1;      // At least one raster layer
  PSP_GC_VectorLayers = 2;      // At least one vector layer
  PSP_GC_ADJUSTMENTLAYERS = 4;  // At least one adjustment layer
  // Additional attributes
  PSP_GC_THUMBNAIL = $01000000;              // Has a thumbnail
  PSP_GC_THUMBNAILTRANSPARENCY = $02000000;  // Thumbnail transp.
  PSP_GC_COMPOSITE = $04000000;              // Has a composite image
  PSP_GC_COMPOSITETRANSPARENCY = $08000000;  // Composite transp.
  PSP_GC_FLATIMAGE = $10000000;              // Just a background
  PSP_GC_SELECTION = $20000000;              // Has a selection
  PSP_GC_FLOATINGSELECTIONLAYER = $40000000; // Has float. selection
  PSP_GC_ALPHACHANNELS = $80000000;          // Has alpha channel(s)

  // character style flags
  PSP_STYLE_ITALIC = 1;         // Italic property bit
  PSP_STYLE_STRUCK = 2;         // Strike-out property bit
  PSP_STYLE_UNDERLINED = 4;     // Underlined property bit

  // layer flags
	PSP_LAYER_VISIBLEFLAG = 1;    // Layer is visible
	PSP_LAYER_MASKPRESENCEFLAG = 2; // Layer has a mask

  // Shape property flags
  PSP_SHAPE_ANTIALIASED = 1;    // Shape is anti-aliased
  PSP_SHAPE_Selected = 2;       // Shape is selected
  PSP_SHAPE_Visible = 4;        // Shape is visible

  // Polyline node type flags
  PSP_NODE_UNCONSTRAINED = 0;   // Default node type
  PSP_NODE_SMOOTH = 1;          // Node is smooth
  PSP_NODE_SYMMETRIC = 2;       // Node is symmetric
  PSP_NODE_ALIGNED = 4;         // Node is aligned
  PSP_NODE_ACTIVE = 8;          // Node is active
  PSP_NODE_LOCKED = 16;         // Node is locked (PSP doc says 0x16 here, but this seems to be a typo)
  PSP_NODE_SELECTED = 32;       // Node is selected (PSP doc says 0x32 here)
  PSP_NODE_VISIBLE = 64;        // Node is visible (PSP doc says 0x64 here)
  PSP_NODE_CLOSED = 128;        // Node is closed (PSP doc says 0x128 here)

  // Blend modes
	LAYER_BLEND_NORMAL = 0;
  LAYER_BLEND_DARKEN = 1;
  LAYER_BLEND_LIGHTEN = 2;
  LAYER_BLEND_HUE = 3;
  LAYER_BLEND_SATURATION = 4;
  LAYER_BLEND_COLOR = 5;
  LAYER_BLEND_LUMINOSITY = 6;
  LAYER_BLEND_MULTIPLY = 7;
  LAYER_BLEND_SCREEN = 8;
  LAYER_BLEND_DISSOLVE = 9;
  LAYER_BLEND_OVERLAY = 10;
  LAYER_BLEND_HARD_LIGHT = 11;
  LAYER_BLEND_SOFT_LIGHT = 12;
  LAYER_BLEND_DIFFERENCE = 13;
  LAYER_BLEND_DODGE = 14;
  LAYER_BLEND_BURN = 15;
  LAYER_BLEND_EXCLUSION = 16;
  LAYER_BLEND_ADJUST = 255;

  // Adjustment layer types
  PSP_ADJUSTMENT_NONE = 0;      // Undefined adjustment layer type
  PSP_ADJUSTMENT_LEVEL = 1;     // Level adjustment
  PSP_ADJUSTMENT_CURVE = 2;     // Curve adjustment
  PSP_ADJUSTMENT_BRIGHTCONTRAST = 3; // Brightness-contrast adjustment
  PSP_ADJUSTMENT_COLORBAL = 4;  // Color balance adjustment
  PSP_ADJUSTMENT_HSL = 5;       // HSL adjustment
  PSP_ADJUSTMENT_CHANNELMIXER = 6; // Channel mixer adjustment
  PSP_ADJUSTMENT_INVERT = 7;    // Invert adjustment
  PSP_ADJUSTMENT_THRESHOLD = 8; // Threshold adjustment
  PSP_ADJUSTMENT_POSTER = 9;    // Posterize adjustment

  // Vector shape types
  PSP_VST_Unknown = 0;          // Undefined vector type
  PSP_VST_TEXT = 1;             // Shape represents lines of text
  PSP_VST_POLYLINE = 2;         // Shape represents a multiple segment line
  PSP_VST_ELLIPSE = 3;          // Shape represents an ellipse (or circle)
  PSP_VST_POLYGON = 4;          // Shape represents a closed polygon

  // Text element types
  PSP_TET_UNKNOWN = 0;          // Undefined text element type
  PSP_TET_CHAR = 1;             // A single character code
  PSP_TET_CHARSTYLE = 2;        // A character style change
  PSP_TET_LINESTYLE = 3;        // A line style change

  // Text alignment types
  PSP_TAT_LEFT = 0;             // Left text alignment
  PSP_TAT_CENTER = 1;           // Center text alignment
  PSP_TAT_RIGHT = 2;            // Right text alignment

  // Paint style types
  PSP_STYLE_NONE = 0;           // Undefined paint style
  PSP_STYLE_COLOR = 1;          // Paint using color (RGB or palette index)
  PSP_STYLE_GRADIENT = 2;       // Paint using gradient

  // Channel types
	PSP_CHANNEL_COMPOSITE = 0;    // Channel of single channel bitmap
	PSP_CHANNEL_RED = 1;          // Red channel of 24 bit bitmap
	PSP_CHANNEL_GREEN = 2;        // Green channel of 24 bit bitmap
	PSP_CHANNEL_BLUE = 3;         // Blue channel of 24 bit bitmap

  // Resolution metrics
  PSP_METRIC_UNDEFINED = 0;	    // Metric unknown
  PSP_METRIC_INCH = 1;          // Resolution is in inches
  PSP_METRIC_CM = 2;            // Resolution is in centimeters

  // Compression types
	PSP_COMP_NONE = 0;            // No compression
	PSP_COMP_RLE = 1;             // RLE compression
	PSP_COMP_LZ77 = 2;            // LZ77 compression
  PSP_COMP_JPEG = 3;            // JPEG compression (only used by thumbnail and composite image)

  // Picture tube placement mode
	PSP_TPM_Random = 0;           // Place tube images in random intervals
	PSPS_TPM_Constant = 1;        // Place tube images in constant intervals

  // Tube selection mode
	PSP_TSM_RANDOM =0;            // Randomly select the next image in tube to display
	PSP_TSM_INCREMENTAL = 1;     // Select each tube image in turn
	PSP_TSM_ANGULAR = 2;          // Select image based on cursor direction
	PSP_TSM_PRESSURE = 3;         // Select image based on pressure (from pressure-sensitive pad)
	PSP_TSM_VELOCITY = 4;         // Select image based on cursor speed

  // Extended data field types
  PSP_XDATA_TRNS_INDEX = 0;     // Transparency index field

  // Creator field types
	PSP_CRTR_FLD_TITLE = 0;       // Image document title field
	PSP_CRTR_FLD_CRT_DATE = 1;    // Creation date field
	PSP_CRTR_FLD_MOD_DATE = 2;    // Modification date field
	PSP_CRTR_FLD_ARTIST = 3;      // Artist name field
	PSP_CRTR_FLD_CPYRGHT = 4;     // Copyright holder name field
	PSP_CRTR_FLD_DESC = 5;        // Image document description field
	PSP_CRTR_FLD_APP_ID = 6;      // Creating app id field
	PSP_CRTR_FLD_APP_VER = 7;     // Creating app version field

  // Creator application identifier
	PSP_CREATOR_APP_UNKNOWN = 0;  // Creator application unknown
	PSP_CREATOR_APP_PAINT_SHOP_PRO = 1; // Creator is Paint Shop Pro

  // Layer types (file version 3)
  PSP_LAYER_NORMAL = 0;         // Normal layer
  PSP_LAYER_FLOATING_SELECTION = 1; // Floating selection layer

  // Layer types (file version 4)
  PSP_LAYER_UNDEFINED = 0;      // Undefined layer type
  PSP_LAYER_RASTER = 1;         // Standard raster layer
  PSP_LAYER_FLOATINGRASTERSELECTION = 2; // Floating selection (raster layer)
  PSP_LAYER_Vector = 3;         // Vector layer
  PSP_LAYER_ADJUSTMENT = 4;     // Adjustment layer

  MagicID = 'Paint Shop Pro Image File';

type
  // These block header structures are here for informational purposes only because the data of those
  // headers is read member by member to generalize code for the different file versions
  TPSPBlockHeader3 = packed record          // block header file version 3
    HeaderIdentifier: array[0..3] of AnsiChar; // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    InitialChunkLength,                     // length of the first sub chunk header or similar
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPBlockHeader4 = packed record          // block header file version 4
    HeaderIdentifier: array[0..3] of AnsiChar; // i.e. "~BK" followed by a zero byte
    BlockIdentifier: Word;                  // one of the block identifiers
    TotalBlockLength: Cardinal;             // length of this block excluding this header
  end;

  TPSPColorPaletteInfoChunk = packed record
    EntryCount: Cardinal;                   // number of entries in the palette
  end;

  TPSPColorPaletteChunk = array[0..255] of TRGBQuad; // might actually be shorter

  TPSPChannelInfoChunk = packed record
    CompressedSize,
    UncompressedSize: Cardinal;
    BitmapType,                             // one of the bitmap types
    ChannelType: Word;                      // one of the channel types
  end;

  // PSP defines a channel content chunk which is just a bunch of bytes (size is CompressedSize).
  // There is no sense to define this record type here.

  PPSPFileHeader = ^TPSPFileHeader;
  TPSPFileHeader = packed record
    Signature: array[0..31] of AnsiChar;    // the string "Paint Shop Pro Image File\n\x1a", padded with zeroes
    MajorVersion,
    MinorVersion: Word;
  end;

  TPSPCompositeImageAttributes = packed record
    Width,
    Height: Integer;
    BitDepth,                               // The bit depth of the color bitmap in each Layer of the image document
                                            // (must be 1, 4, 8, 24 or 48).
    Compression,                            // Type of compression used to compress the composite image (one of PSPCompression, including PSP_COMP_JPEG)
    PlaneCount: Word;                       // Number of planes in the composite image (this value must be 1)
    ColorCount: Cardinal;                   // number of colors in the image (2^bit depth)
    CompositeImageType: Word;               // Type of composite image (PSP_IMAGE_COMPOSITE, PSP_IMAGE_THUMBNAIL)
  end;

  TPSPImageAttributes = packed record
    Width,
    Height: Integer;
    Resolution: Double;                     // Number of pixels per metric
    ResolutionMetric: Byte;                 // Metric used for resolution (one of the metric constants)
    Compression,                            // compression type of image (not thumbnail, it has its own compression)
    BitDepth,                               // The bit depth of the color bitmap in each Layer of the image document
                                            // (must be 1, 4, 8, 24 or 48).
    PlaneCount: Word;                       // Number of planes in each layer of the image document (usually 1)
    ColorCount: Cardinal;                   // number of colors in each layer (2^bit depth)
    GreyscaleFlag: Boolean;                 // Indicates whether the color bitmap in each layer of image document is a
                                            // greyscale (False = not greyscale, True = greyscale).
    TotalImageSize: Cardinal;               // Sum of the sizes of all layer color bitmaps.
    ActiveLayer: Integer;                   // Identifies the layer that was active when the image document was saved.
    LayerCount: Word;                       // Number of layers in the document.
    GraphicContents: Cardinal;              // A series of flags that helps define the image's graphic contents.
  end;

  TPSPLayerInfoChunk = packed record
    //LayerName: array[0..255] of AnsiChar; // Name of layer (in ASCII text). Has been replaced in version 4
                                            // by a Delphi like short string (length word and variable length string)
    LayerType: Byte;                        // Type of layer.
    ImageRectangle,                         // Rectangle defining image border.
    SavedImageRectangle: TRect;             // Rectangle within image rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    LayerOpacity: Byte;                     // Overall layer opacity.
    BlendingMode: Byte;                     // Mode to use when blending layer.
    Visible: Boolean;                       // TRUE if layer was visible at time of save, FALSE otherwise.
    TransparencyProtected: Boolean;         // TRUE if transparency is protected.
    LinkGroupIdentifier: Byte;              // Identifies group to which this layer belongs.
    MaskRectangle,                          // Rectangle defining user mask border.
    SavedMaskRectangle: TRect;              // Rectangle within mask rectangle that contains "significant" data
                                            // (only the contents of this rectangle are saved to the file).
    MaskLinked: Boolean;                    // TRUE if mask linked to layer (i.e., mask moves relative to layer)
    MaskDisabled: Boolean;                  // TRUE if mask is disabled, FALSE otherwise.
    InvertMask: Boolean;                    // TRUE if mask should be inverted when the layer is merged, FALSE otherwise.
    BlendRangeCount: Word;                  // Number of valid source-destination field pairs to follow (note, there are
                                            // currently always 5 such pairs, but they are not necessarily all valid).
    SourceBlendRange1,                      // First source blend range value.
    DestinationBlendRange1,                 // First destination blend range value.
    SourceBlendRange2,
    DestinationBlendRange2,
    SourceBlendRange3,
    DestinationBlendRange3,
    SourceBlendRange4,
    DestinationBlendRange4,
    SourceBlendRange5,
    DestinationBlendRange5: array[0..3] of Byte;
    // These fields are obsolete since file version 4, because there's an own chunk for them.
    // BitmapCount: Word;                      // Number of bitmaps to follow.
    // ChannelCount: Word;                     // Number of channels to follow.
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPSPGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  with PPSPFileHeader(Memory)^ do
    Result := (Size > SizeOf(TPSPFileHeader)) and (StrLIComp(Signature, MagicID, Length(MagicID)) = 0) and
      (MajorVersion >= 3) and (MajorVersion < 20);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPSPGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // To use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure.
  HeaderIdentifier: array[0..3] of AnsiChar; // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  ChunkSize: Cardinal;
  LayerInfo: TPSPLayerInfoChunk;
  ChannelInfo: TPSPChannelInfoChunk;
  LayerName: AnsiString;
  NameLength: Word;

  // file version 4 specific data
  BitmapCount,
  ChannelCount: Word;

  // load and decoding of image data
  R, G, B, C: PByte;
  RedBuffer,
  GreenBuffer,
  BlueBuffer,
  CompBuffer: Pointer;
  X, Y,
  Index: Integer;
  AbsoluteRect: TRECT; // Rect holding the position of the current layer within image
  LayerWidth,
//  LayerHeight,
  LayerRowSize,
  LayerStartOfs: Integer;

  // other data
  RawPalette: array[0..4 * 256 - 1] of Byte;

  LastPosition,
  NextMainBlock,
  NextLayerPosition: PAnsiChar; // PAnsiChar, because then direct pointer arithmethic is accepted.
  Run: PByte;
  iLayer: Integer; // Current layer used for progress updating

  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end).

  begin
    Result := (PAnsiChar(Run) - PAnsiChar(Memory)) < Size;
    if Result then
    begin
      Move(Run^, HeaderIdentifier, SizeOf(HeaderIdentifier));
      Inc(Run, SizeOf(HeaderIdentifier));

      Move(Run^, BlockIdentifier, SizeOf(BlockIdentifier));
      Inc(Run, SizeOf(BlockIdentifier));

      if Header.MajorVersion = 3 then
      begin
        Move(Run^, InitialChunkLength, SizeOf(InitialChunkLength));
        Inc(Run, SizeOf(InitialChunkLength));
      end;
      Move(Run^, TotalBlockLength, SizeOf(TotalBlockLength));
      Inc(Run, SizeOf(TotalBlockLength));
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadAndDecompress(Target: Pointer);

  // reads a stream of data from file stream and decompresses it into Target

  var
    Source: Pointer;

  begin
    Decoder := nil;
    try
      Source := Run;
      case Image.Compression of
        PSP_COMP_RLE:
          begin
            Decoder := TPSPRLEDecoder.Create;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_LZ77:
          begin
            Decoder := TLZ77Decoder.Create(Z_FINISH, False);
            Decoder.DecodeInit;
            Decoder.Decode(Source, Target, ChannelInfo.CompressedSize, ChannelInfo.UncompressedSize);
          end;
        PSP_COMP_JPEG: // here just for completeness, used only in thumbnails and composite images
          ;
      end;
      Inc(Run, ChannelInfo.CompressedSize);
      Decoder.DecodeEnd;
    finally
      FreeAndNil(Decoder);
    end;
  end;

  //---------------------------------------------------------------------------

  procedure ReadChannelData;

  // Reads the actual data of one channel from the current stream position.
  // Decompression is done by the way.

  begin
    ReadBlockHeader;
    if Header.MajorVersion > 3 then
    begin
      Move(Run^, ChunkSize, SizeOf(ChunkSize));
      Inc(Run, SizeOf(ChunkSize));
    end;
    Move(Run^, ChannelInfo, SizeOf(ChannelInfo));
    Inc(Run, SizeOf(ChannelInfo));

    case ChannelInfo.ChannelType of
      PSP_CHANNEL_COMPOSITE: // Single channel bitmap (indexed or transparency mask).
        begin
          // Damaged files can have more than one composite channel. Make sure we do not
          // allocate the buffer more than once without freeing it.
          // Do not use Realloc here as it copies the memory block.
          if Assigned(CompBuffer) then
            FreeMem(CompBuffer);
          GetMem(CompBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(CompBuffer)
          else
          begin
            Move(Run^, CompBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_RED:  // Red channel of 24 bit bitmap.
        begin
          GetMem(RedBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(RedBuffer)
          else
          begin
            Move(Run^, RedBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_GREEN:
        begin
          GetMem(GreenBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(GreenBuffer)
          else
          begin
            Move(Run^, GreenBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
      PSP_CHANNEL_BLUE:
        begin
          GetMem(BlueBuffer, ChannelInfo.UncompressedSize);
          if Image.Compression <> PSP_COMP_NONE then
            ReadAndDecompress(BlueBuffer)
          else
          begin
            Move(Run^, BlueBuffer^, ChannelInfo.CompressedSize);
            Inc(Run, ChannelInfo.CompressedSize);
          end;
        end;
    end;
  end;

  procedure ConvertRows(SourceWidth: Integer; AbsoluteRect: TRect; ChannelStart: PByte);
  var
    RowSize, IgnoredRowsCount, Delta, //OriginalLeft,
    StartOfs, VisibleWidth: Integer;
    X, Y: Integer;
  begin
    Move(Run^, BitmapCount, SizeOf(BitmapCount));
    Inc(Run, SizeOf(BitmapCount));

    Move(Run^, ChannelCount, SizeOf(ChannelCount));
    Inc(Run, SizeOf(ChannelCount));

    // ChannelCount can be 0 for a layer when the actual size of visible area is 0.
    if (ChannelCount = 0) or (BitmapCount = 0) then
      Exit;

    // By now we can reliably say whether we have an alpha channel or not.
    // This kind of information can only be read very late and causes us to
    // possibly reallocate the entire image (because it is copied by the VCL
    // when changing the pixel format).
    // I don't know another way (preferably before the size of the image is set).
    // We need to take BitmapCount into consideration. However since most of
    // the time with BitmapCount = 2 that extra Bitmap is a mask which is
    // effectively an Alpha layer, we just ignore it for now
    if ChannelCount > 3 then
    begin
      FImageProperties.ColorScheme := csRGBA;
      ColorManager.SourceColorScheme := csRGBA;
      ColorManager.TargetColorScheme := csBGRA;
      PixelFormat := pf32Bit;
    end;

    if FImageProperties.Version > 3 then
      Run := ChannelStart; //Pointer(LastPosition + ChunkSize);

    // TODO: For composite images I think we can encounter a color palette
    // block too for images where that is relevant.
    // However we need an example image to test that.

    // allocate memory for all channels and read raw data
    for X := 0 to ChannelCount - 1 do
      ReadChannelData;

    R := RedBuffer;
    G := GreenBuffer;
    B := BlueBuffer;
    C := CompBuffer;

    if ColorManager.TargetColorScheme in [csIndexed, csG] then
    begin
      // Make sure we got a valid grayscale channel
      if not Assigned(C) then
        GraphicExError(gesInvalidImage, ['PSP']);

      if AbsoluteRect.Left < 0 then begin
        Delta := abs(AbsoluteRect.Left);
        AbsoluteRect.Left := 0;
      end
      else
        Delta := 0;

      case FImageProperties.BitsPerSample of // TODO: What about 16 bits per channel
        1: begin
             RowSize := (SourceWidth + 7) div 8;
             StartOfs := (AbsoluteRect.Left + 7) div 8;
           end;
        4: begin
             RowSize := (SourceWidth + 1) div 2;
             StartOfs := (AbsoluteRect.Left + 1) div 2;
           end;
      else // 8
        RowSize := SourceWidth;
        StartOfs := AbsoluteRect.Left;
      end;

      // From PSP spec: Each scanline in the image data is stored on a 4 byte boundary.
      // Therefore we need to make sure LayerRowSize is a multiple of 4.
      // Note: Nowhere do I see any mention that 8 BitsPerSample is being excluded
      // from this but with the sample images we have this seems to be the case.
      if FImageProperties.BitsPerSample <> 8 then
        RowSize := (RowSize + 3) div 4 * 4;

      // Since parts of a layer can be outside of the image boundaries we need to
      // clip the layer data to what is visible in the image.
      if AbsoluteRect.Top < 0 then begin
        IgnoredRowsCount := abs(AbsoluteRect.Top);
        AbsoluteRect.Top := 0;
        Inc(C, IgnoredRowsCount*RowSize + Delta);
      end
      else if Delta > 0 then
        Inc(C, Delta);
      if AbsoluteRect.Right > Width then begin
        AbsoluteRect.Right := Width;
      end;
      if AbsoluteRect.Bottom > Height then begin
        AbsoluteRect.Bottom := Height;
      end;
      VisibleWidth := AbsoluteRect.Right - AbsoluteRect.Left;

      {$IFDEF FPC}
      TargetColorScheme := csBGR;
      TargetBitsPerSample := 8;
      TargetSamplesPerPixel := 3;
      PixelFormat := pf24Bit;
      {$ENDIF}
      for Y := AbsoluteRect.Top to AbsoluteRect.Bottom - 1 do
      begin
        // Note: I don't have any samples for BPS = 1 or 4 and am not
        // sure if StartOfs in those cases is correct.
        ColorManager.ConvertRow([C], PAnsiChar(ScanLine[Y])+StartOfs, VisibleWidth, $FF);
        Inc(C, RowSize);
      end;
    end
    else
    begin // scBGR(A)
      // Make sure we got valid RGB or RGBA channels
      if not (Assigned(R) and Assigned(G) and Assigned(B) and
        ((ChannelCount = 3) or Assigned(C))) then
        GraphicExError(gesInvalidImage, ['PSP']);

      // Since BPS should be always 8 here RowSize is the same as SourceWidth.
      // TODO What about 16 bits channels!
      RowSize := SourceWidth;
      // Since parts of a layer can be outside of the image boundaries we need to
      // clip the layer data to what is visible in the image.
      if AbsoluteRect.Top < 0 then begin
        IgnoredRowsCount := abs(AbsoluteRect.Top);
        AbsoluteRect.Top := 0;
        Inc(R, IgnoredRowsCount*RowSize);
        Inc(G, IgnoredRowsCount*RowSize);
        Inc(B, IgnoredRowsCount*RowSize);
        Inc(C, IgnoredRowsCount*RowSize);
      end;
      if AbsoluteRect.Left < 0 then begin
        Delta := abs(AbsoluteRect.Left);
        VisibleWidth := SourceWidth - Delta;
        Inc(R, Delta);
        Inc(G, Delta);
        Inc(B, Delta);
        Inc(C, Delta);
        AbsoluteRect.Left := 0;
      end
      else
        VisibleWidth := SourceWidth;
      if AbsoluteRect.Right > Width then begin
        Delta := AbsoluteRect.Right - Width;
        VisibleWidth := VisibleWidth - Delta;
        AbsoluteRect.Right := Width;
      end;
      if AbsoluteRect.Bottom > Height then begin
        AbsoluteRect.Bottom := Height;
      end;
      // TODO: Check that AbsoluteRect.Bottom is inside image boundary.
      // PSP has separate channels thus we need to set that in source options.
      ColorManager.SourceOptions := ColorManager.SourceOptions + [coSeparatePlanes];
      // Compute start offset in ScanLine for this layer
      if ColorManager.TargetColorScheme = csBGR then
        StartOfs := AbsoluteRect.Left * 3  // 3 bytes per pixel
      else // csBGRA
        StartOfs := AbsoluteRect.Left * 4; // 4 bytes per pixel
      for Y := AbsoluteRect.Top to AbsoluteRect.Bottom - 1 do
      begin
        ColorManager.ConvertRow([R, G, B, C], PAnsiChar(ScanLine[Y])+StartOfs,
          VisibleWidth, $FF);
        Inc(R, RowSize);
        Inc(G, RowSize);
        Inc(B, RowSize);
        Inc(C, RowSize);
      end;
    end;

    // Since we may be reading multiple layers we need to free the
    // Channel data here or we might cause leaked memory.
    // Since we may get another check for assigned in ReadChannelData or
    // when finishing this function, we need to set all of them to nil.
    if Assigned(RedBuffer) then begin
      FreeMem(RedBuffer);
      RedBuffer := nil;
    end;
    if Assigned(GreenBuffer) then begin
      FreeMem(GreenBuffer);
      GreenBuffer := nil;
    end;
    if Assigned(BlueBuffer) then begin
      FreeMem(BlueBuffer);
      BlueBuffer := nil;
    end;
    if Assigned(CompBuffer) then begin
      FreeMem(CompBuffer);
      CompBuffer := nil;
    end;
  end;

  {$IFDEF USE_GEXJPEG}
  function ConvertJpegChunk(JpegCompressedData: PByte): Boolean;
  var
    CompressedImgSize: Cardinal;
    UncompressedImgSize: Cardinal;
    ImageType: Word;
    JpegContent: TgexJpegImage;
  begin
    Result := False;
    Move(Run^, CompressedImgSize, SizeOf(CompressedImgSize));
    Inc(Run, SizeOf(CompressedImgSize));

    Move(Run^, UncompressedImgSize, SizeOf(UncompressedImgSize));
    Inc(Run, SizeOf(UncompressedImgSize));

    Move(Run^, ImageType, SizeOf(ImageType));
    Inc(Run, SizeOf(ImageType));

    // Now uncompress the Jpeg Compressed Data
    if (ImageType = PSP_DIB_COMPOSITE) and (PWord(JpegCompressedData)^= $d8ff) then begin
      JpegContent := TgexJpegImage.Create;
      try
        // Load the Jpeg Data.
        JpegContent.LoadFromMemory(JpegCompressedData, CompressedImgSize, 0);
        // Move to our PSP bitmap
        // Note this has known side effects to our Image Info which for now we will ignore.
        // TODO: Make sure that our PSP image info is not overwritten.
        Self.Assign(JpegContent);
        Result := True;
      finally
        JpegContent.Free;
      end;
    end;
  end;
  {$ENDIF}

  // Returns True if we can use composite image, False if we need to use the layers.
  function HandleCompositeImage(): Boolean;
  var
    NextBlock: PAnsiChar;
    CompositeImageCount: Cardinal;
    CompositeImgInfo: TPSPCompositeImageAttributes;
    CompositeImgIndex, i: Integer;
    ImageRect: TRect;
  begin
    Result := False;
    // Since all samples of version 4 we have seen only had a smaller scale thumbnail
    // we will only handle composite images for version 5 and up.
    if FImageProperties.Version > 4 then begin
      // First we get the Composite Image Bank Information Chunk
      LastPosition := PAnsiChar(Run);
      // First DWORD is Chunk size
      Move(Run^, ChunkSize, SizeOf(ChunkSize));
      Inc(Run, SizeOf(ChunkSize));
      // Second DWORD is composite image count
      Move(Run^, CompositeImageCount, SizeOf(CompositeImageCount));
      //Inc(Run, SizeOf(CompositeImageCount)); Not needed atm since other data will be skipped anyway.
      // Any other data in this chunk is unknown and should be skipped
      Run := Pointer(LastPosition + ChunkSize);
      LastPosition := PAnsiChar(Run);

      // Now follows CompositeImageCount number of Composite Image Attributes Entries chunks (5.4.6)
      // Each starts with a block header with identifier PSP_COMPOSITE_ATTRIBUTES_BLOCK
      // For us only the info for the block with the full image is relevant so we will skip anything else.
      CompositeImgIndex := -1;
      i := 0;
      repeat
        if not ReadBlockHeader then
          Break;
        NextBlock := Pointer(PAnsiChar(Run) + TotalBlockLength);
        if BlockIdentifier <> PSP_COMPOSITE_ATTRIBUTES_BLOCK then
          Break;
        // Now read the Composite Image Attributes Information Chunk
        //LastPosition := PAnsiChar(Run);
        // First DWORD is Chunk size
        Move(Run^, ChunkSize, SizeOf(ChunkSize));
        Inc(Run, SizeOf(ChunkSize));
        Move(Run^, CompositeImgInfo, SizeOf(CompositeImgInfo));
        // Check if we have the full composite image or a thumbnail:
        if CompositeImgInfo.CompositeImageType = PSP_IMAGE_COMPOSITE then begin
          // TODO: Do we also need to check that Width, Height etc. are also the same
          // as mentioned in the main image info header?
          CompositeImgIndex := i;
          //Break;
        end;
        Inc(i);
        // Any other data in this chunk is unknown and should be skipped
        Run := Pointer(NextBlock);
      until False;
      if CompositeImgIndex = -1 then
        Exit;

      // After that CompositeImageCount number of Composite Image Entries chunks (5.4.7)
      // Each starts with a block header with either identifier PSP_ JPEG_BLOCK or PSP_COMPOSITE_IMAGE_BLOCK
      // We only need to handle block number CompositeImgIndex, skip the others
      i := 0;
      repeat
        if i = CompositeImgIndex then begin
          Break;
        end;
        // skip this block since it's not the one we want
        Run := Pointer(NextBlock);
        if not ReadBlockHeader then
          Break;
        Inc(i);
        NextBlock := Pointer(PAnsiChar(Run) + TotalBlockLength);
      until False;
      if i = CompositeImgIndex then begin
        LastPosition := PAnsiChar(Run);
        // Get length of Jpeg or Composite Image Information Chunk
        Move(Run^, ChunkSize, SizeOf(ChunkSize));
        Inc(Run, SizeOf(ChunkSize));
        case BlockIdentifier of
          PSP_JPEG_BLOCK:
          begin
            {$IFDEF USE_GEXJPEG}
            // Only use Jpeg composite image if the image has vector or adjustment layers
            // (because we can't convert these) and no raster layers. Because
            // Jpeg doesn't handle transparency and we prefer to see transparent areas
            // over not seeing more rare vector or adjustment layers.
            // TODO: We should add an option to our Reader class where the user can choose
            // whether to prefer Jpeg composite image over being able to see transparency.
            if (Image.GraphicContents and (PSP_GC_VECTORLAYERS+PSP_GC_ADJUSTMENTLAYERS) > 0) and
               (Image.GraphicContents and PSP_GC_RASTERLAYERS = 0) then
              Result := ConvertJpegChunk(Pointer(LastPosition + ChunkSize));
            {$ENDIF}
          end;
          PSP_COMPOSITE_IMAGE_BLOCK:
          begin
            if Image.GraphicContents and PSP_GC_ALPHACHANNELS = 0 then begin

            // Set up ImageRect for the whole image and start converting rows
            ImageRect.Top := 0;
            ImageRect.Left := 0;
            ImageRect.Height := FImageProperties.Height-1;
            ImageRect.Width := FImageProperties.Width-1;
            ConvertRows(FImageProperties.Width, ImageRect, Pointer(LastPosition + ChunkSize));

            Result := True;
            end
            else
              Result := False;
          end;
        end;
      end
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;
    RedBuffer := nil;
    GreenBuffer := nil;
    BlueBuffer := nil;
    CompBuffer := nil;
    with FImageProperties do  // TODO: Remove with
    try
      // Initialize outermost progress display.
      InitProgress(Width, 1);
      StartProgressSection(0, '');

      // Start of first progress subsection, guessed at 1%
      StartProgressSection(1, gesLoadingData);

      // Check for valid BitsPerSample
      if not (BitsPerSample in [1, 4, 8, 16]) then
        GraphicExError(gesInvalidColorFormat, ['PSP']);

      Move(Run^, Header, SizeOf(Header));
      Inc(Run, SizeOf(Header));

      // Read general image attribute block.
      ReadBlockHeader;
      LastPosition := PAnsiChar(Run);
      if Version > 3 then
      begin
        Move(Run^, ChunkSize, SizeOf(ChunkSize));
        Inc(Run, SizeOf(ChunkSize));
      end;
      Move(Run^, Image, SizeOf(Image));
      Run := Pointer(LastPosition + TotalBlockLength);

      with ColorManager, Image do // TODO: Remove with
      begin
        SourceOptions := [];
        SourceBitsPerSample := BitsPerSample;
        if BitsPerSample <= 8 then
          TargetBitsPerSample := BitsPerSample
        else
          TargetBitsPerSample := 8;
        SourceSamplesPerPixel := SamplesPerPixel;
        TargetSamplesPerPixel := SamplesPerPixel;
        SourceColorScheme := ColorScheme;
        if ColorScheme = csRGB then begin
          // Even when this is set it doesn't mean we will have an alpha channel
          // next to our color channels apparently.
          // This flag is possibly used when the optional "Alpha Bank Block" is present.
          // The Alpha Bank Block is an optional block that defines alpha channels associated
          // with the image document. (See 2.4 in psp fileformat specification 7.)
          // Example: "Marble head.PspImage"
          {if GraphicContents and PSP_GC_ALPHACHANNELS = PSP_GC_ALPHACHANNELS then
            // alpha channel present (jb: I haven't encountered this yet, example needed)
            TargetColorScheme := csBGRA
          else}
            TargetColorScheme := csBGR;
        end
        else
          TargetColorScheme := ColorScheme;

        PixelFormat := TargetPixelFormat;
      end;

      Self.Width := Width;
      Self.Height := Height;

      // Finish first progress subsection
      FinishProgressSection(False);

      // go through main blocks and read what is needed
      repeat
        if not ReadBlockHeader then
          Break;
        NextMainBlock := Pointer(PAnsiChar(Run) + TotalBlockLength);
        // no more blocks?
        if HeaderIdentifier[0] <> '~' then
          Break;

        case BlockIdentifier of
          PSP_COMPOSITE_IMAGE_BANK_BLOCK:
            begin
              // composite image block, if present then it must appear before the layer start block
              // and represents a composition of several layers
              // If we can find a valid full size composite image then we stop
              // here because then we don't need to loop over all layers to get an image.
              if HandleCompositeImage() then
                Break;
            end;
          PSP_LAYER_START_BLOCK:
          begin
            iLayer := Image.LayerCount;
            // Start next/last progress subsection: loading layers
            StartProgressSection(0, gesLoadingData);
            repeat
              if not ReadBlockHeader then
                Break;

              // calculate start of next (layer) block in case we need to skip this one
              NextLayerPosition := Pointer(PAnsiChar(Run) + TotalBlockLength);
              // if all layers have been considered the break loop to continue with other blocks if necessary
              if BlockIdentifier <> PSP_LAYER_BLOCK then
                Break;

              // layer information chunk
              if Version > 3 then
              begin
                LastPosition := PAnsiChar(Run);
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));

                Move(Run^, NameLength, SizeOf(NameLength));
                Inc(Run, SizeOf(NameLength));
                SetLength(LayerName, NameLength);
                if NameLength > 0 then
                begin
                  Move(Run^, LayerName[1], NameLength);
                  Inc(Run, NameLength);
                end;
                Move(Run^, LayerInfo, SizeOf(LayerInfo));
                Inc(Run, SizeOf(LayerInfo));
                Run := Pointer(LastPosition + ChunkSize);

                // continue only with undefined or raster chunks
                if not (LayerInfo.LayerType in [PSP_LAYER_UNDEFINED, PSP_LAYER_RASTER]) or
                   not LayerInfo.Visible then
                begin
                  Run := Pointer(NextLayerPosition);
                  Continue;
                end;

                // in file version 4 there's also an additional bitmap chunk which replaces
                // two fields formerly located in the LayerInfo chunk
                LastPosition := PAnsiChar(Run);
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));
              end
              else
              begin
                SetLength(LayerName, 256);
                Move(Run^, LayerName[1], 256);
                Inc(Run, 256);

                Move(Run^, LayerInfo, SizeOf(LayerInfo));
                Inc(Run, SizeOf(LayerInfo));

                // Continue only with normal (raster) chunks.
                // We ignore vector and adjustment layers for now.
                if LayerInfo.LayerType <> PSP_LAYER_NORMAL then
                begin
                  Run := Pointer(NextLayerPosition);
                  Continue;
                end;
              end;

              // PSP defines for each layer an ImageRectangle that defines the
              // position of the layer in the image and a SavedImageRectangle that
              // defines the parts inside ImageRectangle that are used (and thus were saved)
              // Therefore we should only convert pixels inside that rectangle and not
              // the entire image!
              // First compute actual rectangle within image
              AbsoluteRect := LayerInfo.ImageRectangle;
              Inc(AbsoluteRect.Left, LayerInfo.SavedImageRectangle.Left);
              AbsoluteRect.Right := AbsoluteRect.Left + LayerInfo.SavedImageRectangle.Right -
                LayerInfo.SavedImageRectangle.Left;
              Inc(AbsoluteRect.Top, LayerInfo.SavedImageRectangle.Top);
              AbsoluteRect.Bottom := AbsoluteRect.Top + LayerInfo.SavedImageRectangle.Bottom -
                LayerInfo.SavedImageRectangle.Top;
              // Saved layer width
              LayerWidth := LayerInfo.SavedImageRectangle.Right - LayerInfo.SavedImageRectangle.Left;
              // Precompute LayerHeight for use in Progress
              // Currently not used for progress, progress needs to be revised for multilayer support.
              //LayerHeight := AbsoluteRect.Bottom - AbsoluteRect.Top;

              ConvertRows(LayerWidth, AbsoluteRect, Pointer(LastPosition + ChunkSize));

              // Update progress
              Dec(iLayer);
              AdvanceProgress( 100 / Image.LayerCount-iLayer, 0, 1, True);
            until False; // layer loop
            FinishProgressSection(False);
          end; // PSP_LAYER_START_BLOCK
          PSP_COLOR_BLOCK:  // color palette block (this is also present for gray scale and b&w images)
            begin
              if Version > 3 then
              begin
                Move(Run^, ChunkSize, SizeOf(ChunkSize));
                Inc(Run, SizeOf(ChunkSize));
              end;
              Move(Run^, Index, SizeOf(Index));
              Inc(Run, SizeOf(Index));

              Move(Run^, RawPalette, Index * SizeOf(TRGBQuad));
              Inc(Run, Index * SizeOf(TRGBQuad));
              Palette := ColorManager.CreateColorPalette([@RawPalette], pfInterlaced8Quad, Index, False {BGR order});
              {$IFDEF FPC}
              ColorManager.SetSourcePalette([@RawPalette], pfInterlaced8Quad, False {BGR order});
              {$ENDIF}
            end;
        end;

        // explicitly set stream position to next main block as we might have read a block only partially
        Run := Pointer(NextMainBlock);
      until False; // main block loop
    finally
      FinishProgressSection(False);
      if Assigned(RedBuffer) then
        FreeMem(RedBuffer);
      if Assigned(GreenBuffer) then
        FreeMem(GreenBuffer);
      if Assigned(BlueBuffer) then
        FreeMem(BlueBuffer);
      if Assigned(CompBuffer) then
        FreeMem(CompBuffer);
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PSP']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPSPGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Header: TPSPFileHeader;
  Image: TPSPImageAttributes;
  // to use the code below for file 3 and 4 I read the parts of the block header
  // separately instead as a structure
  HeaderIdentifier: array[0..3] of AnsiChar; // i.e. "~BK" followed by a zero byte
  BlockIdentifier: Word;                  // one of the block identifiers
  InitialChunkLength,                     // length of the first sub chunk header or similar
  TotalBlockLength: Cardinal;             // length of this block excluding this header

  ChunkSize: Cardinal;

  LastPosition,
  Run: PByte;

  //--------------- local functions -------------------------------------------

  function ReadBlockHeader: Boolean;

  // Fills in the block header variables according to the file version.
  // Returns True if a block header could be read otherwise False (stream end).

  begin
    Result := (PAnsiChar(Run) - PAnsiChar(Memory)) < Size;
    if Result then
    begin
      Move(Run^, HeaderIdentifier, SizeOf(HeaderIdentifier));
      Inc(Run, SizeOf(HeaderIdentifier));

      Move(Run^, BlockIdentifier, SizeOf(BlockIdentifier));
      Inc(Run, SizeOf(BlockIdentifier));

      if Header.MajorVersion = 3 then
      begin
        Move(Run^, InitialChunkLength, SizeOf(InitialChunkLength));
        Inc(Run, SizeOf(InitialChunkLength));
      end;
      Move(Run^, TotalBlockLength, SizeOf(TotalBlockLength));
      Inc(Run, SizeOf(TotalBlockLength));
    end;
  end;

  //--------------- end local functions ---------------------------------------

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Header, SizeOf(Header));
      Inc(Run, SizeOf(Header));

      if (StrLIComp(Header.Signature, MagicID, Length(MagicID)) = 0) and
         (Header.MajorVersion >= 3) and (Header.MajorVersion < 20) then
      begin
        Version := Header.MajorVersion;

        // read general image attribute block
        ReadBlockHeader;
        LastPosition := Run;
        if Header.MajorVersion > 3 then
        begin
          Move(Run^, ChunkSize, SizeOf(ChunkSize));
          Inc(Run, SizeOf(ChunkSize));
        end;
        Move(Run^, Image, SizeOf(Image));
        Run := Pointer(PAnsiChar(LastPosition) + TotalBlockLength);

        if Image.BitDepth >= 24 then
        begin
          SamplesPerPixel := 3;
          BitsPerSample := Image.BitDepth div 3;
          ColorScheme := csRGB; // an alpha channel might exist, this is determined by the layer's channel count
        end
        else
        begin
          BitsPerSample := Image.BitDepth;
          SamplesPerPixel := 1;
          if Image.GreyscaleFlag then
            ColorScheme := csG
          else
            ColorScheme := csIndexed;
        end;
        BitsPerPixel := BitsPerSample * SamplesPerPixel;

        Width := Image.Width;
        Height := Image.Height;

        case Image.Compression of
          PSP_COMP_NONE:
            Compression := ctNone;
          PSP_COMP_RLE:
            Compression := ctRLE;
          PSP_COMP_LZ77:
            Compression := ctLZ77;
          {PSP_COMP_JPEG: // This is not valid for the image as a whole, only for composite/thumbnail images.
            Compression := ctJPEG;}
        else
          Compression := ctUnknown;
        end;
        XResolution := Image.Resolution;
        if Image.ResolutionMetric = PSP_METRIC_CM then
          XResolution := XResolution * 2.54;
        YResolution := XResolution;
        Result := True;
      end
      else
        Result := False;
    end;
end;

{$endif PaintshopProGraphic}

//----------------- TPNGGraphic ----------------------------------------------------------------------------------------

{$ifdef PortableNetworkGraphic}

const
  PNGMagic: PAnsiChar = #137'PNG'#13#10#26#10;

  // Recognized and handled chunk types.
  IHDR: TChunkType = 'IHDR';
  IDAT: TChunkType = 'IDAT';
  IEND: TChunkType = 'IEND';
  PLTE: TChunkType = 'PLTE';
  gAMA: TChunkType = 'gAMA';
  tRNS: TChunkType = 'tRNS';
  bKGD: TChunkType = 'bKGD';
  tEXt: TChunkType = 'tEXt';
  iCCP: TChunkType = 'iCCP';

  CHUNKMASK = $20; // used to check bit 5 in chunk types

type
  // The following chunks structures are those which appear in the data field of the general chunk structure
  // given above.

  // chunk type: 'IHDR'
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width,
    Height: Cardinal;
    BitDepth,          // bits per sample (allowed are 1, 2, 4, 8 and 16)
    ColorType,         // combination of:
                       //   1 - palette used
                       //   2 - colors used
                       //   4 - alpha channel used
                       // allowed values are:
                       //   0 - gray scale (allowed bit depths are: 1, 2, 4, 8, 16)
                       //   2 - RGB (8, 16)
                       //   3 - palette (1, 2, 4, 8)
                       //   4 - gray scale with alpha (8, 16)
                       //   6 - RGB with alpha (8, 16)
    Compression,       // 0 - LZ77, others are not yet defined
    Filter,            // filter mode 0 is the only one currently defined
    Interlaced: Byte;  // 0 - not interlaced, 1 - Adam7 interlaced
  end;

//----------------------------------------------------------------------------------------------------------------------

class function TPNGGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;

begin
  Result := (Size > SizeOf(PNGMagic) + SizeOf(TIHDRChunk)) and (StrLIComp(PAnsiChar(Memory), PNGMagic, 8) = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.IsChunk(ChunkType: TChunkType): Boolean;

// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type

const
  Mask = not $20202020;

begin
  Result := (FHeader.ChunkMask and Mask) = (PDWORD(@ChunkType)^ and Mask);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.LoadAndSwapHeader(var Source: PByte): Cardinal;

// read next chunk header and swap fields to little endian,
// returns the intial CRC value for following checks

begin
  Move(Source^, FHeader, SizeOf(FHeader));
  Inc(Source, SizeOf(FHeader));

  Result := CRC32(0, @FHeader.ChunkType, 4);
  FHeader.Length := SwapEndian(FHeader.Length);
end;

//----------------------------------------------------------------------------------------------------------------------

function PaethPredictor(a, b, c: Byte): Byte;

var
  p, pa, pb, pc: Integer;

begin
  // a = left, b = above, c = upper left
  p := a + b - c;        // initial estimate
  pa := Abs(p - a);      // distances to a, b, c
  pb := Abs(p - b);
  pc := Abs(p - c);
  // return nearest of a, b, c, breaking ties in order a, b, c
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);

// Applies the filter given in Filter to all bytes in Line (potentially using PrevLine).
// Note: The filter type is assumed to be of filter mode 0, as this is the only one currently
//       defined in PNG.
//       In opposition to the PNG documentation different identifiers are used here.
//       Raw refers to the current, not yet decoded value. Decoded refers to the current, already
//       decoded value (this one is called "raw" in the docs) and Prior is the current value in the
//       previous line. For the Paeth prediction scheme a fourth pointer is used (PriorDecoded) to describe
//       the value in the previous line but less the BPP value (Prior[x - BPP]).

var
  I: Integer;
  Raw,
  Decoded,
  Prior,
  PriorDecoded,
  TargetRun: PByte;

begin
  case Filter of
    0: // no filter, just copy data
      Move(Line^, Target^, BytesPerRow);
    1: // subtraction filter
      begin
        Raw := Line;
        TargetRun := Target;
        // Transfer BPP bytes without filtering. This mimics the effect of bytes left to the
        // scanline being zero.
        Move(Raw^, TargetRun^, BPP);

        // now do rest of the line
        Decoded := TargetRun;
        Inc(Raw, BPP);
        Inc(TargetRun, BPP);
        Dec(BytesPerRow, BPP);
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Decoded^);
          Inc(Raw);
          Inc(Decoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    2: // Up filter
      begin
        Raw := Line;
        Prior := PrevLine;
        TargetRun := Target;
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Prior^);
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    3: // average filter
      begin
        // first handle BPP virtual pixels to the left
        Raw := Line;
        Decoded := Line;
        Prior := PrevLine;
        TargetRun := Target;
        for I := 0 to BPP - 1 do
        begin
          TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
        end;
        Dec(BytesPerRow, BPP);

        // now do rest of line
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
   4: // paeth prediction
     begin
       // again, start with first BPP pixel which would refer to non-existing pixels to the left
       Raw := Line;
       Decoded := Target;
       Prior := PrevLine;
       PriorDecoded := PrevLine;
       TargetRun := Target;
       for I := 0 to BPP - 1 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
         Inc(Raw);
         Inc(Prior);
         Inc(TargetRun);
       end;
       Dec(BytesPerRow, BPP);

       // finally do rest of line
       while BytesPerRow > 0 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(PriorDecoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
       end;
     end;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  Description: TIHDRChunk;
  Run: PByte;
  PaletteBuf: Pointer;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    with FImageProperties do
    begin
      Run := Pointer(PAnsiChar(Memory) + 8); // skip magic

      FProgressRect := Rect(0, 0, Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      PaletteBuf := nil;
      FPalette := 0;
      FTransparency := nil;
      FBackgroundColor := clWhite;
      FTransparentColor := clNone;

      // First chunk must be an IHDR chunk.
      FCurrentCRC := LoadAndSwapHeader(Run);

      FRawBuffer := nil;
      ColorManager.SourceOptions := [coNeedByteSwap];
      try
        // read IHDR chunk
        ReadDataAndCheckCRC(Run);
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapCardinalArrayEndian(PCardinal(@Description), 2);

        // currently only one compression type is supported by PNG (LZ77)
        if Compression = ctLZ77 then
        begin
          Decoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
          Decoder.DecodeInit;
        end
        else
          GraphicExError(gesUnsupportedFeature, [gesCompressionScheme, 'PNG']);

        // setup is done, now go for the chunks
        repeat
          FCurrentCRC := LoadAndSwapHeader(Run);
          if IsChunk(IDAT) then
          begin
            Progress(Self, psEnding, 0, False, FProgressRect, '');
            LoadIDAT(Run, Description);
            // After reading the image data the next chunk header has already been loaded
            // so continue with code below instead trying to load a new chunk header.
          end
          else if IsChunk(PLTE) then
          begin
            // palette chunk
            if (FHeader.Length mod 3) <> 0 then
              GraphicExError(gesInvalidPalette, ['PNG']);
            ReadDataAndCheckCRC(Run);
            // load palette only if the image is indexed colors and we
            // haven't loaded a palette yet. Duplicate palettes isn't
            // allowed but broken images might still contain one.
            // Not checking this might cause a memory leak.
            if (Description.ColorType = 3) and not Assigned(PaletteBuf) then
            begin
              // first setup pixel format before actually creating a palette
              FSourceBPP := SetupColorDepth(Description.ColorType, Description.BitDepth);
              {$IFDEF LCMS}
              // if this PNG contains an ICC profile we will have to convert the palette entries:
              if (FICCManager <> nil) and FICCTransformEnabled then begin
                FICCManager.CreateTransformPalette(False, False); // Interlaced thus not Planar; No Alpha.
                FICCManager.ExecuteTransform(FRawBuffer, FHeader.Length div 3);
                FICCManager.DestroyTransform();
              end;
              {$ENDIF}
              FPalette := ColorManager.CreateColorPalette([FRawBuffer], pfInterlaced8Triple, FHeader.Length div 3);
              // We need to copy palette from FRawBuffer because FRawBuffer
              // will be reused...
              // Always needed for fpc but also in Delphi for Indexed with Alpha.
              GetMem(PaletteBuf, FHeader.Length);
              Move(FRawBuffer^, PaletteBuf^, FHeader.Length);
              ColorManager.SetSourcePalette([PaletteBuf], pfInterlaced8Triple);
            end;
            Continue;
          end
          else if IsChunk(gAMA) then
          begin
            ReadDataAndCheckCRC(Run);
            // The PNG specs say that Gamma should only be handled if there is no ICC profile
            {$IFDEF LCMS}if FICCManager = nil then begin{$ENDIF}
              // The file gamma given here is a scaled cardinal (e.g. 0.45 is expressed as 45000).
              ColorManager.SetGamma(SwapEndian(PCardinal(FRawBuffer)^) / 100000);
              ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
            {$IFDEF LCMS}end;{$ENDIF}
            Include(Options, ioUseGamma);
            Continue;
          end
          else if IsChunk(bKGD) then
          begin
            LoadBackgroundColor(Run, Description);
            Continue;
          end
          else if IsChunk(tRNS) then
          begin
            LoadTransparency(Run, Description);
            Continue;
          end
          else if IsChunk(iCCP) then
          begin
            // Gets read in ReadImageProperties. No need to read it twice!
          end;

          // Skip unknown or unsupported chunks (+4 because of always present CRC).
          // IEND will be skipped as well, but this chunk is empty, so the stream will correctly
          // end on the first byte after the IEND chunk.
          Inc(Run, FHeader.Length + 4);
          if IsChunk(IEND) then
            Break;

          // Length = 0 should not happen but I have seen a broken png that has
          // no IEND chunk but does have length = 0
          // Also make sure a broken png doesn't set Run to illegal offset
          if (FHeader.Length = 0) or (NativeUInt(Run) >= NativeUInt(PAnsiChar(Memory)+Size)) then
            Break;

          // Note: According to the specs an unknown, but as critical marked chunk is a fatal error.
          if (Byte(FHeader.ChunkType[0]) and CHUNKMASK) = 0 then
            GraphicExError(gesUnknownCriticalChunk);
        until False;
      finally
        if Assigned(Decoder) then
          Decoder.DecodeEnd;
        if Assigned(FRawBuffer) then
          FreeMem(FRawBuffer);
        if Assigned(PaletteBuf) then
          FreeMem(PaletteBuf);
        if Assigned(FTransparency) then begin
          FreeMem(FTransparency);
          FTransparency := nil;
        end;
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      end;
    end;
  end
  else
    GraphicExError(gesInvalidImage, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Magic: array[0..7] of AnsiChar;
  Description: TIHDRChunk;
  Run: PByte;

begin
  FEOF := PAnsiChar(Memory) + Size;
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;
      Move(Run^, Magic, 8);
      Inc(Run, 8);

      if StrLComp(Magic, PNGMagic, Length(Magic)) = 0 then
      begin
        // first chunk must be an IHDR chunk
        FCurrentCRC := LoadAndSwapHeader(Run);
        if IsChunk(IHDR) then
        begin
          Include(Options, ioBigEndian);
          // Since ReadDataAndCheckCRC is going to allocate FRawBuffer we
          // need to add a try finally before it in case we get an exception
          // which would otherwise cause a memory leak. Note that the crash
          // could already occur inside ReadDataAndCheckCRC so we have to put
          // the try before that (in case of a failed CRC check)
          try
            // read IHDR chunk
            ReadDataAndCheckCRC(Run);
            Move(FRawBuffer^, Description, SizeOf(Description));
            SwapCardinalArrayEndian(PCardinal(@Description), 2);

            if (Description.Width = 0) or (Description.Height = 0) then
              Exit;

            Width := Description.Width;
            Height := Description.Height;

            if Description.Compression = 0 then
              Compression := ctLZ77
            else
              Compression := ctUnknown;

            BitsPerSample := Description.BitDepth;
            SamplesPerPixel := 1;
            case Description.ColorType of
              0:
                ColorScheme := csG;
              2:
                begin
                  ColorScheme := csRGB;
                  SamplesPerPixel := 3;
                end;
              3:
                ColorScheme := csIndexed;
              4:
                begin
                  ColorScheme := csGA;
                  SamplesPerPixel := 2;
                end;
              6:
                begin
                  ColorScheme := csRGBA;
                  SamplesPerPixel := 4;
                end;
            else
              ColorScheme := csUnknown;
            end;

            BitsPerPixel := SamplesPerPixel * BitsPerSample;
            FilterMode := Description.Filter;
            Interlaced := Description.Interlaced <> 0;
            HasAlpha := ColorScheme in [csGA, csRGBA, csBGRA];

            // Find gamma and comment.
            repeat
              FCurrentCRC := LoadAndSwapHeader(Run);
              if IsChunk(gAMA) then
              begin
                ReadDataAndCheckCRC(Run);
                // The file gamma given here is a scaled cardinal (e.g. 0.45 is expressed as 45000).
                FileGamma := SwapEndian(PCardinal(FRawBuffer)^) / 100000;
                Include(Options, ioUseGamma);
                Continue;
              end
              else if IsChunk(tEXt) then
              begin
                LoadText(Run);
                Continue;
              end
              else if IsChunk(iCCP) then
              begin
                LoadICCProfile(Run);
                Continue;
              end
              else if IsChunk(tRNS) then
              begin
                // Transparency chunk present.
                // Checking presence of this chunk is the only way to detect Indexed with Alpha.
                // It's a sort of alpha palette in that case so the Samples per Pixel does not change!
                if TIHDRChunk(Description).ColorType = 3 then begin
                  ColorScheme := csIndexedA;
                end;
                // Don't use continue since we did not read the contents of the chunk!
              end;

              Inc(Run, FHeader.Length + 4);
              if IsChunk(IEND) then
                Break;
              // Length = 0 should not happen but I have seen a broken png that has
              // no IEND chunk but does have length = 0
              // Also make sure a broken png doesn't set Run to illegal offset
              if (FHeader.Length = 0) or (NativeUInt(Run) >= NativeUInt(PAnsiChar(Memory)+Size)) then
                Break;
            until False;
          finally
            if Assigned(FRawBuffer) then
              Freemem(FRawBuffer);
          end;
          Result := True;
        end;
      end
      else
        Result := False;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.DecompressToBuffer(Source: PByte; CompressedSize: Cardinal;
  out DecompressBuf: PByte; out DecompressedSize: Cardinal);
const MAXBUF = 65536; // Size of temporary buffer used for decompressing
var
  ICCDecoder: TLZ77Decoder;
  LocalBuffer, LocalDecompressBuf, ResultBuffer: PByte;
  finished: Boolean;
  RemainingSize: Cardinal;
  ResultSize, ResultMaxSize, SizeIncrease: Cardinal;
  ResultPos: PByte;

  procedure AddResult(ABuffer: PByte; ASize: Cardinal);
  begin
    if ResultSize+ASize > ResultMaxSize then begin
      // Increase with the size of the Compressed data since we don't want to realloc very often.
      Inc(ResultMaxSize, SizeIncrease);
      ReallocMem(ResultBuffer, ResultMaxSize);
      // Update the current position.
      ResultPos := ResultBuffer;
      Inc(ResultPos, ResultSize);
    end;
    // Add the decompressed bytes.
    Move(ABuffer^, ResultPos^, ASize);
    Inc(ResultPos, ASize);
    Inc(ResultSize, ASize);
    // Update decompressed data size
    Inc(DecompressedSize, ASize);
  end;
begin
  DecompressBuf := nil;
  DecompressedSize := 0;
  // Since Decoding the ICC works a little different from decoding image data
  // we use a separate decoder instead of reusing the already created one.
  ICCDecoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
  // Allocate memory for the decode buffer.
  GetMem(LocalBuffer, MAXBUF);
  // Start with an output buffer size of twice the compressed size. Should be enough in most cases.
  SizeIncrease := CompressedSize;
  ResultMaxSize := 2 * SizeIncrease;
  if ResultMaxSize < MAXBUF then
    ResultMaxSize := MAXBUF;
  GetMem(ResultBuffer, ResultMaxSize);
  try
    finished := False;
    RemainingSize := CompressedSize;
    ResultSize := 0;
    ResultPos := ResultBuffer;
    // Initialize decoder and decode ICC
    ICCDecoder.DecodeInit;
    repeat
      // Since Decode moves the Buffer pointers we need an extra variable.
      LocalDecompressBuf := LocalBuffer;
      ICCDecoder.Decode(Pointer(Source), Pointer(LocalDecompressBuf), RemainingSize, MAXBUF);
      AddResult(LocalBuffer, MAXBUF-ICCDecoder.AvailableOutput);
      // Update remaining compressed size
      RemainingSize := ICCDecoder.AvailableInput;
      if ICCDecoder.ZLibResult = Z_STREAM_END then
        break
      else if ICCDecoder.ZLibResult <> Z_OK then
        break;
    until RemainingSize = 0;

    // Check if decoding was ok.
    // Note we currently don't check for correct end code because apparently some
    // (very) old Photoshop produced PNG's may have an invalid end of compression (missing Adler32 checksum)
    // which will cause return code -5 (Z_BUF_ERROR) instead of Z_STREAM_END.
    // Therefor we only check if all input is handled and in that case assume everything is ok.
    // Example see: https://pmt.sourceforge.io/iccp/
    if RemainingSize > 0 then begin
      // TODO: Maybe change this to just adding a warning message
      // since we can still show the image without having an ICC.
      //GraphicExError(gesDecompression, ['PNG']);
      DecompressedSize := 0;
    end
    else begin
      DecompressBuf := ResultBuffer;
      finished := True;
    end;
  finally
    FreeMem(LocalBuffer);
    if not finished then begin
      // Error decompressing. Free the result
      FreeMem(ResultBuffer);
    end;
    // Free the memory we used for decoder and buffer.
    ICCDecoder.DecodeEnd;
    ICCDecoder.Free;
  end;
end;

procedure TPNGGraphic.LoadICCProfile(var Source: PByte);
var
  ProfileName: PAnsiChar;
  ProfileLength: Cardinal;
  Compression: Byte;
  CompressedBytes, DecompressedSize: Cardinal;
  LocalBuffer: PByte;
begin
  ProfileName := PAnsiChar(Source);
  ProfileLength := Length(ProfileName)+1; // +1 to include the null byte
  Inc(Source, ProfileLength); // Skip ProfileName including terminating 0
  Compression := Source^;
  // TODO: Only valid Compression type is 0. But we should only warn here not stop.
  //if Compression <> 0 then
  //  GraphicExError(gesDecompression, ['PNG']);
  Inc(Source); // Skip Compression type
  CompressedBytes := FHeader.Length - ProfileLength - 1;
  {$IFDEF LCMS}
  try
    DecompressToBuffer(Source, CompressedBytes, LocalBuffer, DecompressedSize);
    // Check if anything got decompressed.
    // Since ICC is not essential we will continue even in case of error decompressing ICC.
    if (DecompressedSize > 0) and (LocalBuffer <> nil) then begin
      if FICCManager = nil then
        FICCManager := TICCProfileManager.Create;
      // Load the memory ICC profile in our ICCManager.
      FICCManager.LoadSourceProfileFromMemory(LocalBuffer, DecompressedSize);
    end;
  finally
    // We are responsible for freeing the buffer
    if LocalBuffer <> nil then
      FreeMem(LocalBuffer);
  end;
  {$ELSE}
  {$ENDIF}
  Inc(Source, CompressedBytes);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadBackgroundColor(var Source: PByte; const Description);

// loads the data from the current chunk (must be a bKGD chunk) and fills the bitmpap with that color

var
  Run: PWord;
  R, G, B: Byte;

begin
  ReadDataAndCheckCRC(Source);
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0, 4: // G(A)
        begin
          case BitDepth of
            2:
              FBackgroundColor := MulDiv16(SwapEndian(PWord(FRawBuffer)^), 15, 3);
            16:
              FBackgroundColor := MulDiv16(SwapEndian(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            FBackgroundColor := Byte(SwapEndian(PWord(FRawBuffer)^));
          end;
        end;
      2, 6:  // RGB(A)
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(SwapEndian(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(SwapEndian(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(SwapEndian(Run^), 255, 65535);
          end
          else
          begin
            R := Byte(SwapEndian(Run^)); Inc(Run);
            G := Byte(SwapEndian(Run^)); Inc(Run);
            B := Byte(SwapEndian(Run^));
          end;
          FBackgroundColor := RGB(R, G, B);
        end;
    else // indexed color scheme (3)
      FBackgroundColor := PByte(FRawBuffer)^;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadIDAT(var Source: PByte; const Description);

// loads image data from the current position of the stream

const
  // interlace start and offsets
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask: array[0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

var
  Row: Integer;
  TargetBPP: Integer;
  RowBuffer: array[Boolean] of PAnsiChar; // I use PAnsiChar here instead of simple pointer to ease pointer math below
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  BytesPerRow,
  InterlaceRowBytes,
  InterlaceWidth: Integer;

begin
  Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
  RowBuffer[False] := nil;
  RowBuffer[True] := nil;
  try
    // adjust pixel format etc. if not yet done
    if PixelFormat = pfDevice then
      FSourceBPP := SetupColorDepth(TIHDRChunk(Description).ColorType, TIHDRChunk(Description).BitDepth);

    if TIHDRChunk(Description).BitDepth = 16 then
      TargetBPP := FSourceBPP div 2
    else
      TargetBPP := FSourceBPP;

    // after setting the pixel format we can set the dimensions too without
    // initiating color conversions
    Width := TIHDRChunk(Description).Width;
    Height := TIHDRChunk(Description).Height;

    {$IFNDEF FPC}
    // For pf1Bit Indexed images with a color palette it is necessary to set
    // PixelFormat AFTER setting Width and Height or else we will get a
    // black/white color palette, see also
    // - http://www.efg2.com/Lab/ImageProcessing/pf1bit.htm
    // - http://www.efg2.com/Lab/ImageProcessing/Scanline.htm#pf1bit
    // Even though the showing black part was fixed in Delphi 6, it still appears
    // to be necessary here to set PixelFormat after w/h to use a color palette.
    if (FImageProperties.ColorScheme = csIndexed) and (FImageProperties.BitsPerPixel = 1) then
      PixelFormat := pf1Bit;
    {$ENDIF}

    // Needs to be after setting PixelFormat or we will get a b/w palette in the above case
    if FPalette <> 0 then
      Palette := FPalette;

    // set background and transparency color, these values must be set after the
    // bitmap is actually valid (although, not filled)
    Canvas.Lock;
    try
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(Rect(0, 0, Width, Height));
    finally
      Canvas.Unlock;
    end;
    if FTransparentColor <> clNone then
    begin
      TransparentColor := FTransparentColor;
      Transparent := True;
    end;

    // determine maximum number of bytes per row and consider there's one filter byte at the start of each row
    BytesPerRow := TargetBPP * ((Width * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

    RowBuffer[True] := AllocMem(BytesPerRow);
    RowBuffer[False] := AllocMem(BytesPerRow);

    // there can be more than one IDAT chunk in the file but then they must directly
    // follow each other (handled in ReadRow)
    EvenRow := True;

    {$IFDEF FPC}
    BeginUpdate;
    {$ENDIF}
    // prepare interlaced images
    if TIHDRChunk(Description).Interlaced = 1 then
    begin
      // TODO: Add ICC color profile handling
      for Pass := 0 to 6 do
      begin
        // prepare next interlace run
        if Width <= ColumnStart[Pass] then
          Continue;
        InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 - ColumnStart[Pass]) div ColumnIncrement[Pass];
        InterlaceRowBytes := TargetBPP * ((InterlaceWidth * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

        Row := RowStart[Pass];
        while Row < Height do
        begin
          ReadRow(Source, RowBuffer[EvenRow], InterlaceRowBytes);
          ApplyFilter(Byte(RowBuffer[EvenRow]^),
                      Pointer(RowBuffer[EvenRow] + 1),
                      Pointer(RowBuffer[not EvenRow] + 1),
                      Pointer(RowBuffer[EvenRow] + 1),
                      FSourceBPP,
                      InterlaceRowBytes - 1);

          ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, PassMask[Pass]);
          EvenRow := not EvenRow;
          // continue with next row in interlaced order
          Inc(Row, RowIncrement[Pass]);

          if Pass = 6 then
          begin
            // progress event only for last (and most expensive) pass
            Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
            OffsetRect(FProgressRect, 0, 1);
          end;
        end;
      end;
    end
    else
    begin
      {$IFDEF LCMS}
      if FICCManager = nil then
        // This will allow us to call Transform that directly returns (does nothing)
        // that way we will not have to check for nil inside the loop.
        FICCManager := TICCProfileManager.Create
      else if FICCTransformEnabled then begin
        if TargetBPP >= 3 then
          FICCManager.CreateTransformTosRGB(TargetBPP >= 4)
        else if (TargetBPP = 1) and (FImageProperties.ColorScheme = csG) then
          // TODO: In case of Indexed we shouldn't do anything here but transform the palette!!!
          FICCManager.CreateTransformTosRGB_Gray8();
      end;
      // else dummy Transform will be called.
      {$ENDIF}
      for Row := 0 to Height - 1 do
      begin
        ReadRow(Source, RowBuffer[EvenRow], BytesPerRow);
        ApplyFilter(Byte(RowBuffer[EvenRow]^),
                    Pointer(RowBuffer[EvenRow] + 1),
                    Pointer(RowBuffer[not EvenRow] + 1),
                    Pointer(RowBuffer[EvenRow] + 1),
                    FSourceBPP,
                    BytesPerRow - 1);

        ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, $FF);
        {$IFDEF LCMS}
        FICCManager.ExecuteTransform(ScanLine[Row], Width);
        {$ENDIF}
        EvenRow := not EvenRow;

        Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
        OffsetRect(FProgressRect, 0, 1);
      end;
      {$IFDEF LCMS}
      FICCManager.DestroyTransform();
      {$ENDIF}
    end;
    {$IFDEF FPC}
    EndUpdate;
    {$ENDIF}

    // in order to improve safe failness we read all remaining but not read IDAT chunks here
    while IsChunk(IDAT) do
    begin
      ReadDataAndCheckCRC(Source);;
      FCurrentCRC := LoadAndSwapHeader(Source);
    end;
  finally
    if Assigned(RowBuffer[True]) then
      FreeMem(RowBuffer[True]);
    if Assigned(RowBuffer[False]) then
      FreeMem(RowBuffer[False]);
  end;
  // ending progress event is issued in main method
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadText(var Source: PByte);

var
  Keyword: AnsiString;
  Offset: Cardinal;
  Contents: array of AnsiChar;

begin
  ReadDataAndCheckCRC(Source);
  with FImageProperties do
  begin
    Keyword := PAnsiChar(FRawBuffer); // Keyword is zero terminated in file
    if (Keyword = 'Comment') or (Keyword = 'Description') or (Keyword = 'Title') then
    begin
      // Only text chunks with the 'Comment', 'Description' and 'Title' keywords are loaded
      Offset := Length(Keyword) + 1;
      SetLength(Contents, FHeader.Length - Offset + 1);
      StrLCopy(PAnsiChar(Contents), PAnsiChar(FRawBuffer) + Offset, FHeader.Length - Offset);
      if Comment = '' then
        Comment := PAnsiChar(Contents)
      else // Add NewLine character between multiple comments
        Comment := Comment + #10 + PAnsiChar(Contents);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.LoadTransparency(var Source: PByte; const Description);

// reads the data of the current transparency chunk

var
  Run: PWord;
  R, G, B: Byte;

begin
  ReadDataAndCheckCRC(Source);
  with TIHDRChunk(Description) do
  begin
    case ColorType of
      0: // gray
        begin
          case BitDepth of
            2:
              R := MulDiv16(SwapEndian(PWord(FRawBuffer)^), 15, 3);
            16:
              R := MulDiv16(SwapEndian(PWord(FRawBuffer)^), 255, 65535);
          else // 1, 4, 8 bits gray scale
            R := Byte(SwapEndian(PWord(FRawBuffer)^));
          end;
          FTransparentColor := RGB(R, R, R);
        end;
      2:  // RGB
        begin
          Run := FRawBuffer;
          if BitDepth = 16 then
          begin
            R := MulDiv16(SwapEndian(Run^), 255, 65535); Inc(Run);
            G := MulDiv16(SwapEndian(Run^), 255, 65535); Inc(Run);
            B := MulDiv16(SwapEndian(Run^), 255, 65535);
          end
          else
          begin
            R := Byte(SwapEndian(Run^)); Inc(Run);
            G := Byte(SwapEndian(Run^)); Inc(Run);
            B := Byte(SwapEndian(Run^));
          end;
          FTransparentColor := RGB(R, G, B);
        end;
      4, 6:
        // Formats with full alpha channel, they shouldn't have a transparent color.
        ;
    else
      // Indexed color scheme (3), with at most 256 alpha values (for each palette entry).
      GetMem(FTransparency, 256);
      // read the values (at most 256)...
      Move(FRawBuffer^,  FTransparency^, Min(FHeader.Length, 256));
      // ...and set default values (255, fully opaque) for non-supplied values
      if FHeader.Length < 256 then
        FillChar(FTransparency^[FHeader.Length], 256 - FHeader.Length, $FF);
      // Since we now know that we have an Indexed Scheme with alpha we will
      // have to change some settings
      // For both Delphi and Fpc we will have to use BGRA as target
      FImageProperties.ColorScheme := csIndexedA;
      ColorManager.SourceColorScheme := csIndexedA;
      ColorManager.TargetColorScheme := csBGRA;
      ColorManager.TargetSamplesPerPixel := 4;
      ColorManager.TargetBitsPerSample := 8; // Needed for Delphi, in Fpc we already have it set to 8 here.
      PixelFormat := pf32Bit;
      // Set Alpha Palette in ColorManager
      ColorManager.SetSourceAlphaPalette(FTransparency);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ValidateMemoryPosition(const CurPos: Pointer; const AOffset: Cardinal);
begin
  // Security check: make sure the target memory position doesn't point beyond EOF
  // FEOF here means the memory position of the first byte after the end of file
  // Always convert to UInt64 first since the result of the addition can be
  // larger than High(Cardinal)
  if UInt64(CurPos) + AOffset >= UInt64(FEOF) then
    GraphicExError(gesStreamReadError, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadDataAndCheckCRC(var Source: PByte);

// Allocates memory in FRawBuffer and reads the next Header.Length bytes from Stream.
// Furthermore, the CRC value following the data is read as well and compared with
// the CRC value which is calculated here.
// 2012-04-12 jgb In SoftGems forums there was a thread saying that the below ReallocMem is causing a memory leak

var
  FileCRC: Cardinal;

begin
  ValidateMemoryPosition(Source, FHeader.Length);
  ReallocMem(FRawBuffer, FHeader.Length);
  Move(Source^, FRawBuffer^, FHeader.Length);
  Inc(Source, FHeader.Length);

  Move(Source^, FileCRC, SizeOf(FileCRC));
  Inc(Source, SizeOf(FileCRC));
  FileCRC := SwapEndian(FileCRC);
  // The type field of a chunk is included in the CRC, this serves as initial value
  // for the calculation here and is determined in LoadAndSwapHeader.
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  if FCurrentCRC <> FileCRC then
    GraphicExError(gesInvalidCRC, ['PNG']);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGGraphic.ReadRow(var Source: PByte; RowBuffer: Pointer; BytesPerRow: Integer);

// reads and decodes one scanline

var
  LocalBuffer: Pointer;
  PendingOutput: Integer;

begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  repeat
    // read pending chunk data if available input has dropped to zero
    if TLZ77Decoder(Decoder).AvailableInput = 0 then
    begin
      FIDATSize := 0;
      // read all following chunks until enough data is available or there is no further IDAT chunk
      while FIDATSize = 0 do
      begin
        // finish if the current chunk is not an IDAT chunk
        if not IsChunk(IDAT) then
          Exit;

        ReadDataAndCheckCRC(Source);
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;

        // prepare next chunk (plus CRC)
        FCurrentCRC := LoadAndSwapHeader(Source);
      end;
    end;

    // this decode call will advance Source and Target accordingly
    Decoder.Decode(FCurrentSource, LocalBuffer, FIDATSize - (NativeInt(FCurrentSource) - NativeInt(FRawBuffer)),
      PendingOutput);

    if TLZ77Decoder(Decoder).ZLibResult = Z_STREAM_END then
    begin
       if (TLZ77Decoder(Decoder).AvailableOutput <> 0) or (TLZ77Decoder(Decoder).AvailableInput <> 0) then
         GraphicExError(gesExtraCompressedData, ['PNG']);
      Break;
    end;

    if TLZ77Decoder(Decoder).ZLibResult <> Z_OK then
      GraphicExError(gesDecompression, ['PNG']);

    PendingOutput := BytesPerRow - (NativeInt(LocalBuffer) - NativeInt(RowBuffer));
  until PendingOutput = 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGGraphic.SetupColorDepth(ColorType, BitDepth: Integer): Integer;

begin
  Result := 0;
  // determine color scheme and setup related stuff,
  // Note: The calculated BPP value is always at least 1 even for 1 bits per pixel etc. formats
  //       and used in filter calculation.
  case ColorType of
    0: // gray scale (allowed bit depths are: 1, 2, 4, 8, 16 bits)
      if BitDepth in [1, 2, 4, 8, 16] then
      with ColorManager do
      begin
        SourceColorScheme := csG;
        SourceSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        {$IFNDEF FPC}
        TargetColorScheme := csG;
        TargetSamplesPerPixel := 1;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        case BitDepth of
          2:
            TargetBitsPerSample := 4;
          16:
            TargetBitsPerSample := 8;
        else
          TargetBitsPerSample := BitDepth;
        end;
        {$ELSE}
        TargetColorScheme := csBGR;
        TargetSamplesPerPixel := 3;
        TargetBitsPerSample := 8;
        {$ENDIF}

        PixelFormat := TargetPixelFormat;
        FPalette := CreateGrayscalePalette(False);
        Result := (BitDepth + 7) div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    2: // RGB
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 3;
        TargetSamplesPerPixel := 3;
        SourceColorScheme := csRGB;
        TargetColorScheme := csBGR;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf24Bit;
        Result := BitDepth * 3 div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    3: // palette
      if BitDepth in [1, 2, 4, 8] then
      with ColorManager do
      begin
        SourceColorScheme := csIndexed;
        SourceSamplesPerPixel := 1;
        SourceBitsPerSample := BitDepth;
        {$IFNDEF FPC}
        TargetColorScheme := csIndexed;
        TargetSamplesPerPixel := 1;
        // 2 bits values are converted to 4 bits values because DIBs don't know the former variant
        if BitDepth = 2 then
          TargetBitsPerSample := 4
        else
          TargetBitsPerSample := BitDepth;
        {$ELSE}
        // Convert to BGR since fpc has trouble handling the other bitdepths
        // and indexed mode png might specify a transparency channel in which
        // case we will later change it to BGRA with 4 spp
        TargetColorScheme := csBGR;
        TargetSamplesPerPixel := 3;
        TargetBitsPerSample := 8;
        {$ENDIF}

        {$IFNDEF FPC}
        // See comment in LoadIDAT for the reason why this is necessary in Delphi
        if BitDepth <> 1 then
        {$ENDIF}
          PixelFormat := TargetPixelFormat;
        Result := 1;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    4: // gray scale with alpha, handled by converting to RGBA
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 2;
        SourceBitsPerSample := BitDepth;
        SourceColorScheme := csGA;
        TargetSamplesPerPixel := 4;
        TargetBitsPerSample := 8;
        TargetColorScheme := csBGRA;
        PixelFormat := TargetPixelFormat;
        FPalette := CreateGrayScalePalette(False);
        Result := 2 * BitDepth div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
    6: // RGB with alpha (8, 16)
      if BitDepth in [8, 16] then
      with ColorManager do
      begin
        SourceSamplesPerPixel := 4;
        TargetSamplesPerPixel := 4;
        SourceColorScheme := csRGBA;
        TargetColorScheme := csBGRA;
        SourceBitsPerSample := BitDepth;
        TargetBitsPerSample := 8;
        PixelFormat := pf32Bit;

        Result := BitDepth * 4 div 8;
      end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);
  else
    GraphicExError(gesInvalidColorFormat, ['PNG']);
  end;
end;

{$endif PortableNetworkGraphic}

{$ifdef ArtsAndLettersGraphic}

//----------------- TGEDGraphic ----------------------------------------------------------------------------------------

const
  GEDMagic = 'A&L-' + #0 + 'ARTS & LETTERS';
  GEDEditorVersion40c = 133;
  GEDVersionHeader = $1F;
  GEDDibThumbnail = $51;
  GEDFileDescription = $5F;

class function TGEDGraphic.CanLoad(const Memory: Pointer; Size: Int64): Boolean;
var
  Run: PByte;
begin
  Result := (Size > Length(GEDMagic)) and (StrLIComp(PAnsiChar(Memory), PAnsiChar(GEDMagic), Length(GEDMagic)) = 0);
  if Result then
  begin
    Run := Memory;
    // Skip Arts & Letters ID string.
    Inc(Run, Length(GEDMagic));
    // Seek to the start of the tags and check the version number.
    Inc(Run, GEDVersionHeader);

    Result := Run^ >= GEDEditorVersion40c;
    if Result then
    begin
      Inc(Run, 2);
      // The file description is always first.
      Result := Run^ = GEDFileDescription;
      if Result then
      begin
        // Skip the description tag and 1 extra word
        Inc(Run, 4);

        // Here we should now find a thumbnail tag.
        Result := Run^ = GEDDibThumbnail;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGEDGraphic.LoadFromMemory(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal = 0);

var
  WholeThumbnail: Integer;
  BI: PBitmapInfo;
  TableSize: Integer;
  Bits: Pointer;
  Tag: Word;
  Bytes: Byte;
  Run: PByte;

begin
  inherited;

  if ReadImageProperties(Memory, Size, ImageIndex) then
  begin
    Run := Memory;

    // Skip Arts & Letters ID string.
    Inc(Run, Length(GEDMagic));
    // Seek to the start of the tags.
    Inc(Run, GEDVersionHeader);

    // Get the version number.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    // The file description is always first.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    // Skip the description tag.
    Bytes := Run^;
    Inc(Run, Bytes + 1);

    // Here we should now find a thumbnail tag.
    Move(Run^, Tag, SizeOf(Tag));
    Inc(Run, SizeOf(Tag));

    Move(Run^, WholeThumbnail, SizeOf(WholeThumbnail));
    Inc(Run, SizeOf(WholeThumbnail));

    // Read basic data of the image.
    BI := Pointer(Run);
    // Allocate bitmap now...
    Width := BI.bmiHeader.biWidth;
    Height := BI.bmiHeader.biHeight;

    // Calculate palette size. The image data directly follows the bitmap info.
    TableSize := (1 shl BI.bmiHeader.biBitCount) * SizeOf(TRGBQuad);
    Bits := PAnsiChar(BI) + SizeOf(TBitmapInfoHeader) + TableSize;
    // ... and place them into our bitmap.
    SetDIBitsToDevice(Canvas.Handle, 0, 0, BI.bmiHeader.biWidth, BI.bmiHeader.biHeight, 0, 0, 0,
      BI.bmiHeader.biHeight, Bits, BI^, DIB_RGB_COLORS);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGEDGraphic.ReadImageProperties(const Memory: Pointer; Size: Int64; ImageIndex: Cardinal): Boolean;

var
  Tag: Word;
  ThumbSize: Cardinal;
  Bytes: Byte;
  BI: PBitmapInfoHeader;
  Run: PByte;

begin
  Result := inherited ReadImageProperties(Memory, Size, ImageIndex);

  if Result then
    with FImageProperties do
    begin
      Run := Memory;

      // Skip Arts & Letters ID string.
      Inc(Run, Length(GEDMagic));
      // Seek to the start of the tags.
      Inc(Run, GEDVersionHeader);

      // Get the version number.
      Move(Run^, Tag, SizeOf(Tag));
      Inc(Run, SizeOf(Tag));
      if Tag >= GEDEditorVersion40c then
      begin
        // The file description is always first.
        Move(Run^, Tag, SizeOf(Tag));
        Inc(Run, SizeOf(Tag));
        if Tag = GEDFileDescription then
        begin
          // Skip the description tag.
          Bytes := Run^;
          Inc(Run, Bytes + 1);

          // Here we should now find a thumbnail tag.
          Move(Run^, Tag, SizeOf(Tag));
          Inc(Run, SizeOf(Tag));
          if Tag = GEDDibThumbnail then
          begin
            // skip thumbnail size
            Move(Run^, ThumbSize, SizeOf(ThumbSize));
            Inc(Run, SizeOf(ThumbSize));

            BI := Pointer(Run);

            Options := [];
            Width := BI.biWidth;
            Height := BI.biHeight;
            BitsPerPixel := BI.biBitCount;
            if BitsPerPixel > 8 then
            begin
              BitsPerSample := BitsPerPixel div 8;
              SamplesPerPixel := BitsPerPixel mod 8;
              if SamplesPerPixel = 3 then
                ColorScheme := csBGR
              else
                ColorScheme := csBGRA;
            end
            else
            begin
              BitsPerSample := BitsPerPixel;
              SamplesPerPixel := 1;
              ColorScheme := csIndexed;
            end;

            if BI.biCompression in [BI_RLE8, BI_RLE4] then
              Compression := ctRLE
            else
              Compression := ctNone;

            Result := True;
          end;
        end;
      end
      else
        Result := False;
    end;
end;

{$endif ArtsAndLettersGraphic}

//----------------- TFileFormatList ------------------------------------------------------------------------------------

type
  PClassEntry = ^TClassEntry;
  TClassEntry = record
    GraphicClass: TGraphicClass;
    Description: string;
    Count: Cardinal;
  end;

  PExtensionEntry = ^TExtensionEntry;
  TExtensionEntry = record
    Extension,
    Description: string;
    FormatTypes: TFormatTypes;
    ClassReference: PClassEntry;
  end;

constructor TFileFormatList.Create;

begin
  FClassList := TList.Create;
  FExtensionList := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TFileFormatList.Destroy;

begin
  Clear;
  FClassList.Free;
  FExtensionList.Free;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.Clear;

var
  I: Integer;

begin
  for I := 0 to FClassList.Count - 1 do
  begin
    TPicture.UnregisterGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
    Dispose(PClassEntry(FClassList[I])); // need Dispose with type casting to free strings too
  end;
  FClassList.Clear;

  for I := 0 to FExtensionList.Count - 1 do
    Dispose(PExtensionEntry(FExtensionList[I])); 
  FExtensionList.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindExtension(const Extension: string): Integer;

// Returns the entry which belongs to the given extension string or -1 if there's nothing in the list for this ext.

var
  I: Integer;

begin
  Result := -1;
  if Extension <> '' then
    for I := 0 to FExtensionList.Count - 1 do
      if CompareText(PExtensionEntry(FExtensionList[I]).Extension, Extension) = 0 then
      begin
        Result := I;
        Break;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.FindGraphicClass(GraphicClass: TGraphicClass): Integer;

// returns the entry index which belongs to the given graphic class or -1

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FClassList.Count - 1 do
    if PClassEntry(FClassList[I]).GraphicClass = GraphicClass then
    begin
      Result := I;
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetDescription(Graphic: TGraphicClass): string;

// returns the registered description string for the given class

var
  I: Integer;

begin
  Result := '';
  I := FindGraphicClass(Graphic);
  if I > -1 then
    Result := PClassEntry(FClassList[I]).Description;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.GetExtensionList(List: TStrings);

// returns a list of registered extensions (letters only, no *. part)

var
  I: Integer;
  ExtEntry: PExtensionEntry;

begin
  List.Clear;
  for I := 0 to FExtensionList.Count - 1 do
  begin
    ExtEntry := FExtensionList[I];
    List.Add(ExtEntry.Extension);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GetGraphicFilter(Formats: TFormatTypes; SortType: TFilterSortType;
  Options: TFilterOptions; GraphicClass: TGraphicClass): string;

// Creates a string which can directly be used in an open or save dialog's filter property.
// Formats may be used to limit the number of formats to return.
// SortType determines how to sort the entries.
// Compact determines whether to group extensions (= True) or to put every extension on a separate line.
// AllImages finally determines whether to include the 'All image file' entry which includes all allowed extensions
// which qualify by the other properties.
// Usually all these options determine quite nicely which formats are well suited for a particular task
// but sometimes you may find it better to specify a graphic class to limit returned formats further.
// In this case set GraphicClass to the particular class otherwise set it nil.

var
  I, J: Integer;
  DL, EL, All: TStringList;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;
  S,
  DescriptionFormat: string;

begin
  Result := '';
  if Formats = [] then
    Formats := [ftAnimation..ftVector];
  DL := TStringList.Create;
  DL.Sorted := SortType in [fstDescription, fstBoth];
  EL := TStringList.Create;
  EL.Sorted := SortType in [fstExtension, fstBoth];

  // this string list is used to hold the (possibly sorted) list of all allowed extensions
  All := TStringList.Create;
  All.Sorted := SortType in [fstExtension, fstBoth];

  // using an adjusted format string makes the code below easier for different options
  DescriptionFormat := '%s';
  if foIncludeExtension in Options then
    DescriptionFormat := DescriptionFormat + '%s';

  if foCompact in Options then
  begin
    // all extension for a particular image class on one line
    for I := 0 to FClassList.Count - 1 do
    begin
      ClassEntry := FClassList[I];
      if (GraphicClass = nil) or (GraphicClass = ClassEntry.GraphicClass) then
      begin
        EL.Clear;
        // collect allowed extensions for the current graphic class,
        // this will automatically sort the entries if wanted
        for J := 0 to FExtensionList.Count - 1 do
        begin
          ExtEntry := FExtensionList[J];
          if (ExtEntry.ClassReference = ClassEntry) and ((ExtEntry.FormatTypes * Formats) <> []) then
            EL.Add(ExtEntry.Extension);
        end;

        // Build the extension list and a description entry.
        if foIncludeAll in Options then
          All.AddStrings(EL);
        S := '';
        for J := 0 to EL.Count - 1 do
          S := S + '*.' + EL[J] + '; ';
        // remove last semicolon and space
        SetLength(S, Length(S) - 2);
        if S <> '' then
          DL.AddObject(ClassEntry.Description, Pointer(StrNew(PChar(S))));
      end;
    end;
  end
  else
  begin
    // list each extension separately
    for I := 0 to FExtensionList.Count - 1 do
    begin
      ExtEntry := FExtensionList[I];
      if ((GraphicClass = nil) or (ExtEntry.ClassReference.GraphicClass = GraphicClass)) and
         ((ExtEntry.FormatTypes * Formats) <> []) then
      begin
        S := ExtEntry.Description;
        if S = '' then
          S := ExtEntry.ClassReference.Description;
        if DL.IndexOf(S) = -1 then  // jgb 2012-08-16 fix for possible mem leak
          DL.AddObject(S, Pointer(StrNew(PChar('*.' + ExtEntry.Extension))));
        if foIncludeAll in Options then
          All.Add(ExtEntry.Extension);
      end;
    end;
  end;

  // Build final filter string out of the collected sub strings.
  if (foIncludeAll in Options) and (All.Count > 0) then
  begin
    // First include the general entry if wanted (this entry is never taken into sort order.
    S := '';
    for J := 0 to All.Count - 1 do
      S := S + '*.' + All[J] + ';';
    SetLength(S, Length(S) - 1);
    Result := gesAllImages + '|' + S + '|';
  end;

  for I := 0 to DL.Count - 1 do
  begin
    S := PChar(DL.Objects[I]);
    StrDispose(PChar(DL.Objects[I]));
    Result := Result + Format(DescriptionFormat, [DL[I], ' (' + S + ')']) + '|' + S + '|';
  end;
  // remove last separator in string
  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 1);
  All.Free;
  EL.Free;
  DL.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromExtension(S: string): TGraphicClass;

// Returns the class which belongs to the extension given in S or nil if there's non registered.
// S may contain a regular file name (also UNC is allowed), a string returned from ExtractFileExt (with period) or just
// an extension string.

var
  Index: Integer;

begin
  Result := nil;
  if Pos('.', S) > 0 then
    S := ExtractFileExt(S);
  if S <> '' then
  begin
    Index := Pos('.', S);
    if Index > 0 then
      Delete(S, 1, Index);
    Index := FindExtension(S);
    if Index > -1 then
      Result := PExtensionEntry(FExtensionList[Index]).ClassReference.GraphicClass;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(const FileName: string): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file.

begin
  with TFileMapping.Create(FileName, fmmReadOnly) do
  try
    Result := GraphicFromContent(Memory, Size);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(const Memory: Pointer; Size: Int64): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file. 

var
  I: Integer;
  T: TGraphicExGraphicClass;

begin
  Result := nil;
  for I := 0 to FClassList.Count - 1 do
  begin
    if PClassEntry(FClassList[I]).GraphicClass.InheritsFrom(TGraphicExGraphic) then
    begin
      T := TGraphicExGraphicClass(PClassEntry(FClassList[I]).GraphicClass);
      if T.CanLoad(Memory, Size) then
      begin
        Result := T;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFileFormatList.GraphicFromContent(Stream: TStream): TGraphicExGraphicClass;

// Tries to determine the type of the image in the file. 

var
  LastPos: Int64;

begin
  LastPos := Stream.Position;

  if Stream is TCustomMemoryStream then
  begin
    // Simple case: memory streams already are in memory.
    with Stream as TCustomMemoryStream do
      Result := GraphicFromContent(Memory, Size);
  end
  else
    if (Stream is THandleStream) and (GetFileType(THandleStream(Stream).Handle) = FILE_TYPE_DISK) then
    begin
      // File streams can be mapped to access their content directly.
      with TFileMapping.Create(Stream as THandleStream) do
      try
        Result := GraphicFromContent(Memory, Size);
      finally
        Free;
      end;
    end
    else
    begin
      // Any other stream is converted into a memory stream first.
      with TMemoryStream.Create do
      try
        CopyFrom(Stream, 0);
        Position := 0;
        Result := GraphicFromContent(Memory, Size);
      finally
        Free;
      end;
    end;

  Stream.Position := LastPos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.RegisterFileFormat(const Extension, Common, Individual: string; FormatTypes: TFormatTypes;
  Replace: Boolean; GraphicClass: TGraphicClass);

// Registers the given graphic class with the passed extension string. If there's already a class registered with this
// extension then either the registration of the older entry is replaced by the new one (Replace = True) or an exception
// is raised.
// This method takes also care to register the new extension with TPicture to make the default handling work too.
// Further parameters are:
// - Extension: the new extension to be registered (not necessarily with only 3 characters, but without a period).
// - Common: a description string for all extensions registered with the same class used when several extensions are
//   listed on one filter line. Pass '' to avoid changing a previously set value if there's one.
// - Individual: a description string used when each extension is listed separately.
// - FormatTypes: classifies the given file type as being a raster or vector file, with single or multiple images etc.
// - GraphicClass: the TGraphic descentant to be used to load and save the particular file.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry,
  OldReference: PClassEntry;

  //--------------- local functions -------------------------------------------

  procedure UpdateClassEntry;

  // updates a class entry (creates one if necessary)

  begin
    if ClassIndex = -1 then
    begin
      New(ClassEntry);
      ClassEntry.GraphicClass := GraphicClass;
      ClassEntry.Count := 0;
      FClassList.Add(ClassEntry);
    end
    else
      ClassEntry := FClassList[ClassIndex];

    if Common <> '' then
      ClassEntry.Description := Common;
    Inc(ClassEntry.Count);
    ExtEntry.ClassReference := ClassEntry;
  end;

  //--------------- end local functions ---------------------------------------

var
  S: string;

begin
  if (GraphicClass = nil) or not GraphicClass.InheritsFrom(TGraphic) then
    GraphicExError(gesInvalidGraphicClass, [Extension]);

  if Extension <> '' then
  begin
    ExtIndex := FindExtension(Extension);
    ClassIndex := FindGraphicClass(GraphicClass);
    if ExtIndex = -1 then
    begin
      if Extension[1] = '.' then
        // Extension should be registered without the '.'
        GraphicExError(gesInvalidExtension);
      // extension not yet registered
      New(ExtEntry);
      ExtEntry.Extension := Extension;
      ExtEntry.Description := Individual;
      ExtEntry.FormatTypes := FormatTypes;
      FExtensionList.Add(ExtEntry);
      UpdateClassEntry;
    end
    else
      if Replace then
      begin
        // replace current extension entry with new one
        ExtEntry := FExtensionList[ExtIndex];
        if ExtEntry.ClassReference.GraphicClass <> GraphicClass then
        begin
          // assign existing extension to new graphic class
          OldReference := ExtEntry.ClassReference;
          UpdateClassEntry;
          Dec(OldReference.Count);
          // remove the graphic class entry if no longer used
          if OldReference.Count = 0 then
            FClassList.Remove(OldReference);
        end;
          // otherwise do nothing
      end
      else
        GraphicExError(gesRegistration, [Extension]);

    // finally make TPicture work
    S := Individual;
    if S = '' then
      S := ClassEntry.Description;
    TPicture.RegisterFileFormat(Extension, S, GraphicClass);
  end
  else
    // No extension specified
    GraphicExError(gesNoExtension, [Extension]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFileFormatList.UnregisterFileFormat(const Extension: string; GraphicClass: TGraphicClass);

// Removes the entry for the given extension from the internal list.
// If Extension is '' then all associations for the given GraphicClass are removed otherwise the class is ignored and
// only the one particular extension is removed.
// Unregistration from TPicture is done here too, if necessary.
// If Extension is '' and GraphicClass isn't found then we silently ignore unregistering.
// This makes it possible to unregister a class without being sure if it is registered.

var
  ExtIndex,
  ClassIndex: Integer;
  ExtEntry: PExtensionEntry;
  ClassEntry: PClassEntry;

begin
  ExtIndex := FindExtension(Extension);
  // make sure we don't try to remove a non-registered extension
  if (Extension = '') or (ExtIndex > -1) then
  begin
    if ExtIndex > -1 then
    begin
      // there's an entry for the extension
      ExtEntry := FExtensionList[ExtIndex];
      Dec(ExtEntry.ClassReference.Count);
      // unregister graphic class too if necessary
      if ExtEntry.ClassReference.Count = 0 then
      begin
        TPicture.UnregisterGraphicClass(ExtEntry.ClassReference.GraphicClass);
        Dispose(ExtEntry.ClassReference);
        FClassList.Remove(ExtEntry.ClassReference);
      end;

      // finally delete extension entry
      Dispose(ExtEntry);
      FExtensionList.Delete(ExtIndex);
    end
    else
    begin
      // all entries for the given graphic class must be removed
      ClassIndex := FindGraphicClass(GraphicClass);
      // If GraphicClass is not found then silently Exit
      if ClassIndex = -1 then
        Exit;
      ClassEntry := FClassList[ClassIndex];
      for ExtIndex := FExtensionList.Count - 1 downto 0 do
      begin
        if PExtensionEntry(FExtensionList[ExtIndex]).ClassReference.GraphicClass = GraphicClass then
        begin
          Dec(ClassEntry.Count);
          Dispose(PExtensionEntry(FExtensionList[ExtIndex]));
          FExtensionList.Delete(ExtIndex);
          // no need to run through further entries if all references are done
          if ClassEntry.Count = 0 then
            Break;
        end;
      end;
      Dispose(ClassEntry);
      FClassList.Delete(ClassIndex);
      TPicture.UnregisterGraphicClass(GraphicClass);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  FileFormatList := TFileFormatList.Create;
  with FileFormatList do
  begin
    // Since we are going to add these image types below, we better unregister
    // them first in order to avoid double entries.
    TPicture.UnregisterGraphicClass(TBitmap);
    TPicture.UnregisterGraphicClass(TIcon);
    {$IFNDEF FPC}
    TPicture.UnregisterGraphicClass(TMetafile);
    {$ENDIF}

    RegisterFileFormat('bmp', gesBitmaps, '', [ftRaster], False, TBitmap);
    RegisterFileFormat('ico', gesIcons, '', [ftRaster], False, TIcon);
    {$IFNDEF FPC}
    RegisterFileFormat('wmf', gesMetaFiles, '', [ftVector], False, TMetafile);
    RegisterFileFormat('emf', gesMetaFiles, gesEnhancedMetaFiles, [ftVector], False, TMetafile);
    {$ENDIF}

    {$ifdef JpegGraphic}
    {$IFDEF USE_TJPEGIMAGE}
    TPicture.UnregisterGraphicClass(TJPEGImage);
    RegisterFileFormat('jfif', gesJPGImages, gesJFIFImages, [ftRaster], False, TJPEGImage);
    RegisterFileFormat('jpg', '', gesJPGImages, [ftRaster], False, TJPEGImage);
    RegisterFileFormat('jpe', '', gesJPEImages, [ftRaster], False, TJPEGImage);
    RegisterFileFormat('jpeg', '', gesJPEGImages, [ftRaster], False, TJPEGImage);
    {$ENDIF}
    {$endif ~JpegGraphic}

    // register our own formats
    RegisterFileFormat('rle', gesBitmaps, gesRLEBitmaps, [ftRaster], False, TBitmap);
    RegisterFileFormat('dib', '', gesDIBs, [ftRaster], False, TBitmap);

    {$ifdef PortableNetworkGraphic}
      RegisterFileFormat('png', gesPortableNetworkGraphic, '', [ftRaster], False, TPNGGraphic);
    {$endif PortableNetworkGraphic}

    {$ifdef GIFGraphic}
      RegisterFileFormat('gif', gesGIF, '', [ftRaster, ftMultiImage, ftAnimation], False, TGIFGraphic);
    {$endif GIFGraphic}

    {$ifdef TIFFGraphic}
      // Set the TIFF error handler
      LibTiffDelphiSetErrorHandler(TiffError);
      RegisterFileFormat('tif', gesTIFF, gesPCTIF, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      RegisterFileFormat('tiff', '', gesMacTIFF, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      RegisterFileFormat('fax', '', gesGFIFax, [ftRaster, ftMultiImage], False, TTIFFGraphic);
      {$ifdef EPSGraphic}
        RegisterFileFormat('eps', gesEPS, '', [ftRaster], False, TEPSGraphic);
      {$endif EPSGraphic}
    {$endif TIFFGraphic}

    {$ifdef PortableMapGraphic}
      RegisterFileFormat('ppm', gesPortable, gesPortablePixel, [ftRaster], False, TPPMGraphic);
      RegisterFileFormat('pnm', '', gesPortableAny, [ftRaster], False, TPPMGraphic);
      RegisterFileFormat('pgm', '', gesPortableGray, [ftRaster], False, TPPMGraphic);
      RegisterFileFormat('pbm', '', gesPortableMono, [ftRaster], False, TPPMGraphic);
    {$endif PortableMapGraphic}

    {$ifdef PhotoshopGraphic}
      RegisterFileFormat('psd', gesPhotoshop, '', [ftRaster, ftLayered], False, TPSDGraphic);
      RegisterFileFormat('pdd', '', '', [ftRaster, ftLayered], False, TPSDGraphic);
    {$endif PhotoshopGraphic}

    {$ifdef PaintshopProGraphic}
      RegisterFileFormat('psp', gesPaintshopPro, '', [ftRaster, ftVector], False, TPSPGraphic);
      RegisterFileFormat('pfr', '', gesPaintshopProFrames, [ftRaster, ftVector], False, TPSPGraphic);
      RegisterFileFormat('tub', '', gesPaintshopProTubes, [ftRaster, ftVector], False, TPSPGraphic);

      // Paintshop pro *.msk files are just grayscale bitmaps.
      RegisterFileFormat('msk', '', '', [ftRaster], False, TBitmap);
    {$endif PaintshopProGraphic}

    {$ifdef TargaGraphic}
      RegisterFileFormat('tga', gesTruevision, '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('vst', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('vda', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('win', '', '', [ftRaster], False, TTargaGraphic);
      RegisterFileFormat('icb', '', '', [ftRaster], False, TTargaGraphic);
    {$endif TargaGraphic}

    {$ifdef PCDGraphic}
      RegisterFileFormat('pcd', gesKodakPhotoCD, '', [ftRaster], False, TPCDGraphic);
    {$endif PCDGraphic}

    {$ifdef PCXGraphic}
      RegisterFileFormat('pcx', gesZSoft, '', [ftRaster], False, TPCXGraphic);
      RegisterFileFormat('pcc', '', '', [ftRaster], False, TPCXGraphic);
      RegisterFileFormat('scr', '', gesZSoftWord, [ftRaster], False, TPCXGraphic);
    {$endif PCXGraphic}

    {$ifdef RLAGraphic}
      RegisterFileFormat('rpf', gesAliasWaveFront, '', [ftRaster], False, TRLAGraphic);
      RegisterFileFormat('rla', '', '', [ftRaster], False, TRLAGraphic);
    {$endif RLAGraphic}

    {$ifdef SGIGraphic}
      RegisterFileFormat('sgi', gesSGI, gesSGITrueColor, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('rgba', '', gesSGITrueColorAlpha, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('rgb', '', gesSGITrueColor, [ftRaster], False, TSGIGraphic);
      RegisterFileFormat('bw', '', gesSGIMono, [ftRaster], False, TSGIGraphic);
    {$endif SGIGraphic}

    {$ifdef CUTGraphic}
      RegisterFileFormat('cut', gesHalo, '', [ftRaster], False, TCUTGraphic);
    {$endif CUTGraphic}

    {$ifdef AutodeskGraphic}
      RegisterFileFormat('cel', gesAutodesk, '', [ftRaster], False, TAutodeskGraphic);
      RegisterFileFormat('pic', gesAutodesk, '', [ftRaster], False, TAutodeskGraphic);
    {$endif AutodeskGraphic}

    {$ifdef ArtsAndLettersGraphic}
      RegisterFileFormat('ged', gesArtsAndLettersGraphic, '', [ftRaster], False, TGEDGraphic);
    {$endif ArtsAndLettersGraphic}
  end;
finalization
  // No need to unregister specific file formats here since all formats
  // will be Unregistered in FileFormatList.Clear, which is called by Destroy.
  FileFormatList.Free;
end.

