unit LibJpegDelphi;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$A8} // align on 8 bytes  (64 bits needs to be 8 aligned, 32 bits can also use A4)
{$Z4} // enum size = dword

uses
  Windows, SysUtils, C_Types;

const

  JPEG_SUSPENDED          = 0; // Suspended due to lack of input data
  JPEG_HEADER_OK          = 1; // Found valid image datastream
  JPEG_HEADER_TABLES_ONLY = 2; // Found valid table-specs-only datastream
  // If you pass require_image = TRUE (normal case), you need not check for
  // a TABLES_ONLY return code; an abbreviated file will cause an error exit.
  // JPEG_SUSPENDED is only possible if you use a data source module that can
  // give a suspension return (the stdio source module doesn't).

  JPEG_LIB_VERSION    = 62;  { Version 6b }

  JPEG_RST0 = $D0; // RST0 marker code
  JPEG_EOI  = $D9; // EOI marker code
  JPEG_APP0 = $E0; // APP0 marker code
  JPEG_COM  = $FE; // COM marker code

  JMSG_STR_PARM_MAX   = 80;
  JMSG_LENGTH_MAX     = 200; { recommended size of format_message buffer }
  NUM_QUANT_TBLS      = 4;   { Quantization tables are numbered 0..3 }
  NUM_HUFF_TBLS       = 4;   { Huffman tables are numbered 0..3 }
  NUM_ARITH_TBLS      = 16;  { Arith-coding tables are numbered 0..15 }
  MAX_COMPS_IN_SCAN   = 4;   { JPEG limit on # of components in one scan }
  C_MAX_BLOCKS_IN_MCU = 10;  { compressor's limit on blocks per MCU }
  D_MAX_BLOCKS_IN_MCU = 10;  { decompressor's limit on blocks per MCU }
  DCTSIZE2 = 64;

  JFIF_DENSITY_UNIT_UNKNOWN = 0; // unknown, but valid to use for aspect ratio
  JFIF_DENSITY_UNIT_DPI     = 1; // Dots per inch
  JFIF_DENSITY_UNIT_DPCM    = 2; // Dots per cm

  JPOOL_PERMANENT = 0; // lasts until master record is destroyed
  JPOOL_IMAGE	  = 1; // lasts until done with image/datastream
  JPOOL_NUMPOOLS  = 2;

type
  // Known color spaces.
  J_COLOR_SPACE = (
    JCS_UNKNOWN,   // error/unspecified
    JCS_GRAYSCALE, // monochrome
    JCS_RGB,       // red/green/blue
    JCS_YCbCr,     // Y/Cb/Cr (also known as YUV)
    JCS_CMYK,      // C/M/Y/K
    JCS_YCCK       // Y/Cb/Cr/K
  );

  JSAMPLE = Byte;
  JDIMENSION = Cardinal;
  JSAMPLE_ptr = ^JSAMPLE;
  JSAMPROW_ptr = ^JSAMPROW;

  jTSample = 0..(MaxInt div SIZEOF(JSAMPLE)) - 1;
  JSAMPLE_array = array[jTSample] of JSAMPLE;
  JSAMPROW = ^JSAMPLE_array;    // ptr to one image row of pixel samples.

  jTRow = 0..(MaxInt div SIZEOF(JSAMPROW)) - 1;
  JSAMPROW_array = array[jTRow] of JSAMPROW;
  JSAMParray = ^JSAMPROW_array; // ptr to some rows (a 2-D sample array)

  j_common_ptr = ^RJpegCommonStruct;
  // JPEG library memory manger routines
  jpeg_memory_mgr_ptr = ^jpeg_memory_mgr;
  jpeg_memory_mgr = record
    // Method Pointers
    alloc_small: function (cinfo: j_common_ptr; pool_id: Integer; sizeofobject: size_t): Pointer; cdecl;
    alloc_large: function (cinfo: j_common_ptr; pool_id: Integer; sizeofobject: size_t): Pointer; cdecl;
    alloc_sarray: function (cinfo: j_common_ptr; pool_id: Integer; samplesperrow: JDIMENSION; numrows: JDIMENSION): JSAMParray; cdecl;
    alloc_barray: Pointer;
    request_virt_sarray: Pointer;
    request_virt_barray: Pointer;
    realize_virt_arrays: Pointer;
    access_virt_sarray: Pointer;
    access_virt_barray: Pointer;
    free_pool: Pointer;
    self_destruct: Pointer;
    max_memory_to_use: Longint;
  end;

  PRJpegErrorMgr = ^RJpegErrorMgr;
  PRJpegMemoryMgr = jpeg_memory_mgr_ptr; //Pointer;
  PRJpegProgressMgr = Pointer;
  PRJpegDestinationMgr = ^RJpegDestinationMgr;
  PRJpegSourceMgr = ^RJpegSourceMgr;
  PRJpegComponentInfo = ^RJpegComponentInfo;
  PRJpegQuantTbl = Pointer;
  PRJpegHuffTbl = Pointer;
  PRJpegScanInfo = Pointer;
  PRJpegCompMaster = Pointer;
  PRJpegCMainController = Pointer;
  PRJpegCPrepController = Pointer;
  PRJpegCCoefController = Pointer;
  PRJpegMarkerWriter = Pointer;
  PRJpegColorConverter = Pointer;
  PRJpegDownsampler = Pointer;
  PRJpegForwardDct = Pointer;
  PRJpegEntropyEncoder = Pointer;
  PRJpegSavedMarker = Pointer;
  PRJpegDecompMaster = Pointer;
  PRJpegDMainController = Pointer;
  PRJpegDCoefController = Pointer;
  PRJpegDPosController = Pointer;
  PRJpegInputController = Pointer;
  PRJpegMarkerReader = Pointer;
  PRJpegEntropyDecoder = Pointer;
  PRJpegInverseDct = Pointer;
  PRJpegUpsampler = Pointer;
  PRJpegColorDeconverter = Pointer;
  PRJpegColorQuantizer = Pointer;
  PRJpegCommonStruct = ^RJpegCommonStruct;
  PRJpegCompressStruct = ^RJpegCompressStruct;
  PRJpegDecompressStruct = ^RJpegDecompressStruct;

  TJpegErrorExit = procedure(cinfo: PRJpegCommonStruct); cdecl;
  TJpegEmitMessage = procedure(cinfo: PRJpegCommonStruct; MsgLevel: Integer); cdecl;
  TJpegOutputMessage = procedure(cinfo: PRJpegCommonStruct); cdecl;
  TJpegFormatMessage = procedure(cinfo: PRJpegCommonStruct; Buffer: Pointer); cdecl;
  TJpegResetErrorMgr = procedure(cinfo: PRJpegCommonStruct); cdecl;

  RJpegErrorMgrMsgParm = record
  case Boolean of
    False: (MsgParmI: array[0..7] of Integer);
    True: (MsgParmS: array[0..JMSG_STR_PARM_MAX-1] of AnsiChar);
  end;

  RJpegErrorMgr = record
    ErrorExit: TJpegErrorExit;         { Error exit handler: does not return to caller }
    EmitMessage: TJpegEmitMessage;     { Conditionally emit a trace or warning message }
    OutputMessage: TJpegOutputMessage; { Routine that actually outputs a trace or error message }
    FormatMessage: TJpegFormatMessage; { Format a message string for the most recent JPEG error or message }
    ResetErrorMgr: TJpegResetErrorMgr; { Reset error state variables at start of a new image }
    { The message ID code and any parameters are saved here. A message can have one string parameter or up to 8 int parameters. }
    MsgCode: Integer;
    MsgParm: RJpegErrorMgrMsgParm;
    { Standard state variables for error facility }
    TraceLevel: Integer; {max msg_level that will be displayed}
    { For recoverable corrupt-data errors, we emit a warning message, but keep going unless emit_message chooses to abort.
      emit_message should count warnings in num_warnings.  The surrounding application can check for bad data by seeing if num_warnings
      is nonzero at the end of processing. }
    NumWarnings: Integer;    { number of corrupt-data warnings }
    { These fields point to the table(s) of error message strings. An application can change the table pointer to switch to a different
      message list (typically, to change the language in which errors are reported).  Some applications may wish to add additional
      error codes that will be handled by the JPEG library error mechanism; the second table pointer is used for this purpose.
      First table includes all errors generated by JPEG library itself. Error code 0 is reserved for a "no such error string" message. }
    JpegMessageTable: PPAnsiChar;      { Library errors }
    LastJpegMessage: Integer;      { Table contains strings 0..last_jpeg_message }
    { Second table can be added by application (see cjpeg/djpeg for example). It contains strings numbered
      first_addon_message..last_addon_message. }
    AddonMessageTable: PPAnsiChar;     { Non-library errors }
    FirstAddonMessage: Integer;    { code for first string in addon table }
    LastAddonMessage: Integer;     { code for last string in addon table }
  end;

  TJpegInitDestination = procedure(cinfo: PRJpegCompressStruct); cdecl;
  TJpegEmptyOutputBuffer = function(cinfo: PRJpegCompressStruct): Boolean; cdecl;
  TJpegTermDestination = procedure(cinfo: PRJpegCompressStruct); cdecl;

  RJpegDestinationMgr = record
    NextOutputByte: Pointer;       { => next byte to write in buffer }
    FreeInBuffer: size_t;          { # of byte spaces remaining in buffer }
    InitDestination: TJpegInitDestination;
    EmptyOutputBuffer: TJpegEmptyOutputBuffer;
    TermDestination: TJpegTermDestination;
  end;

  TJpegInitSource = procedure(cinfo: PRJpegDecompressStruct); cdecl;
  TJpegFillInputBuffer = function(cinfo: PRJpegDecompressStruct): Boolean; cdecl;
  TJpegSkipInputData = procedure(cinfo: PRJpegDecompressStruct; NumBytes: Integer); cdecl;
  TJpegResyncToRestart = function(cinfo: PRJpegDecompressStruct; Desired: Integer): Boolean; cdecl;
  TJpegTermSource = procedure(cinfo: PRJpegDecompressStruct); cdecl;

  RJpegSourceMgr = record
    NextInputByte: Pointer;
    BytesInBuffer: size_t;
    InitSource: TJpegInitSource;
    FillInputBuffer: TJpegFillInputBuffer;
    SkipInputData: TJpegSkipInputData;
    ResyncToRestart: TJpegResyncToRestart;
    TermSource: TJpegTermSource;
  end;

  RJpegComponentInfo = record
    { Basic info about one component (color channel). }
    { These values are fixed over the whole image. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOF marker. }
    ComponentId: Integer;          { identifier for this component (0..255) }
    ComponentIndex: Integer;       { its index in SOF or cinfo->comp_info[] }
    HSampFactor: Integer;          { horizontal sampling factor (1..4) }
    VSampFactor: Integer;          { vertical sampling factor (1..4) }
    QuantTblNo: Integer;           { quantization table selector (0..3) }
    { These values may vary between scans. }
    { For compression, they must be supplied by parameter setup; }
    { for decompression, they are read from the SOS marker. }
    { The decompressor output side may not use these variables. }
    DcTblNo: Integer;              { DC entropy table selector (0..3) }
    AsTblNo: Integer;              { AC entropy table selector (0..3) }
    { Remaining fields should be treated as private by applications. }
    { These values are computed during compression or decompression startup: }
    { Component's size in DCT blocks. Any dummy blocks added to complete an MCU are not counted; therefore these values do not depend
      on whether a scan is interleaved or not. }
    WidthInBlocks: Cardinal;
    HeightInBlocks: Cardinal;
    { Size of a DCT block in samples.  Always DCTSIZE for compression. For decompression this is the size of the output from one DCT
      block, reflecting any scaling we choose to apply during the IDCT step. Values of 1,2,4,8 are likely to be supported.  Note that
      different components may receive different IDCT scalings. }
    DctScaledSize: Integer;
    { The downsampled dimensions are the component's actual, unpadded number of samples at the main buffer (preprocessing/compression
      interface), thus downsampled_width = ceil(image_width * Hi/Hmax) and similarly for height.  For decompression, IDCT scaling is
      included, so downsampled_width = ceil(image_width * Hi/Hmax * DCT_scaled_size/DCTSIZE) }
    DownsampledWidth: Cardinal;    { actual width in samples }
    DownsampledHeight: Cardinal;   { actual height in samples }
    { This flag is used only for decompression.  In cases where some of the components will be ignored (eg grayscale output from YCbCr
      image), we can skip most computations for the unused components. }
    ComponentNeeded: LongBool;     { do we need the value of this component? }
    { These values are computed before starting a scan of the component. }
    { The decompressor output side may not use these variables. }
    McuWidth: Integer;             { number of blocks per MCU, horizontally }
    McuHeight: Integer;            { number of blocks per MCU, vertically }
    McuBlocks: Integer;            { MCU_width * MCU_height }
    McuSampleWidth: Integer;       { MCU width in samples, MCU_width*DCT_scaled_size }
    LastColWidth: Integer;         { # of non-dummy blocks across in last MCU }
    LastRowHeight: Integer;        { # of non-dummy blocks down in last MCU }
    { Saved quantization table for component; NULL if none yet saved. See jdinput.c comments about the need for this information. This
      field is currently used only for decompression. }
    QuantTable: PRJpegQuantTbl;
    { Private per-component storage for DCT or IDCT subsystem. }
    DctTable: Pointer;
  end;

  RJpegCommonStruct = record
    Err: PRJpegErrorMgr;           { Error handler module }
    Mem: PRJpegMemoryMgr;          { Memory manager module }
    Progress: PRJpegProgressMgr;   { Progress monitor, or NULL if none }
    ClientData: Pointer;           { Available for use by application }
    IsDecompressor: LongBool;      { So common code can tell which is which }
    GlobalState: Integer;          { For checking call sequence validity }
  end;

  RJpegCompressStruct = record
    Err: PRJpegErrorMgr;           { Error handler module }
    Mem: PRJpegMemoryMgr;          { Memory manager module }
    Progress: PRJpegProgressMgr;   { Progress monitor, or NULL if none }
    ClientData: Pointer;           { Available for use by application }
    IsDecompressor: LongBool;      { So common code can tell which is which }
    GlobalState: Integer;          { For checking call sequence validity }
    { Destination for compressed data }
    Dest: PRJpegDestinationMgr;
    { Description of source image --- these fields must be filled in by outer application before starting compression.
      in_color_space must be correct before you can even call jpeg_set_defaults(). }
    ImageWidth: Cardinal;          { input image width }
    ImageHeight: Cardinal;         { input image height }
    InputComponents: Integer;      { # of color components in input image }
    InColorSpace: J_COLOR_SPACE;   { colorspace of input image }
    InputGamme: Double;            { image gamma of input image }
    { Compression parameters --- these fields must be set before calling jpeg_start_compress().  We recommend calling
      jpeg_set_defaults() to initialize everything to reasonable defaults, then changing anything the application specifically wants
      to change.  That way you won't get burnt when new parameters are added.  Also note that there are several helper routines to
      simplify changing parameters. }
    DataPrecision: Integer;        { bits of precision in image data }
    NumComponents: Integer;        { # of color components in JPEG image }
    JpegColorSpace: J_COLOR_SPACE; { colorspace of JPEG image }
    CompInfo: PRJpegComponentInfo; { comp_info[i] describes component that appears i'th in SOF }
    QuantTblPtrs: array[0..NUM_QUANT_TBLS-1] of PRJpegQuantTbl;   {ptrs to coefficient quantization tables, or NULL if not defined }
    DcHuffTblPtrs: array[0..NUM_HUFF_TBLS-1] of PRJpegHuffTbl;    {ptrs to Huffman coding tables, or NULL if not defined }
    AcHuffTblPtrs: array[0..NUM_HUFF_TBLS-1] of PRJpegHuffTbl;
    ArithDcL: array[0..NUM_ARITH_TBLS-1] of Byte;                 { L values for DC arith-coding tables }
    ArithDcU: array[0..NUM_ARITH_TBLS-1] of Byte;                 { U values for DC arith-coding tables }
    ArithAcK: array[0..NUM_ARITH_TBLS-1] of Byte;                 { Kx values for AC arith-coding tables }
    NumScans: Integer;             { # of entries in scan_info array }
    ScanInfo: PRJpegScanInfo;      { script for multi-scan file, or NULL }
    { The default value of scan_info is NULL, which causes a single-scan sequential JPEG file to be emitted.  To create a multi-scan
      file, set num_scans and scan_info to point to an array of scan definitions. }
    RawDataIn: LongBool;            { TRUE=caller supplies downsampled data }
    ArithCode: LongBool;            { TRUE=arithmetic coding, FALSE=Huffman }
    OptimizeCoding: LongBool;       { TRUE=optimize entropy encoding parms }
    CCIR601Sampling: LongBool;      { TRUE=first samples are cosited }
    SmoothingFactor: Integer;      { 1..100, or 0 for no input smoothing }
    DctMethod: Integer;            { DCT algorithm selector }
    { The restart interval can be specified in absolute MCUs by setting restart_interval, or in MCU rows by setting restart_in_rows
      (in which case the correct restart_interval will be figured for each scan). }
    RestartInterval: Cardinal;     { MCUs per restart, or 0 for no restart }
    RestartInRows: Integer;        { if > 0, MCU rows per restart interval }
    { Parameters controlling emission of special markers. }
    WriteJfifHeader: LongBool;      { should a JFIF marker be written? }
    JfifMajorVersion: Byte;        { What to write for the JFIF version number }
    JFifMinorVersion: Byte;
    { These three values are not used by the JPEG code, merely copied  into the JFIF APP0 marker.  density_unit can be 0 for unknown,
      1 for dots/inch, or 2 for dots/cm.  Note that the pixel aspect ratio is defined by X_density/Y_density even when density_unit=0. }
    DensityUnit: Byte;             { JFIF code for pixel size units }
    XDensity: Word;                { Horizontal pixel density }
    YDensity: WOrd;                { Vertical pixel density }
    WriteAdobeMarker: LongBool;     { should an Adobe marker be written? }
    { State variable: index of next scanline to be written to jpeg_write_scanlines().  Application may use this to control its
      processing loop, e.g., "while (next_scanline < image_height)". }
    NextScanline: Cardinal;        { 0 .. image_height-1  }
    { Remaining fields are known throughout compressor, but generally should not be touched by a surrounding application. }
    { These fields are computed during compression startup }
    ProgressiveMode: LongBool;      { TRUE if scan script uses progressive mode }
    MaxHSampFactor: Integer;       { largest h_samp_factor }
    MaxVSampFactor: Integer;       { largest v_samp_factor }
    TotalIMCURows: Cardinal;       { # of iMCU rows to be input to coef ctlr }
    { The coefficient controller receives data in units of MCU rows as defined for fully interleaved scans (whether the JPEG file is
      interleaved or not). There are v_samp_factor * DCTSIZE sample rows of each component in an "iMCU" (interleaved MCU) row. }
    { These fields are valid during any one scan. They describe the components and MCUs actually appearing in the scan. }
    CompsInScan: Integer;          { # of JPEG components in this scan }
    CurCompInfo: array[0..MAX_COMPS_IN_SCAN-1] of PRJpegComponentInfo;
    { *cur_comp_info[i] describes component that appears i'th in SOS }
    MCUsPerRow: Cardinal;          { # of MCUs across the image }
    MCUsRowsInScan: Cardinal;      { # of MCU rows in the image }
    BlocksInMcu: Integer;          { # of DCT blocks per MCU }
    MCUMembership: array[0..C_MAX_BLOCKS_IN_MCU-1] of Integer;
    { MCU_membership[i] is index in cur_comp_info of component owning i'th block in an MCU }
    Ss,Se,Ah,Al: Integer;          { progressive JPEG parameters for scan }
    { Links to compression subobjects (methods and private variables of modules) }
    Master: PRJpegCompMaster;
    Main: PRJpegCMainController;
    Prep: PRJpegCPrepController;
    Coef: PRJpegCCoefController;
    Marker: PRJpegMarkerWriter;
    CConvert: PRJpegColorConverter;
    Downsample: PRJpegDownsampler;
    FDct: PRJpegForwardDct;
    Entropy: PRJpegEntropyEncoder;
    ScriptSpace: PRJpegScanInfo;   { workspace for jpeg_simple_progression }
    ScriptSpaceSize: Integer;
  end;

  RJpegDecompressStruct = record
    { Fields shared with jpeg_compress_struct }
    Err: PRJpegErrorMgr;           { Error handler module }
    Mem: PRJpegMemoryMgr;          { Memory manager module }
    Progress: PRJpegProgressMgr;   { Progress monitor, or NULL if none }
    ClientData: Pointer;           { Available for use by application }
    IsDecompressor: LongBool;       { So common code can tell which is which }
    GlobalState: Integer;          { For checking call sequence validity }
    { Source of compressed data }
    Src: PRJpegSourceMgr;
    { Basic description of image --- filled in by jpeg_read_header(). }
    { Application may inspect these values to decide how to process image. }
    ImageWidth: Cardinal;          { nominal image width (from SOF marker) }
    ImageHeight: Cardinal;         { nominal image height }
    NumComponents: Integer;        { # of color components in JPEG image }
    JpegColorSpace: J_COLOR_SPACE; { colorspace of JPEG image }
    { Decompression processing parameters --- these fields must be set before calling jpeg_start_decompress().  Note that
      jpeg_read_header() initializes them to default values. }
    OutColorSpace: J_COLOR_SPACE;  { colorspace for output }
    ScaleNum,ScaleDenom: Cardinal; { fraction by which to scale image }
    OutputGamma: Double;           { image gamma wanted in output }
    BufferedImage: LongBool;        { TRUE=multiple output passes }
    RawDataOut: LongBool;           { TRUE=downsampled data wanted }
    DctMethod: Integer;            { IDCT algorithm selector }
    DoFancyUpsampling: LongBool;    { TRUE=apply fancy upsampling }
    DoBlockSmoothing: LongBool;     { TRUE=apply interblock smoothing }
    QuantizeColors: LongBool;       { TRUE=colormapped output wanted }
    { the following are ignored if not quantize_colors: }
    DitherMode: Integer;           { type of color dithering to use }
    TwoPassQuantize: LongBool;      { TRUE=use two-pass color quantization }
    DesiredNumberOfColors: Integer;{ max # colors to use in created colormap }
    { these are significant only in buffered-image mode: }
    Enable1PassQuant: LongBool;     { enable future use of 1-pass quantizer }
    EnableExternalQuant: LongBool;  { enable future use of external colormap }
    Enable2PassQuant: LongBool;     { enable future use of 2-pass quantizer }
    { Description of actual output image that will be returned to application. These fields are computed by jpeg_start_decompress().
      You can also use jpeg_calc_output_dimensions() to determine these values in advance of calling jpeg_start_decompress(). }
    OutputWidth: Cardinal;         { scaled image width }
    OutputHeight: Cardinal;        { scaled image height }
    OutColorComponents: Integer;   { # of color components in out_color_space }
    OutputComponents: Integer;     { # of color components returned }
    { output_components is 1 (a colormap index) when quantizing colors; otherwise it equals out_color_components. }
    RecOutbufHeight: Integer;      { min recommended height of scanline buffer }
    { If the buffer passed to jpeg_read_scanlines() is less than this many rows high, space and time will be wasted due to unnecessary
      data copying. Usually rec_outbuf_height will be 1 or 2, at most 4. }
    { When quantizing colors, the output colormap is described by these fields. The application can supply a colormap by setting
      colormap non-NULL before calling jpeg_start_decompress; otherwise a colormap is created during jpeg_start_decompress or
      jpeg_start_output. The map has out_color_components rows and actual_number_of_colors columns. }
    ActualNumberOfColors: Integer; { number of entries in use }
    Colormap: Pointer;             { The color map as a 2-D pixel array }
    { State variables: these variables indicate the progress of decompression. The application may examine these but must not modify
      them. }
    { Row index of next scanline to be read from jpeg_read_scanlines(). Application may use this to control its processing loop, e.g.,
      "while (output_scanline < output_height)". }
    OutputScanline: Cardinal;      { 0 .. output_height-1 }
    { Current input scan number and number of iMCU rows completed in scan. These indicate the progress of the decompressor input side. }
    InputScanNumber: Integer;      { Number of SOS markers seen so far }
    InputIMcuRow: Cardinal;        { Number of iMCU rows completed }
    { The "output scan number" is the notional scan being displayed by the output side.  The decompressor will not allow output
      scan/row number to get ahead of input scan/row, but it can fall arbitrarily far behind. }
    OutputScanNumber: Integer;     { Nominal scan number being displayed }
    OutputIMcuRow: Cardinal;       { Number of iMCU rows read }
    { Current progression status.  coef_bits[c][i] indicates the precision with which component c's DCT coefficient i (in zigzag order)
      is known. It is -1 when no data has yet been received, otherwise it is the point transform (shift) value for the most recent scan
      of the coefficient (thus, 0 at completion of the progression). This pointer is NULL when reading a non-progressive file. }
    CoefBits: Pointer;             { -1 or current Al value for each coef }
    { Internal JPEG parameters --- the application usually need not look at these fields.  Note that the decompressor output side may
      not use any parameters that can change between scans. }
    { Quantization and Huffman tables are carried forward across input datastreams when processing abbreviated JPEG datastreams. }
    QuantTblPtrs: array[0..NUM_QUANT_TBLS-1] of Pointer;
    { ptrs to coefficient quantization tables, or NULL if not defined }
    DcHuffTblPtrs: array[0..NUM_HUFF_TBLS-1] of Pointer;
    AcHuffTblPtrs: array[0..NUM_HUFF_TBLS-1] of Pointer;
    { ptrs to Huffman coding tables, or NULL if not defined }
    { These parameters are never carried across datastreams, since they are given in SOF/SOS markers or defined to be reset by SOI. }
    DataPrecision: Integer;        { bits of precision in image data }
    CompInfo: PRJpegComponentInfo; { comp_info[i] describes component that appears i'th in SOF }
    ProgressiveMode: LongBool;      { TRUE if SOFn specifies progressive mode }
    ArithCode: LongBool;            { TRUE=arithmetic coding, FALSE=Huffman }
    ArithDcL: array[0..NUM_ARITH_TBLS-1] of Byte;       { L values for DC arith-coding tables }
    ArithDcY: array[0..NUM_ARITH_TBLS-1] of Byte;       { U values for DC arith-coding tables }
    ArithAcK: array[0..NUM_ARITH_TBLS-1] of Byte;       { Kx values for AC arith-coding tables }
    RestartInterval: Cardinal;     { MCUs per restart interval, or 0 for no restart }
    { These fields record data obtained from optional markers recognized by the JPEG library. }
    SawJfifMarker: LongBool;        { TRUE iff a JFIF APP0 marker was found }
    { Data copied from JFIF marker; only valid if saw_JFIF_marker is TRUE: }
    JfifMajorVersion: Byte;        { JFIF version number }
    JfifMinorVersion: Byte;        { JFIF code for pixel size units }
    density_unit: Byte;            // JFIF code for pixel size units
    XDensity: Word;                { Horizontal pixel density }
    YDensity: Word;                { Vertical pixel density }
    SawAdobeMarker: LongBool;       { TRUE iff an Adobe APP14 marker was found }
    AdobeTransform: Byte;          { Color transform code from Adobe marker }
    Ccir601Sampling: LongBool;      { TRUE=first samples are cosited }
    { Aside from the specific data retained from APPn markers known to the library, the uninterpreted contents of any or all APPn and
      COM markers can be saved in a list for examination by the application. }
    MarkerList: PRJpegSavedMarker; { Head of list of saved markers }
    { Remaining fields are known throughout decompressor, but generally should not be touched by a surrounding application. }
    { These fields are computed during decompression startup }
    MaxHSampFactor: Integer;       { largest h_samp_factor }
    MaxVSampFactor: Integer;       { largest v_samp_factor }
    MinDctScaledSize: Integer;     { smallest DCT_scaled_size of any component }
    TotalIMcuRows: Cardinal;       { # of iMCU rows in image }
    { The coefficient controller's input and output progress is measured in units of "iMCU" (interleaved MCU) rows.  These are the same
      as MCU rows in fully interleaved JPEG scans, but are used whether the scan is interleaved or not.  We define an iMCU row as
      v_samp_factor DCT block rows of each component.  Therefore, the IDCT output contains v_samp_factor*DCT_scaled_size sample rows
      of a component per iMCU row. }
    SampleRangeLimit: Pointer;     { table for fast range-limiting }
    { These fields are valid during any one scan. They describe the components and MCUs actually appearing in the scan. Note that the
      decompressor output side must not use these fields. }
    CompsInScan: Integer;          { # of JPEG components in this scan }
    CurCompInfo: array[0..MAX_COMPS_IN_SCAN-1] of PRJpegComponentInfo;
    { *cur_comp_info[i] describes component that appears i'th in SOS }
    McusPerRow: Cardinal;          { # of MCUs across the image }
    McuRowsInScan: Cardinal;       { # of MCU rows in the image }
    BlocksInMcu: Integer;          { # of DCT blocks per MCU }
    McuMembership: array[0..D_MAX_BLOCKS_IN_MCU-1] of Integer;
    { MCU_membership[i] is index in cur_comp_info of component owning  i'th block in an MCU }
    Ss,Se,Ah,Al: Integer;          { progressive JPEG parameters for scan }
    { This field is shared between entropy decoder and marker parser. It is either zero or the code of a JPEG marker that has been read
      from the data source, but has not yet been processed. }
    UnreadMarker: Integer;
    { Links to decompression subobjects (methods, private variables of modules) }
    Master: PRJpegDecompMaster;
    Main: PRJpegDMainController;
    Coef: PRJpegDCoefController;
    Post: PRJpegDPosController;
    InputCtl: PRJpegInputController;
    Marker: PRJpegMarkerReader;
    Entropy: PRJpegEntropyDecoder;
    IDct: PRJpegInverseDct;
    Upsample: PRJpegUpsampler;
    CConvert: PRJpegColorDeconverter;
    CQuantize: PRJpegColorQuantizer;
  end;

procedure jpeg_create_compress(cinfo: PRJpegCompressStruct); cdecl;
procedure jpeg_CreateCompress(cinfo: PRJpegCompressStruct; version: Integer; structsize: size_t); cdecl; external;
procedure jpeg_create_decompress(cinfo: PRJpegDecompressStruct); cdecl;
procedure jpeg_CreateDecompress(cinfo: PRJpegDecompressStruct; version: Integer; structsize: size_t); cdecl; external;
procedure jpeg_abort(cinfo: PRJpegCommonStruct); cdecl; external;
procedure jpeg_set_defaults(cinfo: PRJpegCompressStruct); cdecl; external;
procedure jpeg_set_colorspace(cinfo: PRJpegCompressStruct; colorspace: J_COLOR_SPACE); cdecl; external;
procedure jpeg_set_quality(cinfo: PRJpegCompressStruct; quality: Integer; force_baseline: Byte); cdecl; external;
procedure jpeg_suppress_tables(cinfo: PRJpegCompressStruct; suppress: Byte); cdecl; external;
procedure jpeg_start_compress(cinfo: PRJpegCompressStruct; write_all_tables: Byte); cdecl; external;
function  jpeg_write_scanlines(cinfo: PRJpegCompressStruct; scanlines: PPointer; num_lines: Cardinal): Cardinal; cdecl; external;
function  jpeg_write_raw_data(cinfo: PRJpegCompressStruct; data: Pointer; num_lines: Cardinal): Cardinal; cdecl; external;
procedure jpeg_finish_compress(cinfo: PRJpegCompressStruct); cdecl; external;
procedure jpeg_write_tables(cinfo: PRJpegCompressStruct); cdecl; external;
function  jpeg_read_header(cinfo: PRJpegDecompressStruct; require_image: Boolean): Integer; cdecl; external;
function  jpeg_start_decompress(cinfo: PRJpegDecompressStruct): Boolean; cdecl; external;
function  jpeg_read_scanlines(cinfo: PRJpegDecompressStruct; scanlines: Pointer; max_lines: Cardinal): Cardinal; cdecl; external;
function  jpeg_read_raw_data(cinfo: PRJpegDecompressStruct; data: Pointer; max_lines: Cardinal): Cardinal; cdecl; external;
function  jpeg_finish_decompress(cinfo: PRJpegDecompressStruct): Boolean; cdecl; external;
procedure jpeg_destroy(cinfo: PRJpegCommonStruct); cdecl; external;
function  jpeg_std_error(err: PRJpegErrorMgr): Pointer; cdecl; external;
function  jpeg_resync_to_restart(cinfo: PRJpegDecompressStruct; desired: Integer): Byte; cdecl; external;

{* Destruction of JPEG compression objects *}
procedure jpeg_destroy_compress(cinfo: PRJpegDecompressStruct); cdecl; external;
procedure jpeg_destroy_decompress(cinfo: PRJpegDecompressStruct); cdecl; external;


type
  ELibJpegError = class(Exception);

  // Forward declarations of default error routines.
  procedure JpegError(cinfo: PRJpegCommonStruct); cdecl;
  procedure EmitMessage(cinfo: PRJpegCommonStruct; msg_level: Integer); cdecl;
  procedure OutputMessage(cinfo: PRJpegCommonStruct); cdecl;
  procedure FormatMessage(cinfo: PRJpegCommonStruct; buffer: Pointer); cdecl;
  procedure ResetErrorMgr(cinfo: PRJpegCommonStruct); cdecl;
  // Our error string formatting routine:
  function GetMessage(cinfo: PRJpegCommonStruct): string;

type
  // A way to intercept emitted messages and fatal error messages
  TMessageInterceptor = procedure(const AMessage: string; const AMessageLevel: Integer);

// Returns previous handler or nil
function SetMessageInterceptor(AMessageInterceptor: TMessageInterceptor): TMessageInterceptor;

const
  DefaultErrorManager: RJpegErrorMgr = (
    ErrorExit: JpegError;
    EmitMessage: EmitMessage;
    OutputMessage: OutputMessage;
    FormatMessage: FormatMessage;
    ResetErrorMgr: ResetErrorMgr;
  );

implementation

{$IFNDEF FPC}
uses
  LibStub;
{$ENDIF}

var
  MessageInterceptor: TMessageInterceptor = nil;

// Returns previous handler or nil
function SetMessageInterceptor(AMessageInterceptor: TMessageInterceptor): TMessageInterceptor;
begin
  Result := @MessageInterceptor;
  MessageInterceptor := @AMessageInterceptor;
end;

{ jb I prefer the method used by both Mike Lischke's JPG.pas and Delphi's own jpeg unit
  for handling jpeg's error exit over LibJpegDelphi's because the latter needs
  a change in libjpeg's jerror.c source code. Since the less changes needed to
  the original source means easier updating, we choose the first method for handling
  jpeg errors.
  On second thought. It seems we need to do a lot of extra handling here then,
  and also need to define all error messages from jpeg here too,
  so we leave that for now. In the future we do need to have a look at it though.
  Without source changes would be easier and if there is a way to get the
  message strings from jpeg that would be even better.
}

procedure jpeg_error_exit_raise(cinfo: PRJpegCommonStruct); cdecl;
{$IFDEF FPC} public name '_jpeg_error_exit_raise'; {$ENDIF}
begin
  raise ELibJpegError.CreateFmt('LibJpeg: unrecoverable error %d', [cinfo.Err.MsgCode]);
end;

function GetMessage(cinfo: PRJpegCommonStruct): string;
var
  Template: string;
begin
  if (cinfo.err.MsgCode >= 0) and (cinfo.err.MsgCode <= cinfo.err.LastJpegMessage) then begin
    Template := cinfo.Err.JpegMessageTable[cinfo.err.MsgCode];
    if Pos('%s', Template) > 0 then
      Result := Format(Template, [cinfo.err.MsgParm.MsgParmS])
    else
      with cinfo.err.MsgParm do
        Result := Format(Template, [MsgParmI[0], MsgParmI[1], MsgParmI[2], MsgParmI[3], MsgParmI[4], MsgParmI[5], MsgParmI[6], MsgParmI[7]]);
  end
  else begin
    Result := Format('JpegLib: Unknown error code %d', [cinfo.err.MsgCode]);
  end;
end;

procedure JpegError(cinfo: PRJpegCommonStruct); cdecl;
var
  ErrMsg: string;
begin
  ErrMsg := GetMessage(cinfo);
  if Assigned(@MessageInterceptor) then
    MessageInterceptor(ErrMsg, -2);
  raise ELibJpegError.Create(ErrMsg);
end;

procedure EmitMessage(cinfo: PRJpegCommonStruct; msg_level: Integer); cdecl;
{$ifopt D+} // For debugging only.
var
  ErrMsg: string;
begin
  ErrMsg := GetMessage(cinfo);
  if Assigned(@MessageInterceptor) then
    MessageInterceptor(ErrMsg, msg_level);
  OutputDebugString(PChar(ErrMsg));
end;
{$else}
begin
  if Assigned(@MessageInterceptor) then
    MessageInterceptor(GetMessage(cinfo), msg_level);
end;
{$endif D+}

procedure OutputMessage(cinfo: PRJpegCommonStruct); cdecl;
begin
end;

procedure FormatMessage(cinfo: PRJpegCommonStruct; buffer: Pointer); cdecl;
begin
end;

procedure ResetErrorMgr(cinfo: PRJpegCommonStruct); cdecl;
begin
  cinfo^.err^.NumWarnings := 0;
  cinfo^.err^.MsgCode := 0;
end;

(*****)

procedure jpeg_create_compress(cinfo: PRJpegCompressStruct); cdecl;
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION, SizeOf(RJpegCompressStruct));
end;

procedure jpeg_create_decompress(cinfo: PRJpegDecompressStruct); cdecl;
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION, SizeOf(RJpegDecompressStruct));
end;
{
function  jpeg_get_small(cinfo: PRJpegCommonStruct; sizeofobject: Cardinal): Pointer; cdecl; external;
function  jpeg_get_large(cinfo: PRJpegCommonStruct; sizeofobject: Cardinal): Pointer; cdecl; external;
function  jpeg_mem_available(cinfo: PRJpegCommonStruct; min_bytes_needed: Integer; max_bytes_needed: Integer; already_allocated: Integer): Integer; cdecl; external;
procedure jpeg_open_backing_store(cinfo: PRJpegCommonStruct; info: Pointer; total_bytes_needed: Integer); cdecl; external;
procedure jpeg_free_large(cinfo: PRJpegCommonStruct; objectt: Pointer; sizeofobject: Cardinal); cdecl; external;
procedure jpeg_free_small(cinfo: PRJpegCommonStruct; objectt: Pointer; sizeofobject: Cardinal); cdecl; external;
procedure jpeg_mem_term(cinfo: PRJpegCommonStruct); cdecl; external;
function  jpeg_mem_init(cinfo: PRJpegCommonStruct): Integer; cdecl; external;
procedure jinit_memory_mgr(cinfo: PRJpegCommonStruct); cdecl; external;
function  jpeg_alloc_huff_table(cinfo: PRJpegCommonStruct): Pointer; cdecl; external;
function  jpeg_alloc_quant_table(cinfo: PRJpegCommonStruct): Pointer; cdecl; external;
function  jdiv_round_up(a: Integer; b: Integer): Integer; cdecl; external;
procedure jcopy_sample_rows(input_array: Pointer; source_row: Integer; output_array: Pointer; dest_row: Integer; num_rows: Integer;
               num_cols: Cardinal); cdecl; external;
function  jround_up(a: Integer; b: Integer): Integer; cdecl; external;
procedure jcopy_block_row(input_row: Pointer; output_row: Pointer; num_blocks: Cardinal); cdecl; external;
}
{$IFNDEF FPC}
{$L jmemnobs.obj}
{$L jmemmgr.obj}
{$L jcomapi.obj}
{$L jerror.obj}
{$L jcapimin.obj}
{$L jcmarker.obj}
{$L jutils.obj}
{$L jdapimin.obj}
{$L jdmarker.obj}
{$L jdinput.obj}
{$L jcparam.obj}
{$L jcapistd.obj}
{$L jcinit.obj}
{$L jcmaster.obj}
{$L jccolor.obj}
{$L jcsample.obj}
{$L jcprepct.obj}
{$L jcdctmgr.obj}
{$L jcphuff.obj}
{$L jchuff.obj}
{$L jccoefct.obj}
{$L jcmainct.obj}
{$L jfdctint.obj}
{$L jfdctfst.obj}
{$L jfdctflt.obj}
{$L jdapistd.obj}
{$L jdmaster.obj}
{$L jquant1.obj}
{$L jquant2.obj}
{$L jdmerge.obj}
{$L jdcolor.obj}
{$L jdsample.obj}
{$L jdpostct.obj}
{$L jddctmgr.obj}
{$L jdphuff.obj}
{$L jdhuff.obj}
{$L jdcoefct.obj}
{$L jdmainct.obj}
{$L jidctred.obj}
{$L jidctint.obj}
{$L jidctfst.obj}
{$L jidctflt.obj}
{$ELSE}
  // fpc
  {$IFDEF MSWINDOWS}
    {$IFNDEF CPU64}
      {.$LINKLIB libcrtdll} // _malloc and _free
      {$LINKLIB libmsvcrt.a}
      {.$LINKLIB libkernel32.a}
    {$ELSE}
      {$LINKLIB libmsvcrt.a}
      {$LINKLIB libkernel32.a}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
    Todo...
  {$ENDIF}
  {$LINKLIB libjpeg.a}
{$ENDIF}

end.

