unit LibJpeg;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is JPG.pas, released November 1, 1999.
//
// The initial developer of the original code is Dipl. Ing. Mike Lischke (Pleißa, Germany, www.delphi-gems.com),
//
// Portions created by Dipl. Ing. Mike Lischke are
// Copyright (C) 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
//
// Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
// All Rights Reserved.
// -----------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex

{$IFNDEF FPC}
  {$Include Compilers.inc}

  {$TYPEDADDRESS OFF}
  {$Z4}      // enum size = dword

  // Align record structures to 8 byte boundaries.
  {$ifdef COMPILER_7_UP}
    {$Align 8}
  {$else}
    {$Align On}
  {$endif COMPILER_7_UP}

  {$ifdef COMPILER_7_UP}
    // For some things to work we need code, which is classified as being unsafe for .NET.
    // We switch off warnings about that fact. We know it and we accept it.
    {$warn UNSAFE_TYPE off}
    {$warn UNSAFE_CAST off}
    {$warn UNSAFE_CODE off}
  {$endif COMPILER_7_UP}
{$ELSE}
  // FPC
  {$mode delphi}
  {$Z4} // enum size = dword
  {$A8} // Align record structures to 8 byte boundaries.
{$ENDIF}

interface

uses
  SysUtils, Classes,
  C_Types;

// Our Delphi compiled libjpeg currently uses a 1 byte Boolean while our
// Fpc version uses a 4 byte LongBool.
// TODO: See if we can get the Delphi version to use 4 bytes too in the future.
// For now we use an ifdef to define the type of boolean to use.
type
{$IFDEF FPC}
  JPEG_BOOLEAN = LongBool;
{$ELSE}
  JPEG_BOOLEAN = Boolean;
{$ENDIF}

// ---------- jmorecfg.h ----------

const
  {*
   * Define BITS_IN_JSAMPLE as either
   *   8   for 8-bit sample values (the usual setting)
   *   12  for 12-bit sample values
   * Only 8 and 12 are legal data precisions for lossy JPEG according to the
   * JPEG standard, and the IJG code does not support anything else!
   * We do not support run-time selection of data precision, sorry.
   *}

  {$DEFINE BITS_IN_JSAMPLE_8}
  {$IFDEF BITS_IN_JSAMPLE_8}
  BITS_IN_JSAMPLE       =  8;	// use 8 or 12

type
  JSAMPLE = Byte;
  JSAMPLE_ptr = ^JSAMPLE;

const
  MAXJSAMPLE            = 255;
  CENTERJSAMPLE         = 128;
  {$ENDIF BITS_IN_JSAMPLE_8}

  {$IFNDEF BITS_IN_JSAMPLE_8}
  {$DEFINE BITS_IN_JSAMPLE_12}
  {$ENDIF}
  {$IFDEF BITS_IN_JSAMPLE_12}
type
  {* JSAMPLE should be the smallest type that will hold the values 0..4095.
   * On nearly all machines "short" will do nicely.
   *}
  JSAMPLE = Word;
  JSAMPLE_ptr = ^JSAMPLE;

const
  MAXJSAMPLE            = 4095;
  CENTERJSAMPLE         = 2048;
  {$ENDIF BITS_IN_JSAMPLE_12}

const
  {/*
   * Maximum number of components (color channels) allowed in JPEG image.
   * To meet the letter of the JPEG spec, set this to 255.  However, darn
   * few applications need more than 4 channels (maybe 5 for CMYK + alpha
   * mask).  We recommend 10 as a reasonable compromise; use 4 if you are
   * really short on memory.  (Each allowed component costs a hundred or so
   * bytes of storage, whether actually used in an image or not.)
   *}
  MAX_COMPONENTS        = 10; // maximum number of image components (color channels)

type
  {* Representation of a DCT frequency coefficient.
   * This should be a signed value of at least 16 bits; "short" is usually OK.
   * Again, we allocate large arrays of these, but you can change to int
   * if you have memory to burn and "short" is really slow.
   *}
  JCOEF = SmallInt;
  PJCOEF = ^JCOEF;

  {* Compressed datastreams are represented as arrays of JOCTET.
   * These must be EXACTLY 8 bits wide, at least once they are written to
   * external storage.  Note that when using the stdio data source/destination
   * managers, this is also the data type passed to fread/fwrite.
   *}
  JOCTET = Byte;
  JOCTET_ptr = ^JOCTET;

  {* Datatype used for image dimensions.  The JPEG standard only supports
   * images up to 64K*64K due to 16-bit fields in SOF markers.  Therefore
   * "unsigned int" is sufficient on all machines.  However, if you need to
   * handle larger images and you don't mind deviating from the spec, you
   * can change this datatype.
   *}
  JDIMENSION = Cardinal;

const
  JPEG_MAX_DIMENSION    = 65500;  // a tad under 64K to prevent overflows


// ---------- jpeglib.h ----------

  // Version ID for the JPEG library.
  JPEG_LIB_VERSION      = 62; // Version 6b


  {* Various constants determining the sizes of things.
   * All of these are specified by the JPEG standard, so don't change them
   * if you want to be compatible.
   *}
  DCTSIZE               =  8; // The basic DCT block is 8x8 samples
  DCTSIZE2              = 64; // DCTSIZE squared; # of elements in a block
  NUM_QUANT_TBLS        =  4; // Quantization tables are numbered 0..3
  NUM_HUFF_TBLS         =  4; // Huffman tables are numbered 0..3
  NUM_ARITH_TBLS        = 16; // Arith-coding tables are numbered 0..15
  MAX_COMPS_IN_SCAN     =  4; // JPEG limit on # of components in one scan
  MAX_SAMP_FACTOR       =  4; // JPEG limit on sampling factors

  C_MAX_BLOCKS_IN_MCU   = 10; // compressor's limit on blocks per MCU
  D_MAX_BLOCKS_IN_MCU   = 10; // decompressor's limit on blocks per MCU

  // Error handler
  JMSG_LENGTH_MAX       = 200; // recommended size of format_message buffer
  JMSG_STR_PARM_MAX     = 80;

  // Memory manager
  JPOOL_PERMANENT = 0; // lasts until master record is destroyed
  JPOOL_IMAGE	  = 1; // lasts until done with image/datastream
  JPOOL_NUMPOOLS  = 2;


type
  {* Data structures for images (arrays of samples and of DCT coefficients).
   * On 80x86 machines, the image arrays are too big for near pointers,
   * but the pointer arrays can fit in near memory.
   *}
  JSAMPROW = ^JSAMPLE;        // ptr to one image row of pixel samples.
  JSAMPARRAY = ^JSAMPROW;     // ptr to some rows (a 2-D sample array)
  JSAMPIMAGE = ^JSAMPARRAY;   // a 3-D sample array: top index is color

  {
  JSAMPROW_ptr = ^JSAMPROW;

  jTSample = 0..(MaxInt div SIZEOF(JSAMPLE)) - 1;
  JSAMPLE_array = array[jTSample] of JSAMPLE;

  jTRow = 0..(MaxInt div SIZEOF(JSAMPROW)) - 1;
  JSAMPROW_array = array[jTRow] of JSAMPROW;
  JSAMParray = ^JSAMPROW_array; // ptr to some rows (a 2-D sample array)

  jTarray = 0..(MaxInt div SIZEOF(JSAMParray))-1;
  JSAMP_array = array[jTarray] of JSAMParray;
  JSAMPIMAGE = ^JSAMP_array;    // a 3-D sample array: top index is color}

  JBLOCK = array [0..DCTSIZE2-1] of JCOEF;	// one block of coefficients

  JBLOCKROW = ^JBLOCK;                      // pointer to one row of coefficient blocks
  JBLOCKARRAY = ^JBLOCKROW;                 // a 2-D array of coefficient blocks
  JBLOCKIMAGE = ^JBLOCKARRAY;               // a 3-D array of coefficient blocks

  JCOEFPTR = ^JCOEF;                        // useful in a couple of places


  // DCT coefficient quantization tables.
  JQUANT_TBL_ptr = ^JQUANT_TBL;
  JQUANT_TBL = record
    // This array gives the coefficient quantizers in natural array order
    // (not the zigzag order in which they are stored in a JPEG DQT marker).
    // CAUTION: IJG versions prior to v6a kept this array in zigzag order.
    quantval: array[0..DCTSIZE2 - 1] of Word; // quantization step for each coefficient
    // This field is used only during compression.  It's initialized FALSE when
    // the table is created, and set TRUE when it's been output to the file.
    // You could suppress output of a table by setting this to TRUE.
    // (See jpeg_suppress_tables for an example.)
    sent_table: JPEG_BOOLEAN; // TRUE when table has been output
  end;

  // Huffman coding tables.
  JHUFF_TBL_ptr = ^JHUFF_TBL;
  JHUFF_TBL = record
    // These two fields directly represent the contents of a JPEG DHT marker.
    bits: array[0..16] of Byte;	    // bits[k] = # of symbols with codes of length k bits; bits[0] is unused.
    huffval: array[0..255] of Byte; // The symbols, in order of incr code length.
    // This field is used only during compression.  It's initialized FALSE when
    // the table is created, and set TRUE when it's been output to the file.
    // You could suppress output of a table by setting this to TRUE.
    // (See jpeg_suppress_tables for an example.)
    sent_table: JPEG_BOOLEAN;           // TRUE when table has been output.
  end;


  // Basic info about one component (color channel).
  jpeg_component_info_ptr = ^jpeg_component_info;
  jpeg_component_info = record
    // These values are fixed over the whole image.
    // For compression, they must be supplied by parameter setup;
    // for decompression, they are read from the SOF marker.
    component_id: Integer;    // identifier for this component (0..255)
    component_index: Integer; // its index in SOF or cinfo->comp_info[]
    h_samp_factor: Integer;   // horizontal sampling factor (1..4) */
    v_samp_factor: Integer;   // vertical sampling factor (1..4) */
    quant_tbl_no: Integer;    // quantization table selector (0..3) */
    // These values may vary between scans.
    // For compression, they must be supplied by parameter setup;
    // for decompression, they are read from the SOS marker.
    // The decompressor output side may not use these variables.
    dc_tbl_no: Integer;       // DC entropy table selector (0..3)
    ac_tbl_no: Integer;       // AC entropy table selector (0..3)

    // Remaining fields should be treated as private by applications.

    // These values are computed during compression or decompression startup:
    // Component's size in DCT blocks.
    // Any dummy blocks added to complete an MCU are not counted; therefore
    // these values do not depend on whether a scan is interleaved or not.
    width_in_blocks: JDIMENSION;
    height_in_blocks: JDIMENSION;
    // Size of a DCT block in samples.  Always DCTSIZE for compression.
    // For decompression this is the size of the output from one DCT block,
    // reflecting any scaling we choose to apply during the IDCT step.
    // Values of 1,2,4,8 are likely to be supported.  Note that different
    // components may receive different IDCT scalings.

    DCT_scaled_size: Integer;
    // The downsampled dimensions are the component's actual, unpadded number
    // of samples at the main buffer (preprocessing/compression interface), thus
    // downsampled_width = ceil(image_width * Hi/Hmax)
    // and similarly for height.  For decompression, IDCT scaling is included, so
    // downsampled_width = ceil(image_width * Hi/Hmax * DCT_scaled_size/DCTSIZE)
    downsampled_width: JDIMENSION;  // actual width in samples
    downsampled_height: JDIMENSION; // actual height in samples
    // This flag is used only for decompression.  In cases where some of the
    // components will be ignored (eg grayscale output from YCbCr image),
    // we can skip most computations for the unused components.
    component_needed: JPEG_BOOLEAN;     // do we need the value of this component?

    // These values are computed before starting a scan of the component.
    // The decompressor output side may not use these variables.
    MCU_width: Integer;              // number of blocks per MCU, horizontally
    MCU_height: Integer;             // number of blocks per MCU, vertically
    MCU_blocks: Integer;             // MCU_width * MCU_height
    MCU_sample_width: Integer;       // MCU width in samples, MCU_width*DCT_scaled_size
    last_col_width: Integer;         // # of non-dummy blocks across in last MCU
    last_row_height: Integer;        // # of non-dummy blocks down in last MCU

    // Saved quantization table for component; NULL if none yet saved.
    // See jdinput.c comments about the need for this information.
    // This field is currently used only for decompression.
    quant_table: JQUANT_TBL_ptr;

    // Private per-component storage for DCT or IDCT subsystem.
    dct_table: Pointer;
  end;

  // The script for encoding a multiple-scan file is an array of these:

  jpeg_scan_info_ptr = ^jpeg_scan_info;
  jpeg_scan_info = record
    comps_in_scan: Integer;		// number of components encoded in this scan
    component_index: array [0..MAX_COMPS_IN_SCAN-1] of Integer; // their SOF/comp_info[] indexes
    Ss, Se: Integer;			// progressive JPEG spectral selection parms
    Ah, Al: Integer;			// progressive JPEG successive approx. parms
  end;

  // The decompressor can save APPn and COM markers in a list of these:
  jpeg_saved_marker_ptr = ^jpeg_marker_struct;
  jpeg_marker_struct = record
    next: jpeg_saved_marker_ptr; // next in list, or NULL
    marker: Byte;                // marker code: JPEG_COM, or JPEG_APP0 + n
    original_length: Cardinal;   // # bytes of data in the file
    data_length: Cardinal;       // # bytes of data saved at data[]
    data: JOCTET_ptr;            // the data contained in the marker
    // the marker length word is not counted in data_length or original_length
  end;


  // Known color spaces.
  J_COLOR_SPACE = (
    JCS_UNKNOWN,   // error/unspecified
    JCS_GRAYSCALE, // monochrome
    JCS_RGB,       // red/green/blue
    JCS_YCbCr,     // Y/Cb/Cr (also known as YUV)
    JCS_CMYK,      // C/M/Y/K
    JCS_YCCK       // Y/Cb/Cr/K
  );

  // DCT/IDCT algorithm options.
  J_DCT_METHOD = (
    JDCT_ISLOW,	// slow but accurate Integer algorithm
    JDCT_IFAST,	// faster, less accurate Integer method
    JDCT_FLOAT	// floating-point: accurate, fast on fast HW (Pentium)
  );

  // Dithering options for decompression.
  J_DITHER_MODE = (
    JDITHER_NONE,    // no dithering
    JDITHER_ORDERED, // simple ordered dither
    JDITHER_FS       // Floyd-Steinberg error diffusion dither
  );

  // Define these here because they are needed by jpeg_common_struct
  jpeg_error_mgr_ptr = ^jpeg_error_mgr;
  jpeg_memory_mgr_ptr = ^jpeg_memory_mgr;
  jpeg_progress_mgr_ptr = ^jpeg_progress_mgr;

  // Common fields between JPEG compression and decompression master structs.
  {* Routines that are to be used by both halves of the library are declared
   * to receive a pointer to this structure.  There are no actual instances of
   * jpeg_common_struct, only of jpeg_compress_struct and jpeg_decompress_struct.
   *}
  jpeg_common_struct = record
    err: jpeg_error_mgr_ptr;         // Error handler module
    mem: jpeg_memory_mgr_ptr;        // Memory manager module
    progress: jpeg_progress_mgr_ptr; // Progress monitor, or NIL if none
    client_data: Pointer;            // Available for use by application
    is_decompressor: JPEG_BOOLEAN;       // so common code can tell which is which
    global_state: Integer;           // for checking call sequence validity
  end;

  j_common_ptr     = ^jpeg_common_struct;
  j_compress_ptr   = ^jpeg_compress_struct;
  j_decompress_ptr = ^jpeg_decompress_struct;

  // Needed by jpeg_compress_struct
  jpeg_destination_mgr_ptr = ^jpeg_destination_mgr;
  // Needed by jpeg_decompress_struct
  jpeg_source_mgr_ptr = ^jpeg_source_mgr;


  // Master record for a compression instance
  jpeg_compress_struct = record
    // Fields shared with jpeg_decompress_struct
    err: jpeg_error_mgr_ptr;         // Error handler module
    mem: jpeg_memory_mgr_ptr;        // Memory manager module
    progress: jpeg_progress_mgr_ptr; // Progress monitor, or NIL if none
    client_data: Pointer;            // Available for use by application
    is_decompressor: JPEG_BOOLEAN;       // so common code can tell which is which
    global_state: Integer;           // for checking call sequence validity
    // End of shared fields

    dest: jpeg_destination_mgr_ptr; // Destination for compressed data

    // Description of source image --- these fields must be filled in by
    // outer application before starting compression.  in_color_space must
    // be correct before you can even call jpeg_set_defaults().

    image_width: JDIMENSION;         // input image width
    image_height: JDIMENSION;        // input image height
    input_components: Integer;       // # of color components in input image
    in_color_space: J_COLOR_SPACE;   // colorspace of input image
    input_gamma: double;             // image gamma of input image

    {* Compression parameters --- these fields must be set before calling
     * jpeg_start_compress().  We recommend calling jpeg_set_defaults() to
     * initialize everything to reasonable defaults, then changing anything
     * the application specifically wants to change.  That way you won't get
     * burnt when new parameters are added.  Also note that there are several
     * helper routines to simplify changing parameters.
     *}

    data_precision: Integer;         // bits of precision in image data
    num_components: Integer;         // # of color components in JPEG image
    jpeg_color_space: J_COLOR_SPACE; // colorspace of JPEG image
    comp_info: jpeg_component_info_ptr; // comp_info[i] describes component that appears i'th in SOF

    // ptrs to coefficient quantization tables, or NULL if not defined
    quant_tbl_ptrs: array [0..NUM_QUANT_TBLS - 1] of JQUANT_TBL_ptr;
    // ptrs to Huffman coding tables, or NULL if not defined
    dc_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS - 1] of JHUFF_TBL_ptr;
    ac_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS - 1] of JHUFF_TBL_ptr;

    arith_dc_L: array [0..NUM_ARITH_TBLS - 1] of Byte; // L values for DC arith-coding tables
    arith_dc_U: array [0..NUM_ARITH_TBLS - 1] of Byte; // U values for DC arith-coding tables
    arith_ac_K: array [0..NUM_ARITH_TBLS - 1] of Byte; // Kx values for AC arith-coding tables

    num_scans: Integer;		           // # of entries in scan_info array
    scan_info: jpeg_scan_info_ptr;   // script for multi-scan file, or NIL
    {* The default value of scan_info is NULL, which causes a single-scan
     * sequential JPEG file to be emitted.  To create a multi-scan file,
     * set num_scans and scan_info to point to an array of scan definitions.
     *}

    raw_data_in: JPEG_BOOLEAN;           // TRUE=caller supplies downsampled data
    arith_code: JPEG_BOOLEAN;            // TRUE=arithmetic coding, FALSE=Huffman
    optimize_coding: JPEG_BOOLEAN;       // TRUE=optimize entropy encoding parms
    CCIR601_sampling: JPEG_BOOLEAN;      // TRUE=first samples are cosited
    smoothing_factor: Integer;       // 1..100, or 0 for no input smoothing
    dct_method: J_DCT_METHOD;        // DCT algorithm selector

    {* The restart interval can be specified in absolute MCUs by setting
     * restart_interval, or in MCU rows by setting restart_in_rows
     * (in which case the correct restart_interval will be figured
     * for each scan).
     *}
    restart_interval: Cardinal;      // MCUs per restart, or 0 for no restart
    restart_in_rows: Integer;        // if > 0, MCU rows per restart interval

    // Parameters controlling emission of special markers.
    write_JFIF_header: JPEG_BOOLEAN;     // should a JFIF marker be written?
    JFIF_major_version: Byte;	       // What to write for the JFIF version number
    JFIF_minor_version: Byte;

    // These three values are not used by the JPEG code, merely copied
    // into the JFIF APP0 marker.  density_unit can be 0 for unknown,
    // 1 for dots/inch, or 2 for dots/cm.  Note that the pixel aspect
    // ratio is defined by X_density/Y_density even when density_unit=0.
    density_unit: Byte;              // JFIF code for pixel size units
    X_density: Word;                 // Horizontal pixel density
    Y_density: Word;                 // Vertical pixel density
    write_Adobe_marker: JPEG_BOOLEAN;    // should an Adobe marker be written?

    // State variable: index of next scanline to be written to
    // jpeg_write_scanlines().  Application may use this to control its
    // processing loop, e.g., 'while (next_scanline < image_height)'.
    next_scanline: JDIMENSION;       // 0 .. image_height-1

    // Remaining fields are known throughout compressor, but generally
    // should not be touched by a surrounding application.

    //These fields are computed during compression startup
    progressive_mode: JPEG_BOOLEAN;      // TRUE if scan script uses progressive mode
    max_h_samp_factor: Integer;      // largest h_samp_factor
    max_v_samp_factor: Integer;      // largest v_samp_factor

    total_iMCU_rows: JDIMENSION;     // # of iMCU rows to be input to coef ctlr
    {* The coefficient controller receives data in units of MCU rows as defined
     * for fully interleaved scans (whether the JPEG file is interleaved or not).
     * There are v_samp_factor * DCTSIZE sample rows of each component in an
     * "iMCU" (interleaved MCU) row. }

    {* These fields are valid during any one scan.
     * They describe the components and MCUs actually appearing in the scan. }
    comps_in_scan: Integer;          // # of JPEG components in this scan
    cur_comp_info: array [0..MAX_COMPS_IN_SCAN - 1] of jpeg_component_info_ptr;
    // *cur_comp_info[i] describes component that appears i'th in SOS

    MCUs_per_row: JDIMENSION;        // # of MCUs across the image
    MCU_rows_in_scan: JDIMENSION;    // # of MCU rows in the image
    blocks_in_MCU: Integer;          // # of DCT blocks per MCU
    MCU_membership: array [0..C_MAX_BLOCKS_IN_MCU - 1] of Integer;
    // MCU_membership[i] is index in cur_comp_info of component owning
    // i'th block in an MCU

    // progressive JPEG parameters for scan
    Ss: Integer;
    Se: Integer;
    Ah: Integer;
    Al: Integer;

    // Links to compression subobjects (methods and private variables of modules)
    master: Pointer;
    main: Pointer;
    prep: Pointer;
    coef: Pointer;
    marker: Pointer;
    cconvert: Pointer;
    downsample: Pointer;
    fdct: Pointer;
    entropy: Pointer;

    script_space: jpeg_scan_info_ptr; // workspace for jpeg_simple_progression
    script_space_size: Integer;
  end;


  // Master record for a decompression instance
  jpeg_decompress_struct = record
    // Fields shared with jpeg_decompress_struct
    err: jpeg_error_mgr_ptr;           // Error handler module
    mem: jpeg_memory_mgr_ptr;          // Memory manager module
    progress: jpeg_progress_mgr_ptr;   // Progress monitor, or NIL if none
    client_data: Pointer;              // Available for use by application
    is_decompressor: JPEG_BOOLEAN;         // so common code can tell which is which
    global_state: Integer;             // for checking call sequence validity
    // End of shared fields

    // Source of compressed data
    src: jpeg_source_mgr_ptr;

    // Basic description of image --- filled in by _jpeg_read_header().
    // Application may inspect these values to decide how to process image.
    image_width: JDIMENSION;	         // nominal image width (from SOF marker)
    image_height: JDIMENSION;	         // nominal image height
    num_components: Integer;	         // # of color components in JPEG image
    jpeg_color_space: J_COLOR_SPACE;   // colorspace of JPEG image

    // Decompression processing parameters --- these fields must be set before
    // calling _jpeg_start_decompress().  Note that _jpeg_read_header() initializes
    // them to default values.
    out_color_space: J_COLOR_SPACE;    // colorspace for output

    // fraction by which to scale image
    scale_num: Cardinal;
    scale_denom: Cardinal;

    output_gamma: Double;	             // image gamma wanted in output

    buffered_image: JPEG_BOOLEAN;          // TRUE=multiple output passes
    raw_data_out: JPEG_BOOLEAN;            // TRUE=downsampled data wanted

    dct_method: J_DCT_METHOD;          // IDCT algorithm selector
    do_fancy_upsampling: JPEG_BOOLEAN;     // TRUE = apply fancy upsampling
    do_block_smoothing: JPEG_BOOLEAN;      // TRUE = apply interblock smoothing

    quantize_colors: JPEG_BOOLEAN;         // TRUE=colormapped output wanted
    // the following are ignored if not quantize_colors:
    dither_mode: J_DITHER_MODE;        // type of color dithering to use
    two_pass_quantize: JPEG_BOOLEAN;       // TRUE = use two-pass color quantization
    desired_number_of_colors: Integer; // max # colors to use in created colormap
    // these are significant only in buffered-image mode:
    enable_1pass_quant: JPEG_BOOLEAN;      // enable future use of 1-pass quantizer
    enable_external_quant: JPEG_BOOLEAN;   // enable future use of external colormap
    enable_2pass_quant: JPEG_BOOLEAN;      // enable future use of 2-pass quantizer

    // Description of actual output image that will be returned to application.
    // These fields are computed by _jpeg_start_decompress().
    // You can also use _jpeg_calc_output_dimensions() to determine these values
    // in advance of calling _jpeg_start_decompress().

    output_width: JDIMENSION;          // scaled image width
    output_height: JDIMENSION;         // scaled image height
    out_color_components: Integer;     // # of color components in out_color_space
    output_components: Integer;        // # of color components returned
    // output_components is 1 (a colormap index) when quantizing colors;
    // otherwise it equals out_color_components.

    rec_outbuf_height: Integer;        // min recommended height of scanline buffer
    // If the buffer passed to jpeg_read_scanlines() is less than this many rows
    // high, space and time will be wasted due to unnecessary data copying.
    // Usually rec_outbuf_height will be 1 or 2, at most 4.

    // When quantizing colors, the output colormap is described by these fields.
    // The application can supply a colormap by setting colormap non-NULL before
    // calling _jpeg_start_decompress; otherwise a colormap is created during
    // _jpeg_start_decompress or jpeg_start_output.
    // The map has out_color_components rows and actual_number_of_colors columns.
    actual_number_of_colors: Integer;  // number of entries in use
    colormap: JSAMPARRAY;              // The color map as a 2-D pixel array

    // State variables: these variables indicate the progress of decompression.
    // The application may examine these but must not modify them.

    // Row index of next scanline to be read from jpeg_read_scanlines().
    // Application may use this to control its processing loop, e.g.,
    // "while (output_scanline < output_height)".
    output_scanline: JDIMENSION;       // 0 .. output_height - 1

    // Current input scan number and number of iMCU rows completed in scan.
    // These indicate the progress of the decompressor input side.
    input_scan_number: Integer;        // Number of SOS markers seen so far
    input_iMCU_row: JDIMENSION;        // Number of iMCU rows completed

    // The "output scan number" is the notional scan being displayed by the
    // output side.  The decompressor will not allow output scan/row number
    // to get ahead of input scan/row, but it can fall arbitrarily far behind.
    output_scan_number: Integer;       // Nominal scan number being displayed
    output_iMCU_row: JDIMENSION;       // Number of iMCU rows read

    // Current progression status. coef_bits[c][i] indicates the precision
    // with which component c's DCT coefficient i (in zigzag order) is known.
    // It is -1 when no data has yet been received, otherwise it is the point
    // transform (shift) value for the most recent scan of the coefficient
    // (thus, 0 at completion of the progression).
    // This pointer is NULL when reading a non-progressive file.
    coef_bits: Pointer; // (pointer to array [0..DCTSIZE2-1] of Integer) // -1 or current Al value for each coef

    // Internal JPEG parameters --- the application usually need not look at
    // these fields.  Note that the decompressor output side may not use
    // any parameters that can change between scans.

    // Quantization and Huffman tables are carried forward across input
    // datastreams when processing abbreviated JPEG datastreams.

    // ptrs to coefficient quantization tables, or NULL if not defined
    quant_tbl_ptrs: array [0..NUM_QUANT_TBLS - 1] of JQUANT_TBL_ptr;

    // ptrs to Huffman coding tables, or NULL if not defined
    dc_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS - 1] of JHUFF_TBL_ptr;
    ac_huff_tbl_ptrs: array [0..NUM_HUFF_TBLS - 1] of JHUFF_TBL_ptr;

    // These parameters are never carried across datastreams, since they
    // are given in SOF/SOS markers or defined to be reset by SOI.
    data_precision: Integer;           // bits of precision in image data
    comp_info: jpeg_component_info_ptr;// comp_info[i] describes component that appears i'th in SOF

    progressive_mode: JPEG_BOOLEAN;        // TRUE if SOFn specifies progressive mode
    arith_code: JPEG_BOOLEAN;              // TRUE = arithmetic coding, FALSE=Huffman

    arith_dc_L: array [0..NUM_ARITH_TBLS - 1] of Byte; // L values for DC arith-coding tables
    arith_dc_U: array [0..NUM_ARITH_TBLS - 1] of Byte; // U values for DC arith-coding tables
    arith_ac_K: array [0..NUM_ARITH_TBLS - 1] of Byte; // Kx values for AC arith-coding tables

    restart_interval: Cardinal;        // MCUs per restart interval, or 0 for no restart

    // These fields record data obtained from optional markers recognized by the JPEG library.
    saw_JFIF_marker: JPEG_BOOLEAN;          // TRUE iff a JFIF APP0 marker was found
    // Data copied from JFIF marker; only valid if saw_JFIF_marker is TRUE:
    JFIF_major_version: Byte;          // JFIF version number
    JFIF_minor_version: Byte;
    density_unit: Byte;                // JFIF code for pixel size units
    X_density: Word;                   // Horizontal pixel density
    Y_density: Word;                   // Vertical pixel density
    saw_Adobe_marker: JPEG_BOOLEAN;        // TRUE iff an Adobe APP14 marker was found
    Adobe_transform: Byte;             // Color transform code from Adobe marker

    CCIR601_sampling: JPEG_BOOLEAN;         // TRUE = first samples are cosited

    // Aside from the specific data retained from APPn markers known to the
    // library, the uninterpreted contents of any or all APPn and COM markers
    // can be saved in a list for examination by the application.
    marker_list: jpeg_saved_marker_ptr; // Head of list of saved markers

    // Remaining fields are known throughout decompressor, but generally
    // should not be touched by a surrounding application.

    // These fields are computed during decompression startup
    max_h_samp_factor: Integer;        // largest h_samp_factor
    max_v_samp_factor: Integer;        // largest v_samp_factor

    min_DCT_scaled_size: Integer;      // smallest DCT_scaled_size of any component

    total_iMCU_rows: JDIMENSION;       // # of iMCU rows in image
    // The coefficient controller's input and output progress is measured in
    // units of "iMCU" (interleaved MCU) rows.  These are the same as MCU rows
    // in fully interleaved JPEG scans, but are used whether the scan is
    // interleaved or not.  We define an iMCU row as v_samp_factor DCT block
    // rows of each component.  Therefore, the IDCT output contains
    // v_samp_factor*DCT_scaled_size sample rows of a component per iMCU row.

    sample_range_limit: JSAMPLE_ptr;   // table for fast range-limiting

    // These fields are valid during any one scan.
    // They describe the components and MCUs actually appearing in the scan.
    // Note that the decompressor output side must not use these fields.
    comps_in_scan: Integer;            // # of JPEG components in this scan
    cur_comp_info: array [0..MAX_COMPS_IN_SCAN - 1] of jpeg_component_info_ptr;
    // cur_comp_info[i] describes component that appears i'th in SOS

    MCUs_per_row: JDIMENSION;          // # of MCUs across the image
    MCU_rows_in_scan: JDIMENSION;      // # of MCU rows in the image

    blocks_in_MCU: Integer;            // # of DCT blocks per MCU
    MCU_membership: array [0..D_MAX_BLOCKS_IN_MCU - 1] of Integer;
    // MCU_membership[i] is index in cur_comp_info of component owning i'th block in an MCU.

    // progressive JPEG parameters for scan
    Ss: Integer;
    Se: Integer;
    Ah: Integer;
    Al: Integer;

    // This field is shared between entropy decoder and marker parser.
    // It is either zero or the code of a JPEG marker that has been
    // read from the data source, but has not yet been processed.
    unread_marker: Integer;

    // Links to decompression subobjects (methods, private variables of modules)
    master: Pointer;
    main: Pointer;
    coef: Pointer;
    post: Pointer;
    inputctl: Pointer;
    marker: Pointer;
    entropy: Pointer;
    idct: Pointer;
    upsample: Pointer;
    cconvert: Pointer;
    cquantize: Pointer;
  end;


  {* "Object" declarations for JPEG modules that may be supplied or called
   * directly by the surrounding application.
   * As with all objects in the JPEG library, these structs only define the
   * publicly visible methods and state variables of a module.  Additional
   * private fields may exist after the public ones.
   *}

   // Error handler object
   jpeg_error_mgr = record
     // Error exit handler: does not return to caller
     error_exit: procedure(cinfo: j_common_ptr); cdecl;
     // Conditionally emit a trace or warning message
     emit_message: procedure(cinfo: j_common_ptr; msg_level: Integer); cdecl;
     // Routine that actually outputs a trace or error message
     output_message: procedure(cinfo: j_common_ptr); cdecl;
     // Format a message string for the most recent JPEG error or message
     format_message: procedure(cinfo: j_common_ptr; buffer: PAnsiChar); cdecl;
     // Reset error state variables at start of a new image
     reset_error_mgr: procedure(cinfo: j_common_ptr); cdecl;

     // The message ID code and any parameters are saved here.
     // A message can have one string parameter or up to 8 int parameters.
     msg_code: Integer;
     msg_parm: record
       case Byte of
         0: (i: array [0..7] of Integer);
         1: (s: array [0..JMSG_STR_PARM_MAX - 1] of AnsiChar);
     end;

     // Standard state variables for error facility

     trace_level: Integer;             // max msg_level that will be displayed

     {* For recoverable corrupt-data errors, we emit a warning message,
      * but keep going unless emit_message chooses to abort.  emit_message
      * should count warnings in num_warnings.  The surrounding application
      * can check for bad data by seeing if num_warnings is nonzero at the
      * end of processing.
      *}
     num_warnings: long;               // number of corrupt-data warnings

     // These fields point to the table(s) of error message strings.
     // An application can change the table Pointer to switch to a different
     // message list (typically, to change the language in which errors are
     // reported).  Some applications may wish to add additional error codes
     // that will be handled by the JPEG library error mechanism; the second
     // table Pointer is used for this purpose.

     // First table includes all errors generated by JPEG library itself.
     // Error code 0 is reserved for a "no such error string" message.
     jpeg_message_table: PPAnsiChar;   // Library errors
     last_jpeg_message: Integer;       // Table contains strings 0..last_jpeg_message

     // Second table can be added by application (see cjpeg/djpeg for example).
     // It contains strings numbered first_addon_message..last_addon_message.
     addon_message_table: PPAnsiChar;  // Non-library errors
     first_addon_message: Integer;     // code for first string in addon table
     last_addon_message: Integer;      // code for last string in addon table
   end;

   // Progress monitor object
   jpeg_progress_mgr = record
     progress_monitor: procedure(const cinfo: j_common_ptr); cdecl;
     pass_counter: long;               // work units completed in this pass
     pass_limit: long;                 // total number of work units in this pass
     completed_passes: Integer;	       // passes completed so far
     total_passes: Integer;            // total number of passes expected
   end;

   // Data destination object for compression
   jpeg_destination_mgr = record
     next_output_byte: JOCTET_ptr;     // => next Byte to write in buffer
     free_in_buffer: size_t;           // # of Byte spaces remaining in buffer

     init_destination: procedure (cinfo: j_compress_ptr); cdecl;
     empty_output_buffer: function (cinfo: j_compress_ptr): JPEG_BOOLEAN; cdecl;
     term_destination: procedure (cinfo: j_compress_ptr); cdecl;
   end;

   // Data source object for decompression
   jpeg_source_mgr = record
     next_input_byte: JOCTET_ptr;      // => next Byte to read from buffer
     bytes_in_buffer: size_t;          // # of Bytes remaining in buffer

     init_source: procedure (cinfo: j_decompress_ptr); cdecl;
     fill_input_buffer: function (cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
     skip_input_data: procedure (cinfo: j_decompress_ptr; num_bytes: long); cdecl;
     resync_to_restart: function (cinfo: j_decompress_ptr; desired: Integer): JPEG_BOOLEAN; cdecl;
     term_source: procedure (cinfo: j_decompress_ptr); cdecl;
   end;

   {* Memory manager object.
    * Allocates "small" objects (a few K total), "large" objects (tens of K),
    * and "really big" objects (virtual arrays with backing store if needed).
    * The memory manager does not allow individual objects to be freed; rather,
    * each created object is assigned to a pool, and whole pools can be freed
    * at once.  This is faster and more convenient than remembering exactly what
    * to free, especially where malloc()/free() are not too speedy.
    * NB: alloc routines never return NULL.  They exit to error_exit if not
    * successful.
    *}

   jvirt_sarray_ptr = Pointer;
   jvirt_barray_ptr = Pointer;
   pjvirt_barray_ptr = PPointer;

   jpeg_memory_mgr = record
     // Method Pointers
     alloc_small: function (cinfo: j_common_ptr; pool_id: Integer; sizeofobject: size_t): Pointer; cdecl;
     alloc_large: function (cinfo: j_common_ptr; pool_id: Integer; sizeofobject: size_t): Pointer; cdecl;
     alloc_sarray: function (cinfo: j_common_ptr; pool_id: Integer; samplesperrow: JDIMENSION; numrows: JDIMENSION): JSAMPARRAY; cdecl;
     alloc_barray: function (cinfo: j_common_ptr; pool_id: Integer; blocksperrow: JDIMENSION; numrows: JDIMENSION): JBLOCKARRAY; cdecl;
     request_virt_sarray: function (cinfo: j_common_ptr; pool_id: Integer; pre_zero: JPEG_BOOLEAN;
       samplesperrow: JDIMENSION; numrows: JDIMENSION; maxaccess: JDIMENSION): jvirt_sarray_ptr; cdecl;
     request_virt_barray: function (cinfo: j_common_ptr; pool_id: Integer; pre_zero: JPEG_BOOLEAN;
       blocksperrow: JDIMENSION; numrows: JDIMENSION; maxaccess: JDIMENSION): jvirt_barray_ptr; cdecl;
     realize_virt_arrays: procedure (cinfo: j_common_ptr); cdecl;
     access_virt_sarray: function (cinfo: j_common_ptr; ptr: jvirt_sarray_ptr; start_row: JDIMENSION;
       num_rows: JDIMENSION; writable: JPEG_BOOLEAN): JSAMPARRAY; cdecl;
     access_virt_barray: function (cinfo: j_common_ptr; ptr: jvirt_barray_ptr; start_row: JDIMENSION;
       num_rows: JDIMENSION; writable: JPEG_BOOLEAN): JBLOCKARRAY; cdecl;
     free_pool: procedure (cinfo: j_common_ptr; pool_id: Integer); cdecl;
     self_destruct: procedure (cinfo: j_common_ptr); cdecl;

     {* Limit on memory allocation for this JPEG object.  (Note that this is
      * merely advisory, not a guaranteed maximum; it only affects the space
      * used for virtual-array buffers.)  May be changed by outer application
      * after creating the JPEG object.
      *}
     max_memory_to_use: long;

     // Maximum allocation request accepted by alloc_large.
     max_alloc_chunk: long;
   end;


   // Routine signature for application-supplied marker processing methods.
   // Need not pass marker code since it is stored in cinfo^.unread_marker.
   jpeg_marker_parser_method = function(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;



// Default error-management setup
function jpeg_std_error(err: jpeg_error_mgr_ptr): jpeg_error_mgr_ptr; cdecl;

{* Initialization of JPEG compression objects.
 * jpeg_create_compress() and jpeg_create_decompress() are the exported
 * names that applications should call.  These expand to calls on
 * jpeg_CreateCompress and jpeg_CreateDecompress with additional information
 * passed for version mismatch checking.
 * NB: you must set up the error-manager BEFORE calling jpeg_create_xxx.
 *}
procedure jpeg_create_compress(cinfo: j_compress_ptr);
procedure jpeg_create_decompress(cinfo: j_decompress_ptr);

// Destruction of JPEG compression objects
procedure jpeg_destroy_compress(cinfo: j_compress_ptr); cdecl;
procedure jpeg_destroy_decompress(cinfo: j_decompress_ptr); cdecl;

// Standard data source and destination managers: stdio streams.
// Caller is responsible for opening the file before and closing after.
{$IFNDEF CPU64}
procedure jpeg_stdio_dest(cinfo: j_compress_ptr; output_file: TStream); cdecl;
procedure jpeg_stdio_src(cinfo: j_decompress_ptr; input_file: TStream); cdecl;
{$ENDIF}


// Default parameter setup for compression
procedure jpeg_set_defaults(cinfo: j_compress_ptr); cdecl;
// Compression parameter setup aids
procedure jpeg_set_colorspace(cinfo: j_compress_ptr; colorspace: J_COLOR_SPACE); cdecl;
procedure jpeg_default_colorspace(cinfo: j_compress_ptr); cdecl;
procedure jpeg_set_quality(cinfo: j_compress_ptr; quality: Integer; force_baseline: JPEG_BOOLEAN); cdecl;
procedure jpeg_set_linear_quality(cinfo: j_compress_ptr; scale_factor: Integer; force_baseline: JPEG_BOOLEAN); cdecl;
procedure jpeg_add_quant_table(cinfo: j_compress_ptr; which_tbl: Integer; const basic_table: PCardinal;
  scale_factor: Integer; force_baseline: JPEG_BOOLEAN); cdecl;
procedure jpeg_quality_scaling(quality: Integer); cdecl;
procedure jpeg_simple_progression(cinfo: j_compress_ptr); cdecl;
procedure jpeg_suppress_tables(cinfo: j_compress_ptr; suppress: JPEG_BOOLEAN); cdecl;
function jpeg_alloc_quant_table(cinfo: j_common_ptr): JQUANT_TBL_ptr; cdecl;
function jpeg_alloc_huff_table(cinfo: j_common_ptr): JHUFF_TBL_ptr; cdecl;


// Main entry points for compression
procedure jpeg_start_compress(cinfo: j_compress_ptr; write_all_tables: JPEG_BOOLEAN); cdecl;
function jpeg_write_scanlines(cinfo: j_compress_ptr; scanlines: JSAMPARRAY; num_lines: JDIMENSION): JDIMENSION; cdecl;
procedure jpeg_finish_compress(cinfo: j_compress_ptr); cdecl;

// Replaces jpeg_write_scanlines when writing raw downsampled data.
function jpeg_write_raw_data(cinfo: j_compress_ptr; data: JSAMPIMAGE; num_lines: JDIMENSION): JDIMENSION; cdecl;

// Write a special marker.  See libjpeg.doc concerning safe usage.
procedure jpeg_write_marker(cinfo: j_compress_ptr; marker: Integer; dataptr: JOCTET_ptr; datalen: Cardinal); cdecl;
// Same, but piecemeal.
procedure jpeg_write_m_header(cinfo: j_compress_ptr; marker: Integer; datalen: Cardinal); cdecl;
procedure jpeg_write_m_byte(cinfo: j_compress_ptr; val: Integer); cdecl;

// Alternate compression function: just write an abbreviated table file
procedure jpeg_write_tables(cinfo: j_compress_ptr); cdecl;


// Decompression startup: read start of JPEG datastream to see what's there
function jpeg_read_header(cinfo: j_decompress_ptr; require_image: JPEG_BOOLEAN): Integer; cdecl;

// Return value is one of:
const
  JPEG_SUSPENDED          = 0; // Suspended due to lack of input data
  JPEG_HEADER_OK          = 1; // Found valid image datastream
  JPEG_HEADER_TABLES_ONLY = 2; // Found valid table-specs-only datastream
  // If you pass require_image = TRUE (normal case), you need not check for
  // a TABLES_ONLY return code; an abbreviated file will cause an error exit.
  // JPEG_SUSPENDED is only possible if you use a data source module that can
  // give a suspension return (the stdio source module doesn't).


// Main entry points for decompression
function jpeg_start_decompress(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
function jpeg_read_scanlines(cinfo: j_decompress_ptr; scanlines: JSAMPARRAY; max_lines: JDIMENSION): JDIMENSION; cdecl;
function jpeg_finish_decompress(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;

// Replaces jpeg_read_scanlines when reading raw downsampled data.
function jpeg_read_raw_data(cinfo: j_decompress_ptr; data: JSAMPIMAGE; max_lines: JDIMENSION): JDIMENSION; cdecl;

// Additional entry points for buffered-image mode.
function jpeg_has_multiple_scans(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
function jpeg_start_output(cinfo: j_decompress_ptr; scan_number: Integer): JPEG_BOOLEAN; cdecl;
function jpeg_finish_output(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
function jpeg_input_complete(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl;
procedure jpeg_new_colormap(cinfo: j_decompress_ptr); cdecl;
function jpeg_consume_input(cinfo: j_decompress_ptr): Integer; cdecl;
// Return value is one of:
const
  // function jpeg_consume_input return value is one of:
  //JPEG_SUSPENDED    = 0; // Suspended due to lack of input data [already defined earlier]
  JPEG_REACHED_SOS    = 1; // Reached start of new scan
  JPEG_REACHED_EOI    = 2; // Reached end of image
  JPEG_ROW_COMPLETED  = 3; // Completed one iMCU row
  JPEG_SCAN_COMPLETED = 4; // Completed last iMCU row of a scan


// Precalculate output dimensions for current decompression parameters.
procedure jpeg_calc_output_dimensions(cinfo: j_decompress_ptr); cdecl;

// Control saving of COM and APPn markers into marker_list.
procedure jpeg_save_markers(cinfo: j_decompress_ptr; marker_code: Integer;length_limit: Cardinal); cdecl;

// Install a special processing method for COM or APPn markers.
procedure  jpeg_set_marker_processor(cinfo: j_decompress_ptr; marker_code: Integer;
	     routine: jpeg_marker_parser_method); cdecl;

// Read or write raw DCT coefficients --- useful for lossless transcoding.
function jpeg_read_coefficients(cinfo: j_decompress_ptr): Pjvirt_barray_ptr; cdecl;
procedure jpeg_write_coefficients(cinfo: j_compress_ptr;  coef_arrays: Pjvirt_barray_ptr); cdecl;
procedure jpeg_copy_critical_parameters(srcinfo: j_decompress_ptr; dstinfo: j_compress_ptr); cdecl;


{* If you choose to abort compression or decompression before completing
 * jpeg_finish_(de)compress, then you need to clean up to release memory,
 * temporary files, etc.  You can just call jpeg_destroy_(de)compress
 * if you're done with the JPEG object, but if you want to clean it up and
 * reuse it, call this:
 *}
procedure jpeg_abort_compress(cinfo: j_compress_ptr); cdecl;
procedure jpeg_abort_decompress(cinfo: j_decompress_ptr); cdecl;

{* Generic versions of jpeg_abort and jpeg_destroy that work on either
 * flavor of JPEG object.  These may be more convenient in some places.
 *}
procedure jpeg_abort(cinfo: j_common_ptr); cdecl;
procedure jpeg_destroy(cinfo: j_common_ptr); cdecl;

// Default restart-marker-resync procedure for use by data source modules
function jpeg_resync_to_restart(cinfo: j_decompress_ptr; desired: Integer): JPEG_BOOLEAN; cdecl;

const
  // These marker codes are exported since applications and data source modules
  // are likely to want to use them.
  JPEG_RST0 = $D0; // RST0 marker code
  JPEG_EOI  = $D9; // EOI marker code
  JPEG_APP0 = $E0; // APP0 marker code
  JPEG_COM  = $FE; // COM marker code


// These should be considered private. However they are needed by libtiff
// so they can't be moved to implementation.
procedure jpeg_CreateCompress(cinfo: j_compress_ptr; version: Integer; structsize: size_t); cdecl;
procedure jpeg_CreateDecompress(cinfo: j_decompress_ptr; version: Integer; structsize: size_t); cdecl;

// End of jpeglib.h

// ---------- jversion.h ----------

const
  JVERSION   = '6b  27-Mar-1998';
  JCOPYRIGHT = 'JPEG copyright (C) 1998, Thomas G. Lane';


// ---------- jpegint.h ----------

const
  // Values of global_state field (jdapi.c has some dependencies on ordering!).
  CSTATE_START    = 100; // after create_compress
  CSTATE_SCANNING = 101; // start_compress done, write_scanlines OK
  CSTATE_RAW_OK   = 102; // start_compress done, write_raw_data OK
  CSTATE_WRCOEFS  = 103; // jpeg_write_coefficients done
  DSTATE_START    = 200; // after create_decompress
  DSTATE_INHEADER = 201; // reading header markers, no SOS yet
  DSTATE_READY    = 202; // found SOS, ready for start_decompress
  DSTATE_PRELOAD  = 203; // reading multiscan file in start_decompress
  DSTATE_PRESCAN  = 204; // performing dummy pass for 2-pass quant
  DSTATE_SCANNING = 205; // start_decompress done, read_scanlines OK
  DSTATE_RAW_OK   = 206; // start_decompress done, read_raw_data OK
  DSTATE_BUFIMAGE = 207; // expecting jpeg_start_output
  DSTATE_BUFPOST  = 208; // looking for SOS/EOI in jpeg_finish_output
  DSTATE_RDCOEFS  = 209; // reading file in jpeg_read_coefficients
  DSTATE_STOPPING = 210; // looking for EOI in jpeg_finish_decompress


// Some internal libjpeg stuff that we are going to ignore for now
// But our Delphi seems to need the extern definitions
{$IFNDEF FPC}
  {$DEFINE LIBJPEG_INTERNAL}
{$ENDIF}

{$IFDEF LIBJPEG_INTERNAL}
  HUFF_LOOKAHEAD  = 8; // # of bits of lookahead.

type
  GETJSAMPLE = Integer;
  PInteger = ^Integer;
  bit_buf_type = Integer;

  jTOctet = 0..(MaxInt div SizeOf(JOCTET)) - 1;
  JOCTET_FIELD = array[jTOctet] of JOCTET;
  JOCTET_FIELD_ptr = ^JOCTET_FIELD;


  // Marker reading & parsing
  jpeg_marker_reader_ptr = ^jpeg_marker_reader;
  jpeg_marker_reader = record
    reset_marker_reader: procedure(cinfo: j_decompress_ptr); 
    // Read markers until SOS or EOI.
    // Returns same codes as are defined for jpeg_consume_input:
    // JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.
    read_markers: function (cinfo: j_decompress_ptr): Integer;
    // Read a restart marker --- exported for use by entropy decoder only 
    read_restart_marker: jpeg_marker_parser_method;
    // Application-overridable marker processing methods 
    process_COM: jpeg_marker_parser_method;
    process_APPn: array[0..16 - 1] of jpeg_marker_parser_method;

    // State of marker reader --- nominally internal, but applications
    // supplying COM or APPn handlers might like to know the state. 
    saw_SOI: JPEG_BOOLEAN;            // found SOI?
    saw_SOF: JPEG_BOOLEAN;            // found SOF? 
    next_restart_num: Integer;    // next restart number expected (0-7) 
    discarded_Bytes: Cardinal;    // # of Bytes skipped looking for a marker 
  end;


  c_derived_tbl_ptr = ^c_derived_tbl;
  c_derived_tbl = record
    ehufco: array[0..255] of Cardinal; // code for each symbol
    ehufsi: array[0..255] of AnsiChar;     // length of code for each symbol
    // If no code has been allocated for a symbol S, ehufsi[S] contains 0.
  end;

  TFrequencyarray = array[0..256] of Integer;

  // Derived data constructed for each Huffman table.
  d_derived_tbl_ptr = ^d_derived_tbl;
  d_derived_tbl = record
    // Basic tables: (element [0] of each array is unused).
    maxcode: array[0..17] of Integer;   // largest code of length k (-1 if none).
    // (maxcode[17] is a sentinel to ensure jpeg_huff_decode terminates).
    valoffset: array[0..16] of Integer; // huffval[] offset for codes of length k.
    // valoffset[k] = huffval[] index of 1st symbol of code length k, less
    // the smallest code of length k; so given a code of length k, the
    // corresponding symbol is huffval[code + valoffset[k]].

    // Link to public Huffman table (needed only in jpeg_huff_decode).
    pub: JHUFF_TBL_ptr;

    // Lookahead tables: indexed by the next HUFF_LOOKAHEAD bits of
    // the input data stream.  If the next Huffman code is no more
    // than HUFF_LOOKAHEAD bits long, we can obtain its length and
    // the corresponding symbol directly from these tables.
    look_nbits: array[0..(1 shl HUFF_LOOKAHEAD) - 1] of Integer; // # bits, or 0 if too long
    look_sym: array[0..(1 shl HUFF_LOOKAHEAD) - 1] of Byte; // symbol, or unused
  end;

  // Bitreading working state within an MCU.
  bitread_working_state_ptr = ^bitread_working_state;
  bitread_working_state = record
    // Current data source location.
    // We need a copy, rather than munging the original, in case of suspension.
    next_input_Byte: JOCTET_ptr;  // => Next Byte to read from source.
    Bytes_in_buffer: Integer;     // # of Bytes remaining in source buffer.
    // Bit input buffer --- note these values are kept in register variables,
    // not in this struct, inside the inner loops.
    get_buffer: bit_buf_type;     // Current bit-extraction buffer.
    bits_left: Integer;           // # of unused bits in it.
    // Pointer needed by jpeg_fill_bit_buffer.
    cinfo: j_decompress_ptr;      // back link to decompress master record.
  end;
{$ENDIF}


const
  // These constants are not part of libjpeg. I have defined them for ease of use.
  JFIF_DENSITY_UNIT_UNKNOWN = 0; // unknown, but valid to use for aspect ratio
  JFIF_DENSITY_UNIT_DPI     = 1; // Dots per inch
  JFIF_DENSITY_UNIT_DPCM    = 2; // Dots per cm



// Forward declarations of default error routines.
procedure JpegError(cinfo: j_common_ptr); cdecl;
procedure EmitMessage(cinfo: j_common_ptr; msg_level: Integer); cdecl;
procedure OutputMessage(cinfo: j_common_ptr); cdecl;
procedure FormatMessage(cinfo: j_common_ptr; buffer: PAnsiChar); cdecl;
procedure ResetErrorMgr(cinfo: j_common_ptr); cdecl;
function GetMessage(cinfo: j_common_ptr): string;

var
  DefaultErrorManager: jpeg_error_mgr = (
    error_exit: {$IFDEF FPC}@{$ENDIF}JpegError;
    emit_message: {$IFDEF FPC}@{$ENDIF}EmitMessage;
    output_message: {$IFDEF FPC}@{$ENDIF}OutputMessage;
    format_message: {$IFDEF FPC}@{$ENDIF}FormatMessage;
    reset_error_mgr: {$IFDEF FPC}@{$ENDIF}ResetErrorMgr;
  );

{$IFNDEF CPU64}
// Currently not working in Win 64 due to unresolved references to fread, fflush and ferror
// when calling jpeg_stdio_src even though they are available in linklib msvcrt.a.
// Todo: rework this to use our own io functions.
procedure GetJPEGInfo(FileName: string; var Width, Height: Cardinal); overload;
procedure GetJPEGInfo(Stream: TStream; var Width, Height: Cardinal); overload;
{$ENDIF}


{$IFDEF LIBJPEG_INTERNAL}
procedure jpeg_make_c_derived_tbl(cinfo: j_compress_ptr; isDC: JPEG_BOOLEAN; tblno: Integer; var pdtbl: c_derived_tbl_ptr); cdecl;
procedure jpeg_gen_optimal_table(cinfo: j_compress_ptr; htbl: JHUFF_TBL_ptr; freq: TFrequencyarray); cdecl;
procedure jpeg_make_d_derived_tbl(cinfo: j_decompress_ptr; isDC: JPEG_BOOLEAN; tblno: Integer; var pdtbl: d_derived_tbl_ptr); cdecl;
function jpeg_fill_bit_buffer(state: bitread_working_state_ptr; get_buffer: bit_buf_type; bits_left, nbits: Integer): JPEG_BOOLEAN; cdecl;
function jpeg_huff_decode(state: bitread_working_state_ptr; get_buffer: bit_buf_type; bits_left: Integer; htbl: d_derived_tbl_ptr;
  min_bits: Integer): Integer; cdecl;

// Make some special routines accessible by other libraries (e.g. TIF).
//procedure jpeg_reset_huff_decode(cinfo: j_decompress_ptr; Data: PSingle); cdecl;
{$ENDIF}

//------------------------------------------------------------------------------

type
  // Define an exception class to identify our exceptions
  ELibJpegError = class(Exception);

  // A way to intercept emitted messages and fatal error messages
  TMessageInterceptor = procedure(const AMessage: string; const AMessageLevel: Integer;
    var AContinue: Boolean);

// Returns previous handler or nil
function SetMessageInterceptor(AMessageInterceptor: TMessageInterceptor): TMessageInterceptor;


implementation

{$IFNDEF FPC}
uses Windows, LibStub; // Stubs for external C RTL functions referenced by JPEG OBJ files.
{$ELSE}
  {$IFDEF WINDOWS}
  uses Windows; // OutputDebugString
  {$ENDIF}
{$ENDIF}

var
  MessageInterceptor: TMessageInterceptor = nil;

// Returns previous handler or nil
function SetMessageInterceptor(AMessageInterceptor: TMessageInterceptor): TMessageInterceptor;
begin
  Result := @MessageInterceptor;
  MessageInterceptor := @AMessageInterceptor;
end;

{$IFDEF USE_JPEG_RESOURCESTRINGS}
resourcestring
  JMSG_NOMESSAGE = 'Bogus message code %d';
  JERR_ARITH_NOTIMPL = 'Sorry, there are legal restrictions on arithmetic coding';
  JERR_BAD_ALIGN_TYPE = 'ALIGN_TYPE is wrong, please fix';
  JERR_BAD_ALLOC_CHUNK = 'MAX_ALLOC_CHUNK is wrong, please fix';
  JERR_BAD_BUFFER_MODE = 'Bogus buffer control mode';
  JERR_BAD_COMPONENT_ID = 'Invalid component ID %d in SOS';
  JERR_BAD_DCT_COEF = 'DCT coefficient out of range';
  JERR_BAD_DCTSIZE = 'IDCT output block size %d not supported';
  JERR_BAD_HUFF_TABLE = 'Bogus Huffman table definition';
  JERR_BAD_IN_COLORSPACE = 'Bogus input colorspace';
  JERR_BAD_J_COLORSPACE = 'Bogus JPEG colorspace';
  JERR_BAD_LENGTH = 'Bogus marker length';
  JERR_BAD_LIB_VERSION = 'Wrong JPEG library version: library is %d, caller expects %d';
  JERR_BAD_MCU_SIZE = 'Sampling factors too large for interleaved scan';
  JERR_BAD_POOL_ID = 'Invalid memory pool code %d';
  JERR_BAD_PRECISION = 'Unsupported JPEG data precision %d';
  JERR_BAD_PROGRESSION = 'Invalid progressive parameters Ss=%d Se=%d Ah=%d Al=%d';
  JERR_BAD_PROG_SCRIPT = 'Invalid progressive parameters at scan script entry %d';
  JERR_BAD_SAMPLING = 'Bogus sampling factors';
  JERR_BAD_SCAN_SCRIPT = 'Invalid scan script at entry %d';
  JERR_BAD_STATE = 'Improper call to JPEG library in state %d';
  JERR_BAD_STRUCT_SIZE = 'JPEG parameter struct mismatch: library thinks size is %u, caller expects %u';
  JERR_BAD_VIRTUAL_ACCESS = 'Bogus virtual array access';
  JERR_BUFFER_SIZE = 'Buffer passed to JPEG library is too small';
  JERR_CANT_SUSPEND = 'Suspension not allowed here';
  JERR_CCIR601_NOTIMPL = 'CCIR601 sampling not implemented yet';
  JERR_COMPONENT_COUNT = 'Too many color components: %d, max %d';
  JERR_CONVERSION_NOTIMPL = 'Unsupported color conversion request';
  JERR_DAC_INDEX = 'Bogus DAC index %d';
  JERR_DAC_VALUE = 'Bogus DAC value 0x%x';
  JERR_DHT_INDEX = 'Bogus DHT index %d';
  JERR_DQT_INDEX = 'Bogus DQT index %d';
  JERR_EMPTY_IMAGE = 'Empty JPEG image (DNL not supported)';
  JERR_EMS_READ = 'Read from EMS failed';
  JERR_EMS_WRITE = 'Write to EMS failed';
  JERR_EOI_EXPECTED = 'Didn''t expect more than one scan';
  JERR_FILE_READ = 'Input file read error';
  JERR_FILE_WRITE = 'Output file write error --- out of disk space?';
  JERR_FRACT_SAMPLE_NOTIMPL = 'Fractional sampling not implemented yet';
  JERR_HUFF_CLEN_OVERFLOW = 'Huffman code size table overflow';
  JERR_HUFF_MISSING_CODE = 'Missing Huffman code table entry';
  JERR_IMAGE_TOO_BIG = 'Maximum supported image dimension is %u pixels';
  JERR_INPUT_EMPTY = 'Empty input file';
  JERR_INPUT_EOF = 'Premature end of input file';
  JERR_MISMATCHED_QUANT_TABLE = 'Cannot transcode due to multiple use of quantization table %d';
  JERR_MISSING_DATA = 'Scan script does not transmit all data';
  JERR_MODE_CHANGE = 'Invalid color quantization mode change';
  JERR_NOTIMPL = 'Not implemented yet';
  JERR_NOT_COMPILED = 'Requested feature was omitted at compile time';
  JERR_NO_BACKING_STORE = 'Backing store not supported';
  JERR_NO_HUFF_TABLE = 'Huffman table 0x%02x was not defined';
  JERR_NO_IMAGE = 'JPEG datastream contains no image';
  JERR_NO_QUANT_TABLE = 'Quantization table 0x%02x was not defined';
  JERR_NO_SOI = 'Not a JPEG file: starts with 0x%02x 0x%02x';
  JERR_OUT_OF_MEMORY = 'Insufficient memory (case %d)';
  JERR_QUANT_COMPONENTS = 'Cannot quantize more than %d color components';
  JERR_QUANT_FEW_COLORS = 'Cannot quantize to fewer than %d colors';
  JERR_QUANT_MANY_COLORS = 'Cannot quantize to more than %d colors';
  JERR_SOF_DUPLICATE = 'Invalid JPEG file structure: two SOF markers';
  JERR_SOF_NO_SOS = 'Invalid JPEG file structure: missing SOS marker';
  JERR_SOF_UNSUPPORTED = 'Unsupported JPEG process: SOF type 0x%02x';
  JERR_SOI_DUPLICATE = 'Invalid JPEG file structure: two SOI markers';
  JERR_SOS_NO_SOF = 'Invalid JPEG file structure: SOS before SOF';
  JERR_TFILE_CREATE = 'Failed to create temporary file %s';
  JERR_TFILE_READ = 'Read failed on temporary file';
  JERR_TFILE_SEEK = 'Seek failed on temporary file';
  JERR_TFILE_WRITE = 'Write failed on temporary file --- out of disk space?';
  JERR_TOO_LITTLE_DATA = 'Application transferred too few scanlines';
  JERR_UNKNOWN_MARKER = 'Unsupported marker type 0x%02x';
  JERR_VIRTUAL_BUG = 'Virtual array controller messed up';
  JERR_WIDTH_OVERFLOW = 'Image too wide for this implementation';
  JERR_XMS_READ = 'Read from XMS failed';
  JERR_XMS_WRITE = 'Write to XMS failed';
  JMSG_COPYRIGH =  JCOPYRIGHT;
  JMSG_VERSIO =  JVERSION;
  JTRC_16BIT_TABLES = 'Caution: quantization tables are too coarse for baseline JPEG';
  JTRC_ADOBE = 'Adobe APP14 marker: version %d, flags 0x%04x 0x%04x, transform %d';
  JTRC_APP0 = 'Unknown APP0 marker (not JFIF), length %u';
  JTRC_APP14 = 'Unknown APP14 marker (not Adobe), length %u';
  JTRC_DAC = 'Define Arithmetic Table 0x%02x: 0x%02x';
  JTRC_DHT = 'Define Huffman Table 0x%02x';
  JTRC_DQT = 'Define Quantization Table %d  precision %d';
  JTRC_DRI = 'Define Restart Interval %u';
  JTRC_EMS_CLOSE = 'Freed EMS handle %u';
  JTRC_EMS_OPEN = 'Obtained EMS handle %u';
  JTRC_EOI = 'End Of Image';
  JTRC_HUFFBITS = '       %3d %3d %3d %3d %3d %3d %3d %3d';
  JTRC_JFIF = 'JFIF APP0 marker: version %d.%02d, density %dx%d  %d';
  JTRC_JFIF_BADTHUMBNAILSIZE = 'Warning: thumbnail image size does not match data length %u';
  JTRC_JFIF_EXTENSION = 'JFIF extension marker: type 0x%02x, length %u';
  JTRC_JFIF_THUMBNAIL = '   with %d x %d thumbnail image';
  JTRC_MISC_MARKER = 'Miscellaneous marker 0x%02x, length %u';
  JTRC_PARMLESS_MARKER = 'Unexpected marker 0x%02x';
  JTRC_QUANTVALS = '       %4u %4u %4u %4u %4u %4u %4u %4u';
  JTRC_QUANT_3_NCOLORS = 'Quantizing to %d = %d*%d*%d colors';
  JTRC_QUANT_NCOLORS = 'Quantizing to %d colors';
  JTRC_QUANT_SELECTED = 'Selected %d colors for quantization';
  JTRC_RECOVERY_ACTION = 'At marker 0x%02x, recovery action %d';
  JTRC_RST = 'RST%d';
  JTRC_SMOOTH_NOTIMPL = 'Smoothing not supported with nonstandard sampling ratios';
  JTRC_SOF = 'Start Of Frame 0x%02x: width=%u, height=%u, components=%d';
  JTRC_SOF_COMPONENT = '   Component %d: %dhx%dv q=%d';
  JTRC_SOI = 'Start of Image';
  JTRC_SOS = 'Start Of Scan: %d components';
  JTRC_SOS_COMPONENT = '   Component %d: dc=%d ac=%d';
  JTRC_SOS_PARAMS = '  Ss=%d, Se=%d, Ah=%d, Al=%d';
  JTRC_TFILE_CLOSE = 'Closed temporary file %s';
  JTRC_TFILE_OPEN = 'Opened temporary file %s';
  JTRC_THUMB_JPEG = 'JFIF extension marker: JPEG-compressed thumbnail image, length %u';
  JTRC_THUMB_PALETTE = 'JFIF extension marker: palette thumbnail image, length %u';
  JTRC_THUMB_RGB = 'JFIF extension marker: RGB thumbnail image, length %u';
  JTRC_UNKNOWN_IDS = 'Unrecognized component IDs %d %d %d, assuming YCbCr';
  JTRC_XMS_CLOSE = 'Freed XMS handle %u';
  JTRC_XMS_OPEN = 'Obtained XMS handle %u';
  JWRN_ADOBE_XFORM = 'Unknown Adobe color transform code %d';
  JWRN_BOGUS_PROGRESSION = 'Inconsistent progression sequence for component %d coefficient %d';
  JWRN_EXTRANEOUS_DATA = 'Corrupt JPEG data: %u extraneous bytes before marker 0x%02x';
  JWRN_HIT_MARKER = 'Corrupt JPEG data: premature end of data segment';
  JWRN_HUFF_BAD_CODE = 'Corrupt JPEG data: bad Huffman code';
  JWRN_JFIF_MAJOR = 'Warning: unknown JFIF revision number %d.%02d';
  JWRN_JPEG_EOF = 'Premature end of JPEG file';
  JWRN_MUST_RESYNC = 'Corrupt JPEG data: found marker 0x%02x instead of RST%d';
  JWRN_NOT_SEQUENTIAL = 'Invalid SOS parameters for sequential JPEG';
  JWRN_TOO_MUCH_DATA = 'Application transferred too many scanlines';

const
  JPGMessages: array[0..123] of string = (
    JMSG_NOMESSAGE,
    JERR_ARITH_NOTIMPL,
    JERR_BAD_ALIGN_TYPE,
    JERR_BAD_ALLOC_CHUNK,
    JERR_BAD_BUFFER_MODE,
    JERR_BAD_COMPONENT_ID,
    JERR_BAD_DCT_COEF,
    JERR_BAD_DCTSIZE,
    JERR_BAD_HUFF_TABLE,
    JERR_BAD_IN_COLORSPACE,
    JERR_BAD_J_COLORSPACE,
    JERR_BAD_LENGTH,
    JERR_BAD_LIB_VERSION,
    JERR_BAD_MCU_SIZE,
    JERR_BAD_POOL_ID,
    JERR_BAD_PRECISION,
    JERR_BAD_PROGRESSION,
    JERR_BAD_PROG_SCRIPT,
    JERR_BAD_SAMPLING,
    JERR_BAD_SCAN_SCRIPT,
    JERR_BAD_STATE,
    JERR_BAD_STRUCT_SIZE,
    JERR_BAD_VIRTUAL_ACCESS,
    JERR_BUFFER_SIZE,
    JERR_CANT_SUSPEND,
    JERR_CCIR601_NOTIMPL,
    JERR_COMPONENT_COUNT,
    JERR_CONVERSION_NOTIMPL,
    JERR_DAC_INDEX,
    JERR_DAC_VALUE,
    JERR_DHT_INDEX,
    JERR_DQT_INDEX,
    JERR_EMPTY_IMAGE,
    JERR_EMS_READ,
    JERR_EMS_WRITE,
    JERR_EOI_EXPECTED,
    JERR_FILE_READ,
    JERR_FILE_WRITE,
    JERR_FRACT_SAMPLE_NOTIMPL,
    JERR_HUFF_CLEN_OVERFLOW,
    JERR_HUFF_MISSING_CODE,
    JERR_IMAGE_TOO_BIG,
    JERR_INPUT_EMPTY,
    JERR_INPUT_EOF,
    JERR_MISMATCHED_QUANT_TABLE,
    JERR_MISSING_DATA,
    JERR_MODE_CHANGE,
    JERR_NOTIMPL,
    JERR_NOT_COMPILED,
    JERR_NO_BACKING_STORE,
    JERR_NO_HUFF_TABLE,
    JERR_NO_IMAGE,
    JERR_NO_QUANT_TABLE,
    JERR_NO_SOI,
    JERR_OUT_OF_MEMORY,
    JERR_QUANT_COMPONENTS,
    JERR_QUANT_FEW_COLORS,
    JERR_QUANT_MANY_COLORS,
    JERR_SOF_DUPLICATE,
    JERR_SOF_NO_SOS,
    JERR_SOF_UNSUPPORTED,
    JERR_SOI_DUPLICATE,
    JERR_SOS_NO_SOF,
    JERR_TFILE_CREATE,
    JERR_TFILE_READ,
    JERR_TFILE_SEEK,
    JERR_TFILE_WRITE,
    JERR_TOO_LITTLE_DATA,
    JERR_UNKNOWN_MARKER,
    JERR_VIRTUAL_BUG,
    JERR_WIDTH_OVERFLOW,
    JERR_XMS_READ,
    JERR_XMS_WRITE,
    JMSG_COPYRIGH,
    JMSG_VERSIO,
    JTRC_16BIT_TABLES,
    JTRC_ADOBE,
    JTRC_APP0,
    JTRC_APP14,
    JTRC_DAC,
    JTRC_DHT,
    JTRC_DQT,
    JTRC_DRI,
    JTRC_EMS_CLOSE,
    JTRC_EMS_OPEN,
    JTRC_EOI,
    JTRC_HUFFBITS,
    JTRC_JFIF,
    JTRC_JFIF_BADTHUMBNAILSIZE,
    JTRC_JFIF_EXTENSION,
    JTRC_JFIF_THUMBNAIL,
    JTRC_MISC_MARKER,
    JTRC_PARMLESS_MARKER,
    JTRC_QUANTVALS,
    JTRC_QUANT_3_NCOLORS,
    JTRC_QUANT_NCOLORS,
    JTRC_QUANT_SELECTED,
    JTRC_RECOVERY_ACTION,
    JTRC_RST,
    JTRC_SMOOTH_NOTIMPL,
    JTRC_SOF,
    JTRC_SOF_COMPONENT,
    JTRC_SOI,
    JTRC_SOS,
    JTRC_SOS_COMPONENT,
    JTRC_SOS_PARAMS,
    JTRC_TFILE_CLOSE,
    JTRC_TFILE_OPEN,
    JTRC_THUMB_JPEG,
    JTRC_THUMB_PALETTE,
    JTRC_THUMB_RGB,
    JTRC_UNKNOWN_IDS,
    JTRC_XMS_CLOSE,
    JTRC_XMS_OPEN,
    JWRN_ADOBE_XFORM,
    JWRN_BOGUS_PROGRESSION,
    JWRN_EXTRANEOUS_DATA,
    JWRN_HIT_MARKER,
    JWRN_HUFF_BAD_CODE,
    JWRN_JFIF_MAJOR,
    JWRN_JPEG_EOF,
    JWRN_MUST_RESYNC,
    JWRN_NOT_SEQUENTIAL,
    JWRN_TOO_MUCH_DATA
  );
{$ENDIF}

function jpeg_std_error(err: jpeg_error_mgr_ptr): jpeg_error_mgr_ptr; cdecl; external;

procedure jpeg_CreateCompress(cinfo: j_compress_ptr; version: Integer; structsize: size_t); cdecl; external;
procedure jpeg_CreateDecompress(cinfo: j_decompress_ptr; version: Integer; structsize: size_t); cdecl; external;

procedure jpeg_destroy_compress (cinfo: j_compress_ptr); cdecl; external;
procedure jpeg_destroy_decompress (cinfo: j_decompress_ptr); cdecl; external;

{$IFNDEF CPU64}
procedure jpeg_stdio_dest(cinfo: j_compress_ptr; output_file: TStream); cdecl; external;
procedure jpeg_stdio_src(cinfo: j_decompress_ptr; input_file: TStream); cdecl; external;
{$ENDIF}

procedure jpeg_set_defaults(cinfo: j_compress_ptr); cdecl; external;
procedure jpeg_set_colorspace(cinfo: j_compress_ptr; colorspace: J_COLOR_SPACE); cdecl; external;
procedure jpeg_default_colorspace(cinfo: j_compress_ptr); cdecl; external;
procedure jpeg_set_quality(cinfo: j_compress_ptr; quality: Integer; force_baseline: JPEG_BOOLEAN); cdecl; external;
procedure jpeg_set_linear_quality(cinfo: j_compress_ptr; scale_factor: Integer; force_baseline: JPEG_BOOLEAN); cdecl; external;
procedure jpeg_add_quant_table(cinfo: j_compress_ptr; which_tbl: Integer; const basic_table: PCardinal;
  scale_factor: Integer; force_baseline: JPEG_BOOLEAN); cdecl; external;
procedure jpeg_quality_scaling(quality: Integer); cdecl; external;
procedure jpeg_simple_progression(cinfo: j_compress_ptr); cdecl; external;
procedure jpeg_suppress_tables (cinfo: j_compress_ptr; suppress: JPEG_BOOLEAN); cdecl; external;

function jpeg_alloc_quant_table(cinfo: j_common_ptr): JQUANT_TBL_ptr; cdecl; external;
function jpeg_alloc_huff_table(cinfo: j_common_ptr): JHUFF_TBL_ptr; cdecl; external;


procedure jpeg_start_compress(cinfo: j_compress_ptr; write_all_tables: JPEG_BOOLEAN); cdecl; external;
function jpeg_write_scanlines(cinfo: j_compress_ptr; scanlines: JSAMParray; num_lines: JDIMENSION): JDIMENSION; cdecl; external;
procedure jpeg_finish_compress(cinfo: j_compress_ptr); cdecl; external;

function jpeg_write_raw_data(cinfo: j_compress_ptr; data: JSAMPIMAGE; num_lines: JDIMENSION): JDIMENSION; cdecl; external;

procedure jpeg_write_marker(cinfo: j_compress_ptr; marker: Integer; dataptr: JOCTET_ptr; datalen: Cardinal); cdecl; external;
procedure jpeg_write_m_header(cinfo: j_compress_ptr; marker: Integer; datalen: Cardinal); cdecl; external;
procedure jpeg_write_m_byte(cinfo: j_compress_ptr; val: Integer); cdecl; external;

procedure jpeg_write_tables(cinfo: j_compress_ptr); cdecl; external;


function jpeg_read_header(cinfo: j_decompress_ptr; require_image: JPEG_BOOLEAN): Integer; cdecl; external;

function jpeg_start_decompress(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl; external;
function jpeg_read_scanlines(cinfo: j_decompress_ptr; scanlines: JSAMPARRAY; max_lines: JDIMENSION): JDIMENSION; cdecl; external;
function jpeg_finish_decompress(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl; external;

function jpeg_read_raw_data(cinfo: j_decompress_ptr; data: JSAMPIMAGE; max_lines: JDIMENSION): JDIMENSION; cdecl; external;

function jpeg_has_multiple_scans(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl; external;
function jpeg_start_output(cinfo: j_decompress_ptr; scan_number: Integer): JPEG_BOOLEAN; cdecl; external;
function jpeg_finish_output(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl; external;
function jpeg_input_complete(cinfo: j_decompress_ptr): JPEG_BOOLEAN; cdecl; external;
procedure jpeg_new_colormap(cinfo: j_decompress_ptr); cdecl; external;
function jpeg_consume_input(cinfo: j_decompress_ptr): Integer; cdecl; external;

procedure jpeg_calc_output_dimensions(cinfo: j_decompress_ptr); cdecl; external;

procedure jpeg_save_markers(cinfo: j_decompress_ptr; marker_code: Integer;length_limit: Cardinal); cdecl; external;
procedure  jpeg_set_marker_processor(cinfo: j_decompress_ptr; marker_code: Integer;
	     routine: jpeg_marker_parser_method); cdecl; external;

function jpeg_read_coefficients(cinfo: j_decompress_ptr): Pjvirt_barray_ptr; cdecl; external;
procedure jpeg_write_coefficients(cinfo: j_compress_ptr;  coef_arrays: Pjvirt_barray_ptr); cdecl; external;
procedure jpeg_copy_critical_parameters(srcinfo: j_decompress_ptr; dstinfo: j_compress_ptr); cdecl; external;

procedure jpeg_abort_compress(cinfo: j_compress_ptr); cdecl; external;
procedure jpeg_abort_decompress(cinfo: j_decompress_ptr); cdecl; external;

procedure jpeg_abort(cinfo: j_common_ptr); cdecl; external;
procedure jpeg_destroy(cinfo: j_common_ptr); cdecl; external;

function jpeg_resync_to_restart(cinfo: j_decompress_ptr; desired: Integer): JPEG_BOOLEAN; cdecl; external;

// end of jpeglib.h


{$IFDEF LIBJPEG_INTERNAL}
procedure jpeg_make_c_derived_tbl(cinfo: j_compress_ptr; isDC: JPEG_BOOLEAN; tblno: Integer; var pdtbl: c_derived_tbl_ptr); cdecl; external;
procedure jpeg_gen_optimal_table(cinfo: j_compress_ptr; htbl: JHUFF_TBL_ptr; freq: TFrequencyarray); cdecl; external;
procedure jpeg_make_d_derived_tbl(cinfo: j_decompress_ptr; isDC: JPEG_BOOLEAN; tblno: Integer; var pdtbl: d_derived_tbl_ptr); cdecl; external;
function jpeg_fill_bit_buffer(state: bitread_working_state_ptr; get_buffer: bit_buf_type; bits_left, nbits: Integer): JPEG_BOOLEAN; cdecl; external;
function jpeg_huff_decode(state: bitread_working_state_ptr; get_buffer: bit_buf_type; bits_left: Integer; htbl: d_derived_tbl_ptr;
  min_bits: Integer): Integer; cdecl; external;
{$ENDIF}

//------------------------------------------------------------------------------

{$IFNDEF FPC}
// For Delphi it is required to load these before we use them below
{$L jcapimin.obj}
{$L jcapistd.obj}
{$L jctrans.obj}
{$L jcparam.obj}
{$L jdatadst.obj}
{$L jcinit.obj}
{$L jcmaster.obj}
{$L jcmarker.obj}
{$L jcmainct.obj}
{$L jcprepct.obj}
{$L jccoefct.obj}
{$L jccolor.obj}
{$L jcsample.obj}
{$L jchuff.obj}
{$L jcphuff.obj}
{$L jcdctmgr.obj}
{$L jfdctfst.obj}
{$L jfdctflt.obj}
{$L jfdctint.obj}
{$L jdapimin.obj}
{$L jdapistd.obj}
{$L jdtrans.obj}
{$L jdatasrc.obj}
{$L jdmaster.obj}
{$L jdinput.obj}
{$L jdmarker.obj}
{$L jdhuff.obj}
{$L jdphuff.obj}
{$L jdmainct.obj}
{$L jdcoefct.obj}
{$L jdpostct.obj}
{$L jddctmgr.obj}
{$L jidctfst.obj}
{$L jidctflt.obj}
{$L jidctint.obj}
{$L jidctred.obj}
{$L jdsample.obj}
{$L jdcolor.obj}
{$L jquant1.obj}
{$L jquant2.obj}
{$L jdmerge.obj}
{$L jcomapi.obj}
{$L jutils.obj}
{$L jerror.obj}
{$L jmemmgr.obj}
{$L jmemnobs.obj}
{$ELSE}
  // fpc
  {$IFDEF MSWINDOWS}
    {$IFNDEF CPU64}
      {$LINKLIB libmsvcrt.a}
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


procedure jpeg_create_compress(cinfo: j_compress_ptr);
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION, SizeOf(jpeg_compress_struct));
end;

procedure jpeg_create_decompress(cinfo: j_decompress_ptr);
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION, SizeOf(jpeg_decompress_struct));
end;


//------------------------------------------------------------------------------

procedure jpeg_error_exit_raise(cinfo: j_common_ptr); cdecl;
{$IFDEF FPC} public name '_jpeg_error_exit_raise'; {$ENDIF}
begin
  raise ELibJpegError.CreateFmt('LibJpeg: fatal error %d', [cinfo.err.msg_code]);
end;

{$IFDEF USE_JPEG_RESOURCESTRINGS}
procedure JpegError(cinfo: j_common_ptr); cdecl;
var
  Template: string;
begin
  Template := JPGMessages[cinfo^.err^.msg_code];
  // The error can either be a string or up to 8 integers.
  // Search the message template for %s (the string formatter) to decide, which one we have to use.
  if Pos('%s', Template) > 0 then
    raise EJPGError.CreateFmt(Template, [cinfo^.err^.msg_parm.s])
  else
    with cinfo^.err^.msg_parm do
      raise EJPGError.CreateFmt(Template, [i[0], i[1], i[2], i[3], i[4], i[5], i[6], i[7]]);
end;
                                                         
// For debugging only.
procedure EmitMessage(cinfo: j_common_ptr; msg_level: Integer); cdecl;
{$ifopt D+}
var
  Template: string;
  Message: string;
begin
  Template := JPGMessages[cinfo^.err^.msg_code];
  // The message can either be a string or up to 8 integers.
  // Search the message template for %s (the string formatter) to decide, which one we have to use.
  if Pos('%s', Template) > 0 then
    Message := Format(Template, [cinfo^.err^.msg_parm.s])
  else
    with cinfo^.err^.msg_parm do
      Message := Format(Template, [i[0], i[1], i[2], i[3], i[4], i[5], i[6], i[7]]);
  {$IFDEF WINDOWS}
  OutputDebugString(PChar(Message));
  {$ENDIF}
end;
{$else}
begin
end;
{$endif D+}

{$ELSE}

function GetMessage(cinfo: j_common_ptr): string;
{$IFNDEF FPC}
type
  TArrayOfPChar = array [0..999] of PAnsiChar;
  PArrayOfPChar = ^TArrayOfPChar;
{$ENDIF}
var
  Template: string;
begin
  if (cinfo.err.msg_code >= 0) and (cinfo.err.msg_code <= cinfo.err.last_jpeg_message) then begin
    {$IFDEF FPC}
    Template := cinfo.err.jpeg_message_table[cinfo.err.msg_code];
    {$ELSE}
    // Our Delphi can't interpret PPChar as an array of PChar
    Template := PArrayOfPChar(cinfo.err.jpeg_message_table)^[cinfo.err.msg_code];
    {$ENDIF}
    if Pos('%s', Template) > 0 then
      Result := Format(Template, [cinfo.err.msg_parm.s])
    else
      with cinfo.err.msg_parm do
        Result := Format(Template, [I[0], I[1], I[2], I[3], I[4], I[5], I[6], I[7]]);
  end
  else begin
    Result := Format('LibJpeg: Unknown error code %d', [cinfo.err.msg_code]);
  end;
end;

procedure JpegError(cinfo: j_common_ptr); cdecl;
var
  ErrMsg: string;
  DummyContinue: Boolean;
begin
  ErrMsg := GetMessage(cinfo);
  if Assigned(@MessageInterceptor) then begin
    DummyContinue := False; // Dummy since we will never continue here!
    MessageInterceptor(ErrMsg, -2, DummyContinue);
  end;
  jpeg_destroy(cinfo);
  raise ELibJpegError.Create('LibJpeg: ' + ErrMsg);
end;

procedure EmitMessage(cinfo: j_common_ptr; msg_level: Integer); cdecl;
var
  ShouldWeContinue: Boolean;
  ErrMsg: string;
begin
  if msg_level < 0 then
    Inc(cinfo^.err^.num_warnings);
  // By default we continue for everything non fatal including warnings
  ShouldWeContinue := True;

  if Assigned(@MessageInterceptor) then begin
     ErrMsg := GetMessage(cinfo);
     MessageInterceptor(ErrMsg, msg_level, ShouldWeContinue);
  end
  else
      ErrMsg := '';
{$ifopt D+} // For debugging only.
  {$IFDEF WINDOWS}
  OutputDebugString(PChar(ErrMsg));
  {$ENDIF}
{$endif D+}
  if not ShouldWeContinue then begin
    jpeg_destroy(cinfo);
    raise ELibJpegError.Create('LibJpeg: ' + ErrMsg);
  end;
end;
{$ENDIF}

procedure OutputMessage(cinfo: j_common_ptr); cdecl;
begin
end;               
                                   
procedure FormatMessage(cinfo: j_common_ptr; buffer: PAnsiChar); cdecl;
begin
end;

procedure ResetErrorMgr(cinfo: j_common_ptr); cdecl;
begin
  cinfo^.err^.num_warnings := 0;
  cinfo^.err^.msg_code := 0;
end;

//------------------------------------------------------------------------------

{$IFNDEF CPU64}
procedure GetJPEGInfo(Stream: TStream; var Width, Height: Cardinal);
var
  Context: jpeg_decompress_struct;
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.err := @DefaultErrorManager;
  jpeg_CreateDecompress(@Context, JPEG_LIB_VERSION, 0);
  try
    jpeg_stdio_src(@Context, Stream);
    jpeg_read_header(@Context, False);
    Width := Context.image_width;
    Height := Context.image_height;
  finally
    jpeg_destroy(@Context);
  end;
end;

//------------------------------------------------------------------------------

procedure GetJPEGInfo(FileName: string; var Width, Height: Cardinal);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    GetJPEGInfo(Stream, Width, Height);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

end.
