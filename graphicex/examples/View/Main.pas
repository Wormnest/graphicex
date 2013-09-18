unit Main;

// Demo application for using GraphicEx.
//
// Created by Mike Lischke.

interface

uses
  Windows, Messages, SysUtils, GraphicEx, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ExtDlgs, StdCtrls, Menus, ComCtrls, ToolWin, JPG;

type
  TMainForm = class(TForm)
    OPD: TOpenPictureDialog;
    StatusBar: TStatusBar;
    PopupMenu1: TPopupMenu;
    TruevisionTarga1: TMenuItem;
    SPD: TSavePictureDialog;
    ContextPopup: TPopupMenu;
    PropertyItem: TMenuItem;
    WindowsBitmap1: TMenuItem;
    JPEGImage1: TMenuItem;
    CoolBar2: TCoolBar;
    ToolBar3: TToolBar;
    OpenButton: TToolButton;
    FilterBox: TComboBox;
    Label1: TLabel;
    WidthEdit: TEdit;
    WidthUpDown: TUpDown;
    Label2: TLabel;
    HeightEdit: TEdit;
    HeightUpDown: TUpDown;
    ScaleButton: TToolButton;
    ToolButton1: TToolButton;
    ImageIndexEdit: TEdit;
    ImageIndexUpDown: TUpDown;
    ImageCountLabel: TLabel;
    PaintBox1: TPaintBox;
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure OpenButtonClick(Sender: TObject);
    procedure ScaleClick(Sender: TObject);
    procedure TruevisionTarga1Click(Sender: TObject);
    procedure WidthEditKeyPress(Sender: TObject; var Key: Char);
    procedure ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
      const R: TRect; const Msg: string);
    procedure StatusBarResize(Sender: TObject);
    procedure PropertyItemClick(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure ImageIndexUpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
  private
    FProgressBar: TProgressBar;
    FUpDownUpdating: Boolean;
    FCurrentFile: string;
    FThumbnailBackground: TBitmap;
    FPicture: TPicture;
    procedure CreateDefaultBackground;
    procedure DoLoad(const FileName: string);
    procedure DoLoadByIndex(const FileName: string; ImageIndex: Integer);
    procedure FillBackground(R: TRect; Target: TCanvas);

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ShellAPI, Properties, Math;

{$R *.DFM}

type
  // Describes the mode how to blend pixels.
  TBlendMode = (
    bmConstantAlpha,         // apply given constant alpha
    bmPerPixelAlpha,         // use alpha value of the source pixel
    bmMasterAlpha,           // use alpha value of source pixel and multiply it with the constant alpha value
    bmConstantAlphaAndColor  // blend the destination color with the given constant color und the constant alpha value
  );

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineConstant(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using a constant alpha value.
// The layout of a pixel must be BGRA where A is ignored (but is calculated as the other components).
// ConstantAlpha must be in the range 0..255 where 0 means totally transparent (destination pixel only)
// and 255 totally opaque (source pixel only).
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLinePerPixel(Source, Destination: Pointer; Count, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the alpha value of the source pixels.
// The layout of a pixel must be BGRA.
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// Bias is on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F0          /// MOVQ MM6, MM0
        DB      $0F, $69, $F6          /// PUNPCKHWD MM6, MM6
        DB      $0F, $6A, $F6          /// PUNPCKHDQ MM6, MM6

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C6          /// PMULLW    MM0, MM6,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMaster(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

// Blends a line of Count pixels from Source to Destination using the source pixel and a constant alpha value.
// The layout of a pixel must be BGRA.
// ConstantAlpha must be in the range 0..255.
// Bias is an additional value which gets added to every component and must be in the range -128..127
//
// EAX contains Source
// EDX contains Destination
// ECX contains Count
// ConstantAlpha and Bias are on the stack

asm
        PUSH    ESI                    // save used registers
        PUSH    EDI

        MOV     ESI, EAX               // ESI becomes the actual source pointer
        MOV     EDI, EDX               // EDI becomes the actual target pointer

        // Load MM6 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOV     EAX, [ConstantAlpha]
        DB      $0F, $6E, $F0          /// MOVD      MM6, EAX
        DB      $0F, $61, $F6          /// PUNPCKLWD MM6, MM6
        DB      $0F, $62, $F6          /// PUNPCKLDQ MM6, MM6

        // Load MM5 with the bias value.
        MOV     EAX, [Bias]
        DB      $0F, $6E, $E8          /// MOVD      MM5, EAX
        DB      $0F, $61, $ED          /// PUNPCKLWD MM5, MM5
        DB      $0F, $62, $ED          /// PUNPCKLDQ MM5, MM5

        // Load MM4 with 128 to allow for saturated biasing.
        MOV     EAX, 128
        DB      $0F, $6E, $E0          /// MOVD      MM4, EAX
        DB      $0F, $61, $E4          /// PUNPCKLWD MM4, MM4
        DB      $0F, $62, $E4          /// PUNPCKLDQ MM4, MM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        DB      $0F, $EF, $C0          /// PXOR      MM0, MM0,   clear source pixel register for unpacking
        DB      $0F, $60, $06          /// PUNPCKLBW MM0, [ESI], unpack source pixel byte values into words
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     move higher bytes to lower bytes
        DB      $0F, $EF, $C9          /// PXOR      MM1, MM1,   clear target pixel register for unpacking
        DB      $0F, $60, $0F          /// PUNPCKLBW MM1, [EDI], unpack target pixel byte values into words
        DB      $0F, $6F, $D1          /// MOVQ      MM2, MM1,   make a copy of the shifted values, we need them again
        DB      $0F, $71, $D1, $08     /// PSRLW     MM1, 8,     move higher bytes to lower bytes

        // Load MM7 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        DB      $0F, $6F, $F8          /// MOVQ      MM7, MM0
        DB      $0F, $69, $FF          /// PUNPCKHWD MM7, MM7
        DB      $0F, $6A, $FF          /// PUNPCKHDQ MM7, MM7
        DB      $0F, $D5, $FE          /// PMULLW    MM7, MM6,   source alpha * master alpha
        DB      $0F, $71, $D7, $08     /// PSRLW     MM7, 8,     divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        DB      $0F, $F9, $C1          /// PSUBW     MM0, MM1,   source - target
        DB      $0F, $D5, $C7          /// PMULLW    MM0, MM7,   alpha * (source - target)
        DB      $0F, $FD, $C2          /// PADDW     MM0, MM2,   add target (in shifted form)
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8,     divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        DB      $0F, $F9, $C4          /// PSUBW     MM0, MM4
        DB      $0F, $ED, $C5          /// PADDSW    MM0, MM5
        DB      $0F, $FD, $C4          /// PADDW     MM0, MM4
        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0,   convert words to bytes with saturation
        DB      $0F, $7E, $07          /// MOVD      [EDI], MM0, store the result
@3:
        ADD     ESI, 4
        ADD     EDI, 4
        DEC     ECX
        JNZ     @1
        POP     EDI
        POP     ESI
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlendLineMasterAndColor(Destination: Pointer; Count: Integer; ConstantAlpha, Color: Integer);

// Blends a line of Count pixels in Destination against the given color using a constant alpha value.
// The layout of a pixel must be BGRA and Color must be rrggbb00 (as stored by a COLORREF).
// ConstantAlpha must be in the range 0..255.
//
// EAX contains Destination
// EDX contains Count
// ECX contains ConstantAlpha
// Color is passed on the stack

asm
        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load MM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        DB      $0F, $6E, $D9          /// MOVD      MM3, ECX
        DB      $0F, $61, $DB          /// PUNPCKLWD MM3, MM3
        DB      $0F, $62, $DB          /// PUNPCKLDQ MM3, MM3

        // Calculate factor 2.
        MOV     ECX, $100
        DB      $0F, $6E, $D1          /// MOVD      MM2, ECX
        DB      $0F, $61, $D2          /// PUNPCKLWD MM2, MM2
        DB      $0F, $62, $D2          /// PUNPCKLDQ MM2, MM2
        DB      $0F, $F9, $D3          /// PSUBW     MM2, MM3             // MM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in MM3, but the r and b components of Color must be swapped.
        MOV     ECX, [Color]
        BSWAP   ECX
        ROR     ECX, 8
        DB      $0F, $6E, $C9          /// MOVD      MM1, ECX             // Load the color and convert to word sized values.
        DB      $0F, $EF, $E4          /// PXOR      MM4, MM4
        DB      $0F, $60, $CC          /// PUNPCKLBW MM1, MM4
        DB      $0F, $D5, $CB          /// PMULLW    MM1, MM3             // MM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        DB      $0F, $6E, $00          /// MOVD      MM0, [EAX]
        DB      $0F, $60, $C4          /// PUNPCKLBW MM0, MM4

        DB      $0F, $D5, $C2          /// PMULLW    MM0, MM2             // calculate F1 + F2 * target
        DB      $0F, $FD, $C1          /// PADDW     MM0, MM1
        DB      $0F, $71, $D0, $08     /// PSRLW     MM0, 8               // divide by 256

        DB      $0F, $67, $C0          /// PACKUSWB  MM0, MM0             // convert words to bytes with saturation
        DB      $0F, $7E, $00          /// MOVD      [EAX], MM0           // store the result

        ADD     EAX, 4
        DEC     EDX
        JNZ     @1
end;

//----------------------------------------------------------------------------------------------------------------------

procedure EMMS;

// Reset MMX state to use the FPU for other tasks again.

asm
        DB      $0F, $77               /// EMMS
end;

//----------------------------------------------------------------------------------------------------------------------

function GetBitmapBitsFromDeviceContext(DC: HDC; var Width, Height: Integer): Pointer;

// Helper function used to retrieve the bitmap selected into the given device context. If there is a bitmap then
// the function will return a pointer to its bits otherwise nil is returned.
// Additionally the dimensions of the bitmap are returned. 

var
  Bitmap: HBITMAP;
  DIB: TDIBSection;

begin
  Result := nil;
  Width := 0;
  Height := 0;

  Bitmap := GetCurrentObject(DC, OBJ_BITMAP);
  if Bitmap <> 0 then
  begin
    if GetObject(Bitmap, SizeOf(DIB), @DIB) = SizeOf(DIB) then
    begin
      Assert(DIB.dsBm.bmPlanes * DIB.dsBm.bmBitsPixel = 32, 'Alpha blending error: bitmap must use 32 bpp.');
      Result := DIB.dsBm.bmBits;
      Width := DIB.dsBmih.biWidth;
      Height := DIB.dsBmih.biHeight;
    end;
  end;
  Assert(Result <> nil, 'Alpha blending DC error: no bitmap available.');
end;

//----------------------------------------------------------------------------------------------------------------------

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;

// Helper function to calculate the start address for the given row.

begin
  if Height > 0 then  // bottom-up DIB
    Row := Height - Row - 1;
  // Return DWORD aligned address of the requested scanline.
  Integer(Result) := Integer(Bits) + Row * ((Width * 32 + 31) and not 31) div 8;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AlphaBlend(Source, Destination: HDC; R: TRect; Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

// Optimized alpha blend procedure using MMX instructions to perform as quick as possible.
// For this procedure to work properly it is important that both source and target bitmap use the 32 bit color format.
// R describes the source rectangle to work on.
// Target is the place (upper left corner) in the target bitmap where to blend to. Note that source width + X offset
// must be less or equal to the target width. Similar for the height.
// If Mode is bmConstantAlpha then the blend operation uses the given ConstantAlpha value for all pixels.
// If Mode is bmPerPixelAlpha then each pixel is blended using its individual alpha value (the alpha value of the source).
// If Mode is bmMasterAlpha then each pixel is blended using its individual alpha value multiplied by ConstantAlpha.
// If Mode is bmConstantAlphaAndColor then each destination pixel is blended using ConstantAlpha but also a constant
// color which will be obtained from Bias. In this case no offset value is added, otherwise Bias is used as offset.
// Blending of a color into target only (bmConstantAlphaAndColor) ignores Source (the DC) and Target (the position).
// CAUTION: This procedure does not check whether MMX instructions are actually available! Call it only if MMX is really
//          usable.

var
  Y: Integer;
  SourceRun,
  TargetRun: PByte;

  SourceBits,
  DestBits: Pointer;
  SourceWidth,
  SourceHeight,
  DestWidth,
  DestHeight: Integer;

begin
  if not IsRectEmpty(R) then
  begin
    // Note: it is tempting to optimize the special cases for constant alpha 0 and 255 by just ignoring soure
    //       (alpha = 0) or simply do a blit (alpha = 255). But this does not take the bias into account.
    case Mode of
      bmConstantAlpha:
        begin
          // Get a pointer to the bitmap bits for the source and target device contexts.
          // Note: this supposes that both contexts do actually have bitmaps assigned!
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLineConstant(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmPerPixelAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * R.Left);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              Inc(TargetRun, 4 * Target.X);
              AlphaBlendLinePerPixel(SourceRun, TargetRun, R.Right - R.Left, Bias);
            end;
          end;
          EMMS;
        end;
      bmMasterAlpha:
        begin
          SourceBits := GetBitmapBitsFromDeviceContext(Source, SourceWidth, SourceHeight);
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(SourceBits) and Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              SourceRun := CalculateScanline(SourceBits, SourceWidth, SourceHeight, Y + R.Top);
              Inc(SourceRun, 4 * Target.X);
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + Target.Y);
              AlphaBlendLineMaster(SourceRun, TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
      bmConstantAlphaAndColor:
        begin
          // Source is ignored since there is a constant color value.
          DestBits := GetBitmapBitsFromDeviceContext(Destination, DestWidth, DestHeight);
          if Assigned(DestBits) then
          begin
            for Y := 0 to R.Bottom - R.Top - 1 do
            begin
              TargetRun := CalculateScanline(DestBits, DestWidth, DestHeight, Y + R.Top);
              Inc(TargetRun, 4 * R.Left);
              AlphaBlendLineMasterAndColor(TargetRun, R.Right - R.Left, ConstantAlpha, Bias);
            end;
          end;
          EMMS;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateDefaultBackground;

// Creates the checkered default background for an entry.

begin
  FThumbnailBackground := TBitmap.Create;
  with FThumbnailBackground do
  begin
    Width := 16;
    Height := 16;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Brush.Color := clBtnHighlight;
    Canvas.FillRect(Rect(0, 0, 8, 8));
    Canvas.FillRect(Rect(8, 8, 16, 16));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoLoad(const FileName: string);

var
  Graphic: TGraphic;

begin
  DoLoadByIndex(FileName, 1);
  Graphic := FPicture.Graphic;
  if Graphic is TGraphicExGraphic then
  begin
    ImageIndexUpDown.Max := TGraphicExGraphic(Graphic).ImageProperties.ImageCount;
    ImageCountLabel.Caption := Format(' of %d images', [ImageIndexUpDown.Max]);
  end
  else
  begin
    ImageIndexUpDown.Max := 1;
    ImageCountLabel.Caption := ' of 1 image';
  end;
  ImageIndexUpDown.Position := 0;

  PaintBox1.Width := Max(ClientWidth, FPicture.Width);
  PaintBox1.Height := Max(ClientHeight, FPicture.Height);

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TOpenPictureDialogCast = class(TOpenPictureDialog); // To access a protected property.

procedure TMainForm.FormCreate(Sender: TObject);

begin
  FPicture := TPicture.Create;
  CreateDefaultBackground;
  
  // The following line is to prevent long pauses in the open dialog, when selecting different images.
  TOpenPictureDialogCast(OPD).ImageCtrl.IncrementalDisplay := False;

  OPD.Filter := FileFormatList.GetGraphicFilter([], fstBoth, [foCompact, foIncludeAll, foIncludeExtension], nil);
  FilterBox.ItemIndex := 5;
  DragAcceptFiles(Handle, True);

  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    DoLoad(ParamStr(1));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  FPicture.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);

begin
  PaintBox1.Width := Max(ClientWidth, FPicture.Width);
  PaintBox1.Height := Max(ClientHeight, FPicture.Height);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OpenButtonClick(Sender: TObject);

begin
  if OPD.Execute then
    DoLoad(OPD.FileName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ScaleClick(Sender: TObject);

var
  Start: DWORD;
  NewX, NewY: Integer;

begin
  if not FPicture.Bitmap.Empty then
  begin
    Screen.Cursor := crHourGlass;
    try
      FPicture.Bitmap.PixelFormat := pf24Bit;
      Start := GetTickCount;
      if WidthUpDown.Position = 0 then
        NewX := FPicture.Width
      else
        NewX := WidthUpDown.Position;
      if HeightUpDown.Position = 0 then
        NewY := FPicture.Height
      else
        NewY := HeightUpDown.Position;
      Stretch(NewX, NewY, TResamplingFilter(FilterBox.ItemIndex), 0, FPicture.Bitmap);
      Statusbar.Panels[1].Text := 'stretch time: ' + IntToStr(GetTickCount - Start) + ' ms';
      Statusbar.Panels[0].Text := Format('%d x %d', [FPicture.Width, FPicture.Height]);
      PaintBox1.Invalidate;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TruevisionTarga1Click(Sender: TObject);

var
  Target: TTargaGraphic;

begin
  with SPD do
  begin
    Filter := FileFormatList.GetGraphicFilter([], fstBoth, [foCompact, foIncludeAll, foIncludeExtension], TTargaGraphic);
    if Execute then
    begin
      Target := TTargaGraphic.Create;
      try
        if FPicture.Graphic is TBitmap then
          Target.Assign(FPicture.Graphic)
        else
        begin
          Target.PixelFormat := pf24Bit;
          Target.Width := FPicture.Width;
          Target.Height := FPicture.Height;
          Target.Canvas.Draw(0, 0, FPicture.Graphic);
        end;
        Target.SaveToFile(FileName);
      finally
        Target.Free;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WidthEditKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #13 then
    ScaleButton.Click;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ImageLoadProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: string);

var
  X: Integer;

begin
  with Statusbar do
  begin
    case Stage of
      psStarting:
        begin
          Panels[2].Bevel := pbNone;
          SizeGrip := False;
          FProgressBar := TProgressBar.Create(nil);
          FProgressBar.Parent := StatusBar;
          FProgressBar.Max := 100;
          Statusbar.Panels[1].Text := Msg;
          X := 4 + Panels[0].Width + Panels[1].Width;
          FProgressBar.SetBounds(X, 4, Panels[2].Width, Height - 6);
          FProgressBar.Show;
          Application.ProcessMessages;
        end;
      psEnding:
        begin
          FProgressBar.Free;
          FProgressBar := nil;
          Panels[2].Bevel := pbLowered;
          SizeGrip := True;
        end;
      psRunning:
        begin
          FProgressBar.Position := PercentDone;
          FProgressBar.Update;
          Application.ProcessMessages;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarResize(Sender: TObject);

begin
  with StatusBar do
  begin
    StatusBar.Panels[2].Width := Width - Panels[0].Width - Panels[1].Width - 8;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PaintBox1Paint(Sender: TObject);

var
  X: Integer;
  Y: Integer;
  R: TRect;
  Buffer: TBitmap;

begin
  with Sender as TPaintBox do
  begin
    if ClientWidth > FPicture.Width then
      X := (ClientWidth - FPicture.Width) div 2
    else
      X := 0;
    if ClientHeight > FPicture.Height then
      Y := (ClientHeight - FPicture.Height) div 2
    else
      Y := 0;

    if FPicture.Bitmap.PixelFormat = pf32Bit then
    begin
      Buffer := TBitmap.Create;
      try
        Buffer.Width := Max(ClientWidth, FPicture.Width);
        Buffer.Height := Max(ClientHeight, FPicture.Height);
        Buffer.PixelFormat := pf32Bit;

        R := Rect(0, 0, FPicture.Width, FPicture.Height);
        Fillbackground(ClientRect, Buffer.Canvas);
        Main.AlphaBlend(FPicture.Bitmap.Canvas.Handle, Buffer.Canvas.Handle, R, Point(X, Y), bmPerPixelAlpha, 0, 0);
        Canvas.Draw(0, 0, Buffer);
      finally
        Buffer.Free;
      end;
    end
    else
    begin
      Canvas.Draw(X, Y, FPicture.Graphic);
      ExcludeClipRect(Canvas.Handle, X, Y, X + FPicture.Width, Y + FPicture.Height);
      Fillbackground(ClientRect, Canvas);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PropertyItemClick(Sender: TObject);

begin
  PropertyDialog.Graphic := FPicture.Graphic;
  PropertyDialog.ShowModal;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);

var
  OldValue: SmallInt;

begin
  if not FUpDownUpdating then
  begin
    FUpDownUpdating := True; // recursion stop
    OldValue := (Sender as TUpDown).Position;
    if OldValue = 0 then
      OldValue := 1;

    if Sender = WidthUpDown then
    begin
      HeightUpDown.Position := Round(HeightUpDown.Position * NewValue / OldValue);
    end
    else
    begin
      WidthUpDown.Position := Round(WidthUpDown.Position * NewValue / OldValue);
    end;
    FUpDownUpdating := False;

    PaintBox1.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ImageIndexUpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);

var
  OldValue: SmallInt;
  NewIndex: Integer;

begin
  if not FUpDownUpdating then
  begin
    FUpDownUpdating := True; // recursion stop
    OldValue := (Sender as TUpDown).Position;

    NewIndex := OldValue;
    case Direction of
      updUp:
        begin
          Inc(NewIndex);
          if NewIndex > (Sender as TUpDown).Max then
            NewIndex := (Sender as TUpDown).Max;
        end;
      updDown:
        begin
          Dec(NewIndex);
          if NewIndex < 1 then
            NewIndex := 1;
        end;
    end;

    if NewIndex <> OldValue then
      DoLoadByIndex(FCurrentFile, NewIndex);
    FUpDownUpdating := False;

    PaintBox1.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoLoadByIndex(const FileName: string; ImageIndex: Integer);

var
  Start: DWORD;
  GraphicClass: TGraphicExGraphicClass;
  SecondCaseClass: TGraphicClass;
  Graphic: TGraphic;

begin
  Graphic := nil;
  Screen.Cursor := crHourGlass;
  try
    FCurrentFile := FileName;
    try
      Start := GetTickCount;
      // Determine true file type from content rather than extension.
      GraphicClass := FileFormatList.GraphicFromContent(FileName);
      if GraphicClass = nil then
      begin
        // Some formats (e.g. Dr. Halo CUT images) cannot be determined from content.
        SecondCaseClass := FileFormatList.GraphicFromExtension(FileName);
        if SecondCaseClass.InheritsFrom(GraphicClass) then
          GraphicClass := TGraphicExGraphicClass(SecondCaseClass);
      end;
      if GraphicClass = nil then
        FPicture.LoadFromFile(FileName)
      else
      begin
        // GraphicFromContent always returns TGraphicExGraphicClass
        Graphic := GraphicClass.Create;
        Graphic.OnProgress := ImageLoadProgress;
        TGraphicExGraphic(Graphic).LoadFromFileByIndex(FileName, ImageIndex - 1);
        FPicture.Graphic := Graphic;
      end;
      Statusbar.Panels[0].Text := Format('%d x %d', [FPicture.Width, FPicture.Height]);
      Statusbar.Panels[1].Text := 'load time: ' + IntToStr(GetTickCount - Start) + ' ms';
      Statusbar.Panels[2].Text := FileName;
      FUpDownUpdating := True;
      WidthUpDown.Position := FPicture.Width;
      HeightUpDown.Position := FPicture.Height;
      FUpDownUpdating := False;
      PropertyItem.Enabled := True;
    except
      PropertyItem.Enabled := False;
      raise;
    end;
  finally
    Graphic.Free;
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FillBackground(R: TRect; Target: TCanvas);

// Tiles the background image over the given target bitmap.

var
  X, Y: Integer;
  dX, dY: Integer;

begin
  with Target do
  begin
    dX := FThumbnailBackground.Width;
    dY := FThumbnailBackground.Height;

    Y := 0;
    while Y < R.Bottom - R.Top do
    begin
      X := 0;
      while X < R.Right - R.Left do
      begin
        Draw(X, Y, FThumbnailBackground);
        Inc(X, dX);
      end;
      Inc(Y, dY);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);

var
  Buffer: array[0..MAX_PATH] of Char;
  Count: Cardinal;

begin
  Count := DragQueryFile(Msg.Drop, DWORD(-1), nil, 0);
  if Count > 0 then
  begin
    DragQueryFile(Msg.Drop, 0, Buffer, MAX_PATH);
    DoLoad(Buffer);
    PaintBox1.Invalidate;
    DragFinish(Msg.Drop);
    Msg.Result := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMEraseBkgnd(var Msg: TWMEraseBkgnd);

begin
  Msg.Result := 1; 
end;

//----------------------------------------------------------------------------------------------------------------------

end.


