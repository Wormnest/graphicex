{ gexBlend Blending functions extracted from GraphicEx View demo by Mike Lischke.
           64 bit versions added from the Lazarus version of VirtualTreeView.
  License: MPL 1.1.
  Portions Copyright Jacob Boerema 2013-2015.
}
unit gexBlend;

interface

{$I gexdefines.inc}

uses SysUtils, Windows;

type
  // Describes the mode how to blend pixels.
  TBlendMode = (
    bmConstantAlpha,         // apply given constant alpha
    bmPerPixelAlpha,         // use alpha value of the source pixel
    bmMasterAlpha,           // use alpha value of source pixel and multiply it with the constant alpha value
    bmConstantAlphaAndColor  // blend the destination color with the given constant color und the constant alpha value
  );


// AlphaBlend
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
procedure AlphaBlend(Source, Destination: HDC; R: TRect; Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);


implementation

uses
  gexTypes;

type
  // Exception when trying to perform a blend operation.
  EGexBlendException = class(EgexBaseException);

//------------------------------------------------------------------------------

// Blends a line of Count pixels from Source to Destination using a constant alpha value.
// The layout of a pixel must be BGRA where A is ignored (but is calculated as the other components).
// ConstantAlpha must be in the range 0..255 where 0 means totally transparent (destination pixel only)
// and 255 totally opaque (source pixel only).
// Bias is an additional value which gets added to every component and must be in the range -128..127
procedure AlphaBlendLineConstant(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

{$IFNDEF CPU64}
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
{$ELSE}
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack
asm
        //.NOFRAME

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOVD        XMM3, R9D  // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        MOVD        XMM5, [Bias]
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW     XMM0, XMM4
        PADDSW    XMM0, XMM5
        PADDW     XMM0, XMM4
        PACKUSWB  XMM0, XMM0      // convert words to bytes with saturation
        MOVD      DWORD PTR [RDX], XMM0     // store the result
@3:
        ADD       RCX, 4
        ADD       RDX, 4
        DEC       R8D
        JNZ       @1
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

// Blends a line of Count pixels from Source to Destination using the alpha value of the source pixels.
// The layout of a pixel must be BGRA.
// Bias is an additional value which gets added to every component and must be in the range -128..127
procedure AlphaBlendLinePerPixel(Source, Destination: Pointer; Count, Bias: Integer);

{$IFNDEF CPU64}
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
{$ELSE} // Does not seem to work (correctly) at least in Fpc
asm
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains Bias

        //.NOFRAME

        // Load XMM5 with the bias value.
        MOVD        XMM5, R9D   // Bias
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}    // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RDX]}    // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM3 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM3, XMM0
        PUNPCKHWD   XMM3, XMM3
        PUNPCKHDQ   XMM3, XMM3

        // calculation is: target = (alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM3    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        MOVD        DWORD PTR [RDX], XMM0   // store the result
@3:
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        JNZ         @1
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

// Blends a line of Count pixels from Source to Destination using the source pixel and a constant alpha value.
// The layout of a pixel must be BGRA.
// ConstantAlpha must be in the range 0..255.
// Bias is an additional value which gets added to every component and must be in the range -128..127
procedure AlphaBlendLineMaster(Source, Destination: Pointer; Count: Integer; ConstantAlpha, Bias: Integer);

{$IFNDEF CPU64}
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
{$ELSE}
{$IFDEF FPC}
// jb We need an Fpc equivalent for .SAVENV XMM6 in Delphi
// We follow the example from here: https://github.com/mikerabat/mrmath/blob/master/ASMMatrixMeanOperationsx64.pas
// Note it seems we can also do it by specifying the registers to be saved
// after the final end, e.g. end ['XMM6'], see: http://www.freepascal.org/docs-html/prog/progse12.html#x150-1510003.4
var dXMM6 : array[0..1] of double;
begin
{$ENDIF}
asm
// RCX contains Source
// RDX contains Destination
// R8D contains Count
// R9D contains ConstantAlpha
// Bias is on the stack

        {$IFNDEF FPC}
        .SAVENV XMM6
        {$ELSE}
        movupd      dXMM6, xmm6
        {$ENDIF}

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size.
        MOVD        XMM3, R9D    // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Load XMM5 with the bias value.
        MOV         R10D, [Bias]
        MOVD        XMM5, R10D
        PUNPCKLWD   XMM5, XMM5
        PUNPCKLDQ   XMM5, XMM5

        // Load XMM4 with 128 to allow for saturated biasing.
        MOV         R10D, 128
        MOVD        XMM4, R10D
        PUNPCKLWD   XMM4, XMM4
        PUNPCKLDQ   XMM4, XMM4

@1:     // The pixel loop calculates an entire pixel in one run.
        // Note: The pixel byte values are expanded into the higher bytes of a word due
        //       to the way unpacking works. We compensate for this with an extra shift.
        MOVD        XMM1, DWORD PTR [RCX]   // data is unaligned
        MOVD        XMM2, DWORD PTR [RDX]   // data is unaligned
        PXOR        XMM0, XMM0    // clear source pixel register for unpacking
        PUNPCKLBW   XMM0, XMM1{[RCX]}     // unpack source pixel byte values into words
        PSRLW       XMM0, 8       // move higher bytes to lower bytes
        PXOR        XMM1, XMM1    // clear target pixel register for unpacking
        PUNPCKLBW   XMM1, XMM2{[RCX]}     // unpack target pixel byte values into words
        MOVQ        XMM2, XMM1    // make a copy of the shifted values, we need them again
        PSRLW       XMM1, 8       // move higher bytes to lower bytes

        // Load XMM6 with the source alpha value (replicate it for every component).
        // Expand it to word size.
        MOVQ        XMM6, XMM0
        PUNPCKHWD   XMM6, XMM6
        PUNPCKHDQ   XMM6, XMM6
        PMULLW      XMM6, XMM3    // source alpha * master alpha
        PSRLW       XMM6, 8       // divide by 256

        // calculation is: target = (alpha * master alpha * (source - target) + 256 * target) / 256
        PSUBW       XMM0, XMM1    // source - target
        PMULLW      XMM0, XMM6    // alpha * (source - target)
        PADDW       XMM0, XMM2    // add target (in shifted form)
        PSRLW       XMM0, 8       // divide by 256

        // Bias is accounted for by conversion of range 0..255 to -128..127,
        // doing a saturated add and convert back to 0..255.
        PSUBW       XMM0, XMM4
        PADDSW      XMM0, XMM5
        PADDW       XMM0, XMM4
        PACKUSWB    XMM0, XMM0    // convert words to bytes with saturation
        MOVD        DWORD PTR [RDX], XMM0   // store the result
@3:
        ADD         RCX, 4
        ADD         RDX, 4
        DEC         R8D
        JNZ         @1

{$IFDEF FPC}
        // epilog
        movupd   xmm6, dXMM6
end;
{$ENDIF}
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

// Blends a line of Count pixels in Destination against the given color using a constant alpha value.
// The layout of a pixel must be BGRA and Color must be rrggbb00 (as stored by a COLORREF).
// ConstantAlpha must be in the range 0..255.
procedure AlphaBlendLineMasterAndColor(Destination: Pointer; Count: Integer; ConstantAlpha, Color: Integer);

{$IFNDEF CPU64}
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
{$ELSE}
asm
// RCX contains Destination
// EDX contains Count
// R8D contains ConstantAlpha
// R9D contains Color
        //.NOFRAME

        // The used formula is: target = (alpha * color + (256 - alpha) * target) / 256.
        // alpha * color (factor 1) and 256 - alpha (factor 2) are constant values which can be calculated in advance.
        // The remaining calculation is therefore: target = (F1 + F2 * target) / 256

        // Load XMM3 with the constant alpha value (replicate it for every component).
        // Expand it to word size. (Every calculation here works on word sized operands.)
        MOVD        XMM3, R8D   // ConstantAlpha
        PUNPCKLWD   XMM3, XMM3
        PUNPCKLDQ   XMM3, XMM3

        // Calculate factor 2.
        MOV         R10D, $100
        MOVD        XMM2, R10D
        PUNPCKLWD   XMM2, XMM2
        PUNPCKLDQ   XMM2, XMM2
        PSUBW       XMM2, XMM3             // XMM2 contains now: 255 - alpha = F2

        // Now calculate factor 1. Alpha is still in XMM3, but the r and b components of Color must be swapped.
        BSWAP       R9D  // Color
        ROR         R9D, 8
        MOVD        XMM1, R9D              // Load the color and convert to word sized values.
        PXOR        XMM4, XMM4
        PUNPCKLBW   XMM1, XMM4
        PMULLW      XMM1, XMM3             // XMM1 contains now: color * alpha = F1

@1:     // The pixel loop calculates an entire pixel in one run.
        MOVD        XMM0, DWORD PTR [RCX]
        PUNPCKLBW   XMM0, XMM4

        PMULLW      XMM0, XMM2             // calculate F1 + F2 * target
        PADDW       XMM0, XMM1
        PSRLW       XMM0, 8                // divide by 256

        PACKUSWB    XMM0, XMM0             // convert words to bytes with saturation
        MOVD        DWORD PTR [RCX], XMM0            // store the result

        ADD         RCX, 4
        DEC         EDX
        JNZ         @1
end;
{$ENDIF}

//------------------------------------------------------------------------------

// Reset MMX state to use the FPU for other tasks again.
procedure EMMS;
{$IFNDEF CPU64}
asm
        DB      $0F, $77               /// EMMS
end;
{$ELSE}
inline;
begin
end;
{$ENDIF}

//------------------------------------------------------------------------------

// Helper function used to retrieve the bitmap selected into the given device context. If there is a bitmap then
// the function will return a pointer to its bits otherwise nil is returned.
// Additionally the dimensions of the bitmap are returned.
function GetBitmapBitsFromDeviceContext(DC: HDC; out Width, Height: Integer): Pointer;
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
      if DIB.dsBm.bmPlanes * DIB.dsBm.bmBitsPixel <> 32 then
        raise EGexBlendException.Create('Alpha blending error: source is not a 32 bpp bitmap!');
      Result := DIB.dsBm.bmBits;
      Width := DIB.dsBmih.biWidth;
      Height := DIB.dsBmih.biHeight;
    end;
  end;
  if Result = nil then
    raise EGexBlendException.Create('Alpha blending error: no bitmap available in DC.');
end;

//------------------------------------------------------------------------------

// Helper function to calculate the start address for the given row.
function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;
begin
//  {$IFNDEF FPC}
  if Height > 0 then  // bottom-up DIB
    Row := Height - Row - 1;
//  {$ELSE}
  // fpc TBitmap raw data is never bottom up. If we want to start using something
  // else we may need to re evaluate this
//  {$ENDIF}
  // Return DWORD aligned address of the requested scanline.
//  NativeUInt(Result) := NativeUInt(Bits) + NativeUInt(Row) * ((NativeUInt(Width) * 32 + 31) and not 31) div 8;
  Result := PAnsiChar(Bits) + Row * ((Width * 32 + 31) and not 31) div 8;
end;

//------------------------------------------------------------------------------

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
procedure AlphaBlend(Source, Destination: HDC; R: TRect; Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
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
    // Note: it is tempting to optimize the special cases for constant alpha 0 and 255 by just ignoring source
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


////////////////////////////////////////////////////////////////////////////////
// Documenting some other AlphaBlend info/functions for future use. ////////////
////////////////////////////////////////////////////////////////////////////////

// http://stackoverflow.com/questions/12717003/alphablend-and-transparentblt
// Trick: blending the same colors in whatever ratio results in that same color.
// So, the simplest way (and maybe also the most efficient) is to first draw the transparented
// result to a temporary bitmap, and alphablend that bitmap on the destination canvas.
(**
procedure TfrmMain.PaintBox1Paint(Sender: TObject);
const
  BarSize = 30;
var
  R: TRect;
  Bmp: TBitmap;
  BlendFunc: TBlendFunction;
begin
  with PaintBox1 do
  begin
    R := ClientRect;
    Canvas.Brush.Color := clBlue;
    Canvas.FillRect(R);
    Canvas.Brush.Color := clRed;
    Canvas.Ellipse(R);
    Bmp := TBitmap.Create;
    try
      Bmp.Width := Width;
      Bmp.Height := Height;
      BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0,
        SRCCOPY);
      Bmp.Canvas.Brush.Color := clGreen;
      R := Bounds(Width div 3, 0, Width div 3 + 1, BarSize + 1);
      Bmp.Canvas.Rectangle(R);
      BlendFunc.BlendOp := AC_SRC_OVER;
      BlendFunc.BlendFlags := 0;
      BlendFunc.SourceConstantAlpha := 80;
      BlendFunc.AlphaFormat := 0;
      Windows.AlphaBlend(Canvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle,
        0, 0, Width, Height, BlendFunc);
    finally
      Bmp.Free;
    end;
  end;
end;
**)

// http://stackoverflow.com/questions/5964701/how-to-draw-transparent-image-on-a-form
(**
const
  AC_SRC_OVER = 0;
  AC_SRC_ALPHA = 1;

type
  BLENDFUNCTION = packed record
    BlendOp,
    BlendFlags,
    SourceConstantAlpha,
    AlphaFormat: byte;
  end;

function WinAlphaBlend(hdcDest: HDC; xoriginDest, yoriginDest, wDest, hDest: integer;
  hdcSrc: HDC; xoriginSrc, yoriginSrc, wSrc, hSrc: integer; ftn: BLENDFUNCTION): LongBool;
  stdcall; external 'Msimg32.dll' name 'AlphaBlend';

procedure TForm4.FormClick(Sender: TObject);
var
  hbm: HBITMAP;
  bm: BITMAP;
  bf: BLENDFUNCTION;
  dc: HDC;
begin
  hbm := LoadImage(0,
    'C:\Users\Andreas Rejbrand\Skrivbord\RatingCtrl.bmp',
    IMAGE_BITMAP,
    0,
    0,
    LR_LOADFROMFILE);
  if hbm = 0 then
    RaiseLastOSError;
  try
    if GetObject(hbm, sizeof(bm), @bm) = 0 then RaiseLastOSError;
    dc := CreateCompatibleDC(0);
    if dc = 0 then RaiseLastOSError;
    try
      if SelectObject(dc, hbm) = 0 then RaiseLastOSError;
      bf.BlendOp := AC_SRC_OVER;
      bf.BlendFlags := 0;
      bf.SourceConstantAlpha := 255;
      bf.AlphaFormat := AC_SRC_ALPHA;
      if not WinAlphaBlend(Canvas.Handle,
        10,
        10,
        bm.bmWidth,
        bm.bmHeight,
        dc,
        0,
        0,
        bm.bmWidth,
        bm.bmHeight,
        bf) then RaiseLastOSError;
    finally
      DeleteDC(dc);
    end;
  finally
    DeleteObject(hbm);
  end;
end;
**)
// Nice! BTW, your code works with the regular gdi function declaration, i.e. windows.AlphaBlend(),
// no need to declare the constants, the record and the function.. –  Sertac Akyuz
//
// @Sertac: I am very well aware of that, but I am not sure if AlphaBlend is declared in the
// Windows.pas that ships with the old Delphi 7, the Delphi version of the OP. In addition,
// the declaration of AlphaBlend in Windows.pas in Delphi 2009 (my Delphi version) is very 'ugly':
// function AlphaBlend(DC: HDC; p2, p3, p4, p5: Integer; DC6: HDC; p7, p8, p9, p10: Integer; p11: TBlendFunction): BOOL; stdcall;
// But I guess I should have written a footnote about this. –  Andreas Rejbrand


// http://stackoverflow.com/questions/12700165/why-does-alphablend-always-return-false-drawing-on-canvas
//
// You have various errors in your code.
// You're not filling some members of your BLENDFUNCTION. They are not optional, supply their values.
// Your bitmap object creation should be before the try statement (this is not related with
// why AlphaBlend fails).
// You are requesting the AlphaBlend function to blend from the source more than it has, i.e.
// your bitmap is 99x99 but you want the api to blend 105x105 from it.
// Also note that in the paint handler of the paintbox you're filling an arbitrary rectangle
// (your r is not initialized).
//
(***
procedure alphaBlendf(
        const in_target       : TCanvas;
        const in_transperancy : integer;
        const in_color        : TColor;
        const in_rect         : TRect;
        const in_width        : integer;
        const in_height       : integer);
var
  w          : integer;
  h          : integer;
  bitmap     : TBitmap;
  blendFn    : BLENDFUNCTION;
  ret        : boolean;
begin
  blendFn.BlendOp             := AC_SRC_OVER;
  blendFn.BlendFlags          := 0;
  blendFn.SourceConstantAlpha := 80;
  blendFn.AlphaFormat         := 0;

  bitmap                    := TBitmap.Create;
  try
   w := in_rect.Right - in_rect.Left - 1;
   h := in_rect.Bottom - in_rect.Top - 1;

   bitmap.PixelFormat        := pf32bit;
   bitmap.Width              := w;
   bitmap.Height             := h;
   bitmap.Canvas.Brush.Color := in_color;

   bitmap.Canvas.Rectangle(in_rect);

   ret := Windows.AlphaBlend(
        in_target.Handle,
        0,
        0,
        in_width,
        in_height,
        bitmap.Canvas.Handle,
        0,
        0,
        bitmap.width,
        bitmap.height,
        blendFn);

   if ret then in_target.TextOut(0, 0, 'ok')
          else in_target.TextOut(0, 0, 'fail');
  finally
   bitmap.Free;
  end;
end;

{..............................................................................}
procedure TfrmMain.PaintBox1Paint(Sender: TObject);
var
  r: TRect;
begin
  PaintBox1.Canvas.Brush.Color := $FCFFB5;
  r := Rect(0, 0, 100, 100);
  PaintBox1.Canvas.FillRect(r);

  alphaBlendf(PaintBox1.Canvas, 0, clLime, r,
      PaintBox1.ClientWidth, PaintBox1.ClientHeight);
end;
***)

// http://www.delphi-central.com/tutorials/AlphaBlend.aspx
//
// Delphi Tutorial explaining how to use the AlphaBlend Function
// Alpha blending is used to display an alpha bitmap, which is a bitmap that has transparent
// or semi-transparent pixels.
// In addition to a red, green, and blue color channel, each pixel in an alpha bitmap has
// a transparency component known as its alpha channel. The alpha channel typically contains
// as many bits as a color channel.
//
// For example, an 8-bit alpha channel can represent 256 levels of transparency, from 0
// (the entire bitmap is transparent) to 255 (the entire bitmap is opaque). Alpha blending
// mechanisms are invoked by calling AlphaBlend function.
//
// The AlphaBlend function displays bitmaps that have transparent or semitransparent pixels.
// It is not supported on Microsoft Windows 95 or on versions of Microsoft Windows NT prior
// to Microsoft Windows 2000.
//
// The following code sample divides a window into three horizontal areas. Then it draws
// an alpha-blended bitmap in each of the window areas.

{ Not used currently. For fpc we would have to use Win32extra.pas or jwawingdi.pas for AlphaBlend
const
 AC_SRC_ALPHA = $1;

procedure DrawAlphaBlend (hWnd : HWND;  hdcwnd : HDC);
var
    Ahdc : HDC;              // handle of the DC we will create
    bf : BLENDFUNCTION;      // structure for alpha blending
    Ahbitmap : HBITMAP;      // bitmap handle
    bmi : BITMAPINFO;        // bitmap header
    pvBits : pointer;        // pointer to DIB section
    ulWindowWidth,
    ulWindowHeight : ULONG;  // window width/height
    ulBitmapWidth,
    ulBitmapHeight : ULONG; // bitmap width/height
    rt : TRect;             // used for getting window dimensions
begin
    // get window dimensions
    GetClientRect(hWnd, rt);

    // calculate window width/height
    ulWindowWidth := rt.right - rt.left;
    ulWindowHeight := rt.bottom - rt.top;

    // make sure we have at least some window size
    if ((ulWindowWidth = 0 ) and  (ulWindowHeight=0)) then
        exit;

    // divide the window into 3 horizontal areas
    ulWindowHeight := trunc(ulWindowHeight / 3);

    // create a DC for our bitmap -- the source DC for AlphaBlend
    Ahdc := CreateCompatibleDC(hdcwnd);

    // zero the memory for the bitmap info
    ZeroMemory(@bmi, sizeof(BITMAPINFO));

    // setup bitmap info
    bmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth := trunc(ulWindowWidth - (ulWindowWidth/5)*2);
    ulBitmapWidth := trunc(ulWindowWidth - (ulWindowWidth/5)*2);
    bmi.bmiHeader.biHeight := trunc(ulWindowHeight - (ulWindowHeight/5)*2);
    ulBitmapHeight := trunc(ulWindowHeight - (ulWindowHeight/5)*2);
    bmi.bmiHeader.biPlanes := 1;
    bmi.bmiHeader.biBitCount := 32;         // four 8-bit components
    bmi.bmiHeader.biCompression := BI_RGB;
    bmi.bmiHeader.biSizeImage := ulBitmapWidth * ulBitmapHeight * 4;

    // create our DIB section and select the bitmap into the dc
    Ahbitmap := CreateDIBSection(Ahdc, bmi, DIB_RGB_COLORS, pvBits, 0, 0);
    SelectObject(Ahdc, Ahbitmap);

    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.SourceConstantAlpha := $7f;  // half of 0xff = 50% transparency
    bf.AlphaFormat := 0;             // ignore source alpha channel

    Windows.AlphaBlend(hdcwnd, trunc(ulWindowWidth/5), trunc(ulWindowHeight/5),
                    ulBitmapWidth, ulBitmapHeight,
                    Ahdc, 0, 0, ulBitmapWidth, ulBitmapHeight, bf);


    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.AlphaFormat := AC_SRC_ALPHA;  // use source alpha
    bf.SourceConstantAlpha := $ff;  // opaque (disable constant alpha)

    Windows.AlphaBlend(hdcwnd, trunc(ulWindowWidth/5),
     trunc(ulWindowHeight/5+ulWindowHeight), ulBitmapWidth, ulBitmapHeight,
      Ahdc, 0, 0, ulBitmapWidth, ulBitmapHeight, bf);

    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.AlphaFormat := 0;
    bf.SourceConstantAlpha := $3A;

    Windows.AlphaBlend(hdcwnd, trunc(ulWindowWidth/5),
               trunc(ulWindowHeight/5+2*ulWindowHeight), ulBitmapWidth,
               ulBitmapHeight, Ahdc, 0, 0, ulBitmapWidth,
               ulBitmapHeight, bf);

    // do cleanup
    DeleteObject(Ahbitmap);
    DeleteDC(Ahdc);

end;
}

// See also JVCL JvCaptionButton:
// uses AlphaBlend and TransparentBlt functions

end.
