{
  gexGR32 This unit defines some types and functions from GR32.pas that are needed
          by gex(Basic)BlendModes.pas when the define USE_GR32 is NOT defined.
  License: same as Graphics32.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}

unit gexGR32Replacement;

interface

{$I gexdefines.inc}

type
  PColor32 = ^TColor32;
  TColor32 = type Cardinal;
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..0] of TColor32;

  // WARNING: Although M is defined as a TColor32, it is expected to
  // contain an alpha value in the range 0..255!
  TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: TColor32) of object;

// Color construction and conversion functions
function Color32(R, G, B: Byte; A: Byte = $FF): TColor32;

function RedComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function GreenComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function BlueComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}

// FOR NOW THESE FUNCTIONS, CHECK IF WE CAN USE GraphicColor ColorManager!
// Color space conversion
function HSLtoRGB(H, S, L: Single): TColor32;
procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);

implementation

uses Math;

function Color32(R, G, B: Byte; A: Byte = $FF): TColor32;
{$IFNDEF CPU64}
asm
        MOV  AH,A
        SHL  EAX,16
        MOV  AH,DL
        MOV  AL,CL
end;
{$ELSE}
begin
  Result := (A shl 24) or (R shl 16) or (G shl  8) or B;
end;
{$ENDIF}

function RedComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $00FF0000) shr 16;
end;

function GreenComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $0000FF00) shr 8;
end;

function BlueComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and $000000FF;
end;

{ Color space conversions }

function HSLtoRGB(H, S, L: Single): TColor32;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;

begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result := Color32(R, G, B, 255);
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := RedComponent(RGB) / 255;
  G := GreenComponent(RGB) / 255;
  B := BlueComponent(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D
    else
      if G = Cmax then H  := 2 + (B - R) / D
      else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1
  end;
end;


end.
