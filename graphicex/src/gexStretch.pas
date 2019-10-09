{$TYPEDADDRESS OFF}

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
// Portions created by Jacob Boerema are
// Copyright (C) 2013-2015 Jacob Boerema. All Rights Reserved.
// This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex

unit gexStretch;

interface

{$I gexdefines.inc}

uses Graphics, gexTypes;

type
  // Resampling support types.
  TResamplingFilter = (
    sfBox,
    sfTriangle,
    sfHermite,
    sfBell,
    sfSpline,
    sfLanczos3,
    sfMitchell
  );

// Resampling support routines.
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
    Radius: Single; Source, Target: TBitmap); overload;
procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter;
    Radius: Single; Source: TBitmap); overload;


implementation

uses Math, // Ceil, Floor
  GraphicColor; // color record definitions

resourcestring
  ResStretchInvalidPixelFormat = 'Bitmap must be 24 or 32 bpp!';

type
  // Stretch filter function
  TFilterFunction = function(Value: Single): Single;

  // Contributor for a Pixel
  PContributor = ^TContributor;
  TContributor = record
    Weight: Integer; // Pixel Weight
    Pixel: Integer; // Source Pixel
  end;

  TContributors = array of TContributor;

  // List of source pixels contributing to a destination pixel
  TContributorEntry = record
    N: Integer;
    Contributors: TContributors;
  end;

  TContributorList = array of TContributorEntry;

const
  DefaultFilterRadius: array[TResamplingFilter] of Single = (0.5, 1, 1, 1.5, 2, 3, 2);

threadvar // globally used cache for current image (speeds up resampling about 10%)
  CurrentLineR: array of Integer;
  CurrentLineG: array of Integer;
  CurrentLineB: array of Integer;
  CurrentLineA: array of Integer;

//----------------- filter functions for stretching --------------------------------------------------------------------

function HermiteFilter(Value: Single): Single;

// f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
    Result := (2 * Value - 3) * Sqr(Value) + 1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BoxFilter(Value: Single): Single;

// This filter is also known as 'nearest neighbour' Filter.

begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TriangleFilter(Value: Single): Single;

// aka 'linear' or 'bilinear' filter

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
    Result := 1 - Value
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function BellFilter(Value: Single): Single;

begin
  if Value < 0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
    if Value < 1.5 then
    begin
      Value := Value - 1.5;
      Result := 0.5 * Sqr(Value);
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SplineFilter(Value: Single): Single;

// B-spline filter

var
  Temp: Single;

begin
  if Value < 0 then
    Value := -Value;
  if Value < 1 then
  begin
    Temp := Sqr(Value);
    Result := 0.5 * Temp * Value - Temp + 2 / 3;
  end
  else
    if Value < 2 then
    begin
      Value := 2 - Value;
      Result := Sqr(Value) * Value / 6;
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function Lanczos3Filter(Value: Single): Single;

  //--------------- local function --------------------------------------------

  function SinC(Value: Single): Single;

  begin
    if Value <> 0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value;
    end
    else
      Result := 1;
  end;

  //---------------------------------------------------------------------------

begin
  if Value < 0 then
    Value := -Value;
  if Value < 3 then
    Result := SinC(Value) * SinC(Value / 3)
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function MitchellFilter(Value: Single): Single;

const
  B = 1 / 3;
  C = 1 / 3;

var Temp: Single;

begin
  if Value < 0 then
    Value := -Value;
  Temp := Sqr(Value);
  if Value < 1 then
  begin
    Value := (((12 - 9 * B - 6 * C) * (Value * Temp))
             + ((-18 + 12 * B + 6 * C) * Temp)
             + (6 - 2 * B));
    Result := Value / 6;
  end
  else
    if Value < 2 then
    begin
      Value := (((-B - 6 * C) * (Value * Temp))
               + ((6 * B + 30 * C) * Temp)
               + ((-12 * B - 48 * C) * Value)
               + (8 * B + 24 * C));
      Result := Value / 6;
    end
    else
      Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  FilterList: array[TResamplingFilter] of TFilterFunction = (
    BoxFilter,
    TriangleFilter,
    HermiteFilter,
    BellFilter,
    SplineFilter,
    Lanczos3Filter,
    MitchellFilter
  );

//----------------------------------------------------------------------------------------------------------------------

procedure FillLineCache(N, Delta: Integer; Line: Pointer; UseAlphaChannel: Boolean);

var
  I: Integer;
  Run: PBGRA;

begin
  Run := Line;
  if UseAlphaChannel then
  begin
    for I := 0 to N - 1 do
    begin
      CurrentLineR[I] := Run.R;
      CurrentLineG[I] := Run.G;
      CurrentLineB[I] := Run.B;
      CurrentLineA[I] := Run.A;
      Inc(PByte(Run), Delta);
    end;
  end
  else
  begin
    for I := 0 to N - 1 do
    begin
      CurrentLineR[I] := Run.R;
      CurrentLineG[I] := Run.G;
      CurrentLineB[I] := Run.B;
      Inc(PByte(Run), Delta);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ApplyContributors(N: Integer; Contributors: TContributors; UseAlphaChannel: Boolean): TBGRA;

var
  J: Integer;
  RGB: TRGBAInt;
  Total,
  Weight: Integer;
  Pixel: Cardinal;
  Contr: ^TContributor;

begin
  RGB.R := 0;
  RGB.G := 0;
  RGB.B := 0;
  RGB.A := 0;
  Total := 0;
  Contr := @Contributors[0];

  if UseAlphaChannel then
  begin
    for J := 0 to N - 1 do
    begin
      Weight := Contr.Weight;
      Inc(Total, Weight);
      Pixel := Contr.Pixel;
      Inc(RGB.R, CurrentLineR[Pixel] * Weight);
      Inc(RGB.G, CurrentLineG[Pixel] * Weight);
      Inc(RGB.B, CurrentLineB[Pixel] * Weight);
      Inc(RGB.A, CurrentLineA[Pixel] * Weight);

      Inc(Contr);
    end;
  end
  else
  begin
    for J := 0 to N - 1 do
    begin
      Weight := Contr.Weight;
      Inc(Total, Weight);
      Pixel := Contr.Pixel;
      Inc(RGB.R, CurrentLineR[Pixel] * Weight);
      Inc(RGB.G, CurrentLineG[Pixel] * Weight);
      Inc(RGB.B, CurrentLineB[Pixel] * Weight);

      Inc(Contr);
    end;
  end;

  if Total = 0 then
  begin
    Result.R := ClampByte(RGB.R shr 8);
    Result.G := ClampByte(RGB.G shr 8);
    Result.B := ClampByte(RGB.B shr 8);
    if UseAlphaChannel then
      Result.A := ClampByte(RGB.A shr 8);
  end
  else
  begin
    Result.R := ClampByte(RGB.R div Total);
    Result.G := ClampByte(RGB.G div Total);
    Result.B := ClampByte(RGB.B div Total);
    if UseAlphaChannel then
      Result.A := ClampByte(RGB.A div Total);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DoStretch(Filter: TFilterFunction; Radius: Single; Source, Target: TBitmap);

// This is the actual scaling routine. Target must be allocated already with sufficient size. Source must
// contain valid data, Radius must not be 0 and Filter must not be nil.

var
  ScaleX,
  ScaleY: Single;   // Zoom scale factors
  I, J,
  K, N: Integer;    // Loop variables
  Center: Single;   // Filter calculation variables
  Width: Single;
  Weight: Integer;  // Filter calculation variables
  Left,
  Right: Integer;   // Filter calculation variables
  Work: TBitmap;
  ContributorList: TContributorList;

  SourceLine, DestLine: Pointer; // either points to BGR or BGRA structure
  DestPixel: Pointer;
  UseAlphaChannel: Boolean;
  PixelSize: Integer;
  ContributorResult: TBGRA;

  Delta, DestDelta: Integer;
  SourceHeight,
  SourceWidth,
  TargetHeight,
  TargetWidth: Integer;

begin
  // shortcut variables
  SourceHeight := Source.Height;
  SourceWidth := Source.Width;
  TargetHeight := Target.Height;
  TargetWidth := Target.Width;

  if (SourceHeight = 0) or (SourceWidth = 0) or
     (TargetHeight = 0) or (TargetWidth = 0) then
    Exit;

  UseAlphaChannel := Source.PixelFormat = pf32Bit;

  // Create intermediate image to hold horizontal zoom.
  Work := TBitmap.Create;
  Work.Canvas.Lock;
  try
    Work.PixelFormat := Source.PixelFormat;

    Work.Height := SourceHeight;
    Work.Width := TargetWidth;
    if (SourceWidth = 1) or (TargetWidth = 1) then
      ScaleX :=  TargetWidth / SourceWidth
    else
      ScaleX :=  (TargetWidth - 1) / (SourceWidth - 1);
    if (SourceHeight = 1) or (TargetHeight = 1) then
      ScaleY :=  TargetHeight / SourceHeight
    else
      ScaleY :=  (TargetHeight - 1) / (SourceHeight - 1);

    // pre-calculate filter contributions for a row
    SetLength(ContributorList, TargetWidth);
    // horizontal sub-sampling
    if ScaleX < 1 then
    begin
      // scales from bigger to smaller Width
      Width := Radius / ScaleX;
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Width + 1)));
        Center := I / ScaleX;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleX) * ScaleX * 256);
          if Weight <> 0 then
          begin
            if J < 0 then begin
              N := -J;
              if N >= SourceWidth then // This check is needed for width = 1
                N := SourceWidth + J + SourceWidth - 1;
            end
            else
              if J >= SourceWidth then
                N := SourceWidth - J + SourceWidth - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end
    else
    begin
      // horizontal super-sampling
      // scales from smaller to bigger Width
      for I := 0 to TargetWidth - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Radius + 1)));
        Center := I / ScaleX;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then begin
              N := -J;
              if N >= SourceWidth then // This check is needed for width = 1
                N := SourceWidth + J + SourceWidth - 1;
            end
            else
              if J >= SourceWidth then
                N := SourceWidth - J + SourceWidth - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // now apply filter to sample horizontally from Src to Work
    SetLength(CurrentLineR, SourceWidth);
    SetLength(CurrentLineG, SourceWidth);
    SetLength(CurrentLineB, SourceWidth);
    if UseAlphaChannel then
    begin
      SetLength(CurrentLineA, SourceWidth);
      PixelSize := 4;
    end
    else
      PixelSize := 3;
    for K := 0 to SourceHeight - 1 do
    begin
      SourceLine := Source.ScanLine[K];
      FillLineCache(SourceWidth, PixelSize, SourceLine, UseAlphaChannel);
      DestPixel := Work.ScanLine[K];
      for I := 0 to TargetWidth - 1 do
        with ContributorList[I] do
        begin
          ContributorResult := ApplyContributors(N, ContributorList[I].Contributors, UseAlphaChannel);
          Move(ContributorResult, DestPixel^, PixelSize);
          // move on to next column
          Inc(PByte(DestPixel), PixelSize);
        end;
    end;

    // free the memory allocated for horizontal filter weights, since we need the stucture again
    for I := 0 to TargetWidth - 1 do
      ContributorList[I].Contributors := nil;
    ContributorList := nil;

    // pre-calculate filter contributions for a column
    SetLength(ContributorList, TargetHeight);
    // vertical sub-sampling
    if ScaleY < 1 then
    begin
      // scales from bigger to smaller height
      Width := Radius / ScaleY;
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Width + 1)));
        Center := I / ScaleY;
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Round(Filter((Center - J) * ScaleY) * ScaleY * 256);
          if Weight <> 0 then
          begin
            if J < 0 then begin
              N := -J;
              if N >= SourceHeight then // This check is needed for height = 1
                N := SourceHeight + J + SourceHeight - 1;
            end
            else
              if J >= SourceHeight then
                N := SourceHeight - J + SourceHeight - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end
    end
    else
    begin
      // vertical super-sampling
      // scales from smaller to bigger height
      for I := 0 to TargetHeight - 1 do
      begin
        ContributorList[I].N := 0;
        SetLength(ContributorList[I].Contributors, Ceil(2 * (Radius + 1)));
        Center := I / ScaleY;
        Left := Floor(Center - Radius);
        Right := Ceil(Center + Radius);
        for J := Left to Right do
        begin
          Weight := Round(Filter(Center - J) * 256);
          if Weight <> 0 then
          begin
            if J < 0 then begin
              N := -J;
              if N >= SourceHeight then // This check is needed for height = 1
                N := SourceHeight + J + SourceHeight - 1;
            end
            else
              if J >= SourceHeight then
                N := SourceHeight - J + SourceHeight - 1
              else
                N := J;
            K := ContributorList[I].N;
            Inc(ContributorList[I].N);
            ContributorList[I].Contributors[K].Pixel := N;
            ContributorList[I].Contributors[K].Weight := Weight;
          end;
        end;
      end;
    end;

    // apply filter to sample vertically from Work to Target
    SetLength(CurrentLineR, SourceHeight);
    SetLength(CurrentLineG, SourceHeight);
    SetLength(CurrentLineB, SourceHeight);
    if UseAlphaChannel then
    begin
      SetLength(CurrentLineA, SourceHeight);
      PixelSize := 4;
    end
    else
      PixelSize := 3;

    SourceLine := Work.ScanLine[0];
    // For Source or Dest with Height 1 we can't use Scanline[1] to compute Delta.
    // Since Delta in these cases won't be used anyway we set it to 0.
    if SourceHeight > 1 then
      Delta := NativeInt(Work.ScanLine[1]) - NativeInt(SourceLine)
    else
      Delta := 0;
    DestLine := Target.ScanLine[0];
    if TargetHeight > 1 then
      DestDelta := NativeInt(Target.ScanLine[1]) - NativeInt(DestLine)
    else
      DestDelta := 0;
    for K := 0 to TargetWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      FillLineCache(SourceHeight, Delta, SourceLine, UseAlphaChannel);
      for I := 0 to TargetHeight - 1 do
        with ContributorList[I] do
        begin
          ContributorResult := ApplyContributors(N, ContributorList[I].Contributors, UseAlphaChannel);
          Move(ContributorResult, DestPixel^, PixelSize);
          // move on to next column
          Inc(PByte(DestPixel), DestDelta);
        end;
      Inc(PByte(SourceLine), PixelSize);
      Inc(PByte(DestLine), PixelSize);
    end;

    // free the memory allocated for vertical filter weights
    for I := 0 to TargetHeight - 1 do
      ContributorList[I].Contributors := nil;
    // this one is done automatically on exit, but is here for completeness
    ContributorList := nil;

  finally
    Work.Canvas.Unlock;
    Work.Free;
    CurrentLineR := nil;
    CurrentLineG := nil;
    CurrentLineB := nil;
    if UseAlphaChannel then
      CurrentLineA := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source, Target: TBitmap);

// Scales the source bitmap to the given size (NewWidth, NewHeight) and stores the Result in Target.
// Filter describes the filter function to be applied and Radius the size of the filter area.
// Is Radius = 0 then the recommended filter area will be used (see DefaultFilterRadius).

begin
  if Source.PixelFormat in [pfDevice, pf8Bit, pf16Bit] then
    raise EgexStretchException.Create(ResStretchInvalidPixelFormat);

  if Radius = 0 then
    Radius := DefaultFilterRadius[Filter];
  Target.Height := 0; // Avoid unnecessary image data copies.
  Target.PixelFormat := Source.PixelFormat;
  Target.Width := NewWidth;
  Target.Height := NewHeight;
  DoStretch(FilterList[Filter], Radius, Source, Target);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Stretch(NewWidth, NewHeight: Cardinal; Filter: TResamplingFilter; Radius: Single; Source: TBitmap);

var
  Target: TBitmap;

begin
  if Source.PixelFormat in [pfDevice, pf8Bit, pf16Bit] then
    raise EgexStretchException.Create(ResStretchInvalidPixelFormat);

  if Radius = 0 then
    Radius := DefaultFilterRadius[Filter];
  Target := TBitmap.Create;
  try
    Target.Width := NewWidth;
    Target.Height := NewHeight;
    Target.PixelFormat := Source.PixelFormat;
    DoStretch(FilterList[Filter], Radius, Source, Target);
    Source.Assign(Target);
  finally
    Target.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------


end.
