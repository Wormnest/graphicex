{ gexICC A ICC profile handling class for GraphicEx.
  Dual License: MPL 1.1 or LGPL 2.1 with linking exception (the "FPC modified LGPL License")
  Portions Created by Jacob Boerema are Copyright (C) 2017-2017 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
  Mirror: https://github.com/Wormnest/graphicex
}
unit gexICC;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$I GraphicConfiguration.inc}

uses lcms2dll;


type
  TICCLoadTarget = (iltSource, iltDest);

  TICCProfileManager = class(TObject)
  private
    FTransform: cmsHTRANSFORM;
    FSourceProfile: Pointer;
    FSourceProfileSize: Cardinal;
    // Dest profile not used currently. Added for future enhancements.
    FDestProfile: Pointer;
    FDestProfileSize: Cardinal;

    function GetProfileDescription(AProfile: Pointer; ASize: Cardinal): string;
    function GetSourceProfileDescription(): string;
  protected
    function LoadProfile(AProfile: Pointer; ASize: Cardinal;
      ALoadTarget: TICCLoadTarget = iltSource): Boolean;
    function CreateTransformSourceTosRGB(AColorMode: Cardinal): Boolean; overload;
    function CreateTransformSourceTosRGB(ASourceColorMode, ADestColorMode: Cardinal): Boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadSourceProfileFromMemory(ASource: Pointer; ASize: Cardinal): Boolean;
    function CreateTransformAnyTosRGB(ASourceColorMode: Cardinal; AWithAlpha: Boolean): Boolean;
    function CreateTransformTosRGB(AWithAlpha: Boolean): Boolean;
    function CreateTransformTosRGB_Gray8(): Boolean;
    function CreateTransformPalette(APlanar, AWithAlpha: Boolean): Boolean;
    procedure ExecuteTransform(ABuffer: Pointer; ASize: Cardinal); overload;
    procedure ExecuteTransform(ASourceBuffer, ADestBuffer: Pointer; ASize: Cardinal); overload;
    procedure DestroyTransform;

    property SourceProfileDescription: string read GetSourceProfileDescription;
  end;

implementation

constructor TICCProfileManager.Create;
begin
  inherited Create;
  FTransform := nil;
end;

destructor TICCProfileManager.Destroy;
begin
  DestroyTransform();
  if Assigned(FSourceProfile) then
    FreeMem(FSourceProfile);
  if Assigned(FDestProfile) then
    FreeMem(FDestProfile);
  inherited Destroy;
end;

function TICCProfileManager.LoadProfile(AProfile: Pointer; ASize: Cardinal;
  ALoadTarget: TICCLoadTarget = iltSource): Boolean;
var
  ATarget: Pointer;
begin
  Result := False;
  if not Assigned(AProfile) or (ASize = 0) then
    Exit;
  if ALoadTarget = iltSource then
    ATarget := FSourceProfile
  else
    ATarget := FDestProfile;
  if Assigned(ATarget) then
    FreeMem(ATarget);
  GetMem(ATarget, ASize);
  Move(AProfile^, ATarget^, ASize);
  if ALoadTarget = iltSource then begin
    FSourceProfile := ATarget;
    FSourceProfileSize := ASize;
  end
  else begin
    FDestProfile := ATarget;
    FDestProfileSize := ASize;
  end;
  Result := True;
end;

function TICCProfileManager.LoadSourceProfileFromMemory(ASource: Pointer; ASize: Cardinal): Boolean;
begin
  Result := LoadProfile(ASource, ASize, iltSource);
end;

function TICCProfileManager.GetProfileDescription(AProfile: Pointer; ASize: Cardinal): string;
var
  hp: cmsHPROFILE;
  SizeNeeded{, SizeResult}: Cardinal;
  Buf: Pointer;
begin
  Result := '';
  // Check if Profile is valid
  if (ASize > 0) and Assigned(AProfile) then begin
    hp := cmsOpenProfileFromMem(AProfile, ASize);
    // TODO: Do we need to support localized descriptions?
    // Get the buffer size needed.
    SizeNeeded := cmsGetProfileInfo(hp, cmsInfoDescription, 'en', 'US', nil, 0);
    if SizeNeeded > 0 then begin
      GetMem(Buf, SizeNeeded);
      try
        // Get the description, commented out SizeResult to remove hint: value never used.
        {SizeResult :=} cmsGetProfileInfo(hp, cmsInfoDescription, 'en', 'en', Buf, SizeNeeded);
        Result := PChar(Buf);
      finally
        FreeMem(Buf);
      end;
    end;
    cmsCloseProfile(hp);
  end;
end;

procedure TICCProfileManager.DestroyTransform();
begin
  if FTransform <> nil then begin
    cmsDeleteTransform(FTransform);
    FTransform := nil;
  end;
end;

function TICCProfileManager.CreateTransformSourceTosRGB(AColorMode: Cardinal): Boolean;
var
  SourceProfile, DestProfile: cmsHPROFILE;
begin
  // First close previous transform if present
  DestroyTransform();
  // Then open the source profile we read from the image
  SourceProfile := cmsOpenProfileFromMem(FSourceProfile, FsourceProfileSize);
  // Target for now is always screen sRGB
  DestProfile := cmsCreate_sRGBProfile();
  // Define the transform
  FTransform := cmsCreateTransform(
    SourceProfile, AColorMode,
    DestProfile, AColorMode,
    INTENT_PERCEPTUAL, 0);
  // Close profiles
  cmsCloseProfile(SourceProfile);
  cmsCloseProfile(DestProfile);
  Result := FTransform <> nil;
end;

function TICCProfileManager.CreateTransformSourceTosRGB(ASourceColorMode, ADestColorMode: Cardinal): Boolean;
var
  SourceProfile, DestProfile: cmsHPROFILE;
begin
  // First close previous transform if present
  DestroyTransform();
  // Then open the source profile we read from the image
  SourceProfile := cmsOpenProfileFromMem(FSourceProfile, FsourceProfileSize);
  // Target for now is always screen sRGB
  DestProfile := cmsCreate_sRGBProfile();
  // Define the transform
  FTransform := cmsCreateTransform(
    SourceProfile, ASourceColorMode,
    DestProfile, ADestColorMode,
    INTENT_PERCEPTUAL, 0);
  // Close profiles
  cmsCloseProfile(SourceProfile);
  cmsCloseProfile(DestProfile);
  Result := FTransform <> nil;
end;

function TICCProfileManager.CreateTransformAnyTosRGB(ASourceColorMode: Cardinal; AWithAlpha: Boolean): Boolean;
var
  TransformColorMode: Cardinal;
begin
  if AWithAlpha then
    TransformColorMode := TYPE_BGRA_8
  ELSE
    TransformColorMode := TYPE_BGR_8;
  Result := CreateTransformSourceTosRGB(ASourceColorMode, TransformColorMode);
end;

function TICCProfileManager.CreateTransformTosRGB(AWithAlpha: Boolean): Boolean;
var
  TransformColorMode: Cardinal;
begin
  if AWithAlpha then
    TransformColorMode := TYPE_BGRA_8
  ELSE
    TransformColorMode := TYPE_BGR_8;
  Result := CreateTransformSourceTosRGB(TransformColorMode);
end;

function TICCProfileManager.CreateTransformTosRGB_Gray8(): Boolean;
begin
  Result := CreateTransformSourceTosRGB(TYPE_GRAY_8);
end;

function TICCProfileManager.CreateTransformPalette(APlanar, AWithAlpha: Boolean): Boolean;
var
  TransformColorMode: Cardinal;
begin
  // Select correct color mode for palette
  if APlanar then begin
    if AWithAlpha then
      TransformColorMode := TYPE_BGRA_8_PLANAR
    else
      TransformColorMode := TYPE_BGR_8_PLANAR;
  end
  else begin
    if AWithAlpha then
      TransformColorMode := TYPE_BGRA_8
    else
      TransformColorMode := TYPE_BGR_8;
  end;
  Result := CreateTransformSourceTosRGB(TransformColorMode);
end;

procedure TICCProfileManager.ExecuteTransform(ABuffer: Pointer; ASize: Cardinal);
begin
  // We silently exit when no transform is defined.
  // That way no extra checks inside a loop have to be performed to see if
  // an ICC profile is present and whether transformation is wanted.
  if FTransform = nil then
    Exit;
  cmsDoTransform(FTransform, ABuffer, ABuffer, ASize);
end;

procedure TICCProfileManager.ExecuteTransform(ASourceBuffer, ADestBuffer: Pointer; ASize: Cardinal);
begin
  // We silently exit when no transform is defined.
  // That way no extra checks inside a loop have to be performed to see if
  // an ICC profile is present and whether transformation is wanted.
  if FTransform = nil then
    Exit;
  cmsDoTransform(FTransform, ASourceBuffer, ADestBuffer, ASize);
end;

function TICCProfileManager.GetSourceProfileDescription(): string;
begin
  Result := GetProfileDescription(FSourceProfile, FSourceProfileSize);
end;

end.
