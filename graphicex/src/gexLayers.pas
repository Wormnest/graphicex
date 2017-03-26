{ gexLayers Unit implementing base classes for handling images with layers.
  Based on TPhotoshopLayer and TPhotoshopLayers from GraphicEx.pas.
  License: MPL 1.1
  Portions created by Mike Lischke are
  Copyright (C) 1999, 2008 Mike Lischke. All Rights Reserved.
  Portions Created by Jacob Boerema are Copyright (C) 2013-2015 Jacob Boerema.
  All Rights Reserved.
  This fork of GraphicEx can be found at https://bitbucket.org/jacobb/graphicex
}

unit gexLayers;

interface

{$IFDEF FPC}
  {$mode delphi}
{$ELSE}
  {$Include Compilers.inc}
{$ENDIF}

uses Classes,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.Types,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  GraphicEx;

type
  // A single layer inside an image
  TgexLayer = class
  private
    FGraphic: TGraphicExGraphic;

    // Add some common layer properties that we can expect most layers to have.
    FName: WideString;          // Name of layer
    FWidth,                     // Width of layer
    FHeight: Integer;           // Height of layer
    FOffsetX,                   // X starting offset relative to image
    FOffsetY: Integer;          // Y starting offset relative to image
    FIsVisible: Boolean;        // Is this layer visible
    FOpacity: Byte;             // Opacity of this layer
  public

    constructor Create(Graphic: TGraphicExGraphic); virtual;
    destructor Destroy; override;

    property Graphic: TGraphicExGraphic read FGraphic;      // READ ONLY
    property Name: WideString read FName write FName;       // Name of layer
    property Width: Integer read FWidth write FWidth;       // Width of layer
    property Height: Integer read FHeight write FHeight;    // Height of layer
    property OffsetX: Integer read FOffsetX write FOffsetX; // X starting offset relative to image
    property OffsetY: Integer read FOffsetY write FOffsetY; // Y starting offset relative to image
    property IsVisible: Boolean read FIsVisible write FIsVisible; // Is this layer visible
    property Opacity: Byte read FOpacity write FOpacity;    // Opacity of this layer
  end;

  TgexLayerClass = class of TgexLayer;

  // Class to hold a list of all layers in an image
  TgexLayers = class(TList)
  private
    FGraphic: TGraphicExGraphic;
    FLayerClass: TgexLayerClass;
  protected
    function Get(Index: Integer): TgexLayer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Put(Index: Integer; Layer: TgexLayer);
  public
    constructor Create(Graphic: TGraphicExGraphic; LayerClass: TgexLayerClass);

    function Add(Layer: TgexLayer): Integer;
    function AddNewLayer: TgexLayer;
    function Extract(Layer: TgexLayer): TgexLayer;
    function First: TgexLayer;
    function IndexOf(Layer: TgexLayer): Integer;
    procedure Insert(Index: Integer; Layer: TgexLayer);
    function Last: TgexLayer;
    function Remove(Layer: TgexLayer): Integer;

    property Graphic: TGraphicExGraphic read FGraphic;      // READ ONLY
    property Items[Index: Integer]: TgexLayer read Get write Put; default;
  end;



implementation

//----------------- TgexLayer --------------------------------------------------

constructor TgexLayer.Create(Graphic: TGraphicExGraphic);
begin
  FGraphic := Graphic;
end;

destructor TgexLayer.Destroy;
begin
  inherited;
end;

//----------------- End of TgexLayer -------------------------------------------

//----------------- TgexLayers -------------------------------------------------

constructor TgexLayers.Create(Graphic: TGraphicExGraphic; LayerClass: TgexLayerClass);
begin
  inherited Create; // todo: not needed for Delphi? Check!
  FGraphic := Graphic;
  FLayerClass := LayerClass;
  Capacity := 16; // Set a default layer amount
end;

function TgexLayers.Get(Index: Integer): TgexLayer;
begin
  Result := TgexLayer(inherited Get(Index));
end;

procedure TgexLayers.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and Assigned(Ptr) then
    TgexLayer(Ptr).Free;
  inherited;
end;

procedure TgexLayers.Put(Index: Integer; Layer: TgexLayer);
begin
  inherited Put(Index, Layer);
end;

function TgexLayers.Add(Layer: TgexLayer): Integer;
begin
  Result := inherited Add(Layer);
end;

function TgexLayers.AddNewLayer: TgexLayer;
begin
  Result := FLayerClass.Create(FGraphic);
  inherited Add(Result);
end;

function TgexLayers.Extract(Layer: TgexLayer): TgexLayer;
begin
  Result := inherited Extract(Layer);
end;

function TgexLayers.First: TgexLayer;
begin
  Result := TgexLayer(inherited First);
end;

function TgexLayers.IndexOf(Layer: TgexLayer): Integer;
begin
  Result := inherited IndexOf(Layer);
end;

procedure TgexLayers.Insert(Index: Integer; Layer: TgexLayer);
begin
  inherited Insert(Index, Layer);
end;

function TgexLayers.Last: TgexLayer;
begin
  Result := TgexLayer(inherited Last);
end;

function TgexLayers.Remove(Layer: TgexLayer): Integer;
begin
  Result := inherited Remove(Layer);
  Layer.Free;
end;

//----------------- End of TgexLayers ------------------------------------------

end.
