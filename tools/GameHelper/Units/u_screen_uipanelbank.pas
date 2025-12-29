unit u_screen_uipanelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  u_screen_template, u_texture_list, u_surface_list;

type

{ TScreenUIPanelBank }

  TScreenUIPanelBank = class(TCustomScreenTemplate)
private
  FTextureList: TTexturelist;
  FSurfacesList: TSurfaceList;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;

  procedure ClearView;
  procedure ShowPanelFromBank(const aName: string);
end;

var ScreenUIPanelBank: TScreenUIPanelBank;

implementation

uses u_common, u_ui_objectlist;

{ TScreenUIPanelBank }

procedure TScreenUIPanelBank.CreateObjects;
begin
  ShowLayers([LAYER_TOP, LAYER_UI, LAYER_UIPANEL]);
  ShowSceneBounds;
  CreateCamera([LAYER_UIPANEL, LAYER_SCENEBOUNDS]);

  ZoomOnScene;
end;

procedure TScreenUIPanelBank.FreeObjects;
begin
  FTextureList.Clear;
  FSurfacesList.Clear;

  FreeCamera;
  FScene.Layer[LAYER_UIPANEL].Clear;
  HideSceneBounds;
end;

procedure TScreenUIPanelBank.Initialize;
begin
  FTextureList := TTexturelist.Create;
  FSurfacesList := TSurfaceList.Create;
  FSurfacesList.Textures := FTextureList;
  FSurfacesList.WorkingLayer := LAYER_UIPANEL;
end;

procedure TScreenUIPanelBank.Finalize;
begin
  FSurfacesList.Free;
  FSurfacesList := NIL;
  FTextureList.Free;
  FTextureList := NIL;
end;

function TScreenUIPanelBank.Surfaces: TSurfaceList;
begin
  Result := FSurfacesList;
end;

function TScreenUIPanelBank.Textures: TTextureList;
begin
  Result := FTextureList;
end;

procedure TScreenUIPanelBank.ClearView;
begin
  Surfaces.Clear;
  Textures.Clear;
end;

procedure TScreenUIPanelBank.ShowPanelFromBank(const aName: string);
var item: TPanelDescriptorItem;
begin
  FScene.MakeCurrent;
  ClearView;
  item := PanelBank.GetByName(aName);
  if item = NIL then exit;

  Textures.LoadFromString(item.textures);
  Surfaces.WorkingLayer := LAYER_UIPANEL;
  Surfaces.LoadFromString(item.surfaces);
end;

end.

