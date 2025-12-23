unit u_screen_uipanelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  u_screen_template, u_texture_list, u_surface_list;

type

{ TScreenUIPanelBank }

  TScreenUIPanelBank = class(TScreenWithSurfaceHandling)
private
  FModePanelEditor: boolean;
  FUIPanel: TUIPanelWithEffects;
  FTextureList: TTexturelist;
  FSurfacesList: TSurfaceList;
public
//  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
//  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
//  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
//  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
//  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;
  procedure SetFlagModified; override;


  procedure ClearView;
  procedure ShowPanelFromBank(const aName: string);
public
  property UIPanel: TUIPanelWithEffects read FUIPanel write FUIPanel;
  property ModePanelEditor: boolean read FModePanelEditor write FModePanelEditor;
end;

var ScreenUIPanelBank: TScreenUIPanelBank;

implementation

uses u_common, u_ui_objectlist, form_main;

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

procedure TScreenUIPanelBank.SetFlagModified;
begin
  FrameToolUIPanelEditor.Modified := True;
end;

procedure TScreenUIPanelBank.ClearView;
begin
  Surfaces.Clear;
  Textures.Clear;
  FUIPanel := NIL;
end;

procedure TScreenUIPanelBank.ShowPanelFromBank(const aName: string);
var item: TPanelDescriptorItem;
begin
  FScene.MakeCurrent;
  ClearView;
  item := PanelBank.GetByName(aName);
  if item = NIL then exit;

  Textures.LoadFromString(item.textures);
  Surfaces.LoadFromString(item.surfaces);
  FUIPanel := TUIPanelWithEffects(surfaces.GetRootItem^.surface);
end;

end.

