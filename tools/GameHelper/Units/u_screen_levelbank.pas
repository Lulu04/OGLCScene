unit u_screen_levelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  //OGLCScene,
  u_common, u_screen_template,
  u_surface_list, u_texture_list, OGLCScene, u_levelbank;

type

{ TLevelBankSurfaceList }

TLevelBankSurfaceList = class(TSurfaceList)
  function Textures: TTextureList; override;
end;


{ TScreenLevelBank }

TScreenLevelBank = class(TCustomScreenTemplate)
private
  FSurfaces: TLevelBankSurfaceList;
public
{  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); override;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint; var Handled: Boolean); override;  }

public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure ClearView;
  procedure ShowLevel(aLevel: PLevelBankItem);

  property Surfaces: TLevelBankSurfaceList read FSurfaces;
end;

var ScreenLevelBank: TScreenLevelBank;

implementation

uses u_layerlist;

{ TLevelBankSurfaceList }

function TLevelBankSurfaceList.Textures: TTextureList;
begin
  Result := WorkingLevelGroup.Textures;
end;

{ TScreenLevelBank }

procedure TScreenLevelBank.CreateObjects;
var A: TArrayOfInteger;
begin
  FSurfaces := TLevelBankSurfaceList.Create;
  FSurfaces.SetModeForLevelEditor;
  FSurfaces.WorkingLayer := LAYER_LEVELBANK;

  A := Layers.GetUserLayerIndexes;
  system.Insert(LAYER_LEVELBANK, A, Length(A));
  ShowLayers(A);

  // camera
  A := Layers.GetUserLayerIndexes;
  system.Insert(LAYER_LEVELBANK, A, Length(A));
  CreateCamera(A);
end;

procedure TScreenLevelBank.FreeObjects;
begin
  FSurfaces.Clear;
  FSurfaces.Free;
  FSurfaces := NIL;
  FreeCamera;
end;

procedure TScreenLevelBank.Initialize;
begin
end;

procedure TScreenLevelBank.Finalize;
begin
end;

procedure TScreenLevelBank.ClearView;
begin
  if Assigned(FSurfaces) then
    FSurfaces.Clear;
end;

procedure TScreenLevelBank.ShowLevel(aLevel: PLevelBankItem);
begin
  FSurfaces.LoadFromString(aLevel^.surfaces);
  ZoomViewToFit(Surfaces.GetItemsBounds, 0.8);
end;

end.

