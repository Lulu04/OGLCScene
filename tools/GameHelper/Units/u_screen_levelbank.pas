unit u_screen_levelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  //OGLCScene,
  u_common, u_screen_template,
  u_surface_list, u_texture_list;

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
  procedure ShowLevel(aIndex: integer);

  property Surfaces: TLevelBankSurfaceList read FSurfaces;
end;

var ScreenLevelBank: TScreenLevelBank;

implementation

uses u_levelbank;

{ TLevelBankSurfaceList }

function TLevelBankSurfaceList.Textures: TTextureList;
begin
  Result := LevelBank.Textures;
end;

{ TScreenLevelBank }

procedure TScreenLevelBank.CreateObjects;
begin
  FSurfaces := TLevelBankSurfaceList.Create;
  FSurfaces.WorkingLayer := LAYER_LEVELBANK;
  ShowLayers([LAYER_UI, LAYER_LEVELBANK]);
  // camera
  CreateCamera([LAYER_LEVELBANK]);
end;

procedure TScreenLevelBank.FreeObjects;
begin
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
  FSurfaces.Clear;
end;

procedure TScreenLevelBank.ShowLevel(aIndex: integer);
begin
  FSurfaces.LoadFromString(LevelBank.Mutable[aIndex]^.surfaces);
end;

end.

