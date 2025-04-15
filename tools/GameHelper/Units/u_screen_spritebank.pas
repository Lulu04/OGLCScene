unit u_screen_spritebank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template, u_spritebank,
  u_surface_list, u_texture_list;

type

{ TSpriteBankSurfaceList }

TSpriteBankSurfaceList = class(TSurfaceList)
  function Textures: TTextureList; override;
end;

{ TScreenSpriteBank }

TScreenSpriteBank = class(TCustomScreenTemplate)
private
  FTextures: TTextureList;
  FSurfaces: TSpriteBankSurfaceList;
private
{public
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); override;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint; var Handled: Boolean); override;   }

public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure ClearView;
  procedure ShowSprite(aIndex: integer);

  property Textures: TTextureList read FTextures;
  property Surfaces: TSpriteBankSurfaceList read FSurfaces;
end;

var ScreenSpriteBank: TScreenSpriteBank;

implementation

{ TSpriteBankSurfaceList }

function TSpriteBankSurfaceList.Textures: TTextureList;
begin
  Result := ScreenSpriteBank.Textures;
end;

{ TScreenSpriteBank }

procedure TScreenSpriteBank.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_SPRITEBANK]);
  FContainer.MoveToLayer(LAYER_SPRITEBANK);
end;

procedure TScreenSpriteBank.FreeObjects;
begin
end;

procedure TScreenSpriteBank.Initialize;
begin
  FTextures := TTextureList.Create;
  FSurfaces := TSpriteBankSurfaceList.Create;

  // camera
  CreateCamera([LAYER_SPRITEBANK]);
end;

procedure TScreenSpriteBank.Finalize;
begin
  FreeCamera;
  FTextures.Free;
  FTextures := NIL;
  FSurfaces.Free;
  FSurfaces := NIL;
end;

procedure TScreenSpriteBank.ClearView;
begin
  FSurfaces.Clear;
  FTextures.Clear;
end;

procedure TScreenSpriteBank.ShowSprite(aIndex: integer);
begin
  FTextures.LoadFromString(SpriteBank.Mutable[aIndex]^.textures);
  FSurfaces.LoadFromString(SpriteBank.Mutable[aIndex]^.surfaces);
end;

end.

