unit u_screen_spritebank;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template, u_spritebank,
  u_surface_list, u_texture_list, u_collisionbody_list, u_posture_list;

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
  FBodies: TBodyItemList;
  FPostures: TPosturelist;
private
public
{  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); override; }
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint; var Handled: Boolean); override;

public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure ClearView;
  procedure ShowSprite(aIndex: integer);

  property Textures: TTextureList read FTextures;
  property Surfaces: TSpriteBankSurfaceList read FSurfaces;
  property Bodies: TBodyItemList read FBodies;
  property Postures: TPosturelist read FPostures;
end;

var ScreenSpriteBank: TScreenSpriteBank;

implementation

uses form_main;

{ TSpriteBankSurfaceList }

function TSpriteBankSurfaceList.Textures: TTextureList;
begin
  Result := ScreenSpriteBank.Textures;
end;

{ TScreenSpriteBank }

procedure TScreenSpriteBank.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);

  if FrameToolsSpriteBank.CBShowCollisionBody.Checked then
    Bodies.UpdateNodesPosition;
end;

procedure TScreenSpriteBank.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_SPRITEBANK, LAYER_COLLISION_BODY]);
  // camera
  CreateCamera([LAYER_SPRITEBANK]);
end;

procedure TScreenSpriteBank.FreeObjects;
begin
  FreeCamera;

  FBodies.Clear;
  FPostures.Clear;
  FSurfaces.Clear;
  FTextures.Clear;
end;

procedure TScreenSpriteBank.Initialize;
begin
  FTextures := TTextureList.Create;
  FSurfaces := TSpriteBankSurfaceList.Create;
  FSurfaces.WorkingLayer := LAYER_SPRITEBANK;
  FBodies := TBodyItemList.Create;
  FPostures := TPostureList.Create;

end;

procedure TScreenSpriteBank.Finalize;
begin
  FTextures.Free;
  FTextures := NIL;
  FSurfaces.Free;
  FSurfaces := NIL;
  FBodies.Free;
  FBodies := NIL;
  FPostures.Free;
  FPostures := NIL;
end;

procedure TScreenSpriteBank.ClearView;
begin
  FSurfaces.Clear;
  FTextures.Clear;
  FBodies.Clear;
  FPostures.Clear;
end;

procedure TScreenSpriteBank.ShowSprite(aIndex: integer);
begin
  FTextures.LoadFromString(SpriteBank.Mutable[aIndex]^.textures);
  FSurfaces.LoadFromString(SpriteBank.Mutable[aIndex]^.surfaces);
  if FrameToolsSpriteBank.CBShowCollisionBody.Checked then begin
    FBodies.LoadFromString(SpriteBank.Mutable[aIndex]^.collisionbodies);
    FBodies.SetParentSurface(Surfaces.GetRootItem^.surface);
  end else FBodies.Clear;
  FPostures.LoadFromString(SpriteBank.Mutable[aIndex]^.postures);
end;

end.

