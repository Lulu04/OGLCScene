unit u_screen_levelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template, u_spritebank,
  u_surface_list, u_texture_list, u_collisionbody_list, u_posture_list;

type

{ TScreenLevelBank }

TScreenLevelBank = class(TCustomScreenTemplate)
private
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

end;

var ScreenLevelBank: TScreenLevelBank;

implementation

{ TScreenLevelBank }

procedure TScreenLevelBank.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_LEVELBANK]);
end;

procedure TScreenLevelBank.FreeObjects;
begin

end;

procedure TScreenLevelBank.Initialize;
begin
  // camera
  CreateCamera([LAYER_LEVELBANK]);
end;

procedure TScreenLevelBank.Finalize;
begin
  FreeCamera;
end;

procedure TScreenLevelBank.ClearView;
begin

end;

procedure TScreenLevelBank.ShowLevel(aIndex: integer);
begin

end;

end.

