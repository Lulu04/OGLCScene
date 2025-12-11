unit u_screen_uipaneleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  u_screen_template, u_texture_list, u_surface_list;

type

{ TScreenUIPanelEditor }

TScreenUIPanelEditor = class(TScreenWithSurfaceHandling)
private
  FUIPanel: TUIPanel;
  FTextureList: TTexturelist;
public
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;
  procedure SetFlagModified; override;

public
  property UIPanel: TUIPanel read FUIPanel;

end;

var ScreenUIPanelEditor: TScreenUIPanelEditor;

implementation

uses u_common, u_utils, form_main;

{ TScreenUIPanelEditor }

procedure TScreenUIPanelEditor.ProcessMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseUp(Button, Shift, X, Y);
end;

procedure TScreenUIPanelEditor.ProcessMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseDown(Button, Shift, X, Y);
end;

procedure TScreenUIPanelEditor.ProcessMouseMove(Shift: TShiftState; X, Y: Integer
  );
begin
  inherited ProcessMouseMove(Shift, X, Y);
end;

procedure TScreenUIPanelEditor.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
end;

procedure TScreenUIPanelEditor.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
end;

procedure TScreenUIPanelEditor.CreateObjects;
begin
  ShowLayers([LAYER_TOP, LAYER_UI, LAYER_UIPANEL]);
  ShowSceneBounds;
  CreateCamera([LAYER_UIPANEL, LAYER_SCENEBOUNDS]);

  // create the main panel
  FUIPanel := TUIPanel.Create(FScene);
  FScene.Add(FUIPanel, LAYER_UIPANEL);
  FUIPanel.CenterOnScene;
  FUIPanel.BodyShape.SetShapeRectangle(200, 200, 1.5);


  // link our panel to the editor
  FrameToolUIPanelEditor.TargetUIPanel := FUIPanel;
end;

procedure TScreenUIPanelEditor.FreeObjects;
begin
  // unlink our panel from the frame
  FrameToolUIPanelEditor.TargetUIPanel := NIL;

  FTextureList.Clear;

  FreeCamera;
  FScene.Layer[LAYER_UIPANEL].Clear;
  HideSceneBounds;
end;

procedure TScreenUIPanelEditor.Initialize;
begin
  FTextureList := TTexturelist.Create;

end;

procedure TScreenUIPanelEditor.Finalize;
begin
  FTextureList.Free;
  FTextureList := NIL;
end;

function TScreenUIPanelEditor.Surfaces: TSurfaceList;
begin
  Result := NIL;
end;

function TScreenUIPanelEditor.Textures: TTextureList;
begin
  Result := FTextureList;
end;

procedure TScreenUIPanelEditor.SetFlagModified;
begin
  FrameToolUIPanelEditor.Modified := True;
end;

end.

