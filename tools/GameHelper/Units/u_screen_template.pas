unit u_screen_template;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

type

TMouseState = ( msIdle,

                // sprite builder surface child
                msOverSurface,
                msMouseDownOnSurface,
                msMovingSelection,

                msOverScaleHandle,
                msMouseDownOnScaleHandle,
                msScalingSelection,

                msOverRotateHandle,
                msMouseDownOnRotateHandle,
                msRotatingSelection,

                msOverPivot,
                msMouseDownOnPivot,
                msMovePivotOnSelection,

                // sprite builder collision body
                msOverNode,
                msMouseDownOnNode,
                msMovingNode,

                msToolLine,
                msMouseDownOnToolLine,
                msCreatingLine,

                msToolCircle,
                msMouseDownOnToolCircle,
                msCreatingCircle,

                msToolRectangle,
                msMouseDownOnToolRectangle,
                msCreatingRectangle,

                msToolPolygon,
                msMouseDownOnToolPolygon,
                msCreatingPolygon,
                msWaitingForNextPolygonNode,
                msEnterPressedOnPolygonCreation
              );

{ TCustomScreenTemplate }

TCustomScreenTemplate = class(TScreenTemplate)
private
  FCamera: TOGLCCamera;
  FViewOffset: TPointF;
  FZoom: single;
  FSpacePressed: boolean;
  FCtrlPressed: boolean;
  procedure SetZoom(AValue: single);
  procedure SetViewOffset(AValue: TPointF);
private
  FMouseState: TMouseState;
  FClickOrigin: TPointF;
  procedure SetMouseState(AValue: TMouseState);
public // callback from main form and openglcontrol to manage mouse and keyboard on the view
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
  procedure ProcessOnKeyDown(var Key: Word; Shift: TShiftState); virtual;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); virtual;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
public // camera
  procedure CreateCamera(const aLayerIndexList: array of integer);
  procedure FreeCamera;
  property Camera: TOGLCCamera read FCamera;
  property ViewOffset: TPointF read FViewOffset write SetViewOffset;
  property Zoom: single read FZoom write SetZoom;
public // layers
  // show only the specified layers and hide the other
  procedure ShowLayers(const aLayerIndexList: array of integer);
public // mouse and keyboardinteraction on view
  function TransformCoor(aControlPt: TPointF): TPointF;
  property MouseState: TMouseState read FMouseState write SetMouseState;
  property ClickOrigin: TPointF read FClickOrigin write FClickOrigin;
  property SpacePressed: boolean read FSpacePressed;
  property CtrlPressed: boolean read FCtrlPressed;
public
  procedure AddToSpriteBank;
end;

implementation
uses LCLType, Controls, Math, u_common, form_main, u_ui_atlas;

{ TCustomScreenTemplate }

procedure TCustomScreenTemplate.SetZoom(AValue: single);
begin
  FZoom := EnsureRange(AValue, 0.1, 5.0);
  FCamera.Scale.Value := PointF(FZoom, FZoom);
end;

procedure TCustomScreenTemplate.SetViewOffset(AValue: TPointF);
begin
  FViewOffset := AValue;
  FCamera.MoveTo(FScene.Center + AValue);
end;

procedure TCustomScreenTemplate.SetMouseState(AValue: TMouseState);
var tex: PTexture;
begin
  if FMouseState = AValue then Exit;
  FMouseState := AValue;
  case FMouseState of
    msIdle: tex := texMouseNormal; // FormMain.OGL.Cursor := crDefault;

    msOverSurface: tex := texMouseOverSurface;
    msMouseDownOnSurface: tex := texMouseOverSurface;
    msMovingSelection: tex := texMouseOverSurface;

    msOverRotateHandle: tex := texMouseRotateSurface;
    msRotatingSelection: tex := texMouseRotateSurface;

    msOverPivot: tex := texMouseOverPivot;
    msMovePivotOnSelection: tex := texMouseOverPivot;

    msOverScaleHandle: tex := texMouseScaleSurface;
    msMouseDownOnScaleHandle: tex := texMouseScaleSurface;
    msScalingSelection: tex := texMouseScaleSurface;

    msOverNode: tex := texMouseOverNode;
    msMouseDownOnNode, msMovingNode: tex := texMouseMovingNode;

    msToolLine,
    msCreatingLine: tex := texMouseToolLine;

    msToolCircle,
    msCreatingCircle: tex := texMouseToolCircle;

    msToolRectangle,
    msCreatingRectangle: tex := texMouseToolRectangle;

    msToolPolygon,
    msCreatingPolygon: tex := texMouseToolPolygon;
    msWaitingForNextPolygonNode: tex := texMouseAddNode;
    else tex := texMouseNormal;
  end;

  FScene.Mouse.SetCursorSprite(tex, False, PointF(0,0));
end;

procedure TCustomScreenTemplate.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TCustomScreenTemplate.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TCustomScreenTemplate.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TCustomScreenTemplate.ProcessOnKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: FSpacePressed := True;
    VK_CONTROL: FCtrlPressed := True;
  end;
end;

procedure TCustomScreenTemplate.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: FSpacePressed := False;
    VK_CONTROL: FCtrlPressed := False;
  end;
end;

procedure TCustomScreenTemplate.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var p1, p2, p: TPointF;
  amount: single;
begin
  if Handled then exit;

  if Shift = [] then begin
    // scroll vertically
    amount := FCamera.GetViewRect.Height*0.1;
    if WheelDelta < 0
      then ViewOffset := ViewOffset + PointF(0, amount)
      else ViewOffset := ViewOffset - PointF(0, amount);
  end
  else
  if ssShift in Shift then begin
    // scroll horizontally
    amount := FCamera.GetViewRect.Width*0.1;
    if WheelDelta < 0
      then ViewOffset := ViewOffset - PointF(amount, 0)
      else ViewOffset := ViewOffset + PointF(amount, 0);
  end
  else
  if ssCtrl in Shift then begin
    // zoom
    p1 := FCamera.ControlToWorld(MousePos.x, MousePos.y);
    if WheelDelta < 0
      then Zoom := Zoom - Zoom*0.2
      else Zoom := Zoom + Zoom*0.2;
    // moves the view
    p2 := FCamera.ControlToWorld(MousePos.x, MousePos.y);
    p := p2 - p1;
    ViewOffset := ViewOffset - p;
  end;

  Handled := True;
end;

procedure TCustomScreenTemplate.CreateCamera(
  const aLayerIndexList: array of integer);
begin
  FCamera := FScene.CreateCamera;
  FCamera.AssignToLayer(aLayerIndexList);
  FCamera.MoveTo(PointF(0,0));
  FViewOffset := -FCamera.LookAt.Value;
  Zoom := 1.0;
end;

procedure TCustomScreenTemplate.FreeCamera;
begin
  FScene.KillCamera(FCamera);
end;

procedure TCustomScreenTemplate.ShowLayers(const aLayerIndexList: array of integer);
var i: integer;
  function IndexInArray(aindex: integer): boolean;
  var j: integer;
  begin
    for j:=0 to High(aLayerIndexList) do
      if aLayerIndexList[j] = aIndex then exit(True);
    Result := False;
  end;
begin
  for i:=0 to LAYER_COUNT-1 do
    if IndexInArray(i) then FScene.Layer[i].Opacity.ChangeTo(255, 0.5)
      else FScene.Layer[i].Opacity.ChangeTo(0, 0.5);
end;

function TCustomScreenTemplate.TransformCoor(aControlPt: TPointF): TPointF;
begin
  if FCamera <> NIL
    then Result := FCamera.ControlToWorld(aControlPt)
    else Result := aControlPt;
end;

procedure TCustomScreenTemplate.AddToSpriteBank;
begin

end;

end.

