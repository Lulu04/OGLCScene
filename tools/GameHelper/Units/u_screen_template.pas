unit u_screen_template;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, u_surface_list, u_ui_handle;

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

                msToolPoint,
                msMouseDownOnToolPoint,
                msCreatingPoint,

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
                msBackPressedOnPolygonCreation,

                msCancelShapeCreation  // used when user pess ESCAPE while creating a collision shape
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
  FMoveRestriction: TPointF;
  procedure SetMouseState(AValue: TMouseState);
public // callback from main form and openglcontrol to manage mouse and keyboard on the view
  procedure ProcessMouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessMouseDown({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessOnKeyDown(var Key: Word; {%H-}Shift: TShiftState); virtual;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); virtual;
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
public // mouse and keyboard interaction on view
  function TransformCoor(aControlPt: TPointF): TPointF;
  procedure ComputeMoveRestriction(aPtOrigin, aPtNew: TPointF);
  procedure ApplyMoveRestrictionOn(var aPt: TPointF);
  property MouseState: TMouseState read FMouseState write SetMouseState;
  property ClickOrigin: TPointF read FClickOrigin write FClickOrigin;
  property SpacePressed: boolean read FSpacePressed;
  property CtrlPressed: boolean read FCtrlPressed;
  property MoveRestriction: TPointF read FMoveRestriction;
end;


{ TScreenWithSurfaceHandling }

TScreenWithSurfaceHandling = class(TCustomScreenTemplate)
//protected
  public
  FSelected: ArrayOfPSurfaceDescriptor;
  FAlternateOverlappedIndex: integer;  // used to chose between several overlapped objects
  FScaleHandleType: TScaleHandle;
  function GetSelectedCount: integer;
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor); virtual;
  function AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean; overload;
  function AlreadySelected(item: PSurfaceDescriptor): boolean; overload;
  procedure AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF); virtual;
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF); virtual;
  procedure SetPivotValuesOnSelection(aLocalPosX, aLocalPosY: single);
  procedure SaveCurrentAngleBeforeRotationOnSelection;
  procedure RotateSelection(aPreviousReferencePoint, aReferencePoint: TPointF; aUseIncrement: boolean); virtual;
  procedure SaveCurrentScaleValueBeforeScalingSelection;
  procedure ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean); virtual;
  procedure SaveCurrentPosAndSizeBeforeResizingSelection;
  procedure AddDeltaToSizeOnSelection(aDelta: TPoint; aKeepAspectRatio: boolean);
  procedure DeleteSelection; virtual;
  procedure UpdateHandlePositionOnSelected;
  function MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverScaleHandle(aWorldPt: TPointF): boolean;
protected
  procedure LoopMoveSelection;
  procedure LoopMovePivotOnSelection;
  procedure LoopScaleSelection; virtual;
  procedure LoopRotateSelection;
public
  procedure SelectNone; virtual;
  procedure SelectAll; virtual;

public // align
  function GetFirstSelectedX: single;
  function GetFirstSelectedCenterX: single;
  function GetFirstSelectedRightX: single;
  function GetFirstSelectedY: single;
  function GetFirstSelectedCenterY: single;
  function GetFirstSelectedBottomY: single;

  procedure AlignSelectedLeftTo(aX: single);
  procedure AlignSelectedHCenterTo(aX: single);
  procedure AlignSelectedRightTo(aX: single);
  procedure AlignSelectedTopTo(aY: single);
  procedure AlignSelectedVCenterTo(aY: single);
  procedure AlignSelectedBottomTo(aY: single);



  procedure ReverseAngleOnSelection;
  procedure ToogleScaledAndRotatedHandleOnSelection;
  procedure MoveSelection(aDelta: TPointF);
  procedure SetAngleOnSelection(aAngle: single);
  procedure ResetValuesOnSelection;



  function Surfaces: TSurfaceList; virtual; abstract;
  procedure SetFlagModified; virtual; abstract;

  property Selected: ArrayOfPSurfaceDescriptor read FSelected;
  property SelectedCount: integer read GetSelectedCount;
end;

implementation
uses Forms, LCLType, Controls, Math, u_common, u_ui_atlas, form_main;

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
debug:string;
begin
  if FMouseState = AValue then Exit;
  FMouseState := AValue;
WriteStr(debug, AValue);
FScene.LogDebug('MouseState = '+debug);
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

    msToolPoint,
    msCreatingPoint: tex := texMouseToolPoint;

    msToolLine,
    msCreatingLine: tex := texMouseToolLine;

    msToolCircle,
    msCreatingCircle: tex := texMouseToolCircle;

    msToolRectangle,
    msCreatingRectangle: tex := texMouseToolRectangle;

    msToolPolygon,
    msCreatingPolygon: tex := texMouseToolPolygon;
    msWaitingForNextPolygonNode, msBackPressedOnPolygonCreation: tex := texMouseAddNode;
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
  FCamera.AssignToLayers(aLayerIndexList);
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

procedure TCustomScreenTemplate.ComputeMoveRestriction(aPtOrigin, aPtNew: TPointF);
var delta: TPointF;
begin
  FMoveRestriction := PointF(1, 1);
  if CtrlPressed then begin
    delta := aPtOrigin - aPtNew;
    delta.x := Abs(delta.x);
    delta.y := Abs(delta.y);
    if delta.x > delta.y then FMoveRestriction := PointF(1, 0)
      else if delta.y > delta.x then FMoveRestriction := PointF(0, 1)
  end;
end;

procedure TCustomScreenTemplate.ApplyMoveRestrictionOn(var aPt: TPointF);
begin
  if CtrlPressed then begin
    aPt.x := aPt.x * FMoveRestriction.x;
    aPt.y := aPt.y * FMoveRestriction.y;
  end;
end;

{ TScreenWithSurfaceHandling }

procedure TScreenWithSurfaceHandling.LoopMoveSelection;
var current, delta: TPointF;
begin
  if MouseState = msMovingSelection then exit;
  MouseState := msMovingSelection;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := (current - ClickOrigin);
    ApplyMoveRestrictionOn(delta);
    if (delta.x <> 0) or (delta.y <> 0) then begin
      delta.x := delta.x / Zoom;
      delta.y := delta.y / Zoom;
      AddOffsetCoordinateToSelection(delta);
      SetFlagModified;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
    UpdateHandlePositionOnSelected;
  until MouseState <> msMovingSelection;
end;

procedure TScreenWithSurfaceHandling.LoopMovePivotOnSelection;
var current, delta: TPointF;
begin
  if MouseState = msMovePivotOnSelection then exit;
  MouseState := msMovePivotOnSelection;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    ApplyMoveRestrictionOn(delta);
    if (delta.x <> 0) or (delta.y <> 0) then begin
      AddOffsetToPivotOnSelection(delta);
      SetFlagModified;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
  until MouseState <> msMovePivotOnSelection;
end;

procedure TScreenWithSurfaceHandling.LoopScaleSelection;
var current, delta: TPointF;
begin
  if MouseState = msScalingSelection then exit;
  MouseState := msScalingSelection;
  SaveCurrentScaleValueBeforeScalingSelection;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := Camera.ControlToWorld(current) - Camera.ControlToWorld(ClickOrigin);
    if (delta.x <> 0) or (delta.y <> 0) then begin
      ScaleSelection(delta, CtrlPressed);
      SetFlagModified;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msScalingSelection;
end;

procedure TScreenWithSurfaceHandling.LoopRotateSelection;
var current, delta: TPointF;
begin
  if MouseState = msRotatingSelection then exit;
  MouseState := msRotatingSelection;

  SaveCurrentAngleBeforeRotationOnSelection;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      RotateSelection(ClickOrigin, current, CtrlPressed);
      SetFlagModified;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msRotatingSelection;
end;

function TScreenWithSurfaceHandling.GetSelectedCount: integer;
begin
  Result := Length(FSelected);
end;

procedure TScreenWithSurfaceHandling.AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
var i: integer;
begin
  if Length(aItems) = 0 then exit;

  for i:=0 to High(aItems) do
     aItems[i]^.Selected := True;

  if FSelected = NIL then FSelected := aItems
  else begin
    for i:=0 to High(aItems) do
      if not AlreadySelected(aItems[i]) then begin
        SetLength(FSelected, Length(FSelected)+1);
        FSelected[High(FSelected)] := aItems[i];
      end;
  end;
end;

function TScreenWithSurfaceHandling.AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean;
var i: integer;
begin
  if Length(aItems) = 0 then exit(False);
  if Length(FSelected) < Length(aItems) then exit(False);

  for i:=0 to High(aItems) do
    if not AlreadySelected(aItems[i]) then exit(False);
  Result := True;
end;

function TScreenWithSurfaceHandling.AlreadySelected(item: PSurfaceDescriptor): boolean;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] = item then exit(True);
  Result := False;
end;

procedure TScreenWithSurfaceHandling.AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  if Length(aItems) = 0 then exit;
  AddToSelected([aItems[FAlternateOverlappedIndex]]);
end;

procedure TScreenWithSurfaceHandling.AddOffsetCoordinateToSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.MoveRelative(aOffset, 0);
end;

procedure TScreenWithSurfaceHandling.AddOffsetToPivotOnSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.AddOffsetToPivot(aOffset);
end;

procedure TScreenWithSurfaceHandling.SetPivotValuesOnSelection(aLocalPosX, aLocalPosY: single);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    //FSelected[i]^.surface.Pivot := PointF(aLocalPosX, aLocalPosY);

    FSelected[i]^.Pivot := FSelected[i]^.surface.SurfaceToScene(PointF(aLocalPosX*FSelected[i]^.width, aLocalPosY*FSelected[i]^.height));

  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.SaveCurrentAngleBeforeRotationOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentAngleBeforeRotation;
end;

procedure TScreenWithSurfaceHandling.RotateSelection(aPreviousReferencePoint,
  aReferencePoint: TPointF; aUseIncrement: boolean);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.ComputeAngle(aPreviousReferencePoint, aReferencePoint, aUseIncrement);

  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.SaveCurrentScaleValueBeforeScalingSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentScaleValueBeforeScaling;
end;

procedure TScreenWithSurfaceHandling.ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.ComputeScale(FScaleHandleType, aDelta, aKeepAspectRatio);

  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.SaveCurrentPosAndSizeBeforeResizingSelection;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentPosAndSizeBeforeResizing;
end;

procedure TScreenWithSurfaceHandling.AddDeltaToSizeOnSelection(aDelta: TPoint; aKeepAspectRatio: boolean);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.AddDeltaToSpriteSize(FScaleHandleType, aDelta, aKeepAspectRatio);

  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.DeleteSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;

  for i:=0 to High(FSelected) do
    Surfaces.DeleteItemByID(FSelected[i]^.id);
  FSelected := NIL;
end;

procedure TScreenWithSurfaceHandling.UpdateHandlePositionOnSelected;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.UpdateHandlePosition;
end;

function TScreenWithSurfaceHandling.MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverPivotHandle(aWorldPt) then
      Exit(True);
end;

function TScreenWithSurfaceHandling.MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverRotateHandle(aWorldPt) then
      Exit(True);
end;

function TScreenWithSurfaceHandling.MouseIsOverScaleHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverScaleHandle(aWorldPt, FScaleHandleType) then
      Exit(True);
end;

procedure TScreenWithSurfaceHandling.SelectNone;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.Selected := False;
  FSelected := NIL;
end;

procedure TScreenWithSurfaceHandling.SelectAll;
var i: SizeUInt;
begin
  if Surfaces.Size = 0 then exit;

  FSelected := NIL;
  SetLength(FSelected, Surfaces.Size);
  for i:=0 to Surfaces.Size-1 do begin
    Surfaces.Mutable[i]^.Selected := True;
    FSelected[i] := Surfaces.Mutable[i];
  end;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedX: single;
begin
  Result := FSelected[0]^.surface.X.Value;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedCenterX: single;
begin
  Result := FSelected[0]^.surface.CenterX;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedRightX: single;
begin
 Result := FSelected[0]^.surface.RightX;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedY: single;
begin
  Result := FSelected[0]^.surface.Y.Value;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedCenterY: single;
begin
  Result := FSelected[0]^.surface.CenterY;
end;

function TScreenWithSurfaceHandling.GetFirstSelectedBottomY: single;
begin
  Result := FSelected[0]^.surface.BottomY;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedLeftTo(aX: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.X.Value := aX;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedHCenterTo(aX: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.CenterX := aX;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedRightTo(aX: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.RightX := aX;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedTopTo(aY: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.Y.Value := aY;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedVCenterTo(aY: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.CenterY := aY;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedBottomTo(aY: single);
var i: integer;
begin
  for i:=1 to High(FSelected) do
    FSelected[i]^.surface.BottomY := aY;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.ReverseAngleOnSelection;
var i: integer;
  a: single;
begin
  for i:=0 to High(FSelected) do begin
    a := FSelected[i]^.surface.Angle.Value;
    if a > 0 then a := a - 360 else a := a + 360;
    FSelected[i]^.surface.Angle.Value := a;
  end;
end;

procedure TScreenWithSurfaceHandling.ToogleScaledAndRotatedHandleOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.ToogleScaledAndRotatedHandle;
end;

procedure TScreenWithSurfaceHandling.MoveSelection(aDelta: TPointF);
var i: integer;
begin
  for i:=0 to High(FSelected) do begin
    FSelected[i]^.surface.X.Value := FSelected[i]^.surface.X.Value + aDelta.x;
    FSelected[i]^.surface.Y.Value := FSelected[i]^.surface.Y.Value + aDelta.y;
  end;
end;

procedure TScreenWithSurfaceHandling.SetAngleOnSelection(aAngle: single);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.Angle.Value := aAngle;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.ResetValuesOnSelection;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.SetValuesFromTemporaryVariables;
end;

end.

