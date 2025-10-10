unit u_screen_template;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, u_surface_list, u_ui_handle;

// message to user
resourcestring
// errors
sCantMoveSurfacesBecauseSameLayer='Can''t move surfaces because they aren''t on the same layer';
// hints
sAligned='Aligned';
sDuplicated='duplicated';

type

TArrayOfArrayOfInteger = array of TArrayOfInteger;

TMouseState = ( msIdle,

                msMouseDownOnEmptyPlace,
                msMouseDoingRectangularArea,

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

                msCancelShapeCreation,  // used when user pess ESCAPE while creating a collision shape

                msLevelEditorAddingMultiple   // used by level editor
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
  FMoveRestriction: TPointF; // (1,0) moves selection only horizontaly, (0,1) moves selection only verticaly
  procedure SetMouseState(AValue: TMouseState);
public // callback from main form and openglcontrol to manage mouse and keyboard on the view
  procedure ProcessMouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessMouseDown({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessMouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); virtual;
  procedure ProcessOnKeyDown(var Key: Word; {%H-}Shift: TShiftState); virtual;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); virtual;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
public // camera and view
  procedure CreateCamera(const aLayerIndexList: array of integer);
  procedure FreeCamera;
  procedure ZoomViewToFit(aRect: TRectF; aZoomCorrection: single=1.0);
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
public // hint
  procedure ShowHintTextAtMousepos(const aTxt: string);
  procedure ShowHintText(aX, aY: single; const aTxt: string);
  procedure ShowErrorText(const aTxt: string);
end;


{ TScreenWithSurfaceHandling }

TScreenWithSurfaceHandling = class(TCustomScreenTemplate)
//protected
  public
  FSelected: ArrayOfPSurfaceDescriptor; // array that contain pointers to selected surface descriptor
  FAlternateOverlappedIndex: integer;   // used to chose between several overlapped objects
  FScaleHandleType: TScaleHandle;
  function GetSelectedCount: integer;
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor); virtual;
  procedure RemoveItemsFromSelected(aItems: ArrayOfPSurfaceDescriptor); virtual;
  function AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean; overload;
  function AlreadySelected(item: PSurfaceDescriptor): boolean; overload;
  procedure AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure AddOrRemoveOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
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
  function GetSelectedBounds: TRectF;
  function SortSelectedBySurfaceCenterXFromLeftToRight: ArrayOfPSurfaceDescriptor;
  function SortSelectedBySurfaceCenterYFromTopToBottom: ArrayOfPSurfaceDescriptor;
private // selection by rectangular area
  FRectangularSelectionSprite: TShapeOutline;
public
  procedure UpdateRectangularSelectionSprite(aCurrentMousePt: TPointF);
  procedure KillRectangularSelectionSprite;
  procedure LoopDoRectangularSelection;
protected
  procedure LoopMoveSelection;
  procedure LoopMovePivotOnSelection;
  procedure LoopScaleSelection; virtual;
  procedure LoopRotateSelection;
public
  procedure SelectNone; virtual;
  procedure SelectAll; virtual;
  procedure DuplicateSelection; virtual;
  procedure DuplicateSelectionToTheLeft; virtual;
  procedure DuplicateSelectionToTheRight; virtual;
  procedure DuplicateSelectionToTheTop; virtual;
  procedure DuplicateSelectionToTheBottom; virtual;
public // hint
  procedure ShowHintTextOnSelected(const aTxt: string);
  procedure ShowHintTextOnFirstSelected(const aTxt: string);
public // align
  function GetRefBoundsLeft(aRef: PSurfaceDescriptor): single;
  function GetRefBoundsCenterH(aRef: PSurfaceDescriptor): single;
  function GetRefBoundsRight(aRef: PSurfaceDescriptor): single;
  function GetRefBoundsTop(aRef: PSurfaceDescriptor): single;
  function GetRefBoundsCenterV(aRef: PSurfaceDescriptor): single;
  function GetRefBoundsBottom(aRef: PSurfaceDescriptor): single;
  procedure AlignSelectedLeftTo(aX: single; aRef: PSurfaceDescriptor); virtual;
  procedure AlignSelectedHCenterTo(aX: single; aRef: PSurfaceDescriptor); virtual;
  procedure AlignSelectedRightTo(aX: single; aRef: PSurfaceDescriptor); virtual;
  procedure AlignSelectedTopTo(aY: single; aRef: PSurfaceDescriptor); virtual;
  procedure AlignSelectedVCenterTo(aY: single; aRef: PSurfaceDescriptor); virtual;
  procedure AlignSelectedBottomTo(aY: single; aRef: PSurfaceDescriptor); virtual;
public // distribute
  procedure DistributeSelectionHorizontalyWithSameSpacing;
  procedure DistributeSelectionVerticalyWithSameSpacing;
public // rotate 90, mirror, plane
  procedure RotateSelectedCCW; virtual;
  procedure RotateSelectedCW; virtual;
  procedure MirrorSelectedH; virtual;
  procedure MirrorSelectedV; virtual;
  function GetLayerindexesInSelection: TArrayOfInteger;
  function SelectedAreOnTheSameLayer: boolean;
  function GetSurfaceIndexesInTheirLayerOnSelection: TArrayOfInteger;
  function GetSurfaceIndexesInTheListOnSelection: TArrayOfInteger;
  procedure SelectedToTop;
  procedure SelectedToTopOneStep;
  procedure SelectedToBackOneStep;
  procedure SelectedToBack;

  procedure ZoomAll; virtual;
  procedure ZoomOnSceneSize(const aWorldArea: TRectF); virtual;
  procedure ZoomOnSelection; virtual;

  procedure MoveSelectionToLayer(aLayerIndex: integer); virtual;

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
uses Forms, LCLType, Controls, Math, u_common, form_main, u_utils,
  u_layerlist, u_project;

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

procedure TCustomScreenTemplate.ZoomViewToFit(aRect: TRectF; aZoomCorrection: single);
begin
  Zoom := Min(FScene.Width / aRect.Width, FScene.Height / aRect.Height) * aZoomCorrection;
  ViewOffset := PointF(aRect.Left + aRect.Width * 0.5, aRect.Top + aRect.Height * 0.5) - FScene.Center;
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

    msMouseDownOnEmptyPlace: tex := texMouseNormal;
    msMouseDoingRectangularArea: tex := texSelectSurfaceByRect;

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

procedure TCustomScreenTemplate.CreateCamera(const aLayerIndexList: array of integer);
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

  for i:=0 to Layers.Count-1 do
    FScene.Layer[i].Visible := IndexInArray(i);
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

procedure TCustomScreenTemplate.ShowHintTextAtMousepos(const aTxt: string);
var p: TPoint;
begin
  if not Project.Config.CommonShowFlyingTxt then exit;
  p := FScene.Mouse.Position;
  ShowHintText(p.x, p.y, aTxt);
end;

procedure TCustomScreenTemplate.ShowHintText(aX, aY: single; const aTxt: string);
var o : TFreeText;
  rx, ry, time: single;
begin
  if not Project.Config.CommonShowFlyingTxt then exit;
  o := TFreeText.Create(FScene);
  o.TexturedFont := FHintFont;
  o.Caption := aTxt;
  o.SetCenterCoordinate(aX, aY);
  FScene.Add(o, LAYER_TOP);
  time := 2.0;
  o.Opacity.ChangeTo(0, time, idcStartSlowEndFast);
  if aX > FScene.Width/2
    then rx := -100
    else rx := 100;
  if aY > FScene.Height/2
    then ry := -100
    else ry := 100;
  o.MoveRelative(rx, ry, time, idcSinusoid);
  o.KillDefered(time);
end;

procedure TCustomScreenTemplate.ShowErrorText(const aTxt: string);
var textArea: TUITextArea;
begin
  textArea := TUITextArea.Create(FScene);
  FScene.Add(textArea, LAYER_TOP);
  textArea.VScrollBarMode := sbmNeverShow;
  textArea.HScrollBarMode := sbmNeverShow;
  textArea.BodyShape.SetShapeRoundRect(FScene.Width div 3, FScene.Height div 3, PPIScale(8), PPIScale(8), 2.0);
  textArea.BodyShape.Fill.Color := BGRA(0,0,0);
  textArea.Text.TexturedFont := FErrorFont;
  textArea.Text.Align := taTopCenter;
  textArea.Text.Tint.Value := BGRA(0,0,0,0);
  textArea.Text.Caption := aTxt;
  textArea.BodyShape.ResizeCurrentShape(textArea.Text.DrawingSize.cx+PPIScale(20),
                                        textArea.Text.DrawingSize.cy+PPIScale(20), True);
  textArea.CenterOnScene;
  textArea.AddAndPlayScenario('Wait 2.0'#10+
                              'OpacityChange 0 2.0 idcLinear'#10+
                              'Wait 2.0'#10+
                              'Kill');
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

procedure TScreenWithSurfaceHandling.RemoveItemsFromSelected(aItems: ArrayOfPSurfaceDescriptor);
var i, j: integer;
begin
  if Length(aItems) = 0 then exit;
  if Length(FSelected) = 0 then exit;

  for i:=0 to High(aItems) do begin
    for j:=0 to High(FSelected) do
      if FSelected[j] = aItems[i] then begin
        FSelected[j]^.Selected := False;
        system.Delete(FSelected, j, 1);
        break;
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

procedure TScreenWithSurfaceHandling.AddOrRemoveOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  if Length(aItems) = 0 then exit;

  if aItems[FAlternateOverlappedIndex]^.Selected
    then RemoveItemsFromSelected([aItems[FAlternateOverlappedIndex]])
    else AddToSelected([aItems[FAlternateOverlappedIndex]]);
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

  for i:=High(FSelected) downto 0 do
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

function TScreenWithSurfaceHandling.GetSelectedBounds: TRectF;
var i: integer;
  xmin, xmax, ymin, ymax, xx, yy, w, h: single;
begin
  if Length(FSelected) = 0 then begin
    Result := RectF(0, 0, 0, 0);
    exit;
  end;

  xmin := MaxSingle;
  yMin := MaxSingle;
  xmax := MinSingle;
  ymax := MinSingle;
  for i:=0 to High(FSelected) do begin
    with FSelected[i]^.surface do begin
      xx := X.Value;
      yy := Y.Value;
      w := Width;
      h := Height;
    end;
    if xx < xmin then xmin := xx;
    if yy < ymin then ymin := yy;
    if xx+w > xmax then xmax := xx + w;
    if yy+h > ymax then ymax := yy + h;
  end;

  Result := RectF(PointF(xmin, ymin), PointF(xmax, ymax));
end;

function TScreenWithSurfaceHandling.SortSelectedBySurfaceCenterXFromLeftToRight: ArrayOfPSurfaceDescriptor;
var i: integer;
  temp: PSurfaceDescriptor;
  flag: Boolean;
begin
  Result := NIL;
  if Length(FSelected) = 0 then exit;
  Result := Copy(FSelected, 0 , Length(FSelected));
  if Length(Result) = 1 then exit;
  repeat
    flag := False;
    for i:=0 to High(Result)-1 do
      if Result[i]^.surface.CenterX > Result[i+1]^.surface.CenterX then begin
        temp := Result[i];
        Result[i] := Result[i+1];
        Result[i+1] := temp;
        flag := True;
      end;
  until not flag;
end;

function TScreenWithSurfaceHandling.SortSelectedBySurfaceCenterYFromTopToBottom: ArrayOfPSurfaceDescriptor;
var i: integer;
  temp: PSurfaceDescriptor;
  flag: Boolean;
begin
  Result := NIL;
  if Length(FSelected) = 0 then exit;
  Result := Copy(FSelected, 0 , Length(FSelected));
  if Length(Result) = 1 then exit;
  repeat
    flag := False;
    for i:=0 to High(Result)-1 do
      if Result[i]^.surface.CenterY > Result[i+1]^.surface.CenterY then begin
        temp := Result[i];
        Result[i] := Result[i+1];
        Result[i+1] := temp;
        flag := True;
      end;
  until not flag;
end;

procedure TScreenWithSurfaceHandling.UpdateRectangularSelectionSprite(aCurrentMousePt: TPointF);
var r: TRect;
begin
  if FRectangularSelectionSprite = NIL then begin
    FRectangularSelectionSprite := TShapeOutline.Create(FScene);
    FRectangularSelectionSprite.LineColor := BGRA(255,255,0);
    FRectangularSelectionSprite.LineWidth := 3.0;
    FScene.Add(FRectangularSelectionSprite, LAYER_UI);
  end;

  // compute the rectangular area
  if aCurrentMousePt.x >= FClickOrigin.x then begin
    r.Left := Round(FClickOrigin.x);
    r.Right := Round(aCurrentMousePt.x);
  end else begin
    r.Left := Round(aCurrentMousePt.x);
    r.Right := Round(FClickOrigin.x);
  end;

  if aCurrentMousePt.y >= FClickOrigin.y then begin
    r.Top := Round(FClickOrigin.y);
    r.Bottom := Round(aCurrentMousePt.y);
  end else begin
    r.Top := Round(aCurrentMousePt.y);
    r.Bottom := Round(FClickOrigin.y);
  end;

  // adjust the sprite coord and size
  FRectangularSelectionSprite.SetShapeRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TScreenWithSurfaceHandling.KillRectangularSelectionSprite;
begin
  if FRectangularSelectionSprite = NIL then exit;
  FRectangularSelectionSprite.Kill;
  FRectangularSelectionSprite := NIL;
end;

procedure TScreenWithSurfaceHandling.LoopDoRectangularSelection;
var current: TPointF;
  r: TRectF;
  i: SizeUInt;
begin
  if MouseState = msMouseDoingRectangularArea then exit;
  MouseState := msMouseDoingRectangularArea;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    UpdateRectangularSelectionSprite(current);
    FScene.DoLoop;
    Application.ProcessMessages;
    Sleep(1);
  until MouseState <> msMouseDoingRectangularArea;

  // retrieves the rectangular area in scene coordinates
  r.Left := FRectangularSelectionSprite.X.Value;
  r.Top := FRectangularSelectionSprite.Y.Value;
  r.Right := r.Left + FRectangularSelectionSprite.Width;
  r.Bottom := r.Top + FRectangularSelectionSprite.Height;
  //r.TopLeft := FCamera.ControlToWorld(r.TopLeft);
  //r.BottomRight := FCamera.ControlToWorld(r.BottomRight);

  // add to selected all surfaces in the area
  if Surfaces.Size = 0 then exit;
  for i:=0 to Surfaces.Size-1 do
    if not Surfaces.Mutable[i]^.Selected and Surfaces.Mutable[i]^.IsContainedBy(r) then
      AddToSelected([Surfaces.Mutable[i]]);

  KillRectangularSelectionSprite;
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

procedure TScreenWithSurfaceHandling.DuplicateSelection;
var A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) = 0 then exit;
  A := Copy(FSelected, 0, Length(FSelected));
  SelectNone;
  AddToSelected(Surfaces.DuplicateItemsByID(A));
  ShowHintTextOnSelected(sDuplicated);
end;

procedure TScreenWithSurfaceHandling.DuplicateSelectionToTheLeft;
var A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) = 0 then exit;
  ShowHintTextOnSelected('duplicated on the left');
  A := Copy(FSelected, 0, Length(FSelected));
  SelectNone;
  AddToSelected(Surfaces.DuplicateAndShiftItemsByID(A, 1.0, 0.0, 0.0, 0.0, FrameToolLevelEditor.OverlapValue));
  ShowHintTextOnSelected(sDuplicated);
end;

procedure TScreenWithSurfaceHandling.DuplicateSelectionToTheRight;
var A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) = 0 then exit;
  ShowHintTextOnSelected('duplicated on the right');
  A := Copy(FSelected, 0, Length(FSelected));
  SelectNone;
  AddToSelected(Surfaces.DuplicateAndShiftItemsByID(A, 0.0, 1.0, 0.0, 0.0, FrameToolLevelEditor.OverlapValue));
  ShowHintTextOnSelected(sDuplicated);
end;

procedure TScreenWithSurfaceHandling.DuplicateSelectionToTheTop;
var A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) = 0 then exit;
  ShowHintTextOnSelected('duplicated to the top');
  A := Copy(FSelected, 0, Length(FSelected));
  SelectNone;
  AddToSelected(Surfaces.DuplicateAndShiftItemsByID(A, 0.0, 0.0, 1.0, 0.0, FrameToolLevelEditor.OverlapValue));
  ShowHintTextOnSelected(sDuplicated);
end;

procedure TScreenWithSurfaceHandling.DuplicateSelectionToTheBottom;
var A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) = 0 then exit;
  ShowHintTextOnSelected('duplicated to the bottom');
  A := Copy(FSelected, 0, Length(FSelected));
  SelectNone;
  AddToSelected(Surfaces.DuplicateAndShiftItemsByID(A, 0.0, 0.0, 0.0, 1.0, FrameToolLevelEditor.OverlapValue));
  ShowHintTextOnSelected(sDuplicated);
end;

procedure TScreenWithSurfaceHandling.ShowHintTextOnSelected(const aTxt: string);
var i: integer;
  p: TPointF;
begin
  if not Project.Config.CommonShowFlyingTxt then exit;
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do begin
    with FSelected[i]^.surface do
      p := SurfaceToScene(PointF(Width*0.5, Height*0.5));
    ShowHintText(p.x, p.y, aTxt);
  end;
end;

procedure TScreenWithSurfaceHandling.ShowHintTextOnFirstSelected(const aTxt: string);
var p: TPointF;
begin
  if not Project.Config.CommonShowFlyingTxt then exit;
  if Length(FSelected) = 0 then exit;
  with FSelected[0]^.surface do
    p := SurfaceToScene(PointF(Width*0.5, Height*0.5));
  ShowHintText(p.x, p.y, aTxt);
end;

function TScreenWithSurfaceHandling.GetRefBoundsLeft(aRef: PSurfaceDescriptor): single;
begin
  Result := aRef^.surface.GetQuadAreaInWorldSpace.Bounds.Left;
end;

function TScreenWithSurfaceHandling.GetRefBoundsCenterV(aRef: PSurfaceDescriptor): single;
begin
  with aRef^.surface.GetQuadAreaInWorldSpace.Bounds do
    Result := Top + Height*0.5;
end;

function TScreenWithSurfaceHandling.GetRefBoundsRight(aRef: PSurfaceDescriptor): single;
begin
  Result := aRef^.surface.GetQuadAreaInWorldSpace.Bounds.Right;
end;

function TScreenWithSurfaceHandling.GetRefBoundsCenterH(aRef: PSurfaceDescriptor): single;
begin
  with aRef^.surface.GetQuadAreaInWorldSpace.Bounds do
    Result := Left + Width*0.5;
end;

function TScreenWithSurfaceHandling.GetRefBoundsTop(aRef: PSurfaceDescriptor): single;
begin
  Result := aRef^.surface.GetQuadAreaInWorldSpace.Bounds.Top;
end;

function TScreenWithSurfaceHandling.GetRefBoundsBottom(aRef: PSurfaceDescriptor): single;
begin
  Result := aRef^.surface.GetQuadAreaInWorldSpace.Bounds.Bottom;
end;

procedure TScreenWithSurfaceHandling.AlignSelectedLeftTo(aX: single; aRef: PSurfaceDescriptor);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do
        X.Value := X.Value + aX - GetQuadAreaInWorldSpace.Bounds.Left;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.AlignSelectedHCenterTo(aX: single;
  aRef: PSurfaceDescriptor);
var i: integer;
  r: TRectF;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do begin
        r := GetQuadAreaInWorldSpace.Bounds;
        CenterX := CenterX + aX - (r.Left + r.Width*0.5);
    end;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.AlignSelectedRightTo(aX: single;
  aRef: PSurfaceDescriptor);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do
        RightX := RightX + aX - GetQuadAreaInWorldSpace.Bounds.Right;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.AlignSelectedTopTo(aY: single;
  aRef: PSurfaceDescriptor);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do
        Y.Value := Y.Value + aY - GetQuadAreaInWorldSpace.Bounds.Top;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.AlignSelectedVCenterTo(aY: single;
  aRef: PSurfaceDescriptor);
var i: integer;
  r: TRectF;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do begin
        r := GetQuadAreaInWorldSpace.Bounds;
        CenterY := CenterY + aY - (r.Top + r.Height*0.5);
    end;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.AlignSelectedBottomTo(aY: single;
  aRef: PSurfaceDescriptor);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] <> aRef then
      with FSelected[i]^.surface do
        BottomY := BottomY + aY - GetQuadAreaInWorldSpace.Bounds.Bottom;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnSelected(sAligned);
end;

procedure TScreenWithSurfaceHandling.DistributeSelectionHorizontalyWithSameSpacing;
var i: integer;
  xmin, xmax, delta: single;
  A: ArrayOfPSurfaceDescriptor;
begin
  if Length(FSelected) < 3 then exit;
  A := SortSelectedBySurfaceCenterXFromLeftToRight;

  // compute delta
  xmin := A[0]^.surface.CenterX;
  xmax := A[High(A)]^.surface.CenterX;
  delta := (xmax-xmin) / (Length(A)-1);

  for i:=1 to High(A)-1 do
    A[i]^.surface.CenterX := xmin + delta * i;

  UpdateHandlePositionOnSelected;
  SetFlagModified;
  ShowHintTextOnSelected('Distributed');
end;

procedure TScreenWithSurfaceHandling.DistributeSelectionVerticalyWithSameSpacing;
var i: integer;
  A: ArrayOfPSurfaceDescriptor;
  ymin, ymax, delta: Single;
begin
  if Length(FSelected) < 3 then exit;
  A := SortSelectedBySurfaceCenterYFromTopToBottom;

  // compute delta
  ymin := A[0]^.surface.CenterY;
  ymax := A[High(A)]^.surface.CenterY;
  delta := (ymax-ymin) / (Length(A)-1);

  for i:=1 to High(A)-1 do
    A[i]^.surface.CenterY := ymin + delta * i;

  UpdateHandlePositionOnSelected;
  SetFlagModified;
  ShowHintTextOnSelected('Distributed');
end;

procedure TScreenWithSurfaceHandling.RotateSelectedCCW;
var i: integer;
  r: TRectF;
  centerXRect, centerYRect, xx, yy, s, c: single;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = 1 then begin
    with FSelected[0]^.surface do
      Angle.Value := Angle.Value - 90;
    exit;
  end;

  r := GetSelectedBounds;
  centerXRect := r.Left + r.Width * 0.5;
  centerYRect := r.Top + r.Height * 0.5;

  for i:=0 to High(FSelected) do
    with FSelected[i]^.surface do begin
      Angle.Value := Angle.Value - 90;
      xx := CenterX - centerXRect;
      yy := CenterY - centerYRect;
      sincos(90 * deg2rad, s, c);
      SetCenterCoordinate(centerXRect + xx * c + yy * s, centerYRect - xx * s  + c * yy);
    end;

  UpdateHandlePositionOnSelected;
  ShowHintTextOnFirstSelected('Selection rotated 90°CCW');
end;

procedure TScreenWithSurfaceHandling.RotateSelectedCW;
var i: integer;
  r: TRectF;
  centerXRect, centerYRect, xx, yy, s, c: single;
begin
  if Length(FSelected) = 0 then exit;

  r := GetSelectedBounds;
  centerXRect := r.Left + r.Width * 0.5;
  centerYRect := r.Top + r.Height * 0.5;

  for i:=0 to High(FSelected) do
    with FSelected[i]^.surface do begin
      Angle.Value := Angle.Value + 90;
      xx := CenterX - centerXRect;
      yy := CenterY - centerYRect;
      sincos(-90 * deg2rad, s, c);
      SetCenterCoordinate(centerXRect + xx * c + yy * s, centerYRect - xx * s  + c * yy);
    end;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnFirstSelected('Selection rotated 90°CW');
end;

procedure TScreenWithSurfaceHandling.MirrorSelectedH;
var i: integer;
  r: TRectF;
  halfWidthRect, xRectCenter: single;
begin
  if Length(FSelected) = 0 then exit;

  if Length(FSelected) = 1 then begin
    with FSelected[0]^.surface do FlipH := not FlipH;
    exit;
  end;

  // mirror the selection
  r := GetSelectedBounds;
  halfWidthRect := r.Width*0.5;
  xRectCenter := r.Left + halfWidthRect;

  for i:=0 to High(FSelected) do
    with FSelected[i]^.surface do begin
      FlipH := not FlipH;
      RightX := r.Left + xRectCenter - X.Value + halfWidthRect;
    end;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnFirstSelected('Selection mirrored horizontally');
end;

procedure TScreenWithSurfaceHandling.MirrorSelectedV;
var i: integer;
  r: TRectF;
  halfHeightRect, yRectCenter: single;
begin
  if Length(FSelected) = 0 then exit;

  if Length(FSelected) = 1 then begin
    with FSelected[0]^.surface do FlipV := not FlipV;
    exit;
  end;

  // mirror the selection
  r := GetSelectedBounds;
  halfHeightRect := r.Height*0.5;
  yRectCenter := r.Top + halfHeightRect;

  for i:=0 to High(FSelected) do
    with FSelected[i]^.surface do begin
      FlipV := not FlipV;
      BottomY := r.Top + yRectCenter - Y.Value + halfHeightRect;
    end;
  UpdateHandlePositionOnSelected;
  ShowHintTextOnFirstSelected('Selection mirrored vertically');
end;

function TScreenWithSurfaceHandling.GetLayerindexesInSelection: TArrayOfInteger;
var i: integer;
begin
  Result := NIL;
  if Length(FSelected) = 0 then exit;

  for i:=0 to High(FSelected) do
    Result.AddOnlyOneTime(FSelected[i]^.layerindex);
end;

function TScreenWithSurfaceHandling.SelectedAreOnTheSameLayer: boolean;
var i, li: integer;
begin
  Result := False;
  if Length(FSelected) = 0 then exit;

  li := FSelected[0]^.layerindex;
  for i:=1 to High(FSelected) do
    if li <> FSelected[i]^.layerindex then exit(False);

  Result := True;
end;

function TScreenWithSurfaceHandling.GetSurfaceIndexesInTheirLayerOnSelection: TArrayOfInteger;
var i: integer;
begin
  // we suppose the selected are on the same layer (checked before the call)
  Result := NIL;
  SetLength(Result, Length(Selected));
  for i:=0 to High(FSelected) do
    Result[i] := FSelected[i]^.surface.ParentLayer.IndexOf(FSelected[i]^.surface);
end;

function TScreenWithSurfaceHandling.GetSurfaceIndexesInTheListOnSelection: TArrayOfInteger;
var i: integer;
begin
  Result := NIL;
  SetLength(Result, Length(Selected));
  for i:=0 to High(FSelected) do
    Result[i] := Surfaces.GetItemIndexByID(FSelected[i]^.id);
end;

procedure TScreenWithSurfaceHandling.SelectedToTop;
var indexesInLayer, indexesInList, ids: TArrayOfInteger;
  i, newIndex: integer;
  layer: TLayer;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = Surfaces.Size then exit;
  if not SelectedAreOnTheSameLayer then begin
    ShowErrorText(sCantMoveSurfacesBecauseSameLayer);
    exit;
  end;

  // save the selected IDs
  ids := Surfaces.ItemsDescriptorToArrayOfID(FSelected);

  // retrieve the surface indexes in the layer
  indexesInLayer := GetSurfaceIndexesInTheirLayerOnSelection;
  indexesInLayer.SortFromSmallToHigh;
  // retrieve the surface indexes in the list
  indexesInList := GetSurfaceIndexesInTheListOnSelection;
  indexesInList.SortFromSmallToHigh;

  layer := FSelected[0]^.surface.ParentLayer;
  if layer = NIL then exit;

  // shift the surface in layer
  newIndex := layer.SurfaceCount-1;
  for i:=High(indexesInLayer) downto 0 do begin
    layer.Move(indexesInLayer[i], newIndex);
    dec(newIndex);
  end;

  // shift in the list
  newIndex := Surfaces.Size-1;
  for i:=High(indexesInList) downto 0 do begin
    Surfaces.MoveItemTo(indexesInList[i], newIndex);
    dec(newIndex);
  end;

  // reconstruct the selected because their adress have changed
  for i:=0 to High(ids) do
    FSelected[i] := Surfaces.GetItemByID(ids[i]);

  SetFlagModified;
end;

procedure TScreenWithSurfaceHandling.SelectedToTopOneStep;
var indexesInLayer, indexesInList, allIndexesInList, ids: TArrayOfInteger;
  i, newIndex, previous, currentIndex, previousIndex: integer;
  layer: TLayer;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = Surfaces.Size then exit;
  if not SelectedAreOnTheSameLayer then begin
    ShowErrorText(sCantMoveSurfacesBecauseSameLayer);
    exit;
  end;
  layer := FSelected[0]^.surface.ParentLayer;
  if layer = NIL then exit;

  // save the selected IDs
  ids := Surfaces.ItemsDescriptorToArrayOfID(FSelected);

  // retrieve the surface indexes in the layer
  indexesInLayer := GetSurfaceIndexesInTheirLayerOnSelection;
  indexesInLayer.SortFromSmallToHigh;
  // retrieve the surface indexes in the list
  indexesInList := GetSurfaceIndexesInTheListOnSelection;
  indexesInList.SortFromSmallToHigh;
  // retrieve the indexes of all surface in the layer in the list
  allIndexesInList := Surfaces.GetIndexesForALayer(layer);
  allIndexesInList.SortFromSmallToHigh;


  // shift the surface in layer
  previous := layer.SurfaceCount;
  for i:=High(indexesInLayer) downto 0 do begin
    newIndex := indexesInLayer[i] + 1;
    if newIndex <> previous then begin
      layer.Move(indexesInLayer[i], newIndex);
      previous := newIndex;
    end else previous := indexesInLayer[i];
  end;

  // shift in the list
  previousIndex := Length(allIndexesInList);

  for i:=High(indexesInList) downto 0 do begin
    currentIndex := allIndexesInList.IndexOf(indexesInList[i]);
    newIndex := currentIndex + 1;

    if (newIndex <> previousIndex) and (newIndex < allIndexesInList.Count)  then begin
      Surfaces.MoveItemTo(allIndexesInList[currentIndex], allIndexesInList[newIndex]);
      previousIndex := newIndex;
    end else previousIndex := currentIndex;
  end;

  // reconstruct the selected because their adress have changed
  for i:=0 to High(ids) do
    FSelected[i] := Surfaces.GetItemByID(ids[i]);

  SetFlagModified;
end;

procedure TScreenWithSurfaceHandling.SelectedToBackOneStep;
var indexesInLayer, indexesInList, allIndexesInList, ids: TArrayOfInteger;
  i, newIndex, previous, currentIndex, previousIndex: integer;
  layer: TLayer;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = Surfaces.Size then exit;
  if not SelectedAreOnTheSameLayer then begin
    ShowErrorText(sCantMoveSurfacesBecauseSameLayer);
    exit;
  end;
  layer := FSelected[0]^.surface.ParentLayer;
  if layer = NIL then exit;

  // save the selected IDs
  ids := Surfaces.ItemsDescriptorToArrayOfID(FSelected);

  // retrieve the surface indexes in the layer
  indexesInLayer := GetSurfaceIndexesInTheirLayerOnSelection;
  indexesInLayer.SortFromSmallToHigh;
  // retrieve the surface indexes in the list
  indexesInList := GetSurfaceIndexesInTheListOnSelection;
  indexesInList.SortFromSmallToHigh;
  // retrieve the indexes of all surface in the layer in the list
  allIndexesInList := Surfaces.GetIndexesForALayer(layer);
  allIndexesInList.SortFromSmallToHigh;


  // shift the surface in layer
  previous := 0;
  for i:=0 to High(indexesInLayer) do begin
    newIndex := indexesInLayer[i] - 1;
    if (newIndex <> previous) and (newindex >= 0) then begin
      layer.Move(indexesInLayer[i], newIndex);
      previous := newIndex;
    end else previous := indexesInLayer[i];
  end;

  // shift in the list
  previousIndex := Length(allIndexesInList);

  for i:=0 to High(indexesInList) do begin
    currentIndex := allIndexesInList.IndexOf(indexesInList[i]);
    newIndex := currentIndex - 1;

    if (newIndex <> previousIndex) and (newIndex >= 0)  then begin
      Surfaces.MoveItemTo(allIndexesInList[currentIndex], allIndexesInList[newIndex]);
      previousIndex := newIndex;
    end else previousIndex := currentIndex;
  end;

  // reconstruct the selected because their adress have changed
  for i:=0 to High(ids) do
    FSelected[i] := Surfaces.GetItemByID(ids[i]);

  SetFlagModified;

{var A, ids: TArrayOfInteger;
  i, newIndex, previous: integer;
  surf: TSimpleSurfaceWithEffect;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = Surfaces.Size then exit;
  if not SelectedAreOnTheSameLayer then exit;

  // save the selected IDs
  ids := Surfaces.ItemsDescriptorToArrayOfID(FSelected);

  // get the sorted indexes
  A := Surfaces.GetItemsIndexesByIDSortedSmallToHigh(FSelected);

  // shift the surface in their layer and in the surface list
  previous := -1;
  for i:=0 to High(A) do begin
    newIndex := A[i] - 1;
    if newIndex <> previous then begin
      surf := Surfaces.GetByIndex(A[i])^.surface;
      if newIndex >= 0 then begin
        surf.ParentLayer.Move(A[i], newIndex);
        Surfaces.MoveItemTo(A[i], newIndex);
        previous := newIndex;
      end else previous := A[i];
    end;
  end;

  // reconstruct the selected because their adress have changed
  for i:=0 to High(ids) do
    FSelected[i] := Surfaces.GetItemByID(ids[i]);

  SetFlagModified;   }
end;

procedure TScreenWithSurfaceHandling.SelectedToBack;
var indexesInLayer, indexesInList, ids: TArrayOfInteger;
  i, newIndex: integer;
  layer: TLayer;
begin
  if Length(FSelected) = 0 then exit;
  if Length(FSelected) = Surfaces.Size then exit;
  if not SelectedAreOnTheSameLayer then begin
    ShowErrorText(sCantMoveSurfacesBecauseSameLayer);
    exit;
  end;

  // save the selected IDs
  ids := Surfaces.ItemsDescriptorToArrayOfID(FSelected);

  // retrieve the surface indexes in the layer
  indexesInLayer := GetSurfaceIndexesInTheirLayerOnSelection;
  indexesInLayer.SortFromSmallToHigh;
  // retrieve the surface indexes in the list
  indexesInList := GetSurfaceIndexesInTheListOnSelection;
  indexesInList.SortFromSmallToHigh;

  layer := FSelected[0]^.surface.ParentLayer;
  if layer = NIL then exit;

  // shift the surface in layer
  newIndex := 0;
  for i:=0 to High(indexesInLayer) do begin
    layer.Move(indexesInLayer[i], newIndex);
    inc(newIndex);
  end;

  // shift in the list
  newIndex := 0;
  for i:=0 to High(indexesInList) do begin
    Surfaces.MoveItemTo(indexesInList[i], newIndex);
    inc(newIndex);
  end;

  // reconstruct the selected because their adress have changed
  for i:=0 to High(ids) do
    FSelected[i] := Surfaces.GetItemByID(ids[i]);

  SetFlagModified;
end;

procedure TScreenWithSurfaceHandling.ZoomAll;
begin
  ZoomViewToFit(Surfaces.GetItemsBounds, 0.8);
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.ZoomOnSceneSize(const aWorldArea: TRectF);
var r: TRectF;
begin
  r := FCamera.GetViewRect;
  if r.Left < aWorldArea.Left then r.Left := aWorldArea.Left;
  if r.Top < aWorldArea.Top then r.Top := aWorldArea.Top;
  if r.Left+FScene.Width > aWorldArea.Right then r.Left := aWorldArea.Right-FScene.Width;
  if r.Top+FScene.Height > aWorldArea.Bottom then r.Top := aWorldArea.Bottom-FScene.Height;
  ZoomViewToFit(RectF(r.Left, r.Top, r.Left+FScene.Width, r.Top+FScene.Height), 1.0);
end;

procedure TScreenWithSurfaceHandling.ZoomOnSelection;
begin
  if Length(FSelected) = 0 then exit;
  ZoomViewToFit(GetSelectedBounds, 0.8);
  UpdateHandlePositionOnSelected;
end;

procedure TScreenWithSurfaceHandling.MoveSelectionToLayer(aLayerIndex: integer);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do begin
    FSelected[i]^.surface.MoveToLayer(aLayerIndex);
    FSelected[i]^.layerindex := aLayerIndex;
  end;
  ShowHintTextOnSelected('moved to '+Layers.Names[aLayerIndex-APP_LAYER_COUNT]);
  SetFlagModified;
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

