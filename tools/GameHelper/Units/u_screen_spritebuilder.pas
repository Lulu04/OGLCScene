unit u_screen_spritebuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template,
  u_surface_list, u_texture_list, u_ui_handle,
  u_collisionbody_list, u_posture_list;

type

{ TSpriteBuilderSurfaceList }

TSpriteBuilderSurfaceList = class(TSurfaceList)
  function Textures: TTextureList; override;
end;

{ TScreenSpriteBuilder }

TScreenSpriteBuilder = class(TCustomScreenTemplate)
private
  FTextures: TTextureList;
  FSurfaces: TSpriteBuilderSurfaceList;
private
  FSelected: ArrayOfPSurfaceDescriptor;
  FAlternateOverlappedIndex: integer;  // used to chose between several overlapped objects
  FScaleHandleType: TScaleHandle;
  function GetSelectedCount: integer;
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
  function AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean; overload;
  function AlreadySelected(item: PSurfaceDescriptor): boolean; overload;
  procedure AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF);
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF);
  procedure SaveCurrentAngleBeforeRotationOnSelection;
  procedure RotateSelection(aPreviousReferencePoint, aReferencePoint: TPointF; aUseIncrement: boolean);
  procedure SaveCurrentScaleValueBeforeScalingSelection;
  procedure ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean);
  procedure DeleteSelection;
  procedure UpdateHandlePositionOnSelected;
  function MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverScaleHandle(aWorldPt: TPointF): boolean;
public
  procedure ReverseAngleOnSelection;
  procedure ToogleScaledAndRotatedHandleOnSelection;
private
  procedure LoopMoveSelection;
  procedure LoopMovePivotOnSelection;
private
  procedure ProcessMouseUpForChild(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseDownForChild(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseMoveForChild(Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseWheelForChild({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
  procedure ProcessKeyUpForChild(var Key: Word; {%H-}Shift: TShiftState);
private // collision body
  FBodyList: TBodyItemList;
  FWorkingBody: PBodyItem;
  FWorkingNode: PUINodeHandle;
  procedure UnselectAllNodes;
  procedure LoopCreateLine;
  procedure LoopCreateCircle;
  procedure LoopCreateRectangle;
  procedure LoopCreatePolygon;
  procedure LoopMoveNode;
  procedure ProcessMouseUpForCollisionBody({%H-}Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseDownForCollisionBody({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseMoveForCollisionBody(Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseWheelForCollisionBody({%H-}Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
public
  procedure ProcessKeyUpForCollisionBody(var Key: Word; {%H-}Shift: TShiftState);
  procedure AddNodeBetweenSelectedOnPolygon;
  function ConsecutiveNodeAreSelectedOnPolygon: boolean;

private // posture
  FPostures: TPostureList;
  procedure ProcessKeyUpForPostures(var Key: Word; {%H-}Shift: TShiftState);

public
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); override;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
public // selection
  procedure SetPivotOnSelection;
  procedure LoopRotateSelection;
  procedure LoopScaleSelection;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure SelectNone;
  property Textures: TTextureList read FTextures;
  property Surfaces: TSpriteBuilderSurfaceList read FSurfaces;
  property Bodies: TBodyItemList read FBodyList;
  property Postures: TPostureList read FPostures;
end;

var ScreenSpriteBuilder: TScreenSpriteBuilder;

implementation
uses LCLType, Forms, Dialogs, Controls, u_project, u_app_pref, form_main;

{ TSpriteBuilderSurfaceList }

function TSpriteBuilderSurfaceList.Textures: TTextureList;
begin
  Result := ScreenSpriteBuilder.Textures;
end;

{ TScreenSpriteBuilder }

function TScreenSpriteBuilder.GetSelectedCount: integer;
begin
  Result := Length(FSelected);
end;

procedure TScreenSpriteBuilder.AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
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

  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

function TScreenSpriteBuilder.AlreadySelected(aItems: ArrayOfPSurfaceDescriptor): boolean;
var i: integer;
begin
  if Length(aItems) = 0 then exit(False);
  if Length(FSelected) < Length(aItems) then exit(False);

  for i:=0 to High(aItems) do
    if not AlreadySelected(aItems[i]) then exit(False);
  Result := True;
end;

function TScreenSpriteBuilder.AlreadySelected(item: PSurfaceDescriptor): boolean;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    if FSelected[i] = item then exit(True);
  Result := False;
end;

procedure TScreenSpriteBuilder.AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  if Length(aItems) = 0 then exit;
  AddToSelected([aItems[FAlternateOverlappedIndex]]);
end;

procedure TScreenSpriteBuilder.SelectNone;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.Selected := False;
  FSelected := NIL;

  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.AddOffsetCoordinateToSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.MoveRelative(aOffset, 0);

  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.AddOffsetToPivotOnSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.AddOffsetToPivot(aOffset);

  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.SaveCurrentAngleBeforeRotationOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentAngleBeforeRotation;
end;

procedure TScreenSpriteBuilder.RotateSelection(aPreviousReferencePoint,
  aReferencePoint: TPointF; aUseIncrement: boolean);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.ComputeAngle(aPreviousReferencePoint, aReferencePoint, aUseIncrement);

  UpdateHandlePositionOnSelected;
  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.SaveCurrentScaleValueBeforeScalingSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentScaleValueBeforeScaling;
end;

procedure TScreenSpriteBuilder.ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean);
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.ComputeScale(FScaleHandleType, aDelta, aKeepAspectRatio);

  UpdateHandlePositionOnSelected;
  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.DeleteSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;

  // we can not delete the root if it have child
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsRoot and (Surfaces.Size > 1) then begin
      ShowMessage('You can not delete the root because there are childs attached to it');
      exit;
    end;

  for i:=0 to High(FSelected) do begin
    FSelected[i]^.KillSurface;
    Surfaces.Erase(Surfaces.GetItemIndexByID(FSelected[i]^.id));
  end;
  FSelected := NIL;

  FrameToolsSpriteBuilder.ShowSelectionData(FSelected);
  Surfaces.FillComboBox(FrameToolsSpriteBuilder.CBParent);
  FrameToolsSpriteBuilder.Modified := True;
end;

procedure TScreenSpriteBuilder.UpdateHandlePositionOnSelected;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.UpdateHandlePosition;
end;

function TScreenSpriteBuilder.MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverPivotHandle(aWorldPt) then
      Exit(True);
end;

function TScreenSpriteBuilder.MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverRotateHandle(aWorldPt) then
      Exit(True);
end;

function TScreenSpriteBuilder.MouseIsOverScaleHandle(aWorldPt: TPointF): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsOverScaleHandle(aWorldPt, FScaleHandleType) then
      Exit(True);
end;

procedure TScreenSpriteBuilder.ReverseAngleOnSelection;
var i: integer;
  a: single;
begin
  for i:=0 to High(FSelected) do begin
    a := FSelected[i]^.surface.Angle.Value;
    if a > 0 then a := a - 360 else a := a + 360;
    FSelected[i]^.surface.Angle.Value := a;
  end;
end;

procedure TScreenSpriteBuilder.ToogleScaledAndRotatedHandleOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.ToogleScaledAndRotatedHandle;
end;

procedure TScreenSpriteBuilder.LoopMoveSelection;
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
      FrameToolsSpriteBuilder.Modified := True;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
    UpdateHandlePositionOnSelected;
  until MouseState <> msMovingSelection;
end;

procedure TScreenSpriteBuilder.LoopMovePivotOnSelection;
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
      FrameToolsSpriteBuilder.Modified := True;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
  until MouseState <> msMovePivotOnSelection;
end;

procedure TScreenSpriteBuilder.ProcessMouseUpForChild(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  items := Surfaces.GetItemsAt(X, Y);

  case MouseState of
    msIdle: SelectNone;

    msMouseDownOnSurface: begin
      // item selection
      case Button of
        mbLeft:begin
          if ssAlt in Shift then begin
            inc(FAlternateOverlappedIndex);
            if not (ssShift in Shift) then SelectNone;
          end else FAlternateOverlappedIndex := 0;
          if FAlternateOverlappedIndex > High(items) then
            FAlternateOverlappedIndex := 0;

          if items = NIL then begin
            SelectNone;
            MouseState := msIdle;
          end
          else
          if ssShift in Shift then begin
            AddOnlyTheFirstToSelected(items);
            MouseState := msOverSurface;
          end
          else begin
            if not items[0]^.Selected then begin
              SelectNone;
              AddOnlyTheFirstToSelected(items);
            end else items[FAlternateOverlappedIndex]^.ToogleScaledAndRotatedHandle;
            MouseState := msOverSurface;
          end;
        end;

        mbRight: begin
        end;
      end;//case mouse button
    end;

    msOverSurface, msMovingSelection:
      if items = NIL then MouseState := msIdle
        else MouseState := msOverSurface;

    msScalingSelection: MouseState := msIdle;
    msRotatingSelection: MouseState := msIdle;

    msMovePivotOnSelection: MouseState := msOverPivot;
  end;//case
end;

procedure TScreenSpriteBuilder.ProcessMouseDownForChild(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  items := Surfaces.GetItemsAt(X, Y);
  ClickOrigin := PointF(X, Y);
  case MouseState of
    msIdle: begin
      if Button = mbLeft then begin
        if items <> NIL then MouseState := msOverSurface;
      end;
    end;

    msOverSurface: MouseState := msMouseDownOnSurface;
    msOverScaleHandle: MouseState := msMouseDownOnScaleHandle;
    msOverRotateHandle: MouseState := msMouseDownOnRotateHandle;
    msOverPivot: MouseState := msMouseDownOnPivot;

  end;//case
end;

procedure TScreenSpriteBuilder.ProcessMouseMoveForChild(Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
    thresholdDone: boolean;
begin
  items := Surfaces.GetItemsAt(X, Y);
  thresholdDone := Distance(ClickOrigin, PointF(X, Y)) > PPIScale(5);

  case MouseState of
    msIdle: begin
      // mouse is over pivot handle ? (only one selected)
      if MouseIsOverPivotHandle(PointF(X,Y)) then
        MouseState := msOverPivot
      else
      // mouse is over scale handle ?
      if MouseIsOverScaleHandle(PointF(X,Y)) then
        MouseState := msOverScaleHandle
      else
      // mouse is over rotate handle ?
      if MouseIsOverRotateHandle(PointF(X,Y)) then
        MouseState := msOverRotateHandle
      else // mouse is over a surface ?
      if Length(items) > 0 then
        MouseState := msOverSurface;
    end;

    msOverSurface:
      if items = NIL then mouseState := msIdle
      else begin
        // mouse is over scale handle ?
        if MouseIsOverScaleHandle(PointF(X,Y)) then
          MouseState := msOverScaleHandle
        else
        // mouse is over rotate handle ?
        if MouseIsOverRotateHandle(PointF(X,Y)) then
          MouseState := msOverRotateHandle
        else
        // mouse is over pivot handle ? (only one selected)
        if MouseIsOverPivotHandle(PointF(X,Y)) then
          MouseState := msOverPivot;
      end;

    msMouseDownOnSurface: begin
      if items <> NIL then begin
        if thresholdDone then begin
          if FAlternateOverlappedIndex >= Length(items) then begin
            SelectNone;
            FAlternateOverlappedIndex := 0;
          end;
          if GetSelectedCount = 0 then AddToSelected([items[FAlternateOverlappedIndex]])
          else begin
            if not AlreadySelected([items[FAlternateOverlappedIndex]]) then begin
              if not (ssShift in Shift) then SelectNone;
              AddToSelected([items[FAlternateOverlappedIndex]]);
            end;
          end;
          ComputeMoveRestriction(ClickOrigin, PointF(X,Y));
          LoopMoveSelection;
        end;
      end else MouseState := msIdle;
    end;

    msOverScaleHandle: begin
      if not MouseIsOverScaleHandle(PointF(X,Y)) then
        if items = NIL then MouseState := msIdle
          else MouseState := msOverSurface;
    end;

    msMouseDownOnScaleHandle: begin
      if thresholdDone then begin
        ClickOrigin := PointF(X,Y);
        LoopScaleSelection;
      end;
    end;

    msOverRotateHandle: begin
      if not MouseIsOverRotateHandle(PointF(X,Y)) then
        if items = NIL then MouseState := msIdle
          else MouseState := msOverSurface;
    end;

    msMouseDownOnRotateHandle: begin
      if thresholdDone then begin
        ClickOrigin := PointF(X,Y);
        LoopRotateSelection;
      end;
    end;

    msOverPivot: begin
      if not MouseIsOverPivotHandle(PointF(X,Y)) then
        if items = NIL then MouseState := msIdle
          else MouseState := msOverSurface;
    end;

    msMouseDownOnPivot: begin
      if thresholdDone then begin
        ComputeMoveRestriction(ClickOrigin, PointF(X,Y));
        LoopMovePivotOnSelection;
      end;
    end;

  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseWheelForChild(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  UpdateHandlePositionOnSelected;
end;

procedure TScreenSpriteBuilder.ProcessKeyUpForChild(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE: DeleteSelection;

    VK_R: begin
     ToogleScaledAndRotatedHandleOnSelection;
    end;


    VK_Z: begin
      if ssCtrl in Shift then
        if ssShift in Shift then Surfaces.UndoRedoManager.Redo
          else Surfaces.UndoRedoManager.Undo;
      MouseState := msIdle;
    end;
  end;
end;

procedure TScreenSpriteBuilder.UnselectAllNodes;
begin
  Bodies.UnselectAllNodes;
end;

procedure TScreenSpriteBuilder.LoopCreateLine;
var item: PBodyItem;
  current, delta: TPointF;
  firstTime: boolean;
  parentSurface: TSimpleSurfaceWithEffect;
begin
  if MouseState = msCreatingLine then exit;
  MouseState := msCreatingLine;

  parentSurface := Surfaces.GetRootItem^.surface;

  item := Bodies.AddEmpty;
  item^.BodyType := _btLine;
  item^.ParentSurface := parentSurface;

  firstTime := True;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    if firstTime or (delta.x <> 0) or (delta.y <> 0) then begin
      firstTime := False;
      item^.UpdateAsLine(ClickOrigin, current);
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> msCreatingLine;

  if MouseState = msCancelShapeCreation then begin
    // cancel the shape
    Bodies.DeleteItem(item);
    MouseState := msIdle;
  end else begin
    FrameToolsSpriteBuilder.Modified := True;
    Bodies.UndoRedoManager.AddActionAddShape(item);
  end;
end;

procedure TScreenSpriteBuilder.LoopCreateCircle;
var item: PBodyItem;
  current, delta: TPointF;
  firstTime: boolean;
  parentSurface: TSimpleSurfaceWithEffect;
begin
  if MouseState = msCreatingCircle then exit;
  MouseState := msCreatingCircle;

  parentSurface := Surfaces.GetRootItem^.surface;

  item := Bodies.AddEmpty;
  item^.BodyType := _btCircle;
  item^.ParentSurface := parentSurface;

  firstTime := True;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    current.y := ClickOrigin.y;
    if current.x < ClickOrigin.x then current.x := ClickOrigin.x;
    delta := current - ClickOrigin;
    if firstTime or (delta.x <> 0) or (delta.y <> 0) then begin
      firstTime := False;
      item^.UpdateAsCircle(ClickOrigin, current);
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> msCreatingCircle;

  if MouseState = msCancelShapeCreation then begin
    Bodies.DeleteItem(item);
    MouseState := msIdle;
  end else begin
    FrameToolsSpriteBuilder.Modified := True;
    Bodies.UndoRedoManager.AddActionAddShape(item);
  end;
end;

procedure TScreenSpriteBuilder.LoopCreateRectangle;
var item: PBodyItem;
  current, delta: TPointF;
  firstTime: boolean;
  parentSurface: TSimpleSurfaceWithEffect;
begin
  if MouseState = msCreatingRectangle then exit;
  MouseState := msCreatingRectangle;

  parentSurface := Surfaces.GetRootItem^.surface;

  item := Bodies.AddEmpty;
  item^.BodyType := _btRect;
  item^.ParentSurface := parentSurface;

  firstTime := True;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    if firstTime or (delta.x <> 0) or (delta.y <> 0) then begin
      firstTime := False;
      item^.UpdateAsRectangle(ClickOrigin, current);
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> msCreatingRectangle;

  if MouseState = msCancelShapeCreation then begin
    Bodies.DeleteItem(item);
    MouseState := msIdle;
  end else begin
    FrameToolsSpriteBuilder.Modified := True;
    Bodies.UndoRedoManager.AddActionAddShape(item);
  end;
end;

procedure TScreenSpriteBuilder.LoopCreatePolygon;
var current: TPointF;
  item: PBodyItem;
begin
  // the end of creating a polygon is:
  // - when user press ESC to cancel the creation
  // - when user click on the first point to close the path
  // - when user press ENTER to end the creation
  // - when user click right mouse button to end the creation

  if MouseState in [msCreatingPolygon, msWaitingForNextPolygonNode] then exit;
  MouseState := msCreatingPolygon;

  item := Bodies.AddEmpty;
  item^.BodyType := _btPolygon;
  item^.ParentSurface := Surfaces.GetRootItem^.surface;

  repeat
    if MouseState = msCreatingPolygon then begin
      current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
      if item^.IsOverTheFirstNode(current) then begin
        // close the path and exit
        item^.CloseThePolygon;
        MouseState := msIdle;
        FrameToolsSpriteBuilder.Modified := True;
        Bodies.UndoRedoManager.AddActionAddShape(item);
        exit;
      end;

      item^.UpdateAsPolygon(current);
      MouseState := msWaitingForNextPolygonNode;
    end;

    Application.ProcessMessages;
    FScene.DoLoop;
    if MouseState = msBackPressedOnPolygonCreation then begin
      if Length(item^.Pts) = 1 then MouseState := msCancelShapeCreation
      else begin
        item^.DeleteLastNode;
        MouseState := msWaitingForNextPolygonNode;
      end;
    end;
  until not(MouseState in [msCreatingPolygon, msWaitingForNextPolygonNode]);

  if MouseState = msCancelShapeCreation then begin
    Bodies.DeleteItem(item);
    MouseState := msIdle;
  end else begin
    FrameToolsSpriteBuilder.Modified := True;
    Bodies.UndoRedoManager.AddActionAddShape(item);
  end;
end;

procedure TScreenSpriteBuilder.LoopMoveNode;
var current, delta: TPointF;
  changed: boolean;
  previousItemDescriptor: TOGLCBodyItem;
begin
  if MouseState = msMovingNode then exit;
  MouseState := msMovingNode;

  previousItemDescriptor.CopyFrom(FWorkingBody^.ItemDescriptor);
  changed := False;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    delta.x := delta.x / Zoom;
    delta.y := delta.y / Zoom;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      FWorkingBody^.UpdateSelectedNodePosition(delta);
      ClickOrigin := current;
      changed := True;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> msMovingNode;

  if changed then begin
    FrameToolsSpriteBuilder.Modified := True;
    Bodies.UndoRedoManager.AddActionModify(previousItemDescriptor, FWorkingBody);
  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseUpForCollisionBody(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPUINodeHandle;
  targetBody: PBodyItem;
begin
  items := Bodies.GetItemAndNodesAt(PointF(X, Y), targetBody);

  if Button = mbLeft then begin
    case MouseState of
      msIdle: UnselectAllNodes;

      msMouseDownOnNode: begin
        if items <> NIL then begin
          if not (ssShift in Shift) then Bodies.UnselectAllNodes;
          items[0]^.Selected := True;
          MouseState := msOverNode;
        end else begin
            Bodies.UnselectAllNodes;
            MouseState := msIdle;
          end;
      end;

      msMovingNode: MouseState := msIdle;
      msCreatingLine: MouseState := msIdle;
      msCreatingCircle: MouseState := msIdle;
      msCreatingRectangle: MouseState := msIdle;
      msMouseDownOnToolLine: MouseState := msIdle;
      msMouseDownOnToolCircle: MouseState := msIdle;
      msMouseDownOnToolRectangle: MouseState := msIdle;
      msMouseDownOnToolPolygon: begin
        ClickOrigin := PointF(X, Y);
        LoopCreatePolygon;
      end;
      msWaitingForNextPolygonNode: begin
        ClickOrigin := PointF(X, Y);
        MouseState := msCreatingPolygon;
      end;
    end;
  end else if Button = mbRight then begin
    case MouseState of
      msOverNode: begin
        FrameToolsSpriteBuilder.MIAddNode.Enabled := ConsecutiveNodeAreSelectedOnPolygon;
        FrameToolsSpriteBuilder.MIDeleteNode.Enabled := (FWorkingBody <> NIL) and (FWorkingBody^.SomeNodesAreSelected);
        FrameToolsSpriteBuilder.PopupNode.PopUp;
      end;
    end;
  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseDownForCollisionBody(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPUINodeHandle;
  targetBody: PBodyItem;
begin
  ClickOrigin := PointF(X, Y);
  items := Bodies.GetItemAndNodesAt(PointF(X, Y), targetBody);

  if Button = mbLeft then begin
    case MouseState of
      msIdle: ;
      msOverNode: begin
        if items <> NIL then begin
          MouseState := msMouseDownOnNode;
        end;
      end;
      msToolLine: MouseState := msMouseDownOnToolLine;
      msToolCircle: MouseState := msMouseDownOnToolCircle;
      msToolRectangle: MouseState := msMouseDownOnToolRectangle;
      msToolPolygon: MouseState := msMouseDownOnToolPolygon;
    end;
  end else if Button = mbRight then begin

  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseMoveForCollisionBody(Shift: TShiftState; X, Y: Integer);
var thresholdDone: Boolean;
  items: ArrayOfPUINodeHandle;
  targetBody: PBodyItem;
begin
  thresholdDone := Distance(ClickOrigin, PointF(X, Y)) > PPIScale(5);
  items := Bodies.GetItemAndNodesAt(PointF(X, Y), targetBody);

  case MouseState of
    msIdle: begin
      if items <> NIL then begin
        FWorkingBody := targetBody;
        MouseState := msOverNode;
      end;
    end;
    msOverNode: begin
      if items = NIL then begin
        FWorkingBody := NIL;
        MouseState := msIdle;
      end;
    end;
    msMouseDownOnNode: if thresholdDone and (items <> NIL) then begin
      FWorkingBody := targetBody;
      if not (ssSHift in Shift) then UnselectAllNodes;
      items[0]^.Selected := True;
      FWorkingNode := items[0];
      LoopMoveNode;
    end;
    msMouseDownOnToolLine: if thresholdDone then LoopCreateLine;
    msMouseDownOnToolCircle: if thresholdDone then LoopCreateCircle;
    msMouseDownOnToolRectangle: if thresholdDone then LoopCreateRectangle;
  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseWheelForCollisionBody(
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var items: ArrayOfPUINodeHandle;
  targetBody: PBodyItem;
begin
  items := Bodies.GetItemAndNodesAt(PointF(MousePos), targetBody);
  if items <> NIL then begin
    // select/unselect nodes
    targetBody^.UpdateNodeSelectedFrom(items[0], WheelDelta > 0);
    Handled := True; // avoid mouse interaction on view
  end;
end;

procedure TScreenSpriteBuilder.ProcessKeyUpForCollisionBody(var Key: Word; Shift: TShiftState);
var item: PBodyItem;
  previousItemDescriptor: TOGLCBodyItem;
begin
  case Key of
    VK_DELETE: if Bodies.SelectedNodeBelongToTheSameShape then begin
      if Bodies.SelectedNodesBelongToSinglePolygon(item) then begin
        if item^.AllNodesAreSelected then begin
          if QuestionDlg('','Delete the polygon?', mtWarning,
                      [mrOk,'Delete', mrCancel, 'Cancel'], 0) = mrOk then begin
            Bodies.UndoRedoManager.AddActionDelete(item);
            Bodies.DeleteItem(item);
            FrameToolsSpriteBuilder.Modified := True;
            MouseState := msIdle;
          end;
        end else begin
          previousItemDescriptor.CopyFrom(item^.ItemDescriptor);
          item^.DeleteSelectedNodes;
          Bodies.UndoRedoManager.AddActionModify(previousItemDescriptor, item);
          FrameToolsSpriteBuilder.Modified := True;
          MouseState := msIdle;
        end;
        exit;
      end;
      if QuestionDlg('','Delete the whole shape?',
                    mtWarning,[mrOk,'Delete', mrCancel, 'Cancel'], 0) = mrOk then begin
        Bodies.UndoRedoManager.AddActionDelete(Bodies.GetFirstItemWithSelectedNode);
        Bodies.DeleteShapeWithSelectedNode;
        FrameToolsSpriteBuilder.Modified := True;
        MouseState := msIdle;
      end;
    end;

    VK_ESCAPE: begin
      if MouseState in [msCreatingLine,msCreatingCircle,msCreatingRectangle,
                        msCreatingPolygon,msWaitingForNextPolygonNode] then begin
        MouseState := msCancelShapeCreation;
      end;
    end;

    VK_BACK: begin
      if MouseState in [msCreatingPolygon,msWaitingForNextPolygonNode] then begin
         MouseState := msBackPressedOnPolygonCreation;
      end;
    end;

    VK_Z: begin
      if ssCtrl in Shift then
        if ssShift in Shift then Bodies.UndoRedoManager.Redo
          else Bodies.UndoRedoManager.Undo;
      MouseState := msIdle;
    end;
  end;
end;

procedure TScreenSpriteBuilder.AddNodeBetweenSelectedOnPolygon;
begin
  if FWorkingBody <> NIL then
    FWorkingBody^.AddNodeBetweenSelectedOnPolygon;
end;

function TScreenSpriteBuilder.ConsecutiveNodeAreSelectedOnPolygon: boolean;
begin
  if (FWorkingBody <> NIL) and (FWorkingBody^.BodyType = _btPolygon) then
    Result := FWorkingBody^.ConsecutiveNodeAreSelectedOnPolygon
  else Result := False;
end;

procedure TScreenSpriteBuilder.ProcessKeyUpForPostures(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_R: begin
     ToogleScaledAndRotatedHandleOnSelection;
    end;

    VK_Z: begin
      if ssCtrl in Shift then
        if ssShift in Shift then Postures.UndoRedoManager.Redo
          else Postures.UndoRedoManager.Undo;
      MouseState := msIdle;
    end;
  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseUp(Button, Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsChild or
     FrameToolsSpriteBuilder.SelectedTabIsPosture then
    ProcessMouseUpForChild(Button, Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    ProcessMouseUpForCollisionBody(Button, Shift, X, Y);
end;

procedure TScreenSpriteBuilder.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseDown(Button, Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsChild or
     FrameToolsSpriteBuilder.SelectedTabIsPosture then
    ProcessMouseDownForChild(Button, Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    ProcessMouseDownForCollisionBody(Button, Shift, X, Y);
end;

procedure TScreenSpriteBuilder.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited ProcessMouseMove(Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsChild or
     FrameToolsSpriteBuilder.SelectedTabIsPosture then
    ProcessMouseMoveForChild(Shift, X, Y);

  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    ProcessMouseMoveForCollisionBody(Shift, X, Y);
end;

procedure TScreenSpriteBuilder.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
  if FrameToolsSpriteBuilder.SelectedTabIsChild then
    ProcessKeyUpForChild(Key, Shift);

  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    ProcessKeyUpForCollisionBody(Key, Shift);

  if FrameToolsSpriteBuilder.SelectedTabIsPosture then
    ProcessKeyUpForPostures(Key, Shift);
end;

procedure TScreenSpriteBuilder.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FrameToolsSpriteBuilder.SelectedTabIsChild then
    ProcessMouseWheelForChild(Shift, WheelDelta, MousePos, Handled);

  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    ProcessMouseWheelForCollisionBody(Shift, WheelDelta, MousePos, Handled);

  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);

  if FrameToolsSpriteBuilder.SelectedTabIsChild then
    UpdateHandlePositionOnSelected;
  if FrameToolsSpriteBuilder.SelectedTabIsCollisionBody then
    Bodies.UpdateNodesPosition;
end;

procedure TScreenSpriteBuilder.SetPivotOnSelection;
var i: SizeUInt;
begin
  if GetSelectedCount = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.Pivot := ClickOrigin;
end;

procedure TScreenSpriteBuilder.LoopRotateSelection;
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
      FrameToolsSpriteBuilder.Modified := True;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msRotatingSelection;
end;

procedure TScreenSpriteBuilder.LoopScaleSelection;
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
      FrameToolsSpriteBuilder.Modified := True;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msScalingSelection;
end;

procedure TScreenSpriteBuilder.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_COLLISION_BODY, LAYER_SPRITEBUILDER]);
end;

procedure TScreenSpriteBuilder.FreeObjects;
begin
  SelectNone;
  FTextures.Clear;
  FSurfaces.Clear;
  FBodyList.Clear;
  FPostures.Clear;
end;

procedure TScreenSpriteBuilder.Initialize;
begin
  FTextures := TTextureList.Create;

  FSurfaces := TSpriteBuilderSurfaceList.Create;
  FSurfaces.WorkingLayer := LAYER_SPRITEBUILDER;

  FBodyList := TBodyItemList.Create;

  FPostures := TPostureList.Create;

  // camera
  CreateCamera([LAYER_SPRITEBUILDER]);
end;

procedure TScreenSpriteBuilder.Finalize;
begin
  FreeCamera;
  FTextures.Free;
  FTextures := NIL;
  FSurfaces.Free;
  FSurfaces := NIL;
  FBodyList.Free;
  FBodyList := NIL;
  FPostures.Free;
  FPostures := NIL;
end;


end.

