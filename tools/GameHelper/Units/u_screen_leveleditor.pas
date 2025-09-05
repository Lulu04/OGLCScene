unit u_screen_leveleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template,
  u_texture_list, u_surface_list;

type

{ TLevelEditorSurfaceList }

TLevelEditorSurfaceList = class(TSurfaceList)
  function Textures: TTextureList; override;
end;

{ TScreenLevelEditor }

TScreenLevelEditor = class(TScreenWithSurfaceHandling)
public
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor); override;
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF); override;
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF); override;
  procedure RotateSelection(aPreviousReferencePoint, aReferencePoint: TPointF; aUseIncrement: boolean); override;
  procedure ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean); override;
  procedure DeleteSelection; override;
  procedure LoopScaleSelection; override; // change size instead scale
public
  FSurfaces: TLevelEditorSurfaceList;
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure SelectNone; override;
  procedure SelectAll; override;
  procedure SetPositionOnSelection(aX, aY: single);
  procedure SetSizeOnSelection(aWidth, aHeight: integer);
  procedure SetOriginalSizeOnSelection;
  procedure SetFlipHOnSelection(aValue: boolean);
  procedure SetFlipVOnSelection(aValue: boolean);
  procedure SetOpacityOnSelection(aValue: single);
  procedure SetTintOnSelection(aValue: TBGRAPixel);
  procedure SetTintModeOnSelection(aValue: TTintMode);

  procedure SetFlagModified; override;
  function Surfaces: TSurfaceList; override;
end;

var ScreenLevelEditor: TScreenLevelEditor;

implementation

uses Forms, u_levelbank, frame_tool_leveleditor, form_main, u_app_pref, Controls,
  LCLType;

{ TLevelEditorSurfaceList }

function TLevelEditorSurfaceList.Textures: TTextureList;
begin
  Result := LevelBank.Textures;
end;

{ TScreenLevelEditor }

procedure TScreenLevelEditor.AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  inherited AddToSelected(aItems);
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.AddOffsetCoordinateToSelection(aOffset: TPointF);
begin
  inherited AddOffsetCoordinateToSelection(aOffset);
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.AddOffsetToPivotOnSelection(aOffset: TPointF);
begin
  inherited AddOffsetToPivotOnSelection(aOffset);
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.RotateSelection(aPreviousReferencePoint,
  aReferencePoint: TPointF; aUseIncrement: boolean);
begin
  inherited RotateSelection(aPreviousReferencePoint, aReferencePoint, aUseIncrement);
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean);
begin
  inherited ScaleSelection(aDelta, aKeepAspectRatio);
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.DeleteSelection;
begin
  if Length(FSelected) = 0 then exit;
  inherited DeleteSelection;

  FrameToolLevelEditor.ShowSelectionData(FSelected);
  FrameToolLevelEditor.Modified := True;
end;

procedure TScreenLevelEditor.LoopScaleSelection;
var current, delta: TPointF;
  deltai: TPoint;
begin
  // here we change size instead of scale
  if MouseState = msScalingSelection then exit;
  MouseState := msScalingSelection;
  SaveCurrentPosAndSizeBeforeResizingSelection;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := Camera.ControlToWorld(current) - Camera.ControlToWorld(ClickOrigin);
    deltai := delta.Round;
    if (deltai.x <> 0) or (deltai.y <> 0) then begin
      AddDeltaToSizeOnSelection(deltai, CtrlPressed);
      //ClickOrigin := current;
      FrameToolLevelEditor.ShowSelectionData(FSelected);
      SetFlagModified;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msScalingSelection;
end;

procedure TScreenLevelEditor.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  inherited ProcessMouseUp(Button, Shift, X, Y);

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

procedure TScreenLevelEditor.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  inherited ProcessMouseDown(Button, Shift, X, Y);

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

procedure TScreenLevelEditor.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
    thresholdDone: boolean;
begin
  inherited ProcessMouseMove(Shift, X, Y);

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

procedure TScreenLevelEditor.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
  UpdateHandlePositionOnSelected;
end;

procedure TScreenLevelEditor.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
  case Key of
    VK_DELETE: DeleteSelection;

    VK_R: begin
     ToogleScaledAndRotatedHandleOnSelection;
    end;

    VK_A: begin
     if ssCtrl in Shift then SelectAll;
    end;


    VK_Z: begin
      if ssCtrl in Shift then
        if ssShift in Shift then Surfaces.UndoRedoManager.Redo
          else Surfaces.UndoRedoManager.Undo;
      MouseState := msIdle;
    end;
  end;

end;

procedure TScreenLevelEditor.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_LEVELEDITOR]);
  // camera
  CreateCamera([LAYER_LEVELEDITOR]);
end;

procedure TScreenLevelEditor.FreeObjects;
begin
  //SelectNone;
  FreeCamera;
end;

procedure TScreenLevelEditor.Initialize;
begin
  FSurfaces := TLevelEditorSurfaceList.Create;
  FSurfaces.SetModeForLevelEditor;
  FSurfaces.WorkingLayer := LAYER_LEVELEDITOR;
end;

procedure TScreenLevelEditor.Finalize;
begin
  FSurfaces.Free;
  FSurfaces := NIL;
end;

procedure TScreenLevelEditor.SelectNone;
begin
  inherited SelectNone;
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.SelectAll;
begin
  inherited SelectAll;
  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.SetPositionOnSelection(aX, aY: single);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.SetCoordinate(aX, aY);
  UpdateHandlePositionOnSelected;
end;

procedure TScreenLevelEditor.SetSizeOnSelection(aWidth, aHeight: integer);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do begin
    TSprite(FSelected[i]^.surface).SetSize(aWidth, aHeight);
    FSelected[i]^.surface.CollisionBody.RemoveAll;
    FSelected[i]^.surface.CollisionBody.AddPolygon([PointF(0,0), PointF(aWidth, 0),
                                                    PointF(aWidth, aHeight), PointF(0, aHeight)]);;
  end;
  UpdateHandlePositionOnSelected;
end;

procedure TScreenLevelEditor.SetOriginalSizeOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do begin
    with TSprite(FSelected[i]^.surface) do begin
      SetSizeSameAsTexture;
      CollisionBody.RemoveAll;
      CollisionBody.AddPolygon([PointF(0,0), PointF(Width, 0),
                                PointF(Width, Height), PointF(0, Height)]);
    end;
  end;
  UpdateHandlePositionOnSelected;

  FrameToolLevelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenLevelEditor.SetFlipHOnSelection(aValue: boolean);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.FlipH := aValue;
end;

procedure TScreenLevelEditor.SetFlipVOnSelection(aValue: boolean);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.FlipV := aValue;
end;

procedure TScreenLevelEditor.SetOpacityOnSelection(aValue: single);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.Opacity.Value := aValue;
end;

procedure TScreenLevelEditor.SetTintOnSelection(aValue: TBGRAPixel);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.Tint.Value := aValue
end;

procedure TScreenLevelEditor.SetTintModeOnSelection(aValue: TTintMode);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.TintMode := aValue;
end;

procedure TScreenLevelEditor.SetFlagModified;
begin
  FrameToolLevelEditor.Modified := True;
end;

function TScreenLevelEditor.Surfaces: TSurfaceList;
begin
  Result := FSurfaces;
end;

end.

