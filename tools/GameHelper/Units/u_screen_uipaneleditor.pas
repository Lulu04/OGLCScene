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
  FPanelNameToEdit: string;
  FUIPanel: TUIPanelWithEffects;
  FTextureList: TTexturelist;
  FSurfacesList: TSurfaceList;
public
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor); override;
  procedure RemoveItemsFromSelected(aItems: ArrayOfPSurfaceDescriptor); override;
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF); override;
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF); override;
  procedure RotateSelection(aPreviousReferencePoint, aReferencePoint: TPointF; aUseIncrement: boolean); override;
  procedure ScaleSelection(aDelta: TPointF; aKeepAspectRatio: boolean); override;
  procedure DeleteSelection; override;
  procedure AddDeltaToSizeOnSelection(aDelta: TPoint; aKeepAspectRatio: boolean); override;
  procedure LoopScaleSelection; override; // change size instead scale

  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure SelectNone; override;
  procedure SelectAll; override;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;
  procedure SetFlagModified; override;

  procedure ClearView;
  procedure EditPanelFromBank(const aName: string);
  procedure EditNewPanel;
public
  property UIPanel: TUIPanelWithEffects read FUIPanel write FUIPanel;
end;

var ScreenUIPanelEditor: TScreenUIPanelEditor;

implementation

uses u_common, form_main, u_app_pref, u_ui_objectlist, Controls, LCLType,
  Dialogs, Forms;

{ TScreenUIPanelEditor }

procedure TScreenUIPanelEditor.AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  inherited AddToSelected(aItems);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.RemoveItemsFromSelected(
  aItems: ArrayOfPSurfaceDescriptor);
begin
  inherited RemoveItemsFromSelected(aItems);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.AddOffsetCoordinateToSelection(aOffset: TPointF);
begin
  inherited AddOffsetCoordinateToSelection(aOffset);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.AddOffsetToPivotOnSelection(aOffset: TPointF);
begin
  inherited AddOffsetToPivotOnSelection(aOffset);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.RotateSelection(aPreviousReferencePoint,
  aReferencePoint: TPointF; aUseIncrement: boolean);
begin
  inherited RotateSelection(aPreviousReferencePoint, aReferencePoint, aUseIncrement);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.ScaleSelection(aDelta: TPointF;
  aKeepAspectRatio: boolean);
begin
  inherited ScaleSelection(aDelta, aKeepAspectRatio);
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.DeleteSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;

  // we can not delete the root if it have child
  for i:=0 to High(FSelected) do
    if FSelected[i]^.IsRoot then begin
      ShowMessage('You can not delete the root panel');
      exit;
    end;

  inherited DeleteSelection;

  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
  Surfaces.FillComboBox(FrameToolUIPanelEditor.CBParent);
  FrameToolUIPanelEditor.FillTexturesComboBox;
  FrameToolUIPanelEditor.Modified := True;
end;

procedure TScreenUIPanelEditor.AddDeltaToSizeOnSelection(aDelta: TPoint;
  aKeepAspectRatio: boolean);
var i: integer;
  surf: TSimpleSurfaceWithEffect;
begin
  inherited AddDeltaToSizeOnSelection(aDelta, aKeepAspectRatio);

  // resize also the gradient
  for i:=0 to High(FSelected) do begin
    surf := FSelected[i]^.surface;
    if surf is TUIClickableWithBodyShape then
      TUIClickableWithBodyShape(surf).BackGradient.ComputeVerticesAndIndices(surf.Width, surf.Height);
  end;
end;

procedure TScreenUIPanelEditor.LoopScaleSelection;
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
      FrameToolUIPanelEditor.ShowSelectionData(FSelected);
      SetFlagModified;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until MouseState <> msScalingSelection;
end;

procedure TScreenUIPanelEditor.ProcessMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  items := Surfaces.GetItemsAt(X, Y);

  case MouseState of
    msIdle: SelectNone;

    msMouseDownOnEmptyPlace: MouseState := msIdle;
    msMouseDoingRectangularArea: MouseState := msIdle;

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
            AddOrRemoveOnlyTheFirstToSelected(items);
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

    msMouseDownOnScaleHandle: MouseState := msOverScaleHandle;
    msMouseDownOnRotateHandle: MouseState := msOverRotateHandle;
    msMouseDownOnPivot: MouseState := msOverPivot;
  end;//case
end;

procedure TScreenUIPanelEditor.ProcessMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  items := Surfaces.GetItemsAt(X, Y);
  ClickOrigin := PointF(X, Y);
  case MouseState of
    msIdle: begin
      if Button = mbLeft then begin
        if items <> NIL then MouseState := msOverSurface
        else begin
          if not (ssShift in Shift) then SelectNone;
          MouseState := msMouseDownOnEmptyPlace;
        end;
      end;
    end;

    msOverSurface: MouseState := msMouseDownOnSurface;
    msOverScaleHandle: MouseState := msMouseDownOnScaleHandle;
    msOverRotateHandle: MouseState := msMouseDownOnRotateHandle;
    msOverPivot: MouseState := msMouseDownOnPivot;

  end;//case
end;

procedure TScreenUIPanelEditor.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
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

    msMouseDownOnEmptyPlace:
      if thresholdDone then
        LoopDoRectangularSelection;

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

procedure TScreenUIPanelEditor.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
  UpdateHandlePositionOnSelected;
end;

procedure TScreenUIPanelEditor.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);

  // hack to avoid surface deletion when user erase a SpinEdit value with delete key
  if LastClickedIsControl then exit;

  case Key of
    VK_DELETE: DeleteSelection;

    VK_R: begin
     ToogleScaledAndRotatedHandleOnSelection;
    end;

    VK_A: if ssCtrl in Shift then SelectAll;

    VK_Z: begin
      if ssCtrl in Shift then
        if ssShift in Shift then Surfaces.UndoRedoManager.Redo
          else Surfaces.UndoRedoManager.Undo;
      MouseState := msIdle;
    end;
  end;
end;

procedure TScreenUIPanelEditor.CreateObjects;
var
  item: TPanelDescriptorItem;
begin
  ShowLayers([LAYER_TOP, LAYER_UI, LAYER_UIPANEL]);
  ShowSceneBounds;
  CreateCamera([LAYER_UIPANEL, LAYER_SCENEBOUNDS]);

  ZoomOnScene;

  Surfaces.Clear;
  Textures.Clear;
  FUIPanel := NIL;
  if FPanelNameToEdit <> '' then begin
    item := PanelBank.GetByName(FPanelNameToEdit);
    if item = NIL then exit;

    Textures.LoadFromString(item.textures);
    Surfaces.LoadFromString(item.surfaces);
    FUIPanel := TUIPanelWithEffects(surfaces.GetRootItem^.surface);
    FrameToolUIPanelEditor.TargetUIPanel := FUIPanel;
    FrameToolUIPanelEditor.EditPanelFromBank(item);
    FormMain.ShowPagePanelEditor;
  end else begin
    FUIPanel := Surfaces.AddMainPanel('Panel1', FScene.Width div 3, FScene.Height div 5);
    FrameToolUIPanelEditor.TargetUIPanel := FUIPanel;
    FrameToolUIPanelEditor.EditNewPanel;
  end;

  ZoomOnScene;
end;

procedure TScreenUIPanelEditor.FreeObjects;
begin
  // unlink our panel from the frame
  FrameToolUIPanelEditor.TargetUIPanel := NIL;

  FTextureList.Clear;
  FSurfacesList.Clear;

  FreeCamera;
  FScene.Layer[LAYER_UIPANEL].Clear;
  HideSceneBounds;
end;

procedure TScreenUIPanelEditor.Initialize;
begin
  FTextureList := TTexturelist.Create;
  FSurfacesList := TSurfaceList.Create;
  FSurfacesList.Textures := FTextureList;
  FSurfacesList.WorkingLayer := LAYER_UIPANEL;
end;

procedure TScreenUIPanelEditor.Finalize;
begin
  FSurfacesList.Free;
  FSurfacesList := NIL;
  FTextureList.Free;
  FTextureList := NIL;
end;

procedure TScreenUIPanelEditor.SelectNone;
begin
  inherited SelectNone;
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

procedure TScreenUIPanelEditor.SelectAll;
begin
  inherited SelectAll;
  FrameToolUIPanelEditor.ShowSelectionData(FSelected);
end;

function TScreenUIPanelEditor.Surfaces: TSurfaceList;
begin
  Result := FSurfacesList;
end;

function TScreenUIPanelEditor.Textures: TTextureList;
begin
  Result := FTextureList;
end;

procedure TScreenUIPanelEditor.SetFlagModified;
begin
  FrameToolUIPanelEditor.Modified := True;
end;

procedure TScreenUIPanelEditor.ClearView;
begin
  Surfaces.Clear;
  Textures.Clear;
  FUIPanel := NIL;
end;

procedure TScreenUIPanelEditor.EditPanelFromBank(const aName: string);
begin
  FPanelNameToEdit := aName;
end;

procedure TScreenUIPanelEditor.EditNewPanel;
begin
  FPanelNameToEdit := '';
end;

end.

