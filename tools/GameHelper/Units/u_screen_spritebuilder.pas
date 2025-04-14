unit u_screen_spritebuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_screen_template,
  u_surface_list, u_texture_list;

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
  function GetSelectedCount: integer;
  procedure AddToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
  procedure SelectNone;
  procedure AddOffsetCoordinateToSelection(aOffset: TPointF);
  procedure AddOffsetToPivotOnSelection(aOffset: TPointF);
  procedure SaveCurrentAngleBeforeRotationOnSelection;
  procedure RotateSelection(aReferencePoint: TPointF; aUseIncrement: boolean);
  procedure DeleteSelection;
  procedure UpdateHandlePositionOnSelected;
  function MouseIsOverPivotHandle(aWorldPt: TPointF): boolean;
  function MouseIsOverRotateHandle(aWorldPt: TPointF): boolean;
private
  procedure LoopMoveSelection;
  procedure LoopMovePivotOnSelection;
public
  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState); override;
  procedure ProcessMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
public // selection
  procedure SetPivotOnSelection;
  procedure LoopRotateSelection;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  property Textures: TTextureList read FTextures;
  property Surfaces: TSpriteBuilderSurfaceList read FSurfaces;
end;

var ScreenSpriteBuilder: TScreenSpriteBuilder;
    FContainer: TSpriteContainer;

implementation
uses LCLType, Forms, Controls, u_project, u_app_pref, form_main,
  form_tool_spritebuilder, u_ui_handle;

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
    function alreadyExists(item: PSurfaceDescriptor): boolean;
    var j: integer;
    begin
      for j:=0 to High(FSelected) do
        if FSelected[j] = item then begin
          Result := True;
          exit;
        end;
      Result := False;
    end;
begin
  if Length(aItems) = 0 then exit;

  for i:=0 to High(aItems) do
     aItems[i]^.Selected := True;

  if FSelected = NIL then FSelected := aItems
  else begin
    for i:=0 to High(aItems) do
      if not alreadyExists(aItems[i]) then begin
        SetLength(FSelected, Length(FSelected)+1);
        FSelected[High(FSelected)] := aItems[i];
      end;
  end;

  FormTools.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.AddOnlyTheFirstToSelected(aItems: ArrayOfPSurfaceDescriptor);
begin
  if Length(aItems) = 0 then exit;
  AddToSelected([aItems[0]]);
end;

procedure TScreenSpriteBuilder.SelectNone;
var i: integer;
begin
  for i:=0 to High(FSelected) do
    FSelected[i]^.Selected := False;
  FSelected := NIL;

  FormTools.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.AddOffsetCoordinateToSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.surface.MoveRelative(aOffset, 0);

  FormTools.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.AddOffsetToPivotOnSelection(aOffset: TPointF);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.AddOffsetToPivot(aOffset);

  FormTools.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.SaveCurrentAngleBeforeRotationOnSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.SaveCurrentAngleBeforeRotation;
end;

procedure TScreenSpriteBuilder.RotateSelection(aReferencePoint: TPointF;
  aUseIncrement: boolean);
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do
    FSelected[i]^.ComputeAngle(aReferencePoint, aUseIncrement);

  UpdateHandlePositionOnSelected;
  FormTools.ShowSelectionData(FSelected);
end;

procedure TScreenSpriteBuilder.DeleteSelection;
var i: integer;
begin
  if Length(FSelected) = 0 then exit;
  for i:=0 to High(FSelected) do begin
    FSelected[i]^.KillSurface;
    Surfaces.Erase(Surfaces.GetItemIndexByID(FSelected[i]^.id));
  end;
  FSelected := NIL;

  FormTools.ShowSelectionData(FSelected);
  Project.SetModified;
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

procedure TScreenSpriteBuilder.LoopMoveSelection;
var current, delta: TPointF;
begin
  if MouseState = msMovingSelection then exit;
  MouseState := msMovingSelection;

  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      delta.x := delta.x / Zoom;
      delta.y := delta.y / Zoom;
      AddOffsetCoordinateToSelection(delta);
      Project.SetModified;
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
    if (delta.x <> 0) or (delta.y <> 0) then begin
      delta.x := delta.x / Zoom;
      delta.y := delta.y / Zoom;
      AddOffsetToPivotOnSelection(delta);
      Project.SetModified;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
  until MouseState <> msMovePivotOnSelection;
end;

procedure TScreenSpriteBuilder.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
begin
  inherited ProcessMouseUp(Button, Shift, X, Y);

  items := Surfaces.GetItemsAt(X, Y);
  case MouseState of
    msIdle: begin
      SelectNone;
      MouseState := msIdle;
    end;

    msMouseDownOnSurface: begin
      // item selection
      case Button of
        mbLeft:begin
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
            end else items[0]^.ToogleSelectedAndRotatedHandle;
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

    msRotatingSelection: MouseState := msIdle;

    msMovePivotOnSelection: MouseState := msOverPivot;
  end;//case

end;

procedure TScreenSpriteBuilder.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
    p: single;
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
    msOverRotateHandle: MouseState := msMouseDownOnRotateHandle;
    msOverPivot: MouseState := msMouseDownOnPivot;

  end;//case
end;

procedure TScreenSpriteBuilder.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
var items: ArrayOfPSurfaceDescriptor;
    thresholdDone: boolean;
begin
  inherited ProcessMouseMove(Shift, X, Y);

  items := Surfaces.GetItemsAt(X, Y);
  thresholdDone := Distance(ClickOrigin, PointF(X, Y)) > PPIScale(5);
  case MouseState of
    msIdle: begin
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
        // mouse is over pivot handle ? (only one selected)
        if MouseIsOverPivotHandle(PointF(X,Y)) then
          MouseState := msOverPivot
        else;
      end;

    msMouseDownOnSurface: begin
      if items <> NIL then begin
        if thresholdDone then
          LoopMoveSelection;
      end else MouseState := msIdle;
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
      if thresholdDone then
        LoopMovePivotOnSelection;
    end;

  end;
end;

procedure TScreenSpriteBuilder.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
  case Key of
    VK_DELETE: DeleteSelection;
  end;
end;

procedure TScreenSpriteBuilder.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
  UpdateHandlePositionOnSelected;
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
      RotateSelection(Camera.ControlToWorld(current), CtrlPressed);
      Project.SetModified;
      FScene.DoLoop;
      ClickOrigin := current;
    end;
    Application.ProcessMessages;
  until MouseState <> msRotatingSelection;
end;

procedure TScreenSpriteBuilder.CreateObjects;
begin
  ShowLayers([LAYER_UI, LAYER_SPRITEBUILDER]);
end;

procedure TScreenSpriteBuilder.FreeObjects;
begin
end;

procedure TScreenSpriteBuilder.Initialize;
begin
  FTextures := TTextureList.Create;
  FSurfaces := TSpriteBuilderSurfaceList.Create;

  // create the root container
  FContainer := TSpriteContainer.Create(FScene);
  FScene.Add(FContainer, LAYER_SPRITEBUILDER);
  FContainer.CenterOnScene;
  FContainer.ShowOrigin := True;

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
end;


end.

