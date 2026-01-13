unit u_screen_patheditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, gvector,
  u_common, u_screen_template, u_surface_list, u_texture_list, u_ui_objectlist;

type

{ TScreenPathEditor }

TScreenPathEditor = class(TCustomScreenTemplate)
private
  FPathToFollow: TOGLCPathToFollow;
  FSplinePathToFollow: TOGLCPathToFollow;
  FNodes: array of TSprite;
  FHalf_Width_Point: integer;
  FNodeindexClicked, FNodeIndexToInsert: integer;
  procedure ComputeCurveLineWidth;
  procedure DoLoopMoveNode;
private
  FLevelSurfaces: TSurfaceList;
  FLevelTextures: TTextureList;
  FSpriteSurfaces: TSurfaceList;
  FSpriteTextures: TTextureList;
private
  FPathNameToEdit: string;
public
  Path: TOGLCPath;
  procedure UpdateCurves;
  function GetPathData: string;
  procedure DeleteAllNodes;
  function NodeIndexAt(aX, aY: integer): integer;
  function AddNode(aX, aY: integer): integer;
  procedure InsertNode(aX, aY, aIndex: integer);
  function CanInsertNodeAt(aX, aY: integer; out aIndexToInsert: integer): boolean;
  procedure DeleteNode(aNodeIndex: integer);
  procedure HighlightNode(aNodeindex: integer);
  procedure HighlightNone;

  procedure ShowLevel(aGroupIndex, aLevelIndex: integer);
  procedure HideLevel;

  procedure ShowSprite(aSpriteIndex: integer);
  procedure HideSprite;


  procedure ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseDown(Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;
  procedure ProcessOnKeyUp(var Key: Word; {%H-}Shift: TShiftState); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  procedure EditPathFromBank(const aName: string);
  procedure EditNewPath;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;
end;

var ScreenPathEditor: TScreenPathEditor;

implementation

uses form_main, u_project, u_levelbank, u_spritebank, Math, Controls, Forms;

{ TScreenPathEditor }

procedure TScreenPathEditor.ComputeCurveLineWidth;
begin
  if FPathToFollow <> NIL then
    FPathToFollow.Border.Width := Max(4.0, 1.0/Zoom);

  if FSplinePathToFollow <> NIL then
    FSplinePathToFollow.Border.Width := Max(4.0, 1.0/Zoom);
end;

procedure TScreenPathEditor.DoLoopMoveNode;
var current, delta: TPointF;
  changed: boolean;
begin
  if MouseState = msMovingCircleNode then exit;
  MouseState := msMovingCircleNode;

  changed := False;
  repeat
    current := PointF(FormMain.OGL.ScreenToClient(Mouse.CursorPos));
    delta := current - ClickOrigin;
    delta.x := delta.x / Zoom;
    delta.y := delta.y / Zoom;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      Path[FNodeindexClicked] := Path[FNodeindexClicked] + delta;
      UpdateCurves;
      HighlightNode(FNodeindexClicked);
      ClickOrigin := current;
      changed := True;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> msMovingCircleNode;

  if changed then FramePathEditor.Modified := True;
end;

procedure TScreenPathEditor.UpdateCurves;
var i: integer;
begin
  if FPathToFollow <> NIL then FPathToFollow.Kill;
  FPathToFollow := NIL;
  if FSplinePathToFollow <> NIL then FSplinePathToFollow.Kill;
  FSplinePathToFollow := NIL;
  for i:=0 to High(FNodes) do FNodes[i].Kill;
  FNodes := NIL;

  if Length(Path) = 0 then exit;

  FPathToFollow := TOGLCPathToFollow.Create(FScene);
  FScene.Add(FPathToFollow, LAYER_PATH);
  FPathToFollow.Border.Color := BGRA(255,255,0);
  FPathToFollow.Border.Width := 4.0;
  FPathToFollow.Border.LinePosition := lpMiddle;
  FPathToFollow.InitFromPath(Path, False);

  FSplinePathToFollow := TOGLCPathToFollow.Create(FScene);
  FScene.Add(FSplinePathToFollow, LAYER_PATH);
  FSplinePathToFollow.Border.Color := BGRA(0,255,255);
  FSplinePathToFollow.Border.Width := 4.0;
  FSplinePathToFollow.Border.LinePosition := lpMiddle;
  FSplinePathToFollow.InitFromPath(Path.ToSpline(TSplineStyle(FramePathEditor.ComboBox1.ItemIndex)), False);
  FSplinePathToFollow.Visible := FramePathEditor.CBUseSpline.Checked;

  SetLength(FNodes, Length(Path));
  for i:=0 to High(FNodes) do begin
    FNodes[i] := TSprite.Create(texHandlePathNodeCircle, False);
    FScene.Add(FNodes[i], LAYER_PATH);
    FNodes[i].SetCenterCoordinate(Path[i]);
    FNodes[i].Frame := 2;
    FNodes[i].Visible := FramePathEditor.CheckBox4.Checked;
  end;

  ComputeCurveLineWidth;
end;

function TScreenPathEditor.GetPathData: string;
begin
  Result := Path.SaveNormalizedToString(Project.Config.SceneWidth, Project.Config.SceneHeight);
end;

procedure TScreenPathEditor.DeleteAllNodes;
begin
  Path := NIL;
  UpdateCurves;
end;

function TScreenPathEditor.NodeIndexAt(aX, aY: integer): integer;
var p: TPointF;
  i: integer;
begin
  p := Camera.ControlToWorld(aX, aY);
  for i:=0 to High(Path) do
    if InRange(p.x, Path[i].x-FHalf_Width_Point, Path[i].x+FHalf_Width_Point) and
       InRange(p.y, Path[i].y-FHalf_Width_Point, Path[i].y+FHalf_Width_Point) then exit(i);
  Result := -1;
end;

function TScreenPathEditor.AddNode(aX, aY: integer): integer;
begin
  Path.ConcatPoints([Camera.ControlToWorld(aX, aY)]);
  Result := High(Path);
  UpdateCurves;
  FramePathEditor.Modified := True;
end;

procedure TScreenPathEditor.InsertNode(aX, aY, aIndex: integer);
begin
  system.Insert(Camera.ControlToWorld(aX, aY), Path, aIndex);
  UpdateCurves;
  FramePathEditor.Modified := True;
end;

function TScreenPathEditor.CanInsertNodeAt(aX, aY: integer; out
  aIndexToInsert: integer): boolean;
var i: integer;
  p: TPointF;
  function ColLinePoint(aLinePt1, aLinePt2, aPt: TPointF): boolean;
  var d1, d2, lineLen, buffer: single;
  begin
    // get distance from the point to the two ends of the line
    d1 := Distance(aPt, aLinePt1);
    d2 := Distance(aPt, aLinePt2);
    // get the length of the line
    lineLen := Distance(aLinePt1, aLinePt2);
    // since floats are so minutely accurate, add
    // a little buffer zone that will give collision
    buffer := 0.1; //FHalf_Width_Point;    // higher # = less accurate
    // if the two distances are equal to the line's length, the point is on the line!
    // note we use the buffer here to give a range, rather than one #
    Result := (d1+d2 >= lineLen-buffer) and (d1+d2 <= lineLen+buffer);
  end;
begin
  p := Camera.ControlToWorld(aX, aY);
  for i:=0 to High(Path)-1 do
    if Collision.LinePoint(Path[i], Path[i+1], p) then begin
      Result := True;
      aIndexToInsert := i+1;
      exit;
    end;
  Result := False;
end;

procedure TScreenPathEditor.DeleteNode(aNodeIndex: integer);
begin
  system.Delete(Path, aNodeIndex, 1);
  UpdateCurves;
  FramePathEditor.Modified := True;
end;

procedure TScreenPathEditor.HighlightNode(aNodeindex: integer);
begin
  if InRange(aNodeIndex, 0, High(FNodes)) then
    FNodes[aNodeIndex].Tint.Value := BGRA(255,255,255);
end;

procedure TScreenPathEditor.HighlightNone;
var i: integer;
begin
  for i:=0 to High(FNodes) do
    FNodes[i].Tint.Alpha.Value := 0;
end;

procedure TScreenPathEditor.ShowLevel(aGroupIndex, aLevelIndex: integer);
var group: TLevelGroup;
  level: PLevelBankItem;
begin
  HideLevel;
  if aGroupIndex = -1 then exit;
  if aLevelIndex = -1 then exit;

  group := LevelBank.Items[aGroupIndex];
  FLevelTextures := group.Textures;
  FLevelSurfaces.Textures := FLevelTextures;
  level := group.Mutable[aLevelIndex];

  FLevelSurfaces.LoadFromString(level^.surfaces);
  ZoomViewToFit(Surfaces.GetItemsBounds, 0.8);
end;

procedure TScreenPathEditor.HideLevel;
begin
  FLevelSurfaces.Clear;
  FLevelTextures := NIL;
end;

procedure TScreenPathEditor.ShowSprite(aSpriteIndex: integer);
var itemSprite: PSpriteBankItem;
begin
  HideSprite;
  itemSprite := SpriteBank.GetItemByIndex(aSpriteIndex);
  if itemSprite <> NIL then begin
    FSpriteTextures.LoadFromString(itemSprite^.textures);
    FSpriteSurfaces.LoadFromString(itemSprite^.surfaces);
    FSpriteSurfaces.CenterOnScene;
  end;
end;

procedure TScreenPathEditor.HideSprite;
begin
  FSpriteSurfaces.Clear;
  FSpriteTextures.Clear;
end;

procedure TScreenPathEditor.ProcessMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: Integer;
begin
  i := NodeIndexAt(X, Y);

  case MouseState of
    msMouseDownOnCircleNode: if i = -1 then MouseState := msWaitingForNextCircleNode
      else MouseState := msOverCircleNode;

    msMovingCircleNode: MouseState := msOverCircleNode;
  end;
end;

procedure TScreenPathEditor.ProcessMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: Integer;
begin
 // inherited ProcessMouseDown(Button, Shift, X, Y);
  i := NodeIndexAt(X, Y);
  ClickOrigin := PointF(X, Y);

  case MouseState of
    msIdle:;
    msWaitingForNextCircleNode: begin
      if Button = mbLeft then begin
        FNodeindexClicked := AddNode(X, Y);
        DoLoopMoveNode;
      end;
    end;
    msOverCircleNode: begin
      case Button of
        mbRight: if i <> -1 then DeleteNode(i);
        mbLeft: begin
          MouseState := msMouseDownOnCircleNode;
          FNodeindexClicked := i;
        end;
      end;

    end;
    msCanInsertCircleNode: begin
      InsertNode(X, Y, FNodeIndexToInsert);
      FNodeindexClicked := FNodeIndexToInsert;
      DoLoopMoveNode;
    end;
  end;
end;

procedure TScreenPathEditor.ProcessMouseMove(Shift: TShiftState; X, Y: Integer);
var i: Integer;
  thresholdDone: Boolean;
begin
  //inherited ProcessMouseMove(Shift, X, Y);
  i := NodeIndexAt(X, Y);
  thresholdDone := Distance(ClickOrigin, PointF(X, Y)) > PPIScale(5);

  case MouseState of
    msIdle, msWaitingForNextCircleNode, msOverCircleNode, msCanInsertCircleNode: begin
      if i = -1 then begin
        if CanInsertNodeAt(X, Y, FNodeIndexToInsert) then MouseState := msCanInsertCircleNode
          else begin
            MouseState := msWaitingForNextCircleNode;
            HighlightNone;
          end;
      end else begin
          MouseState := msOverCircleNode;
          HighlightNone;
          HighlightNode(i);
      end;
    end;
    msMouseDownOnCircleNode: if thresholdDone then DoLoopMoveNode;
  end;
end;

procedure TScreenPathEditor.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
  ComputeCurveLineWidth;
end;

procedure TScreenPathEditor.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited ProcessOnKeyUp(Key, Shift);
end;

procedure TScreenPathEditor.CreateObjects;
var item: TPathDescriptorItem;
begin
  ShowLayers([LAYER_TOP, LAYER_UI, LAYER_LEVELBANK, LAYER_SPRITEBANK, LAYER_PATH]);
  ShowSceneBounds;
  CreateCamera([LAYER_LEVELBANK, LAYER_SPRITEBANK, LAYER_PATH, LAYER_SCENEBOUNDS]);

  ZoomOnScene;

  FLevelSurfaces := TSurfaceList.Create;
  FLevelSurfaces.WorkingLayer := LAYER_LEVELBANK;

  FSpriteTextures := TTextureList.Create;
  FSpriteSurfaces := TSurfaceList.Create;
  FSpriteSurfaces.Textures := FSpriteTextures;
  FSpriteSurfaces.WorkingLayer := LAYER_SPRITEBANK;


  if FPathNameToEdit <> '' then begin
    // editing an existing path
    item := PathBank.GetByName(FPathNameToEdit);
    if item = NIL then exit;

    Path := NIL;
    Path.LoadNormalizedFromStringAndExpand(item.PathData,
                                           Project.Config.SceneWidth, Project.Config.SceneHeight,
                                           False, item.SplineStyle);
    UpdateCurves;
    FramePathEditor.EditPathFromBank(item);
    FormMain.ShowPagePathEditor;
    // zoom whole view
    ZoomViewToFit(Path.Bounds, 0.9);
    ComputeCurveLineWidth;
    ComputeSceneBoundsLineWidth;
  end else begin
    // editing a new path
    FramePathEditor.EditNewPath;
    Path := NIL;
    UpdateCurves;
    ZoomOnScene;
  end;

end;

procedure TScreenPathEditor.FreeObjects;
begin
  FLevelSurfaces.Clear;
  FLevelSurfaces.Free;
  FLevelSurfaces := NIL;
  FLevelTextures := NIL;

  FSpriteSurfaces.Clear;
  FSpriteSurfaces.Free;
  FSpriteSurfaces := NIL;
  FSpriteTextures.Free;
  FSpriteTextures := NIL;

  FPathToFollow := NIL;
  FSplinePathToFollow := NIL;
  FNodes := NIL;


  FreeCamera;
  FScene.Layer[LAYER_PATH].Clear;
  HideSceneBounds;
end;

procedure TScreenPathEditor.Initialize;
begin
  FHalf_Width_Point := FScene.ScaleDesignToScene(5);
end;

procedure TScreenPathEditor.Finalize;
begin

end;

procedure TScreenPathEditor.EditPathFromBank(const aName: string);
begin
  FPathNameToEdit := aName;
end;

procedure TScreenPathEditor.EditNewPath;
begin
  FPathNameToEdit := '';
end;

function TScreenPathEditor.Surfaces: TSurfaceList;
begin
  Result := FLevelSurfaces;
end;

function TScreenPathEditor.Textures: TTextureList;
begin
  Result := FLevelTextures;
end;

end.

