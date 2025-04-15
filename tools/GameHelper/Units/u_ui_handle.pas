unit u_ui_handle;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes;

type

{ TUIHandleManager }

TUIHandleManager = record
private
  Pivot: TSprite;
  RotateHandle: array[TOGLCCorner] of TSprite;
  SelectedHandle: array[TOGLCCorner] of TSprite;
public
  IsVisible: boolean;
  function SelectVisible: boolean;
  function RotateVisible: boolean;
  procedure InitDefault;
  procedure KillSurfaces;
  procedure HideAll;
  procedure ShowPivotHandle(aSurface: TSimpleSurfaceWithEffect);
  procedure ToogleSelectedAndRotatedHandle(aSurface: TSimpleSurfaceWithEffect);
  procedure ShowSelectedHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);
  procedure ShowRotateHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);

  procedure UpdateHandlePosition(aSurface: TSimpleSurfaceWithEffect);

  function IsOverPivotHandle(aWorldPt: TPointF): boolean;
  function IsOverRotateHandle(aWorldPt: TPointF): boolean;
end;
PUIHandleManager = ^TUIHandleManager;

{ TUIHandle }

TUIHandle = record
private
  FAtlas: TAtlas;
  texHandlePivot, texHandleRotate, texHandleSelected: PTexture;
  FTargetLayer: integer;
public
  procedure InitDefault;
  procedure CreateAtlas;
  procedure FreeAtlas;

  procedure CreateUIHandles(aUIHandleManager: PUIHandleManager);

  // the layer where the UI appears
  property TargetLayer: integer read FTargetLayer write FTargetLayer;
end;

var UIHandle: TUIHandle;

implementation
uses u_app_pref, u_common;

{ TUIHandleManager }

procedure TUIHandleManager.ShowPivotHandle(aSurface: TSimpleSurfaceWithEffect);
var p: TPointF;
begin
  with aSurface do
    p := GetMatrixSurfaceToWorld.Transform(PointF(Width*Pivot.x, Height*Pivot.y));
  Pivot.SetCenterCoordinate(p);
  Pivot.Visible := True;
end;

procedure TUIHandleManager.ToogleSelectedAndRotatedHandle(aSurface: TSimpleSurfaceWithEffect);
begin
  if IsVisible then begin
    if SelectVisible then ShowRotateHandleAndPivot(aSurface)
      else ShowSelectedHandleAndPivot(aSurface);
  end else ShowSelectedHandleAndPivot(aSurface);
end;

function TUIHandleManager.SelectVisible: boolean;
begin
  Result := SelectedHandle[cTL].Visible;
end;

function TUIHandleManager.RotateVisible: boolean;
begin
  Result := RotateHandle[cTL].Visible;
end;

procedure TUIHandleManager.InitDefault;
begin
  FillChar(Self, SizeOf(TUIHandleManager), 0);
end;

procedure TUIHandleManager.HideAll;
var i: TOGLCCorner;
begin
  Pivot.Visible := False;
  for i in TOGLCCorner do begin
    RotateHandle[i].Visible := False;
    SelectedHandle[i].Visible := False;
  end;
  IsVisible := False;
end;

procedure TUIHandleManager.ShowSelectedHandleAndPivot(
  aSurface: TSimpleSurfaceWithEffect);
var w, h: single;
  r: TRectF;
begin
  HideAll;
  IsVisible := True;

  // pivot
  ShowPivotHandle(aSurface);

  // selected handle
  r := aSurface.GetRectAreaInWorldSpace(True);
  w := SelectedHandle[cTR].Width;
  h := SelectedHandle[cTR].Height;

  SelectedHandle[cTL].Visible := True;
  SelectedHandle[cTL].SetCoordinate(r.TopLeft);

  SelectedHandle[cTR].Visible := True;
  SelectedHandle[cTR].SetCoordinate(r.Right-w, r.Top);

  SelectedHandle[cBR].Visible := True;
  SelectedHandle[cBR].SetCoordinate(r.Right-w, r.Bottom-h);

  SelectedHandle[cBL].Visible := True;
  SelectedHandle[cBL].SetCoordinate(r.Left, r.Bottom-h);
end;

procedure TUIHandleManager.ShowRotateHandleAndPivot(
  aSurface: TSimpleSurfaceWithEffect);
var w, h: single;
  r: TRectF;
begin
  HideAll;
  IsVisible := True;
  ShowPivotHandle(aSurface);

  // rotated handle
  r := aSurface.GetRectAreaInWorldSpace(True);
  w := RotateHandle[cTR].Width;
  h := RotateHandle[cTR].Height;

  RotateHandle[cTL].Visible := True;
  RotateHandle[cTL].SetCoordinate(r.TopLeft);

  RotateHandle[cTR].Visible := True;
  RotateHandle[cTR].SetCoordinate(r.Right-w, r.Top);

  RotateHandle[cBR].Visible := True;
  RotateHandle[cBR].SetCoordinate(r.Right-w, r.Bottom-h);

  RotateHandle[cBL].Visible := True;
  RotateHandle[cBL].SetCoordinate(r.Left, r.Bottom-h);

end;

procedure TUIHandleManager.UpdateHandlePosition(aSurface: TSimpleSurfaceWithEffect);
begin
  if not IsVisible then exit;
  if SelectVisible then ShowSelectedHandleAndPivot(aSurface);
  if RotateVisible then ShowRotateHandleAndPivot(aSurface);
end;

function TUIHandleManager.IsOverPivotHandle(aWorldPt: TPointF): boolean;
var pointCollision: TOGLCBodyItem;
begin
  if not IsVisible then exit(False);

  pointCollision.BodyType:= _btPoint;
  pointCollision.pt := aWorldPt;

  Pivot.CollisionBody.SetSurfaceToWordMatrix(Pivot.GetMatrixSurfaceToWorld);
  Result := Pivot.CollisionBody.CheckCollisionWith(pointCollision);
end;

function TUIHandleManager.IsOverRotateHandle(aWorldPt: TPointF): boolean;
var pointCollision: TOGLCBodyItem;
begin
  if not IsVisible then exit(False);

  pointCollision.BodyType:= _btPoint;
  pointCollision.pt := aWorldPt;

  RotateHandle[cTL].CollisionBody.SetSurfaceToWordMatrix(RotateHandle[cTL].GetMatrixSurfaceToWorld);
  Result := RotateHandle[cTL].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cTR].CollisionBody.SetSurfaceToWordMatrix(RotateHandle[cTR].GetMatrixSurfaceToWorld);
  Result := RotateHandle[cTR].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cBR].CollisionBody.SetSurfaceToWordMatrix(RotateHandle[cBR].GetMatrixSurfaceToWorld);
  Result := RotateHandle[cBR].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cBL].CollisionBody.SetSurfaceToWordMatrix(RotateHandle[cBL].GetMatrixSurfaceToWorld);
  Result := RotateHandle[cBL].CollisionBody.CheckCollisionWith(pointCollision);
end;

procedure TUIHandleManager.KillSurfaces;
var i: TOGLCCorner;
begin
  if Pivot <> NIL then Pivot.Kill;
  Pivot := NIL;
  for i in TOGLCCorner do begin
    if RotateHandle[i] <> NIL then RotateHandle[i].Kill;
    RotateHandle[i] := NIL;
    if SelectedHandle[i] <> NIL then SelectedHandle[i].Kill;
    SelectedHandle[i] := NIL;
  end;
  IsVisible := False;
end;

{ TUIHandle }

procedure TUIHandle.InitDefault;
begin
  FillChar(Self, SizeOf(TUIHandle), 0);
end;

procedure TUIHandle.CreateAtlas;
begin
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 2;

  texHandlePivot := FAtlas.AddFromSVG(GetHandleFolder+'Pivot.svg', PPIScale(12), -1);
  texHandleRotate := FAtlas.AddFromSVG(GetHandleFolder+'Rotate.svg', PPIScale(19), -1);
  texHandleSelected := FAtlas.AddFromSVG(GetHandleFolder+'Selected.svg', PPIScale(19), -1);

  FAtlas.TryToPack;
  FAtlas.Build;
  FAtlas.FreeItemImages;
end;

procedure TUIHandle.FreeAtlas;
begin
  FAtlas.Free;
  FAtlas := NIL;
end;

procedure TUIHandle.CreateUIHandles(aUIHandleManager: PUIHandleManager);
  procedure CreateCollisionRectangle(aSurface: TSimpleSurfaceWithEffect);
  begin
    with aSurface do
      CollisionBody.AddPolygon([PointF(0,0), PointF(Width, 0), PointF(Width, Height), PointF(0, Height)]);
    aSurface.Visible := False;
  end;
begin
  with aUIHandleManager^ do begin
    InitDefault;
    Pivot := FScene.AddSprite(texHandlePivot, False, TargetLayer);
    CreateCollisionRectangle(Pivot);

    RotateHandle[cTL] := FScene.AddSprite(texHandleRotate, False, TargetLayer);
    RotateHandle[cTL].FlipH := True;
    CreateCollisionRectangle(RotateHandle[cTL]);

    RotateHandle[cTR] := FScene.AddSprite(texHandleRotate, False, TargetLayer);
    CreateCollisionRectangle(RotateHandle[cTR]);

    RotateHandle[cBR] := FScene.AddSprite(texHandleRotate, False, TargetLayer);
    RotateHandle[cBR].FlipV := True;
    CreateCollisionRectangle(RotateHandle[cBR]);

    RotateHandle[cBL] := FScene.AddSprite(texHandleRotate, False, TargetLayer);
    RotateHandle[cBL].FlipH := True;
    RotateHandle[cBL].FlipV := True;
    CreateCollisionRectangle(RotateHandle[cBL]);

    SelectedHandle[cTL] := FScene.AddSprite(texHandleSelected, False, TargetLayer);
    SelectedHandle[cTL].FlipH := True;
    SelectedHandle[cTL].Visible := False;

    SelectedHandle[cTR] := FScene.AddSprite(texHandleSelected, False, TargetLayer);
    SelectedHandle[cTR].Visible := False;

    SelectedHandle[cBR] := FScene.AddSprite(texHandleSelected, False, TargetLayer);
    SelectedHandle[cBR].FlipV := True;
    SelectedHandle[cBR].Visible := False;

    SelectedHandle[cBL] := FScene.AddSprite(texHandleSelected, False, TargetLayer);
    SelectedHandle[cBL].FlipH := True;
    SelectedHandle[cBL].FlipV := True;
    SelectedHandle[cBL].Visible := False;
  end;
end;

end.

