unit u_ui_handle;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmap, BGRABitmapTypes;

type

TScaleHandle = (shTopLeft=0, shTopCenter, shTopRight, shCenterRight, shBottomRight,
                shBottomCenter, shBottomLeft, shCenterLeft);

{ TUIHandleManager }

TUIHandleManager = record
private
  FPivot: TSprite;
  RotateHandle: array[TOGLCCorner] of TSprite;
  ScaleHandle: array[TScaleHandle] of TSprite;
public
  function IsVisible: boolean;
  function ScaleVisible: boolean;
  function RotateVisible: boolean;
  function PivotVisible: boolean;
  procedure InitDefault;
  procedure KillSurfaces;
  procedure HideAll;
  procedure ShowPivotHandle(aSurface: TSimpleSurfaceWithEffect);
  procedure ToogleScaledAndRotatedHandle(aSurface: TSimpleSurfaceWithEffect);
  procedure ShowScaleHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);
  procedure ShowRotateHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);

  procedure UpdateHandlePosition(aSurface: TSimpleSurfaceWithEffect);

  function IsOverPivotHandle(aWorldPt: TPointF): boolean;
  function IsOverRotateHandle(aWorldPt: TPointF): boolean;
  function IsOverScaleHandle(aWorldPt: TPointF; out aType: TScaleHandle): boolean;
end;
PUIHandleManager = ^TUIHandleManager;

{ TUIHandle }

TUIHandle = record
private
  FTargetLayer: integer;
public
  procedure InitDefault;

  procedure CreateUIHandles(aUIHandleManager: PUIHandleManager);

  // the layer where the UI appears
  property TargetLayer: integer read FTargetLayer write FTargetLayer;
end;

var UIHandle: TUIHandle;

implementation
uses u_common, u_ui_atlas;

{ TUIHandleManager }

procedure TUIHandleManager.ShowPivotHandle(aSurface: TSimpleSurfaceWithEffect);
var p: TPointF;
begin
  with aSurface do
    p := GetMatrixSurfaceToScene.Transform(PointF(Width*Pivot.x, Height*Pivot.y));
  FPivot.SetCenterCoordinate(p);
  FPivot.Visible := True;
end;

procedure TUIHandleManager.ToogleScaledAndRotatedHandle(
  aSurface: TSimpleSurfaceWithEffect);
begin
  if IsVisible then begin
    if ScaleVisible then ShowRotateHandleAndPivot(aSurface)
      else ShowScaleHandleAndPivot(aSurface);
  end else ShowScaleHandleAndPivot(aSurface);
end;

function TUIHandleManager.IsVisible: boolean;
begin
  Result := ScaleVisible or RotateVisible or PivotVisible;
end;

function TUIHandleManager.ScaleVisible: boolean;
begin
  if ScaleHandle[shTopLeft] = NIL then exit(False)
    else Result := ScaleHandle[shTopLeft].Visible;
end;

function TUIHandleManager.RotateVisible: boolean;
begin
  if RotateHandle[cTL] = NIL then exit(False)
    else Result := RotateHandle[cTL].Visible;
end;

function TUIHandleManager.PivotVisible: boolean;
begin
  if FPivot = NIL then exit(False)
    else Result := FPivot.Visible;
end;

procedure TUIHandleManager.InitDefault;
begin
  Self := Default(TUIHandleManager);
end;

procedure TUIHandleManager.HideAll;
var i: TOGLCCorner;
  j: TScaleHandle;
begin
  FPivot.Visible := False;

  for i in TOGLCCorner do
    RotateHandle[i].Visible := False;

  for j in TScaleHandle do
    ScaleHandle[j].Visible := False;;
end;

procedure TUIHandleManager.ShowScaleHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);
var w, h: single;
  r: TRectF;
begin
  HideAll;

  // pivot
  ShowPivotHandle(aSurface);

  // scale handle
  r := aSurface.GetRectAreaInSceneSpace(True);
  w := ScaleHandle[shTopLeft].Width;
  h := ScaleHandle[shTopLeft].Height;

  ScaleHandle[shTopLeft].Visible := True;
  ScaleHandle[shTopLeft].SetCoordinate(r.Left-w, r.Top-h);

  ScaleHandle[shTopCenter].Visible := True;
  ScaleHandle[shTopCenter].SetCoordinate(r.Left+r.Width*0.5-w*0.5, r.Top-h);

  ScaleHandle[shTopRight].Visible := True;
  ScaleHandle[shTopRight].SetCoordinate(r.Right, r.Top-h);

  ScaleHandle[shCenterRight].Visible := True;
  ScaleHandle[shCenterRight].SetCoordinate(r.Right, r.Top+r.Height*0.5-h*0.5);

  ScaleHandle[shBottomRight].Visible := True;
  ScaleHandle[shBottomRight].SetCoordinate(r.Right, r.Bottom);

  ScaleHandle[shBottomCenter].Visible := True;
  ScaleHandle[shBottomCenter].SetCoordinate(r.Left+r.Width*0.5-w*0.5, r.Bottom);

  ScaleHandle[shBottomLeft].Visible := True;
  ScaleHandle[shBottomLeft].SetCoordinate(r.Left-w, r.Bottom);

  ScaleHandle[shCenterLeft].Visible := True;
  ScaleHandle[shCenterLeft].SetCoordinate(r.Left-w, r.Top+r.Height*0.5-h*0.5);
end;

procedure TUIHandleManager.ShowRotateHandleAndPivot(aSurface: TSimpleSurfaceWithEffect);
var w, h: single;
  r: TRectF;
begin
  HideAll;
  ShowPivotHandle(aSurface);

  // rotated handle
  r := aSurface.GetRectAreaInSceneSpace(True);
  w := RotateHandle[cTR].Width;
  h := RotateHandle[cTR].Height;

  RotateHandle[cTL].Visible := True;
  RotateHandle[cTL].SetCoordinate(r.Left-w, r.Top-h);

  RotateHandle[cTR].Visible := True;
  RotateHandle[cTR].SetCoordinate(r.Right, r.Top-h);

  RotateHandle[cBR].Visible := True;
  RotateHandle[cBR].SetCoordinate(r.Right, r.Bottom);

  RotateHandle[cBL].Visible := True;
  RotateHandle[cBL].SetCoordinate(r.Left-w, r.Bottom);

end;

procedure TUIHandleManager.UpdateHandlePosition(aSurface: TSimpleSurfaceWithEffect);
begin
  if not IsVisible then exit;
  if ScaleVisible then ShowScaleHandleAndPivot(aSurface);
  if RotateVisible then ShowRotateHandleAndPivot(aSurface);
end;

function TUIHandleManager.IsOverPivotHandle(aWorldPt: TPointF): boolean;
var pointCollision: TOGLCBodyItem;
begin
  if not PivotVisible then exit(False);

  pointCollision.BodyType:= _btPoint;
  pointCollision.pt := aWorldPt;

  FPivot.CollisionBody.SetTransformMatrix(FPivot.GetMatrixSurfaceToScene);
  Result := FPivot.CollisionBody.CheckCollisionWith(pointCollision);
end;

function TUIHandleManager.IsOverRotateHandle(aWorldPt: TPointF): boolean;
var pointCollision: TOGLCBodyItem;
begin
  if not RotateVisible then exit(False);

  pointCollision.BodyType:= _btPoint;
  pointCollision.pt := aWorldPt;

  RotateHandle[cTL].CollisionBody.SetTransformMatrix(RotateHandle[cTL].GetMatrixSurfaceToScene);
  Result := RotateHandle[cTL].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cTR].CollisionBody.SetTransformMatrix(RotateHandle[cTR].GetMatrixSurfaceToScene);
  Result := RotateHandle[cTR].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cBR].CollisionBody.SetTransformMatrix(RotateHandle[cBR].GetMatrixSurfaceToScene);
  Result := RotateHandle[cBR].CollisionBody.CheckCollisionWith(pointCollision);
  if Result then exit;

  RotateHandle[cBL].CollisionBody.SetTransformMatrix(RotateHandle[cBL].GetMatrixSurfaceToScene);
  Result := RotateHandle[cBL].CollisionBody.CheckCollisionWith(pointCollision);
end;

function TUIHandleManager.IsOverScaleHandle(aWorldPt: TPointF; out aType: TScaleHandle): boolean;
var pointCollision: TOGLCBodyItem;
  i: TScaleHandle;
begin
  if not ScaleVisible then exit(False);

  pointCollision.BodyType:= _btPoint;
  pointCollision.pt := aWorldPt;

  for i in TScaleHandle do begin
    ScaleHandle[i].CollisionBody.SetTransformMatrix(ScaleHandle[i].GetMatrixSurfaceToScene);
    if ScaleHandle[i].CollisionBody.CheckCollisionWith(pointCollision) then begin
      aType := i;
      exit(True);
    end;
  end;
  Result := False;
end;

procedure TUIHandleManager.KillSurfaces;
var i: TOGLCCorner;
  j: TScaleHandle;
begin
  if FPivot <> NIL then FPivot.Kill;
  FPivot := NIL;

  for i in TOGLCCorner do begin
    if RotateHandle[i] <> NIL then RotateHandle[i].Kill;
    RotateHandle[i] := NIL;
  end;

  for j in TScaleHandle do begin
    if ScaleHandle[j] <> NIL then ScaleHandle[j].Kill;
    ScaleHandle[j] := NIL;
  end;
end;

{ TUIHandle }

procedure TUIHandle.InitDefault;
begin
  FillChar(Self, SizeOf(TUIHandle), 0);
end;

procedure TUIHandle.CreateUIHandles(aUIHandleManager: PUIHandleManager);
var i: TScaleHandle;
  procedure CreateCollisionRectangle(aSurface: TSimpleSurfaceWithEffect);
  begin
    with aSurface do
      CollisionBody.AddPolygon([PointF(0,0), PointF(Width, 0), PointF(Width, Height), PointF(0, Height)]);
    aSurface.Visible := False;
  end;
begin
  with aUIHandleManager^ do begin
    InitDefault;
    FPivot := FScene.AddSprite(texHandlePivot, False, TargetLayer);
    CreateCollisionRectangle(FPivot);

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

    for i in TScaleHandle do begin
      ScaleHandle[i] := FScene.AddSprite(texArrowH, False, TargetLayer);
      ScaleHandle[i].Angle.Value := 45*(Ord(i)+1);
      CreateCollisionRectangle(ScaleHandle[i]);
    end;
  end;
end;

end.

