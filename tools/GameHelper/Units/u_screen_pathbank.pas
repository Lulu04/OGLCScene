unit u_screen_pathbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene, BGRABitmap, BGRABitmapTypes, u_screen_template,
  u_common, u_ui_objectlist, u_surface_list, u_texture_list;

type

{ TScreenPathBank }

TScreenPathBank = class(TCustomScreenTemplate)
private
  FPathToFollow: TOGLCPathToFollow;
  FSplinePathToFollow: TOGLCPathToFollow;
  procedure ComputeCurveLineWidth;
public
  procedure ProcessMouseWheel({%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean); override;

  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure Initialize;
  procedure Finalize;

  function Surfaces: TSurfaceList; override;
  function Textures: TTextureList; override;

  procedure ClearView;
  procedure ShowPathFromBank(const aName: string);
  procedure CreateSplineCurve(const aPath: TOGLCPath; aSplineStyle: TSplineStyle);
end;

var ScreenPathBank: TScreenPathBank;

implementation
uses u_project, Math;

{ TScreenPathBank }

procedure TScreenPathBank.ComputeCurveLineWidth;
begin
  if FPathToFollow <> NIL then
    FPathToFollow.Border.Width := Max(4.0, 1.0/Zoom);

  if FSplinePathToFollow <> NIL then
    FSplinePathToFollow.Border.Width := Max(4.0, 1.0/Zoom);
end;

procedure TScreenPathBank.ProcessMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
  ComputeCurveLineWidth;
end;

procedure TScreenPathBank.CreateObjects;
begin
  ShowLayers([LAYER_TOP, LAYER_UI, LAYER_PATH]);
  ShowSceneBounds;
  CreateCamera([LAYER_PATH, LAYER_SCENEBOUNDS]);

  ZoomOnScene;
end;

procedure TScreenPathBank.FreeObjects;
begin
  FPathToFollow := NIL;
  FSplinePathToFollow := NIL;

  FreeCamera;
  FScene.Layer[LAYER_PATH].Clear;
  HideSceneBounds;
end;

procedure TScreenPathBank.Initialize;
begin

end;

procedure TScreenPathBank.Finalize;
begin

end;

function TScreenPathBank.Surfaces: TSurfaceList;
begin
  Result := NIL;
end;

function TScreenPathBank.Textures: TTextureList;
begin
  Result := NIL;
end;

procedure TScreenPathBank.ClearView;
begin
  if FPathToFollow <> NIL then FPathToFollow.Kill;
  FPathToFollow := NIL;

  if FSplinePathToFollow <> NIL then FSplinePathToFollow.Kill;
  FSplinePathToFollow := NIL;
end;

procedure TScreenPathBank.ShowPathFromBank(const aName: string);
var path: TOGLCPath;
  item: TPathDescriptorItem;
begin
  ClearView;
  path := NIL;
  item := PathBank.GetByName(aName);
  if item = NIL then exit;

  path.LoadNormalizedFromStringAndExpand(item.PathData, Project.Config.SceneWidth, Project.Config.SceneHeight);
  FPathToFollow := TOGLCPathToFollow.Create(FScene);
  FScene.Add(FPathToFollow, LAYER_PATH);
  FPathToFollow.InitFromPath(path, False);
  FPathToFollow.Border.Color := BGRA(255,255,0);
  FPathToFollow.Border.Width := 4.0;

  if item.UseSpline then CreateSplineCurve(path, item.SplineStyle);

  ZoomViewToFit(path.Bounds, 0.9);
  ComputeCurveLineWidth;
  ComputeSceneBoundsLineWidth;
end;

procedure TScreenPathBank.CreateSplineCurve(const aPath: TOGLCPath; aSplineStyle: TSplineStyle);
begin
  if FSplinePathToFollow <> NIL then FSplinePathToFollow.Kill;

  FSplinePathToFollow := TOGLCPathToFollow.Create(FScene);
  FScene.Insert(0, FSplinePathToFollow, LAYER_PATH);
  FSplinePathToFollow.Border.Color := BGRA(0,255,255);
  FSplinePathToFollow.Border.Width := 4.0;
  FSplinePathToFollow.Border.LinePosition := lpMiddle;
  FSplinePathToFollow.InitFromPath(aPath.ToSpline(aSplineStyle), False);

end;

end.

