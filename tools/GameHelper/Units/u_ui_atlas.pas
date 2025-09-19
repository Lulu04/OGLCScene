unit u_ui_atlas;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene;

type

{ TUIAtlas }

TUIAtlas = record
private
  FAtlas: TAtlas;
public
  procedure InitDefault;
  procedure CreateAtlas;
  procedure FreeAtlas;
end;

var
  UIAtlas: TUIAtlas;

  // texture handle
  texHandlePivot, texHandleRotate, texArrowH,
  texHandlePathNode: PTexture;
  // texture mouse cursor
  texMouseNormal,
  texSelectSurfaceByRect,
  texMouseOverSurface, texMouseOverPivot, texMouseRotateSurface, texMouseScaleSurface,
  texMouseOverNode, texMouseMovingNode, texMouseAddNode,
  texMouseToolPoint, texMouseToolLine, texMouseToolCircle, texMouseToolRectangle, texMouseToolPolygon: PTexture;

implementation

uses u_common, u_app_pref;

{ TUIAtlas }

procedure TUIAtlas.InitDefault;
begin
  Self := Default(TUIAtlas);
end;

procedure TUIAtlas.CreateAtlas;
var path: string;
begin
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 2;

  path := GetHandleFolder;
  texHandlePivot := FAtlas.AddFromSVG(path+'Pivot.svg', PPIScale(12), -1);
  texHandleRotate := FAtlas.AddFromSVG(path+'Rotate.svg', PPIScale(19), -1);
  texArrowH := FAtlas.AddFromSVG(path+'ArrowH.svg', PPIScale(19), -1);

  texHandlePathNode := FAtlas.AddMultiFrameImageFromSVG([path+'PathNode.svg',
                                                      path+'PathNodeSelected.svg'],
                                                      PPIScale(12), -1, 2, 1, 2);

  // mouse cursor
  path := GetCursorFolder;
  texMouseNormal := FAtlas.AddFromSVG(path+'Select.svg', PPIScale(32), -1);
  texSelectSurfaceByRect := FAtlas.AddFromSVG(path+'SelectSurfaceByRect.svg', PPIScale(32), -1);
  texMouseOverSurface := FAtlas.AddFromSVG(path+'OverSurface.svg', PPIScale(32), -1);
  texMouseOverPivot := FAtlas.AddFromSVG(path+'OverPivot.svg', PPIScale(32), -1);
  texMouseRotateSurface := FAtlas.AddFromSVG(path+'RotateSurface.svg', PPIScale(32), -1);
  texMouseScaleSurface := FAtlas.AddFromSVG(path+'ScaleSurface.svg', PPIScale(32), -1);

  texMouseOverNode := FAtlas.AddFromSVG(path+'OverNode.svg', PPIScale(32), -1);
  texMouseMovingNode := FAtlas.AddFromSVG(path+'MovingNode.svg', PPIScale(32), -1);
  texMouseAddNode := FAtlas.AddFromSVG(path+'AddNode.svg', PPIScale(32), -1);
  texMouseToolPoint := FAtlas.AddFromSVG(path+'Point.svg', PPIScale(32), -1);
  texMouseToolLine := FAtlas.AddFromSVG(path+'Line.svg', PPIScale(32), -1);
  texMouseToolCircle := FAtlas.AddFromSVG(path+'Circle.svg', PPIScale(32), -1);
  texMouseToolRectangle := FAtlas.AddFromSVG(path+'Rectangle.svg', PPIScale(32), -1);
  texMouseToolPolygon := FAtlas.AddFromSVG(path+'Polygon.svg', PPIScale(32), -1);

  FAtlas.TryToPack;
  FAtlas.Build;
  FAtlas.FreeItemImages;
end;

procedure TUIAtlas.FreeAtlas;
begin
  FAtlas.Free;
  FAtlas := NIL;
end;

end.

