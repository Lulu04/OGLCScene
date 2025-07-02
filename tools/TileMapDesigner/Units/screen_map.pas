unit screen_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  common;

type

{ TScreenMap }

TScreenMap = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas;
  TexGrid: PTexture;
  FGrid: TSprite;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const{%H-}AElapsedTime: single ); override;

  procedure BindTextureAtlas;
end;

var ScreenMap: TScreenMap = NIL;

implementation
uses umaps;

{ TScreenMap }


procedure TScreenMap.CreateObjects;
var o: TSprite;
  ima : TBGRABitmap;
  margin: integer;
  fd: TFontDescriptor;
begin
  FAtlas := FScene.CreateAtlas;
  FAtlas.Spacing := 1;

  fd.Create('Arial', 9, [fsBold], BGRA(200,200,200));
  FEventFont:= FAtlas.AddTexturedFont(fd, FScene.Charsets.ASCII_SYMBOL+FScene.Charsets.SIMPLELATIN);

  fd.Create('Arial', 14, [], BGRA(255,255,100), BGRA(0,0,0,200), 3, BGRA(0,0,0,180), 3, 3, 5);
  FHintFont:= FAtlas.AddTexturedFont(fd, FScene.Charsets.ASCII_SYMBOL+FScene.Charsets.SIMPLELATIN);

  ima := TBGRABitmap.Create(FScene.Width, FScene.Height);
  ima.Fill(FImageBackGround);
  TexGrid := FAtlas.Add(ima);

  FAtlas.TryToPack;
  FAtlas.Build;

  FLabelMapPosition := TFreeText.Create(FScene);
  FLabelMapPosition.TexturedFont := FHintFont;
  FLabelMapPosition.Caption := 'Map position:';
  FLabelMapPosition.SetCoordinate(0, 30);
  FScene.add(FLabelMapPosition, Layer_InfoMap);

  // title
  fd.Create('Arial', FScene.ScaleDesignToScene(40), [fsBold], BGRA(255,255,100));
  o := TSprite.Create(FScene, fd, 'T I L E    M A P    D E S I G N E R');
  FScene.Add(o, Layer_InfoMap);
  o.CenterOnScene;
  o.Opacity.ChangeTo(0, 5, idcStartSlowEndFast);
  o.Scale.ChangeTo(PointF(1.2, 1.2), 6);
  o.KillDefered(5);

  margin := FScene.ScaleDesignToScene(5);

  FLabelTileIndexes := TFreeText.Create(FScene);
  FLabelTileIndexes.TexturedFont := FHintFont;
  FLabelTileIndexes.Caption := 'Tile:';
  FScene.Add(FLabelTileIndexes, Layer_InfoMap);
  FLabelTileIndexes.SetCoordinate(0, FLabelMapPosition.BottomY+margin);

  FLabelSelectionInfo := TFreeText.Create(FScene);
  FLabelSelectionInfo.TexturedFont := FHintFont;
  FLabelSelectionInfo.Caption := 'Sel:';
  FScene.add(FLabelSelectionInfo, Layer_InfoMap);
  FLabelSelectionInfo.SetCoordinate(0, FLabelTileIndexes.BottomY+margin);

  FLabelGroundType := TFreeText.Create(FScene);
  FLabelGroundType.TexturedFont := FHintFont;
  FLabelGroundType.Caption := 'Ground:';
  FLabelGroundType.SetCoordinate(0, FLabelSelectionInfo.BottomY+margin);
  FScene.Add(FLabelGroundType, Layer_InfoMap);

  FLabelEventName := TFreeText.Create(FScene);
  FLabelEventName.TexturedFont := FHintFont;
  FLabelEventName.Caption := 'Event:';
  FLabelEventName.SetCoordinate(0, FLabelGroundType.BottomY+margin);
  FScene.Add(FLabelEventName, Layer_InfoMap);

  FLabelDebug := TFreeText.Create(FScene);
  FLabelDebug.TexturedFont := FEventFont;
  FLabelDebug.Caption := 'Debug';
  FLabelDebug.SetCoordinate(0, FScene.Height-FLabelDebug.Height);
  FScene.Add(FLabelDebug, Layer_InfoMap);

  FGrid := TSprite.create(TexGrid);
  FGrid.SetCoordinate(0, 0);
  FGrid.Opacity.Value := 150;
  FScene.Add(FGrid, Layer_Grid);

  FReady := TRUE;
end;

procedure TScreenMap.FreeObjects;
begin
  FScene.OnAfterPaint := NIL;
  FScene.ClearAllLayer;
  FreeAndNil(FAtlas);
end;

procedure TScreenMap.Update(const AElapsedTime: single);
begin

  FLabelDebug.Caption := 'TileEngine: ('+inttostr(round(MapList.MainMap.TileEngine.X.Value))+','+inttostr(round(MapList.MainMap.TileEngine.Y.Value))+')    '+
                         ' WH: '+inttostr(MapList.MainMap.TileEngine.Width)+','+inttostr(MapList.MainMap.TileEngine.Height);
end;

procedure TScreenMap.BindTextureAtlas;
begin
  FScene.TexMan.Bind(FAtlas.Texture);
end;


end.

