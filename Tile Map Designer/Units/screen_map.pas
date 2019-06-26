unit screen_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

type

{ TScreenMap }

TScreenMap = class( TStageSkeleton )
private
  TexGrid: PTexture;
  FGrid: TSprite;
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( {%H-}AElapsedTime: single ); override;
end;

var ScreenMap: TScreenMap = NIL;

implementation
uses umaps;

{ TScreenMap }


procedure TScreenMap.LoadData;
var o: TGUILabel;
  FImaGrid : TBGRABitmap;
begin
 FTitleFont := GuiFont( 'Arial', 32, [], BGRA(0,255,100), BGRA(20,100,50), 1, BGRA(0,0,0,0), 0, 0, 0 );

 FEventFont:= FontManager.AddFont(GuiFont( 'Arial', 9, [fsBold], BGRA(200,200,200), BGRA(200,200,200,0), 0, BGRA(0,0,0,0), 0, 0, 0));
 FHintFont:= FontManager.AddFont(GuiFont( 'Tahoma', 14, [], BGRA(255,255,100), BGRA(0,0,0,200), 3, BGRA(0,0,0,180), 3, 3, 5 ));

 FLabelMapPosition:=TFreeText.Create;
 FLabelMapPosition.TexturedFont:=FHintFont;
 FLabelMapPosition.Caption:='Map position:';
 FLabelMapPosition.SetCoordinate(0,30);
 FScene.add( FLabelMapPosition, Layer_InfoMap );

 o := TGUILabel.Create('T I L E    M A P    D E S I G N E R', FTitleFont );
 o.SetCenterCoordinate( FScene.Width/2, FScene.Height/2 );
 FScene.Add( o, Layer_InfoMap );
 o.Opacity.ChangeTo( 0, 5, idcStartSlowEndFast );
 o.Scale.ChangeTo( PointF(1.2,1.2), 6 );
 o.KillDefered( 5 );

 FLabelTileIndexes := TFreeText.Create;
 FLabelTileIndexes.TexturedFont:=FHintFont;
 FLabelTileIndexes.Caption:='Tile:';
 FScene.add( FLabelTileIndexes, Layer_InfoMap );
 FLabelTileIndexes.SetCoordinate( 0, FLabelMapPosition.BottomY+5 );

 FLabelSelectionInfo := TFreeText.Create;
 FLabelSelectionInfo.TexturedFont:=FHintFont;
 FLabelSelectionInfo.Caption:='Sel:';
 FScene.add( FLabelSelectionInfo, Layer_InfoMap );
 FLabelSelectionInfo.SetCoordinate( 0, FLabelTileIndexes.BottomY+5 );

 FLabelGroundType := TFreeText.Create;
 FLabelGroundType.TexturedFont:=FHintFont;
 FLabelGroundType.Caption := 'Ground:';
 FLabelGroundType.SetCoordinate( 0, FLabelSelectionInfo.BottomY+5 );
 FScene.Add( FLabelGroundType, Layer_InfoMap );

 FLabelEventName := TFreeText.Create;
 FLabelEventName.TexturedFont:=FHintFont;
 FLabelEventName.Caption := 'Event:';
 FLabelEventName.SetCoordinate( 0, FLabelGroundType.BottomY+5 );
 FScene.Add( FLabelEventName, Layer_InfoMap );

 FLabelDebug := TFreeText.Create;
 FLabelDebug.TexturedFont:=FEventFont;
 FLabelDebug.Caption := 'Debug';
 FLabelDebug.SetCoordinate( 0, FScene.Height-FLabelDebug.Height );
 FScene.Add( FLabelDebug, Layer_InfoMap );

 FImaGrid := TBGRABitmap.Create( FScene.Width, FScene.Height );
 FImaGrid.Fill( FImageBackGround );
 TexGrid := TextureManager.Add( FImaGrid );
 FImaGrid.free;
 FGrid := TSprite.create( TexGrid );
 FGrid.SetCoordinate( 0, 0 );
 FGrid.Opacity.Value := 150;
 FScene.Add( FGrid, Layer_Grid );

 FReady:=TRUE;
end;

procedure TScreenMap.FreeData;
begin
 FScene.OnAfterPaint := NIL;
 FScene.ClearAllLayer;
end;

procedure TScreenMap.Update(AElapsedTime: single);
begin

 FLabelDebug.Caption := 'TileEngine: ('+inttostr(round(MapList.MainMap.TileEngine.X.Value))+','+inttostr(round(MapList.MainMap.TileEngine.Y.Value))+')    '+
                        ' WH: '+inttostr(MapList.MainMap.TileEngine.Width)+','+inttostr(MapList.MainMap.TileEngine.Height);
end;


end.

