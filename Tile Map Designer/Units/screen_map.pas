unit screen_map;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, Graphics,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  VelocityCurve,
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
  procedure Update( AElapsedTime: single ); override;

public
end;

var ScreenMap: TScreenMap = NIL;

implementation
uses Main;

{ TScreenMap }


procedure TScreenMap.LoadData;
var o: TGUILabel;
  FImaGrid : TBGRABitmap;
begin
 FTitleFont := FontManager.AddFont( 'Arial', 32, [], BGRA(0,255,100), BGRA(20,100,50), 1, BGRA(0,0,0,0), 0, 0, 0 );
 FHintFont := FontManager.AddFont( 'Arial', 12, [], BGRA(255,255,100), BGRA(0,0,0,200), 4, BGRA(0,0,0,0), 0, 0, 0 );
 FEventFont := FontManager.AddFont( 'Arial', 9, [fsBold], BGRA(200,200,200), BGRA(200,200,200,0), 0, BGRA(0,0,0,0), 0, 0, 0, fqSystemClearType );

 o := TGUILabel.Create;
 o.Font := FTitleFont;
 o.Caption:='T I L E    M A P    D E S I G N E R';
 o.SetCenterCoordinate( FScene.Width/2, FScene.Height/2 );
 FScene.Add( o, Layer_Info );
 o.Opacity.ChangeTo( 0, 5, idcStartSlowEndFast );
 o.Scale.ChangeTo( PointF(1.2,1.2), 6 );
 o.KillDefered( 5 );

 FLabelMapPosition := TGuiLabel.Create;
 FLabelMapPosition.Font := FHintFont;
 FLabelMapPosition.Caption:='Map position:';
 FLabelMapPosition.SetCoordinate( 0, 0 );
 FScene.Add( FLabelMapPosition, Layer_Info );

 FLabelTileIndexes := TGuiLabel.Create;
 FLabelTileIndexes.Font := FHintFont;
 FLabelTileIndexes.Caption:='Tile:';
 FLabelTileIndexes.SetCoordinate( 0, FLabelMapPosition.BottomY+5 );
 FScene.Add( FLabelTileIndexes, Layer_Info );


 FLabelSelectionInfo := TGuiLabel.Create;
 FLabelSelectionInfo.Font := FHintFont;
 FLabelSelectionInfo.Caption:='Sel:';
 FLabelSelectionInfo.SetCoordinate( 0, FLabelTileIndexes.BottomY+5 );
 FScene.Add( FLabelSelectionInfo, Layer_Info );

 FLabelGroundType := TGuiLabel.Create;
 FLabelGroundType.Font := FHintFont;
 FLabelGroundType.Caption:='Ground:';
 FLabelGroundType.SetCoordinate( 0, FLabelSelectionInfo.BottomY+5 );
 FScene.Add( FLabelGroundType, Layer_Info );

 FLabelEventName := TGuiLabel.Create;
 FLabelEventName.Font := FHintFont;
 FLabelEventName.Caption:='Event:';
 FLabelEventName.SetCoordinate( 0, FLabelGroundType.BottomY+5 );
 FScene.Add( FLabelEventName, Layer_Info );

 FLabelDebug := TGuiLabel.Create;
 FLabelDebug.Font := FEventFont;
 FLabelDebug.Caption:='Debug';
 FLabelDebug.SetCoordinate( 0, FScene.Height-FLabelDebug.Height );
 FScene.Add( FLabelDebug, Layer_Info );

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
 if Form_Principale.CB12.Checked then
 begin
   FBelowOverlayTileEngine.SetCoordinate( FTileEngine.X.Value, FTileEngine.Y.Value );
   FBelowOverlayTileEngine.SetViewSize( FTileEngine.Width, FTileEngine.Height );

   FTopOverlayTileEngine.SetCoordinate( FTileEngine.X.Value, FTileEngine.Y.Value );
   FTopOverlayTileEngine.SetViewSize( FTileEngine.Width, FTileEngine.Height );
 end;

 FLabelDebug.Caption := 'TileEngine: ('+inttostr(round(FTileEngine.X.Value))+','+inttostr(round(FTileEngine.Y.Value))+')    '+
                        ' WH: '+inttostr(FTileEngine.Width)+','+inttostr(FTileEngine.Height);
end;


end.

