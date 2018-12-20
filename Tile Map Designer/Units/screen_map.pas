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
 FTitleFont := GuiFont( 'Arial', 32, [], BGRA(0,255,100), BGRA(20,100,50), 1, BGRA(0,0,0,0), 0, 0, 0 );
// FHintFont := GuiFont( 'Arial', 12, [], BGRA(255,255,100), BGRA(0,0,0,200), 4, BGRA(0,0,0,0), 0, 0, 0 );
// FEventFont := GuiFont( 'Arial', 9, [fsBold], BGRA(200,200,200), BGRA(200,200,200,0), 0, BGRA(0,0,0,0), 0, 0, 0);//, fqSystemClearType );

 FEventFont:= FontManager.AddFont(GuiFont( 'Arial', 9, [fsBold], BGRA(200,200,200), BGRA(200,200,200,0), 0, BGRA(0,0,0,0), 0, 0, 0));
 FHintFont:= FontManager.AddFont(GuiFont( 'Arial', 12, [], BGRA(255,255,100), BGRA(0,0,0,200), 4, BGRA(0,0,0,0), 0, 0, 0 ));

 FLabelMapPosition:=TFreeText.Create;
 FLabelMapPosition.TexturedFont:=FHintFont;
 FLabelMapPosition.Caption:='Map position:';
 FScene.add( FLabelMapPosition, Layer_Info );

 o := TGUILabel.Create('T I L E    M A P    D E S I G N E R', FTitleFont );
 o.SetCenterCoordinate( FScene.Width/2, FScene.Height/2 );
 FScene.Add( o, Layer_Info );
 o.Opacity.ChangeTo( 0, 5, idcStartSlowEndFast );
 o.Scale.ChangeTo( PointF(1.2,1.2), 6 );
 o.KillDefered( 5 );


// FLabelTileIndexes := TGuiLabel.Create('Tile:', FHintFont);
// FLabelTileIndexes.SetCoordinate( 0, FLabelMapPosition2.BottomY+5 );
// FScene.Add( FLabelTileIndexes, Layer_Info );

 FLabelTileIndexes := TFreeText.Create;
 FLabelTileIndexes.TexturedFont:=FHintFont;
 FLabelTileIndexes.Caption:='Tile:';
 FScene.add( FLabelTileIndexes, Layer_Info );
 FLabelTileIndexes.SetCoordinate( 0, FLabelMapPosition.BottomY+5 );


// FLabelSelectionInfo := TGuiLabel.Create('Sel:', FHintFont);
// FLabelSelectionInfo.SetCoordinate( 0, FLabelTileIndexes2.BottomY+5 );
// FScene.Add( FLabelSelectionInfo, Layer_Info );

 FLabelSelectionInfo := TFreeText.Create;
 FLabelSelectionInfo.TexturedFont:=FHintFont;
 FLabelSelectionInfo.Caption:='Sel:';
 FScene.add( FLabelSelectionInfo, Layer_Info );
 FLabelSelectionInfo.SetCoordinate( 0, FLabelTileIndexes.BottomY+5 );

 FLabelGroundType := TFreeText.Create;
 FLabelGroundType.TexturedFont:=FHintFont;
 FLabelGroundType.Caption := 'Ground:';
 FLabelGroundType.SetCoordinate( 0, FLabelSelectionInfo.BottomY+5 );
 FScene.Add( FLabelGroundType, Layer_Info );


 FLabelEventName := TFreeText.Create;
 FLabelEventName.TexturedFont:=FHintFont;
 FLabelEventName.Caption := 'Event:';
 FLabelEventName.SetCoordinate( 0, FLabelGroundType.BottomY+5 );
 FScene.Add( FLabelEventName, Layer_Info );

 FLabelDebug := TFreeText.Create;
 FLabelDebug.TexturedFont:=FEventFont;
 FLabelDebug.Caption := 'Debug';
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

