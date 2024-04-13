unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FtexRoadSheet: PTexture;  // the texture for the tile sheet

  FRoad: TTileEngine;  // the tile engine

  FRoadSheet: TSprite; // a sprite to show the whole tile sheet
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property Road: TTileEngine read FRoad;
  property RoadSheet: TSprite read FRoadSheet;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexRoadSheet := FAtlas.Add(path+'RoadTileSheet.png', 64, 64);  // load the tile sheet: tile size is 64x64 pixels

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // tile engine
  FRoad := TTileEngine.Create(FScene);
  FScene.Add(FRoad, 0);
  with FRoad do begin
   LoadMapFile(path+'RoadStage3.map', [FtexRoadSheet]);
   SetViewSize(13*TileSize.cx, FScene.Height);
   SetCoordinate((FScene.Width - Width)*0.5, 0);
   ScrollSpeed.Value := PointF(0, FScene.ScaleDesignToScene(300));  // we want scroll only vertically
  end;

  // the sprite to show the whole tile sheet
  FRoadSheet := TSprite.Create(FtexRoadSheet, False);
  FScene.Add(FRoadSheet, 0);
  FRoadSheet.Frame := 0;   // frame index of 0 is the whole texture
  FRoadSheet.Scale.Value := PointF(8,8);  // we scale the sprite because its frame Width/Height is 64x64 pixels
  FRoadSheet.CenterOnScene;
  FRoadSheet.Visible := False;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;


end.

