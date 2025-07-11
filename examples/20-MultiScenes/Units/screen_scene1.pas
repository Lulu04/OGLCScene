unit screen_scene1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenDemo1 }

TScreenDemo1 = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FtexRoadSheet: PTexture;  // the texture for the tile sheet

  FRoad: TTileEngine;  // the tile engine

  FCamera: TOGLCCamera;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure ProcessMessage(UserValue: TUserMessageValue); override;
end;

var ScreenDemo1: TScreenDemo1 = NIL;

implementation
uses Forms;

{ TScreenDemo1 }

procedure TScreenDemo1.CreateObjects;
var path: string;
  ima: TBGRABitmap;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene1.CreateAtlas;
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
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // tile engine
  FRoad := TTileEngine.Create(FScene1);
  FScene1.Add(FRoad, 0);
  with FRoad do begin
   LoadMapFile(path+'RoadStage3.map', [FtexRoadSheet]);
   SetViewSize(13*TileSize.cx, Round(FScene1.Height*1.5));
   ForceTheDrawingOfAllTheTiles := True;
   CenterOnScene;
   ScrollSpeed.Value := PointF(0, FScene1.ScaleDesignToScene(300));  // we want scroll only vertically
  end;

  FCamera := FScene1.CreateCamera;
  FCamera.AssignToAllLayers;
  FCamera.Angle.AddConstant(5);
  PostMessage(0); // repeat animation camera zoom in and out

  FScene1.BackgroundColor := BGRA(0,80,0);
end;

procedure TScreenDemo1.FreeObjects;
begin
  FScene1.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
  FScene1.KillCamera(FCamera);
end;

procedure TScreenDemo1.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of
    0: begin
      FCamera.Scale.ChangeTo(PointF(0.5, 0.5), 5.0, idcSinusoid);
      PostMessage(10, 5.0);
    end;
    10: begin
      FCamera.Scale.ChangeTo(PointF(1.0, 1.0), 5.0, idcSinusoid);
      PostMessage(0, 5.0);
    end;

  end;
end;


end.

