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
  FtexStar,             // the texture for the star image
  FtexShip: PTexture;   // the texture for the ship

  FShip: TSprite;       // the sprite for the ship
private
  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

type

{ TStar }

TStar = class(TSprite)
  constructor Create(aTex: PTexture);
  procedure Update(const AElapsedTime: single); override;
end;

{ TStar }

constructor TStar.Create(aTex: PTexture);
var zoom: single;
    b: byte;
begin
  inherited Create(aTex, False);  // creation of the sprite. False because texture is owned by the atlas.
  FScene.Add(self, LAYER_STARS);   // add the sprite instance to the scene
  X.Value := Random(FScene.Width-Width);  // set a random horizontal coordinate
  Y.Value := -Height;      // the star start from the top of the scene
  zoom := 0.5 + Random*0.8;
  Scale.Value := PointF(zoom, zoom);          // scale the sprite
  Speed.y.Value := (100 + Random*50) * zoom;  // set the vertical speed
  b := Random(192)+64;
  Tint.Value := BGRA(b,b,50);                 // set the color
  Angle.AddConstant(Random(360)-180);         // add a rotation effect
end;

procedure TStar.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  // we kill the sprite when it disappear at the bottom of the scene.
  if Y.Value > FScene.Height then Kill;
end;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  i: integer;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));
  FtexShip := FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(FScene.Width/6), -1);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // create a ship on center
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_SHIP);
  FShip.SetCenterCoordinate(FScene.Center.x, FScene.Center.y-FShip.Height);

  // create a ship on the left
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_SHIP);
  FShip.SetCoordinate(FScene.Width*0.25-FShip.Width*0.5, FScene.Height*0.55);

  // create a ship on the right
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_SHIP);
  FShip.SetCoordinate(FScene.Width*0.75-FShip.Width*0.5, FScene.Height*0.55);

  // populate the screen with some random stars
  for i:=0 to 500 do
    with TStar.Create(FtexStar) do
      Y.Value := Random*FScene.Height;

  // reserve memory for post processing engine
  FScene.PostProcessing.CreateTargetTexture;

  // we set the ShockWave effect parameters for the stars layer
  FScene.PostProcessing.SetShockWaveParamsOnLayers(0.5, 0.5, 0.3, BGRA(255,0,255,51), [LAYER_STARS]);
  // or
  // FScene.Layer[LAYER_STARS].PostProcessing.SetShockWaveParams(0.5, 0.5, 0.3, BGRA(255,0,255,51));
  // this is the same! (because in this case only 1 layer is targeted)
end;

procedure TScreenDemo.FreeObjects;
begin
  // free memory for post processing engine
  FScene.PostProcessing.FreeTargetTexture;

  // remove all post-processing effects on all layers
  FScene.PostProcessing.DisableAllFXOnAllLayers;

  FScene.ClearAllLayer;    // kill all surfaces on all layers
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // stars creation
  FTimeAccu := FTimeAccu + AElapsedTime;
  if FTimeAccu > 0.01 then begin
    FTimeAccu := 0.0;
    TStar.Create(FtexStar);  // creation of one star
  end;
end;


end.

