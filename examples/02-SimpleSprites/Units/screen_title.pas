unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenTitle }

TScreenTitle = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture
  FtexTruck: PTexture;   // the texture for the truck
private
  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
end;

var ScreenTitle: TScreenTitle = NIL;

implementation
uses Forms;

{ TScreenTitle }

procedure TScreenTitle.CreateObjects;
var path: string;
  ima: TBGRABitmap;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexTruck := FAtlas.Add(path+'Truck.png', 96, 253); // image Truck.png is composed of 4 frames

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  FScene.BackgroundColor := BGRA(50,50,50);
end;

procedure TScreenTitle.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenTitle.Update(const AElapsedTime: single);
var truck: TSprite;
begin
  inherited Update(AElapsedTime);

  // trucks creation
  FTimeAccu := FTimeAccu + AElapsedTime;
  if FTimeAccu > 1.0 then begin
    FTimeAccu := 0.0;
    truck := TSprite.Create(FtexTruck, False);  // creation of the sprite. False because texture is owned by the atlas.
    FScene.Add(truck);   // add the sprite instance to the scene
    with truck do begin
      SetFrameLoopBounds(1, 4); // truck texture frames start from 1 to 4.
      FrameAddPerSecond(10);    // in one second, we want the frame index to be incremented 10 times
      X.Value := Random(FScene.Width-truck.Width);  // set a random horizontal coordinate
      Y.Value := FScene.Height;      // the truck start from the bottom of the scene
      Y.ChangeTo(-truck.Height, 7);  // we ask the truck Y coordinate to change to the top of the scene in 7 seconds
      KillDefered(7);   // the truck instance will be killed (freed) from the scene after 7 seconds
    end;
  end;
end;


end.

