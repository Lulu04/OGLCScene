unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_sprite_def;

type

TGameState = (gsRunning);

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  // font to draw text
  FTexFont: TTexturedFont;
  // textures used by the game
  FtexStar: PTexture;

  FShip: TPlayerShip;       // the sprite for the ship

  FText: TFreeText;
private
  FGameState: TGameState;
  FStarTimeAccu,
  FMeteorTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;

  property GameState: TGameState read FGameState write FGameState;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, LCLType;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path, s: string;
  ima: TBGRABitmap;
  zoom: single;
  fd: TFontDescriptor;
  i: integer;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  fd.Create('Arial', FScene.ScaleDesignToScene(20), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL);

  path := PathToDataFolder;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));

  // on a 96 PPI screen we choose to have sprites with half of their size in InkScape
  zoom := FScene.ScaleDesignToSceneF(0.5);
  FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(294*zoom), -1);
  FAtlas.AddFromSVG(path+'SpaceShipRadar.svg', Round(50*zoom), -1);
  FAtlas.AddFromSVG(path+'SpaceShipCanon.svg', Round(41*zoom), -1);
  FAtlas.AddFromSVG(path+'SpaceShipLaser.svg', Round(22*zoom), -1);

  FAtlas.AddFromSVG(path+'SpaceMeteor.svg', Round(327*zoom), -1);

  // used by the particle emitter for the ship engine.
  FAtlas.Add(path+'Dust_particle.png');
  // used by the particle emitter for the meteor explosion.
  FAtlas.Add(path+'cloud128x128.png');

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // ship creation
  FShip := TPlayerShip.Create(FAtlas);

  // randomly fill the scene with stars
  for i:=0 to 400 do
    with TStar.Create(FtexStar) do
      Y.Value := Random(FScene.Height-Height);  // set a random vertical coordinate

  s := 'LEFT/RIGHT move the ship'#10+'SHIFT to shoot'#10+'SPACE to show/hide collision box';
  FText := FScene.Add_FreeText(s, FtexFont, LAYER_GUI);
  FText.Tint.Value := BGRA(255,255,0);
  FText.CenterX := FScene.Width*0.5;
  FText.BottomY := FScene.Height;

  GameState := gsRunning;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
var delta: single;
  i: integer;
begin
  inherited Update(AElapsedTime);

  case GameState of

    gsRunning: begin
      // check if player press Right/Left arrow
      delta := FScene.Width*0.2*AElapsedTime;
      if FScene.KeyState[VK_LEFT] then FShip.Move(-delta)
        else if FScene.KeyState[VK_RIGHT] then FShip.Move(delta);

      // check if player press SHIFT
      if FScene.KeyState[VK_SHIFT] then FShip.Shoot;

      // check if player press SPACE
      if FScene.KeyPressed[VK_SPACE] then begin
        FShowCollisionBox := not FShowCollisionBox;
        FShip.CollisionBoxVisible(FShowCollisionBox);
        for i:=0 to FScene.Layer[LAYER_METEOR].SurfaceCount-1 do
          TMeteor(FScene.Layer[LAYER_METEOR].Surface[i]).CollisionBoxVisible(FShowCollisionBox);
      end;

      // check collision between the laser (in LAYER_LASER) and the meteors (in LAYER_METEOR)
      for i:=0 to FScene.Layer[LAYER_LASER].SurfaceCount-1 do
        TLaserBullet(FScene.Layer[LAYER_LASER].Surface[i]).DoCollisionTestWithMeteor;

      // check collision between player ship and meteors
      if FShip.DoCollisionTestWithMeteor then;

      // meteor creation
      FMeteorTimeAccu := FMeteorTimeAccu + AElapsedTime;
      if FMeteorTimeAccu > 0.5 then begin
        FMeteorTimeAccu := 0.0;
        TMeteor.Create(FAtlas);
      end;

      // stars creation
      FStarTimeAccu := FStarTimeAccu + AElapsedTime;
      if FStarTimeAccu > 0.01 then begin
        FStarTimeAccu := 0.0;
        TStar.Create(FtexStar);
      end;
    end;

  end;//case
end;


end.

