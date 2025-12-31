unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

TFirework = record
  PEngine: TParticleEmitter; // particle emitter configured for fireworks
  BusyTime: single;          // time before next shoot
end;


{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FtexFont: TTexturedFont;
  FTextArea: TUITextArea;

  FtexStar,         // the texture for the star image
  FtexShip,         // the texture for the ship
  FtexDustParticle, // the texture for the particle emitter for the ship engine
  FtexSphereParticle, // the texture for the fireworks and fire
  FtexCastle,   // the texure for the castle
  FtexGround,
  FtexWood,
  FtexFlameParticle,
  FtexRainDropParticle: PTexture;

private
  FFire: TParticleEmitter;
private
  FArrayFireworks: array[0..7] of TFirework;
  FFireWorkRect: TRect;
  VMargin, HMargin,
  RectWidth, RectHeight: integer;
  function ComputeFireWorkBusyTime: single;
private
  FStarTimeAccu: single;
  FShip: TSprite;       // the sprite for the ship
  FShipPE: TParticleEmitter;  // the paricle emitter for the ship engine
  FShipHGravity: single;
  function GetParticleCount: integer;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;

  property Ship: TSprite read FShip;
  property ShipPE: TParticleEmitter read FShipPE;
  property ParticleCount: integer read GetParticleCount;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

type

{ TStar }

TStar = class(TSprite)
  procedure Update(const AElapsedTime: single); override;
end;

{ TStar }

procedure TStar.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  // we kill the sprite when it disappear at the bottom of the scene.
  if Y.Value > FScene.Height then Kill;
end;

{ TScreenDemo }

function TScreenDemo.ComputeFireWorkBusyTime: single;
begin
  Result := 2 + random(2300)*0.001;
end;

function TScreenDemo.GetParticleCount: integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to High(FArrayFireworks) do
    Result := Result + FArrayFireworks[i].PEngine.ParticlesCount;
  Result := Result + FShipPE.ParticlesCount;
end;

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fontDescriptor: TFontDescriptor;
  i: Integer;
  ground, castle, wood: TSprite;
  sky: TMultiColorRectangle;
  xx: single;
  smoke, rain: TParticleEmitter;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // we define the font for the text
  fontDescriptor.Create('Roboto', 20, [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fontDescriptor, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL); // use 2 predefined charsets

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));
  FtexShip := FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(FScene.Width/16), -1);

  // the following image is used by the particle emitter for the ship engine. We add it to our atlas.
  FtexDustParticle := FAtlas.Add(path+'Dust_particle.png');

  FtexSphereParticle := FAtlas.Add(path+'sphere_particle.png');
  FtexFlameParticle := FAtlas.Add(path+'Flame_particle.png');
  FtexRainDropParticle := FAtlas.Add(path+'RainDrop_Particle.png');

  FtexCastle := FAtlas.AddFromSVG(path+'Castle.svg', Round(FScene.Width*0.6), -1);
  FtexGround := FAtlas.AddFromSVG(path+'Ground.svg', FScene.ScaleDesignToScene(167), -1);
  FtexWood := FAtlas.AddFromSVG(path+'Wood.svg', Round(FScene.Width/10), -1);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // Text
  FTextArea := FScene.Add_UITextArea(LAYER_TOP);
  FTextArea.BodyShape.SetShapeRectangle(FScene.Width, FScene.Height div 10, 0);
  FTextArea.BodyShape.Fill.Visible := False;
  FTextArea.BodyShape.Border.Visible := False;
  FTextArea.Text.Tint.Value := BGRA(255,255,200);
  FTextArea.Text.Caption := 'You can create your own particle emitters with the ParticleEditor tool (in folder tools)'#10;
  FTextArea.Text.TexturedFont := FtexFont;

  // ship engine particle emitter: we create it first so that it is displayed before the ship
  // i.e. below it.
  FShipPE := TParticleEmitter.Create(FScene);
  FScene.Add(FShipPE);
  // Load the particle emitter configuration: the file '.par' are generated by the ParticleEditor tool.
  // Because:
  //         1) the file .par have a reference to an image file for the texture used by the particle emitter
  //         2) this texture is already loaded in our atlas
  // -> we specify the atlas instance where the load method will search the relevant file.
  FShipPE.LoadFromFile(path+'SpaceShipEngine.par', FAtlas);
  FShipPE.ParticlesPosRelativeToEmitterPos := False;

  // ship creation: it will be rendered after the particle emitter for its engine
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_TOP);
  FShip.X.Value := 0;
  FShip.CenterY := FScene.Height*0.5;
  FShipHGravity := 10.0;
  // we bind the position of the particle emitter to the position of the ship
  FShipPE.BindToSprite(FShip, FShip.Width*0.5, FShip.Height);

  // computes boundary where fireworks will appear
  RectWidth := round(FScene.Width*0.75);
  HMargin := round(FScene.Width*0.25);
  RectHeight := round(FScene.Height*0.4);
  VMargin := round(FScene.Height*0.3);

  // blue sky
  sky := TMultiColorRectangle.Create(Round(FScene.Width*0.75), FScene.Height);
  sky.SetTopColors(BGRA(89,31,178));  //13 BGRA(110,142,255)
  sky.SetBottomColors(BGRA(6,15,89)); // BGRA(13,31,178) (BGRA(65,209,99)); //(BGRA(8,242,130));
  sky.SetCoordinate(FScene.Width*0.25, 0);
  FScene.Add(sky, LAYER_BACK);

  // we create the 8 different fireworks
  for i:=0 to High(FArrayFireworks) do begin
   FArrayFireworks[i].PEngine := TParticleEmitter.Create(FScene);
   FArrayFireworks[i].BusyTime := ComputeFireWorkBusyTime;
   FScene.Add(FArrayFireworks[i].PEngine, LAYER_BACK);
   FArrayFireworks[i].PEngine.LoadFromFile(path+'Fireworks0'+inttostr(i+1)+'.par', FAtlas);
  end;

  // ground at the bottom of the screen
  xx := FScene.Width*0.25;
  while xx < FScene.Width do begin
    ground := TSprite.Create(FtexGround, False);
    FScene.Add(ground, LAYER_BACK);
    ground.SetCoordinate(xx, FScene.Height - ground.Height*0.75);
    xx := xx + ground.Width;
  end;

  // castle
  castle := TSprite.Create(FtexCastle, False);
  FScene.Add(castle, LAYER_MIDDLE);
  castle.X.Value := FScene.Width/4 + (FScene.Width*3/4 - castle.Width)*0.5;
  castle.BottomY := ground.Y.Value;
  FFireWorkRect.Create(Round(castle.X.Value), Round(castle.Y.Value), Round(castle.RightX), Round(castle.Y.Value+castle.Height*0.5));

  // wood
  wood := TSprite.Create(FtexWood, False);
  FScene.Add(wood, LAYER_MIDDLE);
  wood.X.Value := FScene.Width/4 + (FScene.Width*3/4-wood.Width)*0.5;
  wood.BottomY := ground.Y.Value + ground.Height*0.25;

  // fire on wood
  FFire := TParticleEmitter.Create(FScene);
  wood.AddChild(FFire, 0);
  FFire.LoadFromFile(path+'FireLine.par', FAtlas);
  FFire.SetCoordinate(wood.Width*0.20, wood.Height*0.60);
  FFire.SetEmitterTypeRectangle(Round(wood.Width*0.60), Round(wood.Height*0.2));
  // smoke
  smoke := TParticleEmitter.Create(FScene);
  wood.AddChild(smoke, 0);
  smoke.LoadFromFile(path+'FireSmoke.par', FAtlas);
  smoke.SetCoordinate(wood.Width*0.40, wood.Height*0.70);
  smoke.SetEmitterTypeRectangle(Round(wood.Width*0.20), Round(wood.Height*0.1));

  // rain
  rain := TParticleEmitter.Create(FScene);
  FScene.Add(rain, LAYER_TOP);
  rain.LoadFromFile(path+'Rain.par', FAtlas);
  rain.SetCoordinate(FScene.Width*0.25, 0);
  rain.SetEmitterTypeRectangle(Round(FScene.Width*0.75), FScene.Height);

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
const MAX_SHIP_GRAVITY = 80;
var star: TStar;
  zoom: single;
  b: byte;
  i: Integer;
  p: TPointF;
begin
  inherited Update(AElapsedTime);

  // stars creation
  FStarTimeAccu := FStarTimeAccu + AElapsedTime;
  if FStarTimeAccu > 0.05 then begin
    FStarTimeAccu := 0.0;
    star := TStar.Create(FtexStar, False);  // creation of the sprite. False because texture is owned by the atlas.
    FScene.Add(star, LAYER_BACK);   // add the sprite instance to the scene
    with star do begin
      X.Value := Random(FScene.Width div 4-star.Width);  // set a random horizontal coordinate
      Y.Value := -star.Height;      // the star start from the top of the scene
      zoom := 0.5 + Random*0.8;
      Scale.Value := PointF(zoom, zoom);          // scale the sprite
      Speed.y.Value := (100 + Random*50) * zoom;  // set the vertical speed
      b := Random(192)+64;
      Tint.Value := BGRA(b,b,50);                 // set the color
      Angle.AddConstant(Random(180));             // add a rotation effect
    end;
  end;

  // ship moves
  FShip.Speed.X.Value := FShipHGravity;
  if FShip.X.Value > FScene.Width/4*5/8 then begin
    if FShipHGravity > -MAX_SHIP_GRAVITY then FShipHGravity := FShipHGravity - 2;
  end else if FShip.X.Value < FScene.width/4*1/8 then begin
    if FShipHGravity < MAX_SHIP_GRAVITY then FShipHGravity := FShipHGravity + 2;
  end;

  //update fireworks
  for i:=0 to High(FArrayFireworks) do
   with FArrayFireworks[i] do begin
     BusyTime := BusyTime - AElapsedTime;
     if BusyTime <= 0 then begin
       BusyTime := ComputeFireWorkBusyTime;
       p := PointF(FFireWorkRect.Left + random(FFireWorkRect.Width), FFireWorkRect.Top + random(FFireWorkRect.Height));
       PEngine.SetCoordinate(p.x, p.y);
       PEngine.Shoot;
     end;
  end;

end;


end.

