unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_button_def;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FTexFont: TTexturedFont;
  FtexStar,             // the texture for the star image
  FtexShip: PTexture;   // the texture for the ship

  FShip: TSprite;       // the sprite for the ship

  FButtonPlay, FButtonOptions, FButtonQuit: TTitleMenuButton;
private
  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;

  procedure ProcessGUIEvent(Sender: TSimpleSurfaceWithEffect);
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics, form_main;

type
{ TStar }

TStar = class(TSprite)
  procedure Update(const AElapsedTime: single); override;
end;

procedure TStar.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  // we kill the sprite when it disappear at the bottom of the scene.
  if Y.Value > FScene.Height then Kill;
end;

{ TScreenDemo }

procedure TScreenDemo.ProcessGUIEvent(Sender: TSimpleSurfaceWithEffect);
var text: TFreeText;
begin
  if Sender = FButtonQuit then begin
    FormMain.Close;
    exit;
  end;

  text := TFreeText.Create(FScene);
  text.TexturedFont := FtexFont;
  if Sender = FButtonPlay then text.Caption := 'Sorry no game! this is only a demo for button animation';
  if Sender = FButtonOptions then text.Caption := 'This is a fake game! No options...';
  text.Tint.Value := BGRA(50,200,255);
  FScene.Add(text, LAYER_GUI);
  text.CenterX := FScene.Width*0.5;
  text.CenterY := FScene.Height*0.35;

  text.Scale.ChangeTo(PointF(2,2), 5, idcStartFastEndSlow);
  text.Opacity.ChangeTo(0, 5, idcSinusoid);
  text.KillDefered(5);
end;

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  xx, yy: single;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));
  FtexShip := FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(FScene.Width/8), -1);

  fd.Create('Arial', 30, [fsBold], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL);

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
  FShip := TSprite.Create(FtexShip, False);
  FScene.Add(FShip, LAYER_SHIP);
  FShip.CenterX := FScene.Width*0.5;
  FShip.Y.value := FScene.Height - FShip.Height*1.25;

  // menu buttons creation
  xx := FScene.Width*0.75;
  yy := FScene.Height*0.5;
  // button Play
  FButtonPlay := TTitleMenuButton.Create(xx, yy, 'PLAY', FtexFont, LAYER_GUI);
  yy := yy + FButtonPlay.Height + FScene.ScaleDesignToScene(20);
  // button Options
  FButtonOptions := TTitleMenuButton.Create(xx, yy, 'Options', FtexFont, LAYER_GUI);
  yy := yy + FButtonOptions.Height + FScene.ScaleDesignToScene(20);
  // button Quit
  FButtonQuit := TTitleMenuButton.Create(xx, yy, 'Quit', FtexFont, LAYER_GUI);

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
var star: TStar;
  zoom: single;
  b: byte;
begin
  inherited Update(AElapsedTime);

  // stars creation
  FTimeAccu := FTimeAccu + AElapsedTime;
  if FTimeAccu > 0.01 then begin
    FTimeAccu := 0.0;
    star := TStar.Create(FtexStar, False);  // creation of the sprite. False because texture is owned by the atlas.
    FScene.Add(star, LAYER_STARS);   // add the sprite instance to the scene
    with star do begin
      X.Value := Random(FScene.Width-star.Width);  // set a random horizontal coordinate
      Y.Value := -star.Height;      // the star start from the top of the scene
      zoom := 0.5 + Random*0.8;
      Scale.Value := PointF(zoom, zoom);          // scale the sprite
      Speed.y.Value := (100 + Random*50) * zoom;  // set the vertical speed
      b := Random(192)+64;
      Tint.Value := BGRA(b,b,50);                 // set the color
      Angle.AddConstant(Random(180));             // add a rotation effect
    end;
  end;
end;


end.

