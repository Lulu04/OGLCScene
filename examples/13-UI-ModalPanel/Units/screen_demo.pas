unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common, u_sprite_def;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture
  FTexFont: TTexturedFont;
  FLabel: TFreeText;
  FtexStar,             // the texture for the star image
  FtexShip: PTexture;   // the texture for the ship

  FShip: TShip;       // the sprite for the ship
  FInGamePausePanel: TInGamePausePanel; // our 'prepared' modal panel
private
  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, LCLType;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexStar := FAtlas.AddFromSVG(path+'SpaceStar.svg', -1, Round(FScene.Height/100));
  FtexShip := FAtlas.AddFromSVG(path+'SpaceShip.svg', Round(FScene.Width/8), -1);

  fd.Create('Arial', 20, [], BGRA(0,0,0));
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

  // label creation
  FLabel := TFreeText.Create(FScene);
  FScene.Add(FLabel, LAYER_GUI);
  FLabel.TexturedFont := FtexFont;
  FLabel.Caption := 'Press ESCAPE key to show the modal panel';
  FLabel.Tint.Value := BGRA(200,200,200);
  FLabel.SetCoordinate(0, 0);

  // ship creation
  FShip := TShip.Create(FtexShip);

  FInGamePausePanel := TInGamePausePanel.Create(FtexFont);

  // the mouse pointer is hidden
  FScene.Mouse.SystemMouseCursorVisible := False;
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
  if FTimeAccu > 0.05 then begin
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

  // check ESC key
  if FScene.KeyState[VK_ESCAPE] then
    FInGamePausePanel.ShowModal;

end;


end.

