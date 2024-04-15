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
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture
  FtexWarning: PTexture;     // the texture for the warning symbol
  FWarning1, FWarning2: TSprite;
  FLabel: TFreeText;
  FtexFont: TTexturedFont;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const aElapsedTime: single); override;
  procedure ProcessMessage(UserValue: TUserMessageValue); override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  text: TFreeText;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  FtexWarning := FAtlas.AddFromSVG(path+'DlgWarning.svg', FScene.ScaleDesignToScene(200), -1);

  fd.Create('Arial', 20, [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET + ASCII_SYMBOL_CHARSET);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // create the sprites for the demo
  FWarning1 := TSprite.Create(FtexWarning, False);
  FScene.Add(FWarning1);
  with FWarning1 do begin
    SetCoordinate(FScene.Width*0.5-FWarning1.Width*1.5, (FScene.Height-FWarning1.Height)*0.5);
    TintMode := tmMixColor;
    Tint.Value := BGRA(255,0,255,0);
  end;

  FWarning2 := TSprite.Create(FtexWarning, False);
  FScene.Add(FWarning2);
  with FWarning2 do begin
    SetCoordinate(FScene.Width*0.5+FWarning2.Width*0.5, (FScene.Height-FWarning2.Height)*0.5);
    TintMode := tmReplaceColor;
    Tint.Value := BGRA(255,0,255,0);
  end;

  text := TFreeText.Create(FScene);
  text.TexturedFont := FtexFont;
  text.Caption := 'TintMode = tmMixColor';
  text.Tint.Value := BGRA(255,255,0);
  FWarning1.AddChild(text);
  text.Y.Value := FWarning1.Height;
  text.CenterX := FWarning1.Width*0.5;

  text := TFreeText.Create(FScene);
  text.TexturedFont := FtexFont;
  text.Caption := 'TintMode = tmReplaceColor';
  text.Tint.Value := BGRA(255,255,0);
  FWarning2.AddChild(text);
  text.Y.Value := FWarning2.Height;
  text.CenterX := FWarning2.Width*0.5;

  FLabel := TFreeText.Create(Fscene);
  FLabel.TexturedFont := FtexFont;
  FLabel.Caption := 'Tint.Value = (255,0,255)  Alpha = 0';
  FLabel.Tint.Value := BGRA(255,255,255);
  FScene.Add(FLabel);
  FLabel.Y.Value := FWarning1.Y.Value - FLabel.Height*2;
  FLabel.CenterX := FScene.Width*0.5;

  // Start the tint animation
  PostMessage(0);

  //FScene.BackgroundColor := BGRA(50,50,50);
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);

  // update the label
  FLabel.Caption := 'Tint.Value = (255,0,255)  Alpha = '+Trunc(FWarning1.Tint.Alpha.Value).ToString;
end;

procedure TScreenDemo.ProcessMessage(UserValue: TUserMessageValue);
const duration=2.5;
begin
  inherited ProcessMessage(UserValue);
  case UserValue of
    // tint alpha change
    0: begin
      FWarning1.Tint.Alpha.ChangeTo(255, duration);
      FWarning2.Tint.Alpha.ChangeTo(255, duration);
      PostMessage(1, duration);
    end;

    1: begin
      FWarning1.Tint.Alpha.ChangeTo(0, duration);
      FWarning2.Tint.Alpha.ChangeTo(0, duration);
      PostMessage(0, duration);
    end;
  end;
end;


end.

