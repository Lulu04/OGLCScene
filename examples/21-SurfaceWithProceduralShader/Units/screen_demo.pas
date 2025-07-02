unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_MySurfaceWithGradient;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack the font characters in a single texture
  FtexFont: TTexturedFont;
  FTextArea: TUITextArea;

  FGradient: TMySurfaceWithGradient;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var fd: TFontDescriptor;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // we define the font for the text
  fd.Create('Arial', FScene.ScaleDesignToScene(30), [fsBold], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL); // use 2 predefined charsets

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.
  FAtlas.FreeItemImages; // free some memory because we no longer need individual images

  // text area
  FTextArea := FScene.Add_UITextArea(LAYER_UI);
  FTextArea.BodyShape.SetShapeRectangle(FScene.Width, Round(FScene.Height*0.3), 0);
  FTextArea.Text.Tint.Value := BGRA(255,255,255);
  FTextArea.Text.Caption := 'A shader constructs the image in a texture, then we use it as a sprite';
  FTextArea.Text.TexturedFont := FtexFont;
  FTextArea.Text.Align := taCenterCenter;

  FGradient := TMySurfaceWithGradient.Create(FScene, FScene.Height div 4, FScene.Height div 4);
  FScene.Add(FGradient, 0);
  FGradient.CenterX := FScene.Center.x;
  FGradient.BottomY := FScene.Height*0.9;
  FGradient.Speed.Value := PointF(FScene.Width*0.5,FScene.Height*0.5);
  FGradient.RefreshContinuously := True;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layers
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  if (FGradient.RightX >= FScene.Width) or
     (FGradient.X.Value <= 0) then
    FGradient.Speed.x.Value := -FGradient.Speed.x.Value;

  if (FGradient.BottomY >= FScene.Height) or
     (FGradient.Y.Value <= FScene.Height*0.3) then
    FGradient.Speed.y.Value := -FGradient.Speed.y.Value;
end;

end.

