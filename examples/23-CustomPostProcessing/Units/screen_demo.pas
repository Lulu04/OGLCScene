unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_MyPostProcessingRenderer;

type

TFirework = record
  PEngine: TParticleEmitter; // particle emitter configured for fireworks
  BusyTime: single;          // time before next shoot
end;

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to pack all images in a single texture

  FtexJungle: PTexture;
  FSpriteJungle: TSprite;

  FtexFont: TTexturedFont;
  FTextArea: TUITextArea;

  FMyPostProcessingRenderer: TMyPostProcessingRenderer;

public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  yGround: single;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // jungle image from https://www.deviantart.com/fadwaangela/art/Jungle-tranquility-animated-359957676
  FtexJungle := FAtlas.Add(path+'jungle.jpg');

  // we define the font for the text
  fd.Create('Arial', FScene.ScaleDesignToScene(30), [fsBold], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET + ASCII_SYMBOL_CHARSET); // use 2 predefined charsets

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // we define the y coordinate where to trigger the mirror effect (for our custom post processing renderer)
  yGround := FScene.Height - FtexJungle^.TextureHeight*1.2;

  // add the jungle image
  FSpriteJungle := FScene.AddSprite(FtexJungle, False, LAYER_SPRITE);

  // scale the sprite to fit the width of the scene
  FSpriteJungle.Scale.Value := ScaleValueToFitScene(FtexJungle, FScene);
  FSpriteJungle.ScaledX := 0;
  FSpriteJungle.ScaledY := yGround - FSpriteJungle.ScaledHeight+3;

  // text area
  FTextArea := FScene.Add_UITextArea(LAYER_UI);
  FTextArea.BodyShape.SetShapeRectangle(FScene.Width, Round(FSpriteJungle.ScaledY), 0);
  FTextArea.Text.Tint.Value := BGRA(255,255,255);
  FTextArea.Text.Caption := 'Custom post-processing demo'#10#10+
  'Jungle image made by Angela Fadwa'#10+
  'https://www.deviantart.com/fadwaangela/art/Jungle-tranquility-animated-359957676'#10#10+
  'Idea for the shader by LeFauve'#10+
  'https://gamedev.stackexchange.com/questions/90592/how-to-implement-this-kind-of-ripples-with-a-glsl-fragment-shader';
  FTextArea.Text.TexturedFont := FtexFont;
  FTextArea.Text.Align := taCenterCenter;

  // Create our custom post processing renderer and sets its parameters
  FMyPostProcessingRenderer := TMyPostProcessingRenderer.Create(FScene, True);
  FMyPostProcessingRenderer.SetParams(BGRA(0,75,100,90), yGround);

  // enable the pfCustomRenderer effect on LAYER_SPRITE
  FScene.Layer[LAYER_SPRITE].PostProcessing.Enable([ppCustomRenderer]);
  // and ask LAYER_SPRITE to use our custom renderer defined in unit u_MyPostProcessingRenderer.pas
  FScene.Layer[LAYER_SPRITE].PostProcessing.UseCustomRenderer(FMyPostProcessingRenderer);

  // finaly, starts the post processing engine
  FScene.PostProcessing.StartEngine;

end;

procedure TScreenDemo.FreeObjects;
begin
  // stops the post processing engine, and for each layer:
  //   - disable effects previously enabled
  //   - reset effect's parameters to their default values.
  FScene.PostProcessing.StopEngine;

  // destroy our post-processing renderer
  FMyPostProcessingRenderer.Free;
  FMyPostProcessingRenderer := NIL;

  FScene.ClearAllLayer;    // kill all surfaces on all layers
  FreeAndNil(FAtlas);
end;

end.

