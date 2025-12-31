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
  procedure CreateObjects; override;
  procedure FreeObjects; override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var spriteText: TSprite;
  fontDescriptor: TFontDescriptor;
begin
  // we define the font
  fontDescriptor.Create('Roboto', 32, [], BGRA(255,255,255));

  // It's the easiest way to create a texture with text written on it.
  // But this means you'll have one texture for one text, which isn't optimized.
  // It's better to use TAtlas and TTexturedFont objects: see example TexturedFont
  spriteText := TSprite.Create(FScene, fontDescriptor, 'Hello world');
  FScene.Add(spriteText);
  spriteText.CenterOnScene;

  // set the background color
  FScene.BackgroundColor := BGRA(30,0,30);
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;
end;


end.

