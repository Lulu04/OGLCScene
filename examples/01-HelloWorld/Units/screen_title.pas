unit screen_title;

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
  fd: TFontDescriptor;
begin
  // font is drawn in black: this way we can choose the color of the text at sprite level (see below)
  fd.Create('Arial', 32, [], BGRA(0,0,0));

  // It's the easiest way to create a texture with text written on it.
  // But this means you'll have one texture for one text, which isn't optimized for OpenGL.
  // It's better to use a TTexturedFont object: see example 07
  spriteText := TSprite.Create(FScene, fd, 'Hello world');
  FScene.Add(spriteText);
  spriteText.CenterOnScene;
  // we apply a Tint on the sprite because the font is draw in black
  spriteText.Tint.Value := BGRA(255,255,220);
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;
end;


end.

