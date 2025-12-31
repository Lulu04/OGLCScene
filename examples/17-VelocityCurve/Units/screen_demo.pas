unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes, BGRAPath,
  OGLCScene,
  u_common;

type

{ TSpriteForDemo }

TSpriteForDemo = class(TSpriteContainer)
private
  FVelocityCurve: word;
  FDuration: single;
  FLabel: TFreeText;
  FSprite: TSprite;
public
  constructor Create(aFont: TTexturedFont; aTex: PTexture; aY: single; aVelocityCurveID: word);
  procedure Update(const AElapsedTime: single); override;
  property Duration: single read FDuration write FDuration;
end;

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FtexFont: TTexturedFont;
  FtexWarning: PTexture;

  FSprites: array[0..14] of TSpriteForDemo;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TSpriteForDemo }

constructor TSpriteForDemo.Create(aFont: TTexturedFont; aTex: PTexture; aY: single; aVelocityCurveID: word);
begin
  inherited Create(FScene);
  FScene.Add(Self);
  Y.Value := aY;  // this is the position reference for all child surface
  X.Value := 0;

  // first child is a text
  FLabel := TFreeText.Create(FScene);
  AddChild(FLabel, 0);
  with FLabel do begin
    TexturedFont := aFont;
    Caption := CurveIDToString(aVelocityCurveID);
    Y.Value := 0;
    CenterX := FScene.Width*0.5;
    Tint.Value := BGRA(220,220,220);
  end;

  FVelocityCurve := aVelocityCurveID;
  FDuration := 2.0;

  // second child is a sprite
  FSprite := TSprite.Create(aTex, False);
  AddChild(FSprite, 1); // the sprite is above the label
  FSprite.Y.Value := 0;
  FSprite.X.Value := FScene.Width*0.25;
end;

procedure TSpriteForDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // we start a new move only when the previous is terminated
  if FSprite.X.State = psNO_CHANGE then begin
    if FSprite.X.Value = FScene.Width*0.25 then FSprite.X.ChangeTo(FScene.Width*0.75-FSprite.Width, FDuration, FVelocityCurve)
    else
    if FSprite.X.Value = FScene.Width*0.75-FSprite.Width then FSprite.X.ChangeTo(FScene.Width*0.25, FDuration, FVelocityCurve);
  end;
end;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fontDescriptor: TFontDescriptor;
  yy, ydelta: single;
  i, h: integer;
  coloredSquare: TQuad4Color;
begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // Add the texture for the sprite warning
  h := Round(FScene.Height/(High(FSprites)+1)*0.8);
  FtexWarning := FAtlas.AddFromSVG(path+'DlgWarning.svg', -1, h);

  // we define the font for the text
  fontDescriptor.Create('Roboto', h, [], BGRA(255,255,255));
  FtexFont := FAtlas.AddTexturedFont(fontDescriptor, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL); // use 2 predefined charsets

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // creation of the colored square
  coloredSquare := TQuad4Color.Create(FScene);
  FScene.Add(coloredSquare);
  coloredSquare.SetSize(FScene.Width div 2, FScene.Height);
  coloredSquare.SetAllColorsTo(BGRA(0,100,150));
  coloredSquare.SetCoordinate(FScene.Width*0.25, 0);

  yy := 0;
  ydelta := FScene.Height/(High(FSprites)+1);
  // Creation of the sprites
  for i:=0 to High(FSprites) do begin
    FSprites[i] := TSpriteForDemo.Create(FtexFont, FtexWarning, yy, Word(i));
    yy := yy + ydelta; //fontDescriptor.FontHeight * 1.5;
  end;

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;


end.

