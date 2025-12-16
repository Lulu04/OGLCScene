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

  FtexFontTitle,
  FtexFont: TTexturedFont;

  FTitle: TFreeText;
  FText: TFreeText;
  FAlignedText: TFreeTextAligned;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure SetMultilineTextAlignProperty(aValue: integer);
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics;

const
  DEMO_TITLE = 'THE TITLE FOR THIS DEMO';

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path, charset: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> speed optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // we define a big colored font for the title, with outline and shadow
  fd.Create('Arial Black', 80, [], BGRA(255,60,97,200), BGRA(255,255,150), 8, BGRA(0,255,0,255), 20, 20, 15);
  // before creating the font, we have to define which character to use: the charset.
  // because in our title there is several instance of 'T', space, 'E', 'H' and 'I' we use the
  // function AddToCharset() to eliminate the redundant characters.
  charset := '';
  charset := AddToCharset(charset, DEMO_TITLE); // now we are sure we won't have a redundant character in our charset.
  FtexFontTitle := FAtlas.AddTexturedFont(fd, charset); // we can create our textured font.


  // we define another font for the text
  fd.Create('Arial', FScene.ScaleDesignToScene(20), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, FScene.Charsets.SIMPLELATIN + FScene.Charsets.ASCII_SYMBOL); // use 2 predefined charsets

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  // now, the textures are ready, we can create the text objects
  FTitle := TFreeText.Create(FScene);
  FScene.Add(FTitle);
  with FTitle do begin
    TexturedFont := FtexFontTitle;
    Caption := DEMO_TITLE;
    CenterX := FScene.Width*0.5;
    CenterY := FScene.Height / 3;
  end;

  FText := TFreeText.Create(FScene);
  FScene.Add(FText);
  with FText do begin
    TexturedFont := FtexFont;
    Caption := 'TFreeText is a surface that draw a string. It use a TTexturedFont.'#10+
       'With ''#10'' we can jump to the next line';
    Tint.Value := BGRA(255,255,0);
    Y.Value := FTitle.BottomY;  // equivalent to  FText.Y.Value := FTitle.Y.Value + FTitle.Height;
    CenterX := FScene.Center.x;
  end;

  FAlignedText := TFreeTextAligned.Create(FScene, FtexFont, Round(FScene.Width*0.5), Round(FScene.Height/3));
  FScene.Add(FAlignedText);
  with FAlignedText do begin
    Caption := 'TMultilineText is a surface that draw a text in a rectangular area with horizontal and vertical align. '+
               'It use a TTexturedFont.'#10+
               'With ''#10'' we can jump to the next line.'#10#10+
               'The size of the area where this text is written is '+FAlignedText.Width.ToString+'x'+FAlignedText.Height.ToString+#10+
               'Try to change the Align property to center the text differently.';
    Tint.Value := BGRA(255,128,64);
    Y.Value := FText.BottomY + fd.FontHeight;
    CenterX := FScene.Center.x;
    Align := taTopCenter;
  end;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.SetMultilineTextAlignProperty(aValue: integer);
begin
  FAlignedText.Align := TOGLCAlignment(aValue);
end;


end.

