unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes, BGRAPath,
  OGLCScene,
  u_common;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FtexFont: TTexturedFont;
  FtexWarning: PTexture;

  FPathToFollow: TOGLCPathToFollow;   // the container for the path
  FSpriteOnPath: TSpriteOnPathToFollow;    // a sprite specialized to follow a path
  FTextOnPath: TFreeTextOnPathToFollow;  // a text object specialized to follow a path

public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property SpriteOnPath: TSpriteOnPathToFollow read FSpriteOnPath;
  property TextOnPath: TFreeTextOnPathToFollow read FTextOnPath;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path, charset: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  w, h: Integer;
  pathPts: ArrayOfTPointF;

begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // Add the texture for the sprite warning
  FtexWarning := FAtlas.AddFromSVG(path+'DlgWarning.svg', FScene.ScaleDesignToScene(32), -1);

  // we define the font for the text
  fd.Create('Arial', 20, [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET + ASCII_SYMBOL_CHARSET); // use 2 predefined charsets

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // Creation of the path
  w := FScene.Width;
  h := FScene.Height;
  pathPts := ComputeClosedSpline([PointF(w*0.9,h*0.4),PointF(w*0.8,h*0.3),PointF(w*0.4,h*0.8),PointF(w*0.2,h*0.1),PointF(w*0.1,h*0.1),PointF(w*0.1,h*0.4),PointF(w*0.9,h*0.4)],
                              0, 7, ssInside);
  FPathToFollow := TOGLCPathToFollow.Create(FScene);
  FScene.Add(FPathToFollow);
  FPathToFollow.SetCoordinate(0,0);
  FPathToFollow.InitFromPath(pathPts);
  FPathToFollow.Loop := True;          // the sprite/text jump from the end to the begin
  FPathToFollow.Border.Color := BGRA(50,50,50);

  // Creation of the sprite warning: the sprite is created as child of the path
  FSpriteOnPath := TSpriteOnPathToFollow.CreateAsChildOf(FPathToFollow, FtexWarning, False);
  FSpriteOnPath.DistanceTraveled.AddConstant(300);  // the sprite moves on the path by 300 pixels/sec
  FSpriteOnPath.CoeffPositionOnPath := 0;

  // Creation of the text
  FTextOnPath := TFreeTextOnPathToFollow.CreateAsChildOf(FPathToFollow);
  FTextOnPath.TexturedFont := FtexFont;
  FTextOnPath.Caption := 'Love turns man into an ocean of happiness, an image of peace, a temple of wisdom. '+
                         'Love is every man''s very Self, his true beauty, and the glory of his human existence. Swami Muktananda';
  FTextOnPath.Tint.Value := BGRA(255,255,0);
  FTextOnPath.DistanceTraveled.AddConstant(-50);  // the text moves on the path by 50 pixels/sec
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;


end.

