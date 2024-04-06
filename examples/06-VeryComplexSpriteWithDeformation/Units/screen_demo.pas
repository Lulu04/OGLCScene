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
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FLittleRed: TLRFrontView;
  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;

  property LittleRed: TLRFrontView read FLittleRed;
  procedure SetWindSpeed(aValue: single);
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var ima: TBGRABitmap;
   path: string;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  AdditionnalScale := 2.0;
  LoadLRFaceTextures(FAtlas);
  LoadLRFrontViewTextures(FAtlas);
  AdditionnalScale := 1.0;

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individual textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // Character creation
  FLittleRed := TLRFrontView.Create;
  FScene.Add(FLittleRed);
  FLittleRed.CenterOnScene;

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
end;

procedure TScreenDemo.SetWindSpeed(aValue: single);
begin
  FLittleRed.SetWindSpeed(aValue);
end;


end.

