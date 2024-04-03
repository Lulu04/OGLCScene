unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenTitle }

TScreenTitle = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FtexHelicopterBody, FtexHelicopterPropeller: PTexture; // the 2 textures for the helicopter

  // our helicopter sprite will be composed by a main sprite for its body and 2 childs for the 2 propellers
  FHelicopterBody,
  FHelicopterBigPropeller,
  FHelicopterSmallPropeller: TSprite;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;
end;

var ScreenTitle: TScreenTitle = NIL;

implementation
uses Forms;

{ TScreenTitle }

procedure TScreenTitle.CreateObjects;
var path: string;
  ima: TBGRABitmap;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // we create the texture from svg file
  FtexHelicopterBody := FAtlas.AddFromSVG(path+'HelicopterBody.svg', -1, -1);
  FtexHelicopterPropeller := FAtlas.AddFromSVG(path+'HelicopterPropeller.svg', -1, -1);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individual textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // creation of the main sprite: helicopter body
  FHelicopterBody := TSprite.Create(FtexHelicopterBody, False); // the owner of the texture is the atlas
  FScene.Add(FHelicopterBody); // add the sprite to the scene
  FHelicopterBody.CenterOnScene; // center the sprite on the scene
  FHelicopterBody.Angle.AddConstant(-10);

  // creation of the big propeller as a child of FHelicopterBody
  FHelicopterBigPropeller := TSprite.Create(FtexHelicopterPropeller, False);
  // this time we don't add the new sprite to the scene, instead it becomes a child of FHelicopterBody
  FHelicopterBody.AddChild(FHelicopterBigPropeller, 0);
  // the coordinate are relative to the parent
  FHelicopterBigPropeller.SetCenterCoordinate(FHelicopterBody.Width*0.55, FHelicopterBody.Height*0.5);
  FHelicopterBigPropeller.Angle.AddConstant(360*3); // we apply a continuous rotation on the propeller

  // creation of the small propeller as a child of FHelicopterBody
  FHelicopterSmallPropeller := TSprite.Create(FtexHelicopterPropeller, False);
  FHelicopterBody.AddChild(FHelicopterSmallPropeller, 0);
  FHelicopterSmallPropeller.SetCenterCoordinate(FHelicopterBody.Width*0.03, FHelicopterBody.Height*0.5);
  FHelicopterSmallPropeller.Scale.Value := PointF(0.15, 0.15); // we want a reduced small propeller
  FHelicopterSmallPropeller.Angle.AddConstant(360*3);
end;

procedure TScreenTitle.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenTitle.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // we apply a zoom effect on the main sprite to animate a little more the scene
  if FHelicopterBody.Scale.State = psNO_CHANGE then begin  // wait the previous change is done
    if FHelicopterBody.Scale.Value = PointF(2,2) then FHelicopterBody.Scale.ChangeTo(PointF(0.5,0.5), 8, idcSinusoid)
      else FHelicopterBody.Scale.ChangeTo(PointF(2,2), 8, idcSinusoid);
  end;
end;


end.

