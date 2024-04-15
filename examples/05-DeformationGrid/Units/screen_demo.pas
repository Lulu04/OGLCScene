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

  FtexTitle: PTexture; // the texture that contains the title
  FTitle1,
  FTitle2,
  FTitle3,
  FTitle4: TDeformationGrid;

  FTimeAccu: single;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const AElapsedTime: single); override;

  procedure ShowGrid(aValue: boolean);
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
  yy, margin: single;
begin
  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  fd.Create('Arial Black', Round(FScene.Height/10), [], BGRA(255,60,97,200), BGRA(255,255,150), 8, BGRA(0,255,0,255), 20, 20, 15);
  FtexTitle := FAtlas.AddString('dtTumultuousWater', fd, NIL);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individual textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;
  // free some memory because we no longer need individual images
  FAtlas.FreeItemImages;

  margin := FScene.Height/20;
  yy := margin;
  // the first title use the texture previously defined in the atlas
  FTitle1 := TDeformationGrid.Create(FtexTitle, False);
  FScene.Add(FTitle1);
  with FTitle1 do begin
    SetCoordinate((FScene.Width-FTitle1.Width)*0.5, yy);
    SetGrid(20,20);
    ApplyDeformation(dtTumultuousWater);
    DeformationSpeed.Value := PointF(1.5,1.6);
    Amplitude.Value := PointF(0.2,0.3);
  end;

  yy := yy + FTitle1.BottomY;
  // the second title creates its own private texture (just to demonstrate its possible to not use an atlas (not recommended))
  FTitle2 := TDeformationGrid.Create(FScene, fd, 'dtWindingLeft');
  FScene.Add(FTitle2);
  with FTitle2 do begin
    SetCoordinate((FScene.Width-FTitle1.Width)*0.5, yy);
    SetGrid(1,20);
    ApplyDeformation(dtWindingLeft);
    DeformationSpeed.Value := PointF(FScene.Width*0.1,0);
  end;

  yy := yy + FTitle1.BottomY;
  FTitle3 := TDeformationGrid.Create(FScene, fd, 'dtWindingRight');
  FScene.Add(FTitle3);
  with FTitle3 do begin
    SetCoordinate((FScene.Width-FTitle1.Width)*0.5, yy);
    SetGrid(1,20);
    ApplyDeformation(dtWindingRight);
    DeformationSpeed.Value := PointF(FScene.Width*0.1,0);
  end;

  yy := yy + FTitle1.BottomY;
  FTitle4 := TDeformationGrid.Create(FScene, fd, 'dtFlagRight');
  FScene.Add(FTitle4);
  with FTitle4 do begin
    SetCoordinate((FScene.Width-FTitle1.Width)*0.5, yy);
    SetGrid(20,20);
    ApplyDeformation(dtFlagRight);
    DeformationSpeed.Value := PointF(2,2.6);
    Amplitude.Value := PointF(2,1.3);
  end;

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  FTimeAccu := FTimeAccu + AElapsedTime;
  if FTimeAccu > 5.0 then begin
    FTimeAccu := 0;
    FTitle2.DeformationSpeed.x.Value := -FTitle2.DeformationSpeed.x.Value;
    FTitle3.DeformationSpeed.x.Value := -FTitle3.DeformationSpeed.x.Value;
  end;
end;

procedure TScreenDemo.ShowGrid(aValue: boolean);
begin
  FTitle1.ShowGrid := aValue;
  FTitle2.ShowGrid := aValue;
  FTitle3.ShowGrid := aValue;
  FTitle4.ShowGrid := aValue;
end;


end.

