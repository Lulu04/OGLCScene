unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_proceduralcloud;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FClouds: TOGLCSpriteClouds;
  FCloudRenderer: TOGLCCloudsRenderer;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property Clouds: TOGLCSpriteClouds read FClouds;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics, form_main;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var fd: TFontDescriptor;
  presetName, presetData: string;
  title: TSprite;
  sky: TGradientRectangle;
begin
  // create the sky
  sky := TGradientRectangle.Create(FScene);
  FScene.Add(sky);
  sky.Gradient.CreateVertical([BGRA(0,128,255), BGRA(50,200,200)],
                              [0.0, 1.0]);
  sky.SetSize(FScene.Width, FScene.Height);

  // create the planet renderer
  FCloudRenderer := TOGLCCloudsRenderer.Create(FScene, True);

  // create the planet sprite instance
  FClouds := TOGLCSpriteClouds.Create(FScene, FCloudRenderer);
  FClouds.SetSize(FScene.Width, FScene.Height div 2);
  FScene.Add(FClouds);
  FClouds.CenterOnScene;
  FClouds.FadeRight:=0.3;
  FClouds.FadeBottom:=0.2;
  FClouds.FadeTop:=0.2;

  FormMain.PresetManager.GetPresetByIndex(0, presetName, presetData);
  if presetName <> '' then begin
    FClouds.LoadParamsFromString(presetData);
    FormMain.SetPresetNameOnTileForm(presetName);
  end;

  // show title
  fd.Create('Arial', FScene.ScaleDesignToScene(20), [fsBold], BGRA(255,255,200), BGRA(0,0,0), 1.5);
  title := TSprite.Create(FScene, fd, 'OGLCScene Clouds Designer');
  FScene.Add(title);
  with title do begin
    CenterX := FScene.Center.x;
    Y.Value := FScene.Height*0.2;
    Scale.ChangeTo(PointF(2.0,2.0), 5.0);
    Opacity.ChangeTo(0, 5.0, idcStartSlowEndFast);
    KillDefered(5.0);
  end;
end;

procedure TScreenDemo.FreeObjects;
begin
  // kill all surfaces on all layers
  FScene.ClearAllLayer;

  // free the planet renderer
  FCloudRenderer.Free;
  FCloudRenderer := NIL;
end;

end.

