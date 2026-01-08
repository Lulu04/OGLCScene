unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_ProceduralPlanet;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FPlanet: TOGLCSpritePlanet;
  FPlanetRenderer: TOGLCPlanetRenderer;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property Planet: TOGLCSpritePlanet read FPlanet;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics, form_main;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
var fd: TFontDescriptor;
  presetName, presetData: string;
  title: TSprite;
begin
  // create the planet renderer
  FPlanetRenderer := TOGLCPlanetRenderer.Create(FScene, True);

  // create the planet sprite instance
  FPlanet := TOGLCSpritePlanet.Create(FScene, FPlanetRenderer);
  FPlanet.SetSize(FScene.Height div 2, FScene.Height div 2);
  FScene.Add(FPlanet);
  FPlanet.CenterOnScene;

  FormMain.PresetManager.GetPresetByIndex(0, presetName, presetData);
  if presetName <> '' then begin
    FPlanet.LoadParamsFromString(presetData);
    FormMain.SetPresetNameOnTileForm(presetName);
  end;

  // show title
  fd.Create('Arial', FScene.ScaleDesignToScene(20), [fsBold], BGRA(255,255,200), BGRA(0,0,0), 1.5);
  title := TSprite.Create(FScene, fd, 'OGLCScene Planet Designer');
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
  FPlanetRenderer.Free;
  FPlanetRenderer := NIL;
end;

end.

