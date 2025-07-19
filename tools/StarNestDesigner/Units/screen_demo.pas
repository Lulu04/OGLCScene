unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_procedural_starnest;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FStarNestRenderer: TStarNestRenderer;
  FStars: TStarNest;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property Stars: TStarNest read FStars;
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
  // create the renderer
  FStarNestRenderer := TStarNestRenderer.Create(FScene, True);

  // create the stars instance
  FStars := TStarNest.Create(FScene, FStarNestRenderer);
  FScene.Add(FStars);
  FStars.SetSize(FScene.Width, FScene.Height);
  FStars.CenterOnScene;
  FStars.FlipV := True;

  FormMain.PresetManager.GetPresetByIndex(0, presetName, presetData);
  if presetName <> '' then begin
    FStars.LoadParamsFromString(presetData);
    FormMain.SetPresetNameOnTileForm(presetName);
  end;

  // show title
  fd.Create('Arial', FScene.ScaleDesignToScene(20), [fsBold], BGRA(255,255,200), BGRA(0,0,0), 1.5);
  title := TSprite.Create(FScene, fd, 'OGLCScene - StarNest Designer');
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

  // free the renderer
  FStarNestRenderer.Free;
  FStarNestRenderer := NIL;
end;

end.

