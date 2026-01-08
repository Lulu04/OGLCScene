unit screen_demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common,
  u_procedural_glynnjulia;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FGlynnJulia: TGlynnJulia;
  FGlynnJuliaRenderer: TGlynnJuliaRenderer;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  property GlynnJuliaRenderer: TGlynnJuliaRenderer read FGlynnJuliaRenderer;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
begin

  // create the renderer
  FGlynnJuliaRenderer := TGlynnJuliaRenderer.Create(FScene, True);

  // create the sprite instance
  FGlynnJulia := TGlynnJulia.Create(FScene, FGlynnJuliaRenderer);
  FGlynnJulia.SetSize(FScene.Width, FScene.Height);
  FScene.Add(FGlynnJulia);
  FGlynnJulia.CenterOnScene;

end;

procedure TScreenDemo.FreeObjects;
begin
  // kill all surfaces on all layers
  FScene.ClearAllLayer;

  // free the custom renderer
  FGlynnJuliaRenderer.Free;
  FGlynnJuliaRenderer := NIL;
end;

end.

