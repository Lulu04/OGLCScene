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
  FBeam: TOGLCElectricalBeam;
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  property Beam: TOGLCElectricalBeam read FBeam;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms;

{ TScreenDemo }

procedure TScreenDemo.CreateObjects;
begin
  FBeam := TOGLCElectricalBeam.Create(FScene);
  FScene.Add(FBeam);
  with FBeam do begin
    SetCoordinate(FScene.Width*0.5, FScene.Height*0.9); //(20, FScene.Height*0.5);
    SetTargetPoint(PointF(FScene.Width*0.5, 50)); //(PointF(FScene.Width*0.9,FScene.Height*0.5), 20);
    RefreshTime := 0.05;// 0.05;
    Aperture := 5;
    BeamWidth := 3;
    BeamColor.Value := BGRA(255,128,64);
    HaloWidth := 20;
    HaloColor.Value := BGRA(255,0,255,120);
    HaloAttenuation := 2;
    BlendMode := FX_BLEND_ADD;
  end;
end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
end;

end.

