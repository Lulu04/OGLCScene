unit screen_Home;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  common;

type

{ TParticleEmitterWithEmitterShape }

TParticleEmitterWithEmitterShape = class(TParticleEmitter)
  DrawEmitterShape: boolean;
  constructor Create;
  procedure Draw(const aLayerPercentOpacity: single); override;
end;

{ THomeScreen }

THomeScreen = class(TScreenTemplate)
public
  FCamera: TOGLCCamera;
  procedure CreateObjects; override;
  procedure FreeObjects; override;

  procedure ApplyViewOffset(const aOffset: TPoint);
  procedure ApplyViewZoom(aZoom: single);
end;

var HomeScreen: THomeScreen = NIL;
    PE: TParticleEmitterWithEmitterShape;

implementation
uses Main;

{ TParticleEmitterWithEmitterShape }

constructor TParticleEmitterWithEmitterShape.Create;
begin
  inherited Create(FScene);
  FScene.Add(Self, LAYER_PARTICLE);
  DrawEmitterShape := True;
end;

procedure TParticleEmitterWithEmitterShape.Draw(const aLayerPercentOpacity: single);
var p: TPointF;
  polar: TPolarCoor;
  c: TBGRAPixel;
  r: single;
begin
  inherited Draw(aLayerPercentOpacity);
  PushAndPrepareSceneMV;

  if DrawEmitterShape then begin
    FScene.TexMan.UnbindTexture;
    FScene.BlendMode := FX_BLEND_NORMAL;
    p := PointF(0,0);
    c := BGRA(255,0,0);
    case EmitterType of
      etPoint: begin
        DrawLine(FScene, p-PointF(5,5), p+PointF(5,5), c, 2);
        DrawLine(FScene, p+PointF(-5,5), p+PointF(5,-5), c, 2);
      end;
      etLine: begin
        polar.Angle := Direction.Value;
        polar.Distance := EmitterParam.LineSize; // StringToSingle(Form_Principale.Edit5.Text);
        DrawLine(FScene, p, PolarToCartesian(PointF(0,0), polar), c, 2);
      end;
      etRectangle: DrawBox(FScene, 0, 0, Width, Height, BGRA(255,0,0), 2);
      etCircle, etInnerCircle, etOuterCircle: begin
        r := StringToSingle(Form_Principale.Edit7.Text);
        DrawEllipse(FScene, 0, 0, r, r, c, 2);
      end;
      etRing: begin
        r := StringToSingle(Form_Principale.Edit8.Text);
        DrawEllipse(FScene, 0, 0, r, r, c, 2);
        r := StringToSingle(Form_Principale.Edit9.Text);
        DrawEllipse(FScene, 0, 0, r, r, c, 2);
      end;
    end;
  end;

  PopSceneMV;
end;

{ THomeScreen }

procedure THomeScreen.CreateObjects;
var o: TSprite;
  fd: TFontDescriptor;
begin
 fd.Create('Arial', 32, [], BGRA(255,0,200), BGRA(20,20,20), 3);
 o := TSprite.Create(FScene, fd, 'OGLC Particles Engine Editor V1.0');
 FScene.Add(o, LAYER_PARTICLE);
 o.SetCenterCoordinate(FScene.Width*0.5, FScene.Height *0.75);
 o.Opacity.ChangeTo(0, 5, idcStartSlowEndFast);
 o.Scale.ChangeTo(PointF(1.1,1.1), 5);
 o.KillDefered( 5 );

 PE := TParticleEmitterWithEmitterShape.Create;
 Form_Principale.LoadExample;

 FBackGroundColor := TMultiColorRectangle.Create(FScene.Width, FScene.Height);
 FBackGroundColor.SetAllColorsTo(BGRA(80,80,80));
 FScene.Add(FBackGroundColor, LAYER_BACKGROUND);

 FBackGroundRainbow := TMultiColorRectangle.Create(FScene.Width, FScene.Height);
 FScene.Add(FBackGroundRainbow, LAYER_BACKGROUND);

 FCamera := FScene.CreateCamera;
 FCamera.AssignToLayer([LAYER_PARTICLE, LAYER_BACKGROUND]);
end;

procedure THomeScreen.FreeObjects;
begin
 FScene.ClearAllLayer;
 FScene.OnAfterPaint := NIL;
end;

procedure THomeScreen.ApplyViewOffset(const aOffset: TPoint);
begin
  FCamera.MoveTo(FScene.Center + PointF(aOffset));
end;

procedure THomeScreen.ApplyViewZoom(aZoom: single);
begin
  FCamera.Scale.Value := PointF(aZoom, aZoom);
end;

end.

