unit screen_Home;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene, VelocityCurve,
  common;

type

{ THomeScreen }

THomeScreen = class( TStageSkeleton )
private
  procedure ProcessSceneBeforePaint;
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( AElapsedTime: single ); override;
end;

var HomeScreen: THomeScreen = NIL;

implementation
uses Main;

{ THomeScreen }

procedure THomeScreen.ProcessSceneBeforePaint;
var p: TPointF;
  polar: TPolarCoor;
  c: TBGRAPixel;
  r: single;
begin
 if (FPEngine<>NIL) and FDrawEmitterShape then begin
   TextureManager.DisableTextureUsage;
   SetBlendMode( FX_BLEND_NORMAL );
   p := FPEngine.GetXY;
   c := BGRA(255,0,0);
   case FPEngine.EmitterType of
     etPoint: begin
       DrawLine( p-PointF(5,5), p+PointF(5,5), c, 1, 1);
       DrawLine( p+PointF(-5,5), p+PointF(5,-5), c, 1, 1);
     end;
     etLine: begin
       polar.Angle:=FPEngine.Direction.Value;
       polar.Distance:=strtofloat(Form_Principale.Edit5.Text);
       DrawLine( p, PolarToCartesian(p, polar), c, 1, 1);
     end;
     etRectangle: DrawBox( FPEngine.X.Value, FPEngine.Y.Value, FPEngine.Width, FPEngine.Height,BGRA(255,0,0),1);
     etCircle, etInnerCircle, etOuterCircle: begin
       r := strtofloat(Form_Principale.Edit7.Text);
       DrawEllipse( p, r, r, c, 1);
     end;
     etRing: begin
       r := strtofloat(Form_Principale.Edit8.Text);
       DrawEllipse( p, r, r, c, 1);
       r := strtofloat(Form_Principale.Edit9.Text);
       DrawEllipse( p, r, r, c, 1);
     end;
   end;
 end;
end;

procedure THomeScreen.LoadData;
var o: TGUILabel;
  cir: TCircle;
begin
 o := TGUILabel.Create('OGLC Particles Engine Editor V1.0',
                       GuiFont('Arial', 32, [], BGRA(255,0,200), BGRA(20,20,20), 3, BGRA(0,0,0,0), 0, 0, 0 ));
 FScene.Add( o, LAYER_PARTICLE );
 o.SetCoordinate(0,FScene.Height/2); //( FScene.Width/2, FScene.Height/2 );
 o.Opacity.ChangeTo(0, 5, 2 );
 o.MoveRelative(10,0, 5 );
 o.Scale.ChangeTo(PointF(1.1,1.1), 5);
 o.KillDefered( 5 );



 FBackGroundColor := TColorBackground.Create(0,0, FScene.Width, FScene.Height);
 FBackGroundColor.SetAllColorsTo( BGRABlack );
 FScene.Add( FBackGroundColor, LAYER_BACKGROUND );

 FBackGroundRainbow := TColorBackground.Create(0,0, FScene.Width, FScene.Height);
 FScene.Add( FBackGroundRainbow, LAYER_BACKGROUND );

 cir := TCircle.Create( PointF(200,200), 100 );
 cir.Tint.Value:=BGRA(255,255,255);
 FScene.Add(cir, LAYER_PARTICLE);
 cir.Radius.ChangeTo(0, 5);

 cir := TCircle.Create( PointF(400,200), 100 );
 cir.Tint.Value:=BGRA(0,0,255);
 FScene.Add(cir, LAYER_PARTICLE);

 FScene.OnAfterPaint:=@ProcessSceneBeforePaint;
end;

procedure THomeScreen.FreeData;
begin
 FScene.ClearAllLayer;
 FScene.OnAfterPaint := NIL;
end;

procedure THomeScreen.Update(AElapsedTime: single);
begin
end;

end.

