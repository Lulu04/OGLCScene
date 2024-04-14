unit u_button_def;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;

type


{ TTitleMenuButton }

TTitleMenuButton = class(TUIButton)
  FSwing: boolean;
  constructor Create(aX, aY: single; const aCaption: string; aFont: TTexturedFont; aLayer: integer);
  procedure Anim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
  procedure Anim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
  procedure Anim_OnMouseDown(Sender: TSimpleSurfaceWithEffect);
  procedure Anim_OnMouseUp(Sender: TSimpleSurfaceWithEffect);
  procedure Anim_OnClick(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessMessage(aValue: TUserMessageValue); override;
end;

implementation
uses screen_title, u_common, BGRABitmapTypes;

{ TTitleMenuButton }

constructor TTitleMenuButton.Create(aX, aY: single; const aCaption: string;
  aFont: TTexturedFont; aLayer: integer);
begin
  inherited Create(FScene, aCaption, aFont, NIL);
  FScene.Add(Self, aLayer);
  _Label.Tint.Value := BGRA(218,20,255);
  Opacity.Value := 0;
  Opacity.ChangeTo(255, 1);
  AutoSize := False;
  BodyShape.SetShapeRoundRect(Round(Fscene.Width/5), Round(aFont.Font.FontHeight*1.5), 10, 10, 3);
  BodyShape.Border.Color := BGRA(64,128,255);
  BodyShape.Fill.Color := BGRA(30,30,130,150);
  SetCenterCoordinate(aX , aY);

  // define the callbacks for animations
  OnAnimMouseEnter := @Anim_OnMouseEnter;
  OnAnimMouseLeave := @Anim_OnMouseLeave;
  OnAnimMouseDown := @Anim_OnMouseDown;
  OnAnimMouseUp := @Anim_OnMouseUp;
  OnAnimClick := @Anim_OnClick;

  // define the callback when user click on the button
  OnClick := @ScreenDemo.ProcessGUIEvent;
end;

procedure TTitleMenuButton.Anim_OnMouseEnter(Sender: TSimpleSurfaceWithEffect);
begin
  BodyShape.Border.Width := 5;
  BodyShape.Border.Color := BGRA(255,255,255);
  if not FSwing then PostMessage(0);
end;

procedure TTitleMenuButton.Anim_OnMouseLeave(Sender: TSimpleSurfaceWithEffect);
begin
  BodyShape.Border.Width := 3;
  BodyShape.Border.Color := BGRA(64,128,255);
end;

procedure TTitleMenuButton.Anim_OnMouseDown(Sender: TSimpleSurfaceWithEffect);
begin
  Tint.Value := BGRA(0,0,0,180);
end;

procedure TTitleMenuButton.Anim_OnMouseUp(Sender: TSimpleSurfaceWithEffect);
begin
  Tint.Value := BGRA(0,0,0,0);
end;

procedure TTitleMenuButton.Anim_OnClick(Sender: TSimpleSurfaceWithEffect);
begin
 // Tint.Value := BGRA(255,255,255);
end;

procedure TTitleMenuButton.ProcessMessage(aValue: TUserMessageValue);
begin
  case aValue of
    0: begin
      FSwing := True;
      Angle.ChangeTo(5, 0.5, idcSinusoid);
      if MouseIsOver then PostMessage(1, 0.5)
        else PostMessage(2);
    end;
    1: begin
      Angle.ChangeTo(-5, 0.5, idcSinusoid);
      if MouseIsOver then PostMessage(0, 0.5)
        else PostMessage(2);
    end;
    2: begin  // end
      Angle.ChangeTo(0, 0.5, idcSinusoid);
      FSwing := False;
    end;
  end;
end;

end.

