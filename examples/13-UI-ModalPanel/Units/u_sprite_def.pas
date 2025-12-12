unit u_sprite_def;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene;

type

{ TStar }

TStar = class(TSprite)
  procedure Update(const AElapsedTime: single); override;
end;


{ TShip }

TShip = class(TSprite)
  constructor Create(aTex: PTexture);
  procedure Update(const AElapsedTime: single); override;
end;

{ TInGamePausePanel }

TInGamePausePanel = class(TUIModalPanel)
private
  BResumeGame, BExitGame: TUIButton;
  procedure FormatButton(aButton: TUIButton);
  procedure ProcessButtonClick(Sender: TSimpleSurfaceWithEffect);
public
  constructor Create(aFont: TTexturedFont);
  procedure ShowModal; override;
  procedure Hide(aFree: boolean); override;
end;

function PPIScale(aValue: integer): integer;

implementation
uses u_common, LCLType, BGRABitmapTypes, form_main;

function PPIScale(aValue: integer): integer;
begin
  Result := FScene.ScaleDesignToScene(aValue);
end;

{ TInGamePausePanel }

procedure TInGamePausePanel.FormatButton(aButton: TUIButton);
begin
  AddChild(aButton);
  aButton.OnClick := @ProcessButtonClick;
  aButton._Label.Tint.Value := BGRA(255,255,150);
  aButton.BodyShape.SetShapeRoundRect(20, 20, PPIScale(8), PPIScale(8), PPIScale(2));
  aButton.BodyShape.Fill.Color := BGRA(20,20,100);
  aButton.BodyShape.Fill.CenterColor := BGRA(30,30,30);
end;

procedure TInGamePausePanel.ProcessButtonClick(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = BResumeGame then begin
    Hide(False);
  end else
  if Sender = BExitGame then begin
    Hide(False);
    FormMain.Close;
  end;
end;

constructor TInGamePausePanel.Create(aFont: TTexturedFont);
var VMargin, maxWidth: integer;
  title: TUILabel;
begin
  inherited Create(FScene);
  BodyShape.SetShapeRoundRect(20, 20, PPIScale(8), PPIScale(8), PPIScale(2));

  VMargin := aFont.Font.FontHeight div 2;
  maxWidth := 0;

  BResumeGame := TUIButton.Create(FScene, 'RESUME GAME', aFont, NIL);
  FormatButton(BResumeGame);
  if maxWidth < BResumeGame.Width then maxWidth := BResumeGame.Width;
  BResumeGame.AnchorPosToParent(haCenter, haCenter, 0, vaCenter, vaCenter, 0);

  title := TUILabel.Create(FScene);
  title.InitParams('game paused', aFont);
  AddChild(title);
  title.Tint.Value := BGRA(200,200,255);
  title.Blink(-1, 0.5, 0.5);
  title.AnchorPosToSurface(BResumeGame, haCenter, haCenter, 0, vaBottom, vaTop, -VMargin);

  BExitGame := TUIButton.Create(FScene, 'EXIT GAME', aFont, NIL);
  FormatButton(BExitGame);
  BExitGame.AnchorPosToSurface(BResumeGame, haCenter, haCenter, 0, vaTop, vaBottom, VMargin);
  if maxWidth < BExitGame.Width then maxWidth := BExitGame.Width;

  // resize the shape of the modal panel according to the maxWidth and the number of 'lines'
  BodyShape.ResizeCurrentShape(Round(maxWidth*1.5), VMargin*4+aFont.Font.FontHeight*4, True);
  CenterOnScene;
end;

procedure TInGamePausePanel.ShowModal;
begin
  // the mouse pointer is visible
  FScene.Mouse.SystemMouseCursorVisible := True;
  inherited ShowModal;
end;

procedure TInGamePausePanel.Hide(aFree: boolean);
begin
  // the mouse pointer is hidden
  FScene.Mouse.SystemMouseCursorVisible := False;
  inherited Hide(aFree);
end;

{ TShip }

constructor TShip.Create(aTex: PTexture);
begin
  inherited Create(aTex, False);    // create the sprite instance
  FScene.Add(Self, LAYER_SHIP);     // add it to the scene in the wanted layer
  CenterX := FScene.Width*0.5;      // center it on the scene
  Y.Value := FScene.Height*1.1;     // its Y coordinate is greater than the scene height: the ship is not visible
  MoveYCenterTo(FScene.Height*2/3, 1, idcStartFastEndSlow);   // its Y coordinate moves to the 2/3*height of the scene
                                                              // in one seconds
end;

procedure TShip.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  // check Left and Right arrow key to move the ship
  if FScene.KeyState[VK_Left] and (X.Value > 0) then begin
    X.Value := X.Value - 200*AElapsedTime;
  end else
  if FScene.KeyState[VK_Right] and (RightX < FScene.Width) then begin
    X.Value := X.Value + 200*AElapsedTime;
  end;
end;

{ TStar }

procedure TStar.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);
  // we kill the sprite when it disappear at the bottom of the scene.
  if Y.Value > FScene.Height then Kill;
end;

end.

