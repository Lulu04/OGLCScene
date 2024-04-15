unit u_sprite_def;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  OGLCScene, BGRABitmapTypes;

type

{ TShipRadar }

TShipRadar = class(TSprite)
  constructor Create(aTex: PTexture);
end;

{ TShipArm }

TShipArm = class(TSprite)
private
  FArmPart2, FArmPart3, FPliers: TSprite;
  FBusy, FArmIsRetracted: boolean;
public
  constructor Create(aTexArm, atexPlier: PTexture);
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); override;
  procedure DeployArm;
  procedure RetractArm;
  property Busy: boolean read FBusy;
  property ArmIsRetracted: boolean read FArmIsRetracted;
end;

{ TPanelStatus }

TPanelStatus = class (TUIPanel)
private
  FArmIsHarvesting: boolean;
  FArmIsRetracted: boolean;
  FLabelArmRetractedStatus,
  FLabelArmHarvestingStatus: TFreeText;
  FProgress: TUIProgressBar;
  procedure SetArmIsHarvesting(AValue: boolean);
  procedure SetArmIsRetracted(AValue: boolean);
public
  constructor Create(aFont: TTexturedFont);
  procedure Update(const AElapsedTime: single); override;

  property ArmIsRetracted: boolean read FArmIsRetracted write SetArmIsRetracted;
  property ArmIsHarvesting: boolean read FArmIsHarvesting write SetArmIsHarvesting;
  property Gauge: TUIProgressBar read FProgress;
end;

implementation
uses screen_demo, u_common;

{ TPanelStatus }

procedure TPanelStatus.SetArmIsRetracted(AValue: boolean);
begin
  FArmIsRetracted := AValue;
  if AValue then begin
    FLabelArmRetractedStatus.Caption := 'Arm status: RETRACTED';
    FLabelArmRetractedStatus.Tint.Value := BGRA(200,200,200);
  end else begin
    FLabelArmRetractedStatus.Caption := 'Arm status: DEPLOYED';
    FLabelArmRetractedStatus.Tint.Value := BGRA(255,255,0);
  end;
end;

procedure TPanelStatus.SetArmIsHarvesting(AValue: boolean);
begin
  FArmIsHarvesting := AValue;
  if AValue then begin
    FLabelArmHarvestingStatus.Visible := True;
    FLabelArmHarvestingStatus.Blink(-1, 0.5, 0.5);
  end else begin
    FLabelArmHarvestingStatus.StopBlink;
    FLabelArmHarvestingStatus.Visible := False;
  end;
end;

constructor TPanelStatus.Create(aFont: TTexturedFont);
var marg: integer;
begin
  inherited Create(FScene);
  FScene.Add(Self, LAYER_GUI);
  BodyShape.SetShapeRoundRect(Round(FScene.Width/3), Round(FScene.Height*0.1),
                             FScene.ScaleDesignToScene(8), FScene.ScaleDesignToScene(8), FScene.ScaleDesignToScene(3));
  BodyShape.Fill.Color := BGRA(255,0,255,80);
  BodyShape.Border.Color := BGRA(100,0,100);
  SetCoordinate((FScene.Width-Width)*0.5, FScene.ScaleDesignToScene(5));

  marg := FScene.ScaleDesignToScene(5);

  FLabelArmRetractedStatus := TFreeText.Create(FScene);
  AddChild(FLabelArmRetractedStatus);
  FLabelArmRetractedStatus.TexturedFont := aFont;
  FLabelArmRetractedStatus.SetCoordinate(marg, 0);
  SetArmIsRetracted(True);

  FLabelArmHarvestingStatus := TFreeText.Create(FScene);
  AddChild(FLabelArmHarvestingStatus);
  FLabelArmHarvestingStatus.TexturedFont := aFont;
  FLabelArmHarvestingStatus.Caption := 'HARVESTING';
  FLabelArmHarvestingStatus.Tint.Value := BGRA(100,255,50);
  FLabelArmHarvestingStatus.SetCoordinate(marg, FLabelArmRetractedStatus.Height);
  SetArmIsHarvesting(False);

  FProgress := TUIProgressBar.Create(FScene, uioHorizontal);
  AddChild(FProgress);
  FProgress.BodyShape.SetShapeRectangle(FScene.ScaleDesignToScene(100), FScene.ScaleDesignToScene(15), FScene.ScaleDesignToScene(2));
  FProgress.SetCoordinate(marg, FLabelArmRetractedStatus.Height*2);
end;

procedure TPanelStatus.Update(const AElapsedTime: single);
begin
  inherited Update(AElapsedTime);

  if FArmIsHarvesting then FProgress.Percent := FProgress.Percent + AElapsedTime/5; // 5sec to fill entirely the progress
end;

{ TShipArm }

constructor TShipArm.Create(aTexArm, atexPlier: PTexture);
begin
  // self is the part 1 of the arm
  inherited Create(aTexArm, False);
  Pivot := PointF(0.5, 0.95);

  // part 2 of the arm is child of part 1
  FArmPart2 := TSprite.Create(aTexArm, False);
  AddChild(FArmPart2, 0);
  FArmPart2.X.Value := 0;
  FArmPart2.BottomY := Height*0.1;
  FArmPart2.Pivot := PointF(0.5, 0.95);
  FArmPart2.Angle.Value := 180;

  // part 3 of the arm is child of part 2
  FArmPart3 := TSprite.Create(aTexArm, False);
  FArmPart2.AddChild(FArmPart3, 0);
  FArmPart3.X.Value := 0;
  FArmPart3.BottomY := FArmPart2.Height*0.1;
  FArmPart3.Pivot := PointF(0.5, 0.95);
  FArmPart3.Angle.Value := -180;

  // pliers are child of arm part 3
  FPliers := TSprite.Create(atexPlier, False);
  FArmPart3.AddChild(FPliers, 0);
  FPliers.CenterX := FArmPart3.Width*0.5;
  FPliers.BottomY := FArmPart3.Height*0.1;
  FPliers.Pivot := PointF(0.5, 1.0);

  FArmIsRetracted := True;
end;

procedure TShipArm.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of
    // ANIMATION Deploy arm
    100: begin
      Angle.ChangeTo(45, 4.0, idcSinusoid);
      FArmPart2.Angle.ChangeTo(30, 4.0, idcSinusoid);
      FArmPart3.Angle.ChangeTo(-30, 4.0, idcSinusoid);
      PostMessage(101, 4.0);
    end;
    101: begin
      PostMessage(150); // start the vibrations on the pliers
      PostMessage(160); // start the rotation of the pliers
      FArmIsRetracted := False;
      FBusy := False;
      ScreenDemo.PostMessage(MESS_ARM_DEPLOYED);
      ScreenDemo.PostMessage(MESS_HARVESTING_ON);
    end;

    // ANIMATION pliers vibrations
    150: begin
      FPliers.BottomY := FArmPart3.Height*0.14;
      PostMessage(151, 0.05);
    end;
    151: begin
      FPliers.BottomY := FArmPart3.Height*0.1;
      PostMessage(150, 0.05);
    end;

    // ANIMATION pliers rotation
    160: begin
      FPliers.Angle.ChangeTo(50, 3.0, idcSinusoid);
      PostMessage(161, 3.0);
    end;
    161: begin
      FPliers.Angle.ChangeTo(-50, 3.0, idcSinusoid);
      PostMessage(160, 3.0);
    end;

    // ANIMATION Retract arm
    200: begin
      Angle.ChangeTo(0, 4.0, idcSinusoid);
      FArmPart2.Angle.ChangeTo(180, 4.0, idcSinusoid);
      FArmPart3.Angle.ChangeTo(-180, 4.0, idcSinusoid);
      FPliers.Angle.ChangeTo(0, 4.0, idcSinusoid);
      FPliers.BottomY := FArmPart3.Height*0.1; // ensure the pliers are at their right place
      PostMessage(201, 4.0);
    end;
    201: begin
      FArmIsRetracted := True;
      FBusy := False;
      ScreenDemo.PostMessage(MESS_ARM_RETRACTED); // notify the GUI
    end;

  end;
end;

procedure TShipArm.DeployArm;
begin
  if FBusy then exit;
  FBusy := True;
  PostMessage(100);    // start the animation to deploy the arm
end;

procedure TShipArm.RetractArm;
begin
  if FBusy then exit;
  FBusy := True;
  // by clearing the message list, we ensure that no further steps
  // are executed in the ProcessMessage method => stops the looped animations on the pliers
  ClearMessageList;

  ScreenDemo.PostMessage(MESS_HARVESTING_OFF); // notify the GUI
  PostMessage(200);   // start the animation to retract the arm
end;

{ TShipRadar }

constructor TShipRadar.Create(aTex: PTexture);
var s: string;
begin
  inherited Create(aTex, False);
  Pivot := PointF(0.5, 0.8);
  // to animate the radar, we write a scenario
  s := 'RotateTo 80 3.0 idcsinusoid'#10+            // ask a rotation of 80° in 2.0sec with the velocity curve 'sinusoid'
       'Wait 3.0'#10+                               // wait until the rotation is done
       'RotateTo -80 3.0 idcsinusoid'#10+           // ask a rotation to -80° in 2.0sec with the velocity curve 'sinusoid'
       'Wait 3.0'#10+                               // wait until the rotation is done
       'Loop';                                      // loop to the beginning

  AddAndPlayScenario(s);      // add the scenario to our sprite and play it immediatly
end;


end.

