{$ifdef oglcINTERFACE}
{
  TUIPanelWithEffects is an extended TUIPanel.
    - the scene behind the panel can be darkened
    - can execute a scenario to perform an animation when it is shown
    - can execute a scenario to perform an animation when it is hidden
}
type

{ TUIPanelWithEffects }

TUIPanelWithEffects = class(TUIPanel)
private
  FDarkenTheScene: boolean;
  FSceneDarkness: TQuad4Color;
  FFreePanelWhenEndAnimIsDone: boolean;
  FCurrentScenarioID: TIDScenario;
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure ProcessMessage(UserValue: TUserMessageValue); override;
public
  constructor Create(aParentScene: TOGLCScene); reintroduce;
  destructor Destroy; override;
  // Call this method if you want to darken the scene behind the panel when it is displayed.
  // aColor.alpha controls the opacity of the color.
  procedure ActivateSceneDarken(aColor: TBGRAPixel);

  // Show the panel and execute the scenario passed in parameters.
  // Player can interact with the mouse on the panel only when the scenario terminate.
  // To show the panel immediatly without animation, sets aAnimScenario to ''
  procedure Show(const aAnimScenario: string); virtual;

  // Disable the mouse interaction with the panel, execute the scenario passed in parameter, then hide the panel.
  // The panel is hidden only when the scenario terminate.
  // To hide the panel immediatly without animation, sets aAnimScenario to ''
  // The panel is freed if aFree is True.
  procedure Hide(const aAnimScenario: string; aFree: boolean); virtual;

end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TUIPanelWithEffects.Draw(const aLayerPercentOpacity: single);
begin
  // if needed draw the colored rectangle
  if FDarkenTheScene then begin
    FParentScene.ModelViewMatrix.LoadIdentity;
    FSceneDarkness.Draw(aLayerPercentOpacity);
  end;

  inherited Draw(aLayerPercentOpacity);
end;

procedure TUIPanelWithEffects.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of
    100000: begin
      if ScenarioIsPlaying(FCurrentScenarioID) then PostMessage(100000)
        else begin
          DeleteScenario(FCurrentScenarioID);
          MouseInteractionEnabled := True;
        end;
    end;

    100001: begin
      if ScenarioIsPlaying(FCurrentScenarioID) then PostMessage(100001)
        else begin
          DeleteScenario(FCurrentScenarioID);
          MouseInteractionEnabled := False;
          Visible := False;
          if FFreePanelWhenEndAnimIsDone then Kill;
        end;
    end;
  end;
end;

constructor TUIPanelWithEffects.Create(aParentScene: TOGLCScene);
begin
  inherited Create(aParentScene);
  Visible := False;
  MouseInteractionEnabled := False;

  FSceneDarkness := TQuad4Color.Create(aParentScene);
  FSceneDarkness.SetSize(aParentScene.Width, aParentScene.Height);
  FSceneDarkness.Opacity.Value := 0;
end;

destructor TUIPanelWithEffects.Destroy;
begin
  if Assigned(FSceneDarkness) then
    FSceneDarkness.Free;
  FSceneDarkness := NIL;
  inherited Destroy;
end;

procedure TUIPanelWithEffects.ActivateSceneDarken(aColor: TBGRAPixel);
begin
  FDarkenTheScene := True;
  FSceneDarkness.SetAllColorsTo(aColor);
end;

procedure TUIPanelWithEffects.Show(const aAnimScenario: string);
begin
  if aAnimScenario = '' then begin
    MouseInteractionEnabled := True;
    Visible := True;
    exit;
  end;

  FCurrentScenarioID := AddAndPlayScenario(aAnimScenario);
  PostMessage(100000);
end;

procedure TUIPanelWithEffects.Hide(const aAnimScenario: string; aFree: boolean);
begin
  MouseInteractionEnabled := False;
  if aAnimScenario = '' then begin
    Visible := False;
    if aFree then Kill;
    exit;
  end;

  FFreePanelWhenEndAnimIsDone := aFree;
  FCurrentScenarioID := AddAndPlayScenario(aAnimScenario);
  PostMessage(100001);
end;

{$endif oglcIMPLEMENTATION}

