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
  FCurrentScenario: TScenario;
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
  // Player can interact with the mouse on the panel only when the scenario is terminated.
  // To show the panel immediatly without animation, sets aAnimScenario to ''
  procedure Show(const aAnimScenario: string); virtual;

  // Disable the mouse interaction with the panel, execute the scenario passed in parameter, then hide the panel.
  // The panel is hidden only when the scenario terminate.
  // To hide the panel immediatly without animation, sets aAnimScenario to ''
  // The panel is freed if aFree is True.
  procedure Hide(const aAnimScenario: string; aFree: boolean); virtual;

  // This method is called when the show animation is done
  procedure OnShowIsDone; virtual;
  // This method is called when the hide animation is done
  procedure OnHideIsDone; virtual;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TUIPanelWithEffects.Draw(const aLayerPercentOpacity: single);
var m: TOGLCMatrix;
begin
  // if needed draw the colored rectangle
  if FDarkenTheScene then begin
    m.CopyFrom(FParentScene.ModelViewMatrix);
    FParentScene.ModelViewMatrix.LoadIdentity;
    FSceneDarkness.Draw(aLayerPercentOpacity);
    FParentScene.ModelViewMatrix.CopyFrom(m);
  end;

  inherited Draw(aLayerPercentOpacity);
end;

procedure TUIPanelWithEffects.ProcessMessage(UserValue: TUserMessageValue);
begin
  case UserValue of
    100000: begin
      if FCurrentScenario.IsPlaying then PostMessage(100000)
        else OnShowIsDone;
    end;

    100001: begin
      if FCurrentScenario.IsPlaying then PostMessage(100001)
        else OnHideIsDone;
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
end;

destructor TUIPanelWithEffects.Destroy;
begin
  FreeAndNil(FSceneDarkness);
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

  FCurrentScenario := AddAndPlayScenario(aAnimScenario);
  PostMessage(100000);
end;

procedure TUIPanelWithEffects.Hide(const aAnimScenario: string; aFree: boolean);
begin
  MouseInteractionEnabled := False;

  if aAnimScenario = '' then begin
    FFreePanelWhenEndAnimIsDone := aFree;
    OnHideIsDone;
  end else begin
    FFreePanelWhenEndAnimIsDone := aFree;
    FCurrentScenario := AddAndPlayScenario(aAnimScenario);
    PostMessage(100001);
  end;
end;

procedure TUIPanelWithEffects.OnShowIsDone;
begin
  FCurrentScenario.Kill;
  FCurrentScenario := NIL;
  MouseInteractionEnabled := True;
end;

procedure TUIPanelWithEffects.OnHideIsDone;
begin
  if Assigned(FCurrentScenario) then FCurrentScenario.Kill;
  FCurrentScenario := NIL;
  Visible := False;
  if FFreePanelWhenEndAnimIsDone then Kill;
end;

{$endif oglcIMPLEMENTATION}

