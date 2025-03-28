{$ifdef oglcINTERFACE}
type
PModalPanelItem = ^TModalPanelItem;

{ TUIModalPanel }
// When a modal panel is shown, the scene is freezed and little darkened.
// An application of the modal panel can be i.e. when user press ESC key during the game,
// a modal panel show a menu with button to resume game, a button to return to the main game menu...
// When the modal panel is hidden, the scene continue where it left off.
TUIModalPanel = class(TUIPanel)
private type ModalPanelState = (mpsShown, mpsHidden);
private
  FState: ModalPanelState;
  FItemInstance: PModalPanelItem;
  FSceneDarkness: TQuad4Color;
public
  procedure Draw(const aLayerPercentOpacity: single);  override;
public
  // aBlackOpacity is the opacity value for the black color applyed to the whole scene to darken it
  // when the modal panel is shown. 0= no darkening  255=full black
  constructor Create(aParentScene: TOGLCScene; aBlackOpacity: byte=100);
  destructor Destroy; override;

  // Show the modal panel: the scene is freezed and darkened.
  procedure ShowModal; virtual;
  // Hide the modal panel: the scene continue normally.
  // if aFree is True, the panel will be freed on the next frame
  procedure Hide(aFree: boolean); virtual;
end;


TModalPanelItem = record
  Panel: TUIModalPanel;
  Next,Prev: PModalPanelItem;
end;

{ TModalPanelList }

TModalPanelList = class(specialize TOGLCGList<TModalPanelItem>)
private
  FVisibleCount: integer;
  function GetOneModalIsVisible: boolean;
  procedure DecVisibleCount;
  procedure IncVisibleCount;
public
  destructor Destroy; override;
  function Update(const aElapsedTime: single): boolean;
  procedure Draw;

  function Add(aPanel: TUIModalPanel): PModalPanelItem;
  procedure FreeKilledPanels;
  procedure FreeAll;
  property OneModalIsVisible: boolean read GetOneModalIsVisible;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TUIModalPanel }

procedure TUIModalPanel.Draw(const aLayerPercentOpacity: single);
begin
  FParentScene.ModelViewMatrix.LoadIdentity;
  if FSceneDarkness <> NIL then FSceneDarkness.Draw(aLayerPercentOpacity);

  inherited Draw(aLayerPercentOpacity);
end;

constructor TUIModalPanel.Create(aParentScene: TOGLCScene; aBlackOpacity: byte=100);
begin
  inherited Create(aParentScene);
  FItemInstance := FParentScene.FModalPanelList.Add(Self);

  if aBlackOpacity > 0 then begin
    FSceneDarkness := TQuad4Color.Create(aParentScene.Width, aParentScene.Height);
    FSceneDarkness.ParentScene := aParentScene;
    FSceneDarkness.SetAllColorsTo(BGRA(0,0,0));
    FSceneDarkness.Opacity.Value := aBlackOpacity;
  end else FSceneDarkness := NIL;

  FState := mpsHidden;
end;

destructor TUIModalPanel.Destroy;
begin
  if FSceneDarkness <> NIL then begin
    FSceneDarkness.Free;
    FSceneDarkness := NIL;
  end;
  inherited Destroy;
end;

procedure TUIModalPanel.ShowModal;
begin
  if FState = mpsShown then exit;
  FState := mpsShown;
  FParentScene.FModalPanelList.IncVisibleCount;
end;

procedure TUIModalPanel.Hide(aFree: boolean);
begin
  if FState = mpsHidden then exit;
  FState := mpsHidden;
  FParentScene.FModalPanelList.DecVisibleCount;
  if aFree then Kill;
end;

{ TModalPanelList }

function TModalPanelList.GetOneModalIsVisible: boolean;
begin
  Result := FVisibleCount > 0;
end;

procedure TModalPanelList.DecVisibleCount;
begin
  Dec(FVisibleCount);
end;

procedure TModalPanelList.IncVisibleCount;
begin
  Inc(FVisibleCount);
end;

destructor TModalPanelList.Destroy;
begin
  FreeAll;
  inherited Destroy;
end;

function TModalPanelList.Update(const aElapsedTime: single): boolean;
var item: PModalPanelItem;
begin
  // update only the last modal panel
  item := LastItem;
  while item <> NIL do begin
    if item^.Panel.FState = mpsShown then begin
      item^.Panel.Update(aElapsedTime);
      Result := True;
      exit;
    end;
    item := item^.Prev;
  end;
  Result := False;
end;

procedure TModalPanelList.Draw;
var item: PModalPanelItem;
begin
  // render all panels
  item := FirstItem;
  while item <> NIL do begin
    if item^.Panel.FState = mpsShown then
      item^.Panel.Draw(1.0);
    item := item^.Next;
  end;
end;

function TModalPanelList.Add(aPanel: TUIModalPanel): PModalPanelItem;
begin
  Result := AddInstanceToList;
  Result^.Panel := aPanel;
end;

procedure TModalPanelList.FreeKilledPanels;
var item, nextItem: PModalPanelItem;
begin
  // free modal panels with flag kill
  item := FirstItem;
  while item <> NIL do begin
    nextItem := item^.Next;
    if item^.Panel.FKill then begin
      if item^.Panel.FState = mpsShown then DecVisibleCount;
      item^.Panel.Free;
      DoDelete(item);
    end;
    item := nextItem;
  end;
end;

procedure TModalPanelList.FreeAll;
var item: PModalPanelItem;
begin
  item := FirstItem;
  while item <> NIL do begin
    item^.Panel.Free;
    item := item^.Next;
  end;
  Clear;
  FVisibleCount := 0;
end;

{$endif oglcIMPLEMENTATION}

