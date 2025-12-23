{$ifdef oglcINTERFACE}
type
PModalPanelItem = ^TModalPanelItem;

{ TUIModalPanel }
// When a modal panel is shown, the scene is freezed and little darkened.
// When the modal panel is hidden, the scene continue where it left off.
// Modal panel can be used for example for the game pause panel.
TUIModalPanel = class(TUIPanelWithEffects)
private type ModalPanelState = (mpsShown, mpsHidden);
private
  FState: ModalPanelState;
public
  // aBlackOpacity is the opacity value for the black color applyed to the whole scene to darken it
  // when the modal panel is shown. 0= no darkening  255=full black
  constructor Create(aParentScene: TOGLCScene; aBlackOpacity: byte=100);

  // Show the modal panel: the scene is freezed and darkened.
  procedure ShowModal(const aAnimScenario: string); virtual;
  // Hide the modal panel: the scene continue normally.
  // if aFree is True, the panel will be freed on the next frame
  procedure Hide(const aAnimScenario: string; aFree: boolean); override;

  procedure OnHideIsDone; override;
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

constructor TUIModalPanel.Create(aParentScene: TOGLCScene; aBlackOpacity: byte=100);
begin
  inherited Create(aParentScene);
  FParentScene.FModalPanelList.Add(Self);

  if aBlackOpacity > 0 then
    ActivateSceneDarken(BGRA(0,0,0,aBlackOpacity));

  FState := mpsHidden;
end;

procedure TUIModalPanel.ShowModal(const aAnimScenario: string);
begin
  if FState = mpsShown then exit;
  FState := mpsShown;
  FParentScene.FModalPanelList.IncVisibleCount;
  Show(aAnimScenario);
end;

procedure TUIModalPanel.Hide(const aAnimScenario: string; aFree: boolean);
begin
  MouseInteractionEnabled := False;
  if FState = mpsHidden then exit;
  inherited Hide(aAnimScenario, aFree);
end;

procedure TUIModalPanel.OnHideIsDone;
begin
  inherited OnHideIsDone;
  FState := mpsHidden;
  FParentScene.FModalPanelList.DecVisibleCount;
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

