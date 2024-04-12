{$ifdef oglcINTERFACE}
type

TUIScrollBarMode = (sbmNeverShow, sbmAlwaysShow, sbmAuto);

{ TUIClickableAndScrollableWithBodyShape }

TUIClickableAndScrollableWithBodyShape = class(TUIClickableWithBodyShape)
private
  FHScrollBarMode, FVScrollBarMode: TUIScrollBarMode;
  FHScrollBar, FVScrollBar: TUIScrollBar;
  function GetHMouseWheelDeltaValue: ptrint;
  function GetVMouseWheelDeltaValue: ptrint;
  procedure ProcessEventScrollBarChange({%H-}aSurface: TSimpleSurfaceWithEffect); virtual;
  procedure SetHMouseWheelDeltaValue(AValue: ptrint);
  procedure SetHScrollBarMode(AValue: TUIScrollBarMode);
  procedure SetVMouseWheelDeltaValue(AValue: ptrint);
  procedure SetVScrollBarMode(AValue: TUIScrollBarMode);
  function HaveScrollableParent: boolean;
protected
  procedure ResizeAndPlaceVScrollBar;
  procedure ResizeAndPlaceHScrollBar;
  procedure UpdateVScrollBar(aNeedVScrollBar: boolean; aMaxi, aPageSize: ptrint);
  procedure UpdateHScrollBar(aNeedHScrollBar: boolean; aMaxi, aPageSize: ptrint);
public
  constructor Create(aParentScene: TOGLCScene; aUseVScrollBar, aUseHScrollBar: boolean);
  property VScrollBarMode: TUIScrollBarMode read FVScrollBarMode write SetVScrollBarMode;
  property HScrollBarMode: TUIScrollBarMode read FHScrollBarMode write SetHScrollBarMode;
  property VMouseWheelDeltaValue: ptrint read GetVMouseWheelDeltaValue write SetVMouseWheelDeltaValue;
  property HMouseWheelDeltaValue: ptrint read GetHMouseWheelDeltaValue write SetHMouseWheelDeltaValue;
end;


{ TUIScrollBox }

TUIScrollBox = class(TUIClickableAndScrollableWithBodyShape)
private
  FNeedUpdateScrollBar: boolean;
  function GetViewOffset: TPoint;
  procedure ProcessEventScrollBarChange({%H-}aSurface: TSimpleSurfaceWithEffect); override;
  procedure UpdateWidgetSizeAndLayout; override;
protected
  procedure DrawChildSurfacesWithPositiveZOrder(const aLayerPercentOpacity: single); override;
public
  procedure DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean); override;
  procedure Update(const aElapsedTime: single); override;
  procedure AddChild(ASurface: TSimpleSurfaceWithEffect); reintroduce;
  property NeedUpdateScrollBar: boolean read FNeedUpdateScrollBar write FNeedUpdateScrollBar;
  // Return the offset of the top/left corner of the view, according to the scrollbars position.
  // If there is no ScrollBar visible, (0,0) is returned.
  property ViewOffset: TPoint read GetViewOffset;
end;


TUIListBoxItem = record
  Str: string;
  CharIndexes: TTexturedCharIndexes;
  Selected: boolean;
end;
PUIListBoxItem = ^TUIListBoxItem;
TListOfListBoxItem = class(specialize TVector<TUIListBoxItem>);

{ TUIListBoxItemStyleDescriptor }

TUIListBoxItemStyleDescriptor = record
private
  function GetOnChange: TOGLCEvent;
  procedure SetOnChange(AValue: TOGLCEvent);
  property OnChange: TOGLCEvent read GetOnChange write SetOnChange;
public
  // Define the color/gradient to use for the background when rendering unselected items.
  GradientItem: TGradientDescriptor;
  ColorText: TBGRAPixel;
  // Define the color/gradient to use for the background when rendering selected items.
  GradientItemSelected: TGradientDescriptor;
  ColorSelectedText: TBGRAPixel;
end;

{ TUIListBox }

TUIListBox = class(TUIClickableAndScrollableWithBodyShape)
private
  FOnSelectionChange: TUIEvent;
  FTexturedFont: TTexturedFont;
  FItems: TListOfListBoxItem;
  FMultiSelect: boolean;
  FIndexFirstItemVisible: Integer;
  FYFirstItemVisible: integer;
  FIndexFirstItemSelected, FSelectedCount: integer;
  FItemHeight: integer;
  FGradientChanged: boolean;
  function GetCount: integer;
  procedure CheckIndex(aIndex: integer);
  function CreateItem(const aStr: string): TUIListBoxItem;
  function GetFirstSelectedItem: string;
  procedure SetIndexFirstItemSelected(AValue: integer);
  procedure SetItemHeight(AValue: integer);
  procedure SetSelectedStateOnAllItems(aState: boolean);
  function GetSelected(index: integer): boolean;
  function GetStrItem(index: integer): string;
  procedure SetSelected(index: integer; AValue: boolean);
  procedure SetStrItem(index: integer; AValue: string);
  function InternalAdd(const aStr: string): Integer;
  procedure UpdateIndexFirstSelectedItem;
  procedure ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect); override;
  procedure ProcessGradientColorChange;
  procedure UpdateWidgetSizeAndLayout; override;
public
  procedure DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean); override;
  procedure DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint); override;
  procedure DoDraw; override;
public
  // Allow to define the color/gradient for unselected and selected items in the list.
  ItemColor: TUIListBoxItemStyleDescriptor;
  constructor Create(aParentScene: TOGLCScene; aFont: TTexturedFont);
  destructor Destroy; override;

  procedure Clear;
  // Add one string to the list.
  function Add(const aStr: string): integer;
  // Add an array of strings to the list
  procedure Append(const aStrArr: TStringArray);
  // Delete an item by its index.
  procedure Delete(aIndex: integer);
  // Insert an item at the specified index.
  procedure Insert(aIndex: integer; const aStr: string);
  // exchange the position of 2 items.
  procedure Exchange(aIndex1, aIndex2: integer);
  // Return the index of an item, -1 if not found.
  function IndexOf(const aStr: string): integer;
  // Return the index of the item currently on the view at the specified Y coordinate.
  // Return -1 if none.
  function IndexAt(aY: integer): integer;
  //procedure Sort;  // to do
  //procedure CustomSort(CompareFn: TStringListSortCompare);  // to do

  // Unselect all items.
  procedure ClearSelection;
  // Select all items.
  procedure SelectAll;
  // Force an item to be visible in the list.
  procedure MakeItemVisible(aIndex: integer);

  // The item height .
  property ItemHeight: integer read FItemHeight write SetItemHeight;
  // Number of items currently in the listbox.
  property Count: integer read GetCount;
  // Get/Set the index of first selected item.
  property FirstSelectedIndex: integer read FIndexFirstItemSelected write SetIndexFirstItemSelected;
  // Get the string of the item currently selected. '' is returned if there is no selection.
  property FirstSelectedItem: string read GetFirstSelectedItem;
  // Access to Items by their indexes.
  property Items[index: integer]: string read GetStrItem write SetStrItem;
  // Get/Set the selected state of an item by its index.
  property Selected[index: integer]: boolean read GetSelected write SetSelected;
  // Return the selected count.
  property SelectedCount: integer read FSelectedCount;
  // Set this property to True to allow the user to select more than one item in the list with
  // key Shift or Ctrl.
  property MultiSelect: boolean read FMultiSelect write FMultiSelect;
  // Fired when user change the selection.
  property OnSelectionChange: TUIEvent read FOnSelectionChange write FOnSelectionChange;
end;


{ TAlignedTextDescriptor }

TAlignedTextDescriptor = record
private type
  TWord_ = record
    Word: string;
    CharIndexes: TTexturedCharIndexes;
    Location: TPoint;
    Width,
    LineIndex: integer;
  end;
  ArrayOfWord_ = array of TWord_;
private
  FCaption: string;
  FAlign: TOGLCAlignment;
  FTexturedFont: TTexturedFont;
  FSpaceCharWidth: integer;
  FWords: ArrayOfWord_;
  FLineCount: integer;
  FReadyToRender: boolean;
private
  FCallbackSizeChanged: TOGLCEvent;
  procedure InitDefault;
  procedure FreeObjects;
  procedure DistributeTheWordsOnTheSurface;
  function GetCaption: string;
  procedure SetAlign(AValue: TOGLCAlignment);
  procedure SetCaption(AValue: string);
  procedure SetTexturedFont(AValue: TTexturedFont);
  // return True if some words are found on the specified line index.
  // False means this line is empty.
  function GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
  procedure DoVTopAlign;
  procedure DoVCenterAlign;
  procedure DoVBottomAlign;
  procedure DoHLeftAlign;
  procedure DoHCenterAlign;
  procedure DoHRightAlign;
  procedure DoAlign;
public
  Tint: TBGRAParam;
  Area: TRect;
  DrawingRect: TRect;  // real size of the text area
  DrawingSize: TSize; // The real size of the text area
  // Initialize this property with the text you want to see in the text area.
  // Separate the lines with #10 character or any kind of LineEnding.
  property Caption: string read GetCaption write SetCaption;
  property Align: TOGLCAlignment read FAlign write SetAlign;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
  // fired when caption or Textured font change.
  property OnSizeChanged: TOGLCEvent read FCallbackSizeChanged write FCallbackSizeChanged;
  // True if the caption is <> '' and property TexturedFont is initialized
  property ReadyToRender: boolean read FReadyToRender;
end;

{ TUITextArea }

TUITextArea = class(TUIClickableAndScrollableWithBodyShape)
private
  FIndexFirstLineVisible,
  FYFirstLineVisible: integer;
  FNeedFormatText: boolean;
  procedure ProcessCallbackSomethingChangeTheSize; override;
  procedure ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect); override;
  procedure UpdateWidgetSizeAndLayout; override;
public
  procedure DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean); override;
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  // Word wrapped text in a rectangular area.
  // Use LineEnding (#13#10 #13 or #10) for multilines text.
  Text: TAlignedTextDescriptor;
  Constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;
  procedure MakeLastLineVisible;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TUIClickableAndScrollableWithBodyShape }

procedure TUIClickableAndScrollableWithBodyShape.ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect);
begin
end;

procedure TUIClickableAndScrollableWithBodyShape.SetHMouseWheelDeltaValue(AValue: ptrint);
begin
  FHScrollBar.MouseWheelDeltaValue := AValue;
end;

function TUIClickableAndScrollableWithBodyShape.GetVMouseWheelDeltaValue: ptrint;
begin
  Result := FVScrollBar.MouseWheelDeltaValue;
end;

function TUIClickableAndScrollableWithBodyShape.GetHMouseWheelDeltaValue: ptrint;
begin
  Result := FHScrollBar.MouseWheelDeltaValue;
end;

procedure TUIClickableAndScrollableWithBodyShape.SetHScrollBarMode(AValue: TUIScrollBarMode);
begin
  if FHScrollBarMode = AValue then Exit;
  FHScrollBarMode := AValue;
  if FHScrollBar <> NIL then UpdateWidgetSizeAndLayout;
end;

procedure TUIClickableAndScrollableWithBodyShape.SetVMouseWheelDeltaValue(AValue: ptrint);
begin
  FVScrollBar.MouseWheelDeltaValue := AValue;
end;

procedure TUIClickableAndScrollableWithBodyShape.SetVScrollBarMode(AValue: TUIScrollBarMode);
begin
  if FVScrollBarMode = AValue then Exit;
  FVScrollBarMode := AValue;
  if FVScrollBar <> NIL then UpdateWidgetSizeAndLayout;
end;

function TUIClickableAndScrollableWithBodyShape.HaveScrollableParent: boolean;
var o: TSimpleSurfaceWithEffect;
begin
  o := ParentSurface;
  while o <> NIL do begin
    if o is TUIClickableAndScrollableWithBodyShape then exit(True);
    o := o.ParentSurface;
  end;
  Result := False;
end;

procedure TUIClickableAndScrollableWithBodyShape.ResizeAndPlaceVScrollBar;
var newSize: integer;
begin
  if FVScrollBar = NIL then exit;

  newSize := ClientArea.Height;
  if (FHScrollBar <> NIL) and FHScrollBar.Visible then
    newSize := newSize - FHScrollBar.ClientArea.Height;
  if FVScrollBar.BodyShape.Height <> newSize then
    FVScrollBar.BodyShape.ResizeCurrentShape(FVScrollBar.BodyShape.Width, newSize, False);

  FVScrollBar.SetCoordinate(Width-FVScrollBar.Width, ClientArea.top);
end;

procedure TUIClickableAndScrollableWithBodyShape.ResizeAndPlaceHScrollBar;
var newSize: integer;
begin
  if FHScrollBar = NIL then exit;

  newSize := ClientArea.Width;
  if (FVScrollBar <> NIL) and FVScrollBar.Visible then
    newSize := newSize - FVScrollBar.ClientArea.Width;
  if FHScrollBar.BodyShape.Width <> newSize then
    FHScrollBar.BodyShape.ResizeCurrentShape(newSize, FHScrollBar.BodyShape.Height, False);

  FHScrollBar.SetCoordinate(ClientArea.Left, ClientArea.Top+ClientArea.Bottom-FHScrollBar.Height);
end;

procedure TUIClickableAndScrollableWithBodyShape.UpdateVScrollBar(aNeedVScrollBar: boolean; aMaxi, aPageSize: ptrint);
begin
  if FVScrollBar = NIL then exit;

  if aNeedVScrollBar and (FVScrollBarMode <> sbmNeverShow) then begin
    // resize VScrollBar if needed
    ResizeAndPlaceVScrollBar;

    if not FVScrollBar.Visible then begin
      FVScrollBar.Visible := True;
      FVScrollBar.SetParams(0, 0, aMaxi, aPageSize);
    end else begin
      // update VScrollBar parameters
      if (FVScrollBar.Max <> aMaxi) or (FVScrollBar.PageSize <> aPageSize) then
        FVScrollBar.SetParams(FVScrollBar.Position, 0, aMaxi, aPageSize);
    end;
    //FVScrollBar.SetCoordinate(Width-FVScrollBar.Width, ClientArea.top);
  end else begin
    FVScrollBar.Visible := False;
  end;
end;

procedure TUIClickableAndScrollableWithBodyShape.UpdateHScrollBar(aNeedHScrollBar: boolean; aMaxi, aPageSize: ptrint);
begin
  if FHScrollBar = NIL then exit;

  if aNeedHScrollBar and (FHScrollBarMode <> sbmNeverShow) then begin
    // resize HScrollBar if needed
    ResizeAndPlaceHScrollBar;

    if not FHScrollBar.Visible then begin
      FHScrollBar.Visible := True;
      FHScrollBar.SetParams(0, 0, aMaxi, aPageSize);
    end else begin
      // update HScrollBar parameters
      if (FHScrollBar.Max <> aMaxi) or (FHScrollBar.PageSize <> aPageSize) then
        FHScrollBar.SetParams(FHScrollBar.Position, 0, aMaxi, aPageSize);
    end;
    //FHScrollBar.SetCoordinate(ClientArea.Left, ClientArea.Top+ClientArea.Bottom-FHScrollBar.Height);
  end else begin
    FHScrollBar.Visible := False;
  end;
end;

constructor TUIClickableAndScrollableWithBodyShape.Create(aParentScene: TOGLCScene; aUseVScrollBar, aUseHScrollBar: boolean);
begin
  inherited Create(aParentScene);

  if aUseVScrollBar then begin
    FVScrollBar := TUIScrollBar.Create(aParentScene, uioVertical);
    FVScrollBar.BodyShape.Border.Color := FVScrollBar.BodyShape.Fill.Color;
    FVScrollBar.Visible := False;
    FVScrollBar.OnChange := @ProcessEventScrollBarChange;
    AddChild(FVScrollBar);
    FHScrollBarMode := sbmAuto;
  end;

  if aUseHScrollBar then begin
    FHScrollBar := TUIScrollBar.Create(aParentScene, uioHorizontal);
    FHScrollBar.BodyShape.Border.Color := FVScrollBar.BodyShape.Fill.Color;
    FHScrollBar.Visible := False;
    FHScrollBar.OnChange := @ProcessEventScrollBarChange;
    AddChild(FHScrollBar);
    FVScrollBarMode := sbmAuto;
  end;
end;

{ TUIScrollBox }

function TUIScrollBox.GetViewOffset: TPoint;
begin
  if (FHScrollBar <> NIL) and FHScrollBar.Visible then Result.x := FHScrollBar.Position
    else Result.x := 0;
  if (FVScrollBar <> NIL) and FVScrollBar.Visible then Result.y := FVScrollBar.Position
    else Result.y := 0;
end;

procedure TUIScrollBox.ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect);
begin
end;

procedure TUIScrollBox.DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean);
begin
  inherited DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
  if (FVScrollBar <> NIL) and FVScrollBar.Visible then begin
    FVScrollBar.DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
    aHandled := True;
  end;
end;

procedure TUIScrollBox.UpdateWidgetSizeAndLayout;
var i, hmin, hmax, vmin, vmax, v: integer;
  o: TSimpleSurfaceWithEffect;
  needScrollBar: boolean;
begin
  FNeedUpdateScrollBar := False;

  ComputeClientArea;

  // hmin/hmax and vmin/vmax represents the rectangular bounds occupied by the childs.
  // we use this bounds to check if the scrollBars are needed.
  hmin := 0;
  hmax := 0; //ClientArea.Width;
  vmin := 0;
  vmax := 0; //ClientArea.Height;

  for i:=0 to FChilds.Count-1 do begin
    o := TSimpleSurfaceWithEffect(FChilds[i]);
    if (o is TUIClickableObject) and (o <> FHScrollBar) and (o <> FVScrollBar) then begin
      if FHScrollBar <> NIL then begin
        v := Round(o.X.Value);
        if hmin > v then hmin := v;
        v := Round(o.RightX);
        if hmax < v then hmax := v;
      end;
      if FVScrollBar <> NIL then begin
        v := Round(o.Y.Value);
        if vmin > v then vmin := v;
        v := Round(o.BottomY);
        if vmax < v then vmax := v;
      end;
    end;
  end;

  if (FHScrollBar <> NIL) and (FHScrollBarMode <> sbmNeverShow) then begin
    needScrollBar := (hmin < 0) or (hmax > ClientArea.Right-ClientArea.Left) or (FHScrollBarMode = sbmAlwaysShow);
    if needScrollBar then begin
      ResizeAndPlaceHScrollBar;
      if not FHScrollBar.Visible then begin
        FHScrollBar.Visible := True;
        if (FVScrollBar <> NIL) and FVScrollBar.Visible then v := FVScrollBar.Width else v := 0;
        FHScrollBar.SetParams(0, hmin, hmax+v, ClientArea.Width);
      end else FHScrollBar.SetParams(FHScrollBar.Position, hmin, hmax+v, ClientArea.Width);
    end;
  end;

  if (FVScrollBar <> NIL) and (FVScrollBarMode <> sbmNeverShow) then begin
    needScrollBar := (vmin < 0) or (vmax > ClientArea.Bottom-ClientArea.Top) or (FVScrollBarMode = sbmAlwaysShow);

    if needScrollBar then begin
      ResizeAndPlaceVScrollBar;
      if not FVScrollBar.Visible then begin
        FVScrollBar.Visible := True;
        if (FHScrollBar <> NIL) and FHScrollBar.Visible then v := FHScrollBar.Height else v := 0;
        FVScrollBar.SetParams(0, vmin, vmax+v, ClientArea.Height);
        // re-adjust the HScrollBar because now VScrollBar is visible
        ResizeAndPlaceHScrollBar;
        if (FHScrollBar <> NIL) and (FHScrollBarMode <> sbmNeverShow) then begin
          if (FVScrollBar <> NIL) and FVScrollBar.Visible then v := FVScrollBar.Width else v := 0;
          FHScrollBar.SetParams(0, hmin, hmax+v, ClientArea.Width);
        end;
      end else FVScrollBar.SetParams(FVScrollBar.Position, vmin, vmax+v, ClientArea.Height);
    end;
  end;
end;

procedure TUIScrollBox.DrawChildSurfacesWithPositiveZOrder(const aLayerPercentOpacity: single);
var m: TOGLCMatrix;
  i: integer;
  p: TPoint;
  o: TSimpleSurfaceWithEffect;
begin
  m.CopyFrom(FParentScene.ModelViewMatrix);

  p := ClientArea.TopLeft - GetViewOffset;
  FParentScene.ModelViewMatrix.Translate(p.X, p.Y);

  for i:=0 to FChilds.Count-1 do begin
    o := TSimpleSurfaceWithEffect(FChilds.Items[i]);
    if o.Visible and (o <> FHScrollBar) and (o <> FVScrollBar) then
      o.Draw(aLayerPercentOpacity);
  end;

  FParentScene.ModelViewMatrix.CopyFrom(m);

  if FHScrollBar <> NIL then FHScrollBar.Draw(aLayerPercentOpacity);
  if FVScrollBar <> NIL then FVScrollBar.Draw(aLayerPercentOpacity);
end;

procedure TUIScrollBox.Update(const aElapsedTime: single);
var i: integer;
  o: TSimpleSurfaceWithEffect;
  p: TPoint;
  nestedScrollableParent: boolean;
begin
  p := ClientArea.TopLeft - GetViewOffset;
  nestedScrollableParent := HaveScrollableParent;

  // set or add scroll offset on childs
  for i:=0 to ChildCount-1 do begin
    o := Childs[i];
    if (o is TUIClickableObject) and (o <> FHScrollBar) and (o <> FVScrollBar) then
      if nestedScrollableParent then TUIClickableObject(o).ScrollOffset := TUIClickableObject(o).ScrollOffset + p
        else TUIClickableObject(o).ScrollOffset := p;
  end;

  inherited Update(aElapsedTime);
  if not FFreeze then
    if FNeedUpdateScrollBar then UpdateWidgetSizeAndLayout;
end;

procedure TUIScrollBox.AddChild(ASurface: TSimpleSurfaceWithEffect);
begin
  if ASurface = NIL then exit;
  inherited AddChild(ASurface, 0);
  FNeedUpdateScrollBar := True;
end;

{ TUIListBoxItemStyleDescriptor }

function TUIListBoxItemStyleDescriptor.GetOnChange: TOGLCEvent;
begin
  Result := GradientItem.OnChange;
end;

procedure TUIListBoxItemStyleDescriptor.SetOnChange(AValue: TOGLCEvent);
begin
  GradientItem.OnChange := AValue;
  GradientItemSelected.OnChange := AValue;
end;

{ TUIListBox }

procedure TUIListBox.UpdateWidgetSizeAndLayout;
var needVScrollBar: boolean;
   numberOfItemVisible: integer;
   maxi, page: ptrint;
   r: TRect;
begin
  r := ClientArea;
  ComputeClientArea;
  if (ClientArea <> r) or FGradientChanged then begin
    FGradientChanged := False;
    ItemColor.GradientItem.ComputeVerticesAndIndices(Width, FItemHeight);
    ItemColor.GradientItemSelected.ComputeVerticesAndIndices(Width, FItemHeight);
  end;

  if FItemHeight > 0 then numberOfItemVisible := Round(ClientArea.Height/FItemHeight)
    else numberOfItemVisible := 0;

  needVScrollBar := ClientArea.Height > 0; // necessary to avoid a range check error
  if needVScrollBar then
    needVScrollBar := (FItemHeight*FItems.Size > ClientArea.Height) and (numberOfItemVisible > 0);

  if needVScrollBar then begin
    // adjust gradients
    if not FVScrollBar.Visible then begin
      ItemColor.GradientItem.ComputeVerticesAndIndices(Width-FVScrollBar.Width, FItemHeight);
      ItemColor.GradientItemSelected.ComputeVerticesAndIndices(Width-FVScrollBar.Width, FItemHeight);
    end;
    // update VScrollBar
    maxi := FItems.Size*FItemHeight;
    page := numberOfItemVisible*FItemHeight;
    UpdateVScrollBar(needVScrollBar, maxi, page);
    FYFirstItemVisible := -FVScrollBar.Position mod FItemHeight;
  end else begin
    // adjust gradients
    if FVScrollBar.Visible then begin
      ItemColor.GradientItem.ComputeVerticesAndIndices(Width, FItemHeight);
      ItemColor.GradientItemSelected.ComputeVerticesAndIndices(Width, FItemHeight);
    end;
    // update VScrollBar
    UpdateVScrollBar(needVScrollBar, 0, 0);
    FYFirstItemVisible := 0;
  end;
end;

procedure TUIListBox.DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer; aMousePos: TPoint; var aHandled: Boolean);
begin
  inherited DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
  if FVScrollBar.Visible then begin
    FVScrollBar.DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
    aHandled := True;
  end;
end;

procedure TUIListBox.DoOnMouseDown(aButton: TMouseButton; aShift: TShiftState; aPt: TPoint);
var idx, i: integer;
begin
  inherited DoOnMouseDown(aButton, aShift, aPt);

  idx := IndexAt(aPt.Y);
  if FMultiSelect and (idx <> -1) then begin
    if CTRLKEY in aShift then begin  // toogle item state
      FItems.Mutable[idx]^.Selected := not FItems.Mutable[idx]^.Selected;
      if FItems.Mutable[idx]^.Selected then begin
        inc(FSelectedCount);
        if (FirstSelectedindex = -1) or (FirstSelectedindex > idx) then FIndexFirstItemSelected := idx;
      end else begin
        if FirstSelectedindex = idx then FirstSelectedindex := -1
          else dec(FSelectedCount);
        UpdateIndexFirstSelectedItem;
      end;
      if FOnSelectionChange <> NIL then FOnSelectionChange(Self);
    end
    else
    if ssShift in aShift then begin  // range selection
      if FirstSelectedindex = -1 then FirstSelectedindex := idx
        else if FirstSelectedindex <> idx then begin
          if idx < FirstSelectedindex then begin
            repeat
              if not FItems.Mutable[idx]^.Selected then inc(FSelectedCount);
              FItems.Mutable[idx]^.Selected := True;
              inc(idx);
            until idx = FirstSelectedindex;
          end else begin
              i := FirstSelectedindex + 1;
              repeat
                if not FItems.Mutable[i]^.Selected then inc(FSelectedCount);
                FItems.Mutable[i]^.Selected := True;
                inc(i);
              until i > idx;
              FIndexFirstItemSelected := idx;
          end;
          if FOnSelectionChange <> NIL then FOnSelectionChange(Self);
        end;
    end
    else begin
      ClearSelection;
      FirstSelectedindex := idx;
      if FOnSelectionChange <> NIL then FOnSelectionChange(Self);
    end;
  end else begin
    FirstSelectedindex := idx;
    if FOnSelectionChange <> NIL then FOnSelectionChange(Self);
  end;
end;

function TUIListBox.GetCount: integer;
begin
  Result := FItems.Size;
end;

procedure TUIListBox.CheckIndex(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex >= FItems.Size) or (FItems.Size = 0) then
    raise exception.Create('index out of bounds');
end;

function TUIListBox.CreateItem(const aStr: string): TUIListBoxItem;
begin
  Result.Str := aStr;
  Result.CharIndexes := Copy(FTexturedFont.StringToCharIndexes(aStr));
  Result.Selected := False;
end;

function TUIListBox.GetFirstSelectedItem: string;
begin
  if FirstSelectedIndex = -1 then Result := ''
    else Result := Items[FirstSelectedIndex];
end;

procedure TUIListBox.SetIndexFirstItemSelected(AValue: integer);
begin
  if FIndexFirstItemSelected <> -1 then begin
    FItems.Mutable[FIndexFirstItemSelected]^.Selected := False;
    dec(FSelectedCount);
  end;

  if AValue <> -1 then begin
    CheckIndex(AValue);
    FItems.Mutable[AValue]^.Selected := True;
    inc(FSelectedCount);
  end;

  FIndexFirstItemSelected := AValue;
end;

procedure TUIListBox.SetItemHeight(AValue: integer);
begin
  if FItemHeight = AValue then Exit;
  FItemHeight := AValue;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.SetSelectedStateOnAllItems(aState: boolean);
var i: SizeUInt;
begin
  for i:=0 to FItems.Size do
    FItems.Mutable[i]^.Selected := aState;
end;

function TUIListBox.GetSelected(index: integer): boolean;
begin
  CheckIndex(index);
  Result := FItems.Mutable[index]^.Selected
end;

function TUIListBox.GetStrItem(index: integer): string;
begin
  CheckIndex(index);
  Result := FItems.Mutable[index]^.Str;
end;

procedure TUIListBox.SetSelected(index: integer; AValue: boolean);
begin
  CheckIndex(index);
  FItems.Mutable[index]^.Selected := AValue;
end;

procedure TUIListBox.SetStrItem(index: integer; AValue: string);
begin
  CheckIndex(index);
  FItems.Mutable[index]^.Str := AValue;
  FItems.Mutable[index]^.CharIndexes := Copy(FTexturedFont.StringToCharIndexes(AValue));
end;

function TUIListBox.InternalAdd(const aStr: string): Integer;
begin
  FItems.PushBack(CreateItem(aStr));
  Result := FItems.Size-1;
end;

procedure TUIListBox.UpdateIndexFirstSelectedItem;
var i: Integer;
begin
  if FSelectedCount = 0 then begin
    FIndexFirstItemSelected := -1;
    exit;
  end else
    for i:=0 to FItems.Size-1 do
      if FItems.Mutable[i]^.Selected then begin
        FIndexFirstItemSelected := i;
        exit;
      end;
  raise exception.create('bug dans l''algorithme');
end;

procedure TUIListBox.ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect);
begin
  if aSurface = FVScrollBar then
    FIndexFirstItemVisible := Trunc(FVScrollBar.Position/FItemHeight);
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.ProcessGradientColorChange;
begin
  FGradientChanged := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.DoDraw;
var idx, xx, yy: integer;
   m: TOGLCMatrix;
   computedTint: TColorF;
begin
  if (FItems.Size = 0) then exit;

  m.CopyFrom(FParentScene.ModelViewMatrix);
  // draw visible items
  idx := FIndexFirstItemVisible;
  xx := ClientArea.Left;
  yy := ClientArea.Top + FYFirstItemVisible;
  repeat
    FParentScene.ModelViewMatrix.Translate(0, yy);
    // draw item background
    FParentScene.ThreeColorTriangleRenderer.Prepare(ptTriangleStrip, FParentScene.MVPMatrix, FComputedOpacity, FComputedTint, FBlendMode);
    if FItems.Mutable[idx]^.Selected then begin
      FParentScene.ThreeColorTriangleRenderer.PushGradient(@ItemColor.GradientItemSelected);
      computedTint.InitFromTint(tmReplaceColor, ItemColor.ColorSelectedText);
    end else begin
      FParentScene.ThreeColorTriangleRenderer.PushGradient(@ItemColor.GradientItem);
      computedTint.InitFromTint(tmReplaceColor, ItemColor.ColorText);
    end;
    FParentScene.ThreeColorTriangleRenderer.Batch_Flush;
    // draw item string
    FTexturedFont.DrawChars(FItems.Mutable[idx]^.CharIndexes, xx, 0, 0, NIL, NIL, FComputedOpacity, computedTint, BlendMode);

    FParentScene.ModelViewMatrix.CopyFrom(m);
    inc(idx);
    inc(yy, FItemHeight);
  until (yy >= ClientArea.Bottom) or (idx = FItems.Size);
end;

constructor TUIListBox.Create(aParentScene: TOGLCScene; aFont: TTexturedFont);
begin
  inherited Create(aParentScene, True, False);
  FItems := TListOfListBoxItem.Create;
  FIndexFirstItemSelected := -1;

  VScrollBarMode := sbmAuto;
  HScrollBarMode := sbmNeverShow;

  FTexturedFont := aFont;
  FItemHeight := aFont.Font.FontHeight;

  ItemColor.GradientItem.CreateSingleColor(BGRA(30,30,30));
  ItemColor.GradientItemSelected.CreateSingleColor(BGRA(51,153,255));
  ItemColor.ColorText := BGRA(220,220,220);
  ItemColor.ColorSelectedText := BGRA(255,255,255);
  ItemColor.OnChange := @ProcessGradientColorChange;
end;

destructor TUIListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TUIListBox.Clear;
begin
  FItems.Clear;
  UpdateWidgetSizeAndLayout;
end;

function TUIListBox.Add(const aStr: string): integer;
begin
  Result := InternalAdd(aStr);
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.Append(const aStrArr: TStringArray);
var i: Integer;
begin
  if Length(aStrArr) = 0 then exit;
  for i:=0 to High(aStrArr) do InternalAdd(aStrArr[i]);
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.Delete(aIndex: integer);
var flagWasSelected: boolean;
begin
  CheckIndex(aIndex);
  flagWasSelected := FItems.Mutable[aIndex]^.Selected;
  FItems.Erase(aIndex);

  if flagWasSelected then dec(FSelectedCount);
  if FIndexFirstItemSelected = aIndex then UpdateIndexFirstSelectedItem
    else if FIndexFirstItemSelected > aIndex then dec(FIndexFirstItemSelected);

  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.Insert(aIndex: integer; const aStr: string);
begin
  if (aIndex < 0) then raise exception.create('index can not be negative');
  aIndex := Min(int64(aIndex), int64(FItems.Size));

  if FIndexFirstItemSelected >= aIndex then inc(FIndexFirstItemSelected);

  FItems.Insert(aIndex, CreateItem(aStr));
  UpdateWidgetSizeAndLayout;
end;

procedure TUIListBox.Exchange(aIndex1, aIndex2: integer);
var itm: TUIListBoxItem;
begin
  CheckIndex(aIndex1);
  CheckIndex(aIndex2);

  if FIndexFirstItemSelected = aIndex1 then FIndexFirstItemSelected := aIndex2
    else if FIndexFirstItemSelected = aIndex2 then FIndexFirstItemSelected := aIndex1;

  Move(FItems.Mutable[aIndex1]^, itm, SizeOf(TUIListBoxItem));
  Move(FItems.Mutable[aIndex2]^, FItems.Mutable[aIndex1]^, SizeOf(TUIListBoxItem));
  Move(itm, FItems.Mutable[aIndex1]^, SizeOf(TUIListBoxItem));
end;

function TUIListBox.IndexOf(const aStr: string): integer;
var i: SizeUInt;
begin
  for i:=0 to FItems.Size do
    if FItems.Mutable[i]^.Str = aStr then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TUIListBox.IndexAt(aY: integer): integer;
begin
  if FItems.Size = 0 then Result := -1
    else begin
      Result := Trunc((aY-FYFirstItemVisible-ClientArea.Top)/FItemHeight) + FIndexFirstItemVisible;
      if Result >= FItems.Size then Result := -1;
    end;
end;

procedure TUIListBox.ClearSelection;
begin
  SetSelectedStateOnAllItems(False);
  FSelectedCount := 0;
  FIndexFirstItemSelected := -1;
end;

procedure TUIListBox.SelectAll;
begin
  if not FMultiSelect then exit;

  SetSelectedStateOnAllItems(True);
  FSelectedCount := FItems.Size;
  if FSelectedCount > 0 then FIndexFirstItemSelected := 0
    else FIndexFirstItemSelected := -1;
end;

procedure TUIListBox.MakeItemVisible(aIndex: integer);
begin
  if FVScrollBar = NIL then exit;
  if not FVScrollBar.Visible then exit;
  FVScrollBar.Position := aIndex * FItemHeight;
end;

{procedure TUIListBox.Sort;
begin
  FItems.Sort;
end;

procedure TUIListBox.CustomSort(CompareFn: TStringListSortCompare);
begin
  FItems.CustomSort(CompareFn);
end;}

{ TAlignedTextDescriptor }

procedure TAlignedTextDescriptor.InitDefault;
begin
  FillChar(Self, SizeOf(TAlignedTextDescriptor), 0);
  Tint := TBGRAParam.Create;
  Tint.Value := BGRA(220,220,220);
end;

procedure TAlignedTextDescriptor.FreeObjects;
begin
  FreeAndNil(Tint);
end;

procedure TAlignedTextDescriptor.DistributeTheWordsOnTheSurface;
var W: TStringArray;
   iw, j, spaceWidth, xx, yy: Integer;
   flagLastWasLineEnding: boolean;
   procedure _NextLine;
   begin
    yy += FTexturedFont.Font.FontHeight;
    xx := 0;
    inc(FLineCount);
   end;
   procedure PutWordOnCurrentLine;
   begin
    FWords[j].Location := Point(xx, yy);
    FWords[j].LineIndex := FLineCount-1;
    xx += FWords[j].Width+spaceWidth;
    inc(j);
   end;
   procedure DoWordWrapOnCurrentLine;
   var ch: PTexturedChar;
      k, ww: integer;
      fullWord, partialWord: string;
   begin
     ww := 0;
     k := 0;
     fullWord := FWords[j].Word;
     repeat
       // fill the current line
      partialWord := '';
       repeat
         ch := FTexturedFont.GetCharByIndex(FWords[j].CharIndexes[k]);
         if ww + ch^.OriginalCharSize.cx > Area.Width then break;
         ww := ww + ch^.OriginalCharSize.cx;
         partialWord := partialWord + ch^.Character;
         inc(k);
       until k = Length(FWords[j].CharIndexes);
       // if no letter is placed -> exit
       if partialWord = '' then exit;
       // register the partial line
       FWords[j].Width := ww;
       FWords[j].Location := Point(xx, yy);
       FWords[j].LineIndex := FLineCount-1;
       FWords[j].Word := partialWord;
       FWords[j].CharIndexes := TexturedFont.StringToCharIndexes(partialWord);
       // remove partial line from full word
       Delete(fullWord, 1, Length(partialWord));
       // add a new line if there remain some characters to position
       if fullWord <> '' then begin
         SetLength(FWords, Length(FWords)+1);
         _NextLine;
         inc(j);
         FWords[j].Word := fullWord;
         FWords[j].CharIndexes := TexturedFont.StringToCharIndexes(fullWord);
         FWords[j].Width := TexturedFont.CharIndexesToSize(FWords[j].CharIndexes).cx;
         FWords[j].LineIndex := FLineCount-1;
         FWords[j].Location := Point(xx, yy);
         k := 0;
         ww := 0;
       end;
     until fullWord = '';
     xx := FWords[j].Width + spaceWidth;
     inc(j);
   end;

   function CurrentLineIsEmpty: boolean;
   begin
    Result := xx = 0;
   end;

begin
  FLineCount := 0;
  DrawingSize.cx := 0;
  DrawingSize.cy := 0;
  DrawingRect.Width := 0;
  DrawingRect.Height := 0;
  FWords := NIL;
  FReadyToRender := (FCaption <> '') and (FTexturedFont <> NIL);
  if not FReadyToRender then exit;

  W := SplitTextToWordArray(FCaption, ' ');
  // compute the number of word except #10
  xx := 0;
  for j:=0 to High(W) do if W[j] <> #10 then inc(xx);
  SetLength(FWords, xx);

  spaceWidth := FTexturedFont.GetCharWidth(' ');

  xx := 0;
  yy := 0;
  FLineCount := 1;
  iw := 0;
  j := 0;
  flagLastWasLineEnding := False;
  repeat

    if {FWords[j].Word}W[iw] = #10 then begin
      //if CurrentLineIsEmpty then dec(FLineCount) else flagLastWasLineEnding := True;
      //if CurrentLineIsEmpty then dec(FLineCount);
      flagLastWasLineEnding := True;
      _NextLine;
      inc(iw);
    end else begin
      FWords[j].Word := W[iw];
      FWords[j].CharIndexes := FTexturedFont.StringToCharIndexes(W[iw]);
      FWords[j].Width := FTexturedFont.GetTextWidth(FWords[j].Word);

      if xx+FWords[j].Width <= Area.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(iw);
      flagLastWasLineEnding := False;
    end else begin
         flagLastWasLineEnding := False;
         // the word don't fit
         if CurrentLineIsEmpty then begin
           DoWordWrapOnCurrentLine;
           inc(iw);
         end else begin
           _NextLine;
           DoWordWrapOnCurrentLine;
           inc(iw);
         end;
    end;
    end;
    if xx > DrawingSize.cx then DrawingSize.cx := xx;
  until iw = Length(W);

  if flagLastWasLineEnding then dec(FLineCount);

  DrawingSize.cy := FTexturedFont.Font.FontHeight*FLineCount;
end;

function TAlignedTextDescriptor.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TAlignedTextDescriptor.SetAlign(AValue: TOGLCAlignment);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
  DoAlign;
end;

procedure TAlignedTextDescriptor.SetCaption(AValue: string);
begin
  AValue := AdjustLineEnding(AValue);
  if FCaption = AValue then exit;
  FCaption := AValue;
  DistributeTheWordsOnTheSurface;
  DoAlign;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged;
end;

procedure TAlignedTextDescriptor.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont = AValue then exit;
  FTexturedFont := AValue;
  FSpaceCharWidth := FTexturedFont.GetCharWidth(' ');
  DistributeTheWordsOnTheSurface;
  DoAlign;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged;
end;

function TAlignedTextDescriptor.GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
var i: integer;
begin
  iBegin := -1;
  iEnd := -1;
  for i:=low(FWords) to high(FWords) do begin
    if FWords[i].LineIndex = aLineIndex then begin
      Result := True;
      if iBegin = -1 then iBegin := i;
      iEnd := i;
    end else if iBegin <> -1 then break;
   end;
  Result := (iBegin <> -1) and (iEnd <> -1);
end;

procedure TAlignedTextDescriptor.DoVTopAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  DrawingRect.Top := 0;
  yy := 0;
  yDelta := FTexturedFont.Font.FontHeight;
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy := yy + yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoVCenterAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  if DrawingSize.cy = 0 then exit;
  yy := (Area.Height - DrawingSize.cy) div 2;
  yDelta := FTexturedFont.Font.FontHeight;
  DrawingRect.Top := round(yy);
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy := yy + yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoVBottomAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  yy := Area.Height - FTexturedFont.Font.FontHeight;
  yDelta := FTexturedFont.Font.FontHeight;
  DrawingRect.Top := Area.Height - DrawingSize.cy;
  for i:=FLineCount-1 downto 0 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy := yy - yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoHLeftAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  DrawingRect.Left:=0;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
        FWords[j].Location.x := w;
        w := w + FWords[j].Width + FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoHCenterAlign;
var i, j, iBegin, iEnd: integer;
  w: single;
begin
  DrawingRect.Left:=Area.Width;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
       w+= FWords[j].Width;
       if i <> iEnd then w += FSpaceCharWidth;
      end;

      w := (Area.Width - w)*0.5;

      if DrawingRect.Left > w then DrawingRect.Left := Round(w);

      for j:=iBegin to iEnd do begin
        FWords[j].Location.x := Round(w);
        w := w + FWords[j].Width + FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoHRightAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  DrawingRect.Left := Area.Width;

  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := Area.Width;
      for j:=iEnd downto iBegin do begin
        w := w - FWords[j].Width;
        FWords[j].Location.x := w;
        if DrawingRect.Left > w then DrawingRect.Left := w;
        w := w - FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoAlign;
var adjustedAlign: TOGLCAlignment;
begin
  if (FTexturedFont = NIL) or (FLineCount = 0) then exit;

  adjustedAlign := FAlign;
  // vertical center/bottom becomes vertical top if the vertical size of the text don't fit in the area
  if DrawingSize.cy > Area.Height then begin
    case FAlign of
      taCenterLeft, taBottomLeft: adjustedAlign := taTopLeft;
      taCenterCenter, taBottomCenter: adjustedAlign := taTopCenter;
      taCenterRight, taBottomRight: adjustedAlign := taTopRight;
    end;
  end;

  case adjustedAlign of
   taTopLeft, taCenterLeft, taBottomLeft: DoHLeftAlign;
   taTopRight, taCenterRight, taBottomRight: DoHRightAlign;
   taTopCenter, taCenterCenter, taBottomCenter: DoHCenterAlign;
  end;

  case adjustedAlign of
   taTopLeft, taTopCenter, taTopRight: DoVTopAlign;
   taCenterLeft, taCenterCenter, taCenterRight: DoVCenterAlign;
   taBottomLeft, taBottomCenter, taBottomRight: DoVBottomAlign;
  end;

  DrawingRect.Bottom := DrawingRect.Top + DrawingSize.cy;
  DrawingRect.Right := DrawingRect.Left + DrawingSize.cx;
end;

{ TUITextArea }

procedure TUITextArea.DoOnMouseWheel(aShift: TShiftState; aWheelDelta: Integer;
  aMousePos: TPoint; var aHandled: Boolean);
begin
  inherited DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
  if FVScrollBar.Visible then FVScrollBar.DoOnMouseWheel(aShift, aWheelDelta, aMousePos, aHandled);
  aHandled := True;
end;

procedure TUITextArea.ProcessCallbackSomethingChangeTheSize;
begin
  FNeedFormatText := True;
  UpdateWidgetSizeAndLayout;
end;

procedure TUITextArea.ProcessEventScrollBarChange(aSurface: TSimpleSurfaceWithEffect);
var fHeight: integer;
begin
  fHeight := Text.TexturedFont.Font.FontHeight;
  FIndexFirstLineVisible := FVScrollBar.Position div fHeight;
  FYFirstLineVisible := -FVScrollBar.Position mod fHeight;
end;

procedure TUITextArea.UpdateWidgetSizeAndLayout;
var needScrollBar: boolean;
  maxi, page: LongInt;
  r: TRect;
begin
  r := ClientArea;
  ComputeClientArea;

  if FNeedFormatText or (r <> ClientArea) then begin
    r := ClientArea;
    if (FVScrollBar <> NIL) and FVScrollBar.Visible then r.Width := r.Width - FVScrollBar.Width;
    if FNeedFormatText or (Text.Area <> r) then begin
      Text.Area := r;
      Text.DistributeTheWordsOnTheSurface;
      Text.DoAlign;
    end;
    FNeedFormatText := False;
  end;

  // update VScrollBar visibility
  needScrollBar := (VScrollBarMode <> sbmNeverShow) and
                   (ClientArea.Height > 0) and
                   (Text.DrawingRect.Height > ClientArea.Height);
  if needScrollBar then begin
    maxi := Text.DrawingRect.Height;
    page := ClientArea.Height;
    UpdateVScrollBar(needScrollBar, maxi, page);

    Text.Area := ClientArea;
    Text.Area.Width := Text.Area.Width - FVScrollBar.Width;
    //FIndexFirstLineVisible := FVScrollBar.Position div Text.TexturedFont.Font.FontHeight;
    //FYFirstLineVisible := -FVScrollBar.Position mod Text.TexturedFont.Font.FontHeight;
  end else begin
    UpdateVScrollBar(needScrollBar, 0, 0);
    FIndexFirstLineVisible := 0;
    FYFirstLineVisible := 0;

    Text.Area := ClientArea;
  end;
  Text.DistributeTheWordsOnTheSurface;
  Text.DoAlign;
end;

procedure TUITextArea.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if not FFreeze then Text.Tint.OnElapse(aElapsedTime);
end;

procedure TUITextArea.DoDraw;
var i: integer;
  computedTint: TColorF;
  yy, yOffset, minYVisible: integer;
  m: TOGLCMatrix;
begin
  if not Text.ReadyToRender then exit;
  m.CopyFrom(FParentScene.ModelViewMatrix);
  FParentScene.ModelViewMatrix.Translate(ClientArea.Left, ClientArea.Top);

  computedTint.InitFromTint(TintMode, Text.Tint.Value);
  if (FVScrollBar <> NIL) and (FVScrollBar.Visible) then yOffset := FVScrollBar.Position
    else yOffset := 0;

  minYVisible := ClientArea.Top - Text.FTexturedFont.Font.FontHeight+1;

  with Text do
    for i:=0 to High(FWords) do
      if FWords[i].LineIndex >= FIndexFirstLineVisible then begin
        yy := FWords[i].Location.y - yOffset;
        if InRange(yy, minYVisible, ClientArea.Bottom) then
          FTexturedFont.DrawChars(FWords[i].CharIndexes, FWords[i].Location.x, yy, 0, NIL, NIL,
                                   FComputedOpacity, computedTint, FBlendMode);
      end;

  FParentScene.ModelViewMatrix.CopyFrom(m);
end;

constructor TUITextArea.Create(aParentScene: TOGLCScene);
begin
  inherited Create(aParentScene, True, False);
  VScrollBarMode := sbmAuto;
  HScrollBarMode := sbmNeverShow;
  Text.InitDefault;
  Text.OnSizeChanged := @ProcessCallbackSomethingChangeTheSize;
end;

destructor TUITextArea.Destroy;
begin
  Text.FreeObjects;
  inherited Destroy;
end;

procedure TUITextArea.MakeLastLineVisible;
begin
  if FVScrollBar = NIL then exit;
  if FVScrollBar.Visible then FVScrollBar.Position := FVScrollBar.Max;
end;

{$endif oglcIMPLEMENTATION}


