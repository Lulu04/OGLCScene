unit frame_viewlayerlist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Types,
  Graphics,
  u_layerlist;

type

  { TFrameViewLayerList }

  TFrameViewLayerList = class(TFrame)
    CBLoopMode: TComboBox;
    LB: TListBox;
    Panel1: TPanel;
    procedure CBLoopModeCloseUp(Sender: TObject);
    procedure CBLoopModeSelect(Sender: TObject);
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBMouseLeave(Sender: TObject);
    procedure LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FMouseCanMoveItem: boolean;
    FMouseOrigin: TPoint;
    FLeftClickedIndex: integer;
    FItemIndexUnderMouse: integer;
    FShowDecorLoopMode: boolean;
    FShowIconEye: boolean;
    FTextStyleForTextRect: TTextStyle;
    FTextWidth_Used,
    FTextWidth_WorldSize: integer;
    function GetCount: integer;
    function GetNames(index: integer): string;
    function NameExists(const aName: string): boolean;
    function GetSelectedCount: integer;
    procedure SetNames(index: integer; AValue: string);
    procedure SetShowDecorLoopMode(AValue: boolean);
    procedure SetShowIconEye(AValue: boolean);
  private // LB item utils
    function GetUsedLabelArea: TRect;
    function GetLoopModeArea: TRect;
    function XIsInLoopModeArea(aX: integer): boolean;
  private
    FLoopModes: PArrayOfDecorLoopMode;
    FOnDecorLoopModeChange: TNotifyEvent;
    procedure ShowCBLoopMode;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure Fill;
    function GetSelectedIndex: integer;
    function GetNewDefaultLayerName: string;
    // return True if a new layer is added
    function AddLayer: boolean;
    // return True if the deletion is done
    function DeleteSelected: boolean;
    // return true if the item has been shifted
    function MoveSelected(aUp: boolean): boolean;
    // return True if the item is renamed
    function RenameSelected: boolean;

    procedure SaveLayerConfigToLayerList;

    property Count: integer read GetCount;
    property Names[index:integer]: string read GetNames write SetNames;
    property SelectedCount: integer read GetSelectedCount;
    // default is true
    property ShowIconEye: boolean read FShowIconEye write SetShowIconEye;
    // default is true
    property ShowDecorLoopMode: boolean read FShowDecorLoopMode write SetShowDecorLoopMode;

    property LoopModes: PArrayOfDecorLoopMode read FLoopModes write FLoopModes;
    property OnDecorLoopModeChange: TNotifyEvent read FOnDecorLoopModeChange write FOnDecorLoopModeChange;
  end;

implementation
uses LCLType, LCLHelper, u_project, u_utils, u_datamodule,
  u_common, u_levelbank, Math, OGLCScene, Dialogs;

{$R *.lfm}

{ TFrameViewLayerList }

procedure TFrameViewLayerList.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var p: TPoint;
 i, xx, xx1, yy, yy1: integer;
begin
  Control := Control;

  with LB.Canvas do
  begin
    Brush.Style := bsSolid;
    if State >= [odSelected] then begin
      Brush.Color := RGBToColor(94,128,130);
      Font.Color := clWhite;
    end else begin
      if Index and 1 = 0 then Brush.Color := LB.Color
        else Brush.Color := u_utils.PercentColor(LB.Color, 0.15);
      Font.Color := clWhite;
    end;
    // render dot rectangle if mouse is over item
    if Index = FItemIndexUnderMouse then begin
      Pen.Style := psDot;
      Pen.Color := clHighLight; // PercentColor(LB.Color, 0.5);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end else FillRect(ARect);

    // render drag target position
    if FLeftClickedIndex <> -1 then begin
      p := LB.ScreenToClient(Mouse.CursorPos);
      p.y := EnsureRange(p.y, 0, LB.ClientHeight);
      i := LB.GetIndexAtY(p.y);
      if i = Index then begin
        Pen.Style := psClear;
        Brush.Color := clHighLight;
        if i > FLeftClickedIndex then
          Rectangle(ARect.Left-1, ARect.Bottom-3, ARect.Right+1, ARect.Bottom)
        else
          Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Top+3);
      end;
    end;
    Brush.Style := bsClear;

    // render layer name
    xx := ARect.Left;
    if FShowIconEye then xx1 := ARect.Right-DataModule1.ILIconLayerList.Width-PPIScale(5)-
                                FTextWidth_Used-PPIScale(5)-FTextWidth_WorldSize-PPIScale(5)
      else xx1 := ARect.Right;
    TextRect(Rect(xx, ARect.Top, xx1, ARect.Bottom),
             0, 0, Index.ToString+'-'+LB.Items.Strings[Index]{Layers.Names[Index]}, FTextStyleForTextRect);

    // render if layer is used
    i := FScene.Layer[Index+APP_LAYER_COUNT].SurfaceCount;
    if i <> 0 then begin
      xx := ARect.Right - DataModule1.ILIconLayerList.Width - PPIScale(5) - FTextWidth_Used;
      TextOut(xx, ARect.Top+(ARect.Height-TextHeight('used')) div 2, 'used');
    end;

    // render the decor loop mode
    if FShowDecorLoopMode and (FLoopModes <> NIL) then begin
      Brush.Style := bsClear;
      Pen.Style := psSolid;
      xx1 := xx;
      xx := xx - PPIScale(5) - FTextWidth_WorldSize;
      yy1 := ARect.Top + ARect.Height div 2;
      yy := yy1 - TextHeight('T');
      case FLoopModes^[Index] of
        dlmNone: TextOut(xx, ARect.Top, 'no loop');
        dlmRepeatOnDecorSize: begin
          TextOut(xx, yy, 'repeat on');
          TextOut(xx, yy1, 'decor size');
        end;
        dlmRepeatOnWorldSize: begin
          TextOut(xx, yy, 'repeat on');
          TextOut(xx, yy1, 'world size');
        end;
      end;
    end;

    // render icon visible or not
    if FShowIconEye then begin
      xx := ARect.Right-DataModule1.ILIconLayerList.Width;
      yy := ARect.Top + (ARect.Height-DataModule1.ILIconLayerList.Height) div 2;
      if Layers.UserLayerIsVisible(Index) then DataModule1.ILIconLayerList.Draw(LB.Canvas, xx, yy, 0)
        else DataModule1.ILIconLayerList.Draw(LB.Canvas, xx, yy, 1);
    end;
  end;
end;

procedure TFrameViewLayerList.CBLoopModeSelect(Sender: TObject);
var i: integer;
begin
  CBLoopMode.Visible := False;
  if CBLoopMode.ItemIndex = -1 then exit;

  i := LB.ItemIndex;
  if i = -1 then exit;

  FLoopModes^[i] := TOGLCDecorLoopMode(CBLoopMode.ItemIndex);
  LB.Invalidate;

  if Assigned(FOnDecorLoopModeChange) then
    FOnDecorLoopModeChange(Self);
end;

procedure TFrameViewLayerList.CBLoopModeCloseUp(Sender: TObject);
begin
  CBLoopMode.Visible := False;
end;

procedure TFrameViewLayerList.LBKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Shift := Shift;

  if (Key = VK_SPACE) or
     (Key = VK_UP) or
     (Key = VK_DOWN) or
     (Key = VK_Left) or
     (Key = VK_Right) then Key := VK_UNKNOWN;
end;

procedure TFrameViewLayerList.LBMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  Shift := Shift;

  i := LB.GetIndexAtY(Y);

  if Button = mbLeft then begin
    if i = -1 then LB.ItemIndex := -1 // click on empty area = unselect all
    else begin
      // check for drag
      if (X < LB.ClientWidth-DataModule1.ILIconLayerList.Width) and
         FMouseCanMoveItem then begin
           FLeftClickedIndex := i;
           FMouseOrigin := LB.ClientToScreen(Point(X,Y));
      end else FLeftClickedIndex := -1;
    end;
  end;
end;

procedure TFrameViewLayerList.LBMouseLeave(Sender: TObject);
begin
  if FLeftClickedIndex <> -1 then exit;

  FItemIndexUnderMouse := -1;
  LB.Cursor := crDefault;
  LB.Invalidate;
end;

procedure TFrameViewLayerList.LBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  Shift := Shift;
  X := X;

  // dragging an item
  if (FLeftClickedIndex <> -1) and (LB.ItemIndex <> FLeftClickedIndex) then begin
    LB.ItemIndex := FLeftClickedIndex;
    LB.Invalidate;
    exit;
  end;

  // check if the mouse is over an item
  i := LB.GetIndexAtY(Y);
  if i <> FItemIndexUnderMouse then begin
    if i = -1 then
      LB.Cursor := crDefault
    else
      LB.Cursor := crHandPoint;
    FItemIndexUnderMouse := i;
    LB.Invalidate;
  end;
end;

procedure TFrameViewLayerList.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
 flagCanMove: boolean;
begin
  Shift := Shift;
  if Button = mbLeft then begin
    if FLeftClickedIndex <> -1 then begin
      Y := EnsureRange(Y, 0, LB.ClientHeight);
      i := LB.GetIndexAtY(Y);
      // user can moves item only if one is selected
      flagCanMove := (i <> -1) and (i <> FLeftClickedIndex);

      if flagCanMove then begin
        LB.MoveSelection(i-FLeftClickedIndex);
        Project.SetModified;
      end;
      FLeftClickedIndex := -1;
      LB.Invalidate;
    end else begin
      // click on eye icon
      if FShowIconEye and
         (X >= LB.ClientWidth-DataModule1.ILIconLayerList.Width) then begin
        i := LB.GetIndexAtXY(X, Y);
        if i <> -1 then begin
          Layers.SetUserLayerVisible(i, not Layers.UserLayerIsVisible(i));
          LB.Invalidate;
        end;
      end
      else
      // click on loop mode area       Round(DataModule1.ILIconLayerList.Width*1.3)
      if FShowDecorLoopMode and XIsInLoopModeArea(X) then begin
        ShowCBLoopMode;
      end;
    end;
  end;
end;

function TFrameViewLayerList.GetSelectedCount: integer;
begin
  Result := LB.SelCount;
end;

procedure TFrameViewLayerList.SetNames(index: integer; AValue: string);
begin
  LB.Items.Strings[index] := AValue;
end;

procedure TFrameViewLayerList.SetShowDecorLoopMode(AValue: boolean);
begin
  if FShowDecorLoopMode=AValue then Exit;
  FShowDecorLoopMode:=AValue;
  LB.Invalidate;
end;

function TFrameViewLayerList.GetCount: integer;
begin
  Result := LB.Count;
end;

function TFrameViewLayerList.GetNames(index: integer): string;
begin
  Result := LB.Items.Strings[index];
end;

function TFrameViewLayerList.NameExists(const aName: string): boolean;
var i: integer;
begin
  for i:=0 to LB.Count-1 do
    if LB.Items.Strings[i] = aName then exit(True);
  Result := False;
end;

procedure TFrameViewLayerList.SetShowIconEye(AValue: boolean);
begin
  if FShowIconEye = AValue then Exit;
  FShowIconEye := AValue;

  if FShowIconEye then LB.ItemHeight := ScaleDesignToForm(35)
    else LB.ItemHeight := ScaleDesignToForm(20);

  LB.Invalidate;
end;

function TFrameViewLayerList.GetUsedLabelArea: TRect;
begin
  Result.Right := LB.ClientWidth - DataModule1.ILIconLayerList.Width - PPIScale(5);
  Result.Left := Result.Right - FTextWidth_Used;
end;

function TFrameViewLayerList.GetLoopModeArea: TRect;
begin
  Result.Right := GetUsedLabelArea.Left - PPIScale(5);
  Result.Left := Result.Right - FTextWidth_WorldSize;
end;

function TFrameViewLayerList.XIsInLoopModeArea(aX: integer): boolean;
var r: TRect;
begin
  r := GetLoopModeArea;
  Result := InRange(aX, r.Left, r.Right);
end;

procedure TFrameViewLayerList.ShowCBLoopMode;
var i: integer;
  r: TRect;
  p: TPoint;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;

  r := LB.ItemRect(i);
  p.x := GetLoopModeArea.Left;
  p.y := r.Top;
  p := LB.ClientToParent(p);
  CBLoopMode.Left := p.x;
  CBLoopMode.Top := p.y;
  CBLoopMode.Visible := True;
  CBLoopMode.ItemIndex := Ord(FLoopModes^[i]);
  CBLoopMode.DroppedDown := True;
end;

constructor TFrameViewLayerList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItemIndexUnderMouse := -1;
  FLeftClickedIndex := -1;
  LB.ItemHeight := ScaleDesignToForm(35);

  FShowIconEye := True;
  FShowDecorLoopMode := True;

  FTextStyleForTextRect.Alignment := taLeftJustify;
  FTextStyleForTextRect.Layout := Graphics.tlCenter;
  FTextStyleForTextRect.SingleLine := FALSE;
  FTextStyleForTextRect.Wordbreak := TRUE;
  FTextStyleForTextRect.Clipping := TRUE;
  FTextStyleForTextRect.Opaque := FALSE;
  FTextStyleForTextRect.SystemFont := FALSE;
end;

procedure TFrameViewLayerList.Fill;
begin
  FTextWidth_Used := LB.Canvas.TextWidth('used');
  FTextWidth_WorldSize := LB.Canvas.TextWidth('world size');

  Layers.FillListBox(LB);
  LB.Invalidate;
end;

function TFrameViewLayerList.GetSelectedIndex: integer;
begin
  Result := LB.ItemIndex;
end;

function TFrameViewLayerList.GetNewDefaultLayerName: string;
var i: integer;
begin
  i := Count;
  repeat
    Result := 'LAYER_'+i.ToString;
  until not NameExists(Result);
end;

function TFrameViewLayerList.AddLayer: boolean;
var layerName: String;
begin
  Result := False;
  layerName := Trim(InputBox('', 'Enter a name for the new layer:', GetNewDefaultLayerName));
  if Layers.UserLayerNameExists(layerName) then begin
    ShowMessage('There is a layer named '+layerName+LineEnding+'Please try with another name');
    exit;
  end;
  if not IsValidPascalVariableName(layerName, True) then
    exit;

  // add in layer list
  Layers.Add(layerName);
  // add in listbox
  LB.Items.Add(layerName);
  Result := True;
end;

function TFrameViewLayerList.DeleteSelected: boolean;
var i: integer;
begin
  Result := False;
  i := LB.ItemIndex;
  if i = -1 then exit;

  if LevelBank.UseThisLayer(i) then begin
    ShowMessage('This layer can''t be deleted because one or several levels use it.'+LineEnding+
                'Operation canceled');
    exit;
  end;
  // delete in the layer list
  Layers.Delete(i);
  // delete in the listbox
  LB.Items.Delete(i);
  // adjust the layer indexes in all levels
  LevelBank.DecreaseLayerIndexGreaterOrEqualThan(i);

  Result := True;
end;

function TFrameViewLayerList.MoveSelected(aUp: boolean): boolean;
var
  i: Integer;
begin
  Result := False;
  i := LB.ItemIndex;
  if i = -1 then exit;

  if aUp then begin
    // up
    if i = 0 then exit;
    // move in listbox
    LB.MoveSelectionUp;
    // move in layer list
    Layers.Exchange(i, i-1);
    // modify the layer index in LevelBank
    LevelBank.ExchangeLayerIndexInAllSurfaces(i, i-1);
    Result := True;
  end else begin
    // down
    if i = LB.Count-1 then exit;
    LB.MoveSelectionDown;
    Layers.Exchange(i, i+1);
    LevelBank.ExchangeLayerIndexInAllSurfaces(i, i+1);
    Result := True;
  end;
end;

function TFrameViewLayerList.RenameSelected: boolean;
var i: integer;
  oldName, newName: String;
begin
  Result := False;
  i := LB.ItemIndex;
  if i = -1 then exit;

  oldName := Names[i];
  newName := Trim(InputBox('', 'Enter the new name:', oldName));
  if (newName = oldName) or (newName = '') then exit;
  if Layers.UserLayerNameExists(newName) then begin
    ShowMessage('A layer named '''+newName+''' already exists'+LineEnding+'Please try with another name');
    exit;
  end;
  if not IsValidPascalVariableName(newName, True) then
    exit;

  // rename in the listbox
  Names[i] := newName;
  // rename in layer list
  Layers.Names[i] := newName;
  Result := True;
end;

procedure TFrameViewLayerList.SaveLayerConfigToLayerList;
begin
  Layers.InitWith(LB.Items.ToStringArray);
end;

end.

