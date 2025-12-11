unit frame_viewfontbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Dialogs,
  Buttons, Spin, Graphics,
  OGLCScene, u_ui_objectlist, Types, BGRABitmap, BGRABitmapTypes;

type

  { TFrameViewFontBank }

  TFrameViewFontBank = class(TFrame)
    BCancel: TSpeedButton;
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BRename: TSpeedButton;
    BSwapOutlineAndShadowColors: TSpeedButton;
    CBOutline: TCheckBox;
    CBShadow: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckGroup1: TCheckGroup;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    CBGradientType: TComboBox;
    Edit1: TEdit;
    FD1: TFontDialog;
    FSE1: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBFont: TListBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    Panel8: TPanel;
    PanelFont: TPanel;
    RBSingleColor: TRadioButton;
    RBGradient: TRadioButton;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    SE3: TSpinEdit;
    SE4: TSpinEdit;
    BSave: TSpeedButton;
    SE7: TSpinEdit;
    SE8: TSpinEdit;
    SE5: TSpinEdit;
    SE6: TSpinEdit;
    BSwapGradientColors: TSpeedButton;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure BCancelClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BSwapGradientColorsClick(Sender: TObject);
    procedure BSwapOutlineAndShadowColorsClick(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure LBFontDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure LBFontSelectionChange(Sender: TObject; User: boolean);
    procedure SE1Change(Sender: TObject);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TVResize(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FIsEditable: boolean;
    FModified: boolean;
    FInitializing: boolean;
    FOnSelectionChange: TNotifyEvent;
    function WidgetToFontDescriptor: TFontDescriptor;
    procedure FontDescriptorToWidget(const fd: TFontDescriptor);
    procedure UpdatePreview;
    function GetCharset: string;
    function GetFillTexture: TBGRABitmap;
    function WidgetToFontStyle: TFontStyles;
    function CBToGradientType: TGradientType;
    procedure GradientTypeToCB(aGradientType: TGradientType);
    procedure FontStyleToWidget(aFt: TFontStyles);
    function GetGradientOrigin: TPointF;
    function GetGradientD1: TPointF;
    procedure HideToolPanels;
    procedure ShowToolPanels;
    function NodeIsRoot(aNode: TTreeNode): boolean;
    function NodeIsFont(aNode: TTreeNode): boolean;
    function SelectedIsRoot: boolean;
    function SelectedIsFont: boolean;
    procedure DoRenameSelected;
    procedure DoDuplicateSelected;
    procedure DoDeleteSelected;
  public
    procedure Fill;

    property IsEditable: boolean read FIsEditable write FIsEditable;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses u_resourcestring, u_utils, form_main, u_screen_fontbank, u_common,
  LCLType;

{$R *.lfm}

{ TFrameViewFontBank }

procedure TFrameViewFontBank.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('',sIfYouLeaveChangeWillBeLost, mtWarning,
                   [mrOk, sLeaveWithoutSaving, mrCancel, sCancel], 0) = mrCancel then exit;
  FormMain.ShowPageLevelBank;
end;

procedure TFrameViewFontBank.BRenameClick(Sender: TObject);
begin
  if Sender = BRename then
    DoRenameSelected;

  if Sender = BDuplicate then
    DoDuplicateSelected;

  if Sender = BDelete then
    DoDeleteSelected;
end;

procedure TFrameViewFontBank.BSaveClick(Sender: TObject);
var nam: string;
  item: TFontDescriptorItem;
begin
  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if SelectedIsFont and (TV.Selected.Text = nam) then begin
    if QuestionDlg('', sOverwriteTheSelectedFont, mtWarning,
                   [mrOk, sOverwrite, mrCancel, sCancel], 0) <> mrOk then exit;
    // modify the selected item
    item := FontBank.GetByName(nam);
    item.FD := WidgetToFontDescriptor;
    FontBank.Save;
    exit;
  end;

  // add a new item in the bank
  item := FontBank.AddEmpty;
  item._Name := nam;
  item.FD := WidgetToFontDescriptor;
  FontBank.Save;

  // add the font to the treeview
  with TV.Items.AddChild(TV.Items.GetFirstNode, nam) do begin
    ImageIndex := 11;
    SelectedIndex := 11;
    MakeVisible;
  end;
end;

procedure TFrameViewFontBank.BSwapGradientColorsClick(Sender: TObject);
var c: TColor;
  alpha: integer;
begin
  FInitializing := True;
  c := ColorButton1.ButtonColor;
  alpha := SE5.Value;

  ColorButton1.ButtonColor := ColorButton2.ButtonColor;
  SE5.Value := SE6.Value;

  ColorButton2.ButtonColor := c;
  SE6.Value := alpha;
  FInitializing := False;
  UpdatePreview;
end;

procedure TFrameViewFontBank.BSwapOutlineAndShadowColorsClick(Sender: TObject);
var c: TColor;
  alpha: integer;
begin
  FInitializing := True;
  c := ColorButton3.ButtonColor;
  alpha := SE7.Value;

  ColorButton3.ButtonColor := ColorButton4.ButtonColor;
  SE7.Value := SE8.Value;

  ColorButton4.ButtonColor := c;
  SE8.Value := alpha;
  FInitializing := False;
  UpdatePreview;
end;

procedure TFrameViewFontBank.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
begin
  Index := Index;
  if not FInitializing then
    UpdatePreview;
end;

procedure TFrameViewFontBank.LBFontDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  Control := Control;
  with LBFont.Canvas do begin
    Brush.Style := bsSolid;
    if State >= [odSelected] then begin
      Brush.Color := RGBToColor(94,128,130);
      Font.Color := clWhite;
    end else begin
      if Index and 1 = 0 then Brush.Color := LBFont.Color
        else Brush.Color := u_utils.PercentColor(LBFont.Color, 0.15);
      Font.Color := clWhite;
    end;
    // render dot rectangle if mouse is over item
   { if Index = FItemIndexUnderMouse then begin
      Pen.Style := psDot;
      Pen.Color := clHighLight; // PercentColor(LB.Color, 0.5);
      Rectangle(ARect.Left-1, ARect.Top, ARect.Right+1, ARect.Bottom);
    end else} FillRect(ARect);

    Brush.style := bsClear;
//    LBFont.Font.Name := LBFont.Items.Strings[Index];
    TextOut(ARect.Left+3, ARect.Top, LBFont.Items.Strings[Index]);
//    LBFont.Font.Name := 'default';
  end;
end;

procedure TFrameViewFontBank.LBFontSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  User := User;
  if FInitializing then exit;

  i := LBFont.ItemIndex;
  if i <> -1 then Label15.Caption := LBFont.Items.Strings[i]
    else if Label15.Caption = '' then Label15.Caption := 'Arial';
  SE1Change(Self);
end;

procedure TFrameViewFontBank.SE1Change(Sender: TObject);
begin
  ColorButton2.Enabled := RBGradient.Checked;
  CBGradientType.Enabled := RBGradient.Checked;
  CheckBox1.Enabled := RBGradient.Checked;
  CheckBox2.Enabled := RBGradient.Checked;
  BSwapGradientColors.Enabled := RBGradient.Checked;

  Panel2.Enabled := CBOutline.Checked;
  Panel3.Enabled := CBShadow.Checked;

  BSwapOutlineAndShadowColors.Enabled := CBOutline.Checked and CBShadow.Checked;


  if not FInitializing then
    UpdatePreview;
end;

procedure TFrameViewFontBank.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var r: TRect;
  p: TPoint;
  ft: TFontStyles;
begin
  Sender := Sender;
  if (Stage = cdPostPaint) then begin // all is painted
    ft := TV.Font.Style;
    r := Node.DisplayRect(True);
    With TV.Canvas do begin
      // background
      if cdsSelected in State then begin
        Brush.Color := TV.SelectionColor;
        TV.Font.Color := clWhite
      end else begin
        Brush.Color := TV.BackgroundColor;
        TV.Font.Color := RGBToColor(220,220,220);
      end;

      Brush.Style := bsSolid;
      Pen.Style := psClear;
      FillRect(r);
      //Pen.Style := psSolid;

      // hot track (mouse is over)
      p := TV.ScreenToClient(Mouse.CursorPos);
      if TV.GetNodeAt(p.x, p.y) = Node then
        TV.Font.Style := TV.Font.Style+[fsUnderline];

      // text
      Brush.Style := bsClear;
      if Node.Level = 1 then
        TV.Font.Style := TV.Font.Style+[fsBold];
      TextOut(r.Left+2, r.Top+(r.Height-TV.Font.Height) div 2, Node.Text);
      TV.Font.Style := ft;
    end;
    PaintImages := True;
    DefaultDraw := false;
  end else begin
    PaintImages := True;
    DefaultDraw := True;
  end;
end;

procedure TFrameViewFontBank.TVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  Sender := Sender;
  Shift := Shift;
  n := TV.GetNodeAt(X, Y);
  if n = NIL then
    TV.Selected := NIL;

  if (Button = mbLeft) and (TV.Selected <> NIL) then
    ShowToolPanels;
end;

procedure TFrameViewFontBank.TVMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Sender := Sender;
  Shift := Shift;
  WheelDelta := WheelDelta;
  MousePos := MousePos;
  Handled := Handled;
  HideToolPanels;
end;

procedure TFrameViewFontBank.TVResize(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
end;

procedure TFrameViewFontBank.TVSelectionChanged(Sender: TObject);
var item: TFontDescriptorItem;
begin
  Sender := Sender;
  HideToolPanels;

  if SelectedIsFont then begin
    item := FontBank.GetByName(TV.Selected.Text);
    if item = NIL then exit;

    FontDescriptorToWidget(item.FD);
    UpdatePreview;
    Edit1.Text := item._Name;
  end else begin
    ScreenFontBank.ClearView;
    Edit1.Text := '';
  end;

  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);

  ShowToolPanels;
end;

function TFrameViewFontBank.WidgetToFontDescriptor: TFontDescriptor;
var gradient: TFontGradient;
  fontColor1, fontColor2, outlineColor, shadowColor: TBGRAPixel;
  OutlineWidth: single;
  OffsetX, OffsetY, Radius: integer;
begin
  fontColor1 := ColorToBGRA(ColorButton1.ButtonColor, SE5.Value);
  fontColor2 :=  ColorToBGRA(ColorButton2.ButtonColor, SE6.Value);

  if CBOutline.Checked then begin
    outlineColor := ColorToBGRA(ColorButton3.ButtonColor, SE7.Value);
    OutlineWidth := FSE1.Value;
  end else begin
    outlineColor := BGRAPixelTransparent;
    OutlineWidth := 0.0;
  end;

  if CBShadow.Checked then begin
    shadowColor := ColorToBGRA(ColorButton4.ButtonColor, SE8.Value);
    OffsetX := SE2.Value;
    OffsetY := SE3.Value;
    Radius := SE4.Value;
  end else begin
    shadowColor := BGRAPixelTransparent;
    OffsetX := 0;
    OffsetY := 0;
    Radius := 0;
  end;

  if Label15.Caption = '' then Label15.Caption := 'Arial';

  if RBGradient.Checked then begin
    gradient.Create(fontColor1, fontColor2, CBToGradientType,
                    GetGradientOrigin, GetGradientD1, CheckBox1.Checked, CheckBox2.Checked);
    Result.Create(Label15.Caption, SE1.Value, WidgetToFontStyle, gradient,
                  outlineColor, OutlineWidth,
                  ShadowColor, OffsetX, OffsetY, Radius);
  end else Result.Create(Label15.Caption, SE1.Value, WidgetToFontStyle, fontColor1,
                         OutlineColor, OutlineWidth,
                         ShadowColor, OffsetX, OffsetY, Radius);
end;

procedure TFrameViewFontBank.FontDescriptorToWidget(const fd: TFontDescriptor);
begin
  FInitializing := True;
  LBFont.ItemIndex := LBFont.Items.IndexOf(fd.FontName);
  Label15.Caption := fd.FontName;
  SE1.Value := fd.FontHeight;
  FontStyleToWidget(fd.Style);

  if fd.UseGradient then begin
    RBGradient.Checked := True;
    ColorButton1.ButtonColor := fd.Gradient.Color1.ToColor;
    SE5.Value := fd.Gradient.Color1.alpha;
    ColorButton2.ButtonColor := fd.Gradient.Color2.ToColor;
    SE6.Value := fd.Gradient.Color2.alpha;
    GradientTypeToCB(fd.Gradient.GradientType);
    CheckBox1.Checked := fd.Gradient.GammaColorCorrection;
    CheckBox2.Checked := fd.Gradient.Sinus;
  end else begin
    RBSingleColor.Checked := True;
    ColorButton1.ButtonColor := fd.FontColor.ToColor;
    SE5.Value := fd.FontColor.alpha;
  end;

  CBOutline.Checked := fd.UseOutLine;
  if CBOutline.Checked then begin
    FSE1.Value := fd.OutLineWidth;
    ColorButton3.ButtonColor := fd.OutLineColor.ToColor;
    SE7.Value := fd.OutLineColor.alpha;
  end;

  CBShadow.Checked := fd.UseShadow;
  if CBShadow.Checked then begin
    SE2.Value := fd.ShadowOffsetX;
    SE3.Value := fd.ShadowOffsetY;
    SE4.Value := fd.ShadowRadius;
    ColorButton4.ButtonColor := fd.ShadowColor.ToColor;
    SE8.Value := fd.ShadowColor.alpha;
  end;

  FInitializing := False;
end;

procedure TFrameViewFontBank.UpdatePreview;
begin
  ScreenFontBank.UpdatePreview(WidgetToFontDescriptor, GetCharset, GetFillTexture);
end;

function TFrameViewFontBank.GetCharset: string;
begin
  Result := FScene.Charsets.SIMPLELATIN;
end;

function TFrameViewFontBank.GetFillTexture: TBGRABitmap;
begin
  Result := NIL;
{  Result := TBGRABitmap.Create(10, 10);
  Result.FillRect(0,0,4,4, BGRAWhite, dmSet);
  Result.FillRect(5,5,10,10, BGRAWhite, dmSet);
  Result.FillRect(0,5,5,10,BGRA(255,255,0));
  Result.FillRect(5,0,10,5,BGRA(255,255,0));  }
end;

function TFrameViewFontBank.WidgetToFontStyle: TFontStyles;
begin
  Result := [];
  if CheckGroup1.Checked[0] then Include(Result, fsBold);
  if CheckGroup1.Checked[1] then Include(Result, fsItalic);
  if CheckGroup1.Checked[2] then Include(Result, fsUnderline);
  if CheckGroup1.Checked[3] then Include(Result, fsStrikeOut);
end;

function TFrameViewFontBank.CBToGradientType: TGradientType;
begin
  Result := TGradientType(CBGradientType.ItemIndex);
end;

procedure TFrameViewFontBank.GradientTypeToCB(aGradientType: TGradientType);
begin
  CBGradientType.ItemIndex := Ord(aGradientType);
end;

procedure TFrameViewFontBank.FontStyleToWidget(aFt: TFontStyles);
begin
  CheckGroup1.Checked[0] := fsBold in aFt;
  CheckGroup1.Checked[1] := fsItalic in aFt;
  CheckGroup1.Checked[2] := fsUnderline in aFt;
  CheckGroup1.Checked[3] := fsStrikeOut in aFt;

end;

function TFrameViewFontBank.GetGradientOrigin: TPointF;
begin
  Result := PointF(0, 0);
end;

function TFrameViewFontBank.GetGradientD1: TPointF;
begin
  Result := PointF(SE1.Value, SE1.Value);
end;

procedure TFrameViewFontBank.HideToolPanels;
begin
  PanelFont.Visible := False;
end;

procedure TFrameViewFontBank.ShowToolPanels;
var r, r1: TRect;
  procedure MakePanelVisible(aPanel: TPanel);
  var x: integer;
  begin
    x := r.Right+ScaleDesignToForm(10);
    if x+aPanel.Width > TV.ClientWidth then
      x := TV.ClientWidth-aPanel.Width;
    aPanel.Left := x;
    aPanel.Top := r.Top+r.Height div 2 - aPanel.Height div 2;
    aPanel.Visible := True;
  end;
begin
  HideToolPanels;
  if IsEditable then begin
    if TV.Selected = NIL then exit;
    r := TV.Selected.DisplayRect(True);
    r1.TopLeft := TV.ClientToParent(r.TopLeft);
    r1.BottomRight := TV.ClientToParent(r.BottomRight);
    r := r1;

    if SelectedIsFont then MakePanelVisible(PanelFont);
  end;
end;

function TFrameViewFontBank.NodeIsRoot(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 0;
end;

function TFrameViewFontBank.NodeIsFont(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 1;
end;

function TFrameViewFontBank.SelectedIsRoot: boolean;
begin
  Result := NodeIsRoot(TV.Selected);
end;

function TFrameViewFontBank.SelectedIsFont: boolean;
begin
  Result := NodeIsFont(TV.Selected);
end;

procedure TFrameViewFontBank.DoRenameSelected;
var oldName, newName: string;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;

  oldName := TV.Selected.Text;
  newName := Trim(InputBox('', sEnterTheNewName, oldName));
  if newName = oldName then exit;
  if FontBank.NameExists(newName) then begin
    ShowMessage(Format(sAFontNamedAlreadyExists, [newName]));
    exit;
  end;

  // change name in bank and save
  FontBank.GetByName(oldName)._Name := newName;
  FontBank.Save;

  // change name in treeview
  TV.Selected.Text := newName;
end;

procedure TFrameViewFontBank.DoDuplicateSelected;
var srcName, dstName: string;
  k: integer;
  srcItem, dstItem: TFontDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;

  // retrieve the source item
  srcName := TV.Selected.Text;
  srcItem := FontBank.GetByName(srcName);
  if srcItem = NIL then begin
    ShowMessage('Source item not found in the Font Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // create a unique name for the new font
  k := 0;
  repeat
    inc(k);
    if k < 100 then dstName := srcName+'_'+Format('%.2d', [k])
      else dstName := srcName+'_'+k.ToString;
  until not FontBank.NameExists(dstName);

  // add the new item in the bank and save
  dstItem := FontBank.AddEmpty;
  srcItem.DuplicateTo(dstItem);
  dstItem._Name := dstName;
  FontBank.Save;

  // add the new name in the treeview
  with TV.Items.AddChild(TV.Items.GetFirstNode, dstName) do begin
    ImageIndex := 11;
    SelectedIndex := 11;
    MakeVisible;
  end;
end;

procedure TFrameViewFontBank.DoDeleteSelected;
var item: TFontDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;
  if QuestionDlg('',sDeleteThisFont, mtWarning,
                 [mrOk, sDelete, mrCancel, sCancel], 0) = mrCancel then exit;

  item := FontBank.GetByName(TV.Selected.Text);
  if item = NIL then begin
    ShowMessage('Item not found in the Font Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // delete in bank
  FontBank.DeleteByName(TV.Selected.Text);
  FontBank.Save;

  // delete in treeview
  TV.Items.Delete(TV.Selected);

  ScreenFontBank.ClearView;
end;

procedure TFrameViewFontBank.Fill;
var
  root, fdnode: TTreeNode;
  fd: TFontDescriptorItem;
  i: Integer;
begin
  TV.BeginUpdate;
  TV.Items.Clear;
  TV.Items.AddFirst(NIL, sFontDescriptor);
  root := TV.Items.GetFirstNode;
  if FontBank.Size > 0 then
    for i:=0 to FontBank.Size-1 do begin
      fd := FontBank.GetByIndex(i);
      fdnode := TV.Items.AddChild(root, fd._Name);
      fdnode.SelectedIndex := 11;
      fdnode.ImageIndex := 11;
      fdnode.MakeVisible;
    end;
  TV.EndUpdate;

  LBFont.Clear;
  for i:=0 to Screen.Fonts.Count-1 do
    LBFont.Items.Add(Screen.Fonts.Strings[i]);
end;

end.

