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
    CBBold: TCheckBox;
    CBItalic: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    CBGradientType: TComboBox;
    Edit1: TEdit;
    FD1: TFontDialog;
    FSE1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
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
    Panel4: TPanel;
    PB: TPaintBox;
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
    Splitter2: TSplitter;
    Timer1: TTimer;
    TVAvailableFonts: TTreeView;
    TV: TTreeView;
    procedure BCancelClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BSwapGradientColorsClick(Sender: TObject);
    procedure BSwapOutlineAndShadowColorsClick(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
    procedure SE1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVAvailableFontsAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVAvailableFontsSelectionChanged(Sender: TObject);
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
    FPreviewNeedUpdate: boolean;
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
    procedure SetGradientOrigin(aPt: TPointF);
    procedure SetGradientD1(aPt: TPointF);
    procedure HideToolPanels;
    procedure ShowToolPanels;
    function NodeIsRoot(aNode: TTreeNode): boolean;
    function NodeIsFont(aNode: TTreeNode): boolean;
    function SelectedIsRoot: boolean;
    function SelectedIsFont: boolean;
    procedure DoRenameSelected;
    procedure DoDuplicateSelected;
    procedure DoDeleteSelected;
  private
    FGradOriginPercentCoor,
    FGradD1PercentCoor: TPointF;
    FButtonPressed: TMouseButton;
    FGradOriginCursorImage,
    FGradD1CursorImage: TBGRABitmap;
    procedure UpdateGradientOriginControlPoints(aX, aY: integer);
    procedure UpdateGradientD1ControlPoints(aX, aY: integer);
  private
    function AvailableSelectedIsFont: boolean;
    procedure FillTVAvailableFonts;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Fill;

    property IsEditable: boolean read FIsEditable write FIsEditable;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses u_resourcestring, form_main, u_screen_fontbank, u_common,
  u_project, u_utils, LCLType, Math;

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
  newnode: TTreeNode;
  res: TModalResult;
begin
  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if not IsValidPascalVariableName(nam, True) then exit;

  res := mrNone;
  if SelectedIsFont and (TV.Selected.Text = nam) then begin
    res := QuestionDlg('', sOverwriteTheSelectedFont, mtWarning,
                   [mrOk, sOverwrite, mrCancel, sCancel], 0);
    if res <> mrOk then exit;
  end;
  if res = mrOk then begin
    // modify the selected item
    item := FontBank.GetByName(nam);
    item.FD := WidgetToFontDescriptor;
    FontBank.Save;
  end else begin
    // add a new item in the bank
    item := FontBank.AddEmpty;
    item._Name := nam;
    item.FD := WidgetToFontDescriptor;
    FontBank.Save;
    // add the font to the treeview
    newnode :=  TV.Items.AddChild(TV.Items.GetFirstNode, nam);
    with newNode do begin
      ImageIndex := 10;
      SelectedIndex := 10;
      MakeVisible;
    end;
    TV.Selected := newNode;
  end;

  // remove old variable initialization
  Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(item.VariableNameForFontGradient);
  Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(item.VariableNameForFD);

  // create the variables in unit u_common and generate the code to initialize them
  if item.FD.UseGradient then begin
    Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFontGradient, 'TFontGradient');
    Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeGradientVariable);
  end;
  Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFD, 'TFontDescriptor');
  Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeFDVariable);
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

procedure TFrameViewFontBank.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Sender := Sender;
  Shift := Shift;
  FButtonPressed := Button;
  if Button = mbLeft then begin
    UpdateGradientOriginControlPoints(X, Y);
    PB.Tag := 1;
  end else if Button = mbRight then begin
    UpdateGradientD1ControlPoints(X, Y);
    PB.Tag := 1;
  end;
  UpdatePreview;
  PB.Invalidate;
end;

procedure TFrameViewFontBank.PBMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Shift := Shift;
  if PB.Tag = 0 then exit;
  case FButtonPressed of
    mbLeft: UpdateGradientOriginControlPoints(X, Y);
    mbRight: UpdateGradientD1ControlPoints(X, Y);
  end;
  UpdatePreview;
  PB.Invalidate;
end;

procedure TFrameViewFontBank.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Sender := Sender;
  Button := Button;
  Shift := Shift;
  X := X;
  Y := Y;
  PB.Tag := 0;
end;

procedure TFrameViewFontBank.PBPaint(Sender: TObject);
var xx1, yy1, xx2, yy2: integer;
  var ima: TBGRABitmap;
begin
  With PB.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(0, 0, Width, Height);
  end;

  ima := TBGRABitmap.Create(PB.ClientWidth, PB.ClientHeight, BGRABlack);

  xx1 := Round(FGradOriginPercentCoor.x*PB.ClientWidth)-FGradOriginCursorImage.Width div 2;
  yy1 := Round(FGradOriginPercentCoor.y*PB.ClientHeight)-FGradOriginCursorImage.Height div 2;
  ima.PutImage(xx1, yy1, FGradOriginCursorImage, dmDrawWithTransparency);//  FGradOriginCursorImage.Draw(PB.Canvas, xx, yy, False);

  xx2 := Round(FGradD1PercentCoor.x*PB.ClientWidth)-FGradD1CursorImage.Width div 2;
  yy2 := Round(FGradD1PercentCoor.y*PB.ClientHeight)-FGradD1CursorImage.Height div 2;
  ima.PutImage(xx2, yy2, FGradD1CursorImage, dmDrawWithTransparency);//  FGradD1CursorImage.Draw(PB.Canvas, xx, yy, False);

  xx1 := xx1 + FGradOriginCursorImage.Width div 2;
  yy1 := yy1 + FGradOriginCursorImage.Height div 2;
  xx2 := xx2 + FGradD1CursorImage.Width div 2;
  yy2 := yy2 + FGradD1CursorImage.Height div 2;
  ima.DrawLineAntialias(xx1, yy1, xx2, yy2, BGRAWhite, 2.0);

  ima.Draw(PB.Canvas,0, 0);
  ima.Free;
end;

procedure TFrameViewFontBank.SE1Change(Sender: TObject);
begin
  ColorButton2.Enabled := RBGradient.Checked;
  CBGradientType.Enabled := RBGradient.Checked;
  CheckBox1.Enabled := RBGradient.Checked;
  CheckBox2.Enabled := RBGradient.Checked;
  BSwapGradientColors.Enabled := RBGradient.Checked;
  PB.Visible := RBGradient.Checked;

  Panel2.Enabled := CBOutline.Checked;
  Panel3.Enabled := CBShadow.Checked;

  BSwapOutlineAndShadowColors.Enabled := CBOutline.Checked and CBShadow.Checked;


  if not FInitializing then
    UpdatePreview;
end;

procedure TFrameViewFontBank.Timer1Timer(Sender: TObject);
begin
  if FPreviewNeedUpdate then begin
    ScreenFontBank.UpdatePreview(WidgetToFontDescriptor, GetCharset, GetFillTexture);
    FPreviewNeedUpdate := False;
  end;
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

procedure TFrameViewFontBank.TVAvailableFontsAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var r: TRect;
  p: TPoint;
  ft: TFontStyles;
begin
  Sender := Sender;
  if (Stage = cdPostPaint) then begin // all is painted
    ft := TVAvailableFonts.Font.Style;
    r := Node.DisplayRect(True);
    With TVAvailableFonts.Canvas do begin
      // background
      if cdsSelected in State then begin
        Brush.Color := TVAvailableFonts.SelectionColor;
        TVAvailableFonts.Font.Color := clWhite
      end else begin
        Brush.Color := TVAvailableFonts.BackgroundColor;
        TVAvailableFonts.Font.Color := clWhite;
      end;

      Brush.Style := bsSolid;
      Pen.Style := psClear;
      FillRect(r);
      //Pen.Style := psSolid;

      // hot track (mouse is over)
      p := TVAvailableFonts.ScreenToClient(Mouse.CursorPos);
      if (TVAvailableFonts.GetNodeAt(p.x, p.y) = Node) and (Node.Level = 2) then
        TVAvailableFonts.Font.Style := TVAvailableFonts.Font.Style+[fsUnderline];

      // group
      Brush.Style := bsClear;
      if Node.Level = 1 then begin
        TVAvailableFonts.Font.Style := TVAvailableFonts.Font.Style+[fsBold];
        TVAvailableFonts.Font.Color := RGBToColor(255,255,150);
      end;
      TextOut(r.Left+2, r.Top+(r.Height-TVAvailableFonts.Font.Height) div 2, Node.Text);
      TVAvailableFonts.Font.Style := ft;
    end;
    PaintImages := True;
    DefaultDraw := false;
  end else begin
    PaintImages := True;
    DefaultDraw := True;
  end;
end;

procedure TFrameViewFontBank.TVAvailableFontsSelectionChanged(Sender: TObject);
var canBold, canItalic: boolean;
begin
  if FInitializing then exit;

  if AvailableSelectedIsFont then begin
    Label15.Caption := self.TVAvailableFonts.Selected.Text;
    FScene.FontManager.FontHaveBoldOrItalic(Label15.Caption, canBold, canItalic);
    CBBold.Enabled := canBold;
    if not CBBold.Enabled then CBBold.Checked := False;
    CBItalic.Enabled := canItalic;
    if not CBItalic.Enabled then CBItalic.Checked := False;
  end;
  SE1Change(Self);
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
  TVAvailableFonts.Selected := TVAvailableFonts.Items.FindNodeWithText(fd.FontName);
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
    SetGradientOrigin(fd.Gradient.Origin);
    SetGradientD1(fd.Gradient.D1);
    CheckBox1.Checked := fd.Gradient.GammaColorCorrection;
    CheckBox2.Checked := fd.Gradient.Sinus;
    PB.Invalidate;
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
  //ScreenFontBank.UpdatePreview(WidgetToFontDescriptor, GetCharset, GetFillTexture);
  FPreviewNeedUpdate := True;
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
  if CBBold.Checked then Include(Result, fsBold);
  if CBItalic.Checked then Include(Result, fsItalic);
  if CheckBox5.Checked then Include(Result, fsUnderline);
  if CheckBox6.Checked then Include(Result, fsStrikeOut);
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
  CBBold.Checked := fsBold in aFt;
  CBItalic.Checked := fsItalic in aFt;
  CheckBox5.Checked := fsUnderline in aFt;
  CheckBox6.Checked:= fsStrikeOut in aFt;

end;

function TFrameViewFontBank.GetGradientOrigin: TPointF;
begin
  Result.x := FGradOriginPercentCoor.x * SE1.Value;
  Result.y := FGradOriginPercentCoor.y * SE1.Value;
end;

function TFrameViewFontBank.GetGradientD1: TPointF;
begin
  Result.x := FGradD1PercentCoor.x * SE1.Value;
  Result.y := FGradD1PercentCoor.y * SE1.Value;
end;

procedure TFrameViewFontBank.SetGradientOrigin(aPt: TPointF);
begin
  FGradOriginPercentCoor.x := aPt.x / SE1.Value;
  FGradOriginPercentCoor.y := aPt.y / SE1.Value;
end;

procedure TFrameViewFontBank.SetGradientD1(aPt: TPointF);
begin
  FGradD1PercentCoor.x := aPt.x / SE1.Value;
  FGradD1PercentCoor.y := aPt.y / SE1.Value;
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
var oldName, newName, fdVarName, gradVarName: string;
  item: TFontDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;

  item := FontBank.GetByName(TV.Selected.Text);
  if item = NIL then exit;
  fdVarName := item.VariableNameForFD;
  gradVarName := item.VariableNameForFontGradient;

  oldName := TV.Selected.Text;
  if not FontBank.DoRenameByName(oldName, newName, True) then exit;

  // delete the old variables declaration and initialization
  if item.FD.UseGradient then begin
    Project.Config.TargetLazarusProject.UCommonRemoveVar(gradVarName);
    Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(gradVarName);
  end;
  Project.Config.TargetLazarusProject.UCommonRemoveVar(fdVarName);
  Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(fdVarName);

  item := FontBank.GetByName(newName);
  // add variable declaration and initialization in u_common
  if item.FD.UseGradient then begin
    Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFontGradient, 'TFontGradient');
    Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeGradientVariable);
  end;
  Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFD, 'TFontDescriptor');
  Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeFDVariable);

  // change name in treeview
  TV.Selected.Text := newName;
end;

procedure TFrameViewFontBank.DoDuplicateSelected;
var dstName: string;
  newNode: TTreeNode;
  item: TFontDescriptorItem;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;

  if not FontBank.DoDuplicateByName(TV.Selected.Text, dstName, True) then exit;
  // add the new name in the treeview
  newNode :=  TV.Items.AddChild(TV.Items.GetFirstNode, dstName);
  with newNode do begin
    ImageIndex := 10;
    SelectedIndex := 10;
    MakeVisible;
  end;
  TV.Selected := newNode;

  item := FontBank.GetByName(dstName);
  // add variable declaration and initialization in u_common
  if item.FD.UseGradient then begin
    Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFontGradient, 'TFontGradient');
    Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeGradientVariable);
  end;
  Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableNameForFD, 'TFontDescriptor');
  Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeFDVariable);
end;

procedure TFrameViewFontBank.DoDeleteSelected;
var item: TFontDescriptorItem;
  fdVarName, gradVarName: string;
begin
  HideToolPanels;
  if not SelectedIsFont then exit;

  item := FontBank.GetByName(TV.Selected.Text);
  if item = NIL then exit;
  fdVarName := item.VariableNameForFD;
  gradVarName := item.VariableNameForFontGradient;

  if not FontBank.DoDeleteByName(TV.Selected.Text, True) then exit;

  // delete the variables declaration and initialization
  if item.FD.UseGradient then begin
    Project.Config.TargetLazarusProject.UCommonRemoveVar(gradVarName);
    Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(gradVarName);
  end;
  Project.Config.TargetLazarusProject.UCommonRemoveVar(fdVarName);
  Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(fdVarName);

  // delete in treeview
  TV.Items.Delete(TV.Selected);

  ScreenFontBank.ClearView;
end;

function TFrameViewFontBank.AvailableSelectedIsFont: boolean;
begin
  if TVAvailableFonts.Selected = NIL then exit(False)
    else Result := TVAvailableFonts.Selected.Level = 2;
end;

procedure TFrameViewFontBank.FillTVAvailableFonts;
var root, nodeSystem, nodeProject: TTreeNode;
  i: integer;
begin
  TVAvailableFonts.BeginUpdate;
  TVAvailableFonts.Items.Clear;
  TVAvailableFonts.Items.AddFirst(NIL, sAvailableFonts);
  root := TVAvailableFonts.Items.GetFirstNode;
  nodeProject := TVAvailableFonts.Items.AddChild(root, sProject);
  nodeProject.MakeVisible;
  nodeSystem := TVAvailableFonts.Items.AddChild(root, sSystem);
  nodeSystem.MakeVisible;

  FScene.FontManager.ScanProjectFont(Project.Config.TargetLazarusProject.GetFolderDataFonts);

  for i:=0 to FScene.FontManager.ProjectFontCount-1 do
    with TVAvailableFonts.Items.AddChild(nodeProject, FScene.FontManager.ProjectFontName[i]) do
      MakeVisible;

  for i:=0 to FScene.FontManager.SystemFontCount-1 do
    with TVAvailableFonts.Items.AddChild(nodeSystem, FScene.FontManager.SystemFontName[i]) do
      MakeVisible;

  root.MakeVisible;
  TVAvailableFonts.EndUpdate;
end;

procedure TFrameViewFontBank.UpdateGradientOriginControlPoints(aX, aY: integer);
begin
  FGradOriginPercentCoor.x := EnsureRange(aX/PB.ClientWidth, 0, 1);
  FGradOriginPercentCoor.y := EnsureRange(aY/PB.ClientHeight, 0, 1);
  //FGradD1PercentCoor := PointF(1-FGradOriginPercentCoor.x, 1-FGradOriginPercentCoor.y);
end;

procedure TFrameViewFontBank.UpdateGradientD1ControlPoints(aX, aY: integer);
begin
  FGradD1PercentCoor.x := EnsureRange(aX/PB.ClientWidth, 0, 1);
  FGradD1PercentCoor.y := EnsureRange(aY/PB.ClientHeight, 0, 1);
end;

constructor TFrameViewFontBank.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FGradOriginCursorImage := TBGRABitmap.Create(10, 10, BGRAPixelTransparent);
  FGradOriginCursorImage.FillEllipseAntialias(FGradOriginCursorImage.Width div 2,
                           FGradOriginCursorImage.Height div 2,
                           FGradOriginCursorImage.Width div 2,
                           FGradOriginCursorImage.Height div 2,
                           BGRA(255,255,0));

  FGradD1CursorImage := TBGRABitmap.Create(10, 10, BGRAPixelTransparent);
  FGradD1CursorImage.RectangleAntialias(0, 0, FGradD1CursorImage.Width-1, FGradD1CursorImage.Height-1, BGRA(255,255,0), 2.0);
end;

destructor TFrameViewFontBank.Destroy;
begin
  FGradOriginCursorImage.Free;
  FGradD1CursorImage.Free;
  inherited Destroy;
end;

procedure TFrameViewFontBank.Fill;
var  root, fdnode: TTreeNode;
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
      fdnode.SelectedIndex := 10;
      fdnode.ImageIndex := 10;
      fdnode.MakeVisible;
    end;
  TV.EndUpdate;

  FScene.FontManager.ScanProjectFont(Project.Config.TargetLazarusProject.GetFolderDataFonts);

  FillTVAvailableFonts;

  FGradOriginPercentCoor := PointF(0.5, 0.0);
  FGradD1PercentCoor := PointF(0.5, 1.0);
end;

end.

