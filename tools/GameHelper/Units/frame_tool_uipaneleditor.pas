unit frame_tool_uipaneleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls, Dialogs,
  Buttons, Spin,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  frame_texturelist, u_texture_list, u_surface_list, u_ui_objectlist,
  u_presetmanager, Types;

type

  { TFrameToolUIPanelEditor }

  TFrameToolUIPanelEditor = class(TFrame)
    BCancel: TSpeedButton;
    BHelpRootChilds: TSpeedButton;
    BNewChild: TSpeedButton;
    BSave: TSpeedButton;
    CBBlend: TComboBox;
    CBChildType: TComboBox;
    CBParent: TComboBox;
    CBTexturesUICheckChecked: TComboBox;
    CBTexturesUIRadioChecked: TComboBox;
    CBTexturesUICheckUnchecked: TComboBox;
    CBTexturesUIRadioUnchecked: TComboBox;
    CBTexturesUIImage: TComboBox;
    CBTexturesUIButton: TComboBox;
    CBDarkenBG: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorButton4: TColorButton;
    ColorButton5: TColorButton;
    ColorButton6: TColorButton;
    ColorButton7: TColorButton;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    FSE3: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label8: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Notebook1: TNotebook;
    Notebook2: TNotebook;
    NotebookExtra: TNotebook;
    PageControl2: TPageControl;
    PageRadioTextures: TPage;
    PageShapeRadio: TPage;
    PageShapeNone: TPage;
    PageCheckTextures: TPage;
    PageShapeCheck: TPage;
    PageShapeNone1: TPage;
    PageUITextArea: TPage;
    PageUIListBox: TPage;
    PageUIScrollBox: TPage;
    PageUIProgressBar: TPage;
    PageUIScrollBar: TPage;
    PageUIRadio: TPage;
    PageUICheck: TPage;
    PageUIButton: TPage;
    PageUILabel: TPage;
    PageUIImage: TPage;
    PageUIPanel: TPage;
    PageExtraEmpty: TPage;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioGroup1: TRadioGroup;
    RBOptionNormal: TRadioButton;
    RBOptionModal: TRadioButton;
    PageMainPanel: TTabSheet;
    PageTextures: TTabSheet;
    PageChilds: TTabSheet;
    SE100: TSpinEdit;
    SE10: TSpinEdit;
    SE11: TSpinEdit;
    SE14: TSpinEdit;
    SE15: TSpinEdit;
    SE23: TSpinEdit;
    SE24: TSpinEdit;
    SE25: TSpinEdit;
    SE26: TSpinEdit;
    SE27: TSpinEdit;
    SE28: TSpinEdit;
    SE29: TSpinEdit;
    SE30: TSpinEdit;
    SE31: TSpinEdit;
    SE32: TSpinEdit;
    SE33: TSpinEdit;
    SE34: TSpinEdit;
    SE36: TSpinEdit;
    SE9: TSpinEdit;
    SE17: TSpinEdit;
    SE18: TSpinEdit;
    SE1: TFloatSpinEdit;
    SE12: TSpinEdit;
    SE13: TSpinEdit;
    SE20: TSpinEdit;
    SE21: TSpinEdit;
    SE22: TSpinEdit;
    SE3: TFloatSpinEdit;
    SE4: TFloatSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    SE8: TSpinEdit;
    SE2: TFloatSpinEdit;
    BEditOnShow: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    BEditOnHide: TSpeedButton;
    SE19: TSpinEdit;
    SpeedButton2: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SEDummy: TSpinEdit;
    PageTextAreaFont: TTabSheet;
    PageTextAreaText: TTabSheet;
    PageTextAreaAlign: TTabSheet;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SE35: TSpinEdit;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SE37: TSpinEdit;
    SE38: TSpinEdit;
    SpeedButton8: TSpeedButton;
    procedure BCancelClick(Sender: TObject);
    procedure BNewChildClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure CBChildTypeSelect(Sender: TObject);
    procedure CBParentDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure SE1Enter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
  private
    FModified: boolean;
    FInitializing: boolean;
    FTargetUIPanel: TUIPanelWithEffects;
  private // textures
    procedure ProcessTextureListOnModified(Sender: TObject);
    procedure ProcessAskToDeleteTextureEvent(aTextureItem: PTextureItem; var aCanDelete: boolean);
    procedure ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
    function CBToTextureName(aCB: TComboBox): string;
    procedure GetSelectedTextureName(out aTexName1, aTexName2: string);
    function Textures: TTextureList;
    function Surfaces: TSurfaceList;
  private // childs
    procedure ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
    function CBToClassType: classOfSimpleSurfaceWithEffect;
    procedure ParentIDToCB(aID: integer);
    function CBToParentID: integer;
    function GetUniqueName: string;
    function GetWidgetTintMode: TTintMode;
    function GetWidgetTint: TBGRAPixel;
    function CheckChildWidgets: boolean;
    procedure DoAddNewChild;
    procedure DoUpdateChild(aForceRecreateSurface: boolean);
    procedure UpdateExtraPropertiesToWorkingTemporary;
    procedure UpdateValuesToWorkingSurface;
    procedure UpdateEventCheckBox;
  private
    FWorkingChild: PSurfaceDescriptor;
    procedure DoClearAll;
    procedure ShowExtraPropertyPanel;
procedure UpdateDebugLabels;
  public
    FrameTexturesList: TFrameTextureList;
    constructor Create(aOwner: TComponent); override;
    procedure SetFocusToDummy;

    procedure OnShow;
    procedure ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);

    procedure EditNewPanel;
    procedure EditPanelFromBank(aItem: TPanelDescriptorItem);

    procedure ProcessEndResize;

    procedure FillTexturesComboBox;
    procedure FillCBParent;
    procedure FillFontsComboBox;

    // initialized by ScreenUIPanelEditor
    property TargetUIPanel: TUIPanelWithEffects read FTargetUIPanel write FTargetUIPanel;
    property Modified: boolean read FModified write FModified;
  end;

implementation

uses form_main, u_screen_uipaneleditor, u_resourcestring, u_utils, u_project,
  form_edituibodyshape, u_common, form_editgradient, LCLType, Graphics;

{$R *.lfm}

{ TFrameToolUIPanelEditor }

procedure TFrameToolUIPanelEditor.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('', sIfYouLeaveChangeWillBeLost, mtWarning,
                   [mrOk, sLeaveWithoutSaving, mrCancel, sCancel], 0) = mrCancel then exit;
  DoClearAll;
  FormMain.ShowPagePanelBank;
end;

procedure TFrameToolUIPanelEditor.BNewChildClick(Sender: TObject);
begin
  if Sender = BNewChild then DoAddNewChild;
end;

procedure TFrameToolUIPanelEditor.BSaveClick(Sender: TObject);
var nam: string;
  item: TPanelDescriptorItem;
begin
  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if not IsValidPascalVariableName(nam, True) then exit;
  if not PanelBank.AskUserToReplaceExistingItem(nam) then exit;

  // retrieve the existing item or create one
  item := PanelBank.GetByName(nam);
  if item = NIL then item := PanelBank.AddEmpty;

  item._Name := nam;
  item.IsModal := RBOptionModal.Checked;
  item.DarkenBG := CBDarkenBG.Checked;
  item.DarknessColor := ColorToBGRA(ColorButton1.ButtonColor, SE100.Value);
  item.ScenarioOnShow := '';
  item.ScenarioOnHide := '';
  item.textures := Textures.SaveToString;
  item.surfaces := Surfaces.SaveToString;

  PanelBank.Save;
  Modified := False;

  DoClearAll;
  FormMain.ShowPagePanelBank;
end;

procedure TFrameToolUIPanelEditor.CBChildTypeSelect(Sender: TObject);
//var chang: boolean;
var fontItem: TFontDescriptorItem;
  recreateSurface: boolean;
  flag: boolean;
begin
  if FInitializing then exit;

  flag := Assigned(FWorkingChild);
  if flag then flag := Assigned(FWorkingChild^.surface);

  recreateSurface := False;

  if Sender = CBChildType then begin
    Edit5.Text := GetUniqueName;
    ShowExtraPropertyPanel;
  end;

  // TUIPanel
  if ((Sender = SE12) or (Sender = SE13)) and flag then begin
    TUIPanel(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE12.Value, SE13.Value, True);
    FModified := True;
  end;
  // TUIImage
  if (Sender = CBTexturesUIImage) and flag then begin
    FWorkingChild^.textureName := CBTexturesUIImage.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = SE17) or (Sender = SE18)) and flag then begin
    TUIImage(FWorkingChild^.surface).SetSize(SE17.Value, SE18.Value);
    FModified := True;
  end;
  // TUILabel
  if (Sender = Edit2) and flag then begin
    TUILabel(FWorkingChild^.surface).Caption := Edit2.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = ComboBox1) and flag then begin
    FWorkingChild^.FontDescriptorName := ComboBox1.Text;
    recreateSurface := True;
    FModified := True;
  end;
  // TUIButton
  if ((Sender = SE23) or (Sender = SE24)) and flag then begin
    TUIButton(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE23.Value, SE24.Value, True);
    FModified := True;
  end;
  if (Sender = CheckBox1) and flag then begin
    TUIButton(FWorkingChild^.surface).AutoSize := CheckBox1.Checked;
    FModified := True;
  end;
  if (Sender = CBTexturesUIButton) and flag then begin
    FWorkingChild^.textureName := CBTexturesUIButton.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = ComboBox2) and flag then begin
    FWorkingChild^.FontDescriptorName := ComboBox2.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = Edit3) and flag then begin
    TUIButton(FWorkingChild^.surface).Caption := Edit3.Text;
    recreateSurface := True;
    FModified := True;
  end;
  // TUICheck
  if (Sender = Edit4) and flag then begin
    TUICheck(FWorkingChild^.surface).Caption := Edit4.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ComboBox3) or (Sender = ComboBox4)) and flag then begin
    FWorkingChild^.FontDescriptorName := ComboBox3.Text;
    FWorkingChild^.CheckShape := TUICheckShape(ComboBox4.ItemIndex);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = CheckBox12) and flag then begin
    TUICheck(FWorkingChild^.surface).Checked := CheckBox12.Checked;
    FModified := True;
  end;
  if (Sender = ComboBox5) and flag then begin
    FWorkingChild^.CheckFill := TUICheckFill(ComboBox5.ItemIndex);
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ColorButton2) or (Sender = SE14)) and flag then begin
    TUICheck(FWorkingChild^.surface).ColorChecked := ColorToBGRA(ColorButton2.ButtonColor, SE14.Value);
    FModified := True;
  end;
  if ((Sender = CBTexturesUICheckChecked) or (Sender = CBTexturesUICheckUnchecked) or
     (Sender = CheckBox10)) and flag then begin
      FWorkingChild^.textureName := CBTexturesUICheckChecked.Text;
      FWorkingChild^.textureName2 := CBTexturesUICheckUnchecked.Text;
      FWorkingChild^.CheckAdjustImageToFontHeight := CheckBox10.Checked;
    recreateSurface := True;
    FModified := True;
  end;
  // TUIRadio
  if (Sender = Edit6) and flag then begin
    TUIRadio(FWorkingChild^.surface).Caption := Edit6.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ComboBox6) or (Sender = ComboBox7)) and flag then begin
    FWorkingChild^.FontDescriptorName := ComboBox6.Text;
    FWorkingChild^.CheckShape := TUICheckShape(ComboBox7.ItemIndex);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = CheckBox13) and flag then begin
    TUIRadio(FWorkingChild^.surface).Checked := CheckBox13.Checked;
    FModified := True;
  end;
  if (Sender = ComboBox8) and flag then begin
    FWorkingChild^.CheckFill := TUICheckFill(ComboBox8.ItemIndex);
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ColorButton3) or (Sender = SE15)) and flag then begin
    TUIRadio(FWorkingChild^.surface).ColorChecked := ColorToBGRA(ColorButton3.ButtonColor, SE15.Value);
    FModified := True;
  end;
  if ((Sender = CBTexturesUIRadioChecked) or (Sender = CBTexturesUIRadioUnchecked) or
     (Sender = CheckBox11)) and flag then begin
      FWorkingChild^.textureName := CBTexturesUIRadioChecked.Text;
      FWorkingChild^.textureName2 := CBTexturesUIRadioUnchecked.Text;
      FWorkingChild^.CheckAdjustImageToFontHeight := CheckBox11.Checked;
    recreateSurface := True;
    FModified := True;
  end;
  // TUIScrollBar
  if ((Sender = SE25) or (Sender = SE26)) and flag then begin
    TUIScrollBar(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE25.Value, SE26.Value, True);
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = RadioButton5) or (Sender = RadioButton6)) and flag then begin
    if RadioButton5.Checked then FWorkingChild^.uiOrientation := uioVertical
      else FWorkingChild^.uiOrientation := uioHorizontal;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = SE19) or (Sender = SE20) or (Sender = SE21) or (Sender = SE22)) and flag then begin
    TUIScrollBar(FWorkingChild^.surface).SetParams(SE22.Value, SE19.Value, SE20.Value, SE21.Value);
    FModified := True;
  end;
  // TUIProgressBar
  if ((Sender = SE27) or (Sender = SE28)) and flag then begin
    TUIProgressBar(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE27.Value, SE28.Value, True);
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = RadioButton3) or (Sender = RadioButton4) or
      (Sender = CheckBox9)) and flag then begin
    if RadioButton3.Checked then FWorkingChild^.uiOrientation := uioVertical
      else FWorkingChild^.uiOrientation := uioHorizontal;
    TUIProgressBar(FWorkingChild^.surface).Reversed := CheckBox9.Checked;
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = FSE3) and flag then begin
    TUIProgressBar(FWorkingChild^.surface).Percent := FSE3.Value;
    FModified := True;
  end;
  // TUIScrollBox
  if ((Sender = SE29) or (Sender = SE30)) and flag then begin
    TUIScrollBox(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE29.Value, SE30.Value, True);
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = CheckBox14) or (Sender = CheckBox15)) and flag then begin
    FWorkingChild^.scrollboxUseVScrollbar := CheckBox14.Checked;
    FWorkingChild^.scrollboxUseHScrollbar := CheckBox15.Checked;
    recreateSurface := True;
    FModified := True;
  end;
  // TUIListBox
  if ((Sender = SE31) or (Sender = SE32)) and flag then begin
    TUIListBox(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE31.Value, SE32.Value, True);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = ComboBox10) and flag then begin   // listbox item height = font height
    FWorkingChild^.FontDescriptorName := ComboBox10.Text;
    fontItem := FontBank.GetByName(ComboBox10.Text);
    if fontItem <> NIL then SE38.Value := fontItem.FD.FontHeight;
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = CheckBox16) and flag then begin
    FWorkingChild^.listboxMultiSelect := CheckBox16.Checked;
    FModified := True;
  end;
  if (Sender = SE38) and flag then begin
    FWorkingChild^.listboxItemHeight := SE38.Value;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ColorButton4) or (Sender = SE35) or
      (Sender = ColorButton6) or (Sender = SE36)) and flag then begin
    FWorkingChild^.listboxUnselectedTextColor := ColorToBGRA(ColorButton4.ButtonColor, SE35.Value);
    FWorkingChild^.listboxSelectedTextColor := ColorToBGRA(ColorButton6.ButtonColor, SE36.Value);
    recreateSurface := True;
    FModified := True;
  end;
  // TUITextArea
  if ((Sender = SE33) or (Sender = SE34)) and flag then begin
    TUITextArea(FWorkingChild^.surface).BodyShape.ResizeCurrentShape(SE33.Value, SE34.Value, True);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = ComboBox9) and flag then begin   // listbox item height = font height
    FWorkingChild^.FontDescriptorName := ComboBox9.Text;
    recreateSurface := True;
    FModified := True;
  end;
  if ((Sender = ColorButton7) or (Sender = SE37)) and flag then begin
    FWorkingChild^.TextAreaTextTint := ColorToBGRA(ColorButton7.ButtonColor, SE37.Value);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = RadioGroup1) and flag then begin
    FWorkingChild^.TextAlignment := TOGLCAlignment(RadioGroup1.ItemIndex);
    recreateSurface := True;
    FModified := True;
  end;
  if (Sender = Memo3) and flag then begin
    FWorkingChild^.TextAreaText := Memo3.Text;
    recreateSurface := True;
    FModified := True;
  end;



  if FWorkingChild <> NIL then begin
    DoUpdateChild(recreateSurface);
    FModified := True;
  end else begin
    ShowExtraPropertyPanel;
  end;
end;

procedure TFrameToolUIPanelEditor.CBParentDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var s: string;
  item: PSurfaceDescriptor;
  id: Integer;
begin
  Control := Control;
  with CBParent.Canvas do begin
    if odSelected in State then
      Brush.Color := clHighLight;
    Brush.Style := bsSolid;
    FillRect(ARect);

    Brush.Style := bsClear;
    id := CBParent.Items.Strings[Index].ToInteger;
    item := Surfaces.GetItemByID(id);
    if item <> NIL then s := item^.name
      else s := 'Index '+Index.ToString+' unknown';
    TextOut(ARect.Left, ARect.Top, s);
  end;
end;

procedure TFrameToolUIPanelEditor.SE1Enter(Sender: TObject);
begin
  LastClickedIsControl := True;
end;

procedure TFrameToolUIPanelEditor.SpeedButton1Click(Sender: TObject);
begin
  if Sender = SpeedButton1 then CBTexturesUIButton.ItemIndex := -1;
  if Sender = SpeedButton2 then CBTexturesUICheckChecked.ItemIndex := -1;
  if Sender = SpeedButton3 then CBTexturesUICheckUnchecked.ItemIndex := -1;
  if Sender = SpeedButton4 then CBTexturesUIRadioChecked.ItemIndex := -1;
  if Sender = SpeedButton5 then CBTexturesUIRadioUnchecked.ItemIndex := -1;
end;

procedure TFrameToolUIPanelEditor.SpeedButton3Click(Sender: TObject);
var item: PSurfaceDescriptor;
begin
  if ScreenUIPanelEditor.SelectedCount <> 1 then exit;
  item := ScreenUIPanelEditor.Selected[0];

  FormEditBodyShape := TFormEditBodyShape.Create(NIL);
  try
    FormEditBodyShape.Edit(TUIClickableWithBodyShape(item^.surface));
    FormEditBodyShape.ShowModal;

    item^.BodyShapeData := TUIClickableWithBodyShape(item^.surface).BodyShape.SaveToString;
    item^.BackGradientData := TUIClickableWithBodyShape(item^.surface).BackGradient.SaveGradientDataToString;
  finally
    FormEditBodyShape.Free;
    FormEditBodyShape := NIL;
  end;
end;

procedure TFrameToolUIPanelEditor.SpeedButton6Click(Sender: TObject);
var item: PSurfaceDescriptor;
  w, h: integer;
begin
  if ScreenUIPanelEditor.SelectedCount <> 1 then exit;
  item := ScreenUIPanelEditor.Selected[0];
  if item^.surface.ClassName <> 'TUIListBox' then exit;
  w := TUIListBox(item^.surface).ClientArea.Width;
  h := TUIListBox(item^.surface).ItemHeight;

  FormEditGradient := TFormEditGradient.Create(NIL);
  try
    if Sender = SpeedButton6
      then FormEditGradient.Edit(@TUIListBox(item^.surface).ItemColor.GradientItem, w, h)
      else FormEditGradient.Edit(@TUIListBox(item^.surface).ItemColor.GradientItemSelected, w, h);
    FormEditGradient.ShowModal;
    if Sender = SpeedButton6
      then item^.listboxUnselectedGradientData := TUIListBox(item^.surface).ItemColor.GradientItem.SaveGradientDataToString
      else item^.listboxSelectedGradientData := TUIListBox(item^.surface).ItemColor.GradientItemSelected.SaveGradientDataToString;
  finally
    FormEditGradient.Free;
  end;
end;

procedure TFrameToolUIPanelEditor.SpeedButton8Click(Sender: TObject);
var item: PSurfaceDescriptor;
  w, h: integer;
begin
  if ScreenUIPanelEditor.SelectedCount <> 1 then exit;
  item := ScreenUIPanelEditor.Selected[0];
  if item^.surface.ClassName <> 'TUIProgressBar' then exit;
  w := TUIProgressBar(item^.surface).ClientArea.Width;
  h := TUIProgressBar(item^.surface).ClientArea.Height;

  FormEditGradient := TFormEditGradient.Create(NIL);
  try
    if not TUIProgressBar(item^.surface).Gradient.IsInitialized then begin
      TUIProgressBar(item^.surface).Gradient.LoadGradientDataFromString(DEFAULT_GRADIENT);
      TUIProgressBar(item^.surface).Gradient.ComputeVerticesAndIndices(w, h);
    end;
    FormEditGradient.Edit(@TUIProgressBar(item^.surface).Gradient, w, h);
    FormEditGradient.ShowModal;
    item^.ProgressBarGradientData := TUIProgressBar(item^.surface).Gradient.SaveGradientDataToString;
  finally
    FormEditGradient.Free;
  end;
end;

procedure TFrameToolUIPanelEditor.ProcessTextureListOnModified(Sender: TObject);
begin
  FModified := True;
  FillTexturesComboBox;
end;

procedure TFrameToolUIPanelEditor.ProcessAskToDeleteTextureEvent(
  aTextureItem: PTextureItem; var aCanDelete: boolean);
begin
  if Length(Surfaces.GetItemsThatUseThisTexture(aTextureItem)) > 0 then begin
    aCanDelete := False;
    ShowMessage('This texture is used by a surface, you can not delete it');
  end;
end;

procedure TFrameToolUIPanelEditor.ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
var surf: ArrayOfPSurfaceDescriptor;
  i: Integer;
begin
  // recreate the surfaces that use this texture (if any)
  surf := Surfaces.GetItemsThatUseThisTexture(aTextureItem);
  if Length(surf) <> 0 then
    for i:=0 to High(surf) do
      surf[i]^.RecreateSurfaceBecauseTextureChanged;
end;

function TFrameToolUIPanelEditor.CBToTextureName(aCB: TComboBox): string;
begin
  if aCB.ItemIndex = -1 then Result := ''
    else Result := aCB.Items.Strings[aCB.ItemIndex];
end;

procedure TFrameToolUIPanelEditor.GetSelectedTextureName(out aTexName1, aTexName2: string);
begin
  aTexName1 := '';
  aTexName2 := '';
  case CBChildtype.Text of
    'TUIPanel':;
    'TUIImage': aTexName1 := CBToTextureName(CBTexturesUIImage);
    'TUILabel':;
    'TUIButton': aTexName1 := CBToTextureName(CBTexturesUIButton);
    'TUICheck':
      if Notebook1.PageIndex = Notebook1.IndexOf(PageCheckTextures) then begin
        aTexName1 := CBToTextureName(CBTexturesUICheckChecked);
        aTexName2 := CBToTextureName(CBTexturesUICheckUnchecked);
      end;
    'TUIRadio':
      if Notebook2.PageIndex = Notebook2.IndexOf(PageRadioTextures) then begin
        aTexName1 := CBToTextureName(CBTexturesUIRadioChecked);
        aTexName2 := CBToTextureName(CBTexturesUIRadioUnchecked);
      end;
    'TUIProgressBar':;
    'TUIScrollBox':;
    'TUIListBox':;
    'TUITextArea':;
  end;
end;

function TFrameToolUIPanelEditor.Textures: TTextureList;
begin
  Result := ScreenUIPanelEditor.Textures;
end;

function TFrameToolUIPanelEditor.Surfaces: TSurfaceList;
begin
  Result := ScreenUIPanelEditor.Surfaces;
end;

procedure TFrameToolUIPanelEditor.ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
var i: integer;
begin
if aClass = TUIPanelWithEffects then aClass := TUIPanel;
  i := CBChildType.Items.IndexOf(aClass.ClassName);
  if i = -1 then raise exception.create('forgot to implement '+aClass.ClassName+' !');
  CBChildType.ItemIndex := i;
end;

function TFrameToolUIPanelEditor.CBToClassType: classOfSimpleSurfaceWithEffect;
begin
  case CBChildType.Text of
    'TUIPanel': Result := TUIPanel;
    'TUIImage': Result := TUIImage;
    'TUILabel': Result := TUILabel;
    'TUIButton': Result := TUIButton;
    'TUICheck': Result := TUICheck;
    'TUIRadio': Result := TUIRadio;
    'TUIScrollBar': Result := TUIScrollBar;
    'TUIProgressBar': Result := TUIProgressBar;
    'TUIScrollBox': Result := TUIScrollBox;
    'TUIListBox': Result := TUIListBox;
    'TUITextArea': Result := TUITextArea;
    else raise exception.create('forgot to implement!');
  end;
end;

procedure TFrameToolUIPanelEditor.ParentIDToCB(aID: integer);
begin
  CBParent.ItemIndex := CBParent.Items.IndexOf(aID.ToString);
end;

function TFrameToolUIPanelEditor.CBToParentID: integer;
begin
  if CBParent.ItemIndex = -1 then Result := -1
    else Result := CBParent.Items.Strings[CBParent.ItemIndex].ToInteger;
end;

function TFrameToolUIPanelEditor.GetUniqueName: string;
var k: Integer;
begin
  Result := Copy(CBChildType.Text, 4, Length(CBChildType.Text));
  k := 0;
  repeat
    inc(k);
  until not Surfaces.NameExists(Result + k.ToString);
  Result := Result + k.ToString;
end;

function TFrameToolUIPanelEditor.GetWidgetTintMode: TTintMode;
begin
  if RadioButton1.Checked then Result := tmReplaceColor
    else Result := tmMixColor;
end;

function TFrameToolUIPanelEditor.GetWidgetTint: TBGRAPixel;
begin
  Result := ColorToBGRA(ColorButton5.ButtonColor, SE11.Value);
end;

function TFrameToolUIPanelEditor.CheckChildWidgets: boolean;
begin
  if CBChildType.ItemIndex = -1 then exit(False);
  if not IsValidPascalVariableName(Trim(Edit5.Text), True) then exit(False);
  if CBParent.ItemIndex = -1 then exit(False);

  case CBChildType.Text of
    'TUIPanel', 'TUIPanelWithEffects': Result := True;
    'TUIImage': begin
      Result := CBTexturesUIImage.ItemIndex <> -1;
    end;
    'TUILabel': begin
      Result := (Edit2.Text <> '') and (ComboBox1.ItemIndex <> -1);
    end;
    'TUIButton': begin
      Result := (CBTexturesUIButton.ItemIndex <> -1) or
                ((ComboBox2.ItemIndex <> -1) and (Edit3.Text <> ''));
    end;
    'TUICheck': begin
      Result := (Edit4.Text <> '') and (ComboBox3.ItemIndex <> -1) and
                (ComboBox4.ItemIndex <> -1);
      if ComboBox4.ItemIndex in [1..3] then Result := Result and (ComboBox5.ItemIndex <> -1)
      else
      if ComboBox4.ItemIndex = 4 then Result := Result and (CBTexturesUICheckChecked.ItemIndex <> -1) and
                                                (CBTexturesUICheckUnchecked.ItemIndex <> -1);
    end;
    'TUIRadio': begin
      Result := (Edit6.Text <> '') and (ComboBox6.ItemIndex <> -1) and
                (ComboBox7.ItemIndex <> -1);
      if ComboBox7.ItemIndex in [1..3] then Result := Result and (ComboBox8.ItemIndex <> -1)
      else
      if ComboBox7.ItemIndex = 4 then Result := Result and (CBTexturesUIRadioChecked.ItemIndex <> -1) and
                                                (CBTexturesUIRadioUnchecked.ItemIndex <> -1);
    end;
    'TUIScrollBar': begin
      Result := True;
    end;
    'TUIProgressBar': begin
      Result := True;
    end;
    'TUIScrollBox': begin
      Result := True;
    end;
    'TUIListBox': begin
      Result := True;
    end;
    'TUITextArea': begin
      Result := True;
    end;
  end;
end;

procedure TFrameToolUIPanelEditor.DoAddNewChild;
var n: string;
begin
  if not CheckChildWidgets then exit;

  n := Trim(Edit5.Text);
  if Surfaces.NameExists(n) then begin
    ShowMessage('Name already exists "'+n+'"');
    exit;
  end;

  if not IsValidPascalVariableName(n, True) then exit;

  FWorkingChild := Surfaces.AddEmpty;
  FWorkingChild^.parentID := CBToParentID;
  DoUpdateChild(True);

  FWorkingChild := NIL;
  FModified := True;

  ShowSelectionData(NIL);
UpdateDebugLabels;
end;

procedure TFrameToolUIPanelEditor.DoUpdateChild(aForceRecreateSurface: boolean);
var recreateSurface: Boolean;
  texname1, texName2: string;
  surf: TSimpleSurfaceWithEffect;
begin
  if FWorkingChild = NIL then exit;

  UpdateExtraPropertiesToWorkingTemporary;
  surf := FWorkingChild^.surface;

  recreateSurface := aForceRecreateSurface or
                     not (FWorkingChild^.surface is CBToClassType) or
                     (FWorkingChild^.parentID <> CBToParentID);

  // we need to recreate the surface if the size changed (to recreate the collision body)
  if FWorkingChild^.IsTextured then
    recreateSurface := recreateSurface or
                       (FWorkingChild^.surface.Width <> FWorkingChild^.width) or
                       (FWorkingChild^.surface.Height <> FWorkingChild^.height);

  // we never recreate the root panel
  if FWorkingChild^.IsRoot then recreateSurface := False;

  FWorkingChild^.name := Trim(Edit5.Text);

  if recreateSurface then begin
    // save the childs of the surface before killing it
    FWorkingChild^.SaveAndRemoveChilds;
    // create a new surface
    FWorkingChild^.KillSurface;
    FWorkingChild^.classtype := CBToClassType;
    GetSelectedTextureName(texname1, texName2);
    FWorkingChild^.textureName := texname1;
    FWorkingChild^.textureName2 := texName2;
    FWorkingChild^.CreateSurface;
    // restore the childs
    FWorkingChild^.RestoreChilds;
    // set child dependency
    FWorkingChild^.parentID := CBToParentID;
    FWorkingChild^.zOrder := SE8.Value;
    FWorkingChild^.SetChildDependency;
    FModified := True;
    // restore handle
    FWorkingChild^.ShowHandle;
  end;

  UpdateValuesToWorkingSurface;

  // in case the name was changed, we update the combobox parent
  Surfaces.ReplaceNameInComboBox(CBParent);
  ShowSelectionData(ScreenUIPanelEditor.Selected);

  // update the extra panel in case the working surface have changed its type
  if recreateSurface then ShowExtraPropertyPanel;
end;

procedure TFrameToolUIPanelEditor.UpdateExtraPropertiesToWorkingTemporary;
var selectedClass: classOfSimpleSurfaceWithEffect;
  texName1, texName2: string;
  grad: TGradientDescriptor;
begin
  // init extra properties to temporary variable
  if FWorkingChild = NIL then exit;
  selectedClass := CBToClassType;
  if FWorkingChild^.IsRoot then selectedClass := TUIPanelWithEffects;

  case selectedClass.ClassName of
    'TUIPanel', 'TUIPanelWithEffects': begin
      FWorkingChild^.width := SE12.Value;
      FWorkingChild^.height := SE13.Value;
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
    end;
    'TUIImage': begin
      FWorkingChild^.width := SE17.Value;
      FWorkingChild^.height := SE18.Value;
      GetSelectedTextureName(texName1, texName2);
      FWorkingChild^.textureName := texName1;
      FWorkingChild^.textureName2 := '';
    end;
    'TUILabel': begin
      FWorkingChild^.Caption := Edit2.Text;
      FWorkingChild^.FontDescriptorName := ComboBox1.Text;
    end;
    'TUIButton': begin
      FWorkingChild^.width := SE23.Value;
      FWorkingChild^.height := SE24.Value;
      FWorkingChild^.AutoSize := CheckBox1.Checked;
      FWorkingChild^.Caption := Edit3.Text;
      FWorkingChild^.FontDescriptorName := ComboBox2.Text;
      GetSelectedTextureName(texName1, texName2);
      FWorkingChild^.textureName := texName1;
      FWorkingChild^.textureName2 := '';
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
    end;
    'TUICheck': begin
      FWorkingChild^.Caption := Edit4.Text;
      FWorkingChild^.FontDescriptorName := ComboBox3.Text;
      FWorkingChild^.Checked := CheckBox12.Checked;
      FWorkingChild^.CheckShape := TUICheckShape(ComboBox4.ItemIndex);
      FWorkingChild^.CheckFill := TUICheckFill(ComboBox5.ItemIndex);
      FWorkingChild^.CheckColorChecked := ColorToBGRA(ColorButton2.ButtonColor, SE14.Value);
      GetSelectedTextureName(texName1, texName2);
      FWorkingChild^.textureName := texName1;
      FWorkingChild^.textureName2 := texName2;
      FWorkingChild^.CheckAdjustImageToFontHeight := CheckBox10.Checked;
    end;
    'TUIRadio': begin
      FWorkingChild^.Caption := Edit6.Text;
      FWorkingChild^.FontDescriptorName := ComboBox6.Text;
      FWorkingChild^.Checked := CheckBox13.Checked;
      FWorkingChild^.CheckShape := TUICheckShape(ComboBox7.ItemIndex);
      FWorkingChild^.CheckFill := TUICheckFill(ComboBox8.ItemIndex);
      FWorkingChild^.CheckColorChecked := ColorToBGRA(ColorButton3.ButtonColor, SE15.Value);
      GetSelectedTextureName(texName1, texName2);
      FWorkingChild^.textureName := texName1;
      FWorkingChild^.textureName2 := texName2;
      FWorkingChild^.CheckAdjustImageToFontHeight := CheckBox11.Checked;
    end;
    'TUIScrollBar': begin
      FWorkingChild^.width := SE25.Value;
      FWorkingChild^.height := SE26.Value;
      if RadioButton5.Checked then FWorkingChild^.uiOrientation := uioVertical
        else FWorkingChild^.uiOrientation := uioHorizontal;
      FWorkingChild^.scrollbarMin := SE19.Value;
      FWorkingChild^.scrollbarMax := SE20.Value;
      FWorkingChild^.scrollbarPageSize := SE21.Value;
      FWorkingChild^.scrollbarPosition := SE22.Value;
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
    end;
    'TUIProgressBar': begin
      FWorkingChild^.width := SE27.Value;
      FWorkingChild^.height := SE28.Value;
      if RadioButton3.Checked then FWorkingChild^.uiOrientation := uioVertical
        else FWorkingChild^.uiOrientation := uioHorizontal;
      FWorkingChild^.ProgressBarReversed := CheckBox9.Checked;
      FWorkingChild^.ProgressBarPercent := FSE3.Value;
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
      if FWorkingChild^.ProgressBarGradientData = '' then
        FWorkingChild^.ProgressBarGradientData := DEFAULT_GRADIENT;
    end;
    'TUIScrollBox': begin
      FWorkingChild^.width := SE29.Value;
      FWorkingChild^.height := SE30.Value;
      FWorkingChild^.scrollboxUseVScrollbar := CheckBox14.Checked;
      FWorkingChild^.scrollboxUseHScrollbar := CheckBox15.Checked;
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
    end;
    'TUIListBox': begin
      FWorkingChild^.width := SE31.Value;
      FWorkingChild^.height := SE32.Value;
      FWorkingChild^.FontDescriptorName := ComboBox10.Text;
      FWorkingChild^.listboxUnselectedTextColor := ColorToBGRA(ColorButton4.ButtonColor, SE35.Value);
      if FWorkingChild^.listboxUnselectedGradientData = '' then begin
        grad.InitDefault;
        grad.CreateSingleColor(BGRA(30,30,30));
        FWorkingChild^.listboxUnselectedGradientData := grad.SaveGradientDataToString;
      end;
      FWorkingChild^.listboxSelectedTextColor := ColorToBGRA(ColorButton6.ButtonColor, SE36.Value);
      if FWorkingChild^.listboxSelectedGradientData = '' then begin
        grad.InitDefault;
        grad.CreateSingleColor(BGRA(51,153,255));
        FWorkingChild^.listboxSelectedGradientData := grad.SaveGradientDataToString;
      end;
      FWorkingChild^.listboxMultiSelect := CheckBox16.Checked;
      if FWorkingChild^.BodyShapeData = '' then
        FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
    end;
    'TUITextArea': begin
      FWorkingChild^.width := SE33.Value;
      FWorkingChild^.height := SE34.Value;
      if FWorkingChild^.BodyShapeData = '' then FWorkingChild^.BodyShapeData := DEFAULT_BODYSHAPE;
      if FWorkingChild^.BackGradientData = '' then FWorkingChild^.BackGradientData := DEFAULT_GRADIENT;
      FWorkingChild^.FontDescriptorName := ComboBox9.Text;
      FWorkingChild^.TextAreaTextTint := ColorToBGRA(ColorButton7.ButtonColor, SE37.Value);
      FWorkingChild^.TextAlignment := TOGLCAlignment(RadioGroup1.ItemIndex);
      FWorkingChild^.TextAreaText := Memo3.Lines.Text;
    end;
    else raise exception.create('forgot to implement '+selectedClass.ClassName);
  end;
end;

procedure TFrameToolUIPanelEditor.UpdateValuesToWorkingSurface;
begin
  if FWorkingChild = NIL then exit;

  with FWorkingChild^.surface do begin
    X.Value := SE1.Value;
    Y.Value := SE2.Value;
    FWorkingChild^.Pivot := SurfaceToScene(PointF(SE3.Value*Width, SE4.Value*Height));
    Scale.x.Value := SE5.Value;
    Scale.y.Value := SE6.Value;
    Angle.Value := SE7.Value;
    SetZOrder(SE8.Value);
    Opacity.Value := SE10.Value;
    TintMode := GetWidgetTintMode;
    Tint.Value := GetWidgetTint;
    BlendMode := Byte(CBBlend.ItemIndex);

    if FWorkingChild^.IsTextured then begin
      FWorkingChild^.frameindex := SE9.Value;
      Frame := SE9.Value;
    end else begin
      FWorkingChild^.frameindex := 1;
      Frame := 1;
    end;
  end;

  FWorkingChild^.UpdateHandlePosition;
end;

procedure TFrameToolUIPanelEditor.UpdateEventCheckBox;
var flag: boolean;
begin
  FInitializing := True;
  flag := Assigned(FWorkingChild);

  CheckBox2.Enabled := flag;
  CheckBox3.Enabled := flag;
  CheckBox4.Enabled := flag;
  CheckBox5.Enabled := flag;
  CheckBox6.Enabled := flag;
  CheckBox7.Enabled := flag;
  CheckBox8.Enabled := flag;

  CheckBox17.Enabled := flag and ((FWorkingChild^.classtype = TUICheck) or
                                 (FWorkingChild^.classtype = TUIRadio) or
                                 (FWorkingChild^.classtype = TUIScrollBar));
  //if not CheckBox17.Enabled then CheckBox17.Checked := False;

  CheckBox18.Enabled := flag and (FWorkingChild^.classtype = TUIListBox);
  //if not CheckBox18.Enabled then CheckBox18.Checked := False;

  FInitializing := False;
end;

procedure TFrameToolUIPanelEditor.FillCBParent;
begin
  Surfaces.FillComboBox(CBParent); // fill combo box parent
end;

procedure TFrameToolUIPanelEditor.FillFontsComboBox;
begin
  FontBank.FillComboBoxWithNames(ComboBox1);
  FontBank.FillComboBoxWithNames(ComboBox2);
  FontBank.FillComboBoxWithNames(ComboBox3);
  FontBank.FillComboBoxWithNames(ComboBox6);
  FontBank.FillComboBoxWithNames(ComboBox10);
  FontBank.FillComboBoxWithNames(ComboBox9);
end;

procedure TFrameToolUIPanelEditor.DoClearAll;
begin
  FModified := False;
  ScreenUIPanelEditor.SelectNone;
  FWorkingChild := NIL;
  FInitializing := True;

  Surfaces.Clear;
  Textures.Clear;

  CBTexturesUIImage.Clear;
  CBTexturesUIButton.Clear;
  CBTexturesUICheckChecked.Clear;
  CBTexturesUICheckUnchecked.Clear;
  CBTexturesUIRadioChecked.Clear;
  CBTexturesUIRadioUnchecked.Clear;

  //fonts
  ComboBox1.Clear;
  ComboBox2.Clear;
  ComboBox3.Clear;
  ComboBox6.Clear;

  CBParent.Clear;

  Edit5.Text := '';
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Edit4.Text := '';
  Edit6.Text := '';

  FrameTexturesList.Clear;

  PageControl1.PageIndex := PageControl1.IndexOf(PageTextures);
  FInitializing := False;
end;

procedure TFrameToolUIPanelEditor.ShowExtraPropertyPanel;
var i: integer;
  flag: boolean;
begin
  flag := Assigned(FWorkingChild);
  if flag then flag := Assigned(FWorkingChild^.surface);

  i := CBChildType.ItemIndex;
  if i = -1 then NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageExtraEmpty)
  else
    case CBChildType.Items.Strings[i] of
      'TUIPanel': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIPanel);
        SpeedButton14.Enabled := flag;
      end;
      'TUIImage': NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIImage);
      'TUILabel': NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUILabel);
      'TUIButton': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIButton);
        SpeedButton15.Enabled := flag;
      end;
      'TUICheck': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUICheck);
        SpeedButton16.Enabled := flag;
      end;
      'TUIRadio': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIRadio);
        SpeedButton17.Enabled := flag;
      end;
      'TUIScrollBar': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIScrollBar);
        SpeedButton18.Enabled := flag;
        SpeedButton19.Enabled := flag;
      end;
      'TUIProgressBar': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIProgressBar);
        SpeedButton20.Enabled := flag;
      end;
      'TUIScrollBox': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIScrollBox);
        SpeedButton21.Enabled := flag;
      end;
      'TUIListBox': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUIListBox);
        SpeedButton22.Enabled := flag;
      end;
      'TUITextArea': begin
        NotebookExtra.PageIndex := NotebookExtra.IndexOf(PageUITextArea);
        SpeedButton23.Enabled := flag;
      end;
      else raise exception.create('forgot to implement '+CBChildType.Items.Strings[i]);
    end;
end;

procedure TFrameToolUIPanelEditor.UpdateDebugLabels;
begin
  Label2.Caption := 'Surfaces count: '+Surfaces.Size.ToString;
end;

constructor TFrameToolUIPanelEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameTexturesList := TFrameTextureList.Create(Self);
  FrameTexturesList.Parent := PageTextures;
  FrameTexturesList.Align := alClient;
  FrameTexturesList.OnModified := @ProcessTextureListOnModified;
  FrameTexturesList.OnAskToDeleteTexture := @ProcessAskToDeleteTextureEvent;
  FrameTexturesList.OnTextureChanged := @ProcessOnTextureChangedEvent;
  FrameTexturesList.OnGetTextureList := @Textures;
end;

procedure TFrameToolUIPanelEditor.SetFocusToDummy;
begin
  SEDummy.SetFocus;
end;

procedure TFrameToolUIPanelEditor.OnShow;
begin
  FModified := False;

  FrameTexturesList.FillListBox;
  FillTexturesComboBox;
  FillCBParent;
  FillFontsComboBox;

  UpdateEventCheckBox;

  UpdateDebugLabels;
end;

procedure TFrameToolUIPanelEditor.ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
var tex: PTexture;
  procedure ShowFrameIndexWidget(AValue: boolean);
  begin
    Label32.Visible := AValue;
    SE9.Visible := AValue;
  end;
begin
  FInitializing := True;
  FWorkingChild := NIL;

  Label20.Visible := Surfaces.Size = 0;

  if Length(aSelected) = 1 then begin
    // 1 selected -> we can edit its parameters
    FWorkingChild := aSelected[0];
    with FWorkingChild^ do begin
      ClassTypeToCB(classtype);
      //TexturenameToCB(textureName);
      Edit5.Text := name;
      ParentIDToCB(parentID);
      SE1.Value := surface.X.Value;
      SE2.Value := surface.Y.Value;
      SE3.Value := surface.Pivot.x;
      SE4.Value := surface.Pivot.y;
      SE5.Value := surface.Scale.x.Value;
      SE6.Value := surface.Scale.y.Value;
      SE7.Value := surface.Angle.Value;
      SE8.Value := surface.ZOrderAsChild;
      SE10.Value := Round(surface.Opacity.Value);
      ColorButton5.ButtonColor := BGRAToColor(surface.Tint.Value);
      SE11.Value := Round(surface.Tint.Alpha.Value);
      RadioButton1.Checked := surface.TintMode = tmReplaceColor;
      CBBlend.ItemIndex := surface.BlendMode;

      ShowFrameIndexWidget(False);
      SE9.Value := 1;
      SE9.MaxValue := 1;
      if IsTextured then begin
        tex := GetTextureFromTextureName;
        if tex <> NIL then begin
          ShowFrameIndexWidget(tex^.FrameCount > 1);
          SE2.Value := Trunc(surface.Frame);
          SE2.MaxValue := tex^.FrameCount-1;
        end;
      end;

      if surface is TUIPanel then begin
        SE12.Value := surface.Width;
        SE13.Value := surface.Height;
      end;
      if surface is TUIImage then begin
        SE17.Value := surface.Width;
        SE18.Value := surface.Height;
        CBTexturesUIImage.ItemIndex := Textures.GetItemIndexByName(FWorkingChild^.textureName);
      end;
      if surface is TUILabel then begin
        Edit2.Text := TUILabel(surface).Caption;
        ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(FWorkingChild^.FontDescriptorName);
      end;
      if surface is TUIButton then begin
        SE23.Value := surface.Width;
        SE24.Value := surface.Height;
        CheckBox1.Checked := TUIButton(surface).AutoSize;
        CBTexturesUIButton.ItemIndex := CBTexturesUIButton.Items.IndexOf(FWorkingChild^.textureName);
        ComboBox2.ItemIndex := ComboBox2.Items.IndexOf(FWorkingChild^.FontDescriptorName);
        Edit3.Text := TUIButton(surface).Caption;
      end;
      if surface is TUICheck then begin
        Edit4.Text := TUICheck(surface).Caption;
        ComboBox3.ItemIndex := ComboBox3.Items.IndexOf(FWorkingChild^.FontDescriptorName);
        CheckBox12.Checked := FWorkingChild^.Checked;
        ComboBox4.ItemIndex := Ord(FWorkingChild^.CheckShape);

        ComboBox5.Itemindex := Ord(FWorkingChild^.CheckFill);
        ColorButton2.ButtonColor := FWorkingChild^.CheckColorChecked.ToColor;
        SE14.Value := FWorkingChild^.CheckColorChecked.alpha;
        if FWorkingChild^.CheckShape = ctUseTexture then Notebook1.PageIndex := Notebook1.IndexOf(PageCheckTextures)
          else Notebook1.PageIndex := Notebook1.IndexOf(PageShapeCheck);
        CBTexturesUICheckChecked.ItemIndex := CBTexturesUICheckChecked.Items.IndexOf(FWorkingChild^.textureName);
        CBTexturesUICheckUnchecked.ItemIndex := CBTexturesUICheckUnchecked.Items.IndexOf(FWorkingChild^.textureName2);
        CheckBox10.Checked := FWorkingChild^.CheckAdjustImageToFontHeight;
      end;
      if surface is TUIRadio then begin
        Edit6.Text := TUIRadio(surface).Caption;
        ComboBox6.ItemIndex := ComboBox6.Items.IndexOf(FWorkingChild^.FontDescriptorName);
        CheckBox13.Checked := FWorkingChild^.Checked;
        ComboBox7.ItemIndex := Ord(FWorkingChild^.CheckShape);

        ComboBox8.Itemindex := Ord(FWorkingChild^.CheckFill);
        ColorButton3.ButtonColor := FWorkingChild^.CheckColorChecked.ToColor;
        SE15.Value := FWorkingChild^.CheckColorChecked.alpha;
        if FWorkingChild^.CheckShape = ctUseTexture then Notebook2.PageIndex := Notebook2.IndexOf(PageRadioTextures)
          else Notebook2.PageIndex := Notebook2.IndexOf(PageShapeRadio);
        CBTexturesUIRadioChecked.ItemIndex := CBTexturesUIRadioChecked.Items.IndexOf(FWorkingChild^.textureName);
        CBTexturesUIRadioUnchecked.ItemIndex := CBTexturesUIRadioUnchecked.Items.IndexOf(FWorkingChild^.textureName2);
        CheckBox11.Checked := FWorkingChild^.CheckAdjustImageToFontHeight;
      end;
      if surface is TUIScrollBar then begin
        SE25.Value := surface.Width;
        SE26.Value := surface.Height;
        RadioButton6.Checked := FWorkingChild^.uiOrientation = uioHorizontal;
        RadioButton5.Checked := FWorkingChild^.uiOrientation = uioVertical;
        SE19.Value := FWorkingChild^.scrollbarMin;
        SE20.Value := FWorkingChild^.scrollbarMax;
        SE21.Value := FWorkingChild^.scrollbarPageSize;
        SE22.Value := FWorkingChild^.scrollbarPosition;
      end;
      if surface is TUIProgressBar then begin
        SE27.Value := surface.Width;
        SE28.Value := surface.Height;
        RadioButton4.Checked := FWorkingChild^.uiOrientation = uioHorizontal;
        RadioButton3.Checked := FWorkingChild^.uiOrientation = uioVertical;
        CheckBox9.Checked := FWorkingChild^.ProgressBarReversed;
        FSE3.Value := FWorkingChild^.ProgressBarPercent;
      end;
      if surface is TUIScrollBox then begin
        SE29.Value := surface.Width;
        SE30.Value := surface.Height;
      end;
      if surface is TUIListBox then begin
        SE31.Value := surface.Width;
        SE32.Value := surface.Height;
        ComboBox10.ItemIndex := ComboBox10.Items.IndexOf(FWorkingChild^.FontDescriptorName);
        CheckBox16.Checked := FWorkingChild^.listboxMultiSelect;
        SE38.Value := FWorkingChild^.listboxItemHeight;
        ColorButton4.ButtonColor := BGRAToColor(FWorkingChild^.listboxUnselectedTextColor);
        SE35.Value := FWorkingChild^.listboxUnselectedTextColor.alpha;
        ColorButton6.ButtonColor := BGRAToColor(FWorkingChild^.listboxSelectedTextColor);
        SE36.Value := FWorkingChild^.listboxSelectedTextColor.alpha;
      end;
      if surface is TUITextArea then begin
        SE33.Value := surface.Width;
        SE34.Value := surface.Height;
        ComboBox9.ItemIndex := ComboBox9.Items.IndexOf(FWorkingChild^.FontDescriptorName);
        ColorButton7.ButtonColor := FWorkingChild^.TextAreaTextTint.ToColor;
        SE37.Value := FWorkingChild^.TextAreaTextTint.alpha;
        RadioGroup1.ItemIndex := Ord(FWorkingChild^.TextAlignment);
        Memo3.Text := FWorkingChild^.TextAreaText;
      end;


    end;
    CBChildType.Enabled := True;
    Edit5.Enabled := True;
    CBParent.Enabled := True;
    Panel6.Enabled := True;
    BNewChild.Enabled := False;
    ShowExtraPropertyPanel;
    UpdateEventCheckBox;
  end
  else
  if Length(aSelected) > 1 then begin
    // several selected -> we can not edit the parameters
    CBChildType.Enabled := False;
    Edit5.Enabled := False;
    CBParent.Enabled := False;
    CBChildType.ItemIndex := -1;
    Edit5.Text := '';
    CBParent.ItemIndex := -1;
    Panel6.Enabled := False;
    BNewChild.Enabled := False;
    ShowExtraPropertyPanel;
    UpdateEventCheckBox;
  end
  else begin
    // 0 selected -> reset parameters, enable them and activate the button 'ADD'
    CBChildType.ItemIndex := -1;
    Edit5.Text := '';
    CBParent.ItemIndex := -1;
    Panel6.Enabled := True;
    SE100.Value := 0;
    SE1.Value := 0;
    SE3.Value := 0.5;
    SE4.Value := 0.5;
    SE5.Value := 1.0;
    SE6.Value := 1.0;
    SE7.Value := 0.0;
    SE8.Value := 0;
    BNewChild.Enabled := True;
    ShowFrameIndexWidget(False);
    ShowExtraPropertyPanel;
    UpdateEventCheckBox;
  end;

  FInitializing := False;
end;

procedure TFrameToolUIPanelEditor.EditNewPanel;
begin
  // create the root panel
  //TargetUIPanel := TUIPanelWithEffects(Surfaces.GetRootItem^.surface); //.AddMainPanel('Panel1', FScene.Width div 3, FScene.Height div 4);
  OnShow;
end;

procedure TFrameToolUIPanelEditor.EditPanelFromBank(aItem: TPanelDescriptorItem);
begin
  FScene.LogInfo('Start editing panel '+aItem._Name);

{  Textures.LoadFromString(aItem.textures);
  Surfaces.LoadFromString(aItem.surfaces); }

//  TargetUIPanel := TUIPanelWithEffects(Surfaces.GetRootItem^.surface);  //ScreenUIPanelEditor.UIPanel;
//  ScreenUIPanelEditor.UIPanel := TargetUIPanel;

  FInitializing := True;
  if aItem.IsModal then RBOptionModal.Checked := True
    else RBOptionNormal.Checked := True;
  CBDarkenBG.Checked := aItem.DarkenBG;
  ColorButton1.ButtonColor := aItem.DarknessColor.ToColor;
  SE100.Value := aItem.DarknessColor.alpha;
  Edit1.Text := aItem._Name;

  FInitializing := False;

  OnShow;
//  ScreenUIPanelEditor.ZoomOnScene;
end;

procedure TFrameToolUIPanelEditor.ProcessEndResize;
begin
  DoUpdateChild(True);
end;

procedure TFrameToolUIPanelEditor.FillTexturesComboBox;
begin
  Textures.FillComboBox(CBTexturesUIImage);
  Textures.FillComboBox(CBTexturesUIButton);
  Textures.FillComboBox(CBTexturesUICheckChecked);
  Textures.FillComboBox(CBTexturesUICheckUnchecked);
  Textures.FillComboBox(CBTexturesUIRadioChecked);
  Textures.FillComboBox(CBTexturesUIRadioUnchecked);
end;

end.

