unit frame_tool_leveleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Spin,
  Dialogs, Buttons,
  BGRABitmap, BGRABitmapTypes,
  frame_texturelist, u_texture_list, u_surface_list;

type

  { TFrameToolLevelEditor }

  TFrameToolLevelEditor = class(TFrame)
    BAddToLevelBank: TSpeedButton;
    BCancel: TSpeedButton;
    BNewSurface: TSpeedButton;
    CBFlipH: TCheckBox;
    CBFlipV: TCheckBox;
    CBTextures: TComboBox;
    ColorButton1: TColorButton;
    CBAlignReference: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    PC1: TPageControl;
    PageTextures: TTabSheet;
    PageScreen: TTabSheet;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SE1: TFloatSpinEdit;
    SE2: TFloatSpinEdit;
    SE4: TSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    SE8: TSpinEdit;
    SE9: TSpinEdit;
    SE3: TSpinEdit;
    BDebug: TSpeedButton;
    BResetSize: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    procedure BAddToLevelBankClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BDebugClick(Sender: TObject);
    procedure BNewSurfaceClick(Sender: TObject);
    procedure CBTexturesSelect(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FModified: boolean;
    FInitializingWidget: boolean;
  private // textures
    procedure ProcessTextureListOnModified(Sender: TObject);
    procedure ProcessAskToDeleteTextureEvent(aTextureItem: PTextureItem; var aCanDelete: boolean);
    procedure ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
    function Textures: TTextureList;
    procedure TexturenameToCB(aName: string);
    function CBToTextureName: string;
  private // surfaces
    FWorkingChild: PSurfaceDescriptor;
    function Surfaces: TSurfaceList;
    procedure DoClearAll;
    function CheckSurfaceWidgets: boolean;
    function DoAddNewSurface: boolean;
    procedure DoUpdateSurface(aForceRecreateSurface: boolean);
    procedure UpdateValuesToWorkingSurface;
  public
    FrameTextureList: TFrameTextureList;
    constructor Create(TheOwner: TComponent); override;
    procedure OnShow;

    procedure FillListBoxTextureNames;
    procedure ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);

    procedure EditLevelInLevelBank(const aName: string);
    property Modified: boolean read FModified write FModified;
  end;

implementation

uses u_levelbank, u_screen_leveleditor, form_main, u_project, Graphics,
  OGLCScene;

{$R *.lfm}

{ TFrameToolLevelEditor }

procedure TFrameToolLevelEditor.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('','If you leave, changes will be lost.'+LineEnding+'Continue ?', mtWarning,
                   [mrOk, 'Leave', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  DoClearAll;
  FormMain.ShowPageLevelBank;
end;

procedure TFrameToolLevelEditor.BDebugClick(Sender: TObject);
begin
  Label6.Caption := 'surface count: '+Surfaces.Size.ToString;
end;

procedure TFrameToolLevelEditor.BAddToLevelBankClick(Sender: TObject);
var o: PLevelBankItem;
  nam: string;
begin
  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if Textures.Size = 0 then exit;
  if Surfaces.Size = 0 then exit;

  o := LevelBank.GetItemByName(nam);
  if o <> NIL then
    if QuestionDlg('','a level named "'+nam+'" already exists in the bank.'+LineEnding+
                'Would you like to replace it ?',
                mtWarning, [mrOk, 'Replace', mrCancel, 'Cancel'],0) = mrCancel then exit;

  if o = NIL then o := LevelBank.AddEmpty;
  o^.name := nam;
  o^.surfaces := Surfaces.SaveToString;

  Project.SetModified;
  Project.Save;
  Modified := False;

  DoClearAll;
  FormMain.ShowPageLevelBank;
end;

procedure TFrameToolLevelEditor.BNewSurfaceClick(Sender: TObject);
begin
  if Sender = BNewSurface then begin
    if DoAddNewSurface then
      ShowSelectionData(ScreenLevelEditor.Selected);
  end;
end;

procedure TFrameToolLevelEditor.CBTexturesSelect(Sender: TObject);
var
  pTexItem: PTextureItem;
begin
  if FInitializingWidget then exit;

  if Sender = CBTextures then begin
    pTexItem := Textures.GetItemByName(CBToTextureName);
    if pTexItem <> NIL then begin
      SE3.Value := pTexItem^.texture^.FrameWidth;
      SE4.Value := pTexItem^.texture^.FrameHeight;
    end;
  end;

  if ScreenLevelEditor.SelectedCount = 0 then exit;

  if (Sender = SE1) or (Sender = SE2) then
    ScreenLevelEditor.SetPositionOnSelection(SE1.Value, SE2.Value);

  if (Sender = SE3) or (Sender = SE4) then
    ScreenLevelEditor.SetSizeOnSelection(SE3.Value, SE4.Value);

  if Sender = BResetSize then
    ScreenLevelEditor.SetOriginalSizeOnSelection;

  if (Sender = SE5) or (Sender = SE6) then
    ScreenLevelEditor.SetPivotValuesOnSelection(SE5.Value, SE6.Value);

  if Sender = SE7 then
    ScreenLevelEditor.SetAngleOnSelection(SE7.Value);

  if Sender = SE8 then
    ScreenLevelEditor.SetOpacityOnSelection(SE8.Value);

  if Sender = CBFlipH then
    ScreenLevelEditor.SetFlipHOnSelection(CBFlipH.Checked);

  if Sender = CBFlipV then
    ScreenLevelEditor.SetFlipVOnSelection(CBFlipV.Checked);

  if (Sender = ColorButton1) or (Sender = SE9) then
    ScreenLevelEditor.SetTintOnSelection(ColorToBGRA(ColorButton1.ButtonColor, SE9.Value));

  if (Sender = RadioButton1) or (Sender = RadioButton2) then
    if RadioButton1.Checked then ScreenLevelEditor.SetTintModeOnSelection(tmReplaceColor)
      else ScreenLevelEditor.SetTintModeOnSelection(tmMixColor);
end;

procedure TFrameToolLevelEditor.SpeedButton1Click(Sender: TObject);
begin
  if CBAlignReference.ItemIndex = -1 then exit;

  // align to the first selected
  if CBAlignReference.ItemIndex = 0 then begin
    if ScreenLevelEditor.SelectedCount < 2 then exit;
    with ScreenLevelEditor do
      case TSpeedButton(Sender).ImageIndex of
        0: AlignSelectedRightTo(GetFirstSelectedX);
        1: AlignSelectedRightTo(GetFirstSelectedCenterX);
        2: AlignSelectedLeftTo(GetFirstSelectedX);
        3: AlignSelectedHCenterTo(GetFirstSelectedCenterX);
        4: AlignSelectedRightTo(GetFirstSelectedRightX);
        5: AlignSelectedLeftTo(GetFirstSelectedCenterX);
        6: AlignSelectedLeftTo(GetFirstSelectedRightX);

        7: AlignSelectedBottomTo(GetFirstSelectedY);
        8: AlignSelectedBottomTo(GetFirstSelectedCenterY);
        9: AlignSelectedTopTo(GetFirstSelectedY);
        10: AlignSelectedVCenterTo(GetFirstSelectedCenterY);
        11: AlignSelectedBottomTo(GetFirstSelectedBottomY);
        12: AlignSelectedTopTo(GetFirstSelectedCenterY);
        13: AlignSelectedTopTo(GetFirstSelectedBottomY);
      end;
  end;
end;

procedure TFrameToolLevelEditor.ProcessTextureListOnModified(Sender: TObject);
begin
  FModified := True;
  Textures.FillComboBox(CBTextures);
end;

procedure TFrameToolLevelEditor.ProcessAskToDeleteTextureEvent(
  aTextureItem: PTextureItem; var aCanDelete: boolean);
begin
  if Length(Surfaces.GetItemsThatUseThisTexture(aTextureItem)) > 0 then begin
    aCanDelete := False;
    ShowMessage('This texture is used by a surface, you can not delete it');
  end;
end;

procedure TFrameToolLevelEditor.ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
var surf: ArrayOfPSurfaceDescriptor;
  i: Integer;
begin
  // recreate the surfaces that use this texture (if any)
  surf := Surfaces.GetItemsThatUseThisTexture(aTextureItem);
  if Length(surf) <> 0 then
    for i:=0 to High(surf) do
      surf[i]^.RecreateSurfaceBecauseTextureChanged;
end;

function TFrameToolLevelEditor.Textures: TTextureList;
begin
  Result := LevelBank.Textures;
end;

procedure TFrameToolLevelEditor.TexturenameToCB(aName: string);
begin
  CBTextures.ItemIndex := Textures.GetItemIndexByName(aName);
end;

function TFrameToolLevelEditor.CBToTextureName: string;
begin
  if CBTextures.ItemIndex = -1 then Result := ''
    else Result := CBTextures.Items.Strings[CBTextures.ItemIndex];
end;

function TFrameToolLevelEditor.Surfaces: TSurfaceList;
begin
  Result := ScreenLevelEditor.Surfaces;
end;

procedure TFrameToolLevelEditor.DoClearAll;
begin
  FModified := False;
  ScreenLevelEditor.SelectNone;
  FWorkingChild := NIL;
  FInitializingWidget := True;

  Surfaces.Clear;

  CBTextures.Clear;

  PC1.PageIndex := PC1.IndexOf(PageScreen);
  FInitializingWidget := False;
end;

function TFrameToolLevelEditor.CheckSurfaceWidgets: boolean;
begin
  Result := (CBTextures.ItemIndex <> -1);
end;

function TFrameToolLevelEditor.DoAddNewSurface: boolean;
begin
  Result := False;
  if not CheckSurfaceWidgets then exit;

  FWorkingChild := Surfaces.AddEmpty;
  DoUpdateSurface(False);
  ScreenLevelEditor.AddToSelected([FWorkingChild]);

  //FWorkingChild := NIL;
  FModified := True;
  Result := True;
end;

procedure TFrameToolLevelEditor.DoUpdateSurface(aForceRecreateSurface: boolean);
var recreateSurface: Boolean;
begin
  if FWorkingChild = NIL then exit;

  recreateSurface := aForceRecreateSurface or
                     (FWorkingChild^.textureName <> CBToTextureName);

  if recreateSurface then begin
    // create a new surface
    FWorkingChild^.KillSurface;
    FWorkingChild^.classtype := TSprite;
    FWorkingChild^.textureName := CBToTextureName;

//tex := FWorkingChild^.GetTextureFromTextureName;
FWorkingChild^.width := SE3.Value;
FWorkingChild^.height := SE4.Value;
FWorkingChild^.parentID := -1;
FWorkingChild^.name := '';

    FWorkingChild^.CreateSurface;
FWorkingChild^.SetChildDependency;
    FModified := True;
  end;

  UpdateValuesToWorkingSurface;
end;

procedure TFrameToolLevelEditor.UpdateValuesToWorkingSurface;
begin
  if FWorkingChild = NIL then exit;

  with TSprite(FWorkingChild^.surface) do begin
    X.Value := SE1.Value;
    Y.Value := SE2.Value;
    FWorkingChild^.Pivot := SurfaceToScene(PointF(SE5.Value*Width, SE6.Value*Height));
    SetSize(SE3.Value, SE4.Value);
    Angle.Value := SE7.Value;
    Opacity.Value := SE8.Value;
    FlipH := CBFlipH.Checked;
    FlipV := CBFlipV.Checked;
    Tint.Value := ColorToBGRA(ColorButton1.ButtonColor, SE9.Value);
    if RadioButton1.Checked then TintMode := tmReplaceColor
      else TintMode := tmMixColor;
  end;
  FWorkingChild^.UpdateHandlePosition;
end;

constructor TFrameToolLevelEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameTextureList := TFrameTextureList.Create(Self);
  FrameTextureList.Parent := PageTextures;
  FrameTextureList.Align := alClient;
  FrameTextureList.OnModified := @ProcessTextureListOnModified;
  FrameTextureList.OnAskToDeleteTexture := @ProcessAskToDeleteTextureEvent;
  FrameTextureList.OnTextureChanged := @ProcessOnTextureChangedEvent;
  FrameTextureList.OnGetTextureList := @Textures;
end;

procedure TFrameToolLevelEditor.OnShow;
begin
  FillListBoxTextureNames;
  Textures.FillComboBox(CBTextures);

  ShowSelectionData(NIL);
  PC1.PageIndex := PC1.IndexOf(PageScreen);

  FrameTextureList.UpdateTextureWidgetState;
  //UpdateScreenWidgetState;
end;

procedure TFrameToolLevelEditor.FillListBoxTextureNames;
begin
  FInitializingWidget := True;
  FrameTextureList.FillListBox;
  Textures.FillComboBox(CBTextures);
  FInitializingWidget := False;
end;

procedure TFrameToolLevelEditor.ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
begin
  FInitializingWidget := True;
  FWorkingChild := NIL;

  if Length(aSelected) = 1 then begin
    // 1 selected -> we can edit its parameters
    FWorkingChild := aSelected[0];
    with FWorkingChild^ do begin

      TexturenameToCB(textureName);
      SE1.Value := surface.X.Value;
      SE2.Value := surface.Y.Value;
      SE3.Value := surface.Width;
      SE4.Value := surface.Height;
      SE5.Value := surface.Pivot.x;
      SE6.Value := surface.Pivot.y;
      SE7.Value := surface.Angle.Value;
      SE8.Value := surface.Opacity.Value;
      CBFlipH.Checked := surface.FlipH;
      CBFlipV.Checked := surface.FlipV;
      ColorButton1.ButtonColor := surface.Tint.Value.ToColor;
      SE9.Value := Round(surface.Tint.Alpha.Value);
      case surface.TintMode of
        tmMixColor: RadioButton2.Checked := True;
        tmReplaceColor: RadioButton1.Checked := True;
      end;
    end;
    CBTextures.Enabled := True;
    Panel2.Visible := True;
    BNewSurface.Enabled := False;
  end
  else
  if Length(aSelected) > 1 then begin
    // several selected -> we can not edit the parameters
    CBTextures.Enabled := False;
    CBTextures.ItemIndex := -1;
    Panel2.Visible := False;
    BNewSurface.Enabled := False;
  end
  else begin
    // 0 selected -> reset parameters, enable them and activate the button 'ADD'
    CBTextures.ItemIndex := -1;
    Panel2.Visible := True;
    SE1.Value := 0;
    SE2.Value := 0;
    SE3.Value := 100;
    SE4.Value := 100;
    SE5.Value := 0.5;
    SE6.Value := 0.5;
    SE7.Value := 0.0;
    SE8.Value := 255;
    CBFlipH.Checked := False;
    CBFlipV.Checked := False;
    ColorButton1.ButtonColor := clBlack;
    SE9.Value := 0;
    RadioButton1.Checked := True;
    BNewSurface.Enabled := True;
  end;

  FInitializingWidget := False;
end;

procedure TFrameToolLevelEditor.EditLevelInLevelBank(const aName: string);
var o: PLevelBankItem;
begin
  o := LevelBank.GetItemByName(aName);
  if o = NIL then exit;

  Edit1.Text := o^.name;
  Surfaces.LoadFromString(o^.surfaces);
  FModified := False;
end;

end.

