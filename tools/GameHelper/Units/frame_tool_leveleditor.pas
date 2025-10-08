unit frame_tool_leveleditor;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Spin,
  Dialogs, Buttons,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes,
  frame_texturelist, u_texture_list, u_surface_list, frame_viewlayerlist,
  lcl_utils;

type

  { TFrameToolLevelEditor }

  TFrameToolLevelEditor = class(TFrame)
    BAddToLevelBank: TSpeedButton;
    BCancel: TSpeedButton;
    BHelpWorld: TSpeedButton;
    BHelpSurfaces: TSpeedButton;
    BHelpLayers: TSpeedButton;
    BNewSurface: TSpeedButton;
    BZoomOnSelection: TSpeedButton;
    CBFlipH: TCheckBox;
    CBFlipV: TCheckBox;
    CBTextures: TComboBox;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    CBAlignReference: TComboBox;
    ColorButton2: TColorButton;
    CBLayers: TComboBox;
    Edit1: TEdit;
    FSEOverlap: TFloatSpinEdit;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PC1: TPageControl;
    PageTextures: TTabSheet;
    PageSurfaces: TTabSheet;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SE1: TFloatSpinEdit;
    SE10: TSpinEdit;
    SE2: TFloatSpinEdit;
    SE4: TSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    SE8: TSpinEdit;
    SE9: TSpinEdit;
    SE3: TSpinEdit;
    BResetSize: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    BRotate90CCW: TSpeedButton;
    BRotate90CW: TSpeedButton;
    BMirrorH: TSpeedButton;
    BMirrorV: TSpeedButton;
    BMoveToTop: TSpeedButton;
    BAddMultiple: TSpeedButton;
    BDistributeH: TSpeedButton;
    BDistributeV: TSpeedButton;
    SpeedButton2: TSpeedButton;
    BMoveToTopOneStep: TSpeedButton;
    BMoveToBottomOneStep: TSpeedButton;
    BMoveToBottom: TSpeedButton;
    BZoomAll: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpinEdit1: TFloatSpinEdit;
    SpinEdit2: TFloatSpinEdit;
    SpinEdit3: TFloatSpinEdit;
    SpinEdit4: TFloatSpinEdit;
    PageWorld: TTabSheet;
    PageLayers: TTabSheet;
    SEDummy: TSpinEdit;
    procedure BAddToLevelBankClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BDistributeHClick(Sender: TObject);
    procedure BHelpLayersClick(Sender: TObject);
    procedure BHelpSurfacesClick(Sender: TObject);
    procedure BHelpWorldClick(Sender: TObject);
    procedure BNewSurfaceClick(Sender: TObject);
    procedure BZoomAllClick(Sender: TObject);
    procedure CBTexturesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBTexturesSelect(Sender: TObject);
    procedure BRotate90CCWClick(Sender: TObject);
    procedure FSEOverlapChange(Sender: TObject);
    procedure SE1Enter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
  private
    FModified: boolean;
    FInitializingWidget: boolean;
    ToggleSpeedButtonManager: TToggleSpeedButtonManager;
    procedure CheckCreationOfSpriteMultiple;
  private // textures
    procedure ProcessTextureListOnModified(Sender: TObject);
    procedure ProcessAskToDeleteTextureEvent(aTextureItem: PTextureItem; var aCanDelete: boolean);
    procedure ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
    function Textures: TTextureList;
    procedure LayerIndexToCB(aIndex: integer);
    function CBToLayerIndex: integer;
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
  private
    function GetOverlapValue: single;
  public
    FrameTextureList: TFrameTextureList;
    FrameViewLayerList: TFrameViewLayerList;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    // create a TSprite instance with the current parameters
    function GetSpriteForAddMultiple: TSprite;
    procedure SetCenterCoordinates(aX, aY: single);
    procedure AddSurfaceFromCurrentData;
    procedure ExitModeAddMultiple;

    procedure SendParamToWorldBounds;

    procedure SetFocusToDummy;


    procedure FillListBoxTextureNames;
    procedure ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);

    procedure EditLevelInLevelBank(const aName: string);
    property Modified: boolean read FModified write FModified;

    property OverlapValue: single read GetOverlapValue;
  end;

implementation

uses u_levelbank, u_screen_leveleditor, form_main, u_project, u_common,
  u_layerlist, form_showhelp, Graphics;

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

procedure TFrameToolLevelEditor.BDistributeHClick(Sender: TObject);
begin
  if Sender = BDistributeH then ScreenLevelEditor.DistributeSelectionHorizontalyWithSameSpacing;
  if Sender = BDistributeV then ScreenLevelEditor.DistributeSelectionVerticalyWithSameSpacing;
end;

procedure TFrameToolLevelEditor.BHelpLayersClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('This the layer list for your project.'#10+
  'You can show or hide a layer by clicking its eye icon.'#10+
  'Hidden layers are still exported to the Pascal unit.');
end;

procedure TFrameToolLevelEditor.BHelpSurfacesClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('After added some textures, start to construct the level.'#10+
     'TO ADD A SURFACE:'#10+
     ' - select a layer and a texture.'#10+
     ' - sets the values for coordinates, size, etc... or keep the default.'#10+
     ' - Click ''+'' button to add one instance of the surface at the specified coordinates.'#10+
     ' OR'#10+
     '   Click ''+Multiple'' button, then move the mouse on desired position and press ''A'' or LEFT click to add an instance of the surface. Repeat as you need.'#10+
     '   Press ''ESC'' key or RIGHT click when you''ve finished.'#10#10+
     'SELECTION:'#10+
     ' Left click on a surface to select it.'#10+
     ' Left click + SHIFT key to add/remove a surface to the current selection.'#10+
     ' Left click on empty place then drag the mouse to draw a rectangle. The surfaces inside the rectangle are selected.'#10+
     ' Left click + ALT key to alternate between overlaped surfaces.'#10#10+
     'When a surface is selected, another click on it will switch between sizing handles and rotating handles.'#10+
     'If the surface is hidden, select it with ALT key then press R key to switch sizing/rotating handles.'#10#10+
     'When a surface is selected, you can modify its pivot dragging the small cross at its center or directly by entering x and y values in the right panel.'#10#10+
     'MODIFY A SURFACE:'#10+
     ' select a surface then modify its parameters (layer, coor, size, etc...). Texture can''t be changed.'#10#10+
     'MOVE A SURFACE:'#10+
     ' simply drag the surface with the mouse.'#10+
     ' Hold CTRL key to moves only horizontaly or only verticaly (the first that appears win).'#10#10+
     'ROTATE A SURFACE:'#10+
     ' select a surface, click another time on it then drag one of the rotate handles.'#10+
     ' Hold CTRL key to rotate by step of 15°.'#10#10+
     'RESIZE A SURFACE:'#10+
     ' select a surface then drag one of the resize handles.'#10+
     ' Hold CTRL key to keep aspect ratio.'#10#10+
     'ALIGN, ROTATE 90, MIRROR, DISTRIBUTE:'#10+
     ' select one or several surfaces then click the appropriate button.'#10+
     ' NOTE: for some align button only, the overlap value specify the pixel overlaping when surfaces are aligned. This is usefull to avoid artifact when the game is running.'#10#10+
     'DUPLICATE:'#10+
     ' CTRL + D to duplicate the selected surfaces at the same place.'#10+
     ' CTRL + LEFT key to duplicate the selected on their left (overlap value is used).'#10+
     ' CTRL + RIGHT key to duplicate the selected on their right (overlap value is used).'#10+
     ' CTRL + UP key to duplicate the selected on their top (overlap value is used).'#10+
     ' CTRL + DOWN key to duplicate the selected on their bottom (overlap value is used).'#10#10+
     'VIEW:'#10+
     ' Mouse wheel to scroll up/down.'#10+
     ' Mouse wheel + SHIFT key to scroll left/right.'#10+
     ' Mouse wheel + CTRL key to zoom in/out.'#10#10+
     'SAVE THE LEVEL: enter the name of the level then click ''Save to Level Bank''.'#10#10+
     'EXIT THE LEVEL EDITOR WITHOUT SAVING: click on the red cross at top right of the panel.');

end;

procedure TFrameToolLevelEditor.BHelpWorldClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('Here you can define the world size for this level.'#10+
  'The world is a rectangular area that is exported when you export the game levels to a Pascal unit and '+
  'you can read its values in your game, for example to set the bounds of a camera.'#10+
  'So it''s important to define this area.'#10#10+
  'Click on ''Compute from surfaces'' to adjust the coord and size according to the area occupied by the surfaces on the screen.'#10+
  'Or simply fill the values with what you need.');
end;

procedure TFrameToolLevelEditor.BAddToLevelBankClick(Sender: TObject);
var o: PLevelBankItem;
  nam: string;
  wi: TWorldInfo;
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

  wi.InitDefault;
  wi.x := SpinEdit3.Value;
  wi.y := SpinEdit4.Value;
  wi.width := SpinEdit1.Value;
  wi.height := SpinEdit2.Value;
  wi.boundscolor := ColorToBGRA(ColorButton2.ButtonColor);
  wi.showbounds := CheckBox1.Checked;
  o^.worldinfo := wi.SaveToString;

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

  // disable 'add multiple' if the param aren't correct
  if Sender = BAddMultiple then begin
    with ToggleSpeedButtonManager do
      Checked[BAddMultiple] := Checked[BAddMultiple] and CheckSurfaceWidgets;
    CheckCreationOfSpriteMultiple;
  end;
end;

procedure TFrameToolLevelEditor.BZoomAllClick(Sender: TObject);
begin
  if Sender = BZoomAll then ScreenLevelEditor.ZoomAll;
  if Sender = BZoomOnSelection then ScreenLevelEditor.ZoomOnSelection;
end;

procedure TFrameToolLevelEditor.CBTexturesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Key := 0;
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

      Label28.Visible := pTexItem^.isMultiFrame;
      SE10.Visible := Label28.Visible;
      SE10.MaxValue := pTexItem^.framecount;
      SE10.MinValue := 1;
    end;
  end;

  if Sender = CBLayers then begin
   ScreenLevelEditor.MoveSelectionToLayer(CBToLayerIndex);
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

  if Sender = SE10 then
    ScreenLevelEditor.SetFrameIndexOnSelection(SE10.Value);
end;

procedure TFrameToolLevelEditor.BRotate90CCWClick(Sender: TObject);
begin
  if ScreenLevelEditor.SelectedCount = 0 then exit;

  case TSpeedButton(Sender).ImageIndex of
    14: ScreenLevelEditor.RotateSelectedCCW;
    15: ScreenLevelEditor.RotateSelectedCW;
    16: ScreenLevelEditor.MirrorSelectedH;
    17: ScreenLevelEditor.MirrorSelectedV;
    18: ScreenLevelEditor.SelectedToTop;
    19: ScreenLevelEditor.SelectedToTopOneStep;
    20: ScreenLevelEditor.SelectedToBackOneStep;
    21: ScreenLevelEditor.SelectedToBack;
  end;

end;

procedure TFrameToolLevelEditor.FSEOverlapChange(Sender: TObject);
begin
  Project.Config.LevelEditorOverlap := FSEOverlap.Value;
  Project.SetModified;
end;

procedure TFrameToolLevelEditor.SE1Enter(Sender: TObject);
begin
  LastClickedIsControl := True;
end;

procedure TFrameToolLevelEditor.SpeedButton1Click(Sender: TObject);
var ref: PSurfaceDescriptor;
begin
  if CBAlignReference.ItemIndex = -1 then exit;
  if ScreenLevelEditor.SelectedCount < 2 then exit;
  ref := NIL;
  case CBAlignReference.ItemIndex of
    0: ref := ScreenLevelEditor.Selected[0]; // align to the first selected
    1: ref := ScreenLevelEditor.Selected[ScreenLevelEditor.SelectedCount-1]; // align to the last selected
    else raise exception.create('forgot to implement');
  end;

  with ScreenLevelEditor do
    case TSpeedButton(Sender).ImageIndex of
      0: AlignSelectedRightTo(GetRefBoundsLeft(ref) + FSEOverlap.Value, ref);
      1: AlignSelectedRightTo(GetRefBoundsCenterH(ref), ref);
      2: AlignSelectedLeftTo(GetRefBoundsLeft(ref), ref);
      3: AlignSelectedHCenterTo(GetRefBoundsCenterH(ref), ref);
      4: AlignSelectedRightTo(GetRefBoundsRight(ref), ref);
      5: AlignSelectedLeftTo(GetRefBoundsCenterH(ref), ref);
      6: AlignSelectedLeftTo(GetRefBoundsRight(ref) - FSEOverlap.Value, ref);

      7: AlignSelectedBottomTo(GetRefBoundsTop(ref) + FSEOverlap.Value, ref);
      8: AlignSelectedBottomTo(GetRefBoundsCenterV(ref), ref);
      9: AlignSelectedTopTo(GetRefBoundsTop(ref), ref);
      10: AlignSelectedVCenterTo(GetRefBoundsCenterV(ref), ref);
      11: AlignSelectedBottomTo(GetRefBoundsBottom(ref), ref);
      12: AlignSelectedTopTo(GetRefBoundsCenterV(ref), ref);
      13: AlignSelectedTopTo(GetRefBoundsBottom(ref) - FSEOverlap.Value, ref);
    end;
end;

procedure TFrameToolLevelEditor.SpinEdit3Change(Sender: TObject);
var r: TRectF;
begin
  if FInitializingWidget then exit;
  if Sender = SpeedButton23 then begin
    r := Surfaces.GetItemsBounds;
    SpinEdit3.Value := r.Left;
    SpinEdit4.Value := r.Top;
    SpinEdit1.Value := r.Width;
    SpinEdit2.Value := r.Height;
  end;

  SendParamToWorldBounds;
  FModified := True;
end;

procedure TFrameToolLevelEditor.CheckCreationOfSpriteMultiple;
begin
  with ToggleSpeedButtonManager do begin
    if Checked[BAddMultiple] then ScreenLevelEditor.EnterModeAddMultiple;
    Checked[BAddMultiple] := Checked[BAddMultiple] and ScreenLevelEditor.IsInModeAddMultiple;
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

procedure TFrameToolLevelEditor.LayerIndexToCB(aIndex: integer);
begin
  CBLayers.ItemIndex := aIndex - APP_LAYER_COUNT;
end;

function TFrameToolLevelEditor.CBToLayerIndex: integer;
begin
  Result := CBLayers.ItemIndex + APP_LAYER_COUNT;
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

  PC1.PageIndex := PC1.IndexOf(PageSurfaces);
  FInitializingWidget := False;
end;

function TFrameToolLevelEditor.CheckSurfaceWidgets: boolean;
begin
  Result := (CBLayers.ItemIndex <> -1) and (CBTextures.ItemIndex <> -1);
end;

function TFrameToolLevelEditor.DoAddNewSurface: boolean;
begin
  Result := False;
  if not CheckSurfaceWidgets then exit;

  FWorkingChild := Surfaces.AddEmpty;
  DoUpdateSurface(False);
  ScreenLevelEditor.AddToSelected([FWorkingChild]);
  ScreenLevelEditor.ShowHintTextOnSelected('Added');

  //FWorkingChild := NIL;
  FModified := True;
  Result := True;
end;

procedure TFrameToolLevelEditor.DoUpdateSurface(aForceRecreateSurface: boolean);
var recreateSurface: Boolean;
begin
  if FWorkingChild = NIL then exit;

  if FWorkingChild^.IsTextured then
    recreateSurface := aForceRecreateSurface or
                       (FWorkingChild^.textureName <> CBToTextureName);

  if recreateSurface then begin
    // create a new surface
    FWorkingChild^.KillSurface;
    FWorkingChild^.classtype := TSprite;
    if FWorkingChild^.IsTextured then
      FWorkingChild^.textureName := CBToTextureName;
    FWorkingChild^.layerindex := CBToLayerIndex;

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
    Frame := SE10.Value;
    Tint.Value := ColorToBGRA(ColorButton1.ButtonColor, SE9.Value);
    if RadioButton1.Checked then TintMode := tmReplaceColor
      else TintMode := tmMixColor;
  end;
  FWorkingChild^.UpdateHandlePosition;
end;

function TFrameToolLevelEditor.GetOverlapValue: single;
begin
  Result := FSEOverlap.Value;
end;

procedure TFrameToolLevelEditor.SendParamToWorldBounds;
begin
  ScreenLevelEditor.UpdateWorldBounds(SpinEdit3.Value, SpinEdit4.Value,
                                      SpinEdit1.Value, SpinEdit2.Value,
                                      ColorButton2.ButtonColor, CheckBox1.Checked);
end;

procedure TFrameToolLevelEditor.SetFocusToDummy;
begin
  SEDummy.SetFocus;
end;

constructor TFrameToolLevelEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FInitializingWidget := True;
  SpinEdit1.Value := FScene.Width;
  SpinEdit2.Value := FScene.Height;

  SpinEdit3.Value := 0;
  SpinEdit4.Value := 0;
  FInitializingWidget := False;

  FrameTextureList := TFrameTextureList.Create(Self);
  FrameTextureList.Parent := PageTextures;
  FrameTextureList.Align := alClient;
  FrameTextureList.OnModified := @ProcessTextureListOnModified;
  FrameTextureList.OnAskToDeleteTexture := @ProcessAskToDeleteTextureEvent;
  FrameTextureList.OnTextureChanged := @ProcessOnTextureChangedEvent;
  FrameTextureList.OnGetTextureList := @Textures;

  FrameViewLayerList := TFrameViewLayerList.Create(Self);
  FrameViewLayerList.Parent := Panel12;
  FrameViewLayerList.Align := alClient;

  ToggleSpeedButtonManager := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager.ToggleType := tsbLikeCheckBox;
  ToggleSpeedButtonManager.SetActivatedColors(RGBToColor(255,255,0), clBlack);
  ToggleSpeedButtonManager.SetDeactivatedColors($00ADADAD, clBlack);
  ToggleSpeedButtonManager.Add(BAddMultiple, False);

end;

destructor TFrameToolLevelEditor.Destroy;
begin
  ToggleSpeedButtonManager.Free;
  ToggleSpeedButtonManager := NIL;
  inherited Destroy;
end;

procedure TFrameToolLevelEditor.OnShow;
begin
  FInitializingWidget := True;

  FillListBoxTextureNames;
  Textures.FillComboBox(CBTextures);
  Layers.FillComboBox(CBLayers);
  FrameViewLayerList.Fill;

  ShowSelectionData(NIL);
  PC1.PageIndex := PC1.IndexOf(PageSurfaces);

  FrameTextureList.UpdateTextureWidgetState;

  // retrieve the overlap value
  FSEOverlap.Value := Project.Config.LevelEditorOverlap;

  FInitializingWidget := False;
end;

function TFrameToolLevelEditor.GetSpriteForAddMultiple: TSprite;
var texItem: PTextureItem;
  r: TRectF;
begin
  Result := NIL;
  if not ToggleSpeedButtonManager.Checked[BAddMultiple] then exit;
  if not CheckSurfaceWidgets then exit;

  texItem := Textures.GetItemByName(CBToTextureName);
  if texItem = NIL then exit;

  Result := TSprite.Create(texItem^.texture, False);
  FScene.Add(Result, CBToLayerIndex);
  Result.SetSize(SE3.Value, SE4.Value);
  Result.Pivot := PointF(SE5.Value, SE6.Value);
  Result.Angle.Value := SE7.Value;
  Result.FlipH := CBFlipH.Checked;
  Result.FlipV := CBFlipV.Checked;
  Result.Tint.Value := ColorToBGRA(ColorButton1.ButtonColor, SE9.Value);
  if RadioButton1.Checked then Result.TintMode := tmReplaceColor
    else Result.TintMode := tmMixColor;
  r := ScreenLevelEditor.Camera.GetViewRect;
  Result.SetCoordinate(r.Left, r.Top);
end;

procedure TFrameToolLevelEditor.SetCenterCoordinates(aX, aY: single);
begin
  FInitializingWidget := True;
  SE1.Value := aX - SE3.Value * 0.5;
  SE2.Value := aY - SE4.Value * 0.5;
  FInitializingWidget := False;
end;

procedure TFrameToolLevelEditor.AddSurfaceFromCurrentData;
begin
  if not CheckSurfaceWidgets then exit;

  FWorkingChild := Surfaces.AddEmpty;
  DoUpdateSurface(True);
  FWorkingChild^.HideHandle;
  //ScreenLevelEditor.AddToSelected([FWorkingChild]);
  ScreenLevelEditor.ShowHintTextAtMousepos('Added');

  FWorkingChild := NIL;
  FModified := True;
end;

procedure TFrameToolLevelEditor.ExitModeAddMultiple;
begin
  ToggleSpeedButtonManager.Checked[BAddMultiple] := False;
  ShowSelectionData(NIL);
end;

procedure TFrameToolLevelEditor.FillListBoxTextureNames;
begin
  FInitializingWidget := True;
  FrameTextureList.FillListBox;
  Textures.FillComboBox(CBTextures);
  FInitializingWidget := False;
end;

procedure TFrameToolLevelEditor.ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
var r,g,b: byte;
  A: TArrayOfInteger;
begin
  FInitializingWidget := True;
  FWorkingChild := NIL;

  if Length(aSelected) = 1 then begin
    // 1 selected -> we can edit its parameters
    FWorkingChild := aSelected[0];
    with FWorkingChild^ do begin
      LayerIndexToCB(GetSurfaceLayerIndex);
      if IsTextured then begin
        CBTextures.Enabled := True;
        TexturenameToCB(textureName);
      end else begin
        CBTextures.Enabled := False;
        CBTextures.ItemIndex := -1;
      end;
      if HaveTextureWithMultipleFrame then begin
        SE10.Value := Trunc(frameindex);
        SE10.MaxValue := GetTextureFrameCount;
      end else begin
        SE10.Value := 1;
        SE10.MaxValue := 1;
      end;
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
      surface.Tint.Value.ToRGB(r, g, b);
      ColorButton1.ButtonColor := RGBToColor(r, g, b);
      SE9.Value := Round(surface.Tint.Alpha.Value);
      case surface.TintMode of
        tmMixColor: RadioButton2.Checked := True;
        tmReplaceColor: RadioButton1.Checked := True;
      end;
    end;
    Label8.Visible := True;
    Label8.Caption := 'ID '+FWorkingChild^.id.ToString+' index '+Surfaces.GetItemIndexByID(FWorkingChild^.id).ToString+'/'+(integer(Surfaces.Size)-1).ToString+
                      '  Index in layer '+FWorkingChild^.surface.ParentLayer.IndexOf(FWorkingChild^.surface).ToString+'/'+(FWorkingChild^.surface.ParentLayer.SurfaceCount-1).ToString;
    CBLayers.Enabled := True;
    CBTextures.Enabled := True;
    Panel2.Visible := True;
    BNewSurface.Enabled := False;
    ToggleSpeedButtonManager.Checked[BAddMultiple] := False; // True;
    BAddMultiple.Enabled := True;
    CheckCreationOfSpriteMultiple;
  end
  else
  if Length(aSelected) > 1 then begin
    // several selected -> we can not edit the parameters except the layer
    Label8.Visible := False;
    CBTextures.Enabled := False;
    CBTextures.ItemIndex := -1;
    CBLayers.Enabled := True;
    A := ScreenLevelEditor.GetLayerindexesInSelection;
    if Length(A) <> 1 then CBLayers.ItemIndex := -1
      else CBLayers.ItemIndex := A[0];
    Panel2.Visible := False;
    BNewSurface.Enabled := False;
    ToggleSpeedButtonManager.Checked[BAddMultiple] := False;
    BAddMultiple.Enabled := False;
    CheckCreationOfSpriteMultiple;
  end
  else begin
    // 0 selected -> reset parameters, enable them and activate the button 'ADD'
    CBTextures.ItemIndex := -1;
    CBTextures.Enabled := True;
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
    SE10.Value := 1;
    RadioButton1.Checked := True;
    BNewSurface.Enabled := True;
    Label8.Visible := False;
    ToggleSpeedButtonManager.Checked[BAddMultiple] := False;
    BAddMultiple.Enabled := True;
    CheckCreationOfSpriteMultiple;
  end;

  FInitializingWidget := False;
end;

procedure TFrameToolLevelEditor.EditLevelInLevelBank(const aName: string);
var o: PLevelBankItem;
  wi: TWorldInfo;
begin
  o := LevelBank.GetItemByName(aName);
  if o = NIL then exit;

  Edit1.Text := o^.name;
  Surfaces.LoadFromString(o^.surfaces);

  FInitializingWidget := True;
  if Length(o^.worldinfo) > 0 then begin
    wi.InitDefault;
    wi.LoadFromString(o^.worldinfo);
    SpinEdit3.Value := wi.x;
    SpinEdit4.Value := wi.y;
    SpinEdit1.Value := wi.width;
    SpinEdit2.Value := wi.height;
    ColorButton2.ButtonColor := wi.boundscolor.ToColor;
    CheckBox1.Checked := wi.showbounds;
  end else begin
    SpinEdit3.Value := 0;
    SpinEdit4.Value := 0;
    SpinEdit1.Value := FScene.Width;
    SpinEdit2.Value := FScene.Height;
    CheckBox1.Checked := True;
  end;
  FInitializingWidget := False;

  FModified := False;
end;

end.

