unit frame_tool_spritebuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Spin, Menus, Arrow,
  BGRABitmapTypes,
  OGLCScene,
  u_surface_list, u_texture_list, Types,
  u_screen_spritebuilder, u_collisionbody_list, u_posture_list, u_undo_redo;

type

  { TFrameToolsSpriteBuilder }

  TFrameToolsSpriteBuilder = class(TFrame)
    ArrowRight: TArrow;
    ArrowUp: TArrow;
    ArrowDown: TArrow;
    ArrowLeft: TArrow;
    BAddTexture: TSpeedButton;
    BAddToSpriteBank: TSpeedButton;
    BChooseImageFile: TSpeedButton;
    BDeleteBody: TSpeedButton;
    BDeleteTexture: TSpeedButton;
    BDeletePosture: TSpeedButton;
    BBodyRedo: TSpeedButton;
    BBodyUndo: TSpeedButton;
    BRectangle: TSpeedButton;
    BCircle: TSpeedButton;
    BNewChild: TSpeedButton;
    BPolygon: TSpeedButton;
    BRenamePosture: TSpeedButton;
    BPoint: TSpeedButton;
    BTextureRedo: TSpeedButton;
    BPostureRedo: TSpeedButton;
    BTextureUndo: TSpeedButton;
    BPostureUndo: TSpeedButton;
    BUpdateTexture: TSpeedButton;
    BUpdateTextureListbox: TSpeedButton;
    CBParent: TComboBox;
    CBChildType: TComboBox;
    CBTextures: TComboBox;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    FSE2: TFloatSpinEdit;
    FSE1: TFloatSpinEdit;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBTextureNames: TListBox;
    LBPostureNames: TListBox;
    MIDeleteNode: TMenuItem;
    MIAddNode: TMenuItem;
    OD1: TOpenDialog;
    PageChilds: TTabSheet;
    PageTextures: TTabSheet;
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
    PC1: TPageControl;
    PopupNode: TPopupMenu;
    SE1: TFloatSpinEdit;
    SE10: TSpinEdit;
    SE11: TSpinEdit;
    SE12: TSpinEdit;
    SE2: TFloatSpinEdit;
    SE3: TFloatSpinEdit;
    SE4: TFloatSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    SE8: TSpinEdit;
    SE9: TSpinEdit;
    BNew: TSpeedButton;
    PageCollisionBody: TTabSheet;
    BLine: TSpeedButton;
    BSelect: TSpeedButton;
    BCancel: TSpeedButton;
    PagePostures: TTabSheet;
    BAddPostureToList: TSpeedButton;
    BUpdateposture: TSpeedButton;
    BReverseAngle: TSpeedButton;
    BResetPos: TSpeedButton;
    procedure ArrowUpClick(Sender: TObject);
    procedure BAddPostureToListClick(Sender: TObject);
    procedure BAddToSpriteBankClick(Sender: TObject);
    procedure BChooseImageFileClick(Sender: TObject);
    procedure BLineClick(Sender: TObject);
    procedure BNewChildClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure CBParentDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CBChildTypeSelect(Sender: TObject);
    procedure LBPostureNamesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure LBTextureNamesMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBTextureNamesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIAddNodeClick(Sender: TObject);
    procedure PC1Change(Sender: TObject);
  private // textures
    FInitializingWidget: boolean;
    procedure UpdateTextureWidgetState;
    function LBToTextureName: string;
    function CheckTextureWidgets: boolean;
    procedure DoAddTexture;
    procedure DoDeleteTexture;
    procedure DoUpdateTexture;
  private // childs
    procedure ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
    function CBToClassType: classOfSimpleSurfaceWithEffect;
    procedure TexturenameToCB(aName: string);
    function CBToTextureName: string;
    procedure ParentIDToCB(aID: integer);
    function CBToParentID: integer;
    function CheckChildWidgets: boolean;
    function DoAddNewChild: boolean;
    procedure DoUpdateChild(aForceRecreateSurface: boolean);
  private // postures
    FPostureTabIsActive, FAddingPosture: boolean;
    function LBSelectedToName: string;
    procedure UpdatePostureWidgetState;
    procedure DoAddposture;
    procedure DoUpdatePosture;
    procedure DoRenamePosture;
    procedure DoDeletePosture;
    procedure DoReverseAngleOnPosture;
  private
    FWorkingChild: PSurfaceDescriptor;
    procedure UpdateValuesToWorkingSurface;
    function Textures: TTextureList;
    function Surfaces: TSpriteBuilderSurfaceList;
    function Bodies: TBodyItemList;
    function Postures: TPostureList;
    procedure DoClearAll;
  private
    FModified: boolean;
  public
    procedure OnShow;
    procedure FillListBoxTextureNames;
    procedure ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);

    procedure EditSpriteInSpriteBank(const aName: string);

    function SelectedTabIsChild: boolean;
    function SelectedTabIsCollisionBody: boolean;
    function SelectedTabIsPosture: boolean;

    property Modified: boolean read FModified write FModified;
  end;

implementation
uses form_main, u_project, u_common, u_utils, u_spritebank, u_screen_template,
  LCLType;
{$R *.lfm}

{ TFrameToolsSpriteBuilder }

procedure TFrameToolsSpriteBuilder.BChooseImageFileClick(Sender: TObject);
var i: integer;
  item: PTextureItem;
begin
  if sender=BUpdateTextureListbox then
    Textures.FillListBox(LBTextureNames);

  if Sender = BChooseImageFile then begin
    if not OD1.Execute then exit;
    Label2.Caption := OD1.FileName;
    Edit1.Text := 'tex'+ChangeFileExt(ExtractFileName(OD1.FileName), '');
    if ExtractFileExt(OD1.FileName) = '.svg' then begin
      SE9.Value := -1;
      SE10.Value := -1;
    end;
    UpdateTextureWidgetState;
  end;

  if Sender = BUpdateTexture then
    DoUpdateTexture;

  if Sender = BAddTexture then begin
    DoAddTexture;
    UpdateTextureWidgetState;
  end;

  if Sender = BDeleteTexture then begin
    DoDeleteTexture;
    UpdateTextureWidgetState;
  end;

  if Sender = BTextureUndo then begin
    Textures.UndoRedoManager.Undo;
    UpdateTextureWidgetState;
  end;

  if Sender = BTextureRedo then begin
    Textures.UndoRedoManager.Redo;
    UpdateTextureWidgetState;
  end;

  if Sender = BCancel then begin
    if FModified then
      if QuestionDlg('','If you leave, changes will be lost.'+LineEnding+'Continue ?', mtWarning,
                     [mrOk, 'Leave', mrCancel, 'Cancel'], 0) = mrCancel then exit;
    DoClearAll;
    FormMain.ShowPageSpriteBank;
  end;
end;

procedure TFrameToolsSpriteBuilder.BLineClick(Sender: TObject);
begin
  if Sender = BSelect then begin
    ScreenSpriteBuilder.MouseState := msIdle;
  end;

  if Sender = BPoint then begin
    ScreenSpriteBuilder.MouseState := msToolPoint;
  end;

  if Sender = BLine then begin
    ScreenSpriteBuilder.MouseState := msToolLine;
  end;

  if Sender = BCircle then begin
    ScreenSpriteBuilder.MouseState := msToolCircle;
  end;

  if Sender = BRectangle then begin
    ScreenSpriteBuilder.MouseState := msToolRectangle;
  end;

  if Sender = BPolygon then begin
    ScreenSpriteBuilder.MouseState := msToolPolygon;
  end;
end;

procedure TFrameToolsSpriteBuilder.BNewChildClick(Sender: TObject);
begin
  if Sender = BNewChild then begin
    if DoAddNewChild then
      ShowSelectionData(NIL);
  end;

end;

procedure TFrameToolsSpriteBuilder.BNewClick(Sender: TObject);
begin
  if QuestionDlg('Warning', 'if you continue, the modification will be lost',
             mtWarning, [mrOk, 'Continue', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  DoClearAll;
end;

procedure TFrameToolsSpriteBuilder.BAddToSpriteBankClick(Sender: TObject);
var o: PSpriteBankItem;
  nam: string;
begin
  nam := Trim(Edit2.Text);
  if nam = '' then exit;
  if Textures.Size = 0 then exit;
  if Surfaces.Size = 0 then exit;
  if not Surfaces.RootIsDefined then exit;

  o := SpriteBank.GetItemByName(nam);
  if o <> NIL then
    if QuestionDlg('','a sprite named "'+nam+'" already exists in the bank.'+LineEnding+
                'Would you like to replace it ?',
                mtWarning, [mrOk, 'Replace', mrCancel, 'Cancel'],0) = mrCancel then exit;

  if o = NIL then o := SpriteBank.AddEmpty;

  // restore the originals surfaces values
  if FPostureTabIsActive then begin
    FPostureTabIsActive := False;
    Surfaces.SetValuesFromTemporaryVariables;
  end;

  o^.name := nam;
  o^.textures := Textures.SaveToString;
  o^.surfaces := Surfaces.SaveToString;
  o^.collisionbodies := Bodies.SaveToString;
  o^.postures := Postures.SaveToString;

  Project.SetModified;

  DoClearAll;
  FormMain.ShowPageSpriteBank;
end;

procedure TFrameToolsSpriteBuilder.BAddPostureToListClick(Sender: TObject);
begin
  if Sender = BResetPos then
    ScreenSpriteBuilder.ResetValuesOnSelection;

  if Sender = BReverseAngle then
    DoReverseAngleOnPosture;

  if Sender = BAddPostureToList then
    DoAddposture;

  if Sender = BUpdatePosture then
    DoUpdatePosture;

  if Sender = BDeletePosture then
    DoDeletePosture;

  if Sender = BRenamePosture then
    DoRenamePosture;

  if Sender = BPostureUndo then begin
    Postures.UndoRedoManager.Undo;
    UpdatePostureWidgetState;
  end;

  if Sender = BPostureRedo then begin
    Postures.UndoRedoManager.Redo;
    UpdatePostureWidgetState;
  end;

  if Sender = Edit3 then
    UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.ArrowUpClick(Sender: TObject);
begin
  if Sender = FSE2 then begin
    if ScreenSpriteBuilder.SelectedCount = 1 then
      ScreenSpriteBuilder.SetAngleOnSelection(FSE2.Value);
  end;

  if Sender = ArrowUp then begin
    ScreenSpriteBuilder.MoveSelection(PointF(0, -1));
  end;

  if Sender = ArrowDown then begin
    ScreenSpriteBuilder.MoveSelection(PointF(0, 1));
  end;

  if Sender = ArrowLeft then begin
    ScreenSpriteBuilder.MoveSelection(PointF(-1, 0));
  end;

  if Sender = ArrowRight then begin
    ScreenSpriteBuilder.MoveSelection(PointF(1, 0));
  end;
end;

procedure TFrameToolsSpriteBuilder.CBParentDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var s: string;
begin
  with CBParent.Canvas do begin
    if odSelected in State then
      Brush.Color := clHighLight;
    Brush.Style := bsSolid;
    FillRect(ARect);

    Brush.Style := bsClear;
    s := Surfaces.GetItemByID(CBParent.Items.Strings[Index].ToInteger)^.name;
    TextOut(ARect.Left, ARect.Top, s);
  end;
end;

procedure TFrameToolsSpriteBuilder.CBChildTypeSelect(Sender: TObject);
var texItem: PTextureItem;
  chang: boolean;
begin
  if FInitializingWidget then exit;

  if Sender = CBTextures then begin
    if Edit5.Text = '' then begin
      texItem := Textures.GetItemByName(CBTextures.Text);
      if texItem <> NIL then
        Edit5.Text := ChangeFileExt(ExtractFilename(texItem^.filename), '');
    end;
  end;

  if FWorkingChild <> NIL then begin
    with FWorkingChild^.surface do
      chang := not (FWorkingChild^.surface is CBToClassType) or
                   (FWorkingChild^.textureName <> CBToTextureName) or
                   (FWorkingChild^.parentID <> CBToParentID) or
                   (FWorkingChild^.name <> Trim(Edit5.Text)) or
                   (X.Value <> SE1.Value) or (Y.Value <> SE2.Value) or
                   (Pivot.x <> SE3.Value) or (Pivot.y <> SE4.Value) or
                   (Scale.x.Value <> SE5.Value) or (Scale.y.Value <> SE6.Value) or
                   (Angle.Value <> SE7.Value) or (ZOrderAsChild <> SE8.Value);
    if chang then begin
      DoUpdateChild(False);
      FModified := True;
    end;
  end;
end;

procedure TFrameToolsSpriteBuilder.LBPostureNamesSelectionChange(Sender: TObject; User: boolean);
var i, j: integer;
  post: PPostureItem;
  surf: PSurfaceDescriptor;
begin
  if FAddingPosture then exit;
  i := LBPostureNames.ItemIndex;
  if i = -1 then exit;
  if Surfaces.Size = 0 then exit;

  post := Postures.GetItemByName(LBSelectedToName);
  if post = NIL then raise exception.create('bug: post is NIL');
  if Length(post^.Values) <> Surfaces.Size then raise exception.create('bug');
  Edit3.Text := post^.name;
  for j:=0 to Surfaces.Size-1 do begin
    surf := Surfaces.Mutable[j];
    surf^.surface.X.ChangeTo(post^.Values[j].x, FSE1.Value, idcSinusoid);
    surf^.surface.Y.ChangeTo(post^.Values[j].y, FSE1.Value, idcSinusoid);
    surf^.surface.Angle.ChangeTo(post^.Values[j].angle, FSE1.Value, idcSinusoid);
    surf^.surface.Scale.X.ChangeTo(post^.Values[j].scalex, FSE1.Value, idcSinusoid);
    surf^.surface.Scale.Y.ChangeTo(post^.Values[j].scaley, FSE1.Value, idcSinusoid);
  end;
  ScreenSpriteBuilder.SelectNone;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.LBTextureNamesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var lb: TListBox;
begin
  lb := Sender as TListBox;
  lb.ItemIndex := lb.GetIndexAtXY(X, Y);
end;

procedure TFrameToolsSpriteBuilder.LBTextureNamesSelectionChange(
  Sender: TObject; User: boolean);
var i: integer;
  p: PTextureItem;
begin
  if FInitializingWidget then exit;
  FInitializingWidget := True;

  i := LBTextureNames.ItemIndex;
  if i = -1 then begin
    Label2.Caption := '';
    Edit1.Text := '';
    SE9.Value := 0;
    SE10.Value := 0;
    CheckBox1.Checked := False;
    SE11.Value := 0;
    SE12.Value := 0;
  end else begin
    p := Textures.GetItemByName(LBTextureNames.Items.Strings[i]); // Textures.Mutable[i];
    Label2.Caption := p^.filename;
    Edit1.Text := p^.name;
    SE9.Value := p^.width;
    SE10.Value := p^.height;
    CheckBox1.Checked := p^.isMultiFrame;
    SE11.Value := p^.frameWidth;
    SE12.Value := p^.frameHeight;
  end;
  FInitializingWidget := False;

  UpdateTextureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.MIAddNodeClick(Sender: TObject);
var key: word;
begin
  if Sender = MIAddNode then begin
    ScreenSpriteBuilder.AddNodeBetweenSelectedOnPolygon;
  end;

  if Sender = MIDeleteNode then begin
    key := VK_DELETE;
    ScreenSpriteBuilder.ProcessKeyUpForCollisionBody(key, []);
  end;
end;

procedure TFrameToolsSpriteBuilder.PC1Change(Sender: TObject);
begin
  ScreenSpriteBuilder.SelectNone;
  ScreenSpriteBuilder.MouseState := msIdle;

  // interdire la selection des éléments du sprite
  // et autoriser la sélection des shapes de collision
  FScene.Layer[LAYER_COLLISION_BODY].Visible := SelectedTabIsCollisionBody;
  if SelectedTabIsCollisionBody then Bodies.UpdateNodesPosition;

  // if user enter the postures tab, we save the surfaces values (x, y, angle, scale...)
  // when user exit from postures tab, we restore the surfaces values.
  if FPostureTabIsActive and not SelectedTabIsPosture then
    Surfaces.SetValuesFromTemporaryVariables
  else
  if not FPostureTabIsActive and SelectedTabIsPosture then
    Surfaces.CopySurfaceValuesToTemporaryVariables;
  FPostureTabIsActive := SelectedTabIsPosture;

end;

procedure TFrameToolsSpriteBuilder.UpdateTextureWidgetState;
var siz: TSize;
begin
  if not FileExists(Label2.Caption) then begin
    BUpdateTexture.Enabled := False;
    BAddTexture.Enabled := False;
  end
  else
  if Textures.GetItemByFilename(Label2.Caption) <> NIL then begin
    BUpdateTexture.Enabled := True;
    BAddTexture.Enabled := False;
  end
  else
  begin
    BUpdateTexture.Enabled := False;
    BAddTexture.Enabled := True;
  end;

  // width and height are read only for non svg file
  SE9.Enabled := ExtractFileExt(Label2.Caption) = '.svg';
  SE10.Enabled := SE9.Enabled;
  if not SE9.Enabled and FileExists(Label2.Caption) then begin
    siz := GetImageSize(Label2.Caption);
    SE9.Value := siz.cx;
    SE10.Value := siz.cy;
  end;

  Label7.Enabled := CheckBox1.Checked;
  Label8.Enabled := CheckBox1.Checked;
  SE11.Enabled := CheckBox1.Checked;
  SE12.Enabled := CheckBox1.Checked;

  BDeleteTexture.Enabled := LBTextureNames.ItemIndex <> -1;
  BTextureUndo.Enabled := Textures.UndoRedoManager.CanUndo;
  BTextureRedo.Enabled := Textures.UndoRedoManager.CanRedo;
end;

procedure TFrameToolsSpriteBuilder.ClassTypeToCB(aClass: classOfSimpleSurfaceWithEffect);
var i: integer;
begin
  i := CBChildType.Items.IndexOf(aClass.ClassName);
  if i = -1 then raise exception.create('forgot to implement '+aClass.ClassName+' !');
  CBChildType.ItemIndex := i;
end;

function TFrameToolsSpriteBuilder.CBToClassType: classOfSimpleSurfaceWithEffect;
begin
  case CBChildType.Items.Strings[CBChildType.ItemIndex] of
    'TSprite': Result := TSprite;
    'TSpriteWithElasticCorner': Result := TSpriteWithElasticCorner;
    'TTiledSprite': Result := TTiledSprite;
    'TPolarSprite': Result := TPolarSprite;
    'TScrollableSprite': Result := TScrollableSprite;
    'TShapeOutline': Result := TShapeOutline;
    'TGradientRectangle': Result := TGradientRectangle;
    'TQuad4Color': Result := TQuad4Color;
    'TSpriteContainer': Result := TSpriteContainer;
    'TDeformationGrid': Result := TDeformationGrid;
      else raise exception.create('forgot to implement!');
  end;
end;

procedure TFrameToolsSpriteBuilder.TexturenameToCB(aName: string);
begin
  CBTextures.ItemIndex := Textures.GetItemIndexByName(aName);
end;

function TFrameToolsSpriteBuilder.CBToTextureName: string;
begin
  if CBTextures.ItemIndex = -1 then Result := ''
    else Result := CBTextures.Items.Strings[CBTextures.ItemIndex];
end;

procedure TFrameToolsSpriteBuilder.ParentIDToCB(aID: integer);
begin
  CBParent.ItemIndex := CBParent.Items.IndexOf(aID.ToString);
end;

function TFrameToolsSpriteBuilder.CBToParentID: integer;
begin
  if CBParent.ItemIndex = -1 then Result := -1
    else Result := CBParent.Items.Strings[CBParent.ItemIndex].ToInteger;
end;

function TFrameToolsSpriteBuilder.CheckChildWidgets: boolean;
var definingRoot: boolean;
begin
  definingRoot := Surfaces.Size = 0;

  Result := (CBChildType.ItemIndex <> -1) and
            (Trim(Edit5.Text) <> '');

  if not definingRoot then
    Result := Result and (CBTextures.ItemIndex <> -1) and
                         (CBParent.ItemIndex <> -1);
end;

function TFrameToolsSpriteBuilder.DoAddNewChild: boolean;
begin
  Result := False;
  if not CheckChildWidgets then exit;
  if Surfaces.NameExists(Trim(Edit5.Text)) then begin
    ShowMessage('Duplicate name "'+Trim(Edit5.Text)+'"');
    exit;
  end;

  FWorkingChild := Surfaces.AddEmpty;
  DoUpdateChild(False);
  Postures.AddValueEntryOnEachPosture;

  CBParent.Items.Add(FWorkingChild^.ID.ToString);
  FWorkingChild := NIL;
  FModified := True;
  Result := True;
end;

procedure TFrameToolsSpriteBuilder.DoUpdateChild(aForceRecreateSurface: boolean);
var recreateSurface: Boolean;
begin
  if FWorkingChild = NIL then exit;

  recreateSurface := aForceRecreateSurface;
  if not (FWorkingChild^.surface is CBToClassType) then
    recreateSurface := True;

  if FWorkingChild^.textureName <> CBToTextureName then
    recreateSurface := True;

  if FWorkingChild^.parentID <> CBToParentID then
    recreateSurface := True;

  FWorkingChild^.name := Trim(Edit5.Text);

  if recreateSurface then begin
    // save the childs of the surface before killing it
    FWorkingChild^.SaveAndRemoveChilds;
    // create a new surface
    FWorkingChild^.KillSurface;
    FWorkingChild^.classtype := CBToClassType;
    FWorkingChild^.textureName := CBToTextureName;
    FWorkingChild^.CreateSurface;
    // restore the childs
    FWorkingChild^.RestoreChilds;
    // set child dependency
    FWorkingChild^.parentID := CBToParentID;
    FWorkingChild^.zOrder := SE8.Value;
    FWorkingChild^.SetChildDependency;
    FModified := True;
  end;

  UpdateValuesToWorkingSurface;

  // in case the name was changed, we update the combobox parent
  Surfaces.ReplaceNameInComboBox(CBParent);
end;

function TFrameToolsSpriteBuilder.LBSelectedToName: string;
var i: Integer;
begin
  i := LBPostureNames.ItemIndex;
  if i = -1 then Result := ''
    else Result := LBPostureNames.Items.Strings[i];
end;

procedure TFrameToolsSpriteBuilder.UpdatePostureWidgetState;
var s: string;
begin
  BDeletePosture.Enabled := LBPostureNames.ItemIndex <> -1;
  BPostureUndo.Enabled := Postures.UndoRedoManager.CanUndo;
  BPostureRedo.Enabled := Postures.UndoRedoManager.CanRedo;
  s := Trim(Edit3.Text);
  BAddPostureToList.Enabled := (Surfaces.Size > 0) and
                               (s <> '') and not Postures.NameAlreadyExists(s);

  BUpdateposture.Enabled := (Surfaces.Size > 0) and
                            (s <> '') and Postures.NameAlreadyExists(s);
end;

procedure TFrameToolsSpriteBuilder.DoAddposture;
var item: PPostureItem;
  nam: string;
begin
  nam := Trim(Edit3.Text);
  if nam = '' then exit;
  if Postures.GetItemByName(nam) <> NIL then exit;

  item := Postures.AddEmpty;
  item^.name := nam;
  item^.TakeValuesFrom(Surfaces);

  Postures.UndoRedoManager.AddActionAddPosture(item);

  FAddingPosture := True;
  LBPostureNames.ItemIndex := LBPostureNames.Items.Add(item^.name);
  Edit3.Text := '';
  FAddingPosture := False;
  Modified := True;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.DoUpdatePosture;
var item: PPostureItem;
  i: integer;
  nam: string;
  undoredoItem: PPostureUndoRedoItem;
begin
  i := LBPostureNames.ItemIndex;
  if i = -1 then exit;
  item := Postures.GetItemByName(LBPostureNames.Items.Strings[i]);
  if item = NIL then exit;

  nam := Trim(Edit3.Text);
  if LBPostureNames.Items.Strings[i] = nam then begin
    if QuestionDlg('','Replace the selected posture ?', mtConfirmation,
                   [mrOk, 'Yes', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  end else
    if QuestionDlg('','A posture with this name is already defined', mtConfirmation,
               [mrOk, 'Replace', mrCancel, 'Cancel'], 0) = mrCancel then exit;

  // construct the undo/redo item
  undoredoItem := Postures.UndoRedoManager.AddEmpty;
  undoredoItem^.action := puratModifyPosture;
  undoredoItem^.data.name := item^.name;
  undoredoItem^.data.Values := Copy(item^.Values);
  undoredoItem^.newData.name := nam;
  undoredoItem^.newData.TakeValuesFrom(Surfaces);

  item^.name := nam;
  item^.TakeValuesFrom(Surfaces);
  LBPostureNames.Items.Strings[i] := nam;
  Modified := True;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.DoRenamePosture;
var i: Integer;
  oldName, newName: string;
  undoredoItem: PPostureUndoRedoItem;
begin
  i := LBPostureNames.ItemIndex;
  if i = -1 then exit;

  oldName := LBPostureNames.Items.Strings[i];
  newName := InputBox('', 'Enter the new name:', oldName);
  if newName = oldName then exit;

  // construct the undo/redo item
  undoredoItem := Postures.UndoRedoManager.AddEmpty;
  undoredoItem^.action := puratRenamePosture;
  undoredoItem^.data.name := oldName;
  undoredoItem^.newData.name := newName;

  Postures.GetItemByName(oldName)^.name := newName;
  LBPostureNames.Items.Strings[i] := newName;
  Modified := True;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.DoDeletePosture;
var i: Integer;
  item: PPostureItem;
  undoredoItem: PPostureUndoRedoItem;
begin
  i := LBPostureNames.ItemIndex;
  if i = -1 then exit;
  item := Postures.GetItemByName(LBPostureNames.Items.Strings[i]);
  if item = NIL then exit;

  // construct the undo/redo item
  undoredoItem := Postures.UndoRedoManager.AddEmpty;
  undoredoItem^.action := puratDeletePosture;
  undoredoItem^.ListBoxitemIndexWhenDeleted := i;
  undoredoItem^.data.name := item^.name;
  undoredoItem^.data.Values := Copy(item^.Values);

  Postures.DeleteItemByName(LBPostureNames.Items.Strings[i]);
  LBPostureNames.Items.Delete(i);
  Modified := True;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.DoReverseAngleOnPosture;
var i: integer;
  item: PPostureItem;
  undoredoItem: PPostureUndoRedoItem;
begin
  i := LBPostureNames.ItemIndex;
  if i = -1 then exit;
  item := Postures.GetItemByName(LBPostureNames.Items.Strings[i]);
  if item = NIL then exit;

  ScreenSpriteBuilder.ReverseAngleOnSelection;
  // construct the undo/redo item
  undoredoItem := Postures.UndoRedoManager.AddEmpty;
  undoredoItem^.action := puratModifyPosture;
  undoredoItem^.data.name := item^.name;
  undoredoItem^.data.Values := Copy(item^.Values);
  undoredoItem^.newData.name := item^.name;
  undoredoItem^.newData.TakeValuesFrom(Surfaces);

  item^.TakeValuesFrom(Surfaces);
  Modified := True;
  UpdatePostureWidgetState;
end;

function TFrameToolsSpriteBuilder.LBToTextureName: string;
var i: Integer;
begin
  Result := '';
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;
  Result := LBTextureNames.Items.Strings[i];
end;

function TFrameToolsSpriteBuilder.CheckTextureWidgets: boolean;
begin
  Result := FileExists(Label2.Caption) and
            (Trim(Edit1.Text) <> '') and
            not Textures.NameAlreadyExists(Trim(Edit1.Text)) and
            (SE9.Value <> 0) and
            (SE10.Value <> 0);
  if CheckBox1.Checked then
    Result := Result and (SE11.Value <> 0) and (SE12.Value <> 0);
end;

procedure TFrameToolsSpriteBuilder.DoAddTexture;
var texName: string;
i:integer;
begin
  if not CheckTextureWidgets then exit;

  texName := Trim(Edit1.Text);

{FScene.LogDebug('TFrameToolsSpriteBuilder.DoAddTexture: BEFORE Textures.Size='+Textures.Size.tostring);
for i:=0 to Textures.Size-1 do
  FScene.LogDebug('  Textures index '+i.tostring+' name='+Textures.mutable[i]^.name); }

FScene.LogDebug('  ADDING '+Label2.Caption+'  '+texName);
  Textures.Add(Label2.Caption, texName, SE9.Value, SE10.Value,
               CheckBox1.Checked, SE11.Value, SE12.Value);

{FScene.LogDebug('TFrameToolsSpriteBuilder.DoAddTexture: AFTER Textures.Size='+Textures.Size.tostring);
for i:=0 to Textures.Size-1 do
  FScene.LogDebug('  Textures index '+i.tostring+' name='+Textures.mutable[i]^.name);  }

  FInitializingWidget := True;
  LBTextureNames.ItemIndex := LBTextureNames.Items.Add(texName);
  FInitializingWidget := False;
  UpdateTextureWidgetState;

  Textures.UndoRedoManager.AddActionAddTexture(texName);
  Textures.FillComboBox(CBTextures); // refresh the combobox in childs page
  FModified := True;
end;

procedure TFrameToolsSpriteBuilder.DoDeleteTexture;
var textureName: string;
begin
  textureName := LBToTextureName;
  if textureName = '' then exit;

  // check if the texture is used by a surface
  if Surfaces.TextureNameisUsedByASurface(textureName) then begin
    ShowMessage('This texture is used by a surface, you can not delete it');
    exit;
  end;
  Textures.UndoRedoManager.AddActionDeleteTexture(textureName);
  Textures.DeleteByName(textureName);
  LBTextureNames.Items.Delete(LBTextureNames.ItemIndex);
  Textures.FillComboBox(CBTextures);

  FModified := True;

  FInitializingWidget := True;
  Label2.Caption := '';
  Edit1.Text := '';
  SE9.Value := 0;
  SE10.Value := 0;
  CheckBox1.Checked := False;
  SE11.Value := 0;
  SE12.Value := 0;
  FInitializingWidget := False;
end;

procedure TFrameToolsSpriteBuilder.DoUpdateTexture;
var i, j: Integer;
  item: PTextureItem;
  surf: ArrayOfPSurfaceDescriptor;
begin
  i := LBTextureNames.ItemIndex;
  if i = -1 then exit;

  item := Textures.GetItemByName(LBTextureNames.Items.Strings[i]);
  if item = NIL then exit;

  Textures.UndoRedoManager.AddActionModifyTexture(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value);
  Textures.Update(item, Label2.Caption, Edit1.Text, SE9.Value, SE10.Value,
                  CheckBox1.Checked, SE11.Value, SE12.Value);

  // recreate the surfaces that use this texture (if any)
  surf := Surfaces.GetItemsThatUseThisTexture(item);
  if Length(surf) <> 0 then begin
    for j:=0 to High(surf)do
      surf[j]^.RecreateSurfaceBecauseTextureChanged;
  end;

  Label2.Caption := '';
  UpdateTextureWidgetState;
  FModified := True;
end;

procedure TFrameToolsSpriteBuilder.UpdateValuesToWorkingSurface;
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
  end;
  FWorkingChild^.UpdateHandlePosition;
end;

function TFrameToolsSpriteBuilder.Textures: TTextureList;
begin
  Result := ScreenSpriteBuilder.Textures;
end;

function TFrameToolsSpriteBuilder.Surfaces: TSpriteBuilderSurfaceList;
begin
  Result := ScreenSpriteBuilder.Surfaces;
end;

function TFrameToolsSpriteBuilder.Bodies: TBodyItemList;
begin
  Result := ScreenSpriteBuilder.Bodies;
end;

function TFrameToolsSpriteBuilder.Postures: TPostureList;
begin
  Result := ScreenSpriteBuilder.Postures;
end;

procedure TFrameToolsSpriteBuilder.DoClearAll;
begin
  FModified := False;
  ScreenSpriteBuilder.SelectNone;
  FWorkingChild := NIL;
  FInitializingWidget := True;

  Surfaces.Clear;
  Textures.Clear;
  Bodies.Clear;
  Postures.Clear;

  CBTextures.Clear;
  Edit5.Text := '';
  Surfaces.FillComboBox(CBParent);
  Edit2.Text := '';

  LBTextureNames.Clear;
  Label2.Caption := '';
  Edit1.Text := '';
  CheckBox1.Checked := False;

  PC1.PageIndex := PC1.IndexOf(PageTextures);
  FInitializingWidget := False;
end;

procedure TFrameToolsSpriteBuilder.OnShow;
begin
  FillListBoxTextureNames;
  Surfaces.FillComboBox(CBParent);
  Textures.FillComboBox(CBTextures);
  Postures.FillListBox(LBPostureNames);
  ShowSelectionData(NIL);
  PC1.PageIndex := PC1.IndexOf(PageChilds);

  FScene.Layer[LAYER_COLLISION_BODY].Visible := False;
  Bodies.UpdateNodesPosition;

  UpdateTextureWidgetState;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.FillListBoxTextureNames;
begin
  Textures.FillListBox(LBTextureNames);
  Label2.Caption := '';
  SE9.Value := 0;
  SE10.Value := 0;
  CheckBox1.Checked := False;
  SE11.Value := 0;
  SE12.Value := 0;
end;

procedure TFrameToolsSpriteBuilder.ShowSelectionData(aSelected: ArrayOfPSurfaceDescriptor);
begin
  FInitializingWidget := True;
  FWorkingChild := NIL;

  Label20.Visible := Surfaces.Size = 0;

  if Length(aSelected) = 1 then begin
    // 1 selected -> we can edit its parameters
    FWorkingChild := aSelected[0];
    with FWorkingChild^ do begin
      ClassTypeToCB(classtype);
      TexturenameToCB(textureName);
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
    end;
    CBChildType.Enabled := True;
    CBTextures.Enabled := True;
    Edit5.Enabled := True;
    CBParent.Enabled := True;
    Panel4.Visible := True;
    BNewChild.Enabled := False;
  end
  else
  if Length(aSelected) > 1 then begin
    // several selected -> we can not edit the parameters
    CBChildType.Enabled := False;
    CBTextures.Enabled := False;
    Edit5.Enabled := False;
    CBParent.Enabled := False;
    CBChildType.ItemIndex := -1;
    CBTextures.ItemIndex := -1;
    Edit5.Text := '';
    CBParent.ItemIndex := -1;
    Panel4.Visible := False;
    BNewChild.Enabled := False;
  end
  else begin
    // 0 selected -> reset parameters, enable them and activate the button 'ADD'
    CBChildType.ItemIndex := -1;
    CBTextures.ItemIndex := -1;
    Edit5.Text := '';
    CBParent.ItemIndex := -1;
    Panel4.Visible := True;
    SE1.Value := 0;
    SE2.Value := 0;
    SE3.Value := 0.5;
    SE4.Value := 0.5;
    SE5.Value := 1.0;
    SE6.Value := 1.0;
    SE7.Value := 0.0;
    SE8.Value := 0;
    BNewChild.Enabled := True;
  end;

  FInitializingWidget := False;
end;

procedure TFrameToolsSpriteBuilder.EditSpriteInSpriteBank(const aName: string);
var o: PSpriteBankItem;
begin
  o := SpriteBank.GetItemByName(aName);
  if o = NIL then exit;

  Textures.LoadFromString(o^.textures);
  Surfaces.LoadFromString(o^.surfaces);
  Bodies.LoadFromString(o^.collisionbodies);
  Bodies.SetParentSurface(Surfaces.GetRootItem^.surface);
  Postures.LoadFromString(o^.postures);
  FModified := False;

  Edit2.Text := o^.name;
end;

function TFrameToolsSpriteBuilder.SelectedTabIsChild: boolean;
begin
  Result := PC1.PageIndex = PC1.IndexOf(PageChilds);
end;

function TFrameToolsSpriteBuilder.SelectedTabIsCollisionBody: boolean;
begin
  Result := PC1.PageIndex = PC1.IndexOf(PageCollisionBody);
end;

function TFrameToolsSpriteBuilder.SelectedTabIsPosture: boolean;
begin
  Result := PC1.PageIndex = PC1.IndexOf(PagePostures);
end;

end.

