unit frame_tool_spritebuilder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, Spin, Menus, Arrow,
  BGRABitmapTypes,
  OGLCScene,
  u_surface_list, u_texture_list, Types,
  u_screen_spritebuilder, u_collisionbody_list, u_posture_list, u_undo_redo,
  frame_texturelist;

type

  { TFrameToolsSpriteBuilder }

  TFrameToolsSpriteBuilder = class(TFrame)
    ArrowRight: TArrow;
    ArrowUp: TArrow;
    ArrowDown: TArrow;
    ArrowLeft: TArrow;
    BAddToSpriteBank: TSpeedButton;
    BDeleteBody: TSpeedButton;
    BDeletePosture: TSpeedButton;
    BBodyRedo: TSpeedButton;
    BBodyUndo: TSpeedButton;
    BHelpPostures: TSpeedButton;
    BHelpRootChilds: TSpeedButton;
    BHelpCollisionBody: TSpeedButton;
    BRectangle: TSpeedButton;
    BCircle: TSpeedButton;
    BNewChild: TSpeedButton;
    BPolygon: TSpeedButton;
    BRenamePosture: TSpeedButton;
    BPoint: TSpeedButton;
    BPostureRedo: TSpeedButton;
    BPostureUndo: TSpeedButton;
    CBParent: TComboBox;
    CBChildType: TComboBox;
    CBTextures: TComboBox;
    CBFlipH: TCheckBox;
    CBFlipV: TCheckBox;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    FSE2: TFloatSpinEdit;
    FSE1: TFloatSpinEdit;
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
    Label20: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label9: TLabel;
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
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    PC1: TPageControl;
    PopupNode: TPopupMenu;
    SE1: TFloatSpinEdit;
    SE2: TFloatSpinEdit;
    SE3: TFloatSpinEdit;
    SE4: TFloatSpinEdit;
    SE5: TFloatSpinEdit;
    SE6: TFloatSpinEdit;
    SE7: TFloatSpinEdit;
    SE8: TSpinEdit;
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
    SEDummy: TSpinEdit;
    procedure ArrowUpClick(Sender: TObject);
    procedure BAddPostureToListClick(Sender: TObject);
    procedure BAddToSpriteBankClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BHelpCollisionBodyClick(Sender: TObject);
    procedure BHelpPosturesClick(Sender: TObject);
    procedure BHelpRootChildsClick(Sender: TObject);
    procedure BLineClick(Sender: TObject);
    procedure BNewChildClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure CBParentDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CBChildTypeSelect(Sender: TObject);
    procedure LBPostureNamesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIAddNodeClick(Sender: TObject);
    procedure PC1Change(Sender: TObject);
    procedure SE1Enter(Sender: TObject);
  private // textures
    FInitializingWidget: boolean;
    procedure ProcessTextureListOnModified(Sender: TObject);
    procedure ProcessAskToDeleteTextureEvent(aTextureItem: PTextureItem; var aCanDelete: boolean);
    procedure ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
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
    FrameTextureList: TFrameTextureList;
    constructor Create(TheOwner: TComponent); override;

    procedure SetFocusToDummy;

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
uses form_main, u_project, u_common, u_spritebank, u_screen_template,
  form_showhelp, LCLType;
{$R *.lfm}

{ TFrameToolsSpriteBuilder }

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
  Project.Save;
  Modified := False;

  DoClearAll;
  FormMain.ShowPageSpriteBank;
end;

procedure TFrameToolsSpriteBuilder.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('','If you leave, changes will be lost.'+LineEnding+'Continue ?', mtWarning,
                   [mrOk, 'Leave', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  DoClearAll;
  FormMain.ShowPageSpriteBank;
end;

procedure TFrameToolsSpriteBuilder.BHelpCollisionBodyClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('Here you can define the collision body for the sprite.'#10+
  'It is made of one or several collision shape: point, line, circle, rectangle or polygon.'#10+
  'In the game, you can test whether the collision body collides with other surfaces.'#10#10+
  'ADD A POINT:'#10+
  ' - click the Point button.'#10+
  ' - click on the sprite to define its location.'#10#10+
  'ADD A LINE:'#10+
  ' - click the Line button'#10+
  ' - click on the sprite to place the first point, drag the mouse to place the second point, release the mouse.'#10#10+
  'ADD A CIRCLE:'#10+
  ' - click the Circle button.'#10+
  ' - click on the sprite to place the center of the circle, drag the mouse to define its radius, release the mouse.'#10#10+
  'ADD A RECTANGLE:'#10+
  ' - click the Rectangle button.'#10+
  ' - click on the sprite to place the top/left corner, drag the mouse to place the bottom/right corner, release the mouse.'#10+
  'NOTE: use a rectangle only if the sprite never rotate. If it rotate, use polygon instead.'#10#10+
  'ADD A POLYGON:'#10+
  ' - click the Polygon button.'#10+
  ' - click on the sprite to place the first node, release the mouse.'#10+
  ' - click on another place to place the second node, release the mouse.'#10+
  ' - etc... repeat until the shape is done.'#10+
  ' - press Backspace key to delete the last point.'#10+
  ' - close the polygon by clicking on the first point.'#10+
  'NOTE: its not obligatory to close a polygon, press ENTER to interrupt the node input.'#10+
  'KEEP IN MIND: The more complex the polygon, the longer the collision test will take!'#10#10+
  'CANCEL THE CURRENT SHAPE:'#10+
  ' - press ESC key.'#10#10+
  'SELECTING A NODE:'#10+
  ' - left click on a node to select it.'#10#10+
  'SELECTING SEVERAL NODES:'#10+
  ' - left click + Shift key to add a node to the current selection.'#10+
  ' OR'#10+
  ' - place the mouse over a node and use the mouse wheel.'#10#10+
  'DELETE THE SELECTED NODES:'#10+
  ' - press Delete key.'#10#10+
  'MOVE ONE NODE:'#10+
  ' - simply drag the node with the mouse.'#10#10+
  'MOVE SEVERAL NODES:'#10+
  ' - select the nodes you want to move.'#10+
  ' - hold down the Shift key and drag one of the selected nodes.'#10#10+

  'SAVE AND EXIT TO SPRITE BANK:'#10+
  ' - enter a name for the sprite at the bottom of the panel.'#10+
  ' - then click ''Save to Sprite Bank''.'#10#10+
  'EXIT TO SPRITE BANK WITHOUT SAVING:'#10+
  '  click on the red cross on top right of the panel.');
end;

procedure TFrameToolsSpriteBuilder.BHelpPosturesClick(Sender: TObject);
begin
  form_showhelp.ShowHelp(
  'Postures are used to construct smooth animations.'#10+
  'For example, we can create 4 postures for a character with arms and legs to make it walk. '+
  'Then in your game, chains these postures with the right timing to have a smooth animation of a walking character.'#10+
  'The postures are also exported when you export the sprite to a Pascal unit.'#10#10+

  'CREATE NEW POSTURE:'#10+
  ' - from the sprite on the 2D view, move/rotate/scale the surfaces to create the desired posture.'#10+
  ' - enter a name for the posture.'#10+
  ' - click ''Add to posture list''.'#10#10+
  'EDIT AN EXISTING POSTURE:'#10+
  ' - select the posture in the list.'#10+
  ' - modify the posture on the 2D view (position/rotation/scale of the surfaces).'#10+
  ' - click ''Update posture''.'#10#10+

  'SAVE AND EXIT TO SPRITE BANK:'#10+
  ' - enter a name for the sprite at the bottom of the panel.'#10+
  ' - then click ''Save to Sprite Bank''.'#10#10+
  'EXIT TO SPRITE BANK WITHOUT SAVING:'#10+
  '  click on the red cross on top right of the panel.');
end;

procedure TFrameToolsSpriteBuilder.BHelpRootChildsClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('A complex sprite is made of one root surface and one or several childs.'#10+
  'The first surface is the root. It can be a TSprite or a TSpriteContainer. Other type don''t work for now.'#10+
  'See at the end of this text how to choose.'#10+
  'CREATING THE ROOT:'#10+
  ' - select the type of the root surface: TSprite or TSpriteContainer'#10+
  ' - if it''s a TSprite, choose a texture'#10+
  ' - enter a name for the root.'#10+
  ' - click ''+'' button.'#10#10+
  'CREATING A CHILD:'#10+
  ' - click on empty part on the 2D view to unselect all.'#10+
  ' - select the type of the surface (only TSprite work for now).'#10+
  ' - select a texture.'#10+
  ' - enter a name for the child. This name will be used later as variable name when you export the sprite to Pascal unit.'#10+
  ' - select its parent.'#10+
  ' - give a value to ZOrder: negative value means the child is behind its parent, positive value means the child is forward its parent.'#10+
  ' - click ''+'' button.'#10#10+
  'SELECTION:'#10+
  ' Left click on a surface to select it.'#10+
  ' Left click + SHIFT key to add/remove a surface to the current selection.'#10+
  ' Left click on empty place then drag the mouse to draw a rectangle. The surface inside the rectangle are selected.'#10+
  ' Left click + ALT key to alternate between overlaped surfaces.'#10#10+
  'When a surface is selected, another click on it will switch between scale handles and rotate handles.'#10+
  'If the surface is hidden, select it with ALT key then press R key to switch scale/rotate handles.'#10#10+
  'When a surface is selected, you can modify its pivot dragging the small cross at its center or directly by entering x and y values in the right panel.'#10#10+
  'MODIFY A SURFACE:'#10+
  ' select a surface then modify its parameters (texture, name, parent, coor, scale, angle, etc...).'#10#10+
  'MOVE A SURFACE:'#10+
  ' simply drag the surface with the mouse.'#10+
  ' Hold CTRL key to moves only horizontaly or only verticaly (the first that appears win).'#10#10+
  'ROTATE A SURFACE:'#10+
  ' select a surface, click another time on it then drag one of the rotate handles.'#10+
  ' Hold CTRL key to rotate by step of 15°.'#10#10+
  'SCALE A SURFACE:'#10+
  ' select a surface then drag one of the scale handles.'#10+
  ' Hold CTRL key to keep aspect ratio.'#10#10+
  'FLIP A SURFACE:'#10+
  ' select a surface then check the FlipH or FlipV.'#10#10+

  'VIEW:'#10+
  ' Mouse wheel to scroll up/down.'#10+
  ' Mouse wheel + SHIFT key to scroll left/right.'#10+
  ' Mouse wheel + CTRL key to zoom in/out.'#10#10+

  'SAVE THE SPRITE:'#10+
  ' - enter a name for the sprite at bottom of the panel.'#10+
  ' - then click ''Save to Sprite Bank''.'#10#10+
  'EXIT TO SPRITE BANK WITHOUT SAVING:'#10+
  '  click on the red cross on top right of the panel.'#10#10+

  'ROOT: TSPRITE OR TSPRITE CONTAINER ?'#10+
  'Which one should I choose for the root ?'#10+
  'To explain, let''s take some examples:'#10+
  'First, let''s take the example of a ship in a shoot ''em up game.'#10+
  'The ship is made of a body and two turret on his sides.'#10+
  'When the ship moves or turns, both turrets must follow.'#10+
  'So, we can choose the root as a TSprite with two childs: the left turret and the right turret.'#10#10+

  'Now, let''s take another example: a character facing to the right.'#10+
  'This character is made of a head, two arms, an abdomen and two legs.'#10+
  'To animate it, it makes sense to have the head and the two arms childs of the abdomen.'#10+
  'Because if the abdomen bend down for example, the head and arms follow. Its logical.'#10+
  'If we sets also the two legs childs of the abdomen, they will follow. Its not logical.'#10+
  'Look at your body: you can bend down but your legs stay on the same axis!'#10+
  'So, we need an intermediary between the legs and the abdomen.'#10+
  'A TSpriteContainer will be perfect because its a surface that don''t render anything except its childs.'#10+
  'We build the character as follow:'#10+
  ' - we start building our character with a TSpriteContainer as root.'#10+
  ' - abdomen is child of the container, arms and head are child of abdomen.'#10+
  ' - both legs are childs of the container.'#10+
  'Adjusting the pivot of each part, we can rotate them to have a good animation.'#10+
  'To move/rotate/scale the whole sprite, we can move/rotate/scale the container (root).');
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
                   (Angle.Value <> SE7.Value) or (ZOrderAsChild <> SE8.Value) or
                   (FlipH <> CBFlipH.Checked) or (FlipV <> CBFlipV.Checked);
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

procedure TFrameToolsSpriteBuilder.SE1Enter(Sender: TObject);
begin
  LastClickedIsControl := True;
end;

procedure TFrameToolsSpriteBuilder.ProcessTextureListOnModified(Sender: TObject);
begin
  FModified := True;
  Textures.FillComboBox(CBTextures); // refresh the combobox in childs page
end;

procedure TFrameToolsSpriteBuilder.ProcessAskToDeleteTextureEvent(
  aTextureItem: PTextureItem; var aCanDelete: boolean);
begin
  if Length(Surfaces.GetItemsThatUseThisTexture(aTextureItem)) > 0 then begin
    aCanDelete := False;
    ShowMessage('This texture is used by a surface, you can not delete it');
  end;
end;

procedure TFrameToolsSpriteBuilder.ProcessOnTextureChangedEvent(aTextureItem: PTextureItem);
var surf: ArrayOfPSurfaceDescriptor;
  i: Integer;
begin
  // recreate the surfaces that use this texture (if any)
  surf := Surfaces.GetItemsThatUseThisTexture(aTextureItem);
  if Length(surf) <> 0 then
    for i:=0 to High(surf) do
      surf[i]^.RecreateSurfaceBecauseTextureChanged;
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
    FlipH := CBFlipH.Checked;
    FlipV := CBFlipV.Checked;
  end;
  FWorkingChild^.UpdateHandlePosition;
end;

function TFrameToolsSpriteBuilder.Textures: TTextureList;
begin
  Result := ScreenSpriteBuilder.Textures;
end;

function TFrameToolsSpriteBuilder.Surfaces: TSpriteBuilderSurfaceList;
begin
  Result := TSpriteBuilderSurfaceList(ScreenSpriteBuilder.Surfaces);
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

  FrameTextureList.Clear;

  PC1.PageIndex := PC1.IndexOf(PageTextures);
  FInitializingWidget := False;
end;

constructor TFrameToolsSpriteBuilder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameTextureList := TFrameTextureList.Create(Self);
  FrameTextureList.Parent := PageTextures;
  FrameTextureList.Align := alClient;
  FrameTextureList.OnGetTextureList := @Textures;
  FrameTextureList.OnAskToDeleteTexture := @ProcessAskToDeleteTextureEvent;
  FrameTextureList.OnTextureChanged := @ProcessOnTextureChangedEvent;
  FrameTextureList.OnModified := @ProcessTextureListOnModified;
end;

procedure TFrameToolsSpriteBuilder.SetFocusToDummy;
begin
  SEDummy.SetFocus;
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

  FrameTextureList.UpdateTextureWidgetState;
  UpdatePostureWidgetState;
end;

procedure TFrameToolsSpriteBuilder.FillListBoxTextureNames;
begin
  FrameTextureList.FillListBox;
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
      CBFlipH.Checked := surface.FlipH;
      CBFlipV.Checked := surface.FlipV;
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
    CBFlipH.Checked := False;
    CBFlipV.Checked := False;
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

