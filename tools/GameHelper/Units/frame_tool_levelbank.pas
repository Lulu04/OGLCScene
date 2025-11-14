unit frame_tool_levelbank;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls, Dialogs,
  ComCtrls, u_levelbank;

type

  { TFrameToolLevelBank }

  TFrameToolLevelBank = class(TFrame)
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BEdit: TSpeedButton;
    BExportLevelOnly: TSpeedButton;
    BExportToLevelAndScreen: TSpeedButton;
    BHelp: TSpeedButton;
    BRedo: TSpeedButton;
    BRename: TSpeedButton;
    BUndo: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label3: TLabel;
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
    LB: TListBox;
    Notebook1: TNotebook;
    PageLevelAndScreen: TPage;
    PageLevelOnly: TPage;
    Panel1: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel8: TPanel;
    RadioButton3: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SD1: TSaveDialog;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure BExportLevelOnlyClick(Sender: TObject);
    procedure BExportToLevelAndScreenClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
  private
    function UserWantCamera: boolean;
    function UserWantAllLevels: boolean;
    procedure InitLabelExportFromLevelName;
    function CheckExportToPascalScreenUnit: boolean;
    procedure DoExportToPascalScreenUnit;
  private
    FUndoRedoManager: TLevelBankUndoRedoManager;
    FInitializingWidget: boolean;
    procedure FillLB;
    procedure ShowLevel(aIndex: integer);
    procedure UpdateWidgetState;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    // called from project configuration window
    procedure ClearAll;
  end;

implementation

uses u_screen_levelbank, u_project, form_main,  form_showhelp,
  u_utils, u_layerlist, OGLCScene;

{$R *.lfm}

{ TFrameToolLevelBank }

procedure TFrameToolLevelBank.BDeleteClick(Sender: TObject);
var oldName, newName: string;
  i, k: integer;
  item, newLevel: PLevelBankItem;
begin
  i := LB.ItemIndex;

  if Sender = BDelete then begin
    if i = -1 then exit;
    if QuestionDlg('','Delete this level ?', mtWarning,
                   [mrOk, 'Delete', mrCancel, 'Cancel'], 0) = mrCancel then exit;
    FUndoRedoManager.AddActionDeleteLevel(i);
    LevelBank.DeleteByIndex(i);
    LB.Items.Delete(i);
    ScreenLevelBank.ClearView;
    Project.SetModified;
    UpdateWidgetState;
  end;

  if Sender = BDuplicate then begin
    if i = -1 then exit;
    oldName := LB.Items.Strings[i];
    k := 0;
    repeat
      inc(k);
      if k < 100 then newName := oldName+'_'+Format('%.2d', [k])
        else newName := oldName+'_'+k.ToString;
    until not LevelBank.NameExists(newName);
    item := LevelBank.GetItemByName(oldName);
    if item = NIL then exit;
    newLevel := LevelBank.AddEmpty;
    newLevel^.name := newName;
    newLevel^.surfaces := item^.surfaces;
    LB.ItemIndex := LB.Items.Add(newName);
    FUndoRedoManager.AddActionDuplicateLevel(LB.ItemIndex);
    Project.SetModified;
  end;

  if Sender = BRename then begin
    if i = -1 then exit;
    oldName := LB.Items.Strings[i];
    newName := Trim(InputBox('', 'Enter the new name:', oldName));
    if (newName = oldName) or (newName = '') then exit;
    LevelBank.GetItemByName(LB.Items.Strings[i])^.name := newName;
    FUndoRedoManager.AddActionRenameLevel(i, oldName);
    LB.Items.Strings[i] := newName;
    Project.SetModified;
  end;

  if Sender = BUndo then begin
    FUndoRedoManager.Undo;
    Project.SetModified;
    UpdateWidgetState;
  end;

  if Sender = BRedo then begin
    FUndoRedoManager.Redo;
    Project.SetModified;
    UpdateWidgetState;
  end;
end;

procedure TFrameToolLevelBank.BEditClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  LB.ItemIndex := -1;
  FormMain.EditLevelInLevelBank(LB.Items.Strings[i]);
end;

procedure TFrameToolLevelBank.BExportLevelOnlyClick(Sender: TObject);
var t: TStringlist;
  nameUnit, nameClass: string;
  i: integer;
  s, texFilename, sw, sh: string;
  p: SizeInt;
begin
  if LevelBank.Size = 0 then exit;
  if LevelBank.Textures.Size = 0 then exit;
  nameClass := Trim(Edit2.Text);
  if Length(nameClass) < 2 then exit;
  nameUnit := Trim(Edit1.Text);
  if Length(nameUnit) < 2 then exit;
  SD1.FileName := nameUnit;
  if not SD1.Execute then exit;

  nameUnit := ExtractFilename(SD1.FileName);

  t := TStringList.Create;

  //unit interface
  t.Add('{');
  AddFileGeneratedByGameHelper(t);
  t.AddText('  Usage:'#10+
            '    - call '+nameClass+'.LoadTexture() when you construct your texture atlas.'#10+
            '    - call LoadLevel() to load the current game level.'#10+
            '    - retrieve the world area with property WordArea.'#10+
            '}');
  t.Add('');
  AddInterface(t, ChangeFileExt(nameUnit, ''));
  t.AddText('{ '+nameClass+' }'#10#10+
            nameClass+' = class(TOGLCDecorManager)'#10+
            'protected'#10+
            '  function ScaleWF(AValue: single): single; override;'#10+
            '  function ScaleHF(AValue: single): single; override;'#10+
            'public'#10+
            '  class procedure LoadTexture(aAtlas: TAtlas); override;'#10+
            '  procedure BuildLevel(aIndex: integer);'#10+
            'end;'#10#10);
  // implementation
  {t.AddText('implementation'#10+
            'uses u_app, u_common;'); }
  AddImplementation(t);
  // CONST level data in string format
  t.Add('const');
  for i:=0 to LevelBank.Size-1 do
    if (RadioGroup1.ItemIndex = 1) or
       ((RadioGroup1.ItemIndex = 0) and LB.Selected[i]) then
      LevelBank.Mutable[i]^.ExportToPascalConst(t);
  t.Add('');

  t.AddText('{ '+nameClass+' }'+#10);
  // decor protected method
  AddImplementationOfDecorProtectedMethod(t, nameClass);

  // method for class LoadTexture()
  t.AddText('class procedure '+nameClass+'.LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
            'var dataFolder: string;'#10+
            'begin'#10+
            '  inherited LoadTexture(aAtlas);'#10+
            '  dataFolder := u_common.DataFolder;');
  with LevelBank.Textures do
    for i:=0 to Size-1 do begin
      // texture filename must be relative to application Data folder
      texFilename := Mutable[i]^.filename;
      p := texFilename.LastIndexOf(DirectorySeparator+'Data'+DirectorySeparator);
      texFilename := texFilename.Remove(0, p+6);
      texFilename := 'dataFolder+'''+texFilename+'''';

      s := '  '; //+Mutable[i]^.name + ' := ';
      if ExtractFileExt(Mutable[i]^.filename) = '.svg' then begin
        if Mutable[i]^.width = -1 then sw := '-1'
          else sw := 'u_app.ScaleW('+Mutable[i]^.width.ToString+')';
        if Mutable[i]^.height = -1 then sh := '-1'
          else sh := 'u_app.ScaleH('+Mutable[i]^.height.ToString+')';

        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImageFromSVG('+texFilename+
             ', '+sw+', '+sh+
             ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
             ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+
             ', 0);'
        else
          s := s + 'aAtlas.AddFromSVG('+texFilename+', '+sw+', '+sh+');';
      end else begin
        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImage('+texFilename+
          ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
          ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+');'
        else
          s := s + 'aAtlas.Add('+texFilename+');';
      end;
      t.Add(s);
    end;
  t.AddText('end;'#10+#10);

  // procedure to build a level
  t.AddText('procedure '+nameClass+'.BuildLevel(aIndex: integer);'#10+
            'begin'#10+
            '  case aIndex of');
  for i:=0 to LevelBank.Size-1 do
    t.Add('    '+i.ToString+': DoBuildLevel('+'Data_'+LevelBank.Mutable[i]^.name+');');
  t.AddText('    else raise exception.create(''level index out of bounds'');'#10+
            '  end;'#10+
            'end;'#10#10);

  // end of file
  t.Add('end.');
  try
    t.SaveToFile(SD1.FileName);
    ShowMessage('Pascal unit created');
  finally
    t.Free;
  end;
end;

procedure TFrameToolLevelBank.BExportToLevelAndScreenClick(Sender: TObject);
begin
  if Sender = RadioGroup2 then begin
    case RadioGroup2.ItemIndex of
      0: NoteBook1.PageIndex := NoteBook1.IndexOf(PageLevelOnly);
      1: NoteBook1.PageIndex := NoteBook1.IndexOf(PageLevelAndScreen);
    end;
  end;

  if Sender = CheckBox2 then begin
    Panel16.Enabled := CheckBox2.Checked;
  end;

  if Sender = Edit3 then begin
    InitLabelExportFromLevelName;
  end;

  if Sender = BExportToLevelAndScreen then begin
    if CheckExportToPascalScreenUnit then
      DoExportToPascalScreenUnit;
  end;
end;

procedure TFrameToolLevelBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Level Bank contains the levels that you have created for your game.'#10#10+
  'TO ADD A NEW LEVEL:'#10+
  ' - click ''LEVEL EDITOR'' button.'#10#10+
  'TO EDIT AN EXISTING LEVEL:'#10+
  ' - select the level in the list.'#10+
  ' - click ''Edit in Level Editor''.'#10#10+
  'EXPORT LEVELS TO PASCAL UNIT:'#10+
  '  exporting to a Pascal unit allow to easily include the levels definition to your program.'#10+
  ' - enter a class name, for example TGameLevels. Code will be generated to encapsulate the levels in a class with this name.'#10+
  ' - enter a unit name, for example ''u_gamelevels.pas''. The code is saved in a Pascal unit with this name.'#10+
  ' - click ''Export all levels to Pascal unit''.'#10#10+
  'HOW TO USE THIS UNIT IN YOUR PROGRAM:'#10+
  ' - under Lazarus, open the unit in the source editor.'#10+
  ' - add it the to project: Project->Project Inspector->Add->Files from editor, select the unit and click ''Add files''.'#10#10+
  'As you can see, the unit contains one class with:'#10+
  ' - a nice procedure LoadTexture(aAtlas: TAtlas); to load the level textures in your atlas.'#10+
  ' - a nice procedure BuildLevel(aIndex: integer); to build the level (index is 0 based).'#10+
  ' - a property WorldArea: TRectF to retrieve the world size. This property is not directly visible because it is declared in the ancestor of the class.');
end;

procedure TFrameToolLevelBank.Edit2Change(Sender: TObject);
begin
  if FInitializingWidget then exit;

  if Sender = Edit2 then
    Project.Config.LevelBankExportClassName := Trim(Edit2.Text);

  if Sender = Edit1 then
    Project.Config.LevelBankExportUnitName := Trim(Edit1.Text);

  Project.SetModified;
end;

procedure TFrameToolLevelBank.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LB.ItemIndex := LB.GetIndexAtXY(X, Y);
end;

procedure TFrameToolLevelBank.LBSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateWidgetState;
  ShowLevel(LB.ItemIndex);
end;

function TFrameToolLevelBank.UserWantCamera: boolean;
begin
  Result := CheckBox2.Checked;
end;

function TFrameToolLevelBank.UserWantAllLevels: boolean;
begin
  Result := RadioGroup1.ItemIndex = 1;
end;

procedure TFrameToolLevelBank.InitLabelExportFromLevelName;
var s: string;
begin
  s := Trim(Edit3.Text);
  if s <> '' then begin
    Label43.Caption := 'screen_'+s.ToLower;
    Label41.Caption := 'TScreen'+s;
    Label45.Caption := 'Screen'+s;
    Label47.Caption := 'TLevel'+s;
    Label49.Caption := 'FLevel'+s;
  end else begin
    Label43.Caption := '';
    Label41.Caption := '';
    Label45.Caption := '';
    Label47.Caption := '';
    Label49.Caption := '';
  end;
end;

function TFrameToolLevelBank.CheckExportToPascalScreenUnit: boolean;
begin
  Result := (LevelBank.Size > 0) and
            (Length(Label43.Caption) > 7) and
            (Length(Label41.Caption) > 1) and
            (Length(Label45.Caption) > 6) and
            (Length(Label47.Caption) > 6) and
            (Length(Label49.Caption) > 6);
end;

procedure TFrameToolLevelBank.DoExportToPascalScreenUnit;
var t: TStringList;
  nameUnit, screenClassName, screenVarName, decorClassName, decorVarName,
    texFilename, s, sw, sh: string;
  layersUsed, temp: TArrayOfInteger;
  i: integer;
  p: SizeInt;
begin
  nameUnit := Label43.Caption;
  SD1.FileName := nameUnit;
  if not SD1.Execute then exit;
  screenClassName := Label41.Caption;
  screenVarName := Label45.Caption;
  decorClassName := Label47.Caption;
  decorVarName := Label49.Caption;

  t := TStringList.Create;
  t.Add('{');
  AddFileGeneratedByGameHelper(t);
  t.AddText('  Usage:'#10+
            '   - add this file in your project with the Project Inspector'#10+
            '   - add the creation of the screen in ''form_main.LoadCommonData()'''#10+
            '   - add the destruction of the screen in ''form_main.FreeCommonData()'''#10+
            '   - insert ''FScene.RunScreen('+screenVarName+')'' somewhere in your code');
  t.Add('}');
  t.Add('');
  AddInterface(t, nameUnit);

  // declaration of decor class
  AddDeclarationOfDecorClass(t, decorClassName);

  t.AddText('{ '+screenClassName+'}'#10#10+
            screenClassName+' = class(TScreenTemplate)');

  // retrieve the index of layers used
  layersUsed := NIL;
  if UserWantCamera then begin
    for i:=0 to LevelBank.Size-1 do
      if UserWantAllLevels or (not UserWantAllLevels and LB.Selected[i]) then begin
        temp := LevelBank.Mutable[i]^.GetUserLayerIndexesUsed;
        if Length(temp) > Length(layersUsed) then
          layersUsed := Copy(temp);
      end;
  end;

  t.Add('private');
  // declare the camera variables
  if Length(layersUsed) > 0 then
    for i:=0 to High(layersUsed) do
      t.Add('  FCamera'+(i+1).ToString+': TOGLCCamera;');
  // declare the decor variable
  t.Add('  '+decorVarName+': '+decorClassName+';');
  // declare the atlas variable (if needed)
  if CheckBox1.Checked then
    t.Add('  FAtlas: TAtlas;');
  t.AddText('public'#10+
            '  procedure CreateObjects; override;'#10+
            '  procedure FreeObjects; override;'#10+
            'end;');
  t.Add('');
  AddImplementation(t);

  // CONST decor data in string format
  t.Add('const');
  for i:=0 to LevelBank.Size-1 do
    if (RadioGroup1.ItemIndex = 1) or
       ((RadioGroup1.ItemIndex = 0) and LB.Selected[i]) then
      LevelBank.Mutable[i]^.ExportToPascalConst(t);

  // implementation of decor.LoadTexture()
  t.AddText('class procedure '+decorClassName+'.LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
            'var dataFolder: string;'#10+
            'begin'#10+
            '  inherited LoadTexture(aAtlas);'#10+
            '  dataFolder := u_common.DataFolder;');
  with LevelBank.Textures do
    for i:=0 to Size-1 do begin
      // texture filename must be relative to application Data folder
      texFilename := Mutable[i]^.filename;
      p := texFilename.LastIndexOf(DirectorySeparator+'Data'+DirectorySeparator);
      texFilename := texFilename.Remove(0, p+6);
      texFilename := 'dataFolder+'''+texFilename+'''';

      s := '  '; //+Mutable[i]^.name + ' := ';
      if ExtractFileExt(Mutable[i]^.filename) = '.svg' then begin
        if Mutable[i]^.width = -1 then sw := '-1'
          else sw := 'u_common.ScaleW('+Mutable[i]^.width.ToString+')';
        if Mutable[i]^.height = -1 then sh := '-1'
          else sh := 'u_common.ScaleH('+Mutable[i]^.height.ToString+')';

        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImageFromSVG('+texFilename+
             ', '+sw+', '+sh+
             ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
             ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+
             ', 0);'
        else
          s := s + 'aAtlas.AddFromSVG('+texFilename+', '+sw+', '+sh+');';
      end else begin
        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImage('+texFilename+
          ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
          ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+');'
        else
          s := s + 'aAtlas.Add('+texFilename+');';
      end;
      t.Add(s);
    end;
  t.AddText('end;'#10+#10);

  // implementation of decor.BuildLevel
  AddImplementationOfDecorBuildLevel(t, decorClassName);

  // implementation of screen class .CreateObjects
  t.Add('{ '+screenClassName+'}');
  t.Add('');
  t.AddText('procedure '+screenClassName+'.CreateObjects;'#10+
            'begin');

  if CheckBox1.Checked then begin
    // we need a local atlas
    t.AddText('  // atlas creation'#10+
              '  FAtlas := FScene.CreateAtlas;'#10+
              '  FAtlas.Spacing := 2;'#10+
              '  AdditionnalScale := 1.0;'#10+
              '  '+decorClassName+'.LoadTexture(FAtlas);'#10+
              '  FAtlas.TryToPack;'#10+
              '  FAtlas.Build;'#10);
  end;
  // camera creation
  if Length(layersUsed) = 1 then begin
    t.Add('  FCamera := FScene.CreateCamera;');
    s := '  FCamera.AssignToLayers([';
    for i:=0 to High(layersUsed) do begin
      s := s + Layers.Names[layersUsed[i]];
      if i < High(layersUsed) then s := s + ', ';
    end;
    t.Add(s+']);');
    if RadioButton3.Checked then
      t.Add('  FCamera.AutoFollow.ComputeBoundsFromWorldArea('+decorVarName+'.WorldArea);')
    else t.Add('  FCamera.AutoFollow.ApplyBounds := False;');
  end else begin
    for i:=0 to High(layersUsed) do begin
      t.Add('  FCamera'+(i+1).ToString+' := FScene.CreateCamera;');
      t.Add('  FCamera'+(i+1).ToString+'.AssignToLayer('+Layers.Names[layersUsed[i]]+');');
      if RadioButton3.Checked then
        t.Add('  FCamera'+(i+1).ToString+'.AutoFollow.ComputeBoundsFromWorldArea('+decorVarName+'.WorldArea);')
      else t.Add('  FCamera'+(i+1).ToString+'.AutoFollow.ApplyBounds := False;');
    end;
  end;
  t.Add('end;');
  t.Add('');

  // implementation of screen class .FreeObjects
  t.AddText('procedure '+screenClassName+'.FreeObjects;'#10+
            'begin'#10+
            '  FreeAndNil('+decorVarName+');');
  if Length(layersUsed) > 0 then
    for i:=0 to High(layersUsed) do
      t.Add('  FScene.KillCamera(FCamera'+(i+1).ToString+');');
  if CheckBox1.Checked then
    t.Add('  FreeAndNil(FAtlas);');
  t.AddText('  FScene.ClearAllLayer;'#10+
            'end;'#10);

  t.Add('');
  t.Add('end.');

  try
    t.SaveToFile(ExtractFilePath(SD1.FileName)+nameUnit+'.pas');
  finally
    t.Free;
  end;
end;

procedure TFrameToolLevelBank.FillLB;
var i: SizeUInt;
begin
  LB.Clear;
  if LevelBank.Size = 0 then exit;
  for i:=0 to LevelBank.Size-1 do
    LB.Items.Add(LevelBank.Mutable[i]^.name);
end;

procedure TFrameToolLevelBank.ShowLevel(aIndex: integer);
begin
  ScreenLevelBank.ClearView;
  if aIndex <> -1 then
    ScreenLevelBank.ShowLevel(aIndex);
end;

procedure TFrameToolLevelBank.UpdateWidgetState;
begin
  BDuplicate.Enabled := LB.ItemIndex <> -1;
  BRename.Enabled := BDuplicate.Enabled;
  BDelete.Enabled := BDuplicate.Enabled;
  BUndo.Enabled := FUndoRedoManager.CanUndo;
  BRedo.Enabled := FUndoRedoManager.CanRedo;
end;

constructor TFrameToolLevelBank.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FUndoRedoManager := TLevelBankUndoRedoManager.Create;
end;

destructor TFrameToolLevelBank.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TFrameToolLevelBank.OnShow;
begin
  FInitializingWidget := True;
  FillLB;
  UpdateWidgetState;
  Edit1.Text := Project.Config.LevelBankExportUnitName;
  Edit2.Text := Project.Config.LevelBankExportClassName;

  FInitializingWidget := False;
end;

procedure TFrameToolLevelBank.ClearAll;
begin
  LB.Clear;
end;

end.

