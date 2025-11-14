unit frame_tool_spritebank;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Dialogs, Menus, u_screen_spritebank, u_texture_list,
  u_collisionbody_list, u_posture_list, u_spritebank;

type

  { TFrameToolSpriteBank }

  TFrameToolSpriteBank = class(TFrame)
    BDuplicate: TSpeedButton;
    BHelp: TSpeedButton;
    BRename: TSpeedButton;
    BUndo: TSpeedButton;
    BRedo: TSpeedButton;
    CBShowCollisionBody: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LB: TListBox;
    MIRedo: TMenuItem;
    MIUndo: TMenuItem;
    Panel8: TPanel;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    MIDuplicate: TMenuItem;
    MIDelete: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    SD1: TSaveDialog;
    BEdit: TSpeedButton;
    BExportToPascalUnit: TSpeedButton;
    BDelete: TSpeedButton;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure CBShowCollisionBodyChange(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure BExportToPascalUnitClick(Sender: TObject);
  private
    procedure FillLB;
    procedure ShowSprite(aIndex: integer);
    procedure UpdateWidgetState;
  private
    FUndoRedoManager: TSpriteBankUndoRedoManager;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    function Textures: TTextureList;
    function Surfaces: TSpriteBankSurfaceList;
    function Bodies: TBodyItemList;
    function Postures: TPostureList;
  end;

implementation

uses LCLType, Graphics, form_main, u_project, form_showhelp, u_utils, OGLCScene,
  BGRABitmap, BGRABitmapTypes;

{$R *.lfm}

{ TFrameToolSpriteBank }

procedure TFrameToolSpriteBank.LBSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  UpdateWidgetState;
  ShowSprite(LB.ItemIndex);

  i := LB.ItemIndex;
  if i = -1 then Edit1.Text := ''
    else Edit1.Text := 'T'+SpriteBank.Mutable[i]^.name;
end;

procedure TFrameToolSpriteBank.BExportToPascalUnitClick(Sender: TObject);
var t: TStringlist;
  nameClass: string;
begin
  if LB.ItemIndex = -1 then exit;

  nameClass := Trim(Edit1.Text);
  if Length(nameClass) < 2 then exit;
  if nameClass[1] <> 'T' then nameClass := 'T'+nameClass;
  SD1.FileName := Copy(nameClass, 2, Length(Edit1.Text));
  if not SD1.Execute then exit;

  t := TStringlist.Create;
  ExportSpriteToPascalUnit(t, Textures, Surfaces, Postures, LB.Items.Strings[LB.ItemIndex], nameClass, SD1.FileName,
    CheckBox1.Checked, CheckBox2.Checked);
  try
    t.SaveToFile(SD1.FileName);
    ShowMessage('Pascal unit created');
  finally
    t.Free;
  end;
end;

procedure TFrameToolSpriteBank.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LB.ItemIndex := LB.GetIndexAtXY(X, Y);
end;

procedure TFrameToolSpriteBank.BEditClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  LB.ItemIndex := -1;
  FormMain.EditSpriteInSpriteBank(LB.Items.Strings[i]);
end;

procedure TFrameToolSpriteBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Sprite Bank contains the definition of all sprites that you have defined in your project.'#10#10+
  'CREATE A NEW SPRITE:'#10+
  ' - click ''SPRITE BUILDER'' button.'#10#10+
  'EDIT AN EXISTING SPRITE:'#10+
  ' - select the sprite in the list.'#10+
  ' -  click ''Edit in Sprite Builder''.'#10#10+
  'EXPORT THE SELECTED SPRITE TO PASCAL UNIT:'#10+
  ' - select the sprite in the list.'#10+
  ' - enter a name for the class that be generated.'#10+
  ' - check the options if needed.'#10+
  ' - click ''Export sprite to Pascal unit''.'#10+
  ' - in the save dialogs that open, enter the name of the unit.'#10);
end;

procedure TFrameToolSpriteBank.BDeleteClick(Sender: TObject);
var oldName, newName: string;
  i, k: integer;
  item, newSprite: PSpriteBankItem;
begin
  i := LB.ItemIndex;

  if Sender = BDelete then begin
    if i = -1 then exit;
    if QuestionDlg('','Delete this sprite ?', mtWarning,
                   [mrOk, 'Delete', mrCancel, 'Cancel'], 0) = mrCancel then exit;
    FUndoRedoManager.AddActionDeleteSprite(i);
    SpriteBank.DeleteByIndex(i);
    LB.Items.Delete(i);
    ScreenSpriteBank.ClearView;
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
    until not SpriteBank.SpriteNameExists(newName);
    item := SpriteBank.GetItemByName(oldName);
    if item = NIL then exit;
    newSprite := SpriteBank.AddEmpty;
    newSprite^.name := newName;
    newSprite^.textures := item^.textures;
    newSprite^.surfaces := item^.surfaces;
    newSprite^.collisionbodies := item^.collisionbodies;
    newSprite^.postures := item^.postures;
    LB.ItemIndex := LB.Items.Add(newName);
    FUndoRedoManager.AddActionDuplicateSprite(LB.ItemIndex);
    Project.SetModified;
  end;

  if Sender = BRename then begin
    if i = -1 then exit;
    oldName := LB.Items.Strings[i];
    newName := InputBox('', 'Enter the new name:', oldName);
    if newName = oldName then exit;
    SpriteBank.GetItemByName(LB.Items.Strings[i])^.name := newName;
    FUndoRedoManager.AddActionRenameSprite(i, oldName);
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

procedure TFrameToolSpriteBank.CBShowCollisionBodyChange(Sender: TObject);
begin
  ShowSprite(LB.ItemIndex);
end;

procedure TFrameToolSpriteBank.FillLB;
var i: SizeUInt;
begin
  LB.Clear;
  if SpriteBank.Size = 0 then exit;
  for i:=0 to SpriteBank.Size-1 do
    LB.Items.Add(SpriteBank.Mutable[i]^.name);
end;

procedure TFrameToolSpriteBank.ShowSprite(aIndex: integer);
begin
  ScreenSpriteBank.ClearView;
  if aIndex <> -1 then
    ScreenSpriteBank.ShowSprite(aIndex);
end;

procedure TFrameToolSpriteBank.UpdateWidgetState;
begin
  BDuplicate.Enabled := LB.ItemIndex <> -1;
  BRename.Enabled := BDuplicate.Enabled;
  BDelete.Enabled := BDuplicate.Enabled;
  BUndo.Enabled := FUndoRedoManager.CanUndo;
  BRedo.Enabled := FUndoRedoManager.CanRedo;
end;

constructor TFrameToolSpriteBank.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FUndoRedoManager := TSpriteBankUndoRedoManager.Create;
end;

destructor TFrameToolSpriteBank.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TFrameToolSpriteBank.OnShow;
begin
  FillLB;
  UpdateWidgetState;
end;

function TFrameToolSpriteBank.Textures: TTextureList;
begin
  Result := ScreenSpriteBank.Textures;
end;

function TFrameToolSpriteBank.Surfaces: TSpriteBankSurfaceList;
begin
  Result := ScreenSpriteBank.Surfaces;
end;

function TFrameToolSpriteBank.Bodies: TBodyItemList;
begin
  Result := ScreenSpriteBank.Bodies;
end;

function TFrameToolSpriteBank.Postures: TPostureList;
begin
  Result := ScreenSpriteBank.Postures
end;

end.

