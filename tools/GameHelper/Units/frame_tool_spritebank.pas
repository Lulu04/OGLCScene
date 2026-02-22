unit frame_tool_spritebank;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Dialogs, Menus, u_screen_spritebank, u_texture_list, u_surface_list,
  u_collisionbody_list, u_posture_list, u_spritebank, Types;

type

  { TFrameToolSpriteBank }

  TFrameToolSpriteBank = class(TFrame)
    BDeleteSprite: TSpeedButton;
    BExportPascalUnit: TSpeedButton;
    BRemovePascalUnit: TSpeedButton;
    BNewSprite: TSpeedButton;
    BDuplicateSprite: TSpeedButton;
    BHelp: TSpeedButton;
    BImportSprite: TSpeedButton;
    BRenameSprite: TSpeedButton;
    BUndo: TSpeedButton;
    BRedo: TSpeedButton;
    CBShowCollisionBody: TCheckBox;
    Label24: TLabel;
    LB: TListBox;
    MIRedo: TMenuItem;
    MIUndo: TMenuItem;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel8: TPanel;
    PanelSpriteTool: TPanel;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    MIDuplicate: TMenuItem;
    MIDelete: TMenuItem;
    SD1: TSaveDialog;
    BEditInSpriteBuilder: TSpeedButton;
    Splitter1: TSplitter;
    procedure BHelpClick(Sender: TObject);
    procedure BNewSpriteClick(Sender: TObject);
    procedure CBShowCollisionBodyChange(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    procedure FillLB;
    procedure ShowSprite(aIndex: integer);
    procedure UpdateWidgetState;
    procedure DoDeleteSelectedSprite;
    procedure DoDuplicateSelectedSprite;
    procedure DoRenameSelectedSprite;
    procedure DoEditInSpriteBuilder;
    procedure DoNewSprite;
    procedure DoImportSpriteFromAnotherProject;
    procedure DoAddPascalUnitToLazarusProject;
    procedure DoRemovePascalUnitFromLazarusProject;
    procedure DoSaveSpriteBank;
  private
    FUndoRedoManager: TSpriteBankUndoRedoManager;
    procedure HideToolPanels;
    procedure ShowToolPanel;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    function Textures: TTextureList;
    function Surfaces: TSurfaceList;
    function Bodies: TBodyItemList;
    function Postures: TPostureList;
  end;

implementation

uses LCLType, Graphics, form_main, u_project, form_showhelp,
  u_target_lazarusproject, u_connection_to_ide, OGLCScene, BGRABitmap,
  BGRABitmapTypes, utilitaire_fichier;

{$R *.lfm}

{ TFrameToolSpriteBank }

procedure TFrameToolSpriteBank.LBSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateWidgetState;
  if LB.ItemIndex <> -1 then ShowToolPanel
    else HideToolPanels;
  Application.ProcessMessages;
  ShowSprite(LB.ItemIndex);
end;

procedure TFrameToolSpriteBank.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LB.ItemIndex := LB.GetIndexAtXY(X, Y);
end;

procedure TFrameToolSpriteBank.LBMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Shift := Shift;
  WheelDelta := WheelDelta;
  MousePos := MousePos;
  Handled := True;
  HideToolPanels;
end;

procedure TFrameToolSpriteBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Sprite Bank contains the sprites that you have defined in your project.'#10#10+
  'CREATE A NEW SPRITE:'#10+
  ' - click the ''+New'' button.'#10#10+
  'IMPORT A SPRITE FROM AN ANOTHER PROJECT:'#10+
  ' - click import and follow the instruction in the window'#10#10+
  'EDIT/RENAME/DUPLICATE/DELETE AN EXISTING SPRITE:'#10+
  ' - select a sprite in the list and click the appropriate button on the right.'#10#10+
  'SPRITE AND PASCAL UNIT:'#10+
  ' - a Pascal unit is added to the Lazarus project each time a sprite is created.'#10+
  ' - when the sprite is modified under Game Helper, the unit is updated.'#10+
  ' - The unit''s name is formated as "u_sprite_<sprite_name>.pas" and is located in directory Units\Sprites\ in the Lazarus Project');
end;

procedure TFrameToolSpriteBank.BNewSpriteClick(Sender: TObject);
begin
  if Sender = BNewSprite then
    DoNewSprite;

  if Sender = BImportSprite then
    DoImportSpriteFromAnotherProject;

  if Sender = BDeleteSprite then
    DoDeleteSelectedSprite;

  if Sender = BDuplicateSprite then
    DoDuplicateSelectedSprite;

  if Sender = BRenameSprite then
    DoRenameSelectedSprite;

  if Sender = BEditInSpriteBuilder then begin
    DoEditInSpriteBuilder;
  end;

  if Sender = BExportPascalUnit then begin
    DoAddPascalUnitToLazarusProject;
  end;

  if Sender = BRemovePascalUnit then begin
    DoRemovePascalUnitFromLazarusProject;
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
  BNewSprite.Enabled := True;
  BImportSprite.Enabled := True;
  BUndo.Enabled := False; //FUndoRedoManager.CanUndo;
  BRedo.Enabled := False; //FUndoRedoManager.CanRedo;
end;

procedure TFrameToolSpriteBank.DoDeleteSelectedSprite;
var i: integer;
  item: PSpriteBankItem;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;
  if QuestionDlg('','Delete this sprite ?', mtWarning,
                 [mrOk, 'Delete', mrCancel, 'Cancel'], 0) = mrCancel then exit;

  item := SpriteBank.GetItemByName(LB.Items.Strings[i]);
  if item = NIL then begin
    ShowMessage('Item not found in the Sprite Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // remove unit from target lazarus project
  Project.Config.TargetLazarusProject.Unit_RemoveFromProject(item^.name, ulSprites, uePas);

  // delete file in Units folder
  Project.Config.TargetLazarusProject.Unit_DeleteFile(item^.name, ulSprites, uePas);

  // delete in sprite bank
  SpriteBank.DeleteByIndex(i);
  DoSaveSpriteBank;

  // delete in listbox
  LB.Items.Delete(i);

  ScreenSpriteBank.ClearView;
  UpdateWidgetState;
end;

procedure TFrameToolSpriteBank.DoDuplicateSelectedSprite;
var srcName, dstName, srcFilename, dstFilename, s1, s2: string;
  i, k: integer;
  srcItem, dstItem: PSpriteBankItem;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;

  // retrieve the source item
  srcName := LB.Items.Strings[i];
  srcItem := SpriteBank.GetItemByName(srcName);
  if srcItem = NIL then begin
    ShowMessage('Source item not found in the Sprite Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // create a unique name for the new sprite
  k := 0;
  repeat
    inc(k);
    if k < 100 then dstName := srcName+'_'+Format('%.2d', [k])
      else dstName := srcName+'_'+k.ToString;
  until not SpriteBank.SpriteNameExists(dstName);

  // duplicate unit file with new name
  srcFilename := Project.Config.TargetLazarusProject.SpriteNameToUnitFullFilename(srcName);
  dstFilename := Project.Config.TargetLazarusProject.SpriteNameToUnitFullFilename(dstName);
  if not CopieFichier(srcFilename, dstFilename, True) then begin
    ShowMessage('Can not create the duplicated pascal unit on the disk...'+LineEnding+'Operation canceled');
    exit;
  end;
  // replace the class name
  s1 := 'T'+srcName;
  s2 := 'T'+dstName;
  Project.Config.TargetLazarusProject.Unit_ReplaceAllOccurrencesOf(dstFilename, s1, s2);
  // rename unit inside the file
  s1 := Project.Config.TargetLazarusProject.SpriteNameToUnitNameWithoutExt(srcName);
  s2 := Project.Config.TargetLazarusProject.SpriteNameToUnitNameWithoutExt(dstName);
  Project.Config.TargetLazarusProject.Unit_ReplaceAllOccurrencesOf(dstFilename, s1, s2);

  // add the new item in the bank and save
  dstItem := SpriteBank.AddEmpty;
  // retrieve the src item because its adress can change
  srcItem := SpriteBank.GetItemByName(srcName);
  srcItem^.DuplicateTo(dstItem);
  dstItem^.name := dstName;
  DoSaveSpriteBank;

  // generate the Pascal unit
  dstItem^.ExportSpriteToPascalUnit;

  // add the new name in the listbox
  LB.ItemIndex := LB.Items.Add(dstName);

  // add the new unit to project
  Project.Config.TargetLazarusProject.Unit_AddToProject(dstItem^.GetUnitName, ulSprites, uePas);

  //FUndoRedoManager.AddActionDuplicateSprite(LB.ItemIndex);
end;

procedure TFrameToolSpriteBank.DoRenameSelectedSprite;
var oldName, newName, oldFilename, newFilename, s1, s2: string;
  i: integer;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;

  oldName := LB.Items.Strings[i];
  newName := Trim(InputBox('', 'Enter the new name:', oldName));
  if newName = oldName then exit;
  if SpriteBank.SpriteNameExists(newName) then begin
    ShowMessage('A sprite named "'+newName+'" already exists. Please, try with another name');
    exit;
  end;

  // ask the IDE to remove old unit from project
  IdeConnect.AskIdeToRemoveUnitFromProject(oldName, ulSprites, uePas);

  // rename file on disk
  oldFilename := Project.Config.TargetLazarusProject.Unit_GetFullFilename(oldName, ulSprites, uePas);
  newFilename := Project.Config.TargetLazarusProject.Unit_GetFullFilename(newName, ulSprites, uePas);
  if not RenommeFichier(oldFilename, newFilename) then begin
    ShowMessage('Can not rename Pascal unit on disk...'+LineEnding+'Operation canceled');
    IdeConnect.AskIdeToAddUnitToProject(oldName, ulSprites, uePas);
    exit;
  end;

  // rename class name in the new file
  s1 := 'T'+oldName;
  s2 := 'T'+newName;
  Project.Config.TargetLazarusProject.Unit_ReplaceAllOccurrencesOf(newFilename, s1, s2);
  // rename unit inside the file
  s1 := Project.Config.TargetLazarusProject.SpriteNameToUnitNameWithoutExt(oldName);
  s2 := Project.Config.TargetLazarusProject.SpriteNameToUnitNameWithoutExt(newName);
  Project.Config.TargetLazarusProject.Unit_ReplaceAllOccurrencesOf(newFilename, s1, s2);

  // ask the IDE to add the renamed unit in project
  IdeConnect.AskIdeToAddUnitToProject(newName, ulSprites, uePas);

  // change name in sprite bank and save
  SpriteBank.GetItemByName(oldName)^.name := newName;
  DoSaveSpriteBank;

  // change name in listbox
  LB.Items.Strings[i] := newName;

end;

procedure TFrameToolSpriteBank.DoEditInSpriteBuilder;
var i: integer;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;
  LB.ItemIndex := -1;

  FrameToolsSpriteBuilder.EditSpriteInSpriteBank(LB.Items.Strings[i]);
  FormMain.ShowPageSpriteBuilder;
end;

procedure TFrameToolSpriteBank.DoNewSprite;
begin
  HideToolPanels;
  FrameToolsSpriteBuilder.EditNewSprite;
  FormMain.ShowPageSpriteBuilder;
end;

procedure TFrameToolSpriteBank.DoImportSpriteFromAnotherProject;
begin
  HideToolPanels;
  // to do
end;

procedure TFrameToolSpriteBank.DoAddPascalUnitToLazarusProject;
var i: integer;
  item: PSpriteBankItem;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;

  // retrieve the item
  item := SpriteBank.GetItemByName(LB.Items.Strings[i]);
  if item = NIL then begin
    ShowMessage('item not found in the Sprite Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // generate the unit
  item^.ExportSpriteToPascalUnit;

  // add it to the lazarus project
  Project.Config.TargetLazarusProject.Unit_AddToProject(item^.name, ulSprites, uePas);
end;

procedure TFrameToolSpriteBank.DoRemovePascalUnitFromLazarusProject;
var i: integer;
  item: PSpriteBankItem;
begin
  HideToolPanels;
  i := LB.ItemIndex;
  if i = -1 then exit;

  // retrieve the item
  item := SpriteBank.GetItemByName(LB.Items.Strings[i]);
  if item = NIL then begin
    ShowMessage('item not found in the Sprite Bank... it''s a bug!'+LineEnding+'Operation Canceled');
    exit;
  end;

  // remove the unit from lazarus project
  Project.Config.TargetLazarusProject.Unit_RemoveFromProject(item^.name, ulSprites, uePas);
end;

procedure TFrameToolSpriteBank.DoSaveSpriteBank;
begin
  SpriteBank.SaveToPath(Project.Config.TargetLazarusProject.GetFolderGameHelperFiles);
end;

procedure TFrameToolSpriteBank.HideToolPanels;
begin
  PanelSpriteTool.Visible := False;
end;

procedure TFrameToolSpriteBank.ShowToolPanel;
var r, r1: TRect;
  i: integer;
  procedure MakePanelVisible(aPanel: TPanel);
  var x: integer;
  begin
    x := r.Right+ScaleDesignToForm(10);
    if x+aPanel.Width > LB.ClientWidth then
      x := LB.ClientWidth-aPanel.Width;
    aPanel.Left := x;
    aPanel.Top := r.Top + (r.Height - aPanel.Height) div 2;
    aPanel.Visible := True;
  end;

begin
  HideToolPanels;

  i := LB.ItemIndex;
  if i = -1 then exit;
  r := LB.ItemRect(i);
  r1.TopLeft := LB.ClientToParent(r.TopLeft, Self); //LB.ClientToScreen(r);
  r1.BottomRight := LB.ClientToParent(r.BottomRight, Self);
  r := r1;
  //r := self.ScreenToClient(r);
  MakePanelVisible(PanelSpriteTool);
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
  HideToolPanels;
end;

function TFrameToolSpriteBank.Textures: TTextureList;
begin
  Result := ScreenSpriteBank.Textures;
end;

function TFrameToolSpriteBank.Surfaces: TSurfaceList;
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

