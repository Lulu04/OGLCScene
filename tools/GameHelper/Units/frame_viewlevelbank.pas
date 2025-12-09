unit frame_viewlevelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus, Dialogs, ExtCtrls,
  Buttons, u_levelbank, Types;

type

  { TFrameViewLevelBank }

  TFrameViewLevelBank = class(TFrame)
    BGroupDescription: TSpeedButton;
    BLevelDescription: TSpeedButton;
    BRenameLevel: TSpeedButton;
    BDeleteGroup: TSpeedButton;
    BDeleteLevel: TSpeedButton;
    BDuplicateLevel: TSpeedButton;
    BEditLevel: TSpeedButton;
    MIEditLevel: TMenuItem;
    MILevelDescription: TMenuItem;
    MIGroupDescription: TMenuItem;
    MIRenameGroup: TMenuItem;
    MIDeleteGroup: TMenuItem;
    MIDuplicateLevel: TMenuItem;
    MINewGroup: TMenuItem;
    MIAddLevel: TMenuItem;
    MIRenameLevel: TMenuItem;
    MIDeleteLevel: TMenuItem;
    PanelLevel: TPanel;
    PanelRoot: TPanel;
    PanelGroup: TPanel;
    PopupGroup: TPopupMenu;
    PopupLevel: TPopupMenu;
    PopupEmpty: TPopupMenu;
    BAddGroup: TSpeedButton;
    BAddLevel: TSpeedButton;
    BRenameGroup: TSpeedButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TV: TTreeView;
    procedure BAddGroupClick(Sender: TObject);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TVMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TVResize(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FIsEditable: boolean;
    FOnSelectionChange: TNotifyEvent;
    function NodeIsRoot(aNode: TTreeNode): boolean;
    function NodeIsGroup(aNode: TTreeNode): boolean;
    function NodeisLevel(aNode: TTreeNode): boolean;
    procedure DoAddgroup;
    procedure DoAddLevel;
    procedure DoRenameGroup;
    procedure DoChangeGroupDescription;
    procedure DoDeleteGroup;
    procedure DoRenameLevel;
    procedure DoDuplicateLevel;
    procedure DoChangeLevelDescription;
    procedure DoDeleteLevel;
    procedure DoEditLevel;
    procedure DoSaveLevelBank;
  private
    procedure HideToolPanels;
    procedure ShowToolPanel;
  public
    procedure FillWithLevelBank;
    procedure Clear;

    function ASelectionIsAvailable: boolean;
    function SelectedIsRoot: boolean;
    function SelectedIsGroup: boolean;
    function SelectedIsLevel: boolean;
    function GetSelectedWorkingLevelGroup: TLevelGroup;
    function GetSelectedLevel: PLevelBankItem;
    function GetSelectedText: string;

    // delete the selected group or level. Do nothing if nothing is selected
    procedure DeleteSelected;

    property IsEditable: boolean read FIsEditable write FIsEditable;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;

implementation

uses Graphics, LCLType, LazUTF8, LCLIntf, u_project, u_common,
  u_target_lazarusproject, form_main, u_connection_to_ide,
  form_enter_description, u_utils, u_resourcestring, LazFileUtils;

{$R *.lfm}

{ TFrameViewLevelBank }

procedure TFrameViewLevelBank.TVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  Sender := Sender;
  Shift := Shift;
  n := TV.GetNodeAt(X, Y);
  if n = NIL then
    TV.Selected := NIL;

  if (Button = mbLeft) and (TV.Selected <> NIL) then
    ShowToolPanel;
end;

procedure TFrameViewLevelBank.TVMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var node: TTreeNode;
  group: TLevelGroup;
  level: PLevelBankItem;
begin
  // update hint with group or level description
  TV.ShowHint := False;
  Application.ProcessMessages;
  node := TV.GetNodeAt(X, Y);
  if NodeIsGroup(node) then begin
    group := LevelBank.GetGroupByName(node.Text);
    if group <> NIL then begin
      if group.GroupDescription <> '' then TV.Hint := sDescription+LineEnding+group.GroupDescription
        else TV.Hint := sThereIsntGroupDesc;
      TV.ShowHint := True;
    end;
  end
  else
  if NodeIsLevel(node) then begin
    group := LevelBank.GetGroupByName(node.Parent.Text);
    if group <> NIL then begin
      level := group.GetItemByName(node.Text);
      if level <> NIL then begin
        if level^.description <> '' then TV.Hint := sDescription+LineEnding+level^.description
          else TV.Hint := sThereIsntLevelDesc;
        TV.ShowHint := True;
      end;
    end;
  end;
end;

procedure TFrameViewLevelBank.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
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

procedure TFrameViewLevelBank.BAddGroupClick(Sender: TObject);
begin
  if (Sender = BAddGroup) or (Sender = MINewGroup) then DoAddgroup;
  if (Sender = BAddLevel) or (Sender = MIAddLevel) then DoAddLevel;
  if (Sender = BRenameGroup) or (Sender = MIRenameGroup) then DoRenameGroup;
  if (Sender = BGroupDescription) or (Sender = MIGroupDescription) then DoChangeGroupDescription;
  if (Sender = BDeleteGroup) or (Sender = MIDeleteGroup) then DoDeleteGroup;
  if (Sender = BRenameLevel) or (Sender = MIRenameLevel) then DoRenameLevel;
  if (Sender = BDuplicateLevel) or (Sender = MIDuplicateLevel) then DoDuplicateLevel;
  if (Sender = BLevelDescription) or (Sender = MILevelDescription) then DoChangeLevelDescription;
  if (Sender = BDeleteLevel) or (Sender = MIDeleteLevel) then DoDeleteLevel;
  if (Sender = BEditLevel) or (Sender = MIEditLevel) then DoEditLevel;
end;

procedure TFrameViewLevelBank.TVMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  Sender := Sender;
  Shift := Shift;
  X := X;
  Y := Y;
  if (Button = mbRight) and IsEditable then begin
    n := TV.Selected;
    if (n = NIL) or NodeIsRoot(n) then PopupEmpty.Popup
    else
    if NodeIsGroup(n) then PopupGroup.PopUp
    else
    if NodeIsLevel(n) then PopupLevel.PopUp;
  end;
end;

procedure TFrameViewLevelBank.TVMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Sender := Sender;
  Shift := Shift;
  WheelDelta := WheelDelta;
  MousePos := MousePos;
  Handled := Handled;
  HideToolPanels;
end;

procedure TFrameViewLevelBank.TVResize(Sender: TObject);
begin
  Sender := Sender;
  HideToolPanels;
end;

procedure TFrameViewLevelBank.TVSelectionChanged(Sender: TObject);
var n: TTreeNode;
begin
  Sender := Sender;
  WorkingLevelGroup := NIL;
  n := TV.Selected;
  if n <> NIL then begin
    if NodeIsLevel(n) then n := n.Parent;
    if NodeIsGroup(n) then
      WorkingLevelGroup := LevelBank.GetGroupByName(n.Text);
  end;

  if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);

  ShowToolPanel;
end;

function TFrameViewLevelBank.NodeIsRoot(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 0;
end;

function TFrameViewLevelBank.NodeIsGroup(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 1;
end;

function TFrameViewLevelBank.NodeisLevel(aNode: TTreeNode): boolean;
begin
  if aNode = NIL then exit(False);
  Result := aNode.Level = 2;
end;

procedure TFrameViewLevelBank.DoAddgroup;
var groupname: String;
  rootnode: TTreeNode;
begin
  groupname := Trim(InputBox('', sEnterANameForTheGroup, ''));
  if groupname = '' then exit;
  if LevelBank.GroupNameExists(groupname) then begin
    ShowMessage(Format(sTheGroupxxAlreadyExists, [groupname]));
    exit;
  end;

  // add the group to the bank
  LevelBank.AddGroup(groupname);
  // add the group to the tree
  rootnode := TV.Items.GetFirstNode;
  with TV.Items.AddChild(rootnode, groupname) do begin
    SelectedIndex := 0;
    ImageIndex := 0;
    MakeVisible;
  end;

  // generate the new unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoAddLevel;
begin
  FormMain.ShowPageLevelEditor;
end;

procedure TFrameViewLevelBank.DoRenameGroup;
var group: TLevelGroup;
  oldName, newName: String;
begin
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;
  oldName := group.GroupName;

  newName := Trim(InputBox('', sEnterTheNewName, oldName));
  if (newName = '') or (newName = oldName) then exit;
  if LevelBank.GroupNameExists(newName) then begin
    ShowMessage(Format(sTheGroupxxAlreadyExists, [newName]));
    exit;
  end;

  // change name in tree view
  TV.Selected.text := newName;

  // change name in level bank
  group.GroupName := newName;

  // generate the level unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoChangeGroupDescription;
var group: TLevelGroup;
  newDescription: string;
begin
  if not SelectedIsGroup then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  FormEnterDescription := TFormEnterDescription.Create(NIL);
  try
    FormEnterDescription.DescriptionAsText := group.GroupDescription;
    if FormEnterDescription.ShowModal <> mrOk then exit;
    newDescription := FormEnterDescription.DescriptionAsText;
  finally
    FormEnterDescription.Free;
  end;
  if newDescription = group.GroupDescription then exit;

  group.GroupDescription := ReplaceCharacterNotAllowedByUnderscore(newDescription);

  // generate the level unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoDeleteGroup;
var group: TLevelGroup;
  levelFilename: String;
begin
  if not SelectedIsGroup then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  if QuestionDlg('', sDeleteTheGroupAndItsLevels, mtWarning,
               [mrOk, sDelete, mrCancel, sCancel], 0) = mrCancel then exit;
  // delete the group in bank
  LevelBank.DeleteGroupByName(group.GroupName);

  // remove entry in tree view
  TV.Items.Delete(TV.Selected);

  if LevelBank.Size > 0 then begin
    // generate the level unit file and add it to project
    LevelBank.ExportToFileGameLevel;
    Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);
  end else begin
    // there isn't group in the list -> we remove the level unit from project and disk
    levelFilename := Project.Config.TargetLazarusProject.GetFilenameGameLevels;
    Project.Config.TargetLazarusProject.Unit_RemoveFromProject(LEVEL_UNIT_NAME, ulLevels, uePas);
    if FileExistsUTF8(levelFilename) then
      DeleteFileUTF8(levelFilename);
  end;

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoRenameLevel;
var oldName, newName: string;
  level: PLevelBankItem;
  group: TLevelGroup;
begin
  if not SelectedIsLevel then exit;
  level := GetSelectedLevel;
  if level = NIL then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  oldName := level^.name;
  newName := Trim(InputBox('', sEnterTheNewName, oldName));
  if (newName = '') or (newName = oldName) then exit;
  if group.NameExists(newName) then begin
    ShowMessage(Format(sTheLevelxxAlreadyExists, [newName]));
    exit;
  end;

  // change name in LevelBank
  level^.name := newName;

  // change name in tree view
  TV.Selected.Text := newName;

  // generate the new unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoDuplicateLevel;
var k: integer;
  n: TTreeNode;
  oldName, newName: String;
  srcItem, newLevel: PLevelBankItem;
  group: TLevelGroup;
begin
  n := TV.Selected;
  if not NodeIsLevel(n) then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  // construct the new name (unique)
  oldName := n.Text;
  k := 0;
  repeat
    inc(k);
    if k < 100 then newName := oldName+'_'+Format('%.2d', [k])
      else newName := oldName+'_'+k.ToString;
  until not LevelBank.GroupNameExists(newName);

  // create item in target group
  srcItem := group.GetItemByName(oldName);
  if srcItem = NIL then exit;
  newLevel := group.AddEmpty;
  srcItem^.DuplicateTo(newLevel);
  newLevel^.name := newName;

  // add new item in the tree
  with TV.Items.AddChild(n.Parent, newName) do begin
    SelectedIndex := 1;
    ImageIndex := 1;
  end;

  // generate the level unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoChangeLevelDescription;
var group: TLevelGroup;
  newDescription: String;
  level: PLevelBankItem;
begin
  if not SelectedIsLevel then exit;
  level := GetSelectedLevel;
  if level = NIL then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  //newDescription := Trim(InputBox('', 'Enter the new description for the level:', level^.description));
  FormEnterDescription := TFormEnterDescription.Create(NIL);
  try
    FormEnterDescription.DescriptionAsText := level^.description;
    if FormEnterDescription.ShowModal <> mrOk then exit;
    newDescription := FormEnterDescription.DescriptionAsText;
  finally
    FormEnterDescription.Free;
  end;
  if newDescription = level^.description then exit;

  level^.description := ReplaceCharacterNotAllowedByUnderscore(newDescription);

  // generate the level unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoDeleteLevel;
var group: TLevelGroup;
begin
  if not SelectedIsLevel then exit;
  group := GetSelectedWorkingLevelGroup;
  if group = NIL then exit;

  if QuestionDlg('', sDeleteThisLevel, mtWarning,
                 [mrOk, sDelete, mrCancel, sCancel], 0) = mrCancel then exit;

  // delete entry in the LevelBank
  group.DeleteByName(TV.Selected.Text);

  // delete entry in the tree view
  TV.Items.Delete(TV.Selected);

  // generate the level unit file and add it to project
  LevelBank.ExportToFileGameLevel;
  Project.Config.TargetLazarusProject.Unit_AddToProject(LEVEL_UNIT_NAME, ulLevels, uePas);

  DoSaveLevelBank;
end;

procedure TFrameViewLevelBank.DoEditLevel;
begin
  if not SelectedIsLevel then exit;
  FormMain.EditLevelInLevelBank(GetSelectedText);
end;

procedure TFrameViewLevelBank.DoSaveLevelBank;
begin
  LevelBank.SaveToPath(Project.Config.TargetLazarusProject.GetFolderGameHelperFiles);
end;

procedure TFrameViewLevelBank.HideToolPanels;
begin
  PanelRoot.Visible := False;
  PanelGroup.Visible := False;
  PanelLevel.Visible := False;
end;

procedure TFrameViewLevelBank.ShowToolPanel;
var r: TRect;
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
  if not IsEditable then exit;

  if TV.Selected = NIL then exit;
  r := TV.Selected.DisplayRect(True);

  if SelectedIsRoot then MakePanelVisible(PanelRoot)
  else
  if SelectedIsGroup then MakePanelVisible(PanelGroup)
  else
  if SelectedIsLevel then MakePanelVisible(PanelLevel);
end;

procedure TFrameViewLevelBank.FillWithLevelBank;
var i, j: integer;
  rootnode, groupnode, levelnode: TTreeNode;
  levelGroup: TLevelGroup;
begin
  TV.BeginUpdate;
  TV.Items.Clear;
  TV.Items.AddFirst(NIL, sGroups);
  if LevelBank.Size > 0 then begin
    rootnode := TV.Items.GetFirstNode;
    for i:=0 to LevelBank.Size-1 do begin
      levelGroup := LevelBank.Mutable[i]^;
      groupnode := TV.Items.AddChild(rootnode, levelGroup.GroupName);
      groupnode.SelectedIndex := 0;
      groupnode.ImageIndex := 0;
      groupnode.MakeVisible;
      if levelGroup.Size > 0 then
        for j:=0 to levelGroup.Size-1 do begin
          levelnode := TV.Items.AddChild(groupnode, levelGroup.Mutable[j]^.name);
          levelnode.SelectedIndex := 1;
          levelnode.ImageIndex := 1;
          levelnode.MakeVisible;
        end;
    end;
  end;

  TV.EndUpdate;
end;

procedure TFrameViewLevelBank.Clear;
begin
  TV.Items.Clear;
end;

function TFrameViewLevelBank.ASelectionIsAvailable: boolean;
begin
  Result := NodeIsGroup(TV.Selected) or NodeIsLevel(TV.Selected);
end;

function TFrameViewLevelBank.SelectedIsRoot: boolean;
begin
  Result := NodeIsRoot(TV.Selected);
end;

function TFrameViewLevelBank.SelectedIsGroup: boolean;
begin
  Result := NodeIsGroup(TV.Selected);
end;

function TFrameViewLevelBank.SelectedIsLevel: boolean;
begin
 Result := NodeIsLevel(TV.Selected);
end;

function TFrameViewLevelBank.GetSelectedWorkingLevelGroup: TLevelGroup;
var n: TTreeNode;
begin
  n := TV.Selected;
  if n = NIL then exit(NIL);
  if NodeIsLevel(n) then n := n.Parent;
  Result := LevelBank.GetGroupByName(n.Text);
end;

function TFrameViewLevelBank.GetSelectedLevel: PLevelBankItem;
begin
  if NodeIsLevel(TV.Selected) then Result := WorkingLevelGroup.GetItemByName(TV.Selected.Text)
    else Result := NIL;
end;

function TFrameViewLevelBank.GetSelectedText: string;
begin
  if SelectedIsGroup or SelectedIsLevel then Result := TV.Selected.Text
    else Result := '';
end;

procedure TFrameViewLevelBank.DeleteSelected;
var n: TTreeNode;
begin
  n := TV.Selected;
  if n = NIL then exit;
  if NodeIsGroup(n) then DoDeleteGroup
    else DoDeleteLevel;
end;

end.

