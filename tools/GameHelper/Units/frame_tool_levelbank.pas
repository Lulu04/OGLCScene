unit frame_tool_levelbank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls, Dialogs,
  u_levelbank;

type

  { TFrameToolLevelBank }

  TFrameToolLevelBank = class(TFrame)
    BDelete: TSpeedButton;
    BDuplicate: TSpeedButton;
    BEdit: TSpeedButton;
    BExportToPascalUnit: TSpeedButton;
    BRedo: TSpeedButton;
    BRename: TSpeedButton;
    BUndo: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LB: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel8: TPanel;
    SD1: TSaveDialog;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure BExportToPascalUnitClick(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
  private
    FUndoRedoManager: TLevelBankUndoRedoManager;
    procedure FillLB;
    procedure ShowLevel(aIndex: integer);
    procedure UpdateWidgetState;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;
  end;

implementation

uses u_screen_levelbank, u_project, form_main;

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
    newName := InputBox('', 'Enter the new name:', oldName);
    if newName = oldName then exit;
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

procedure TFrameToolLevelBank.BExportToPascalUnitClick(Sender: TObject);
var t: TStringlist;
  s: string;
begin
  if LB.ItemIndex = -1 then exit;
  s := Trim(Edit1.Text);
  if Length(s) < 2 then exit;
  if s[1] <> 'T' then s := 'T'+s;
  SD1.FileName := Copy(s, 2, Length(Edit1.Text));
  if not SD1.Execute then exit;

  t := TStringList.Create;

  try
    t.SaveToFile(SD1.FileName);
    ShowMessage('Pascal unit created');
  finally
    t.Free;
  end;
end;

procedure TFrameToolLevelBank.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LB.ItemIndex := LB.GetIndexAtXY(X, Y);
end;

procedure TFrameToolLevelBank.LBSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  UpdateWidgetState;
  ShowLevel(LB.ItemIndex);

  i := LB.ItemIndex;
  if i = -1 then Edit1.Text := ''
    else Edit1.Text := 'T'+LevelBank.Mutable[i]^.name;
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
  FillLB;
  UpdateWidgetState;
end;

end.

