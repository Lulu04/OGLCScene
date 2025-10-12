unit u_presetmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  StdCtrls, ComCtrls;

const PRESET_SEPARATOR: char = '|';

type

  TPresetToWidget = procedure(const A: TStringArray) of Object;
  TWidgetToPreset = function: string of Object;

  { TPresetManager }

  TPresetManager = class(TForm)
    LB: TListBox;
    MenuItem1: TMenuItem;
    MIManager: TMenuItem;
    MIAdd: TMenuItem;
    PopupMenu1: TPopupMenu;
    BUpdate: TSpeedButton;
    BDelete: TSpeedButton;
    BRename: TSpeedButton;
    UpDown1: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIManagerClick(Sender: TObject);
    procedure BUpdateClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FList: TStringList;
    FPresetToWidget: TPresetToWidget;
    FWidgetToPreset: TWidgetToPreset;
    FFilename: string;
    procedure ProcessUserButtonClick(Sender: TObject);
    procedure ProcessPresetClic(Sender: TObject);
    procedure Clear;
    function ArrayToString(const A: TStringArray): string;
    procedure UpdateWidgets;
  public
    procedure Init1(const aTitle: string; aUserButton: TSpeedButton;
                    const aFilename: string);
    procedure Init2(aPresetToWidget: TPresetToWidget;
                    aWidgetToPreset: TWidgetToPreset);
    procedure Save;
    procedure Load;
  end;


implementation

uses LCLType, LCLHelper, u_common;

{$R *.lfm}

{ TPresetManager }

procedure TPresetManager.ProcessPresetClic(Sender: TObject);
var m: TMenuItem;
  A: TStringArray;
  i: integer;
begin
  m := Sender as TMenuItem;
  i := PopupMenu1.items.IndexOf(m);
  try
    A := FList.Strings[i-3].Split([PRESET_SEPARATOR]);
    Delete(A, 0, 1); // delete the name of the preset
    FPresetToWidget(A);
  except
    On E :Exception do begin
      FScene.LogError('Preset "'+m.Caption+'" from "'+ExtractFilename(FFilename)+'" raise exception "'+E.Message+'"');
    end;
  end;
end;

procedure TPresetManager.Clear;
begin
  LB.Clear;
  while PopupMenu1.Items.Count > 3 do
    PopupMenu1.Items.Delete(3);
  FList.Clear;
end;

function TPresetManager.ArrayToString(const A: TStringArray): string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(A) do begin
    if i <> 0 then Result := Result + PRESET_SEPARATOR;
    Result := Result + A[i];
  end;
end;

procedure TPresetManager.UpdateWidgets;
begin
  BUpdate.Enabled := LB.ItemIndex > -1;
  BDelete.Enabled := BUpdate.Enabled;
  BRename.Enabled := BUpdate.Enabled;
  UpDown1.Enabled := BUpdate.Enabled;
end;

procedure TPresetManager.MIManagerClick(Sender: TObject);
begin
  ShowModal;
end;

procedure TPresetManager.BUpdateClick(Sender: TObject);
var i: integer;
  A: TStringArray;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  if QuestionDlg('', 'Replace preset data ?', mtWarning, [mrOk, mrCancel], 0) = mrOk then begin
    A := FList.Strings[i].Split([PRESET_SEPARATOR]);
    FList.Strings[i] := A[0] + PRESET_SEPARATOR + FWidgetToPreset();
    Save;
  end;
end;

procedure TPresetManager.BDeleteClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  if QuestionDlg('','Delete this preset ?', mtWarning, [mrOk, mrCancel], 0) = mrOk then begin
    FList.Delete(i);
    PopupMenu1.Items.Delete(i+3);
    LB.Items.Delete(i);
    Save;
  end;
end;

procedure TPresetManager.BRenameClick(Sender: TObject);
var i: Integer;
  n: string;
  men: TMenuItem;
  A: TStringArray;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  n := LB.Items.Strings[i];
  if not InputQuery('', 'New name:', n) then exit;
  if Trim(n) = '' then begin
    ShowMessage('Bad name...');
    exit;
  end;
  men := PopupMenu1.Items[i+3];
  men.Caption := n;
  LB.Items.Strings[i] := n;
  A := FList.Strings[i].Split([PRESET_SEPARATOR]);
  A[0] := n;
  FList.Strings[i] := ArrayToString(A);
  Save;
end;

procedure TPresetManager.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;

  case Button of
    btNext: if i > 0 then begin
      FList.Exchange(i-1, i);
      LB.MoveSelectionUp;
      Save;
    end;

    btPrev: if i < LB.Count-1 then begin
      FList.Exchange(i+1, i);
      LB.MoveSelectionDown;
      Save;
    end;
  end;
end;

procedure TPresetManager.MIAddClick(Sender: TObject);
var n: string;
  men: TMenuItem;
begin
  n := '';
  if not InputQuery('New preset', 'Name for the new preset:', n) then exit;
  if Trim(n) = '' then begin
    ShowMessage('The preset is not registered: Bad name "'+n+'"');
    exit;
  end;
  FList.Add(n + PRESET_SEPARATOR + FWidgetToPreset());
  LB.Items.Add(n);
  men := TMenuItem.Create(Self);
  men.Caption := n;
  men.OnClick := @ProcessPresetClic;
  PopupMenu1.Items.Add(men);
  Save;
end;

procedure TPresetManager.FormCreate(Sender: TObject);
begin
  FList := TStringList.Create;
end;

procedure TPresetManager.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TPresetManager.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TPresetManager.FormShow(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TPresetManager.LBSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateWidgets;
end;

procedure TPresetManager.ProcessUserButtonClick(Sender: TObject);
begin
  PopupMenu1.PopUp;
end;

procedure TPresetManager.Init1(const aTitle: string; aUserButton: TSpeedButton;
  const aFilename: string);
begin
  Caption := aTitle;
  aUserButton.OnClick := @ProcessUserButtonClick;
  FFilename := aFilename;
  Load;
end;

procedure TPresetManager.Init2(aPresetToWidget: TPresetToWidget; aWidgetToPreset: TWidgetToPreset);
begin
  FPresetToWidget := aPresetToWidget;
  FWidgetToPreset := aWidgetToPreset;
end;

procedure TPresetManager.Save;
begin
  if FList.Count = 0 then exit;
  try
    FList.SaveToFile(FFilename);
  finally
  end;
end;

procedure TPresetManager.Load;
var i: integer;
  m: TMenuItem;
  A: TStringArray;
begin
 Clear;
 if not FileExists(FFilename) then exit;

 FList.LoadFromFile(FFilename);
 // sets the presets on the listbox and in the menu
 for i:=0 to FList.Count-1 do begin
   A := FList.Strings[i].Split([PRESET_SEPARATOR]);
   LB.Items.Add(A[0]);
   m := TMenuItem.Create(Self);
   m.Caption := A[0];
   m.OnClick := @ProcessPresetClic;
   PopupMenu1.Items.Add(m);
 end;
end;

end.

