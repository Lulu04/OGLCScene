unit u_presetmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  StdCtrls, ComCtrls, Types;

resourcestring
sAPresetWithTheNamexxxAlreadyExists='A preset with the name %s already exists. Please retry with another name.';
sNewPreset='New preset';
sReplacePresetData='Replace preset data ?';
sDeleteThisPreset='Delete this preset ?';
sNewName='New name:';
sBadName='Bad name...';
sNameForTheNewpreset='Name for the new preset:';

const PRESET_SEPARATOR: char = '|';

type

  TPresetToWidget = procedure(const data: string) of Object;
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
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure MIAddClick(Sender: TObject);
    procedure MIManagerClick(Sender: TObject);
    procedure BUpdateClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BRenameClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FPresetToWidget: TPresetToWidget;
    FWidgetToPreset: TWidgetToPreset;
    FFilename: string;
    procedure ProcessUserButtonClick(Sender: TObject);
    procedure ProcessMenuClick(Sender: TObject);
    procedure Clear;
    function GetPresetName(aIndex: integer): string;
    function GetPresetData(aIndex: integer): string;
    procedure SetPresetName(aIndex: integer; const aName: string);
    procedure SetPresetData(aIndex: integer; const aData: string);
    function PresetNameAlreadyExists(const aName: string): boolean;
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

uses LCLType, LCLHelper, u_common, LazFileUtils;

{$R *.lfm}

{ TPresetManager }

procedure TPresetManager.ProcessMenuClick(Sender: TObject);
var m: TMenuItem;
  A: TStringArray;
  i: integer;
begin
  m := Sender as TMenuItem;
  i := PopupMenu1.items.IndexOf(m);
  try
    A := LB.Items.Strings[i-3].Split([PRESET_SEPARATOR]);
    if Length(A) = 2 then
      FPresetToWidget(A[1]);
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
end;

function TPresetManager.GetPresetName(aIndex: integer): string;
var A: TStringArray;
begin
 A := LB.Items.Strings[aIndex].Split([PRESET_SEPARATOR]);
 if Length(A) = 2 then Result := A[0]
   else Result := '?????';
end;

function TPresetManager.GetPresetData(aIndex: integer): string;
var A: TStringArray;
begin
 A := LB.Items.Strings[aIndex].Split([PRESET_SEPARATOR]);
 if Length(A) = 2 then Result := A[1]
   else Result := '';
end;

procedure TPresetManager.SetPresetName(aIndex: integer; const aName: string);
var A: TStringArray;
begin
  A := LB.Items.Strings[aIndex].Split([PRESET_SEPARATOR]);
  if Length(A) = 2 then
    LB.Items.Strings[aIndex] := aName + PRESET_SEPARATOR + A[1];
end;

procedure TPresetManager.SetPresetData(aIndex: integer; const aData: string);
var A: TStringArray;
begin
  A := LB.Items.Strings[aIndex].Split([PRESET_SEPARATOR]);
  if Length(A) = 2 then
    LB.Items.Strings[aIndex] := A[0] + PRESET_SEPARATOR + aData;
end;

function TPresetManager.PresetNameAlreadyExists(const aName: string): boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to LB.Count-1 do
    if GetPresetName(i) = aName then exit(True);
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
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  if QuestionDlg('', sReplacePresetData, mtWarning, [mrOk, mrCancel], 0) = mrOk then begin
    SetPresetData(i, FWidgetToPreset());
    Save;
  end;
end;

procedure TPresetManager.BDeleteClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;

  if QuestionDlg('', sDeleteThisPreset, mtWarning, [mrOk, mrCancel], 0) = mrOk then begin
    PopupMenu1.Items.Delete(i+3);
    LB.Items.Delete(i);
    Save;
  end;
end;

procedure TPresetManager.BRenameClick(Sender: TObject);
var i: Integer;
  nam: string;
  men: TMenuItem;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  nam := LB.Items.Strings[i];

  if not InputQuery('', sNewName, nam) then exit;

  if Trim(nam) = '' then begin
    ShowMessage(sBadName);
    exit;
  end;

  // check if the name already exists
  nam := nam.Replace(' ', '_', [rfReplaceAll]);
  if PresetNameAlreadyExists(nam) then begin
    ShowMessage(Format(sAPresetWithTheNamexxxAlreadyExists, [nam]));
    exit;
  end;

  // replace name in menu
  men := PopupMenu1.Items[i+3];
  men.Caption := nam;
  // replace name in listbox
  SetPresetName(i, nam);
  Save;
end;

procedure TPresetManager.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var i: integer;
  temp: string;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;

  case Button of
    btNext: if i > 0 then begin
      temp := PopupMenu1.Items.Items[i+3].Caption;
      PopupMenu1.Items.Items[i+3].Caption := PopupMenu1.Items.Items[i+3-1].Caption;
      PopupMenu1.Items.Items[i+3-1].Caption := temp;

      LB.MoveSelectionUp;
      Save;
    end;

    btPrev: if i < LB.Count-1 then begin
      temp := PopupMenu1.Items.Items[i+3].Caption;
      PopupMenu1.Items.Items[i+3].Caption := PopupMenu1.Items.Items[i+3+1].Caption;
      PopupMenu1.Items.Items[i+3+1].Caption := temp;

      LB.MoveSelectionDown;
      Save;
    end;
  end;
end;

procedure TPresetManager.MIAddClick(Sender: TObject);
var nam: string;
  men: TMenuItem;
begin
  nam := '';
  if not InputQuery(sNewPreset, sNameForTheNewpreset, nam) then exit;

  if Trim(nam) = '' then begin
    ShowMessage(sBadName);
    exit;
  end;

  nam := nam.Replace(' ', '_', [rfReplaceAll]);
  if PresetNameAlreadyExists(nam) then begin
    ShowMessage(Format(sAPresetWithTheNamexxxAlreadyExists, [nam]));
    exit;
  end;

  LB.Items.Add(nam + PRESET_SEPARATOR + FWidgetToPreset());
  men := TMenuItem.Create(Self);
  men.Caption := nam;
  men.OnClick := @ProcessMenuClick;
  PopupMenu1.Items.Add(men);
  Save;
end;

procedure TPresetManager.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TPresetManager.FormShow(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TPresetManager.LBDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  Control := Control;
  with LB.Canvas do begin
    Brush.Style := bsSolid;
    if State >= [odSelected] then begin
      Brush.Color := RGBToColor(94,128,130);
      Font.Color := clWhite;
    end else begin
      Brush.Color := LB.Color;
      Font.Color := clBlack;
    end;
    FillRect(ARect);
    Brush.Style := bsClear;

    TextOut(ARect.Left, ARect.Top, GetPresetName(Index));
  end;
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
  if not DirectoryExistsUTF8(ExtractFilePath(FFilename)) then exit;

  try
    LB.Items.SaveToFile(FFilename);
  except
    On E :Exception do begin
      FScene.LogError('Presets '+Caption+': exception while saving to file "'+FFilename);
      FScene.LogError(E.Message, 1);
    end;
  end;
end;

procedure TPresetManager.Load;
var i: integer;
  m: TMenuItem;
begin
  Clear;
  if not FileExists(FFilename) then exit;

  try
    LB.Items.LoadFromFile(FFilename);
    // add the presets in the menu
    for i:=0 to LB.Count-1 do begin
      m := TMenuItem.Create(Self);
      m.Caption := GetPresetName(i);
      m.OnClick := @ProcessMenuClick;
      PopupMenu1.Items.Add(m);
    end;
  except
    On E :Exception do begin
      FScene.LogError('Presets '+Caption+': exception while loading from file "'+FFilename);
      FScene.LogError(E.Message, 1);
    end;
  end;
end;

end.

