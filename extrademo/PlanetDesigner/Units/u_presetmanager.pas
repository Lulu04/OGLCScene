unit u_presetmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  StdCtrls, ComCtrls;

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
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    function GetPresetCount: integer;
    procedure ProcessUserButtonClick(Sender: TObject);
    procedure ProcessPresetClic(Sender: TObject);
    procedure Clear;
    function MergeArray(const A: TStringArray): string;
    procedure UpdateWidgets;
  public
    procedure Init1(const aTitle: string; aUserButton: TSpeedButton;
                    const aFilename: string);
    procedure Init2(aPresetToWidget: TPresetToWidget;
                    aWidgetToPreset: TWidgetToPreset);
    procedure Save;
    procedure Load;

    function AddPreset(const aName, aData: string): integer;
    procedure GetPresetByIndex(aPresetIndex: integer; out aName, aData: string);
    procedure SelectPreset(aPresetindex: integer);

    property PresetCount: integer read GetPresetCount;
  end;


implementation

uses u_common, LCLType, form_main;

{$R *.lfm}

const PRESET_SEPARATOR = '~';

{ TPresetManager }

procedure TPresetManager.ProcessPresetClic(Sender: TObject);
var m: TMenuItem;
  i: integer;
begin
  m := Sender as TMenuItem;
  i := PopupMenu1.items.IndexOf(m);
  SelectPreset(i-3);
end;

procedure TPresetManager.Clear;
begin
  LB.Clear;
  while PopupMenu1.Items.Count > 3 do
    PopupMenu1.Items.Delete(3);
  FList.Clear;
end;

function TPresetManager.MergeArray(const A: TStringArray): string;
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
  if QuestionDlg('Replace preset data', 'Replace preset data ?', mtConfirmation,
                 [mrYes, 'Yes', mrCancel, 'Cancel', 'IsDefault'], '') = mrYes then begin
    A := FList.Strings[i].Split([PRESET_SEPARATOR]);
    FList.Strings[i] := A[0] + PRESET_SEPARATOR+FWidgetToPreset();
    Save;
    Close;
  end;
end;

procedure TPresetManager.BDeleteClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  if QuestionDlg('', 'Delete this preset ?', mtConfirmation,
                 [mrYes, 'Yes', mrCancel, 'Cancel', 'IsDefault'], '') = mrYes then begin
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
  if InputQuery('Rename', 'Enter the preset new name', n) then begin
    men := PopupMenu1.Items[i+3];
    men.Caption := n;
    LB.Items.Strings[i] := n;
    A := FList.Strings[i].Split([PRESET_SEPARATOR]);
    A[0] := n;
    FList.Strings[i] := MergeArray(A);
    Save;
  end;
end;

procedure TPresetManager.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var temp: string;
  i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;

  case Button of
    btNext: if i > 0 then begin
      FList.Exchange(i-1, i);
      temp := LB.GetSelectedText;
      LB.Items.Strings[LB.ItemIndex] := LB.Items.Strings[LB.ItemIndex-1];
      LB.Items.Strings[LB.ItemIndex-1] := temp;
      LB.ItemIndex := LB.ItemIndex-1;
      Save;
    end;

    btPrev: if i < LB.Count-1 then begin
      FList.Exchange(i+1, i);
      temp := LB.GetSelectedText;
      LB.Items.Strings[LB.ItemIndex] := LB.Items.Strings[LB.ItemIndex+1];
      LB.Items.Strings[LB.ItemIndex+1] := temp;
      LB.ItemIndex := LB.ItemIndex+1;
      Save;
    end;
  end;
end;

procedure TPresetManager.MIAddClick(Sender: TObject);
var n: string;
begin
  n := '';
  if not InputQuery('Add', 'Name for the new preset:', n) then exit;
  AddPreset(n, FWidgetToPreset());
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

function TPresetManager.GetPresetCount: integer;
begin
  Result := FList.Count;
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

function TPresetManager.AddPreset(const aName, aData: string): integer;
var men: TMenuItem;
begin
  Result := FList.Add(aName + PRESET_SEPARATOR + aData);
  LB.Items.Add(aName);
  men := TMenuItem.Create(Self);
  men.Caption := aName;
  men.OnClick := @ProcessPresetClic;
  PopupMenu1.Items.Add(men);
  Save;
end;

procedure TPresetManager.GetPresetByIndex(aPresetIndex: integer; out aName, aData: string);
var
  A: TStringArray;
begin
  aName := '';
  aData := '';
  if FList.Count = 0 then exit;
  if (aPresetIndex < 0) or (aPresetIndex >= FList.Count) then exit;

  A := FList.Strings[aPresetIndex].Split([PRESET_SEPARATOR]);
  if Length(A) = 2 then begin
    aName := A[0];
    aData := A[1];
  end;
end;

procedure TPresetManager.SelectPreset(aPresetindex: integer);
var A: TStringArray;
begin
 try
   A := FList.Strings[aPresetindex].Split([PRESET_SEPARATOR]);
   FormMain.SetPresetNameOnTileForm(A[0]);
   Delete(A, 0, 1); // delete the name of the preset
   FPresetToWidget(A); // call the user decode function
 except
   On E :Exception do begin
     FScene.LogError('Preset['+aPresetindex.ToString+'] from "'+ExtractFilename(FFilename)+'" raise exception "'+E.Message+'"');
   end;
 end;
end;

end.

