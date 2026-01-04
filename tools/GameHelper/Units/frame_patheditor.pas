unit frame_patheditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Arrow, Buttons,
  u_ui_objectlist, BGRABitmap, BGRABitmapTypes;

type

  { TFramePathEditor }

  TFramePathEditor = class(TFrame)
    Arrow1: TArrow;
    Arrow2: TArrow;
    Arrow3: TArrow;
    Arrow4: TArrow;
    BCancel: TSpeedButton;
    BHelp: TSpeedButton;
    BSave: TSpeedButton;
    CBUseSpline: TCheckBox;
    CBShowLevel: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel8: TPanel;
    BClearAllNodes: TSpeedButton;
    procedure BCancelClick(Sender: TObject);
    procedure BClearAllNodesClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure CBUseSplineChange(Sender: TObject);
  private
    FInitializing,
    FModified: boolean;
  public
    procedure OnShow;

    procedure EditPathFromBank(aItem: TPathDescriptorItem);
    procedure EditNewPath;

    property Modified: boolean read FModified write FModified;
  end;

implementation

uses form_showhelp, u_screen_patheditor, u_project, u_utils, form_main,
  u_resourcestring, u_levelbank, Dialogs;

{$R *.lfm}

{ TFramePathEditor }

procedure TFramePathEditor.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('PATH EDITOR'#10#10+
     'Here you can define path that some surface can follow.');
end;

procedure TFramePathEditor.BClearAllNodesClick(Sender: TObject);
begin
  if QuestionDlg('','Delete all nodes?', mtWarning,
                 [mrOk, sDelete, mrCancel, sCancel], 0) = mrCancel then exit;
  ScreenPathEditor.DeleteAllNodes;
end;

procedure TFramePathEditor.BCancelClick(Sender: TObject);
begin
  if FModified then
    if QuestionDlg('','If you leave, changes will be lost.'+LineEnding+'Continue ?', mtWarning,
                   [mrOk, 'Leave', mrCancel, 'Cancel'], 0) = mrCancel then exit;
  FormMain.ShowPagePathBank;
end;

procedure TFramePathEditor.BSaveClick(Sender: TObject);
var nam: string;
  item: TPathDescriptorItem;
begin
  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if not IsValidPascalVariableName(nam, True) then exit;
  if not PathBank.AskUserToReplaceExistingItem(nam) then exit;

  // retrieve the existing item or create one
  item := PathBank.GetByName(nam);
  if item = NIL then item := PathBank.AddEmpty;

  item._Name := nam;
  item.UseSpline := CBUseSpline.Checked;
  item.SplineStyle := TSplineStyle(ComboBox1.ItemIndex);
  item.PathData := ScreenPathEditor.GetPathData;

  PathBank.Save;
  Modified := False;

  FormMain.ShowPagePathBank;
end;

procedure TFramePathEditor.CBUseSplineChange(Sender: TObject);
begin
  ComboBox1.Enabled := CBUseSpline.Checked;
  Label2.Enabled := CBShowLevel.Checked;
  ComboBox2.Enabled := CBShowLevel.Checked;
  Label3.Enabled := CBShowLevel.Checked;
  ComboBox3.Enabled := CBShowLevel.Checked;

  if FInitializing then exit;

  if (Sender = CBUseSpline) or
     (Sender = ComboBox1) or
     (Sender = CheckBox4) then
    ScreenPathEditor.UpdateCurves;

  if Sender = ComboBox2 then
    LevelBank.Mutable[ComboBox2.ItemIndex]^.FillComboBoxWithLevels(ComboBox3);

  if (Sender = ComboBox3) and CBShowLevel.Checked then
    ScreenPathEditor.ShowLevel(ComboBox2.ItemIndex, ComboBox3.ItemIndex);

  if Sender = CBShowLevel then begin
    if CBShowLevel.Checked then ScreenPathEditor.ShowLevel(ComboBox2.ItemIndex, ComboBox3.ItemIndex)
      else ScreenPathEditor.HideLevel;
  end;

end;

procedure TFramePathEditor.OnShow;
begin
  LevelBank.FillComboBoxWithGroups(ComboBox2);
end;

procedure TFramePathEditor.EditPathFromBank(aItem: TPathDescriptorItem);
begin
  FInitializing := True;
  CBUseSpline.Checked := aItem.UseSpline;
  ComboBox1.ItemIndex := Ord(aItem.SplineStyle);
  CheckBox4.Checked := True;
  Edit1.Text := aItem._Name;
  FInitializing := False;
  FModified := False;
  OnShow;
end;

procedure TFramePathEditor.EditNewPath;
begin
  FInitializing := True;
  CBUseSpline.Checked := True;
  ComboBox1.ItemIndex := Ord(ssCrossingWithEnds);
  CheckBox4.Checked := True;
  Edit1.Text := '';
  FInitializing := False;
  FModified := False;
  OnShow;
end;

end.

