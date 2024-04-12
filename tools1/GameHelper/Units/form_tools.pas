unit form_tools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Spin;

type

  { TFormTools }

  TFormTools = class(TForm)
    BAddScreen1: TSpeedButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    LBScreen: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PC1: TPageControl;
    PageAtlas: TTabSheet;
    Page: TTabSheet;
    PageScreen: TTabSheet;
    BDeleteScreen: TSpeedButton;
    BAddScreen: TSpeedButton;
    BRenameScreen: TSpeedButton;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    TreeView1: TTreeView;
    procedure BAddScreenClick(Sender: TObject);
    procedure BDeleteScreenClick(Sender: TObject);
    procedure BRenameScreenClick(Sender: TObject);
    procedure LBScreenSelectionChange(Sender: TObject; User: boolean);
  private
    procedure SaveScreenList;
  public
    procedure LoadScreenList;

  end;

var
  FormTools: TFormTools;

implementation

uses form_main, u_project, OGLCScene;

{$R *.lfm}

{ TFormTools }

procedure TFormTools.BAddScreenClick(Sender: TObject);
var s: string;
begin
  s := InputBox('new screen', 'Enter the name of the screen', '');
  s := Trim(s);
  if s = '' then exit;
  LBScreen.ItemIndex := LBScreen.Items.Add(s);
  SaveScreenList;
end;

procedure TFormTools.BDeleteScreenClick(Sender: TObject);
begin
  if LBScreen.ItemIndex = -1 then exit;
  LBScreen.DeleteSelected;
  SaveScreenList;
end;

procedure TFormTools.BRenameScreenClick(Sender: TObject);
var s: string;
begin
  if LBScreen.ItemIndex = -1 then exit;
  s := InputBox('new screen', 'Enter the name of the screen', LBScreen.GetSelectedText);
  s := Trim(s);
  if s = '' then exit;
  LBScreen.Items.Strings[LBScreen.ItemIndex] := s;
  FormMain.UpdateWidgets;
  SaveScreenList;
end;

procedure TFormTools.LBScreenSelectionChange(Sender: TObject; User: boolean);
begin
  FormMain.UpdateWidgets;
end;

procedure TFormTools.SaveScreenList;
var s: string;
  i: integer;
begin
  s := '';
  for i:=0 to LBScreen.Count-1 do begin
    s := s + LBScreen.Items.Strings[i];
    if i < LBScreen.Count-1 then s := s + '-';
  end;
  Project.ScreenList := s;
end;

procedure TFormTools.LoadScreenList;
var A: TStringArray;
  i: integer;
begin
  LBScreen.Clear;
  if Project.ScreenList = '' then exit;
  A := Project.ScreenList.Split(['-']);
  LBScreen.Items.AddStrings(A);
end;

end.

