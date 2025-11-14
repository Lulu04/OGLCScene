unit form_newproject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Buttons;

type

  { TFormNewProject }

  TFormNewProject = class(TForm)
    DE: TDirectoryEdit;
    Edit1: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    function CheckParams: boolean;
  public
    function GetLazarusProjectFolder: string;
    function GetLazarusProjectName: string;
  end;

var
  FormNewProject: TFormNewProject;

implementation

uses u_utils, u_project;

{$R *.lfm}

{ TFormNewProject }

procedure TFormNewProject.SpeedButton1Click(Sender: TObject);
begin
  if not CheckParams then exit;
  ModalResult := mrOk;
end;

function TFormNewProject.CheckParams: boolean;
begin
  Result := IsValidLazarusProjectName(Trim(Edit1.Text)) and
            (DE.Directory <> '');
end;

function TFormNewProject.GetLazarusProjectFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(DE.Directory);
end;

function TFormNewProject.GetLazarusProjectName: string;
begin
  Result := Trim(Edit1.Text);
end;

end.

