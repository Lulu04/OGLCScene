unit form_enter_description;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFormEnterDescription }

  TFormEnterDescription = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    procedure BCancelClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Memo1Change(Sender: TObject);
  private
    function GetDescriptionAsText: string;
    procedure SetDescriptionAsText(AValue: string);

  public
    property  DescriptionAsText: string read GetDescriptionAsText write SetDescriptionAsText;
  end;

var
  FormEnterDescription: TFormEnterDescription;

implementation
uses LCLType, u_utils;

{$R *.lfm}

{ TFormEnterDescription }

procedure TFormEnterDescription.BOkClick(Sender: TObject);
begin
  if not CheckCharacterAllowedInKeyboardInput(Memo1.Lines.Text) then exit;
  ModalResult := mrOk;
end;

procedure TFormEnterDescription.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then ModalResult := mrCancel;
end;

procedure TFormEnterDescription.Memo1Change(Sender: TObject);
begin
  Label2.Visible := not CheckCharacterAllowedInKeyboardInput(Memo1.Lines.Text);
end;

procedure TFormEnterDescription.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFormEnterDescription.GetDescriptionAsText: string;
begin
  Result := Memo1.Lines.Text;
end;

procedure TFormEnterDescription.SetDescriptionAsText(AValue: string);
begin
  Memo1.Clear;
  Memo1.Lines.AddText(AValue);
end;

end.

