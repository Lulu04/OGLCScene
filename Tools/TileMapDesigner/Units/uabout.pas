unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm_About }

  TForm_About = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form_About: TForm_About;

implementation

{$R *.lfm}

{ TForm_About }

procedure TForm_About.Button1Click(Sender: TObject);
begin
 Close;
end;

end.

