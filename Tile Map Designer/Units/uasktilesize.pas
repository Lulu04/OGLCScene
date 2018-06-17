unit uAskTileSize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm_AskTileSize }

  TForm_AskTileSize = class(TForm)
    Button1: TButton;
    E1: TEdit;
    E2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form_AskTileSize: TForm_AskTileSize;

implementation

{$R *.lfm}

{ TForm_AskTileSize }

procedure TForm_AskTileSize.Button1Click(Sender: TObject);
begin
 if ( E1.Text='' ) or ( E2.Text='' ) then exit;
 Close;
end;

end.

