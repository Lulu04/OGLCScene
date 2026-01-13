unit form_asktilesize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TFormAskTileSize }

  TFormAskTileSize = class(TForm)
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
  FormAskTileSize: TFormAskTileSize;

implementation

{$R *.lfm}

{ TFormAskTileSize }

procedure TFormAskTileSize.Button1Click(Sender: TObject);
begin
 if ( E1.Text='' ) or ( E2.Text='' ) then exit;
 Close;
end;

end.

