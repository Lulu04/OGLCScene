unit uasknewlayermapinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TForm_AskNewLayerMapInfo }

  TForm_AskNewLayerMapInfo = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form_AskNewLayerMapInfo: TForm_AskNewLayerMapInfo;

implementation

{$R *.lfm}

{ TForm_AskNewLayerMapInfo }

procedure TForm_AskNewLayerMapInfo.FormShow(Sender: TObject);
begin
  Edit1.Text:='';
  Edit1.SetFocus;
end;

end.

