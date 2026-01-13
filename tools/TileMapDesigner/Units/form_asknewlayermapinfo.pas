unit form_asknewlayermapinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TFormAskNewLayerMapInfo }

  TFormAskNewLayerMapInfo = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormAskNewLayerMapInfo: TFormAskNewLayerMapInfo;

implementation

{$R *.lfm}

{ TFormAskNewLayerMapInfo }

procedure TFormAskNewLayerMapInfo.FormShow(Sender: TObject);
begin
  Edit1.Text:='';
  Edit1.SetFocus;
end;

end.

