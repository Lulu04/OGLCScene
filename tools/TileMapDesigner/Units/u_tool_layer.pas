unit u_tool_layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TForm_ToolLayer }

  TForm_ToolLayer = class(TForm)
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form_ToolLayer: TForm_ToolLayer;

implementation
uses LCLType;

{$R *.lfm}

{ TForm_ToolLayer }

procedure TForm_ToolLayer.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TForm_ToolLayer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_L then Hide;
end;

procedure TForm_ToolLayer.FormShow(Sender: TObject);
begin
end;

end.

