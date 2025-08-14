unit form_entercoordinates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  Spin, BGRABitmap, BGRABitmapTypes;

type

  { TFormEnterCoordinate }

  TFormEnterCoordinate = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    SEX: TSpinEdit;
    SEY: TSpinEdit;
    procedure SpeedButton1Click(Sender: TObject);
  private
    FScreenWidth, FScreenHeight: Integer;
  public
    procedure SetScreenResolution(aWidth, aHeight: integer);
    function GetResultPoint: TPointF;
  end;

var
  FormEnterCoordinate: TFormEnterCoordinate;

implementation

{$R *.lfm}

{ TFormEnterCoordinate }

procedure TFormEnterCoordinate.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormEnterCoordinate.SetScreenResolution(aWidth, aHeight: integer);
begin
  FScreenWidth := aWidth;
  FScreenHeight := aHeight;
end;

function TFormEnterCoordinate.GetResultPoint: TPointF;
begin
  Result.x := SEX.Value / FScreenWidth;
  Result.y := SEY.Value / FScreenHeight;
end;

end.

