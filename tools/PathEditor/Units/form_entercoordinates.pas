unit form_entercoordinates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  Spin, ExtCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TFormEnterCoordinate }

  TFormEnterCoordinate = class(TForm)
    FloatSpinEdit1: TFloatSpinEdit;
    FloatSpinEdit2: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SEX: TSpinEdit;
    SEY: TSpinEdit;
    SpeedButton1: TSpeedButton;
    procedure RadioButton1Change(Sender: TObject);
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

procedure TFormEnterCoordinate.RadioButton1Change(Sender: TObject);
begin
  Panel1.Enabled := RadioButton1.Checked;
  Panel2.Enabled := RadioButton2.Checked;
end;

procedure TFormEnterCoordinate.SetScreenResolution(aWidth, aHeight: integer);
begin
  FScreenWidth := aWidth;
  FScreenHeight := aHeight;
end;

function TFormEnterCoordinate.GetResultPoint: TPointF;
begin
  if RadioButton1.Checked then begin
    Result.x := FloatSpinEdit1.Value;
    Result.y := FloatSpinEdit2.Value;
  end else begin
    Result.x := SEX.Value / FScreenWidth;
    Result.y := SEY.Value / FScreenHeight;
  end;
end;

end.

