unit u_export_to_string;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFormCopyToClipboard }

  TFormCopyToClipboard = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure ShowAsPascalCode;
    procedure ShowToShareOnForum;
  public

  end;

var
  FormCopyToClipboard: TFormCopyToClipboard;

implementation

uses Clipbrd, screen_demo, form_main;

{$R *.lfm}

{ TFormCopyToClipboard }

procedure TFormCopyToClipboard.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormCopyToClipboard.ShowAsPascalCode;
var packedParams, formated: string;
  i: integer;
  function GetNext80Characters: string;
  var c: integer;
  begin
    Result := '';
    c := 0;
    while (i <= Length(packedParams)) and (c < 80) do begin
     Result := Result + packedParams[i];
     inc(i);
     inc(c);
    end;
  end;

begin
  packedParams := ScreenDemo.Clouds.SaveParamsToString;

  // format the packed string for ease integration into a fpc/lazarus program
  i := 1;
  formated := 'const CLOUDS_'+UpperCase(FormMain.GetLastSelectedPresetNameWithoutSpace)+' ='+LineEnding;
  while i <= Length(packedParams) do begin
    formated := formated +'          '''+GetNext80Characters+'''';
    if i < Length(packedParams) then formated := formated + '+'+LineEnding
      else formated := formated + ';';
  end;

  // show the packed string
  Memo1.Clear;
  Memo1.Lines.Add(formated);

  // push a copy of the formatted text into clipboard
  Clipboard.AsText := formated;
end;

procedure TFormCopyToClipboard.ShowToShareOnForum;
var formated: string;
begin
  formated := '[code=pascal]'+
               FormMain.GetLastSelectedPresetNameWithSpace+LINEENDING+
               ScreenDemo.Clouds.SaveParamsToString+
               '[/code]';

  // show the packed string
  Memo1.Clear;
  Memo1.Lines.Add(formated);

  // push a copy of the formatted text into clipboard
  Clipboard.AsText := formated;
end;

procedure TFormCopyToClipboard.FormShow(Sender: TObject);
begin
  RadioButton1Change(NIL);
end;

procedure TFormCopyToClipboard.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.Checked then begin
    ShowAsPascalCode;
    Label1.Caption := '1) add the unit u_proceduralcloud.pas to your game/app,'+LineEnding+
           '2) somewhere paste this string as constant, rename it according to your need,'+LineEnding+
           '3) create a TProceduralPlanet instance in your scene and call TProceduralPlanet.LoadFromString(...).';
  end else begin
    ShowToShareOnForum;
    Label1.Caption := 'Go to the FPC/Lazarus forum, open the right thread and paste it into your message to share.'+Lineending+
         'Note: the string is already encapsulated between [code=pascal][/code] tags.';
  end;
end;


end.

