unit form_importfromforum;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFormImportPreset }

  TFormImportPreset = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public

  end;

var
  FormImportPreset: TFormImportPreset;

implementation

uses u_ProceduralPlanet, form_main;

{$R *.lfm}

{ TFormImportPreset }

procedure TFormImportPreset.SpeedButton1Click(Sender: TObject);
var presetName, presetData: string;
   params: TPlanetParams;
   err: boolean;
   i: Integer;
begin
  if Memo1.Lines.Count = 0 then begin
    Showmessage('Empty!');
    exit;
  end;

  if Memo1.Lines.Count <> 2 then begin
    ShowMessage('a preset must have 2 lines!');
    exit;
  end;


  err := False;
  // try to decode the preset
  try
    presetName := Trim(Memo1.lines.Strings[0]);
    presetData := Trim(Memo1.lines.Strings[1]);
    if not params.CheckString(presetData) then begin
      ShowMessage('can not decode the preset: bad format!');
      err := True;
    end;

  except
    Showmessage('bad data: an exception occurs when decoding the preset!');
    err := true;
  end;

  if err then exit;

  try
    i := FormMain.PresetManager.AddPreset(presetName, presetData);
  except
    err := True;
  end;

  if not err then begin
    Showmessage('Preset "'+presetName+'" added with success');
    FormMain.PresetManager.SelectPreset(i);
    Close;
  end;
end;

procedure TFormImportPreset.FormShow(Sender: TObject);
begin
  Memo1.Clear;
end;

end.

