unit form_showhelp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormShowHelp }

  TFormShowHelp = class(TForm)
    Memo1: TMemo;
  private

  public

  end;


procedure ShowHelp(const txt: string);

implementation

uses form_main;

procedure ShowHelp(const txt: string);
begin
  with TFormShowHelp.Create(FormMain) do begin
    Memo1.Clear;
    Memo1.Lines.AddText(txt);
    Show;
  end;
end;

{$R *.lfm}

end.

