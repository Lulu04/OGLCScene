unit form_exportgroundtype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls;

type

  { TFormExportGroundType }

  TFormExportGroundType = class(TForm)
    BitBtn1: TBitBtn;
    StaticText1: TStaticText;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormExportGroundType: TFormExportGroundType;

implementation
uses form_askgroundtype;

{$R *.lfm}

{ TFormExportGroundType }

procedure TFormExportGroundType.FormShow(Sender: TObject);
const PREFIX = '  GROUND_';
var i, j, LMax: integer;
    s: string;
begin
 // retrieve tyle type with max length
 LMax := 0;
 for i:=0 to FormAskGroundType.LB.Count-1 do
   if LMax < Length( FormAskGroundType.LB.Items.Strings[i] )
     then LMax := Length( FormAskGroundType.LB.Items.Strings[i] );
 LMax += Length( PREFIX );

 SynEdit1.Clear;
 SynEdit1.Lines.Add('// Ground type');
 SynEdit1.Lines.Add('const');

 for i:=0 to FormAskGroundType.LB.Count-1 do
  begin
    s := PREFIX + UpperCase( FormAskGroundType.LB.Items.Strings[i] );
    for j:=0 to LMax - Length( s ) do s += ' ';
    s += '= ' + inttostr( i ) + ';';
    SynEdit1.Lines.Add( s );
  end;

 SynEdit1.SelectAll;
 SynEdit1.CopyToClipboard;
end;

end.

