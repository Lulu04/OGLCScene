unit uexportgroundtype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls;

type

  { TForm_ExportGroundType }

  TForm_ExportGroundType = class(TForm)
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
  Form_ExportGroundType: TForm_ExportGroundType;

implementation
uses uAskGroundType;

{$R *.lfm}

{ TForm_ExportGroundType }

procedure TForm_ExportGroundType.FormShow(Sender: TObject);
const PREFIX = '  GROUND_';
var i, j, LMax: integer;
    s: string;
begin
 // retrieve tyle type with max length
 LMax := 0;
 for i:=0 to Form_AskGroundType.LB.Count-1 do
   if LMax < Length( Form_AskGroundType.LB.Items.Strings[i] )
     then LMax := Length( Form_AskGroundType.LB.Items.Strings[i] );
 LMax += Length( PREFIX );

 SynEdit1.Clear;
 SynEdit1.Lines.Add('// Ground type');
 SynEdit1.Lines.Add('const');

 for i:=0 to Form_AskGroundType.LB.Count-1 do
  begin
    s := PREFIX + UpperCase( Form_AskGroundType.LB.Items.Strings[i] );
    for j:=0 to LMax - Length( s ) do s += ' ';
    s += '= ' + inttostr( i ) + ';';
    SynEdit1.Lines.Add( s );
  end;

 SynEdit1.SelectAll;
 SynEdit1.CopyToClipboard;
end;

end.

