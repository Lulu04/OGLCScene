unit Form_InsertLineColumn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin;

type

  { TFormInsertLineColumn }

  TFormInsertLineColumn = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    SE1: TSpinEdit;
    Shape1: TShape;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormInsertLineColumn: TFormInsertLineColumn;

implementation

{$R *.lfm}

end.

