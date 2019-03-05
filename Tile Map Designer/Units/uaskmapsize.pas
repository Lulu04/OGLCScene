unit uAskMapSize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Buttons,
  OGLCScene;

type

  { TForm_AskMapSize }

  TForm_AskMapSize = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SE2: TSpinEdit;
    Shape1: TShape;
    SE1: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form_AskMapSize: TForm_AskMapSize;

implementation
uses umaps;

{$R *.lfm}

{ TForm_AskMapSize }

procedure TForm_AskMapSize.FormShow(Sender: TObject);
var s: string;
begin
 s := inttostr( MapList.MainMap.TileEngine.MapTileCount.cx ) + ' column';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s += 's';
 Label4.Caption := s;

 s := inttostr( MapList.MainMap.TileEngine.MapTileCount.cy ) + ' row';
 if MapList.MainMap.TileEngine.MapTileCount.cx > 1 then s += 's';
 Label3.Caption := s;

 SE1.Value := MapList.MainMap.TileEngine.MapTileCount.cx;
 SE2.Value := MapList.MainMap.TileEngine.MapTileCount.cy;
end;

procedure TForm_AskMapSize.BitBtn1Click(Sender: TObject);
begin
 MapList.SetSize( SE2.Value, SE1.Value );
end;

end.

