unit u_tool_minimap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  LCLType, LCLIntf,
  ExtCtrls, Menus, ComCtrls,
  BGRABitmap, BGRABitmapTypes,
  common,
  umaps;

type

  { TForm_Minimap }

  TForm_Minimap = class(TForm)
    PB2: TPaintBox;
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PB2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PB2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PB2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PB2Paint(Sender: TObject);
  private
    FMiniMapTarget: TPoint;
    procedure PB2SetSizeAndPos;
    procedure SetMapsCoordinates( aX, aY: integer );
  public

  end;

var
  Form_Minimap: TForm_Minimap;

implementation
uses form_main, Math, OGLCScene;

{$R *.lfm}

{ TForm_Minimap }

procedure TForm_Minimap.PB2Paint(Sender: TObject);
const TARGET_RADIUS=5;
var ima: TBGRABitmap;
begin
  ima := TBGRABitmap.Create(PB2.Width, PB2.Height, BGRA(0,0,0));
  ima.Rectangle(0, 0, ima.Width, ima.Height, BGRA(180,180,180), dmSet);

  with FMiniMapTarget do
   ima.FillEllipseLinearColorAntialias(x, y, TARGET_RADIUS, TARGET_RADIUS, BGRA(200,180,50), BGRA(200,180,80));

  ima.Draw(PB2.Canvas, 0, 0);
  ima.Free;
end;

procedure TForm_Minimap.PB2SetSizeAndPos;
var factor: single;
  ll, tt, ww, hh: integer;
begin
  if MapList.Count = 0 then exit;
  if MapList.MainMap.TileEngine.TileSize.cx = 0 then exit;
  if MapList.MainMap.TileEngine.MapSize.cy = 0 then exit;
  if MapList.MainMap.TileEngine.MapSize.cx = 0 then exit;

  factor := MapList.MainMap.TileEngine.MapSize.cx / MapList.MainMap.TileEngine.MapSize.cy;
  if MapList.MainMap.TileEngine.MapSize.cx > MapList.MainMap.TileEngine.MapSize.cy then
  begin // map width > map height
    ll := 0;
    ww := ClientWidth;
    hh := round(ww/factor);
    tt := round((ClientHeight-hh )/2);
  end else begin
    tt := 0;
    hh := ClientHeight;
    ww := round(hh*factor);
    ll := round((ClientWidth-ww)/2);
  end;
  PB2.SetBounds(ll, tt, ww, hh);
  PB2.Invalidate;
end;

procedure TForm_Minimap.SetMapsCoordinates(aX, aY: integer);
var xx, yy, delta: integer;
  te: TTileEngine;
begin
  if aX > PB2.Width then aX := PB2.Width;
  if aX < 0 then aX := 0;
  if aY > PB2.Height then aY := PB2.Height;
  if aY < 0 then aY := 0;

  te := MapList.MainMap.TileEngine;

  delta := (FScene.Width div te.TileSize.cx) * te.TileSize.cx;
  xx := te.MapSize.cx * aX div PB2.Width;
  xx := (xx div te.TileSize.cx) * te.TileSize.cx;
  xx := xx - delta;

  delta := (FScene.Height div te.TileSize.cy) * te.TileSize.cy;
  yy := te.MapSize.cy * aY div PB2.Height;
  yy := (yy div te.TileSize.cy) * te.TileSize.cy;
  yy := yy - delta;

  // in the case of the whole map fit on the scene, we center it
  if MapList.MainMap.TileEngine.MapSize.cx <= FScene.Width
    then xx := 0;
  if MapList.MainMap.TileEngine.MapSize.cy <= FScene.Height
   then yy := 0;

  MapList.MoveTileEngineTo(xx, yy);
  FMiniMapTarget := Point(aX, aY);
  PB2.Invalidate;
end;

procedure TForm_Minimap.PB2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    PB2.Tag := 1;
    SetMapsCoordinates(X, Y);
  end;
end;

procedure TForm_Minimap.PB2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if PB2.Tag = 0 then exit;
  SetMapsCoordinates(X, Y);
end;

procedure TForm_Minimap.PB2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then PB2.Tag:=0;
end;

procedure TForm_Minimap.FormShow(Sender: TObject);
var p: TPoint;
begin
 p.x := FormMain.SpeedButton3.Left;
 p.y := FormMain.SpeedButton3.Top+FormMain.SpeedButton3.Height;
 p := FormMain.ClientToScreen(p);

 p := Mouse.CursorPos;

 Top := p.y-Height div 2;
 Left := p.x-Width div 2;
 PB2SetSizeAndPos
end;

procedure TForm_Minimap.FormDeactivate(Sender: TObject);
begin
// Hide;
end;

procedure TForm_Minimap.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_M then begin
   if Active then Hide;
 end;
end;

procedure TForm_Minimap.FormResize(Sender: TObject);
begin
  if Width < ScaleDesignToForm(40) then Width := ScaleDesignToForm(40);
  if Height < ScaleDesignToForm(30) then Height := ScaleDesignToForm(30);
end;


end.

