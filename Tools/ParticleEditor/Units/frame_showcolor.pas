unit frame_ShowColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene;

type

  { TFrameShowColor }

  TFrameShowColor = class(TFrame)
    PB: TPaintBox;
    procedure PBPaint(Sender: TObject);
  private
    FImage,
    FBackground: TBGRABitmap;
    procedure DrawBackGroundImage;
  public
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure FreeData;
    procedure UpdateColor(P: array of TPColor);
  end;

implementation

{$R *.lfm}

{ TFrameShowColor }

procedure TFrameShowColor.PBPaint(Sender: TObject);
var temp: TBGRABitmap;
begin
  if FBackground = NIL then DrawBackGroundImage
    else if (FBackground.Width <> PB.Width) or (FBackground.Height <> PB.Height) then DrawBackGroundImage;

  if FImage = NIL then begin
    FBackground.Draw(PB.Canvas, 0, 0);
    exit;
  end;

  temp := TBGRABitmap.Create(PB.Width, PB.Height);
  temp.PutImage(0, 0, FBackground, dmSet);
  temp.PutImage(0, 0, FImage, dmDrawWithTransparency);
  temp.Draw(PB.Canvas, 0, 0);
  temp.Free;
end;

procedure TFrameShowColor.DrawBackGroundImage;
var ima, ima1: TBGRABitmap;
begin
  if FBackground = NIL then begin
    FBackground:= TBGRABitmap.Create(PB.Width, PB.Height);
    FBackground.FontAntialias := TRUE;
    FBackground.FontHeight := 10;
  end else if (FBackground.Width <> PB.Width) or (FBackground.Height <> PB.Height)
             then FBackground.SetSize(PB.Width, PB.Height);

  // white/gray image for grid
  ima1 := TBGRABitmap.Create(8, 8, BGRA(220,220,220));
  ima := TBGRABitmap.Create(16, 16, BGRAWhite);
  ima.PutImage(8, 0, ima1, dmSet, 255);
  ima.PutImage(0, 8, ima1, dmSet, 255);
  ima1.Free;
  FBackground.Fill(ima);
  ima.Free;
end;

procedure TFrameShowColor.EraseBackground(DC: HDC);
begin
end;

procedure TFrameShowColor.FreeData;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FImage);
end;

procedure TFrameShowColor.UpdateColor(P: array of TPColor);
var cprevious, ccurrent: TBGRAPixel;
  i, xbegin, xend, xx: integer;
  dr,dg,db,da,r,g,b,a,deltax:single;
  txt:string;
begin
  if FImage = NIL then begin
    FImage:= TBGRABitmap.Create(PB.Width, PB.Height);
    FImage.FontAntialias := TRUE;
    FImage.FontHeight := 10;
  end else if (FImage.Width <> PB.Width) or (FImage.Height <> PB.Height)
             then FImage.SetSize(PB.Width, PB.Height);

  FImage.Fill(BGRAPixelTransparent);

  if Length(P) = 0 then exit;

  if Length(P) = 1 then begin
    FImage.Fill(P[0].C);
    exit;
  end;
  i := 0;
  repeat
   inc(i);
   cprevious := P[i-1].C;
   ccurrent := P[i].C;

   xbegin := round(FImage.Width*P[i-1].Life);
   xend := round(FImage.Width*P[i].Life);
   deltax := 1/(xend-xbegin);

   dr := (ccurrent.red-cprevious.red)*deltax;
   dg := (ccurrent.green-cprevious.green)*deltax;
   db := (ccurrent.blue-cprevious.blue)*deltax;
   da := (ccurrent.alpha-cprevious.alpha)*deltax;

   r := cprevious.red;
   g := cprevious.green;
   b := cprevious.blue;
  // a := cprevious.alpha;
   a:=255;

   for xx:=xbegin to xend do begin
   // FImage.DrawVertLine( xx, 0, FImage.Height, cprevious );
    FImage.DrawVertLine( xx, 0, FImage.Height, BGRA(round(r),round(g),round(b),round(a)));
    r+=dr;
    g+=dg;
    b+=db;
   // a+=da;
   end;

   txt := BGRAPixelToHex(cprevious);
   FImage.TextOut(xbegin,0,txt,BGRAWhite);

  until i = Length(P)-1;
  FImage.TextOut(0, FImage.Height-FImage.FontHeight, inttostr(Length(P))+' pts',BGRAWhite);
  PB.Invalidate;
end;

end.

