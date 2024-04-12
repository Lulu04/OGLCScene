unit Frame_CurveEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  Math;

const GRID_SIZE = 20;
      HALF_WIDTH_POINT = 5 ;

type

  TOnCurveChange = procedure(p: PPointF; count: integer) of object;
  TOnDeleteCallBack = procedure(Index: integer) of object;
  TOnPointDblClick = procedure(PointIndex: integer) of object;
  TOnAddPoint = procedure(PointIndex: integer; P: TPointF) of object;

  { TFrameEditCurve }

  TFrameEditCurve = class(TFrame)
    PB: TPaintBox;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PBPaint(Sender: TObject);
  private
    FLinePts: ArrayOfTPointF;
    FImage: TBGRABitmap;
    FPPreviousPts,
    FPNextPts,
    FPCurrentPts: PPointF;
    CurveColor: TBGRAPixel;
    HandleColor: TBGRAPixel;
    IndexPointToDrag: integer;
    FCurveChanged: boolean;
    FOnChangeCallBack: TOnCurveChange;
    FOnDeleteCallBack: TOnDeleteCallBack;
    FOnPointDblClick: TOnPointDblClick;
    FOnAddPoint: TOnAddPoint;
    function PointIsUnderMouse(const APts: TPointF; X, Y: Integer): boolean;
    function PercentXToPaintBoxX(aValue: single): single;
    function PercentYToPaintBoxY(aValue: single): single;
    function PaintBoxXToPercentX(aValue: single): single;
    function PaintBoxYToPercentY(aValue: single): single;
    procedure SetPreviousCurrentNextPts(aIndex: integer);
    procedure ClearPreviousCurrentNextPts;
  private
    FLegendMax,
    FLegendMin: string;
    FHorizAxisArray: array of single;
  public
   procedure EraseBackground({%H-}DC: HDC); override;
   procedure FreeData; // call before exit program to free memory used by TFrameEditCurve
   procedure Clear; // remove all points
   // aTimePercent [0..1] is a percentage of particle life
   // aPercentValue [0..1] is Value/MaxValue
   function AddPoint(aTimePercent, aPercentValue: single; aSelectIt: boolean=FALSE): integer;

   procedure SetLegendMinMax(const aMin, aMax: string);
   procedure AddHorizAxis(aArray: array of single);

   property OnCurveChange: TOnCurveChange read FOnChangeCallBack write FOnChangeCallBack;
   property OnDeleteCallBack: TOnDeleteCallBack read FOnDeleteCallBack write FOnDeleteCallBack;
   property OnPointDblClick: TOnPointDblClick read FOnPointDblClick write FOnPointDblClick;
   property OnAddPoint: TOnAddPoint read FOnAddPoint write FOnAddPoint;
  end;

implementation

{ TFrameEditCurve }

procedure TFrameEditCurve.PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
    c: TBGRAPixel;
begin
  FCurveChanged := FALSE;
  c := ColorToBGRA(PB.Canvas.Pixels[X,Y]);

  // left click on curve -> add point
  if (Button = mbLeft) and (c = CurveColor) then begin
    i := AddPoint( PaintBoxXToPercentX(X), PaintBoxYToPercentY(Y), TRUE);
    FCurveChanged := TRUE;
    if FOnAddPoint <> NIL then FOnAddPoint(i, FLinePts[i]);
    PB.Invalidate;
    exit;
  end;

  // left click on point -> get points clicked
  if (Button = mbLeft) and (c = HandleColor) and not (ssDouble in Shift) then begin
    for i:=0 to Length(FLinePts)-1 do
      if PointIsUnderMouse(FLinePts[i], X, Y) then begin
        SetPreviousCurrentNextPts(i);
        break;
      end;
    PB.Invalidate;
    exit;
  end;

  // double left click on point
  // left click on point -> get points clicked
  if (Button = mbLeft) and (c = HandleColor) and (ssDouble in Shift)then begin
    for i:=0 to Length(FLinePts)-1 do
      if PointIsUnderMouse(FLinePts[i], X, Y) then begin
        if FOnPointDblClick <> NIL then FOnPointDblClick(i);
        break;
      end;
    PB.Invalidate;
    exit;
  end;

  // right click on point -> delete it
  if (Button = mbRight) and (c = HandleColor) then begin
    for i:=High(FLinePts)-1 downto 1 do
      if PointIsUnderMouse( FLinePts[i], X, Y ) then begin
        if FOnDeleteCallBack <> NIL then FOnDeleteCallBack(i);
        Delete(FLinePts, i, 1);
        FOnChangeCallBack(@FLinePts[0], Length(FLinePts));
        ClearPreviousCurrentNextPts;
        PB.Invalidate;
        exit;
      end;
  end;
end;

procedure TFrameEditCurve.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer );
var c: TBGRAPixel;
    xx: integer;
begin
  if FPCurrentPts <> NIL then begin
    // on empêche que le point en dépasse un autre horizontalement
    if FPPreviousPts <> NIL then begin
      xx := round(PercentXToPaintBoxX(FPPreviousPts^.x));
      if X <= xx then X := xx + HALF_WIDTH_POINT;
    end else X := 0;
    if FPNextPts <> NIL then begin
      xx := round(PercentXToPaintBoxX(FPNextPts^.x));
      if X >= xx then X:= xx;
    end else X := PB.Width - 1;
    if Y < 0 then Y := 0;
    if Y > PB.Height-1 then Y := PB.Height-1;
    if X < 0 then X := 0;
    if X > PB.Width-1 then X := PB.Width-1;
    //  if IndexPointToDrag=0 then X:=0;
    //  if IndexPointToDrag=Length(FLinePts)-1 then X:=PB.Width-1;
    // on actualise les coordonnées du point
    FPCurrentPts^.x := PaintBoxXToPercentX(X);
    FPCurrentPts^.Y := PaintBoxYToPercentY(Y);
    FCurveChanged := TRUE;
    PB.Invalidate ;
  end else begin // no point locked by mouse
    c := ColorToBGRA(PB.Canvas.Pixels[X,Y]);
    if (c = HandleColor) or (c = CurveColor) then PB.Cursor := crHandPoint
      else PB.Cursor := crDefault;
    FCurveChanged := FALSE;
  end;
end;

procedure TFrameEditCurve.PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then begin
    ClearPreviousCurrentNextPts;
    IndexPointToDrag := -1;
    if FCurveChanged and (FOnChangeCallBack <> NIL)
      then FOnChangeCallBack(@FLinePts[0], Length(FLinePts));
    FCurveChanged := FALSE;
  end;
end;

procedure TFrameEditCurve.SetPreviousCurrentNextPts(aIndex: integer);
begin
  FPCurrentPts := @FLinePts[aIndex];
  IndexPointToDrag := aIndex;

  if aIndex = 0 then FPPreviousPts := NIL
    else FPPreviousPts := @FLinePts[aIndex-1];

  if aIndex = Length(FLinePts)-1 then FPNextPts := NIL
    else FPNextPts := @FLinePts[aIndex+1];
end;

procedure TFrameEditCurve.ClearPreviousCurrentNextPts;
begin
  FPCurrentPts := NIL;
  FPPreviousPts := NIL;
  FPNextPts := NIL;
end;

procedure TFrameEditCurve.EraseBackground(DC: HDC);
begin
end;

procedure TFrameEditCurve.PBPaint(Sender: TObject);
var i: integer;
    yy: single;
    pts: array of TPointF;
    gridSize: integer;
begin
  if FImage = NIL then begin
    CurveColor := BGRA(255,140,0);
    HandleColor := BGRAWhite;
    FImage := TBGRABitmap.Create(PB.Width, PB.Height);
    FImage.FontAntialias := TRUE;
    FImage.FontHeight := 10;
  end else if (FImage.Width <> PB.Width) or (FImage.Height <> PB.Height)
              then FImage.SetSize(PB.Width, PB.Height);
  with FImage do begin
   // background
   Fill(BGRA(50,20,20));
   Rectangle(0, 0, 1, 1, BGRA(90,90,90), BGRA(50,20,20),dmSet);
   DrawLineAntialias(1, Height shr 1, Width-2, Height shr 1, BGRA(60,40,40), 1);
   DrawLineAntialias(Width shr 1, 1, Width shr 1, Height -2, BGRA(60,40,40), 1);
   //grid
   gridSize := Round(Height/20);// ScaleDesignToForm(GRID_SIZE);
   i := 0;
   repeat
    inc(i);
    DrawLineAntialias(1, Height shr 1 + i*gridSize, Width-1, Height shr 1 + i*gridSize, BGRA(60,40,40), 1);
    DrawLineAntialias(1, Height shr 1 - i*gridSize, Width-1, Height shr 1 - i*gridSize, BGRA(60,40,40), 1);
    DrawLineAntialias(Width shr 1 + i*gridSize, 1, Width shr 1 + i*gridSize, Height-1, BGRA(60,40,40), 1);
    DrawLineAntialias(Width shr 1 - i*gridSize, 1, Width shr 1 - i*gridSize, Height-1, BGRA(60,40,40), 1);
   until Width shr 1 + i*gridSize > Width;
   // horizontal axis
   for i:=0 to Length(FHorizAxisArray)-1 do begin
    yy := Height-Height*FHorizAxisArray[i];
    DrawLineAntialias(1, yy, Width-2, yy, BGRA(90,60,60), 1.5);
   end;

   // Legends
   TextOut(0, 0, FLegendMax, BGRA(255,255,0));
   TextOut(0, Height-FontHeight, FLegendMin, BGRA(255,255,0));

   // compute point on paintbox
   pts := NIL;
   SetLength(pts, length(FLinePts));
   for i:=0 to length(FLinePts)-1 do
    begin
     pts[i].x := PercentXToPaintBoxX(FLinePts[i].x);
     pts[i].y := PercentYToPaintBoxY(FLinePts[i].y);
    end;

   // render curve
   DrawPolylineAntialias(pts, BGRA(255,140,0), 4);
   // render points
   for i:=0 to Length(FLinePts)-1 do
    RectangleAntialias(pts[i].x-HALF_WIDTH_POINT, pts[i].y-HALF_WIDTH_POINT,
                       pts[i].x+HALF_WIDTH_POINT, pts[i].y+HALF_WIDTH_POINT,
                       HandleColor, 1, HandleColor);

  end;
  FImage.Draw(PB.Canvas, 0, 0, TRUE);
end;

function TFrameEditCurve.PointIsUnderMouse(const APts: TPointF; X, Y: Integer): boolean;
var p: TPointF;
begin
  p.x := PercentXToPaintBoxX(APts.x);
  p.y := PercentYToPaintBoxY(APts.y);
  Result:= (X >= p.x-HALF_WIDTH_POINT) and
           (X <= p.x+HALF_WIDTH_POINT) and
           (Y >= p.y-HALF_WIDTH_POINT) and
           (Y <= p.y+HALF_WIDTH_POINT);
end;

function TFrameEditCurve.PercentXToPaintBoxX(aValue: single): single;
begin
  Result := aValue * (PB.Width-1);
end;

function TFrameEditCurve.PercentYToPaintBoxY(aValue: single): single;
begin
  Result := PB.Height - aValue*PB.Height;
end;

function TFrameEditCurve.PaintBoxXToPercentX(aValue: single): single;
begin
  Result := aValue/(PB.Width-1);
end;

function TFrameEditCurve.PaintBoxYToPercentY(aValue: single): single;
begin
  Result := (PB.Height-aValue)/PB.Height;
end;

procedure TFrameEditCurve.Clear;
begin
  SetLength(FLinePts, 0);
  PB.Invalidate;
end;

function TFrameEditCurve.AddPoint(aTimePercent, aPercentValue: single; aSelectIt: boolean): integer;
var i, j: integer;
begin
  aTimePercent := EnsureRange(aTimePercent, 0.0, 1.0);
  aPercentValue := EnsureRange(aPercentValue, 0.0, 1.0);

  SetLength(FLinePts, Length(FLinePts)+1);
  i := 0;
  if Length(FLinePts) > 1 then begin
    repeat
      j := i;
      if aTimePercent = FLinePts[i].x then begin
        FLinePts[i].y := aPercentValue;        // the point already exists. we change only its value
        exit;
      end else if aTimePercent > FLinePts[i].x then inc(i);
    until (i = j) or (i = Length(FLinePts)-1);

    if i < Length(FLinePts)-1
      then for j:=Length(FLinePts)-2 downto i do
             FLinePts[j+1] := FLinePts[j];
  end;
  FLinePts[i] := PointF(aTimePercent, aPercentValue);
  Result := i;
  if aSelectIt then SetPreviousCurrentNextPts(i)
    else ClearPreviousCurrentNextPts;
  PB.Invalidate;
end;

procedure TFrameEditCurve.FreeData;
begin
  if FImage <> NIL then FImage.Free;
  FImage := NIL;
end;

procedure TFrameEditCurve.SetLegendMinMax(const aMin, aMax: string);
begin
  FLegendMax := aMax;
  FLegendMin := aMin;
  PB.Invalidate;
end;

procedure TFrameEditCurve.AddHorizAxis(aArray: array of single);
var i: integer;
begin
  SetLength(FHorizAxisArray, Length(aArray));
  for i:=0 to Length(aArray)-1 do FHorizAxisArray[i] := aArray[i];
end;


{$R *.lfm}

end.

