unit Frame_CurveEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAPath,
  Math;

const GRID_SIZE = 20;
      HALF_WIDTH_POINT = 5 ;

type

  TOnCurveChange = procedure(p: PPointF; count: integer) of object;
  TOnDeleteCallBack = procedure(Index: integer) of object;
  TOnPointDblClick = procedure(P: PPointF) of object;
  TOnAddPoint = procedure(PointIndex: integer; P: TPointF) of object;
  TOnMouseOverNode = procedure(P: PPointF) of object;

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
    FShowNodes: boolean;
    FPPreviousPts,
    FPNextPts,
    FPCurrentPts,
    FWorkingPoint: PPointF;
    CurveColor: TBGRAPixel;
    HandleColor: TBGRAPixel;
    FCurveChanged: boolean;
    FOnChangeCallBack: TOnCurveChange;
    FOnDeleteCallBack: TOnDeleteCallBack;
    FOnPointDblClick: TOnPointDblClick;
    FOnAddPoint: TOnAddPoint;
    FOnMouseOverNode: TOnMouseOverNode;
    function PointIsUnderMouse(const APts: TPointF; X, Y: Integer): boolean;
    function GetPointIndexAtCoordinate(X, Y: integer): integer;
    procedure DoDeletePoint(aIndex: integer);
    function PercentXToPaintBoxX(aValue: single): single;
    function PercentYToPaintBoxY(aValue: single): single;
    function PaintBoxXToPercentX(aValue: single): single;
    function PaintBoxYToPercentY(aValue: single): single;
  private
    FHorizAxisArray: array of single;
    FGridStep: integer;
    FShowGrid: boolean;
    FActivateSplineMode: boolean;
    FSplineStyle: TSplineStyle;
    function GetNodeCount: integer;
    procedure SetShowGrid(AValue: boolean);
    procedure SetShowNodes(AValue: boolean);
  public
   destructor Destroy; override;
   procedure EraseBackground({%H-}DC: HDC); override;
   procedure Clear; // remove all points
   // aX and aY range is [0..1]
   function AddPoint(aX, aY: single; aSelectIt: boolean=FALSE): integer;
   procedure DeleteLastPoint;

   procedure ActivateSplineMode(aSplineStyle: TSplineStyle);
   procedure DeactivateSplineMode;
   procedure MoveCurve(aDeltaX, aDeltaY: single);

   procedure SaveToFile(const aFilename: string);
   procedure LoadFromFile(const aFilename: string);

   procedure SetGridSize(aValue: integer);
   procedure AddHorizAxis(aArray: array of single);

   property OnCurveChange: TOnCurveChange read FOnChangeCallBack write FOnChangeCallBack;
   property OnDeleteCallBack: TOnDeleteCallBack read FOnDeleteCallBack write FOnDeleteCallBack;
   property OnPointDblClick: TOnPointDblClick read FOnPointDblClick write FOnPointDblClick;
   property OnAddPoint: TOnAddPoint read FOnAddPoint write FOnAddPoint;
   property OnMouseOverNode: TOnMouseOverNode read FOnMouseOverNode write FOnMouseOverNode;

   property ShowGrid: boolean read FShowGrid write SetShowGrid;
   property ShowNodes: boolean read FShowNodes write SetShowNodes;
   property NodeCount: integer read GetNodeCount;

   property SplineModeActivated: boolean read FActivateSplineMode;
   property SplineStyle: TSplineStyle read FSplineStyle;
  end;

implementation
uses OGLCScene;

{ TFrameEditCurve }

procedure TFrameEditCurve.PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
    c: TBGRAPixel;
begin
  FCurveChanged := FALSE;
  c := ColorToBGRA(PB.Canvas.Pixels[X,Y]);
  i := GetPointIndexAtCoordinate(X, Y);

  if (Button = mbLeft) then begin
    if c = CurveColor then begin
      // left click on curve -> insert point
    end
    else
    if c = HandleColor then begin
      // left click on point -> get points clicked
      if ssDouble in Shift then begin
        if FOnPointDblClick <> NIL then FOnPointDblClick(@FLinePts[i]);
      end else begin
        if i <> -1 then FWorkingPoint := @FLinePts[i];
        PB.Invalidate;
        exit;
      end;
    end
    else begin
     // left click on empty space -> add point
     i := AddPoint(PaintBoxXToPercentX(X), PaintBoxYToPercentY(Y), TRUE);
     FCurveChanged := TRUE;
     if FOnAddPoint <> NIL then FOnAddPoint(i, FLinePts[i]);
     PB.Invalidate;
     exit;
    end;
  end;

{  // left click on point -> get points clicked
  if (Button = mbLeft) and (c = HandleColor) and not (ssDouble in Shift) then begin
    for i:=0 to Length(FLinePts)-1 do
      if PointIsUnderMouse(FLinePts[i], X, Y) then begin
        FWorkingPoint := @FLinePts[i];
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
  end;  }

  // right click on point -> delete it
  if (Button = mbRight) and (c = HandleColor) and (i <> -1) then
    DoDeletePoint(i);
end;

procedure TFrameEditCurve.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer );
var c: TBGRAPixel;
    i: integer;
begin
  if FWorkingPoint <> NIL then begin
    X := EnsureRange(X, 0, PB.Width);
    Y := EnsureRange(Y, 0, PB.Height);
    // update the point coordinates
    FWorkingPoint^.x := PaintBoxXToPercentX(X);
    FWorkingPoint^.Y := PaintBoxYToPercentY(Y);
    FCurveChanged := TRUE;
    PB.Invalidate ;
  end else begin // no point locked by mouse
     i := GetPointIndexAtCoordinate(X, Y);
     if i <> -1 then begin
       PB.Cursor := crHandPoint;
       if FOnMouseOverNode <> NIL then FOnMouseOverNode(@FLinePts[i]);
     end else PB.Cursor := crDefault;
     FCurveChanged := FALSE;

  {  c := ColorToBGRA(PB.Canvas.Pixels[X,Y]);
    if (c = HandleColor) or (c = CurveColor) then begin
      PB.Cursor := crHandPoint;
      if FOnMouseOverNode <> NIL then FOnMouseOverNode(@FLinePts[i]);
    end else PB.Cursor := crDefault;
    FCurveChanged := FALSE; }
  end;
end;

procedure TFrameEditCurve.PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then begin
    FWorkingPoint := NIL;
    if FCurveChanged and (FOnChangeCallBack <> NIL)
      then FOnChangeCallBack(@FLinePts[0], Length(FLinePts));
    FCurveChanged := FALSE;
  end;
end;

procedure TFrameEditCurve.SetShowNodes(AValue: boolean);
begin
  if FShowNodes = AValue then Exit;
  FShowNodes := AValue;
  PB.Invalidate;
end;

destructor TFrameEditCurve.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TFrameEditCurve.GetNodeCount: integer;
begin
  Result := Length(FLinePts);
end;

procedure TFrameEditCurve.SetShowGrid(AValue: boolean);
begin
  if FShowGrid = AValue then Exit;
  FShowGrid := AValue;
  PB.Invalidate;
end;

procedure TFrameEditCurve.EraseBackground(DC: HDC);
begin
end;

procedure TFrameEditCurve.PBPaint(Sender: TObject);
var i: integer;
    yy: single;
    pts: array of TPointF;
    splines: TOGLCPath;
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

   // render grid
   if FShowGrid then begin
      i:=0;
      repeat
       inc(i, FGridStep);
       DrawLineAntialias( 0, i, Width-1, i, BGRA(60,40,40), 1);
       DrawLineAntialias( i, 0, i, Height-1, BGRA(60,40,40), 1);
      until (i > Width) and (i > Height);
   end;

   // compute point on paintbox
   pts := NIL;
   SetLength(pts, length(FLinePts));
   for i:=0 to length(FLinePts)-1 do
    begin
     pts[i].x := PercentXToPaintBoxX(FLinePts[i].x);
     pts[i].y := PercentYToPaintBoxY(FLinePts[i].y);
    end;

   if FActivateSplineMode then begin
     // compute spline curves
     splines := NIL;
     splines.ConcatPoints(ComputeOpenedSpline(pts, FSplineStyle));
   end;

   // render curve
   if FActivateSplineMode then DrawPolylineAntialias(splines, BGRA(255,140,0), 4)
     else DrawPolylineAntialias(pts, BGRA(255,140,0), 4);
   // render points
   if FShowNodes then
     for i:=0 to High(pts) do
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

function TFrameEditCurve.GetPointIndexAtCoordinate(X, Y: integer): integer;
var i: integer;
begin
  for i:=0 to Length(FLinePts)-1 do
    if PointIsUnderMouse(FLinePts[i], X, Y) then
      exit(i);
  Result := -1;
end;

procedure TFrameEditCurve.DoDeletePoint(aIndex: integer);
begin
  if Length(FLinePts) = 0 then exit;
  if FOnDeleteCallBack <> NIL then FOnDeleteCallBack(aIndex);
  Delete(FLinePts, aIndex, 1);
  if FOnChangeCallBack <> NIL then FOnChangeCallBack(@FLinePts[0], Length(FLinePts));
  FWorkingPoint := NIL;
  PB.Invalidate;
end;

function TFrameEditCurve.PercentXToPaintBoxX(aValue: single): single;
begin
  Result := aValue * PB.Width;
end;

function TFrameEditCurve.PercentYToPaintBoxY(aValue: single): single;
begin
  Result := aValue * PB.Height;
end;

function TFrameEditCurve.PaintBoxXToPercentX(aValue: single): single;
begin
  Result := aValue / PB.Width;
end;

function TFrameEditCurve.PaintBoxYToPercentY(aValue: single): single;
begin
  Result := aValue / PB.Height;
end;

procedure TFrameEditCurve.Clear;
begin
  SetLength(FLinePts, 0);
  FWorkingPoint := NIL;
  PB.Invalidate;
end;

function TFrameEditCurve.AddPoint(aX, aY: single; aSelectIt: boolean): integer;
var i, j: integer;
begin
  aX := EnsureRange(aX, 0.0, 1.0);
  aY := EnsureRange(aY, 0.0, 1.0);

  i := Length(FLinePts);
  SetLength(FLinePts, i+1);
  FLinePts[i] := PointF(aX, aY);
  Result := i;
  if aSelectIt then FWorkingPoint := @FLinePts[i]
    else FWorkingPoint := NIL;
  PB.Invalidate;
{  i := 0;
  if Length(FLinePts) > 1 then begin
    repeat
      j := i;
      if aX = FLinePts[i].x then begin
        FLinePts[i].y := aY;        // the point already exists. we change only its value
        exit;
      end else if aX > FLinePts[i].x then inc(i);
    until (i = j) or (i = Length(FLinePts)-1);

    if i < Length(FLinePts)-1
      then for j:=Length(FLinePts)-2 downto i do
             FLinePts[j+1] := FLinePts[j];
  end;
  FLinePts[i] := PointF(aX, aY);
  Result := i;
  if aSelectIt then FWorkingPoint := @FLinePts[i]
    else FWorkingPoint := NIL;
  PB.Invalidate;   }
end;

procedure TFrameEditCurve.DeleteLastPoint;
begin
  DoDeletePoint(High(FLinePts));
end;

procedure TFrameEditCurve.ActivateSplineMode(aSplineStyle: TSplineStyle);
begin
  FActivateSplineMode := True;
  FSplineStyle := aSplineStyle;
  PB.Invalidate;
end;

procedure TFrameEditCurve.DeactivateSplineMode;
begin
  FActivateSplineMode := False;
  PB.Invalidate;
end;

procedure TFrameEditCurve.MoveCurve(aDeltaX, aDeltaY: single);
var i: integer;
begin
 for i:=0 to High(FLinePts) do begin
   FLinePts[i].x := FLinePts[i].x + aDeltaX;
   FLinePts[i].y := FLinePts[i].y + aDeltaY;
  end;
 PB.Invalidate;
end;

procedure TFrameEditCurve.SaveToFile(const aFilename: string);
var t: TStringList;
    s: string;
    i: integer;
    p: TPointF;
    prop: TProperties;
begin
 t := TStringList.Create;
 try
   prop.Init('|');
   prop.Add('NodeCount', Length(FLinePts));
   prop.Add('UseSpline', FActivateSplineMode);
   prop.Add('SplineStyle', Ord(FSplineStyle));
   s := '';
   for i:=0 to High(FLinePts) do begin
     s := s + FormatFloatWithDot('0.000', FLinePts[i].x)+' '+FormatFloatWithDot('0.000', FLinePts[i].y);
     if i < High(FLinePts) then s := s + ' ';
   end;
   prop.Add('Nodes', s);

   t.Add('[OGLC_PATH]');
   t.Add(prop.PackedProperty);
   t.SaveToFile( aFilename );
 finally
   t.Free;
 end;
end;

procedure TFrameEditCurve.LoadFromFile(const aFilename: string);
var t: TStringList;
    A: TStringArray;
    c, k, i: integer;
    s: string;
    prop: TProperties;
begin
 Clear;
 t := TStringList.Create;
 try
  t.LoadFromFile(aFilename);
  c := 0;
  k := 0;
  s := '';
  prop.SplitFrom(t, '[OGLC_PATH]', '|');
  prop.IntegerValueOf('NodeCount', c, 0);
  prop.BooleanValueOf('UseSpline', FActivateSplineMode, False);
  prop.IntegerValueOf('SplineStyle', k, 0);
  FSplineStyle := TSplineStyle(k);
  prop.StringValueOf('Nodes', s, '');
  A := s.Split([' ']);
  i := 0;
  while c > 0 do begin
   AddPoint(StringToSingle(A[i]), StringToSingle(A[i+1]), FALSE);
   inc(i, 2);
   dec(c);
  end;
 finally
   t.Free;
 end;
end;

procedure TFrameEditCurve.SetGridSize(aValue: integer);
begin
  if aValue < 2 then aValue := 2;
  FGridStep := aValue;
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

