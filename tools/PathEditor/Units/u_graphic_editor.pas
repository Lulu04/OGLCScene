unit u_graphic_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  BGRABitmap, BGRABitmapTypes,
  Math;

const HALF_WIDTH_POINT = 5 ;

type

  TGraphPoint = TPointF;
  PGraphPoint = ^TGraphPoint;

  TOnCurveChange = procedure( PointList: TList ) of object;
  TOnPointDblClick = procedure( PointIndex: integer ) of object;
  TOnAddPoint = procedure( PointIndex: integer; P: PGraphPoint ) of object;
  TOnDeletePoint = procedure( PointIndex: integer ) of object;
  TOnDrawBackground = procedure( BG: TBGRABitmap ) of object;
  TOnRightClickOnPoint = procedure( P: PGraphPoint; X, Y: integer; Shift: TShiftState ) of object;


  { TGraphEditor }

  TGraphEditor = class
   Constructor Create;
   Destructor Destroy; override;
  private
    PB: TPaintBox;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
  private
    FList: TList;
    FPtDraggedByMouse: PGraphPoint;
    FDrawingName: string;
    FImage: TBGRABitmap;
    CurveColor: TBGRAPixel;
    FPointColor,
    FLineColor: TBGRAPixel;
    FCurveChanged: boolean;
    FUpdateCallBack: TOnCurveChange;
    FOnPointDblClick: TOnPointDblClick;
    FOnAddPoint: TOnAddPoint;
    FOnDeletePoint: TOnDeletePoint;
    FOnDrawBackground: TOnDrawBackground;
    FOnRightClickOnPoint: TOnRightClickOnPoint;
    procedure DoClear;
    function GetPointByIndex( Index: integer): PGraphPoint;
    function PointIndexOverMouse( X, Y: integer ): integer;
    function PtsIsMousePointed( const APts: PGraphPoint; X, Y: Integer ): boolean;
    function PercentXToPaintBoxX( aValue: single ): single;
    function PercentYToPaintBoxY( aValue: single ): single;
    function PaintBoxXToPercentX( aValue: single ): single;
    function PaintBoxYToPercentY( aValue: single ): single;
  private
    FGridStep: integer;
    FShowGrid,
    FShowPoint,
    FStayOnGrid: boolean;
    function GetPointCount: integer;
    procedure SetLineColor(AValue: TBGRAPixel);
    procedure SetPointColor(AValue: TBGRAPixel);
    procedure SetShowGrid( aValue: boolean );
    procedure SetShowPoint(AValue: boolean);
  public
   procedure SetPaintBoxTarget( aPB: TPaintBox );
   procedure SetGridStep( aValue: integer );
   procedure Clear; // remove all points
   function AddPoint( aXPercent, aYPercent: single; aSelectIt: boolean ): integer; // [0..1] range
   procedure DeletePointByIndex( aIndex: integer );
   procedure DeletePoint( aPoint: PGraphPoint );
   procedure SaveToFile( const aFilename: string );
   procedure LoadFromFile( const aFilename: string );
   procedure MovePoints( aDeltaX, aDeltaY: integer);

   property ShowGrid: boolean read FShowGrid write SetShowGrid;
   property ShowPoint: boolean read FShowPoint write SetShowPoint;
   property StayOnGrid: boolean read FStayOnGrid write FStayOnGrid;
   property PointColor: TBGRAPixel read FPointColor write SetPointColor;
   property LineColor: TBGRAPixel read FLineColor write SetLineColor;

   property PointCount: integer read GetPointCount;
   property DrawingName: string read FDrawingName write FDrawingName;

   property OnCurveChange: TOnCurveChange read FUpdateCallBack write FUpdateCallBack;
   property OnPointDblClick: TOnPointDblClick read FOnPointDblClick write FOnPointDblClick;
   property OnAddPoint: TOnAddPoint read FOnAddPoint write FOnAddPoint;
   property OnDeletePoint: TOnDeletePoint read FOnDeletePoint write FOnDeletePoint;
   property OnDrawBackground: TOnDrawBackground read FOnDrawBackground write FOnDrawBackground;
   property OnRightClickOnPoint: TOnRightClickOnPoint read FOnRightClickOnPoint write FOnRightClickOnPoint;
  end;

var
  Editor: TGraphEditor=NIL;

implementation
uses OGLCScene;

type
ArrayOfString= array of string;

function SplitLineToStringArray(ALine: string; aCharSeparator: char): ArrayOfString;
var i, w: integer;
begin
 Result := NIL;
 SetLength( Result, 0 );
 if Length( ALine ) = 0 then exit;

 if ALine[1] <> aCharSeparator
   then ALine := aCharSeparator + ALine;

 i := 1;
 w := -1;
 repeat
  if ALine[i] <> aCharSeparator
    then Result[w] := Result[w] + ALine[i]
    else begin
          inc ( w );
          SetLength( Result, w + 1 );
          Result[w] := '';
    end;
  inc ( i );
 until i > Length( ALine );
end;

{ TGraphEditor }

constructor TGraphEditor.Create;
begin
 FList := TList.Create;
 FGridStep := 10;
 FShowGrid := TRUE;
 FStayOnGrid := TRUE;
 FShowPoint := TRUE;
 FPointColor := BGRA(255,255,255);
 FLineColor := BGRA(255,180,80);
end;

destructor TGraphEditor.Destroy;
begin
 DoClear;
 FList.Free;
 if FImage <> NIL then FImage.Free;
 FImage := NIL;
 inherited Destroy;
end;

procedure TGraphEditor.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var flag : boolean;
    i, IndexOverMouse : integer;
begin
 if FStayOnGrid then begin
   X := round(X / FGridStep) * FGridStep;
   Y := round(Y / FGridStep) * FGridStep;
 end;

 flag := FALSE;
 FCurveChanged:=FALSE;
 IndexOverMouse := PointIndexOverMouse( X, Y );

 // left click on empty place -> add point
 if ( Button = mbLeft ) and ( IndexOverMouse = -1 )
   then begin
         flag:=TRUE;
         i := AddPoint( PaintBoxXToPercentX(X), PaintBoxYToPercentY(Y), TRUE);
         FCurveChanged := TRUE;
         PB.Cursor := crHandPoint;
         if FOnAddPoint<>NIL then FOnAddPoint( i, GetPointByIndex(i) );
   end;

 // left click on point -> get points clicked
 if ( Button = mbLeft ) and ( IndexOverMouse<>-1 )
     and not(ssDouble in Shift) then begin
         flag:=TRUE;
         FPtDraggedByMouse := GetPointByIndex( IndexOverMouse );
 end;

 // double left click on point
 if ( Button = mbLeft ) and ( IndexOverMouse <> -1 ) and (ssDouble in Shift)then begin
   flag:=TRUE;
   if FOnPointDblClick<>NIL then FOnPointDblClick( IndexOverMouse );
 end;

 // right click on point -> user popup menu on point
 if ( Button = mbRight ) and ( IndexOverMouse<>-1 )
     and ( FOnRightClickOnPoint<>NIL ) then begin
   FOnRightClickOnPoint( GetPointByIndex( IndexOverMouse ), X, Y, Shift );
   flag:=TRUE;
   FCurveChanged := TRUE;
   FPtDraggedByMouse := NIL;
 end;

 if flag then PB.Invalidate;
end;

procedure TGraphEditor.PBMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if FStayOnGrid then begin
   X := round(X / FGridStep) * FGridStep;
   Y := round(Y / FGridStep) * FGridStep;
 end;

 if FPtDraggedByMouse <> NIL
   then begin // mouse drag point
         X := EnsureRange( X, 0, PB.Width-1 );
         Y := EnsureRange( Y, 0, PB.Height-1 );
         // update point coord.
         FPtDraggedByMouse^.x := PaintBoxXToPercentX(X);
         FPtDraggedByMouse^.Y := PaintBoxYToPercentY(Y);
         FCurveChanged := TRUE;
         PB.Invalidate ;
        end
   else begin // mouse don't drag a point
         if PointIndexOverMouse( X, Y ) <> -1
           then PB.Cursor := crHandPoint
           else PB.Cursor := crDefault;
         FCurveChanged:=FALSE;
        end;
end;

procedure TGraphEditor.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then begin
    FPtDraggedByMouse := NIL;
    PB.Cursor := crDefault;
    if FCurveChanged and (FUpdateCallBack<>NIL)
      then FUpdateCallBack(FList);
    FCurveChanged:=FALSE;
  end;
end;

procedure TGraphEditor.PBPaint(Sender: TObject);
var i : integer;
    x1,y1, x2,y2: single;
    p1, p2: PGraphPoint;
begin
 if FImage=NIL then begin
    CurveColor := BGRA(255,140,0);
    FPointColor := BGRAWhite;
    FImage:= TBGRABitmap.Create(PB.Width, PB.Height);
    FImage.FontAntialias:=TRUE;
    FImage.FontHeight := 12;
   end else if (FImage.Width<>PB.Width) or (FImage.Height<>PB.Height)
              then FImage.SetSize(PB.Width, PB.Height);
 with FImage do
  begin
   // render background
   if FOnDrawBackground<>NIL
     then FOnDrawBackground( FImage )  // user background
     else begin
        Fill(BGRA(50,20,20));          // native background
        Rectangle(0, 0, 1, 1, BGRA(90,90,90), BGRA(50,20,20),dmSet);
        DrawLineAntialias( 1, Height shr 1, Width-2, Height shr 1, BGRA(60,40,40), 1);
        DrawLineAntialias( Width shr 1, 1, Width shr 1, Height -2, BGRA(60,40,40), 1);
   end;

   // render grid
   if FShowGrid then begin
      i:=0;
      repeat
       inc(i, FGridStep);
       DrawLineAntialias( 0, i, Width-1, i, BGRA(60,40,40), 1);
       DrawLineAntialias( i, 0, i, Height-1, BGRA(60,40,40), 1);
      until (i>Width) and (i>Height);
   end;

   if FList.Count>0 then begin
     // render curve
     p1 := GetPointByIndex(0);
     for i:=1 to FList.Count-1 do
      begin
       x1 := PercentXToPaintBoxX( p1^.x );
       y1 := PercentYToPaintBoxY( p1^.y );
       p2 := GetPointByIndex(i);
       x2 := PercentXToPaintBoxX( p2^.x );
       y2 := PercentYToPaintBoxY( p2^.y );
       DrawLineAntialias(x1,y1,x2,y2, FLineColor, 1.5 );
       p1 := p2
      end;

     // render points
     if FShowPoint then
       for i:=0 to FList.Count-1 do begin
        p1 := GetPointByIndex(i);
        x1 := PercentXToPaintBoxX(p1^.x);
        y1 := PercentYToPaintBoxY(p1^.y);
        FillRectAntialias(x1-HALF_WIDTH_POINT , y1-HALF_WIDTH_POINT,
                          x1+HALF_WIDTH_POINT , y1+HALF_WIDTH_POINT, FPointColor );
     end;

     for i:=0 to FList.Count-1 do begin
      p1 := GetPointByIndex(i);
      x1 := PercentXToPaintBoxX(p1^.x);
      y1 := PercentYToPaintBoxY(p1^.y);
      TextOut(x1,y1,inttostr(i),BGRABlack, taCenter);
     end;


    end;
  end;
 FImage.Draw( PB.Canvas, 0, 0, TRUE );
end;

procedure TGraphEditor.DoClear;
var i: integer;
begin
 for i:=0 to FList.Count-1 do Dispose( GetPointByIndex(i) );
 FList.Clear;
end;

function TGraphEditor.GetPointByIndex(Index: integer): PGraphPoint;
begin
 Result := PGraphPoint(FList.Items[Index]);
end;

function TGraphEditor.PointIndexOverMouse(X, Y: integer): integer;
var i: integer;
begin
 Result := -1;
 for i:=FList.Count-1 downto 0 do begin
  if PtsIsMousePointed( GetPointByIndex(i), X, Y ) then begin
    Result := i;
    exit;
  end;
 end;
end;

function TGraphEditor.PtsIsMousePointed(const APts: PGraphPoint; X, Y: Integer
  ): boolean;
var p: TPointF;
begin
 p.x := PercentXToPaintBoxX(APts^.x);
 p.y := PercentYToPaintBoxY(APts^.y);
 Result:= ( X >= p.x-HALF_WIDTH_POINT) and
          ( X <= p.x+HALF_WIDTH_POINT) and
          ( Y >= p.y-HALF_WIDTH_POINT) and
          ( Y <= p.y+HALF_WIDTH_POINT);
end;

function TGraphEditor.PercentXToPaintBoxX(aValue: single): single;
begin
 Result := aValue*(PB.Width-1);
end;

function TGraphEditor.PercentYToPaintBoxY(aValue: single): single;
begin
 Result := aValue*PB.Height;
end;

function TGraphEditor.PaintBoxXToPercentX(aValue: single): single;
begin
 Result := aValue/(PB.Width-1);
end;

function TGraphEditor.PaintBoxYToPercentY(aValue: single): single;
begin
 Result := aValue/PB.Height;
end;

procedure TGraphEditor.SetShowGrid(aValue: boolean);
begin
 FShowGrid := aValue;
 PB.Invalidate;
end;

procedure TGraphEditor.SetShowPoint(AValue: boolean);
begin
  if FShowPoint=AValue then Exit;
  FShowPoint:=AValue;
  PB.Invalidate;
end;

procedure TGraphEditor.SetPointColor(AValue: TBGRAPixel);
begin
  if FPointColor=AValue then Exit;
  FPointColor:=AValue;
  PB.Invalidate;
end;

procedure TGraphEditor.SetLineColor(AValue: TBGRAPixel);
begin
  if FLineColor=AValue then Exit;
  FLineColor:=AValue;
  PB.Invalidate;
end;

function TGraphEditor.GetPointCount: integer;
begin
 Result := FList.Count;
end;

procedure TGraphEditor.SetPaintBoxTarget(aPB: TPaintBox);
begin
 if PB<>NIL then begin
   PB.OnMouseDown := NIL;
   PB.OnMouseUp := NIL;
   PB.OnMouseMove := NIL;
   PB.OnPaint:=NIL;
 end;
 PB := aPB;
 PB.OnMouseDown := @PBMouseDown;
 PB.OnMouseUp := @PBMouseUp;
 PB.OnMouseMove := @PBMouseMove;
 PB.OnPaint := @PBPaint;
 PB.Invalidate;
end;

procedure TGraphEditor.SetGridStep(aValue: integer);
begin
 if aValue < 2 then aValue := 2;
 FGridStep := aValue;
 PB.Invalidate;
end;

procedure TGraphEditor.Clear;
begin
 DoClear;
 PB.Invalidate;
end;

function TGraphEditor.AddPoint(aXPercent, aYPercent: single; aSelectIt: boolean): integer;
var o: PGraphPoint;
begin
  aXPercent := EnsureRange(aXPercent, 0.0, 1.0);
  aYPercent := EnsureRange(aYPercent, 0.0, 1.0);

  new(o);
  o^ := PointF(aXPercent, aYPercent);

   Result := FList.Add(o);

  if aSelectIt then FPtDraggedByMouse := o
               else FPtDraggedByMouse := NIL;
  PB.Invalidate;
end;

procedure TGraphEditor.DeletePointByIndex(aIndex: integer);
begin
  if (aIndex < 0) or (aIndex>FList.Count-1) then exit;

  Dispose(GetPointByIndex(aIndex));
  FList.Delete(aIndex);

  PB.Invalidate;
end;

procedure TGraphEditor.DeletePoint(aPoint: PGraphPoint);
begin
  DeletePointByIndex(FList.IndexOf(aPoint));
end;

procedure TGraphEditor.SaveToFile(const aFilename: string);
var s: TStringList;
    txt: string;
    i: integer;
    p:PGraphPoint;
begin
 s := TStringList.Create;
 try
   s.Add('[OGLC_PATH]');
   s.Add(inttostr(FList.Count));
   for i:=0 to FList.Count-1 do begin
    p := GetPointByIndex(i);
    txt := FormatFloatWithDot('0.000', p^.x)+' '+FormatFloatWithDot('0.000', p^.y);
    s.Add(txt);
   end;
   s.SaveToFile( aFilename );
 finally
   s.Free;
 end;
end;

procedure TGraphEditor.LoadFromFile(const aFilename: string);
var s: TStringList;
    SplittedTxt: ArrayOfString;
    c, k: integer;
begin
 s := TStringList.Create;
 try
  s.LoadFromFile( aFilename );
  k := s.IndexOf('[OGLC_PATH]');
  if k = -1
    then Exception.Create('File '+ExtractFileName(aFilename)+' bad format...')
    else FDrawingName := s.Strings[k+1];

  c := strtoint( s.Strings[k+1] );
  Clear;
  repeat
    inc(k);
    SplittedTxt := s.Strings[k].Split([' ']);
    AddPoint(StringToSingle(SplittedTxt[0]), StringToSingle(SplittedTxt[1]), FALSE);
    dec(c);
  until c=0;

 finally
   s.Free;
 end;
end;

procedure TGraphEditor.MovePoints(aDeltaX, aDeltaY: integer);
var i: integer;
    p: TPointF;
begin
 for i:=0 to FList.Count-1 do
  with GetPointByIndex(i)^ do begin

   p.x := PercentXToPaintBoxX(x) + aDeltaX;
   if p.x>PB.Width-1
     then p.x := p.x-PB.Width-1
   else if p.x<0 then p.x := p.x+PB.Width-1;

   p.y := PercentYToPaintBoxY(y) + aDeltaY;
   if p.y>PB.Height-1
     then p.y := p.y-PB.Height-1
   else if p.y<0 then p.y := p.y+PB.Height-1;

   x := PaintBoxXToPercentX(p.x);
   y := PaintBoxYToPercentY(p.y);
  end;
 PB.Invalidate;
end;


end.

