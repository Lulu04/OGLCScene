unit frame_gradientrow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Dialogs, Spin,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  Math;

const GRID_SIZE = 20;
      HALF_WIDTH_POINT = 5 ;

type

  TGradientMouseState = (gmsIdle,

                         gmsMouseOverNode,
                         gmsMouseDownOnNode,
                         gmsMovingNode,

                         gmsMouseDownOnNodeOpacity,
                         gmsChangingOpacity);

  TOnNodeModified = procedure(Sender: TObject; aNodeIndex: integer) of object;
  TOnDeleteNode = procedure(Sender: TObject; aNodeIndex: integer) of object;
  TOnAddNode = procedure(Sender: TObject; aNodeIndex: integer; {aXPosition: single;} const aColor: TBGRAPixel) of object;

  { TFrameGradientRow }

  TFrameGradientRow = class(TFrame)
    CD1: TColorDialog;
    FSE1: TFloatSpinEdit;
    Panel1: TPanel;
    PB: TPaintBox;
    procedure FSE1Change(Sender: TObject);
    procedure FSE1Enter(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PBPaint(Sender: TObject);
  private
    FClickOrigin: TPoint;
    FWorkingNodeIndex: integer;
    procedure SetMouseState(AValue: TGradientMouseState);
    procedure DoLoopMoveNode;
    procedure DoLoopChangeOpacity;
  private
    FIndex: integer;
    FNodes: PArrayOfGradientItem;
    FImage: TBGRABitmap;
    FOnNodeModified: TOnNodeModified;
    FPPreviousPts,
    FPNextPts,
    FPCurrentPts: PGradientItem;
    CurveColor: TBGRAPixel;
    FSelected: boolean;
    HandleColor: TBGRAPixel;
    FCurveChanged: boolean;
    FOnChangeCallBack: TNotifyEvent;
    FOnDeleteNode: TOnDeleteNode;
    FOnAddNode: TOnAddNode;
    FInitializing: boolean;
    FMouseState: TGradientMouseState;
    procedure FireOnChangeEvent;
    procedure FireEventOnNodeModified(aNodeIndex: integer);
    function GetNodeIndexAt(aX: integer): integer;
    function GetYPosition: single;
    function PointIsUnderMouse(const APts: TPointF; X, Y: Integer): boolean;
    function PercentXToPaintBoxX(aValue: single): single;
    function PercentYToPaintBoxY(aValue: single): single;
    function PaintBoxXToPercentX(aValue: single): single;
    function PaintBoxYToPercentY(aValue: single): single;
    procedure SetPreviousCurrentNextPts(aIndex: integer);
    procedure ClearPreviousCurrentNext;
    procedure SetSelected(AValue: boolean);
    procedure DoSelectSelf;
    property MouseState: TGradientMouseState read FMouseState write SetMouseState;
    property ClickOrigin: TPoint read FClickOrigin write FClickOrigin;
  public
   destructor Destroy; override;
   procedure EraseBackground({%H-}DC: HDC); override;
   procedure Clear; // remove all points
   // aTimePercent [0..1] is a percentage of particle life
   // aPercentValue [0..1] is Value/MaxValue
   function AddPoint(aXPercent: single; aColor: TBGRAPixel; aSelectIt: boolean=FALSE): integer;

   procedure Edit(aYPosition: single; aItems: PArrayOfGradientItem);
   procedure SetSameColorOnAllNodes(aColor: TBGRAPixel);

   property OnCurveChange: TNotifyEvent read FOnChangeCallBack write FOnChangeCallBack;
   property OnNodeModified: TOnNodeModified read FOnNodeModified write FOnNodeModified;

   property OnAddNode: TOnAddNode read FOnAddNode write FOnAddNode;
   property OnDeleteNode: TOnDeleteNode read FOnDeleteNode write FOnDeleteNode;

   property Index: integer read FIndex write FIndex;
   property Selected: boolean read FSelected write SetSelected;
   property YPosition: single read GetYPosition;
   property Nodes: PArrayOfGradientItem read FNodes write FNodes;
  end;

implementation

uses u_app_pref, form_editgradient, u_common;

{ TFrameGradientRow }

procedure TFrameGradientRow.PBMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, nodeIndexUnderMouse: integer;
begin
  FCurveChanged := FALSE;
  nodeIndexUnderMouse := GetNodeIndexAt(X);
  ClickOrigin := Point(X, Y);

  if not Selected then
    DoSelectSelf;

  case MouseState of
    gmsIdle: begin
     // left click on curve -> add point
     if (Button = mbLeft) and (nodeIndexUnderMouse = -1) then begin
       i := AddPoint(PaintBoxXToPercentX(X), BGRAWhite, True);
       FCurveChanged := TRUE;
       FOnAddNode(Self, i, FNodes^[i].Color);
       PB.Invalidate;
       MouseState := gmsMouseOverNode;
       exit;
     end;
     // left click on point -> select points clicked
     if (Button = mbLeft) and (nodeIndexUnderMouse <> -1) and not (ssDouble in Shift) then begin
       SetPreviousCurrentNextPts(nodeIndexUnderMouse);
       PB.Invalidate;
       MouseState := gmsMouseDownOnNode;
       exit;
     end;
    end;

    gmsMouseOverNode: begin
     // double left click on point
     if (Button = mbLeft) and (nodeIndexUnderMouse <> -1) and (ssDouble in Shift) then begin
       CD1.Color := FNodes^[nodeIndexUnderMouse].Color;
       if CD1.Execute then begin
         FNodes^[nodeIndexUnderMouse].Color := ColorToBGRA(CD1.Color);
         PB.Invalidate;
         FOnNodeModified(Self, nodeIndexUnderMouse);
         //FireOnChangeEvent;
       end;
       exit;
     end
     else
     if ssCtrl in Shift then MouseState := gmsMouseDownOnNodeOpacity
       else MouseState := gmsMouseDownOnNode;

    // right click on point -> delete it
    if (Button = mbRight) and (nodeIndexUnderMouse <> -1) and
       (nodeIndexUnderMouse <> 0) and (nodeIndexUnderMouse <> High(FNodes^))  then begin
      system.Delete(FNodes^, nodeIndexUnderMouse, 1);
      FOnDeleteNode(Self, nodeIndexUnderMouse);
      //FireOnChangeEvent;
      ClearPreviousCurrentNext;
      PB.Invalidate;
      exit;
    end;
    end;

  end;//case

end;

procedure TFrameGradientRow.FSE1Change(Sender: TObject);
begin
  if FInitializing then exit;
  FireOnChangeEvent;
end;

procedure TFrameGradientRow.FSE1Enter(Sender: TObject);
begin
  DoSelectSelf;
end;

procedure TFrameGradientRow.Panel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button := Button;
  Shift := Shift;
  X := X;
  Y := Y;
  DoSelectSelf;
end;

procedure TFrameGradientRow.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer );
var nodeIndexUnderMouse: integer;
    thresholdDone: boolean;
begin
  Shift := Shift;

  nodeIndexUnderMouse := GetNodeIndexAt(X);
  thresholdDone := Distance(PointF(ClickOrigin), PointF(X, Y)) > PPIScale(3);

  case MouseState of
    gmsIdle: begin
      if nodeIndexUnderMouse <> -1 then
        MouseState := gmsMouseOverNode;
    end;
    gmsMouseOverNode: begin
     if nodeIndexUnderMouse = -1 then MouseState := gmsIdle;
    end;

    gmsMouseDownOnNode: if thresholdDone then begin
      FWorkingNodeIndex := nodeIndexUnderMouse;
      DoLoopMoveNode;
    end;
    gmsMouseDownOnNodeOpacity: if thresholdDone then begin
      FWorkingNodeIndex := nodeIndexUnderMouse;
      DoLoopChangeOpacity;
    end;
  end;//case

{  if FPCurrentPts <> NIL then begin
    // force current point to be between the previous and next
    if FPPreviousPts <> NIL then begin
      xx := Round(PercentXToPaintBoxX(FPPreviousPts^.XPosition));
      if X <= xx + PPIScale(HALF_WIDTH_POINT) then X := xx + PPIScale(HALF_WIDTH_POINT);
    end else X := 0;
    if FPNextPts <> NIL then begin
      xx := round(PercentXToPaintBoxX(FPNextPts^.XPosition));
      if X >= xx-PPIScale(HALF_WIDTH_POINT) then X := xx-PPIScale(HALF_WIDTH_POINT);
    end else X := PB.Width - 1;
    if X < 0 then X := 0;
    if X > PB.Width-1 then X := PB.Width-1;
    // update point
    FPCurrentPts^.XPosition := PaintBoxXToPercentX(X);
    FCurveChanged := TRUE;
    PB.Invalidate;
  end else begin // no point locked by mouse
    if nodeIndexUnderMouse <> -1 then PB.Cursor := crHandPoint
      else PB.Cursor := crDefault;
    FCurveChanged := FALSE;
  end;  }
end;

procedure TFrameGradientRow.PBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var nodeIndexUnderMouse: integer;
begin
  Button := Button;
  Shift := Shift;
  Y := Y;

  nodeIndexUnderMouse := GetNodeIndexAt(X);

  case MouseState of
    gmsMouseDownOnNode,
    gmsMovingNode,
    gmsMouseDownOnNodeOpacity,
    gmsChangingOpacity: if nodeIndexUnderMouse <> -1 then MouseState := gmsMouseOverNode
                          else MouseState := gmsIdle;
  end;//case

{  if (Button = mbLeft) or (Button = mbRight) then begin
    ClearPreviousCurrentNext;
    if FCurveChanged then FireOnChangeEvent;
    FCurveChanged := FALSE;
  end;  }
end;

procedure TFrameGradientRow.SetPreviousCurrentNextPts(aIndex: integer);
begin
  FPCurrentPts := @FNodes^[aIndex];

  if aIndex = 0 then FPPreviousPts := NIL
    else FPPreviousPts := @FNodes^[aIndex-1];

  if aIndex = Length(FNodes^)-1 then FPNextPts := NIL
    else FPNextPts := @FNodes^[aIndex+1];
end;

procedure TFrameGradientRow.ClearPreviousCurrentNext;
begin
  FPCurrentPts := NIL;
  FPPreviousPts := NIL;
  FPNextPts := NIL;
end;

procedure TFrameGradientRow.SetSelected(AValue: boolean);
begin
  if FSelected = AValue then Exit;
  FSelected := AValue;
  PB.Invalidate;
end;

procedure TFrameGradientRow.DoSelectSelf;
begin
  FormEditGradient.SelectNone;
  Selected := True;
end;

destructor TFrameGradientRow.Destroy;
begin
  if FImage <> NIL then FImage.Free;
  FImage := NIL;
  inherited Destroy;
end;

procedure TFrameGradientRow.EraseBackground(DC: HDC);
begin
end;

procedure TFrameGradientRow.PBPaint(Sender: TObject);
var i: integer;
    xx, yy: single;
    c: TBGRAPixel;
    procedure RenderNode(aNodeIndex: integer);
    begin
      if FNodes^[aNodeIndex].Color.Lightness > 32768 then c := BGRA(50,50,50)
        else c := BGRAWhite;
      xx := PercentXToPaintBoxX(FNodes^[aNodeIndex].XPosition);
      //if xx = 0 then xx := PPIScale(HALF_WIDTH_POINT)
      //else if xx = PB.Width-1 then xx := PB.Width-1-PPIScale(HALF_WIDTH_POINT);
      FImage.RectangleAntialias(xx-PPIScale(HALF_WIDTH_POINT), 0,
                         xx+PPIScale(HALF_WIDTH_POINT), Height,
                         c, 1, BGRAPixelTransparent);
      yy := Height - Height*FNodes^[aNodeIndex].Color.alpha*OneDiv255;
      FImage.RectangleAntialias(xx-PPIScale(HALF_WIDTH_POINT), yy,
                         xx+PPIScale(HALF_WIDTH_POINT), Height,
                         c, 1, FNodes^[aNodeIndex].Color);
    end;

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
   if Selected then Fill(BGRA(0,128,255))
     else Fill(BGRA(50,20,20));

   yy := Height*0.5;
   // horizontal axis
   DrawLineAntialias(1, yy, Width-2, yy, BGRA(90,60,60), 1.5);

   // first render last point to avoid the second to last to be hidden
   RenderNode(High(FNodes^));
   for i:=0 to High(FNodes^)-1 do
    RenderNode(i);

  end;
  FImage.Draw(PB.Canvas, 0, 0, TRUE);
end;

procedure TFrameGradientRow.SetMouseState(AValue: TGradientMouseState);
begin
  if FMouseState = AValue then Exit;
  FMouseState := AValue;
  case AValue of
    gmsIdle: PB.Cursor := crDefault;

    gmsMouseOverNode,
    gmsMouseDownOnNode,
    gmsMovingNode: PB.Cursor := crHandPoint;

    gmsChangingOpacity: PB.Cursor := crSizeNS;
  end;
end;

procedure TFrameGradientRow.DoLoopMoveNode;
var current, delta: TPoint;
    newPos, marg: single;
begin
  if (FWorkingNodeIndex = -1) or
     (FWorkingNodeIndex = 0) or
     (FWorkingNodeIndex = High(FNodes^)) or
     (MouseState = gmsMovingNode) then exit;

  MouseState := gmsMovingNode;

  SetPreviousCurrentNextPts(FWorkingNodeIndex);
  marg := PPIScale(HALF_WIDTH_POINT)/PB.Width * 2;
  repeat
    current := PB.ScreenToClient(Mouse.CursorPos);
    delta := current - ClickOrigin;
    if delta.x <> 0 then begin
      //newPos := EnsureRange((ClickOrigin.x+delta.x)/PB.Width, PPIScale(HALF_WIDTH_POINT)/PB.Width, (PB.Width-PPIScale(HALF_WIDTH_POINT))/PB.Width);
      newPos := (ClickOrigin.x + delta.x) / PB.Width;
      newPos := EnsureRange(newPos, FPPreviousPts^.XPosition + marg, FPNextPts^.XPosition - marg);
      FNodes^[FWorkingNodeIndex].XPosition := newPos;
      ClickOrigin := current;
      FireOnChangeEvent;
      FireEventOnNodeModified(FWorkingNodeIndex);
      PB.Invalidate;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> gmsMovingNode;



{  if FPCurrentPts <> NIL then begin
    // force current point to be between the previous and next
    if FPPreviousPts <> NIL then begin
      xx := Round(PercentXToPaintBoxX(FPPreviousPts^.XPosition));
      if X <= xx + PPIScale(HALF_WIDTH_POINT) then X := xx + PPIScale(HALF_WIDTH_POINT);
    end else X := 0;
    if FPNextPts <> NIL then begin
      xx := round(PercentXToPaintBoxX(FPNextPts^.XPosition));
      if X >= xx-PPIScale(HALF_WIDTH_POINT) then X := xx-PPIScale(HALF_WIDTH_POINT);
    end else X := PB.Width - 1;
    if X < 0 then X := 0;
    if X > PB.Width-1 then X := PB.Width-1;
    // update point
    FPCurrentPts^.XPosition := PaintBoxXToPercentX(X);
    FCurveChanged := TRUE;
    PB.Invalidate;
  end  }
end;

procedure TFrameGradientRow.DoLoopChangeOpacity;
var current, delta: TPoint;
begin
  if (FWorkingNodeIndex = -1) or
     (MouseState = gmsChangingOpacity) then exit;

  MouseState := gmsChangingOpacity;

  SetPreviousCurrentNextPts(FWorkingNodeIndex);
  repeat
    current := PB.ScreenToClient(Mouse.CursorPos);
    delta := current - ClickOrigin;
    if delta.y <> 0 then begin
      FNodes^[FWorkingNodeIndex].Color.alpha := EnsureRange(255-Round((ClickOrigin.y+delta.y)/PB.Height*255), 0, 255);
      ClickOrigin := current;
      FireOnChangeEvent;
      FireEventOnNodeModified(FWorkingNodeIndex);
      PB.Invalidate;
    end;
    Application.ProcessMessages;
    FScene.DoLoop;
  until MouseState <> gmsChangingOpacity;
end;

procedure TFrameGradientRow.FireOnChangeEvent;
begin
  if FOnChangeCallBack <> NIL then
    FOnChangeCallBack(Self);
end;

procedure TFrameGradientRow.FireEventOnNodeModified(aNodeIndex: integer);
begin
  if FOnNodeModified <> NIL then
    FOnNodeModified(Self, aNodeIndex);
end;

function TFrameGradientRow.GetNodeIndexAt(aX: integer): integer;
var i, xPB: integer;
begin
  for i:=0 to Length(FNodes^)-1 do begin
    xPB := Round(PercentXToPaintBoxX(FNodes^[i].XPosition));
    if (aX >= xPB-PPIScale(HALF_WIDTH_POINT)) and (aX <= xPB+PPIScale(HALF_WIDTH_POINT)) then
      exit(i);
  end;
  Result := -1;
end;

function TFrameGradientRow.GetYPosition: single;
begin
  Result := FSE1.Value;
end;

function TFrameGradientRow.PointIsUnderMouse(const APts: TPointF; X, Y: Integer): boolean;
var p: TPointF;
begin
  p.x := PercentXToPaintBoxX(APts.x);
  p.y := PercentYToPaintBoxY(APts.y);
  Result:= (X >= p.x-PPIScale(HALF_WIDTH_POINT)) and
           (X <= p.x+PPIScale(HALF_WIDTH_POINT)) and
           (Y >= p.y-PPIScale(HALF_WIDTH_POINT)) and
           (Y <= p.y+PPIScale(HALF_WIDTH_POINT));
end;

function TFrameGradientRow.PercentXToPaintBoxX(aValue: single): single;
begin
  Result := aValue * (PB.Width-1);
end;

function TFrameGradientRow.PercentYToPaintBoxY(aValue: single): single;
begin
  Result := PB.Height - aValue*PB.Height;
end;

function TFrameGradientRow.PaintBoxXToPercentX(aValue: single): single;
begin
  Result := aValue/(PB.Width-1);
end;

function TFrameGradientRow.PaintBoxYToPercentY(aValue: single): single;
begin
  Result := (PB.Height-aValue)/PB.Height;
end;

procedure TFrameGradientRow.Clear;
begin
  SetLength(FNodes^, 0);
  PB.Invalidate;
end;

function TFrameGradientRow.AddPoint(aXPercent: single; aColor: TBGRAPixel; aSelectIt: boolean): integer;
var i, j: integer;
begin
  aXPercent := EnsureRange(aXPercent, 0.0, 1.0);

  SetLength(FNodes^, Length(FNodes^)+1);
  i := 0;
  if Length(FNodes^) > 1 then begin
    repeat
      j := i;
      if aXPercent = FNodes^[i].XPosition then begin
        FNodes^[i].Color := aColor;        // the point already exists. we change only its value
        exit;
      end else if aXPercent > FNodes^[i].XPosition then inc(i);
    until (i = j) or (i = Length(FNodes^)-1);

    if i < Length(FNodes^)-1
      then for j:=Length(FNodes^)-2 downto i do
             FNodes^[j+1] := FNodes^[j];
  end;
  FNodes^[i].XPosition := aXPercent;
  FNodes^[i].Color := aColor;
  Result := i;
  if aSelectIt then SetPreviousCurrentNextPts(i)
    else ClearPreviousCurrentNext;
  PB.Invalidate;
end;

procedure TFrameGradientRow.Edit(aYPosition: single; aItems: PArrayOfGradientItem);
begin
  FInitializing := True;
  FSE1.Value := aYPosition;
  FNodes := aItems;
  // check if the row is empty -> fill it with two points
  if Length(FNodes^) < 2 then begin
    SetLength(FNodes^, 2);
    FNodes^[0].XPosition := 0.0;
    FNodes^[0].Color := BGRAWhite;
    FNodes^[1].XPosition := 1.0;
    FNodes^[1].Color := BGRAWhite;
  end;
  FInitializing := False;
end;

procedure TFrameGradientRow.SetSameColorOnAllNodes(aColor: TBGRAPixel);
var i: integer;
begin
  for i:=0 to High(FNodes^) do
    FNodes^[i].Color := aColor;
  PB.Invalidate;
end;


{$R *.lfm}

end.

