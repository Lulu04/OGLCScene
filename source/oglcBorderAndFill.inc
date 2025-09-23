{$ifdef oglcINTERFACE}
type

{ TOGLCFill }

TOGLCFill = record
private
  FBlendMode: byte;
  FVertex: TArrayOfxycolor;
  FIndices: TArrayOfVertexIndex;
  FColor, FCenterColor: TBGRAPixel;
  FCenterColorUseColor: boolean;
  FVisible, FMaskVisible: boolean;
  FCenter: TPointF;
  function GetVisible: boolean;
  procedure SetCenterColor(AValue: TBGRAPixel);
  procedure SetColor(AValue: TBGRAPixel);
  procedure ClearArrays;
public
  Tint: TBGRAPixel;
  procedure InitDefault;
  procedure InitFromPath(const aPath: ArrayOfTPointF; const aCenter: TPointF);
  function VertexCount: ptrint;
  function IndiceCount: ptrint;
  procedure CopyVertexTo(p: Pxycolor);
  procedure CopyIndexTo(p: PVertexIndex);
  property CenterColor: TBGRAPixel read FCenterColor write SetCenterColor;
  property Color: TBGRAPixel read FColor write SetColor;
  property BlendMode: byte read FBlendMode write FBlendMode;
  property Visible: boolean read GetVisible write FVisible;
end;
POGLCFill = ^TOGLCFill;

{ TOGLCBorder }

TOGLCBorder = record
private const VERTEX_PER_SEGMENT = 6;
              INDICE_PER_SEGMENT = 12;
private
  FBlendMode: byte;
  FBorderPosition: TOGLCLinePosition;
  FPath: TOGLCPath;
  FVertices: TArrayOfLineVertexAttributes;
  FIndices: TArrayOfVertexIndex;
  FColor: TBGRAPixel;
  FVisible, FMaskVisible: boolean;
  FLineWidth: single;
  FThreshold1, FThreshold2: single;
  function GetVisible: boolean;
  procedure SetBorderPosition(AValue: TOGLCLinePosition);
  procedure SetColor(AValue: TBGRAPixel);
  procedure SetLineWidth(AValue: single);
  function BorderWidthToThreshold(aWidth: single): single;
  procedure InitArrays;
  procedure ClearArrays;
public
  Tint: TBGRAPixel;
  procedure InitDefault;
  procedure InitFromPath(const aPath: ArrayOfTPointF; w: single);
  function VertexCount: ptrint;
  function IndexCount: ptrint;
  procedure CopyVertexTo(p: PLineVertexAttributes);
  procedure CopyIndexTo(p: PVertexIndex);
  property Width: single read FLineWidth write SetLineWidth;
  property Color: TBGRAPixel read FColor write SetColor;
  property BlendMode: byte read FBlendMode write FBlendMode;
  property LinePosition: TOGLCLinePosition read FBorderPosition write SetBorderPosition;
  property Visible: boolean read GetVisible write FVisible;
  property Path: TOGLCPath read FPath;
  property Indices: TArrayOfVertexIndex read FIndices;
  property Vertices: TArrayOfLineVertexAttributes read FVertices;
end;
POGLCBorder = ^TOGLCBorder;

TGradientItem = record
  XPosition: single;
  Color: TBGRAPixel;
end;
ArrayOfGradientItem = array of TGradientItem;
type
TGradientRow = record
  YPosition: single;
  Items: ArrayOfGradientItem;
end;
TArrayOfGradientRow = array of TGradientRow;


{ TGradientDescriptor }

TGradientDescriptor = record
private
  FUpdating: boolean;
  FOnChange: TOGLCEvent;
  procedure AddRowItem(const aItem: TGradientRow);
  function CreateArrayOfGradientItem(const aColors: ArrayOfTBGRAPixel; const aPos: ArrayOfSingle): ArrayOfGradientItem;
  function GradientRow(aYPos: single; const Items: ArrayOfGradientItem): TGradientRow;
public
  Rows: TArrayOfGradientRow;
  Vertices: TArrayOfxycolor;
  Indices: TArrayOfVertexIndex;
  procedure InitDefault;
  // clear the gradient.
  procedure Clear;

  procedure CopyFrom(const aGradient: TGradientDescriptor);

  // Initialize the gradient with a single color.
  procedure CreateSingleColor(const aColor: TBGRAPixel);
  // Initialize the gradient with a serie of colors placed horizontaly from the left to the right.
  // The XPos array contains values in range 0..1, they are the 'position' for each colors.
  procedure CreateHorizontal(const aColors: ArrayOfTBGRAPixel; const aXPos: ArrayOfSingle);
  // Initialize the gradient with a serie of colors placed verticaly from the top to the bottom.
  // The YPos array contains values in range 0..1, they are the 'position' for each colors.
  procedure CreateVertical(const aColors: ArrayOfTBGRAPixel; const aYPos: ArrayOfSingle);
  // Must be called before a call to AddRow(...).
  procedure BeginUpdate;
  // Allow to define a complex gradient. You can add as much rows as you want.
  // Constraints are:
  //     - the first row must have aYPos=0.0
  //     - each row must have the same number of entries and with the same values of XPos
  //     - each row must begin with a XPos=0.0 and finish with a XPos=1.0
  //     - the last row must have aYPos=1.0
  procedure AddRow(aYPosition: single; const aColors: ArrayOfTBGRAPixel; const aXPos: ArrayOfSingle);
  // Must be called after the last call to AddRow(...).
  procedure EndUpdate;

  // Compute the mesh using triangle strip to apply the gradient on the specified rectangular area.
  procedure ComputeVerticesAndIndices(aWidth, aHeight: integer);
  // This event is fired when the gradient colors are modified with one of the Createxxxx(...) method or AddRow(...).
  property OnChange: TOGLCEvent read FOnChange write FOnChange;
end;
PGradientDescriptor = ^TGradientDescriptor;
ArrayOfGradientDescriptor = array of TGradientDescriptor;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCFill }

procedure TOGLCFill.SetColor(AValue: TBGRAPixel);
var i: integer;
  c: TColorF;
begin
  if FColor = AValue then Exit;
  FColor := AValue;

  c.InitFromBGRA(FColor);
  if Length(FVertex) > 0 then begin
    if FCenterColorUseColor then FVertex[0].c.CopyFrom(c)
      else FVertex[0].c.InitFromBGRA(FCenterColor);

    for i:=1 to High(FVertex) do
      FVertex[i].c.CopyFrom(c);
  end;
end;

function TOGLCFill.GetVisible: boolean;
begin
  Result := FVisible and FMaskVisible;
end;

procedure TOGLCFill.SetCenterColor(AValue: TBGRAPixel);
begin
  if FCenterColor = AValue then Exit;
  FCenterColor := AValue;
  FCenterColorUseColor := False;
  if Length(FVertex) > 0 then
    FVertex[0].c.InitFromBGRA(AValue);
end;

procedure TOGLCFill.ClearArrays;
begin
  FVertex := NIL;
  FIndices := NIL;
end;

procedure TOGLCFill.InitDefault;
begin
  ClearArrays;
  FColor := BGRA(30,30,30);
  FCenterColor := FColor;
  FCenterColorUseColor := True;
  FVisible := True;
  FMaskVisible := True;
  FBlendMode := FX_BLEND_NORMAL;
end;

procedure TOGLCFill.InitFromPath(const aPath: ArrayOfTPointF; const aCenter: TPointF);
var p: Pxycolor;
  i: Integer;
  ii: TVertexIndex;
  col: TColorF;
begin
  FCenter := aCenter;
  FVertex := NIL;
  FIndices := NIL;
  if Length(aPath) = 0 then exit;

  col.InitFromBGRA(FColor);
  SetLength(FVertex, Length(aPath)+2); // TRIANGLE_FAN: center + points + first point
  SetLength(FIndices, Length(aPath)+2);
  p := @FVertex[0];
  // center
  p^.x := aCenter.x;
  p^.y := aCenter.y;
  if FCenterColorUseColor then p^.c.CopyFrom(col)
    else p^.c.InitFromBGRA(FCenterColor);
  FIndices[0] := 0;
  ii := 1;
  // points
  for i:=0 to High(aPath) do
  begin
    inc(p);
    p^.x := aPath[i].x;
    p^.y := aPath[i].y;
    p^.c.CopyFrom(col);
    FIndices[i+1] := ii; inc(ii);
  end;
  // first point to close the shape
  inc(p);
  p^.x := aPath[0].x;
  p^.y := aPath[0].y;
  p^.c.CopyFrom(col);
  FIndices[High(FIndices)] := ii;
end;

procedure TOGLCFill.CopyVertexTo(p: Pxycolor);
begin
  if Length(FVertex) > 0 then
    Move(FVertex[0], p^, SizeOf(Txycolor)*Length(FVertex));
end;

procedure TOGLCFill.CopyIndexTo(p: PVertexIndex);
begin
  if Length(FIndices) > 0 then
    Move(FIndices[0], p^, SizeOf(TVertexIndex)*Length(FIndices));
end;

function TOGLCFill.VertexCount: ptrint;
begin
  Result := Length(FVertex);
end;

function TOGLCFill.IndiceCount: ptrint;
begin
  Result := Length(FIndices);
end;

{ TOGLCBorder }

procedure TOGLCBorder.SetLineWidth(AValue: single);
begin
  if FLineWidth = AValue then Exit;
  FLineWidth := AValue;
  InitArrays;
end;

procedure TOGLCBorder.SetColor(AValue: TBGRAPixel);
var i, j: Integer;
  col: TColorF;
  p: PLineVertexAttributes;
begin
  if AValue = FColor then exit;
  FColor := AValue;
  if Length(FVertices) = 0 then exit;

  col.InitFromBGRA(AValue);
  p := @FVertices[0];
  i := 0;
  repeat
    j := VERTEX_PER_SEGMENT;
    repeat
      p^.c.CopyFrom(col);
      inc(p);
      dec(j);
    until j = 0;
    inc(i, VERTEX_PER_SEGMENT);
  until i = Length(FVertices);
end;

procedure TOGLCBorder.SetBorderPosition(AValue: TOGLCLinePosition);
begin
  if FBorderPosition = AValue then Exit;
  FBorderPosition := AValue;
  InitArrays;
end;

function TOGLCBorder.GetVisible: boolean;
begin
  Result := FVisible and FMaskVisible;
end;

function TOGLCBorder.BorderWidthToThreshold(aWidth: single): single;
var eq: TStraightLineEquation;
begin
  if aWidth >= 30 then begin
    Result := 0.96;
    exit;
  end
  else if aWidth >= 20 then begin Result:=0.94; exit; end
  else if aWidth >= 15 then
    eq.InitFrom(PointF(15, 0.9), PointF(20, 0.94))
  else if aWidth >= 10 then
    eq.InitFrom(PointF(10, 0.8), PointF(15, 0.9))
  else if aWidth >= 5 then
    eq.InitFrom(PointF(5, 0.6), PointF(10, 0.8))
  else if aWidth >= 2 then
    eq.InitFrom(PointF(2, 0.2), PointF(5, 0.6))
  else begin
    Result := 0.09;
    exit;
  end;
  Result := eq.a*aWidth+eq.b;
end;

procedure TOGLCBorder.InitDefault;
begin
  ClearArrays;
//  FLoop := False;
  FColor := BGRA(220,220,220);
  FLineWidth := 1.5;
  FThreshold1 := BorderWidthToThreshold(FLineWidth*0.5);
  FThreshold2 := FThreshold1;
  FVisible := True;
  FMaskVisible := True;
end;

procedure TOGLCBorder.InitFromPath(const aPath: ArrayOfTPointF; w: single);
begin
  FPath := Copy(aPath);
  FLineWidth := w;
  InitArrays;
end;

procedure TOGLCBorder.InitArrays;
var p: PLineVertexAttributes;
  pIndice: PVertexIndex;
  currentIndice: TVertexIndex;
  i, j, quadCount: Integer;
  col: TColorF;
  quad1, quad2: TQuadF;
  width1, width2: single;
    function DoLineSegmentToQuadF(const pt1, pt2: TPointF): TQuadF;
    begin
      case FBorderPosition of
        lpMiddle: Result := LineSegmentToQuadF_PosMiddle(pt1, pt2, width1, width2);
        lpInside: Result := LineSegmentToQuadF_PosInside(pt1, pt2, width1, width2);
        else raise exception.create('you forgot to implement this !');
      end;
    end;

    procedure InitVerticesAndIndicesForQuad1;
    begin
      case FBorderPosition of
        lpMiddle: begin
          // path point pt1 and pt2
          p^.x := FPath[i].x; p^.y := FPath[i].y; p^.distance := 0; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := FPath[i+1].x; p^.y := FPath[i+1].y; p^.distance := 0; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          // quad1 topleft, topright, bottomright, bottomleft
          p^.x := quad1[cTL].x; p^.y := quad1[cTL].y; p^.distance := 1; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cTR].x; p^.y := quad1[cTR].y; p^.distance := 1; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cBR].x; p^.y := quad1[cBR].y; p^.distance := 1; p^.threshold := FThreshold2; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cBL].x; p^.y := quad1[cBL].y; p^.distance := 1; p^.threshold := FThreshold2; p^.c.CopyFrom(col); inc(p);
          // indices for triangle 1
          pIndice[0] := currentIndice;
          pIndice[1] := currentIndice+1;
          pIndice[2] := currentIndice+2;
          // indices for triangle 2
          pIndice[3] := currentIndice+1;
          pIndice[4] := currentIndice+3;
          pIndice[5] := currentIndice+2;
          // indices for triangle 3
          pIndice[6] := currentIndice;
          pIndice[7] := currentIndice+1;
          pIndice[8] := currentIndice+5;
          // indices for triangle 4
          pIndice[9] := currentIndice+1;
          pIndice[10] := currentIndice+4;
          pIndice[11] := currentIndice+5;

          inc(currentIndice, VERTEX_PER_SEGMENT);
          inc(pIndice, INDICE_PER_SEGMENT);
        end;

        lpInside: begin
          // path point pt1 and pt2
          p^.x := FPath[i].x; p^.y := FPath[i].y; p^.distance := 1; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := FPath[i+1].x; p^.y := FPath[i+1].y; p^.distance := 1; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          // quad1 topleft, topright, bottomright, bottomleft
          p^.x := quad1[cTL].x; p^.y := quad1[cTL].y; p^.distance := 0; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cTR].x; p^.y := quad1[cTR].y; p^.distance := 0; p^.threshold := FThreshold1; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cBR].x; p^.y := quad1[cBR].y; p^.distance := 1; p^.threshold := FThreshold2; p^.c.CopyFrom(col); inc(p);
          p^.x := quad1[cBL].x; p^.y := quad1[cBL].y; p^.distance := 1; p^.threshold := FThreshold2; p^.c.CopyFrom(col); inc(p);
          // indices for triangle 1
          pIndice[0] := currentIndice+2;
          pIndice[1] := currentIndice;
          pIndice[2] := currentIndice+3;
          // indices for triangle 2
          pIndice[3] := currentIndice+3;
          pIndice[4] := currentIndice+1;
          pIndice[5] := currentIndice;
          // indices for triangle 3
          pIndice[6] := currentIndice+2;
          pIndice[7] := currentIndice+3;
          pIndice[8] := currentIndice+4;
          // indices for triangle 4
          pIndice[9] := currentIndice+2;
          pIndice[10] := currentIndice+5;
          pIndice[11] := currentIndice+4;

          inc(currentIndice, VERTEX_PER_SEGMENT);
          inc(pIndice, INDICE_PER_SEGMENT);
        end;
        else raise exception.create('hey! you forgot to implement this !');
      end;
    end;
begin
  FVertices := NIL;
  FIndices := NIL;
  if (FLineWidth <= 0) or (Length(FPath) < 2) then exit;

  col.InitFromBGRA(FColor);
  case FBorderPosition of
    lpMiddle: begin
      width1 := FLineWidth * 0.5;
      width2 := width1;
      FThreshold1 := BorderWidthToThreshold(width1);
      FThreshold2 := BorderWidthToThreshold(width2);
    end;
    lpInside: begin
      if FLineWidth > 3 then width1 := 1.5 else width1 := FLineWidth*0.5;
      width2 := Max(1.0, FLineWidth - width1);   // at least 1 pixel
      FThreshold1 := BorderWidthToThreshold(width1);
      FThreshold2 := BorderWidthToThreshold(width2);
    end;
  end;

  currentIndice := 0;
  if Length(FPath) = 2 then begin
    // there area only 2 points
    SetLength(FVertices, VERTEX_PER_SEGMENT);
    p := @FVertices[0];
    SetLength(FIndices, INDICE_PER_SEGMENT);
    pIndice := @FIndices[0];
    quad1 := DoLineSegmentToQuadF(FPath[0], FPath[1]);
    i := 0;
    InitVerticesAndIndicesForQuad1;
    exit;
  end;

  quadCount := Length(FPath)-1;
  SetLength(FVertices, quadCount*VERTEX_PER_SEGMENT);
  SetLength(FIndices, quadCount*INDICE_PER_SEGMENT);

  // compute the previousTopLeft and previousBottomLeft
  quad1 := DoLineSegmentToQuadF(FPath[0], FPath[1]);

  p := @FVertices[0];
  pIndice := @FIndices[0];

  i := 0;
  for j:=0 to High(FPath)-2 do
  begin
    quad2 := DoLineSegmentToQuadF(FPath[j+1], FPath[j+2]);
    AdjustCornerOnConsecutiveQuadF(quad1, quad2);
    InitVerticesAndIndicesForQuad1;
    Move(quad2, quad1, SizeOf(TQuadF));
    inc(i);
  end;
  InitVerticesAndIndicesForQuad1; // quad1 is equal to quad2

  dec(p);
  if p <> @FVertices[High(FVertices)] then
    raise exception.create('vertex buffer overload or underload');

  dec(pIndice);
  if pIndice <>@FIndices[High(FIndices)] then
    raise exception.create('index buffer overload or underload');
end;

procedure TOGLCBorder.ClearArrays;
begin
  FPath := NIL;
  FVertices := NIL;
end;

procedure TOGLCBorder.CopyVertexTo(p: PLineVertexAttributes);
begin
  if Length(FVertices) > 0 then
    Move(FVertices[0], p^, SizeOf(TLineVertexAttributes)*Length(FVertices));
end;

procedure TOGLCBorder.CopyIndexTo(p: PVertexIndex);
begin
  if Length(FIndices) > 0 then
    Move(FIndices[0], p^, SizeOf(TVertexIndex)*Length(FIndices));
end;

function TOGLCBorder.VertexCount: ptrint;
begin
  Result := Length(FVertices);
end;

function TOGLCBorder.IndexCount: ptrint;
begin
  Result := Length(FIndices);
end;

{ TGradientDescriptor }

procedure TGradientDescriptor.AddRowItem(const aItem: TGradientRow);
var i: integer;
begin
  if Length(Rows) = 0 then i := 0
    else if Rows[High(Rows)].YPosition < aItem.YPosition then i := Length(Rows)
      else for i:=0 to High(Rows) do
             if Rows[i].YPosition > aItem.YPosition then break;
  Insert(aItem, Rows, i);
end;

function TGradientDescriptor.CreateArrayOfGradientItem(const aColors: ArrayOfTBGRAPixel; const aPos: ArrayOfSingle
  ): ArrayOfGradientItem;
var i: integer;
begin
  Result := NIL;
  SetLength(Result, Length(aColors));
  for i:=0 to High(aColors) do begin
    Result[i].Color := aColors[i];
    Result[i].XPosition := aPos[i];
  end;
end;

function TGradientDescriptor.GradientRow(aYPos: single; const Items: ArrayOfGradientItem): TGradientRow;
begin
  Result.YPosition := aYPos;
  Result.Items := Copy(Items);
end;

procedure TGradientDescriptor.InitDefault;
begin
  FillChar(Self, SizeOf(TGradientDescriptor), 0);
end;

procedure TGradientDescriptor.Clear;
begin
  Rows := NIL;
  Vertices := NIL;
  Indices := NIL;
  FUpdating := False;
end;

procedure TGradientDescriptor.CopyFrom(const aGradient: TGradientDescriptor);
begin
  Clear;
  Rows := Copy(aGradient.Rows);
  if FOnChange <> NIL then FOnChange;
end;

procedure TGradientDescriptor.CreateSingleColor(const aColor: TBGRAPixel);
begin
  CreateHorizontal([aColor, aColor], [0.0, 1.0]);
end;

procedure TGradientDescriptor.CreateHorizontal(const aColors: ArrayOfTBGRAPixel; const aXPos: ArrayOfSingle);
begin
  if Length(aColors) <> Length(aXPos) then raise exception.create('length of colors/positions array don''t match');
  Rows := NIL;
  SetLength(Rows, 2);
  Rows[0] := GradientRow(0, CreateArrayOfGradientItem(aColors, aXPos));
  Rows[1] := GradientRow(1, CreateArrayOfGradientItem(aColors, aXPos));
  if FOnChange <> NIL then FOnChange;
end;

procedure TGradientDescriptor.CreateVertical(const aColors: ArrayOfTBGRAPixel; const aYPos: ArrayOfSingle);
var i: integer;
begin
  if Length(aColors) <> Length(aYPos) then raise exception.create('length of colors/positions array don''t match');
  Rows := NIL;
  for i:=0 to High(aYPos) do
    AddRowItem(GradientRow(aYPos[i], CreateArrayOfGradientItem([aColors[i], aColors[i]], [0.0, 1.0])));
  if FOnChange <> NIL then FOnChange;
end;

procedure TGradientDescriptor.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TGradientDescriptor.AddRow(aYPosition: single; const aColors: ArrayOfTBGRAPixel; const aXPos: ArrayOfSingle);
var i: integer;
begin
  if Length(aXPos) < 2 then raise exception.create('array length must be minimum of 2');
  if Length(aColors) <> Length(aXPos) then raise exception.create('length of colors/positions array don''t match');
  if (aXpos[0] <> 0.0) or (aXPos[High(aXPos)] <> 1.0) then raise exception.create('aXPos array must begin with value 0.0 and end with value 1.0');
  if (Length(Rows) > 0) and (Length(Rows[0].Items) <> Length(aXPos)) then raise exception.create('rows must have the same length');
  for i:=0 to High(aXPos)-1 do
    if aXPos[i] >= aXPos[i+1] then
      raise exception.create('bad value in array aXPos. Values must be in ascending order and not equal');

  AddRowItem(GradientRow(aYPosition, CreateArrayOfGradientItem(aColors, aXPos)));
end;

procedure TGradientDescriptor.EndUpdate;
begin
  if FUpdating then begin
    FUpdating := False;
    if FOnChange <> NIL then FOnChange;
  end;
end;

procedure TGradientDescriptor.ComputeVerticesAndIndices(aWidth, aHeight: integer);
var r, c, cv, ci, lineWidth: integer;
begin
  Vertices := NIL;
  Indices := NIL;
  if Length(Rows) < 2 then exit;

  // retrieve the needed vertices and indices count using triangle strip.
  cv := 0;
  ci := 0;
  for r:=0 to High(Rows) do
    cv := cv + Length(Rows[r].Items);

  for r:=0 to High(Rows)-1 do
    ci := ci + Length(Rows[r].Items)*2;
  if Length(Rows) > 2 then ci := ci + Length(Rows)-2; // add number of primitive restart

  SetLength(Vertices, cv);
  SetLength(Indices, ci);

  // computed indices
  cv := 0;
  ci := 0;
  for r:=0 to High(Rows)-1 do begin
    lineWidth := Length(Rows[r].Items);
    Indices[ci] := cv+lineWidth;
    Indices[ci+1] := cv;
    inc(ci, 2);
    for c:=1 to High(Rows[r].Items) do begin
      Indices[ci] := cv+lineWidth+1;
      Indices[ci+1] := cv+1;
      inc(ci, 2);
      inc(cv);
    end;
    inc(cv);
    if r <> High(Rows)-1 then begin
      // add primitive restart
      Indices[ci] := PRIMITIVE_INDEX_RESTART_VALUE;
      inc(ci);
    end;
  end;

  // compute vertices
  cv := 0;
  for r:=0 to High(Rows) do begin
    for c:=0 to High(Rows[r].Items) do begin
      Vertices[cv].x := Rows[r].Items[c].XPosition*aWidth;
      Vertices[cv].y := Rows[r].YPosition*aHeight;
      Vertices[cv].c.InitFromBGRA(Rows[r].Items[c].Color);
      inc(cv);
    end;
  end;
end;

{$endif oglcIMPLEMENTATION}

