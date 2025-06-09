{$ifdef oglcINTERFACE}
type
  ArrayOfArrayOfTPointF = array of array of TPointF;

  { TDeformationGrid }

  TDeformationType = ( dtNone,
                       dtBasic,
                       dtWaveH, dtWaveV,
                       dtTumultuousWater,
                       dtFinFish,
                       dtSnakeH, dtSnakeV,
                       dtFlagRight,
                       dtWindingUp, dtWindingDown, dtWindingLeft, dtWindingRight);

TDeformedPoint = record
  x, y,                      // transformed node coordinates
  DeformationAmount,         // [0..1] to control the amount of deformation per node
  TimeMultiplicator: single; // <1 is slower  =1 is normal   >1 is faster
end;
PDeformedPoint = ^TDeformedPoint;
ArrayOfArrayOfDeformedPoint = array of array of TDeformedPoint;

TDeformationGrid = class(TSprite)
private
  FGridPts: ArrayOfArrayOfDeformedPoint;
  FRowCount, FColumnCount: integer;
  FShowGrid, FGridInitialized: boolean;
  FDeformationType: TDeformationType;
  FTimeCount: single;
  FCellHeight, FCellWidth: single;
  procedure Init; override;
public
  Destructor Destroy; override;
  procedure Update(const aElapsedTime: single); override;
  procedure DoDraw; override;
public
  DeformationSpeed: TPointFParam; // <1 = slower  >1 faster   .X=Horizontam speed  .Y=Vertical speed
  Amplitude: TPointFParam; // deformation amplitude
  // apply a prebuild deformation on the grid
  procedure ApplyDeformation(aType: TDeformationType);
public
  // set grid size.
  // Row index range is 0 to aRowCount
  // Column index range is 0 to aColumnCount
  procedure SetGrid(aRowCount, aColumnCount: integer);

  // Allow to control the amount of deformation to apply on a particular row of the grid.
  // i.e. SetDeformationAmountOnRow(0, 0.0) sets no deformation on the first row.
  procedure SetDeformationAmountOnRow(aRowIndex: integer; aDeformationAmount: single);
  // Allow to control the amount of deformation to apply on a particular column of the grid.
  procedure SetDeformationAmountOnColumn(aColIndex: integer; aDeformationAmount: single);

  // Allow to speed up or slow down the deformations on a particular row.
  // i.e. SetTimeMultiplicatorOnRow(4, 2.0) speed up 2 time the deformation on the row of index 4.
  procedure SetTimeMultiplicatorOnRow(aRowIndex: integer; aTimeMultiplicator: single);
  // Allow to speed up or slow down the deformations on a particular column.
  procedure SetTimeMultiplicatorOnColumn(aColIndex: integer; aTimeMultiplicator: single);

  // Allow to control the amount of deformation on all sides (on the periphery) of the grid.
  procedure SetDeformationAmountOnAllSide(aDeformationAmount: single);
  // Allow to control the deformation speed on all sides (on the periphery) of the grid.
  procedure SetTimeMultiplicatorOnAllSide(aTimeMultiplicator: single);

  // access grid points
  property Grid: ArrayOfArrayOfDeformedPoint read FGridPts;
  property ShowGrid: boolean read FShowGrid write FShowGrid;
  property RowCount: integer read FRowCount;
  property ColumnCount: integer read FColumnCount;
end;
  {$endif oglcINTERFACE}
  {$ifdef oglcIMPLEMENTATION}

  { TDeformationGrid }

destructor TDeformationGrid.Destroy;
begin
  DeformationSpeed.Free;
  DeformationSpeed := NIL;
  Amplitude.Free;
  Amplitude := NIL;
  inherited Destroy;
end;

procedure TDeformationGrid.Init;
begin
  inherited Init;
  FShowGrid := FALSE;
  FGridInitialized := False;

  FDeformationType := dtNone;

  DeformationSpeed := TPointFParam.Create;
  DeformationSpeed.Value := PointF(1,1);

  Amplitude := TPointFParam.Create;
  Amplitude.Value := PointF(1,1);
end;

procedure TDeformationGrid.Update(const aElapsedTime: single);
var ix, iy: integer;
    ampX, ampY, speedX, speedY: single;
    p: PDeformedPoint;
begin
  inherited Update(aElapsedTime);
  if FFreeze then exit;
  if (FDeformationType = dtNone) or not FGridInitialized then exit;

  DeformationSpeed.OnElapse(aElapsedTime);
  Amplitude.OnElapse(aElapsedTime);

  ampX := Amplitude.x.Value;
  ampY := Amplitude.y.Value;
  FTimeCount := FTimeCount + AElapsedTime;
  speedX := DeformationSpeed.x.Value * FTimeCount;
  speedY := DeformationSpeed.y.Value * FTimeCount;

  for iy:=0 to RowCount do
   for ix:=0 to ColumnCount do begin
    p := @Grid[iy][ix];
    case FDeformationType of
     dtBasic: begin
      p^.x := ix*FCellWidth+cos(speedX*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;
      p^.y := iy*FCellHeight+sin(speedY*p^.TimeMultiplicator)*FCellHeight*0.5*ampY*p^.DeformationAmount;
     end;

     dtTumultuousWater: begin
       p^.x := ix*FCellWidth+cos((speedX+ix+iy*0.5)*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;
       p^.y := iy*FCellHeight+sin((speedY+iy+ix*0.5)*p^.TimeMultiplicator)*FCellHeight*0.5*ampY*p^.DeformationAmount;
     end;

     dtWaveV: p^.y := iy*FCellHeight+sin((speedY+iy)*p^.TimeMultiplicator)*FCellHeight*0.5*ampY*p^.DeformationAmount;

     dtWaveH: p^.x := ix*FCellWidth+cos((speedX+ix)*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;

     dtFinFish: p^.x := ix*FCellWidth + cos((speedX-(PI/(ix+1)))*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;

     dtSnakeH: begin
       p^.y := iy*FCellHeight+sin((speedY+(iy*0.5))*p^.TimeMultiplicator)*FCellHeight*0.5*ampY*p^.DeformationAmount;
       p^.x := ix*FCellWidth+cos((speedX+(ix*0.3))*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;
     end;

     dtSnakeV: begin
       p^.y := iy*FCellHeight+sin((speedY+(iy*0.3))*p^.TimeMultiplicator)*FCellHeight*0.5*ampY*p^.DeformationAmount;
       p^.x := ix*FCellWidth+cos((speedX+(ix*0.5))*p^.TimeMultiplicator)*FCellWidth*0.5*ampX*p^.DeformationAmount;
     end;

     dtFlagRight: begin
       p^.x := ix*FCellWidth+cos((speedX+ix*0.5)*p^.TimeMultiplicator)*FCellWidth*2/(ColumnCount-ix+2)*ampX*p^.DeformationAmount;
       p^.y := iy*FCellHeight+sin((speedY+ix*0.5)*p^.TimeMultiplicator)*FCellHeight*0.5/(ColumnCount-ix+2)*ampY*p^.DeformationAmount;
     end;

     dtWindingUp: begin
      if DeformationSpeed.y.Value > 0 then begin
        if p^.y > 0 then begin
          p^.y := p^.y - aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.y.Value*ampY*p^.DeformationAmount;
          if p^.y < 0 then p^.y := 0
            else if p^.y > Height then p^.y := Height;
        end;
      end else begin
        if p^.y < iy*FCellHeight then begin
          p^.y := p^.y - aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.y.Value*ampY*p^.DeformationAmount;
          if p^.y > iy*FCellHeight then p^.y := iy*FCellHeight;
        end;
      end;
     end;

     dtWindingDown: begin
      if DeformationSpeed.y.Value > 0 then begin
        if p^.y < Height then begin
          p^.y := p^.y + aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.y.Value*ampY*p^.DeformationAmount;
          if p^.y > Height then p^.y := Height
            else if p^.y < 0 then p^.y := 0;
        end;
       end else begin
        if p^.y > iy*FCellHeight then begin
          p^.y := p^.y + aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.y.Value*ampY*p^.DeformationAmount;
          if p^.y < iy*FCellHeight then p^.y := iy*FCellHeight;
        end;
       end;
     end;

     dtWindingLeft: begin
      if DeformationSpeed.x.Value > 0 then begin
        if p^.x > 0 then begin
          p^.x := p^.x - aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.x.Value*ampX*p^.DeformationAmount;
          if p^.x < 0 then p^.x := 0;
        end;
      end else begin
        if p^.x < ix*FCellWidth then begin
          p^.x := p^.x - aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.x.Value*ampX*p^.DeformationAmount;
          if p^.x > ix*FCellWidth then p^.x := ix*FCellWidth;
        end;
      end;
     end;

     dtWindingRight: begin
      if DeformationSpeed.x.Value > 0 then begin
        if p^.x < Width then begin
          p^.x := p^.x + aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.x.Value*ampX*p^.DeformationAmount;
          if p^.x > Width then p^.x := Width;
        end;
      end else begin
        if p^.x > ix*FCellWidth then begin
          p^.x := p^.x + aElapsedTime*p^.TimeMultiplicator*DeformationSpeed.x.Value*ampX*p^.DeformationAmount;
          if p^.x < ix*FCellWidth then p^.x := ix*FCellWidth;
        end;
      end;
     end;
    end;//case
   end;
end;

procedure TDeformationGrid.DoDraw;
var WholeTexCoor, PartTexCoor, ScreenCellCoor: TQuadCoor;
    tx, ty, tw, th: single;
    ix, iy, ixf, iyf: integer;
    pt1, pt2: TPointF;
begin
  if not FGridInitialized then exit;
  WholeTexCoor := FTexture^.FramesCoord[trunc(FFrame)];

  ty := WholeTexCoor[cTL].y;
  tw := (WholeTexCoor[cTR].x - WholeTexCoor[cTL].x)/FColumnCount;
  th := (WholeTexCoor[cTR].y - WholeTexCoor[cBR].y)/FRowCount;

  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangles, FTexture, 0, FBlendmode);
  for iy:=0 to FRowCount-1 do
  begin
    tx := WholeTexCoor[cTL].x;
    for ix:=0 to FColumnCount-1 do
    begin
      if FFlipH then ixf := FColumnCount-1-ix else ixf := ix;
      if FFlipV then iyf := FRowCount-1-iy else iyf := iy;

      ScreenCellCoor[cTL].x := FGridPts[iyf][ixf].x;
      ScreenCellCoor[cTL].y := FGridPts[iyf][ixf].y;

      ScreenCellCoor[cTR].x := FGridPts[iyf][ixf+1].x;
      ScreenCellCoor[cTR].y := FGridPts[iyf][ixf+1].y;

      ScreenCellCoor[cBR].x := FGridPts[iyf+1][ixf+1].x;
      ScreenCellCoor[cBR].y := FGridPts[iyf+1][ixf+1].y;

      ScreenCellCoor[cBL].x := FGridPts[iyf+1][ixf].x;
      ScreenCellCoor[cBL].y := FGridPts[iyf+1][ixf].y;

      PartTexCoor[cTL].x := tx;
      PartTexCoor[cTL].y := ty;

      PartTexCoor[cTR].x := tx + tw;
      PartTexCoor[cTR].y := ty;

      PartTexCoor[cBR].x := PartTexCoor[cTR].x;
      PartTexCoor[cBR].y := ty - th;

      PartTexCoor[cBL].x := tx;
      PartTexCoor[cBL].y := PartTexCoor[cBR].y;

      tx := PartTexCoor[cTR].x;

      FParentScene.TexturedMVTriangleRenderer.PushQuad2(ScreenCellCoor, PartTexCoor, FlipToIndex, NIL, FComputedOpacity, FComputedTint);
    end;
    ty := {%H-}PartTexCoor[cBR].y;
  end;

  if FShowGrid then begin
    FParentScene.FastLineRenderer.Prepare(FParentScene.MVPMatrix, FX_BLEND_NORMAL);
    for iy:=0 to FRowCount-1 do
     for ix:=0 to FColumnCount-1 do begin
       pt1 := PointF(FGridPts[iy][ix].x, FGridPts[iy][ix].y);
       pt2 := PointF(FGridPts[iy][ix+1].x, FGridPts[iy][ix+1].y);
       FParentScene.FastLineRenderer.PushLine(pt1, pt2, BGRA(255,0,0), 1.0, lpMiddle);
       pt1 := PointF(FGridPts[iy+1][ix+1].x, FGridPts[iy+1][ix+1].y);
       FParentScene.FastLineRenderer.PushLine(pt2, pt1, BGRA(255,0,0), 1.0, lpMiddle);
       pt2 := PointF(FGridPts[iy+1][ix].x, FGridPts[iy+1][ix].y);
       FParentScene.FastLineRenderer.PushLine(pt1, pt2, BGRA(255,0,0), 1.0, lpMiddle);
       pt1 := PointF(FGridPts[iy][ix].x, FGridPts[iy][ix].y);
       FParentScene.FastLineRenderer.PushLine(pt2, pt1, BGRA(255,0,0), 1.0, lpMiddle);
     end;
  end;
end;

procedure TDeformationGrid.SetGrid(aRowCount, aColumnCount: integer);
var ix, iy: integer;
begin
  if aRowCount < 1 then aRowCount := 1;
  if aColumnCount < 1 then aColumnCount := 1;

  FRowCount := aRowCount;
  FColumnCount := aColumnCount;
  SetLength(FGridPts, aRowCount+1, aColumnCount+1);

  FCellHeight := Height/aRowCount;
  FCellWidth := Width/aColumnCount;

  for iy:=0 to High(FGridPts) do
   for ix:=0 to High(FGridPts[0]) do begin
    with FGridPts[iy][ix] do begin
      x := ix*FCellWidth;
      y := iy*FCellHeight;
      DeformationAmount := 1.0;
      TimeMultiplicator := 1.0;
    end;
   end;
  FGridInitialized := True;
end;

procedure TDeformationGrid.SetDeformationAmountOnRow(aRowIndex: integer; aDeformationAmount: single);
var i: integer;
begin
  if not FGridInitialized then exit;
  for i:=0 to FColumnCount do
    FGridPts[aRowIndex][i].DeformationAmount := aDeformationAmount;
end;

procedure TDeformationGrid.SetDeformationAmountOnColumn(aColIndex: integer; aDeformationAmount: single);
var i: integer;
begin
  if not FGridInitialized then exit;
  for i:=0 to FRowCount do
    FGridPts[i][aColIndex].DeformationAmount := aDeformationAmount;
end;

procedure TDeformationGrid.SetTimeMultiplicatorOnRow(aRowIndex: integer; aTimeMultiplicator: single);
var i: integer;
begin
  if not FGridInitialized then exit;
  for i:=0 to FColumnCount do
    FGridPts[aRowIndex][i].TimeMultiplicator := aTimeMultiplicator;
end;

procedure TDeformationGrid.SetTimeMultiplicatorOnColumn(aColIndex: integer; aTimeMultiplicator: single);
var i: integer;
begin
  if not FGridInitialized then exit;
  for i:=0 to FRowCount do
    FGridPts[i][aColIndex].TimeMultiplicator := aTimeMultiplicator;
end;

procedure TDeformationGrid.SetDeformationAmountOnAllSide(aDeformationAmount: single);
begin
  if not FGridInitialized then exit;
  SetDeformationAmountOnRow(0, aDeformationAmount);
  SetDeformationAmountOnRow(FRowCount, aDeformationAmount);
  SetDeformationAmountOnColumn(0, aDeformationAmount);
  SetDeformationAmountOnColumn(FColumnCount, aDeformationAmount);
end;

procedure TDeformationGrid.SetTimeMultiplicatorOnAllSide(aTimeMultiplicator: single);
begin
  if not FGridInitialized then exit;
  SetTimeMultiplicatorOnRow(0, aTimeMultiplicator);
  SetTimeMultiplicatorOnRow(FRowCount, aTimeMultiplicator);
  SetTimeMultiplicatorOnColumn(0, aTimeMultiplicator);
  SetTimeMultiplicatorOnColumn(FColumnCount, aTimeMultiplicator);
end;

procedure TDeformationGrid.ApplyDeformation(aType: TDeformationType);
begin
  FDeformationType := aType;
end;

{$endif oglcIMPLEMENTATION}


