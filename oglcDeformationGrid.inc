{$ifdef oglcINTERFACE}
type
  ArrayOfArrayOfTPointF = array of array of TPointF;

  { TDeformationGrid }

  TDeformationType = ( dtNone, dtWaveH, dtWaveV, dtTumultuousWater, dtTumultuousWater2, dtFinFish,
                       dtSnakeH, dtSnakeV,
                       dtFlagRight);

TDeformationGrid = class( TSprite )
    Destructor Destroy; override;
  private
    FGridPts: ArrayOfArrayOfTPointF;
    FWidth, FHeight: integer;
    FRowCount, FColumnCount: integer;
    FShowGrid: boolean;
    FDeformationType: TDeformationType;
    FTimeCount: single;

    FHCell, FWCell,
    YpcRef: single;
    FApplyShadows: boolean;
    procedure Init; override;
    // draw part of texture
    procedure DrawTexturePart( aTexture: PTexture; aScreenCoor, aTexCoor: PTexCoor; aLayerPercentOpacity : single );

    // draw part of texture and darkened the hollow areas
    // the idea is that the y coordinate of the vertex represents its height
    // if a vertex.y > aHeightRef -> mix color White
    // else the color of the vertex is proportionally closer to black
    procedure DrawTexturePartWithShadows( aTexture: PTexture; aScreenCoor, aTexCoor: PTexCoor;
                               aHeightRef, aLayerPercentOpacity : single );
  protected
    function GetWidth: integer; override;
    function GetHeight: integer; override;
  public
    procedure Update( const aElapsedTime: single ); override;
    procedure Draw( const aLayerPercentOpacity: single ); override;
  public
   DeformationSpeed: TPointFParam; // <1 = slower  >1 faster   .X=Horizontam speed  .Y=Vertical speed
   Amplitude: TPointFParam; // deformation amplitude
   // apply a prebuild transformation on the grid
   procedure ApplyDeformation( aType: TDeformationType );
  public
   // set grid size
   procedure SetGrid( aWidth, aHeight, aRowCount, aColumnCount: integer );

   // access grid points
   property Grid: ArrayOfArrayOfTPointF read FGridPts;
   property ShowGrid: boolean read FShowGrid write FShowGrid;
   property RowCount: integer read FRowCount;
   property ColumnCount: integer read FColumnCount;

   property ApplyShadows: boolean read FApplyShadows write FApplyShadows;
   property pcHeightRef: single read YpcRef write YpcRef;
end;
  {$endif oglcINTERFACE}
  {$ifdef oglcIMPLEMENTATION}

  { TDeformationGrid }

destructor TDeformationGrid.Destroy;
begin
 DeformationSpeed.Free;
 Amplitude.Free;
 inherited Destroy;
end;

procedure TDeformationGrid.Init;
begin
 inherited Init;
 SetLength( FGridPts, 2, 2 );
 FGridPts[0][0] := PointF(0,0);
 FGridPts[0][1] := PointF(50,0);
 FGridPts[1][0] := PointF(0,50);
 FGridPts[1][1] := PointF(50,50);

 FRowCount := 2;
 FColumnCount := 2;

 FWidth := 50;
 FHeight := 50;

 FShowGrid := FALSE;

 FDeformationType := dtNone;

 FHCell := 50;
 FWCell := 50;
 YpcRef := 0.5;
 FApplyShadows:=FALSE;

 DeformationSpeed := TPointFParam.Create;
 DeformationSpeed.Value := PointF(1,1);

 Amplitude := TPointFParam.Create;
 Amplitude.Value := PointF(1,1);
end;

procedure TDeformationGrid.DrawTexturePart(aTexture: PTexture; aScreenCoor,
  aTexCoor: PTexCoor; aLayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  AlphaTint:byte;
  i: integer;
  procedure DrawVertex ;
   begin
{    glBegin( GL_TRIANGLES );
      glTexCoord2fv( @aTexCoor^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
    glBegin( GL_TRIANGLES );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;  }
    glBegin( GL_QUADS );
      glTexCoord2fv( @aTexCoor^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;
 if not FVisible then exit;

 quad[0] := aScreenCoor^[0];
 quad[1] := aScreenCoor^[1];
 quad[2] := aScreenCoor^[2];
 quad[3] := aScreenCoor^[3];

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip
 i:=0;
 if FFlipH then inc( i );
 if FFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

 if Tint.alpha.Value < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
         DrawVertex;
        end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
         glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
         DrawVertex;
        end;
end;

procedure TDeformationGrid.DrawTexturePartWithShadows(aTexture: PTexture;
  aScreenCoor, aTexCoor: PTexCoor; aHeightRef, aLayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  AlphaTint:byte;
  i: integer;
  ColorFactor, opac: single;

  procedure ComputeTopVertexColor( ay: single ); inline;
  begin
   ColorFactor := ay - aHeightRef;
   if ColorFactor<=0 then ColorFactor:=1.0
    else begin
    ColorFactor:= 1-(ColorFactor/FHCell);
    if ColorFactor<0 then ColorFactor:=0;
   end;
  end;
  procedure ComputeBottomVertexColor( ay: single ); inline;
  begin
   ColorFactor := ay - aHeightRef;
   if ColorFactor<=0 then ColorFactor:=1.0
    else begin
    ColorFactor:= 1-(ColorFactor/FHCell);
    if ColorFactor<0 then ColorFactor:=0.0;
ColorFactor+=0.5;
   end;
  end;

  procedure DrawVertex ;
   begin
{    glBegin( GL_TRIANGLES );
      glTexCoord2fv( @aTexCoor^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
    glBegin( GL_TRIANGLES );
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;  }
    glBegin( GL_QUADS );

      ComputeTopVertexColor( quad[0].y );
      glColor4f(ColorFactor, ColorFactor, ColorFactor, opac);
      glTexCoord2fv( @aTexCoor^[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );

      ComputeTopVertexColor( quad[1].y );
      glColor4f(ColorFactor, ColorFactor, ColorFactor, opac);
      glTexCoord2fv( @aTexCoor^[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );

      ComputeBottomVertexColor( quad[2].y );
      glColor4f(ColorFactor, ColorFactor, ColorFactor, opac);
      glTexCoord2fv( @aTexCoor^[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );

      ComputeBottomVertexColor( quad[3].y );
      glColor4f(ColorFactor, ColorFactor, ColorFactor, opac);
      glTexCoord2fv( @aTexCoor^[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;
 if not FVisible then exit;

 quad[0] := aScreenCoor^[0];
 quad[1] := aScreenCoor^[1];
 quad[2] := aScreenCoor^[2];
 quad[3] := aScreenCoor^[3];

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip
 i:=0;
 if FFlipH then inc( i );
 if FFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

 opac := Opacity.Value/255 * aLayerPercentOpacity;

 if Tint.alpha.Value < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
         DrawVertex;
        end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
         glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
         DrawVertex;
        end;
end;

function TDeformationGrid.GetWidth: integer;
begin
  Result := FWidth;
end;

function TDeformationGrid.GetHeight: integer;
begin
  Result := FHeight;
end;

procedure TDeformationGrid.Update(const aElapsedTime: single);
var ix,iy: integer;
    ixbegin, ixend, iybegin, iyend: integer;
begin
 inherited Update(aElapsedTime);
 if FFreeze then exit;
 if FDeformationType=dtNone then exit;

 DeformationSpeed.OnElapse( aElapsedTime );
 Amplitude.OnElapse( aElapsedTime );

 if FDeformationType in [dtSnakeH,dtSnakeV,dtTumultuousWater2]{(FDeformationType=dtSnakeH) or FDeformationType in [
    (FDeformationType=dtSnakeV)} then begin
   ixbegin := 0;
   ixend := ColumnCount-1;
   iybegin := 0;
   iyend := RowCount-1;
 end else if (FDeformationType=dtFlagRight) then begin
   ixbegin := 1;
   ixend := ColumnCount-1;
   iybegin := 0;
   iyend := RowCount-1;
 end else begin
   ixbegin := 1;
   ixend := ColumnCount-2;
   iybegin := 1;
   iyend := RowCount-2;
 end;

 FTimeCount += AElapsedTime;
 for iy:=iybegin to iyend do
  for ix:=ixbegin to ixend do begin
   case FDeformationType of
    //FGrid.Grid[iy][ix].x := ix*FGrid.Width/FGrid.ColumnCount + cos(TimeCount)*3*(ix+1);
    //FGrid.Grid[iy][ix].y := iy*FGrid.Height/FGrid.RowCount + sin(TimeCount*(iy*0.5+1))*2*(iy+1);

    // FGrid.Grid[iy][ix].y := iy*FGrid.Height/FGrid.RowCount + sin(TimeCount*(iy*0.2))*2*(iy+1);

    dtTumultuousWater, dtTumultuousWater2: begin
      Grid[iy][ix].x := ix*Width/(ColumnCount)+cos(DeformationSpeed.x.Value*FTimeCount+ix+iy*0.5)*FWCell*0.5*Amplitude.x.Value;
      Grid[iy][ix].y := iy*Height/(RowCount)+sin(DeformationSpeed.y.Value*FTimeCount+iy+ix*0.5)*FHCell*0.5*Amplitude.y.Value;
    end;

    dtWaveV: Grid[iy][ix].y := iy*Height/(RowCount)+sin(DeformationSpeed.y.Value*FTimeCount+iy)*FHCell*0.5*Amplitude.y.Value;

    dtWaveH: Grid[iy][ix].x := ix*Width/(ColumnCount)+cos(DeformationSpeed.x.Value*FTimeCount+ix)*FWCell*0.5*Amplitude.x.Value;

    dtFinFish: Grid[iy][ix].x := ix*Width/(ColumnCount) + cos(DeformationSpeed.x.Value*FTimeCount-(PI/(ix+1)))*FWCell*0.5*Amplitude.x.Value;

    dtSnakeH: begin
      Grid[iy][ix].y := iy*Height/(RowCount)+sin(DeformationSpeed.y.Value*FTimeCount+(iy*0.5))*FHCell*0.5*Amplitude.y.Value;
      Grid[iy][ix].x := ix*Width/(ColumnCount)+cos(DeformationSpeed.x.Value*FTimeCount+(ix*0.3))*FWCell*0.5*Amplitude.x.Value;
    end;

    dtSnakeV: begin
      Grid[iy][ix].y := iy*Height/(RowCount)+sin(DeformationSpeed.y.Value*FTimeCount+(iy*0.3))*FHCell*0.5*Amplitude.y.Value;
      Grid[iy][ix].x := ix*Width/(ColumnCount)+cos(DeformationSpeed.x.Value*FTimeCount+(ix*0.5))*FWCell*0.5*Amplitude.x.Value;
    end;

    dtFlagRight: begin
      Grid[iy][ix].x := ix*Width/(ColumnCount)+cos(DeformationSpeed.x.Value*FTimeCount+ix*0.5)*FWCell*2/(ixend-ix+2)*Amplitude.x.Value;
      Grid[iy][ix].y := iy*Height/(RowCount)+sin(DeformationSpeed.y.Value*FTimeCount+ix*0.5)*FHCell*0.5/(ixend-ix+2)*Amplitude.y.Value;
    end;
   end;//case
  end;

end;

procedure TDeformationGrid.Draw(const aLayerPercentOpacity: single);
var WholeTexCoor, PartTexCoor, ScreenCellCoor: TTexCoor;
    tx, ty, tw, th: single;
    ix, iy: integer;
    yref: single;
begin
 if FTexture=NIL then exit;
 if (FWidth=0) or (FHeight=0) then exit;
 if (FRowCount<2) or (FColumnCount<2) then exit;

 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );
 TextureManager.BindTexture( FTexture );

 WholeTexCoor := FTexture^.FramesCoord[trunc(FFrame)];

 ty := WholeTexCoor[0].y;
 tw := (WholeTexCoor[1].x - WholeTexCoor[0].x)/(FColumnCount-1);
 th := (WholeTexCoor[1].y - WholeTexCoor[2].y)/(FRowCount-1);

 for iy:=0 to FRowCount-2 do begin
  if FApplyShadows then yref := FHCell * ( iy + YpcRef );

  tx := WholeTexCoor[0].x;
  for ix:=0 to FColumnCount-2 do
   begin
    ScreenCellCoor[0].x := FGridPts[iy][ix].x;
    ScreenCellCoor[0].y := FGridPts[iy][ix].y;

    ScreenCellCoor[1].x := FGridPts[iy][ix+1].x;
    ScreenCellCoor[1].y := FGridPts[iy][ix+1].y;

    ScreenCellCoor[2].x := FGridPts[iy+1][ix+1].x;
    ScreenCellCoor[2].y := FGridPts[iy+1][ix+1].y;

    ScreenCellCoor[3].x := FGridPts[iy+1][ix].x;
    ScreenCellCoor[3].y := FGridPts[iy+1][ix].y;

    PartTexCoor[0].x := tx;
    PartTexCoor[0].y := ty;

    PartTexCoor[1].x := tx + tw;
    PartTexCoor[1].y := ty;

    PartTexCoor[2].x := PartTexCoor[1].x;
    PartTexCoor[2].y := ty - th;

    PartTexCoor[3].x := tx;
    PartTexCoor[3].y := PartTexCoor[2].y;

    tx := PartTexCoor[1].x;

    if FApplyShadows
     then DrawTexturePartWithShadows( FTexture, @ScreenCellCoor, @PartTexCoor, {%H-}yref, aLayerPercentOpacity )
     else DrawTexturePart( FTexture, @ScreenCellCoor, @PartTexCoor, aLayerPercentOpacity );
   end;
  ty := {%H-}PartTexCoor[2].y;
 end;

 if FShowGrid then begin
  TextureManager.DisableTextureUsage;
  for iy:=0 to FRowCount-2 do
   for ix:=0 to FColumnCount-2 do
   begin
    DrawLine( FGridPts[iy][ix], FGridPts[iy][ix+1], BGRA(255,0,0), 1.0 );
    DrawLine( FGridPts[iy][ix+1], FGridPts[iy+1][ix+1], BGRA(255,0,0), 1.0 );
    DrawLine( FGridPts[iy+1][ix+1], FGridPts[iy+1][ix], BGRA(255,0,0), 1.0 );
    DrawLine( FGridPts[iy+1][ix], FGridPts[iy][ix], BGRA(255,0,0), 1.0 );
   end;
 end;

 DrawChildSurfaces( aLayerPercentOpacity );

 PopMatrix;
end;

procedure TDeformationGrid.SetGrid(aWidth, aHeight, aRowCount, aColumnCount: integer);
var ix, iy: integer;
begin
 if aRowCount<2 then aRowCount := 2;
 if aColumnCount<2 then aColumnCount := 2;
 if aWidth<1 then aWidth:=1;
 if aHeight<1 then aHeight:=1;

 FWidth := aWidth;
 FHeight := aHeight;

 FRowCount := aRowCount;
 FColumnCount := aColumnCount;
 SetLength( FGridPts, aRowCount, aColumnCount );

 for iy:=0 to aRowCount-1 do
  for ix:=0 to aColumnCount-1 do begin
   FGridPts[iy][ix] := PointF( aWidth/aColumnCount*ix, aHeight/aRowCount*iy );
  end;

 FHCell := aHeight/(aRowCount-1);
 FWCell := aWidth/(aColumnCount-1);
end;

procedure TDeformationGrid.ApplyDeformation(aType: TDeformationType);
begin
// FTimeCount := 0;
 FDeformationType := aType;
end;

{$endif oglcIMPLEMENTATION}


