{$ifdef oglcINTERFACE}
type
{ TParticleEmitter }
TEmitterType = (etPoint, etLine, etRectangle, etCircle, etRing, etInnerCircle, etOuterCircle );

const
MAX_PARTICLES = 1024;

type

PParticle = ^TParticle;
TParticle = record
              X, Y: single;
              VX, VY: single;
              CurrentLife: single;
              InitialLife: single;

              FrameIndex: single;
              FrameCoeff: single;

              CurrentColor: TBGRAPixel;
              FColorIndex: integer;

              InitialSize: single;
              CurrentSize: single;
              FSizeIndex: integer;

              InitialVelocity: single;
              CurrentVelocity: single;
              FVelocityIndex: integer;

              InitialAVelocity: single;
              CurrentAVelocity: single;
              FAVelocityIndex: integer;

              CurrentDirection: single;

              InitialSpin: single;
              FSpinIndex: integer;
              CurrentAngle: single ;
             end;

// called when a particle reachs the end of its trajectory (life)
TOnParticleDies= procedure ( aPar: PParticle; aParIndex: integer ) of object;

PPColor = ^TPColor ;
TPColor = packed record
            C: TBGRAPixel;
            Life: single;
end;
PPsingle = ^TPSingle;
TPSingle = packed record
             Value: single;
             Life: single;
end;

TParticleParameter = record
                      Texture: PTexture;
                      StartFrame: integer;
                      EndFrame: integer;
                      ColorMode: byte;
                      BlendMode: byte;
                      Life: single;
                      LifeVariation: single;
                      ArrayColor: array of TPColor;
                      Velocity: single ;
                      VelocityVariation: single;
                      ArrayVelocity: array of TPSingle;
                      AVelocity: single;
                      AVelocityVariation: single;
                      ArrayAVelocity: array of TPSingle;
                      Size: single;
                      SizeVariation: single;
                      ArraySize: array of TPSingle;
                      Spin: single;
                      SpinVariation: single;
                      ArraySpin: array of TPSingle;
                      StartAngle: single;
                      StartAngleVariation: single;
end;



TParticleEmitter = class( TSimpleSurfaceWithEffect )
 Constructor Create;
 Destructor Destroy ; override;
protected
 FParticles : array [0..MAX_PARTICLES-1] of TParticle;
 FCurrentLife : single;
 FParticleRate : single;
 FParticleRateCount : single;
 FParticleCount : integer;
 FEmitterType: TEmitterType;
 FWidth, FHeight: integer;
 FLineP2: TPointF;
 FLineDirection, FLineSize: single;
 FSmallRadius, FBigRadius: single;
 procedure SetSize( AWidth, AHeight: integer );
 function GetWidth: integer; override;
 function GetHeight: integer; override;
 procedure DrawParticleTexture( aTexture: PTexture; AFrameIndex: integer; aTint: TBGRAPixel; aX, aY, aAngle, aZoom: single );
private
 // callback
 FOnParticleDies: TOnParticleDies;
public
 procedure Update ( const aElapsedTime : single ) ; override;
 procedure Draw( const aLayerPercentOpacity : single ) ; override;
public
 // acces to particle parameters
 FParticleParam : TParticleParameter;
public
 ParticlesToEmit: TBoundedFParam;      // max particles count to emit at the same time [0..1024]
 Gravity        : TPointFParam;        // gravity in pixels per second
 Direction      : TBoundedFParam;      // [0..360] degree
 Spread         : TBoundedFParam;      // [0..360] degree
 EmitterLife    : single;              // duration in seconds of one shoot if emitter is not in loop mode
 LoopMode       : boolean;
 ParticlesPositionsRelativeToEmitterPosition: boolean; // FALSE by default
 procedure LoadFromFile( const aFilename: string );
 procedure Shoot ;
 procedure Clear ;

 procedure SetEmitterTypePoint; // (X,Y) is the position of the point
 procedure SetEmitterTypeLine( P2: TPointF ); // (X,Y) is the first point of the line
 procedure SetEmitterTypeLine( aDirection, aSize: single ); // (X,Y) is the first point of the line
 procedure SetEmitterTypeRectangle( aWidth, aHeight: integer ); // (X,Y) is the top left corner of the rectangle
 procedure SetEmitterTypeCircle( Radius: single ); // (X,Y) is the center of the circle
 procedure SetEmitterTypeInnerCircle( Radius: single );
 procedure SetEmitterTypeOuterCircle( Radius: single );
 procedure SetEmitterTypeRing( SmallRadius, BigRadius: single );

 property EmitterType: TEmitterType read FEmitterType;
 property ParticlesCount: integer read FParticleCount;
 property OnParticleDies: TOnParticleDies read FOnParticleDies write FOnParticleDies;
end;
{$endif oglcINTERFACE}

{$ifdef oglcIMPLEMENTATION}
{ TParticleEmitter }

constructor TParticleEmitter.Create ;
var i:integer ;
begin
 inherited Create;
 for i:=0 to MAX_PARTICLES-1 do
  begin
   FParticles[i].VX := 0;
   FParticles[i].VY := 0;
   FParticles[i].InitialLife := 0;
   FParticles[i].CurrentLife := 0;
  end;
 FEmitterType := etPoint;
 LoopMode := TRUE;
 EmitterLife := 3;
 FCurrentLife := 0;
 FParticleCount := 0;
 ParticlesToEmit := CreateBoundedFParam( 0, 1024 );
 Gravity := TPointFParam.Create;
 Spread := CreateBoundedFParam( 0, 360, TRUE );
 Direction := CreateBoundedFParam( 0, 360, TRUE );
 ParticlesPositionsRelativeToEmitterPosition := FALSE;
 SetSize(1,1);
end;

destructor TParticleEmitter.Destroy;
begin
 SetLength( FParticleParam.ArrayColor, 0 ) ;
 SetLength( FParticleParam.ArraySpin, 0 ) ;
 SetLength( FParticleParam.ArraySize, 0 ) ;
 SetLength( FParticleParam.ArrayVelocity, 0 ) ;
 SetLength( FParticleParam.ArrayAVelocity, 0 ) ;
 TextureManager.Delete( FParticleParam.Texture );
 ParticlesToEmit.Free;
 Gravity.Free;
 Spread.Free;
 Direction.Free;
 inherited Destroy;
end;

procedure TParticleEmitter.Update(const aElapsedTime: single);
var i, PToCreate, _Spread : integer ;
    iLife, coeff, ang, siz{, Dir, Vel} : single ;
    pc, nc : PPColor ;
    ps, ns : PPSingle ;
    offsetX, offsetY: single;
begin
 inherited Update( aElapsedTime );
 if FFreeze then exit;
 // update parameters
 Gravity.OnElapse( aElapsedTime );
 ParticlesToEmit.OnElapse( aElapsedTime );
 Spread.OnElapse( aElapsedTime );
 Direction.OnElapse( aElapsedTime );
 _Spread := round( Spread.Value );
 // update particles
 for i:=0 to MAX_PARTICLES-1 do
 with FParticles[i], FParticleParam do
  begin
   if CurrentLife > 0
     then begin
           CurrentLife -= aElapsedTime ;
           if CurrentLife <= 0
             then begin
                   dec(FParticleCount);
                   if FOnParticleDies<>NIL then FOnParticleDies( @FParticles[i], i );
                  end
             else begin
                   iLife := (InitialLife - CurrentLife) / InitialLife;
                   // frame index
                   FrameIndex := FrameIndex + FrameCoeff * aElapsedTime;
                   // color
                   while ArrayColor[FColorIndex].Life < iLife do
                    inc(FColorIndex) ;
                   pc := @ArrayColor[FColorIndex-1] ;
                   nc := @ArrayColor[FColorIndex] ;
                   coeff := (iLife - pc^.Life) / (nc^.Life - pc^.Life) ;
                   CurrentColor.red := pc^.C.red + round( ( nc^.C.red - pc^.C.red ) * coeff ) ;
                   CurrentColor.green := pc^.C.green + round( ( nc^.C.green - pc^.C.green ) * coeff ) ;
                   CurrentColor.blue := pc^.C.blue + round( ( nc^.C.blue - pc^.C.blue ) * coeff ) ;
                   CurrentColor.alpha := pc^.C.alpha + round( ( nc^.C.alpha - pc^.C.alpha ) * coeff ) ;
                   // size
                   while ArraySize[FSizeIndex].Life < iLife do inc(FSizeIndex) ;
                   ps := @ArraySize[FSizeIndex-1] ;
                   ns := @ArraySize[FSizeIndex] ;
                   CurrentSize := InitialSize * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) );
                   // Spin
                   while ArraySpin[FSpinIndex].Life < iLife do inc(FSpinIndex) ;
                   ps := @ArraySpin[FSpinIndex-1] ;
                   ns := @ArraySpin[FSpinIndex] ;
                   CurrentAngle := CurrentAngle + InitialSpin * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) ) * aElapsedTime ;
                   // Velocity
                   while ArrayVelocity[FVelocityIndex].Life < iLife do inc(FVelocityIndex) ;
                   ps := @ArrayVelocity[FVelocityIndex-1] ;
                   ns := @ArrayVelocity[FVelocityIndex] ;
                   CurrentVelocity := InitialVelocity + InitialVelocity * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) ) ;
                   // Angular Velocity
                   while ArrayAVelocity[ FAVelocityIndex ].Life < iLife do INC( FAVelocityIndex );
                   ps     := @ArrayAVelocity[ FAVelocityIndex-1 ];
                   ns     := @ArrayAVelocity[ FAVelocityIndex ];
                   CurrentAVelocity := InitialAVelocity + InitialAVelocity * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) );
                   CurrentDirection += CurrentAVelocity * aElapsedTime ;

                   VX := cos( CurrentDirection * deg2rad ) * CurrentVelocity ;
                   VY := sin( CurrentDirection * deg2rad ) * CurrentVelocity ;
                   // Gravity
                   X += ( VX + Gravity.x.Value ) * aElapsedTime ;
                   Y += ( VY + Gravity.y.Value ) * aElapsedTime ;
                  end;
          end;
  end;
 // Emitter life
 FCurrentLife -= aElapsedTime ;
 if ( FCurrentLife <= 0 ) and not LoopMode
   then begin
         FCurrentLife := 0 ;
         exit ;
        end;
 // create new particles
 if ParticlesToEmit.Value = 0.0 then exit;
 FParticleRate := 1/ParticlesToEmit.Value;
 FParticleRateCount += aElapsedTime ;
 PToCreate := 0 ;
 while FParticleRateCount > FParticleRate do
  begin
   FParticleRateCount -= FParticleRate;
   inc(PToCreate) ;
  end;
  if FParticleCount + PToCreate > MAX_PARTICLES then PToCreate := MAX_PARTICLES - FParticleCount ;
  if PToCreate > 0
    then begin
          FParticleCount += PToCreate ;
          i := 0 ;
          repeat
           while FParticles[i].CurrentLife > 0 do inc( i ) ;
           FParticles[i].InitialLife := FParticleParam.Life + random(round(FParticleParam.LifeVariation*1000))*0.001;
           FParticles[i].CurrentLife := FParticles[i].InitialLife ;

           FParticles[i].FrameIndex := FParticleParam.StartFrame;
           FParticles[i].FrameCoeff := (FParticleParam.EndFrame - FParticleParam.StartFrame) / FParticles[i].InitialLife;

           FParticles[i].CurrentColor := FParticleParam.ArrayColor[0].C ;
           FParticles[i].FColorIndex := 1 ;

           FParticles[i].CurrentAngle := FParticleParam.StartAngle + random( round(FParticleParam.StartAngleVariation*1000) )*0.001 ;

           FParticles[i].InitialSpin := FParticleParam.Spin + random( round(FParticleParam.SpinVariation*1000) )*0.001 ;
           FParticles[i].FSpinIndex := 1 ;

           FParticles[i].InitialSize := FParticleParam.Size + random( round(FParticleParam.SizeVariation*1000) )*0.001 ;
           FParticles[i].InitialSize := FParticles[i].InitialSize * FParticleParam.ArraySize[0].Value ;
           FParticles[i].CurrentSize := FParticles[i].InitialSize ;
           FParticles[i].FSizeIndex := 1 ;

           offsetX := -FParticleParam.Texture^.FrameWidth*0.5;
           offsetY := -FParticleParam.Texture^.FrameHeight*0.5;
           if not ParticlesPositionsRelativeToEmitterPosition then begin
            offsetX += Self.X.Value;
            offsetY += Self.Y.Value;
           end;
           FParticles[i].CurrentDirection := ( Direction.value + random( _Spread ) - _Spread * 0.5 );
           case FEmitterType of
            etPoint: begin
              FParticles[i].X := offsetX;
              FParticles[i].Y := offsetY;
            end;
            etLine: begin
             siz := random(round(FLineSize*1000))*0.001;
             ang := Direction.Value * deg2rad;
             FParticles[i].X := cos( ang ) * siz + offsetX;
             FParticles[i].Y := sin( ang ) * siz + offsetY;
             FParticles[i].CurrentDirection := FParticles[i].CurrentDirection + 90;
            end;
            etRectangle: begin
             FParticles[i].X := random( Width ) + offsetX;
             FParticles[i].Y := random( Height ) + offsetY;
            end;
            etCircle: begin
             ang := random(360) * deg2rad;
             siz := random(round(FSmallRadius*1000))*0.001;
             FParticles[i].X := cos( ang ) * siz + offsetX;
             FParticles[i].Y := sin( ang ) * siz + offsetY;
            end;
            etInnerCircle: begin
             ang := random(360) * deg2rad;
             FParticles[i].X := cos( ang ) * FSmallRadius + offsetX;
             FParticles[i].Y := sin( ang ) * FSmallRadius + offsetY;
             FParticles[i].CurrentDirection := CartesianToPolar( PointF(FParticles[i].X,FParticles[i].Y), GetXY ).Angle;
            end;
            etOuterCircle: begin
             ang := random(360) * deg2rad;
             FParticles[i].X := cos( ang ) * FSmallRadius + offsetX;
             FParticles[i].Y := sin( ang ) * FSmallRadius + offsetY;
             FParticles[i].CurrentDirection := CartesianToPolar( GetXY, PointF(FParticles[i].X,FParticles[i].Y) ).Angle;
            end;
            etRing: begin
             ang := random(360) * deg2rad;
             siz := random(round((FBigRadius-FSmallRadius)*1000))*0.001+FSmallRadius;
             FParticles[i].X := cos( ang ) * siz + offsetX;
             FParticles[i].Y := sin( ang ) * siz + offsetY;
            end;
           end;//case

           FParticles[i].InitialVelocity := FParticleParam.Velocity + random( round( FParticleParam.VelocityVariation ) ) - FParticleParam.VelocityVariation * 0.5 ;
           FParticles[i].CurrentVelocity := FParticles[i].InitialVelocity + FParticles[i].InitialVelocity * FParticleParam.ArrayVelocity[0].Value ;
           FParticles[i].FVelocityIndex := 1 ;

           FParticles[i].InitialAVelocity := FParticleParam.AVelocity + random( round( FParticleParam.AVelocityVariation ) ) - FParticleParam.AVelocityVariation * 0.5 ;
           FParticles[i].CurrentAVelocity := FParticles[i].InitialAVelocity + FParticles[i].InitialAVelocity * FParticleParam.ArrayAVelocity[0].Value ;
           FParticles[i].FAVelocityIndex := 1 ;

           inc( i ) ;
           dec( PToCreate ) ;
          until PToCreate = 0 ;
         end;
end;

procedure TParticleEmitter.Draw(const aLayerPercentOpacity: single);
var i: integer ;
    a : byte ;
    aa: single;
    {$IFDEF DEBUG_MODE_ON}
      xL,xR,yT,yB: single;
    {$ENDIF}
begin
 if FParticleCount = 0
   then begin
         {$IFDEF DEBUG_MODE_ON}
           DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
         {$ENDIF}
         exit ;
   end;
 if FParticleParam.Texture = NIL then exit ;
 SetBlendMode( FParticleParam.BlendMode );

 TextureManager.BindTexture( FParticleParam.Texture );

 if FParticleParam.ColorMode = FX_COLOR_SET
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
         glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
        end
   else glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

 aa := Opacity.Value/255;

 {$IFDEF DEBUG_MODE_ON}
   xL := X.Value;
   xR := RightX;
   yT := Y.Value;
   yB := BottomY;
 {$ENDIF}


 if ParticlesPositionsRelativeToEmitterPosition then begin
// PushAndPrepareMatrix;
   glPushMatrix;
   if FParentSurface = NIL then glTranslatef( X.Value + Width * 0.5, Y.Value + Height * 0.5 , 0 )
                           else glTranslatef( X.Value + Width * 0.5 * Scale.x.Value, Y.Value + Height * 0.5 * Scale.y.Value, 0 );
   glScalef( Scale.x.Value, Scale.y.Value, 0 );
   glRotatef( Angle.Value, 0, 0, 1 );
   glTranslatef( -Width * 0.5, -Height * 0.5, 0 );
 end;

 for i:=0 to MAX_PARTICLES-1 do
  with FParticles[i] do
   if CurrentLife > 0
     then begin
           a := CurrentColor.alpha;
           CurrentColor.alpha := round ( CurrentColor.alpha * aLayerPercentOpacity * aa );
           DrawParticleTexture( FParticleParam.Texture, round( FrameIndex ), CurrentColor, X, Y, CurrentAngle, CurrentSize );
           CurrentColor.alpha := a;
           {$IFDEF DEBUG_MODE_ON}
             if xL > X then xL := X;
             if xR < X then xR := X;
             if yT > Y then yT := Y;
             if yB < Y then yB := Y;
           {$ENDIF}
     end;

 if ParticlesPositionsRelativeToEmitterPosition then PopMatrix;

 {$IFDEF DEBUG_MODE_ON}
   SetBlendMode(FX_BLEND_NORMAL);
   TextureManager.DisableTextureUsage;
   DrawBox( xL, yT, xR-xL, yB-yT, BGRA(255,255,0));
 {$ENDIF}
end;

procedure TParticleEmitter.LoadFromFile(const aFilename: string);
var F: TStringList;
    i, k, count :integer;
    SplittedText: ArrayOfString;
begin
 Clear;
 F := TStringList.Create;
 try
   F.LoadFromFile( aFilename );

   k := F.IndexOf('E_Coor');
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   x.Value := StrToFloat( SplittedText[0] );
   y.Value := StrToFloat( SplittedText[1] );

   k := F.IndexOf('E_Direction');
   Direction.Value := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_Spread');
   Spread.Value := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_Life');
   EmitterLife := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_LoopMode');
   LoopMode := (F.Strings[k+1] = 'TRUE') or (F.Strings[k+1] = 'True') or (F.Strings[k+1] = 'true');

   k := F.IndexOf('E_Emission');
   ParticlesToEmit.Value := StrToInt( F.Strings[k+1] );
   FParticleRate := 1/ParticlesToEmit.Value;
   FParticleRateCount := 0;

   k := F.IndexOf('E_Gravity');
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   Gravity.x.Value := StrToFloat( SplittedText[0] );
   Gravity.y.Value := StrToFloat( SplittedText[1] );

   k := F.IndexOf('E_GlobalOpacity');
   if k<>-1
     then Opacity.Value := StrToFloat( F.Strings[k+1] )
     else Opacity.Value := 255;

  k := F.IndexOf('E_Type');
  if k<>-1 then begin
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   case strtoint( SplittedText[0] ) of
    1: SetEmitterTypeLine( Direction.Value, StrToFloat(SplittedText[1]) );
    2: SetEmitterTypeRectangle( strtoint(SplittedText[1]), strtoint(SplittedText[2]) );
    3: SetEmitterTypeCircle( StrToFloat(SplittedText[1]) );
    4: SetEmitterTypeRing( StrToFloat(SplittedText[1]), StrToFloat(SplittedText[1]) );
    5: SetEmitterTypeInnerCircle( StrToFloat(SplittedText[1]) );
    6: SetEmitterTypeOuterCircle( StrToFloat(SplittedText[1]) )
    else SetEmitterTypePoint;
   end;//case
  end else SetEmitterTypePoint;

  k := F.IndexOf('P_ParticlesPositionsRelativeToEmitterPosition');
  if k<>-1 then
    ParticlesPositionsRelativeToEmitterPosition := (F.Strings[k+1] = 'TRUE') or (F.Strings[k+1] = 'True') or (F.Strings[k+1] = 'true')
  else ParticlesPositionsRelativeToEmitterPosition:=FALSE;

   k := F.IndexOf('P_Texture');
   SplittedText := SplitLineToStringArray( F.Strings[k+2], ' ' );
   if FParticleParam.Texture <> NIL
     then TextureManager.Delete( FParticleParam.Texture );
   FParticleParam.Texture := TextureManager.Add( ExtractFilePath( aFilename ) + F.Strings[k+1],
                                                 strtoint( SplittedText[0] ), strtoint( SplittedText[1] ));

   k := F.IndexOf('P_Frame');   // StartFrameIndex EndFrameIndex
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.StartFrame := StrToInt( SplittedText[0] );
   FParticleParam.EndFrame := StrToInt( SplittedText[1] );

   k := F.IndexOf('P_Life');  // Life  LifeVariation
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Life := StrToFloat( SplittedText[0] );
   FParticleParam.LifeVariation := StrToFloat( SplittedText[1] );

   k := F.IndexOf('P_ColorMode');
   FParticleParam.ColorMode := StrToint( F.Strings[k+1] );

   k := F.IndexOf('P_BlendMode');
   FParticleParam.BlendMode := StrToint( F.Strings[k+1] );

   k := F.IndexOf('P_Color');    // count   lifepos red green blue alpha    life red green...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   count := StrToInt( SplittedText[0] );
   SetLength( FParticleParam.ArrayColor, count );
   k := 1 ;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayColor[i].Life := StrToFloat( SplittedText[k] ) ;
     FParticleParam.ArrayColor[i].C.red := StrToInt( SplittedText[k+1] ) ;
     FParticleParam.ArrayColor[i].C.green := StrToInt( SplittedText[k+2] ) ;
     FParticleParam.ArrayColor[i].C.blue := StrToInt( SplittedText[k+3] ) ;
     FParticleParam.ArrayColor[i].C.alpha := StrToInt( SplittedText[k+4] ) ;
     inc(k,5);
    end;

   k := F.IndexOf('P_Velocity');  // velocity velocityVariation count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Velocity := StrToFloat( SplittedText[0] );
   FParticleParam.VelocityVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArrayVelocity, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayVelocity[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArrayVelocity[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_AVelocity');     // AngleVelocity AngleVelocityVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.AVelocity := StrToFloat( SplittedText[0] );
   FParticleParam.AVelocityVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArrayAVelocity, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayAVelocity[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArrayAVelocity[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_Size');   // Size SizeVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Size := StrToFloat( SplittedText[0] );
   FParticleParam.SizeVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArraySize, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArraySize[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArraySize[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_StartAngle');  // StartAngle StartAngleVariation
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.StartAngle := StrToFloat( SplittedText[0] );
   FParticleParam.StartAngleVariation := StrToFloat( SplittedText[1] );

   k := F.IndexOf('P_Spin');    // Spin SpinVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Spin := StrToFloat( SplittedText[0] );
   FParticleParam.SpinVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArraySpin, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArraySpin[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArraySpin[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;
 finally
  F.Free ;
 end;
end;

function TParticleEmitter.GetWidth: integer;
begin
 Result := FWidth;
end;

function TParticleEmitter.GetHeight: integer;
begin
 Result := FHeight;
end;

procedure TParticleEmitter.DrawParticleTexture(aTexture: PTexture;
  AFrameIndex: integer; aTint: TBGRAPixel; aX, aY, aAngle, aZoom: single);
var
  quad : array[ 0..3 ] of TPointF;
  TexCoord : TTexCoor;
  aW, aH : Single ;
  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
begin

 if aTexture = nil then exit;
 aW := aTexture^.FrameWidth;
 aH := aTexture^.FrameHeight;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 // Zoom
 if aZoom <> 1 Then
   begin
     aX := aX + ( aW - aW * aZoom ) / 2 ;
     aY := aY + ( aH - aH * aZoom ) / 2 ;
     aW := aW * aZoom ;
     aH := aH * aZoom ;
   end;
 // Angle
 if aAngle <> 0
   then begin
         x1 := -aW / 2;
         y1 := -aH / 2;
         x2 := -x1;
         y2 := -y1;
         cX :=  aX + x2;
         cY :=  aY + y2;
         sincos( aAngle * deg2rad, s, c );
         quad[ 0 ].X := x1 * c - y1 * s + cX; quad[ 0 ].Y := x1 * s + y1 * c + cY;
         quad[ 1 ].X := x2 * c - y1 * s + cX; quad[ 1 ].Y := x2 * s + y1 * c + cY;
         quad[ 2 ].X := x2 * c - y2 * s + cX; quad[ 2 ].Y := x2 * s + y2 * c + cY;
         quad[ 3 ].X := x1 * c - y2 * s + cX; quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end
   else begin
         quad[ 0 ].X := aX; quad[ 0 ].Y := aY;
         quad[ 1 ].X := aX + aW; quad[ 1 ].Y := aY;
         quad[ 2 ].X := aX + aW; quad[ 2 ].Y := aY + aH;
         quad[ 3 ].X := aX; quad[ 3 ].Y := aY + aH;
        end;

 glColor4ub( aTint.red, aTint.green, aTint.blue, aTint.alpha );
 glBegin( GL_QUADS );
  glTexCoord2fv( @TexCoord[0] );  glVertex2fv( @quad[ 0 ] );
  glTexCoord2fv( @TexCoord[1] );  glVertex2fv( @quad[ 1 ] );
  glTexCoord2fv( @TexCoord[2] );  glVertex2fv( @quad[ 2 ] );
  glTexCoord2fv( @TexCoord[3] );  glVertex2fv( @quad[ 3 ] );
 glEnd;
end;

procedure TParticleEmitter.Shoot;
begin
 FCurrentLife := EmitterLife;
 FParticleRateCount := 0;
end;

procedure TParticleEmitter.Clear;
var i: integer ;
begin
 for i:=0 to MAX_PARTICLES-1 do
  begin
   FParticles[i].CurrentLife := 0 ;
   FParticles[i].InitialLife := 0 ;
  end;
 FParticleCount := 0 ;
end;

procedure TParticleEmitter.SetSize(AWidth, AHeight: integer);
begin
 FWidth := AWidth;
 FHeight := AHeight;
end;

procedure TParticleEmitter.SetEmitterTypePoint;
begin
 FEmitterType := etPoint;
end;

procedure TParticleEmitter.SetEmitterTypeLine( P2: TPointF);
var polar: TPolarCoor;
begin
 FEmitterType := etLine;
 FLineP2 := P2;

 polar := CartesianToPolar( GetXY, P2);
 FLineSize := polar.Distance;
 Direction.Value := polar.Angle;
end;

procedure TParticleEmitter.SetEmitterTypeLine(aDirection, aSize: single );
var polar: TPolarCoor;
begin
 polar.Angle:=aDirection;
 polar.Distance:=aSize;
 FLineP2 := PolarToCartesian( GetXY, polar);

 FEmitterType := etLine;
 Direction.Value := aDirection;
 FLineSize := aSize;

end;

procedure TParticleEmitter.SetEmitterTypeRectangle(aWidth, aHeight: integer);
begin
 FEmitterType := etRectangle;
 SetSize( aWidth, aHeight );
end;

procedure TParticleEmitter.SetEmitterTypeCircle(Radius: single);
begin
 FEmitterType := etCircle;
 FSmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeInnerCircle(Radius: single);
begin
 FEmitterType := etInnerCircle;
 FSmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeOuterCircle(Radius: single);
begin
 FEmitterType := etOuterCircle;
 FSmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeRing( SmallRadius,
  BigRadius: single);
begin
 FEmitterType := etRing;
 if SmallRadius < BigRadius then begin
  FSmallRadius := SmallRadius;
  FBigRadius := BigRadius;
 end else begin
  FSmallRadius := BigRadius;
  FBigRadius := SmallRadius;
 end;
end;
{$endif oglcIMPLEMENTATION}

