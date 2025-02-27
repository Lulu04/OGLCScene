{$ifdef oglcINTERFACE}
type

{
case Mode of
  FX_COLOR_MIX:
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    end;
  FX_COLOR_SET:
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB);
    end;
end;

}

{ TOGLCParticleRenderer }

TOGLCParticleRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txyuvtintopacitymatrix>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec4 aVertexAndTextureCoor;'#10+
   '  layout(location = 1) in vec4 aVertexTint;'#10+
   '  layout(location = 2) in float aOpacity;'#10+
   '  layout(location = 3) in mat4 aModelViewMatrix;'#10+
   '  uniform mat4 uProjectionMatrix;'#10+
   '  out vec2 TexCoords;'#10+
   '  flat out vec4 Tint;'#10+
   '  flat out float Opacity;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uProjectionMatrix*aModelViewMatrix*vec4(aVertexAndTextureCoor.xy, 0.0, 1.0);'#10+
   '  TexCoords = aVertexAndTextureCoor.zw;'#10+
   '  Tint = aVertexTint;'#10+
   '  Opacity = aOpacity;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  in vec2 TexCoords;'#10+
   '  flat in vec4 Tint;'#10+
   '  flat in float Opacity;'#10+
   '  uniform sampler2D uTexUnit;'#10+
   'void main()'#10+
   '{'#10+
   '  vec4 col = texture(uTexUnit, TexCoords);'#10+
'  float a = col.a*Opacity;'#10+
   '  if (a == 0.0) discard;'#10+
   // Tint and opacity
//   '  float a = col.a*Opacity;'#10+
   '  float tintAlphaX2 = Tint.a*2;'#10+
   '  if (Tint.a > 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   '     col = col * (1-tintAlphaX2) + Tint * tintAlphaX2;'#10+  // replace color
   '   }'#10+
   '  else if (Tint.a > 0)'#10+
   '   {'#10+
   '     col = col + Tint*tintAlphaX2;'#10+     // mix color
   '   }'#10+

//   '  col = col * (1-Tint.a) + Tint * Tint.a;'#10+
   '  col.a = a;'#10+
   '  FragColor = col;'#10+
   '}' ;
private
  FLocProjectionMatrix,
  FLocTextureUnit: GLint;
  FTexture: PTexture;
  FTextureUnit: integer;
  FProjectionMatrix: TOGLCMatrix;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);

  procedure PushQuad(const aFrameIndex, aFlipIndex: integer; aModelViewMatrix: POGLCMatrix44;
                     const aOpacity: single; const aComputedTint: TColorF);
end;

type
{ TParticleEmitter }
TEmitterType = (etPoint, etLine, etRectangle, etCircle, etRing, etInnerCircle, etOuterCircle);

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
              CurrentAngle: single;
             end;

// called when a particle is at the end of its life
TOnParticleDies= procedure(aPar: PParticle; aParIndex: integer) of object;

PPColor = ^TPColor;
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
  TextureOwner: boolean;
  StartFrame: integer;
  EndFrame: integer;
  Life: single;
  LifeVariation: single;
  ArrayColor: array of TPColor;
  Velocity: single;
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

TEmitterParameter = record
  EmitterType: TEmitterType;
  LineSize: single;
  SmallRadius, BigRadius: single;
end;

TParticleEmitter = class(TSimpleSurfaceWithEffect)
private
  FOnParticleDies: TOnParticleDies;
  FParticlesPosRelativeToEmitterPos: boolean;
  procedure SetParticlesPosRelativeToEmitterPos(AValue: boolean);
protected
  FCurrentLife: single;
  FParticleRate: single;
  FParticleRateCount: single;
  FParticleCount: integer;
  FEmitterType: TEmitterType;
  FWidth, FHeight: integer;
  FLineSize: single;
  FSmallRadius, FBigRadius: single;
  procedure SetSize(AWidth, AHeight: integer);
  function GetWidth: integer; override;
  function GetHeight: integer; override;
  property LineSize: single read FLineSize;
  property SmallRadius: single read FSmallRadius;
  property BigRadius: single read FBigRadius;
public
  procedure Update(const aElapsedTime: single); override;
  procedure Draw(const aLayerPercentOpacity: single); override;
  procedure DoDraw; override;
public
  // acces to particle parameters
  FParticles: array [0..MAX_PARTICLES-1] of TParticle;
  FParticleParam: TParticleParameter;
  EmitterParam: TEmitterParameter;
public
  ParticlesToEmit: TBoundedFParam; // max particles count to emit at the same time [0..1024]
  Gravity: TPointFParam;           // gravity in pixels per second
  Direction: TBoundedFParam;       // [0..360] degree
  Spread: TBoundedFParam;          // [0..360] degree
  EmitterLife: single;             // duration in seconds of one shoot if emitter is not in loop mode
  LoopMode: boolean;
  Constructor Create(aParentScene: TOGLCScene);
  Destructor Destroy; override;
  // Load the parameters from a file '.par' (made with the tool ParticleEditor)
  // You can specify an atlas where the used texture can be found.
  // If you don't use an atlas, set aAtlas parameter to NIL. In that case, the texture
  // will be created (the texture image must be in the same directory than aFilename).
  procedure LoadFromFile(const aFilename: string; aAtlas: TOGLCTextureAtlas);
  // When the emitter is not looped (property LoopMode to False), call this method to start
  // the particle creation.
  procedure Shoot;
  // Kill all particles currently active. If the emitter is in LoopMode,
  // it continue to create new particles immediatly.
  procedure Clear;

  // (X,Y) is the position of the point
  procedure SetEmitterTypePoint;
  // (X,Y) is the first point of the line
  procedure SetEmitterTypeLine(P2: TPointF);
  // (X,Y) is the first point of the line
  procedure SetEmitterTypeLine(aDirection, aSize: single);
  // (X,Y) is the top left corner of the rectangle
  procedure SetEmitterTypeRectangle(aWidth, aHeight: integer);
  // (X,Y) is the center of the circle
  procedure SetEmitterTypeCircle(Radius: single);
  procedure SetEmitterTypeInnerCircle(Radius: single);
  procedure SetEmitterTypeOuterCircle(Radius: single);
  procedure SetEmitterTypeRing(aSmallRadius, aBigRadius: single);

  // Return TRUE if at least one particle collide the specified surface
  function ParticlesCollisionTestWith(aSurface: TSimpleSurfaceWithEffect): boolean;

  // Sets this property to True if you want the particles follow the moves of the emitter.
  // Sets it to False to have the particles independant from the emitter position. Default is False.
  property ParticlesPosRelativeToEmitterPos: boolean read FParticlesPosRelativeToEmitterPos write SetParticlesPosRelativeToEmitterPos;
  property EmitterType: TEmitterType read FEmitterType;
  // The number of particles currently active
  property ParticlesCount: integer read FParticleCount;
  // Fired when a particle dies
  property OnParticleDies: TOnParticleDies read FOnParticleDies write FOnParticleDies;
end;

ArrayofParticleEmitter = array of TParticleEmitter;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCParticleRenderer }

procedure TOGLCParticleRenderer.InitShaderCodeAndCallBack;
begin
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCParticleRenderer.DefineVertexAttribPointer;
begin
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), NIL); // xyuv
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*4));  // color
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*8));  // opacity
  glEnableVertexAttribArray(2);

  glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*9));  // model view matrix
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*13));
  glEnableVertexAttribArray(4);
  glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*17));
  glEnableVertexAttribArray(5);
  glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, sizeof(Txyuvtintopacitymatrix), PChar(SizeOf(single)*21));
  glEnableVertexAttribArray(6);
end;

procedure TOGLCParticleRenderer.GetUniformLocation;
begin
  with FShader do
  begin
    Use;
    FLocProjectionMatrix := GetUniform('uProjectionMatrix');
    FLocTextureUnit := GetUniform('uTexUnit');
    Release;
  end;
end;

procedure TOGLCParticleRenderer.SetUniformValuesAndTexture;
begin
  glUniform1i(FLocTextureUnit, FTextureUnit);
  glUniformMatrix4fv(FLocProjectionMatrix, 1, GL_FALSE, @FProjectionMatrix.Matrix[0,0]);
  FParentScene.TexMan.Bind(FTexture, FTextureUnit);
end;

procedure TOGLCParticleRenderer.Prepare(aTexture: PTexture; aTextureUnit: integer; aBlendMode: byte);
var forceFlush: Boolean;
begin
  forceFlush := not FProjectionMatrix.EqualTo(FParentScene.ProjectionMatrix);
  Batch_CheckIfNeedFlush(Self, ptTriangles, aTexture, aTextureUnit, aBlendMode, forceFlush);
  FTexture := aTexture;
  FTextureUnit := aTextureUnit;
  if forceFlush then FProjectionMatrix.CopyFrom(FParentScene.ProjectionMatrix);
end;

procedure TOGLCParticleRenderer.PushQuad(const aFrameIndex,
  aFlipIndex: integer; aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF);
var w, h: single;
    tci: PQuadCornerIndexes;
    texCoord: PQuadCoor;
    p: Pxyuvtintopacitymatrix;
    fi: Integer;
    pIndex: PVertexIndex;
    currentIndex: TVertexIndex;
begin
 // if aModelViewMatrix = NIL then aModelViewMatrix := @FParentScene.ModelViewMatrix.Matrix;

  fi := EnsureRange(aFrameIndex, 0, FTexture^.FrameCount-1);
  texCoord := @FTexture^.FramesCoord[fi];
  tci := @FLIP_INDEXES[aFlipIndex];
  w := FTexture^.FrameWidth;
  h := FTexture^.FrameHeight;

  // push the 6 indices
  currentIndex := FIndexInAttribsArray;
  pIndex := QueryIndex(6);      // B   BD
  pIndex[0] := currentIndex;    // AC  C
  pIndex[1] := currentIndex+1;
  pIndex[2] := currentIndex+2;
  pIndex[3] := currentIndex+2;
  pIndex[4] := currentIndex+1;
  pIndex[5] := currentIndex+3;

  // push the 4 vertex        // vertex coord      24
  p := QueryVertex(4);        //                   13
  with p[0] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoord^[ tci^[0] ].x;
    v := texCoord^[ tci^[0] ].y;
    x := 0;
    y := h;
    opacity := aOpacity;
  end;
  with p[1] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoord^[ tci^[1] ].x;
    v := texCoord^[ tci^[1] ].y;
    x := 0;
    y := 0;
    opacity := aOpacity;
  end;
  with p[2] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoord^[ tci^[2] ].x;
    v := texCoord^[ tci^[2] ].y;
    x := w;
    y := h;
    opacity := aOpacity;
  end;
  with p[3] do begin
    tint.CopyFrom(aComputedTint);
    mv.CopyFrom(aModelViewMatrix);
    u := texCoord^[ tci^[3] ].x;
    v := texCoord^[ tci^[3] ].y;
    x := w;
    y := 0;
    opacity := aOpacity;
  end;

exit;

  case FBatch^.CurrentPrimitiveType of
    ptTriangleStrip: begin
      p := QueryVertex(4);
      with p[0] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[0] ].x;
        v := texCoord^[ tci^[0] ].y;
        x := 0;
        y := h;
        opacity := aOpacity;
      end;
      with p[1] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := 0;
        y := 0;
        opacity := aOpacity;
      end;
      with p[2] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w;
        y := h;
        opacity := aOpacity;
      end;
      with p[3] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w;
        y := 0;
        opacity := aOpacity;
      end;
    end;

    ptTriangles: begin
      p := QueryVertex(6);
      with p[0] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[0] ].x;
        v := texCoord^[ tci^[0] ].y;
        x := 0;
        y := h;
        opacity := aOpacity;
      end;
      with p[1] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[1] ].x;
        v := texCoord^[ tci^[1] ].y;
        x := 0;
        y := 0;
        opacity := aOpacity;
      end;
      with p[2] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[2] ].x;
        v := texCoord^[ tci^[2] ].y;
        x := w;
        y := h;
        opacity := aOpacity;
      end;
      Move(p[2], p[3], SizeOf(Txyuvtintopacitymatrix)); // C
      Move(p[1], p[4], SizeOf(Txyuvtintopacitymatrix)); // B
      with p[5] do begin
        tint.CopyFrom(aComputedTint);
        mv.CopyFrom(aModelViewMatrix);
        u := texCoord^[ tci^[3] ].x;
        v := texCoord^[ tci^[3] ].y;
        x := w;
        y := 0;
        opacity := aOpacity;
      end;
    end;
  end;
end;

{ TParticleEmitter }

constructor TParticleEmitter.Create(aParentScene: TOGLCScene);
var i:integer ;
begin
  inherited Create;
  FParentScene := aParentScene;
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
  ParticlesToEmit := CreateBoundedFParam(0, 1024);
  Gravity := TPointFParam.Create;
  Spread := CreateBoundedFParam(0, 360, TRUE);
  Direction := CreateBoundedFParam(0, 360, TRUE);
  ParticlesPosRelativeToEmitterPos := FALSE;
  SetSize(1, 1);
end;

destructor TParticleEmitter.Destroy;
begin
  if FParticleParam.TextureOwner then FParentScene.TexMan.Delete(FParticleParam.Texture);
  ParticlesToEmit.Free;
  Gravity.Free;
  Spread.Free;
  Direction.Free;
  inherited Destroy;
end;

procedure TParticleEmitter.Update(const aElapsedTime: single);
var i, PToCreate, _Spread: integer;
    iLife, coeff, ang, siz: single;
    pc, nc: PPColor;
    ps, ns: PPSingle;
    offsetX, offsetY: single;
begin
  inherited Update(aElapsedTime);
  if FFreeze then exit;
  if FParticleParam.Texture = NIL then exit;

  // update parameters
  Gravity.OnElapse(aElapsedTime);
  ParticlesToEmit.OnElapse(aElapsedTime);
  Spread.OnElapse(aElapsedTime);
  Direction.OnElapse(aElapsedTime);
  _Spread := Round(Spread.Value);

  // update particles
  for i:=0 to MAX_PARTICLES-1 do
   with FParticles[i], FParticleParam do begin
     if CurrentLife > 0 then begin
       CurrentLife -= aElapsedTime;
       if CurrentLife <= 0 then begin
         dec(FParticleCount);
         if FOnParticleDies <> NIL then FOnParticleDies(@FParticles[i], i);
       end
       else begin
         iLife := (InitialLife - CurrentLife) / InitialLife;
         // frame index
         FrameIndex := FrameIndex + FrameCoeff * aElapsedTime;
         // color
         while ArrayColor[FColorIndex].Life < iLife do inc(FColorIndex);
         pc := @ArrayColor[FColorIndex-1];
         nc := @ArrayColor[FColorIndex];
         coeff := (iLife - pc^.Life) / (nc^.Life - pc^.Life);
         if coeff < 0 then coeff := 0; // necessary for tool Particles Editor
         CurrentColor.red := pc^.C.red + round((nc^.C.red - pc^.C.red) * coeff);
         CurrentColor.green := pc^.C.green + round(( nc^.C.green - pc^.C.green ) * coeff);
         CurrentColor.blue := pc^.C.blue + round((nc^.C.blue - pc^.C.blue) * coeff );
         CurrentColor.alpha := pc^.C.alpha + round((nc^.C.alpha - pc^.C.alpha) * coeff);
         // size
         while ArraySize[FSizeIndex].Life < iLife do inc(FSizeIndex);
         ps := @ArraySize[FSizeIndex-1];
         ns := @ArraySize[FSizeIndex];
         CurrentSize := InitialSize * (ps^.Value + (ns^.Value - ps^.Value) * (iLife - ps^.Life) / (ns^.Life - ps^.Life));
         // Spin
         while ArraySpin[FSpinIndex].Life < iLife do inc(FSpinIndex);
         ps := @ArraySpin[FSpinIndex-1] ;
         ns := @ArraySpin[FSpinIndex] ;
         CurrentAngle := CurrentAngle + InitialSpin * (ps^.Value + (ns^.Value - ps^.Value) * (iLife - ps^.Life) / (ns^.Life - ps^.Life)){ * aElapsedTime};
         // Velocity
         while ArrayVelocity[FVelocityIndex].Life < iLife do inc(FVelocityIndex);
         ps := @ArrayVelocity[FVelocityIndex-1] ;
         ns := @ArrayVelocity[FVelocityIndex] ;
         CurrentVelocity := InitialVelocity + InitialVelocity * (ps^.Value + (ns^.Value - ps^.Value) * (iLife - ps^.Life) / (ns^.Life - ps^.Life));
         // Angular Velocity
         while ArrayAVelocity[FAVelocityIndex].Life < iLife do inc(FAVelocityIndex);
         ps := @ArrayAVelocity[ FAVelocityIndex-1 ];
         ns := @ArrayAVelocity[ FAVelocityIndex ];
         CurrentAVelocity := InitialAVelocity + InitialAVelocity * (ps^.Value + (ns^.Value - ps^.Value) * (iLife - ps^.Life) / (ns^.Life - ps^.Life));
         CurrentDirection += CurrentAVelocity * aElapsedTime;

         VX := cos(CurrentDirection * deg2rad) * CurrentVelocity;
         VY := sin(CurrentDirection * deg2rad) * CurrentVelocity;
         // Gravity
         X += (VX + Gravity.x.Value) * aElapsedTime;
         Y += (VY + Gravity.y.Value) * aElapsedTime;
       end;
     end;
   end;

  // Emitter life
  FCurrentLife -= aElapsedTime;
  if not LoopMode and (FCurrentLife <= 0) then begin
    FCurrentLife := 0;
    exit;
  end;

  // create new particles
  if ParticlesToEmit.Value = 0.0 then exit;
  FParticleRate := 1/ParticlesToEmit.Value;
  FParticleRateCount += aElapsedTime;
  PToCreate := 0;
  while FParticleRateCount > FParticleRate do begin
    FParticleRateCount -= FParticleRate;
    inc(PToCreate);
  end;
  if FParticleCount + PToCreate > MAX_PARTICLES then PToCreate := MAX_PARTICLES - FParticleCount;
  if PToCreate > 0 then begin
    FParticleCount += PToCreate;
    i := 0;
    repeat
      while FParticles[i].CurrentLife > 0 do inc(i);
      FParticles[i].InitialLife := FParticleParam.Life + random(round(FParticleParam.LifeVariation*1000))*0.001-FParticleParam.LifeVariation*0.5;
      FParticles[i].CurrentLife := FParticles[i].InitialLife;

      FParticles[i].FrameIndex := FParticleParam.StartFrame;
      FParticles[i].FrameCoeff := (FParticleParam.EndFrame - FParticleParam.StartFrame) / FParticles[i].InitialLife;

      FParticles[i].CurrentColor := FParticleParam.ArrayColor[0].C;
      FParticles[i].FColorIndex := 1;

      FParticles[i].CurrentAngle := FParticleParam.StartAngle + random(round(FParticleParam.StartAngleVariation*1000))*0.001;
      //FParticles[i].CurrentAngle := FParticleParam.StartAngle + random*FParticleParam.StartAngleVariation-FParticleParam.StartAngleVariation*0.5; // random(round(FParticleParam.StartAngleVariation*1000))*0.001;

      FParticles[i].InitialSpin := FParticleParam.Spin + random*FParticleParam.SpinVariation-FParticleParam.SpinVariation*0.5; // random(round(FParticleParam.SpinVariation*1000))*0.001;
      FParticles[i].FSpinIndex := 1;

      FParticles[i].InitialSize := FParticleParam.Size + random*FParticleParam.SizeVariation-FParticleParam.SizeVariation*0.5; //+ random(round(FParticleParam.SizeVariation*1000))*0.001;
      FParticles[i].InitialSize := FParticles[i].InitialSize * FParticleParam.ArraySize[0].Value;
      FParticles[i].CurrentSize := FParticles[i].InitialSize;
      FParticles[i].FSizeIndex := 1;

      offsetX := -FParticleParam.Texture^.FrameWidth*0.5;
      offsetY := -FParticleParam.Texture^.FrameHeight*0.5;
      if not ParticlesPosRelativeToEmitterPos then begin
        offsetX += Self.X.Value;
        offsetY += Self.Y.Value;
      end;
      FParticles[i].CurrentDirection := Direction.value + random(_Spread) - _Spread * 0.5 - 90;
      case FEmitterType of
        etPoint: begin
          FParticles[i].X := offsetX;
          FParticles[i].Y := offsetY;
        end;
        etLine: begin
          siz := random(round(FLineSize*1000))*0.001;
          ang := Direction.Value * deg2rad;
          FParticles[i].X := cos(ang) * siz + offsetX;
          FParticles[i].Y := sin(ang) * siz + offsetY;
        end;
        etRectangle: begin
          FParticles[i].X := random(Width) + offsetX;
          FParticles[i].Y := random(Height) + offsetY;
        end;
        etCircle: begin
          ang := random(360) * deg2rad;
          siz := random(round(FSmallRadius*1000))*0.001;
          FParticles[i].X := cos(ang) * siz + offsetX;
          FParticles[i].Y := sin(ang) * siz + offsetY;
        end;
        etInnerCircle: begin
          ang := random(360) * deg2rad;
          FParticles[i].X := cos(ang) * FSmallRadius + offsetX;
          FParticles[i].Y := sin(ang) * FSmallRadius + offsetY;
          FParticles[i].CurrentDirection := CartesianToPolar(PointF(FParticles[i].X,FParticles[i].Y), GetXY ).Angle;
        end;
        etOuterCircle: begin
          ang := random(360) * deg2rad;
          FParticles[i].X := cos(ang) * FSmallRadius + offsetX;
          FParticles[i].Y := sin(ang) * FSmallRadius + offsetY;
          FParticles[i].CurrentDirection := CartesianToPolar( GetXY, PointF(FParticles[i].X,FParticles[i].Y) ).Angle;
        end;
        etRing: begin
          ang := random(360) * deg2rad;
          siz := random(round((FBigRadius-FSmallRadius)*1000))*0.001+FSmallRadius;
          FParticles[i].X := cos(ang) * siz + offsetX;
          FParticles[i].Y := sin(ang) * siz + offsetY;
        end;
      end;//case

      FParticles[i].InitialVelocity := FParticleParam.Velocity + random*FParticleParam.VelocityVariation - FParticleParam.VelocityVariation * 0.5;
      //FParticles[i].InitialVelocity := FParticleParam.Velocity + random(round(FParticleParam.VelocityVariation)) - FParticleParam.VelocityVariation * 0.5;
      FParticles[i].CurrentVelocity := FParticles[i].InitialVelocity + FParticles[i].InitialVelocity * FParticleParam.ArrayVelocity[0].Value;
      FParticles[i].FVelocityIndex := 1;

      FParticles[i].InitialAVelocity := FParticleParam.AVelocity + random * FParticleParam.AVelocityVariation - FParticleParam.AVelocityVariation * 0.5;
      //FParticles[i].InitialAVelocity := FParticleParam.AVelocity + random*FParticleParam.AVelocityVariation - FParticleParam.AVelocityVariation * 0.5;
      FParticles[i].CurrentAVelocity := FParticles[i].InitialAVelocity + FParticles[i].InitialAVelocity * FParticleParam.ArrayAVelocity[0].Value;
      FParticles[i].FAVelocityIndex := 1;

      inc(i);
      dec(PToCreate);
    until PToCreate = 0;
  end;
end;

procedure TParticleEmitter.Draw(const aLayerPercentOpacity: single);
var i: integer ;
    {$IFDEF DEBUG_MODE_ON}
      xL,xR,yT,yB: single;
    {$ENDIF}
    computedTint: TColorF;
    computedOpacity: single;
    pivotX, pivotY: single;
    mv: TOGLCMatrix;
    renderer: TOGLCParticleRenderer;
begin
  if FParticleParam.Texture = NIL then exit;
  if FParticleCount = 0 then begin
    {$IFDEF DEBUG_MODE_ON}
    DrawBox(FParentScene, X.Value, Y.Value, Width, Height, BGRA(255,0,0));
    {$ENDIF}
    exit;
  end;
  {$IFDEF DEBUG_MODE_ON}
    xL := X.Value;
    xR := RightX;
    yT := Y.Value;
    yB := BottomY;
  {$ENDIF}

  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if ParticlesPosRelativeToEmitterPos then PushAndPrepareSceneMV;

  renderer := FParentScene.ParticleRenderer;
  renderer.Prepare(FParticleParam.Texture, 0, FBlendMode);

  pivotX := FParticleParam.Texture^.FrameWidth*0.5;
  pivotY := FParticleParam.Texture^.FrameHeight*0.5;
  for i:=0 to MAX_PARTICLES-1 do
   with FParticles[i] do
    if CurrentLife > 0 then begin
      mv.CopyFrom(FParentScene.ModelViewMatrix);
      mv.AddTransform(X, Y, CurrentSize, CurrentSize, CurrentAngle, pivotX, pivotY);
      computedTint.InitFromBGRA(CurrentColor);
      computedTint.a := 1.0;
      //computedTint.InitFromTint(TintMode, CurrentColor);
      computedOpacity := FComputedOpacity*CurrentColor.alpha*OneDiv255;
      renderer.PushQuad(Trunc(FrameIndex), 0, @mv.Matrix, computedOpacity, computedTint);
      {$IFDEF DEBUG_MODE_ON}
       if xL > X then xL := X;
       if xR < X then xR := X;
       if yT > Y then yT := Y;
       if yB < Y then yB := Y;
      {$ENDIF}
    end;

  if ParticlesPosRelativeToEmitterPos then PopSceneMV;

  {$IFDEF DEBUG_MODE_ON}
    SetBlendMode(FX_BLEND_NORMAL);
    DrawBox(FParentScene, xL, yT, xR-xL, yB-yT, BGRA(255,0,0));
  {$ENDIF}
end;

procedure TParticleEmitter.DoDraw;
begin
end;

procedure TParticleEmitter.LoadFromFile(const aFilename: string; aAtlas: TOGLCTextureAtlas);
var F: TStringList;
    i, k, count :integer;
    SplittedText: TStringArray;
    fileToLoad: String;
    ima: TBGRABitmap;
begin
  Clear;
  F := TStringList.Create;
  try
    F.LoadFromFile(aFilename);

    k := F.IndexOf('E_Coor');
    SplittedText := F.Strings[k+1].Split([' ']);
    x.Value := StringToSingle(SplittedText[0]);
    y.Value := StringToSingle(SplittedText[1]);

    k := F.IndexOf('E_Direction');
    Direction.Value := StringToSingle(F.Strings[k+1]);

   k := F.IndexOf('E_Spread');
   Spread.Value := StringToSingle(F.Strings[k+1]);

   k := F.IndexOf('E_Life');
   EmitterLife := StringToSingle(F.Strings[k+1]);

   k := F.IndexOf('E_LoopMode');
   LoopMode := (F.Strings[k+1] = 'TRUE') or (F.Strings[k+1] = 'True') or (F.Strings[k+1] = 'true');

   k := F.IndexOf('E_Emission');
   ParticlesToEmit.Value := F.Strings[k+1].ToInteger;
   FParticleRate := 1/ParticlesToEmit.Value;
   FParticleRateCount := 0;

   k := F.IndexOf('E_Gravity');
   SplittedText := F.Strings[k+1].Split([' ']);
   Gravity.x.Value := StringToSingle(SplittedText[0]);
   Gravity.y.Value := StringToSingle(SplittedText[1]);

   k := F.IndexOf('E_GlobalOpacity');
   if k<>-1
     then Opacity.Value := StringToSingle(F.Strings[k+1])
     else Opacity.Value := 255;

  k := F.IndexOf('E_Type');
  if k<>-1 then
  begin
    SplittedText := F.Strings[k+1].Split([' ']);
    case SplittedText[0].ToInteger of
     1: SetEmitterTypeLine(Direction.Value, StringToSingle(SplittedText[1]));
     2: SetEmitterTypeRectangle(SplittedText[1].ToInteger, SplittedText[2].ToInteger);
     3: SetEmitterTypeCircle(StringToSingle(SplittedText[1]));
     4: SetEmitterTypeRing(StringToSingle(SplittedText[1]), StringToSingle(SplittedText[1]));
     5: SetEmitterTypeInnerCircle(StringToSingle(SplittedText[1]));
     6: SetEmitterTypeOuterCircle(StringToSingle(SplittedText[1]))
     else SetEmitterTypePoint;
    end;//case
  end else SetEmitterTypePoint;

  k := F.IndexOf('P_ParticlesPositionsRelativeToEmitterPosition');
  if k<>-1 then
    ParticlesPosRelativeToEmitterPos := LowerCase(F.Strings[k+1]) = 'true'
  else ParticlesPosRelativeToEmitterPos := FALSE;

   k := F.IndexOf('P_Texture');
   SplittedText := F.Strings[k+2].Split([' ']);
   if (FParticleParam.Texture <> NIL) and FParticleParam.TextureOwner then
     FParentScene.TexMan.Delete(FParticleParam.Texture);
   if aAtlas = NIL then begin
     // construct the texture from image file
     fileToLoad := ExtractFilePath(aFilename) + F.Strings[k+1];
     if LowerCase(ExtractFileExt(fileToLoad)) = '.svg' then begin
        try
         ima := LoadBitmapFromSVGScaledPPI(FParentScene, fileToLoad);
         FParticleParam.Texture := FParentScene.TexMan.Add(ima,
                                            FParentScene.ScaleDesignToScene(SplittedText[0].ToInteger),
                                            FParentScene.ScaleDesignToScene(SplittedText[1].ToInteger));
         ima.Free;
        except
          FParentScene.LogError('TParticleEmitter.LoadFromFile: fail to load texture from svg file');
          FParentScene.LogMess('particle file: '+aFilename, 1);
          FParentScene.LogMess('texture filename: '+F.Strings[k+1], 1);
        end;

     end else begin
       // load bitmap and resize it in respect of monitor PPI
       try
         ima := LoadBitmapScaledPPI(FParentScene, fileToLoad);
         FParticleParam.Texture := FParentScene.TexMan.Add(ima,
                                            FParentScene.ScaleDesignToScene(SplittedText[0].ToInteger),
                                            FParentScene.ScaleDesignToScene(SplittedText[1].ToInteger));
         ima.Free;
       except
         FParentScene.LogError('TParticleEmitter.LoadFromFile: fail to load texture from file');
         FParentScene.LogMess('particle file: '+aFilename, 1);
         FParentScene.LogMess('texture filename: '+F.Strings[k+1], 1);
       end;
     end;
     if FParticleParam.Texture <> NIL then
       FParticleParam.Texture^.Filename := ExtractFileName(fileToLoad);
     FParticleParam.TextureOwner := True;
   end else begin
     // retrieves the texture from the atlas
     FParticleParam.Texture := aAtlas.RetrieveTextureByFileName(F.Strings[k+1]);
     FParticleParam.TextureOwner := False;
     if FParticleParam.Texture = NIL then begin
       FParentScene.LogError('TParticleEmitter.LoadFromFile: texture not found in atlas');
       FParentScene.LogMess('particle file: '+aFilename, 1);
       FParentScene.LogMess('texture filename: '+F.Strings[k+1], 1);
     end;
   end;

   k := F.IndexOf('P_Frame');   // StartFrameIndex EndFrameIndex
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.StartFrame := SplittedText[0].ToInteger;
   FParticleParam.EndFrame := SplittedText[1].ToInteger;

   k := F.IndexOf('P_Life');  // Life  LifeVariation
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.Life := StringToSingle(SplittedText[0]);
   FParticleParam.LifeVariation := StringToSingle(SplittedText[1]);

   k := F.IndexOf('P_ColorMode');
   TintMode := TTintMode(F.Strings[k+1].ToInteger);

   k := F.IndexOf('P_BlendMode');
   FBlendMode := F.Strings[k+1].ToInteger;

   k := F.IndexOf('P_Color');    // count   lifepos red green blue alpha    life red green...
   SplittedText := F.Strings[k+1].Split([' ']);
   count := SplittedText[0].ToInteger;
   SetLength(FParticleParam.ArrayColor, count);
   k := 1 ;
   for i:=0 to count-1 do
    begin
     if i = 0 then FParticleParam.ArrayColor[i].Life := 0.0
       else if i = count-1 then FParticleParam.ArrayColor[i].Life := 1.0
         else FParticleParam.ArrayColor[i].Life := StringToSingle(SplittedText[k]);
     FParticleParam.ArrayColor[i].C.red := SplittedText[k+1].ToInteger;
     FParticleParam.ArrayColor[i].C.green := SplittedText[k+2].ToInteger;
     FParticleParam.ArrayColor[i].C.blue := SplittedText[k+3].ToInteger;
     FParticleParam.ArrayColor[i].C.alpha := SplittedText[k+4].ToInteger;
     inc(k, 5);
    end;

   k := F.IndexOf('P_Velocity');  // velocity velocityVariation count   lifepos value   lifepos value...
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.Velocity := StringToSingle(SplittedText[0]);
   FParticleParam.VelocityVariation := StringToSingle(SplittedText[1]);
   count := SplittedText[2].ToInteger;
   SetLength(FParticleParam.ArrayVelocity, count);
   k := 3;
   for i:=0 to count-1 do
    begin
     if i = 0 then FParticleParam.ArrayVelocity[i].Life := 0.0
       else if i = count-1 then FParticleParam.ArrayVelocity[i].Life := 1.0
         else FParticleParam.ArrayVelocity[i].Life := StringToSingle(SplittedText[k]);
     FParticleParam.ArrayVelocity[i].Value := StringToSingle(SplittedText[k+1]);
     inc(k, 2);
    end;

   k := F.IndexOf('P_AVelocity');     // AngleVelocity AngleVelocityVariation Count   lifepos value   lifepos value...
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.AVelocity := StringToSingle(SplittedText[0]);
   FParticleParam.AVelocityVariation := StringToSingle(SplittedText[1]);
   count := SplittedText[2].ToInteger;
   SetLength(FParticleParam.ArrayAVelocity, count);
   k := 3;
   for i:=0 to count-1 do
    begin
     if i = 0 then FParticleParam.ArrayAVelocity[i].Life := 0.0
       else if i = count-1 then FParticleParam.ArrayAVelocity[i].Life := 1.0
         else FParticleParam.ArrayAVelocity[i].Life := StringToSingle(SplittedText[k]);
     FParticleParam.ArrayAVelocity[i].Value := StringToSingle(SplittedText[k+1]);
     inc(k, 2);
    end;

   k := F.IndexOf('P_Size');   // Size SizeVariation Count   lifepos value   lifepos value...
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.Size := StringToSingle(SplittedText[0]);
   FParticleParam.SizeVariation := StringToSingle(SplittedText[1]);
   count := SplittedText[2].ToInteger;
   SetLength(FParticleParam.ArraySize, count);
   k := 3;
   for i:=0 to count-1 do
    begin
     if i = 0 then FParticleParam.ArraySize[i].Life := 0.0
       else if i = count-1 then FParticleParam.ArraySize[i].Life := 1.0
         else FParticleParam.ArraySize[i].Life := StringToSingle(SplittedText[k]);
     FParticleParam.ArraySize[i].Value := StringToSingle(SplittedText[k+1]);
     inc(k, 2);
    end;

   k := F.IndexOf('P_StartAngle');  // StartAngle StartAngleVariation
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.StartAngle := StringToSingle(SplittedText[0]);
   FParticleParam.StartAngleVariation := StringToSingle(SplittedText[1]);

   k := F.IndexOf('P_Spin');    // Spin SpinVariation Count   lifepos value   lifepos value...
   SplittedText := F.Strings[k+1].Split([' ']);
   FParticleParam.Spin := StringToSingle(SplittedText[0]);
   FParticleParam.SpinVariation := StringToSingle(SplittedText[1]);
   count := SplittedText[2].ToInteger;
   SetLength(FParticleParam.ArraySpin, count);
   k := 3;
   for i:=0 to count-1 do
    begin
     if i = 0 then FParticleParam.ArraySpin[i].Life := 0.0
       else if i = count-1 then FParticleParam.ArraySpin[i].Life := 1.0
         else FParticleParam.ArraySpin[i].Life := StringToSingle(SplittedText[k]);
     FParticleParam.ArraySpin[i].Value := StringToSingle(SplittedText[k+1]);
     inc(k, 2);
    end;
 finally
  F.Free;
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

procedure TParticleEmitter.Shoot;
begin
  FCurrentLife := EmitterLife;
  FParticleRateCount := 0;
end;

procedure TParticleEmitter.Clear;
var i: integer;
begin
  for i:=0 to MAX_PARTICLES-1 do
  begin
    FParticles[i].CurrentLife := 0;
    FParticles[i].InitialLife := 0;
  end;
  FParticleCount := 0 ;
end;

procedure TParticleEmitter.SetParticlesPosRelativeToEmitterPos(AValue: boolean);
begin
  if FParticlesPosRelativeToEmitterPos = AValue then Exit;
  FParticlesPosRelativeToEmitterPos := AValue;
  Clear;
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

procedure TParticleEmitter.SetEmitterTypeLine(P2: TPointF);
var polar: TPolarCoor;
begin
  FEmitterType := etLine;
  polar := CartesianToPolar(GetXY, P2);
  FLineSize := polar.Distance;
  Direction.Value := polar.Angle;
  EmitterParam.LineSize := FLineSize;
end;

procedure TParticleEmitter.SetEmitterTypeLine(aDirection, aSize: single);
begin
  FEmitterType := etLine;
  Direction.Value := aDirection;
  FLineSize := aSize;
  EmitterParam.LineSize := aSize;
end;

procedure TParticleEmitter.SetEmitterTypeRectangle(aWidth, aHeight: integer);
begin
  FEmitterType := etRectangle;
  SetSize(aWidth, aHeight);
end;

procedure TParticleEmitter.SetEmitterTypeCircle(Radius: single);
begin
  FEmitterType := etCircle;
  FSmallRadius := Radius;
  EmitterParam.SmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeInnerCircle(Radius: single);
begin
  FEmitterType := etInnerCircle;
  FSmallRadius := Radius;
  EmitterParam.SmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeOuterCircle(Radius: single);
begin
  FEmitterType := etOuterCircle;
  FSmallRadius := Radius;
  EmitterParam.SmallRadius := Radius;
end;

procedure TParticleEmitter.SetEmitterTypeRing(aSmallRadius, aBigRadius: single);
begin
  FEmitterType := etRing;
  if aSmallRadius < aBigRadius then begin
    FSmallRadius := aSmallRadius;
    FBigRadius := aBigRadius;
  end else begin
    FSmallRadius := aBigRadius;
    FBigRadius := aSmallRadius;
  end;
  EmitterParam.SmallRadius := FSmallRadius;
  EmitterParam.BigRadius := FBigRadius;
end;

function TParticleEmitter.ParticlesCollisionTestWith( aSurface: TSimpleSurfaceWithEffect): boolean;
var i: Integer;
begin
 Result := FALSE;
 for i:=0 to length(FParticles)-1 do begin
  if FParticles[i].CurrentLife > 0
   then Result := aSurface.CollisionTestWith(FParticles[i].X, FParticles[i].Y,
                                     FParticleParam.Texture^.FrameWidth*FParticles[i].CurrentSize,
                                     FParticleParam.Texture^.FrameHeight*FParticles[i].CurrentSize);
  if Result then exit;
 end;
end;

{$endif oglcIMPLEMENTATION}

