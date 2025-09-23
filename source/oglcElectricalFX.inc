{$ifdef oglcINTERFACE}
type

{ TOGLCElectricalBeamRenderer }

TOGLCElectricalBeamRenderer = class(specialize TOGLCGenericPrimitiveRenderer<Txydistance>)
private const
  VERTEX_SHADER =
   '#version 330 core'#10+
   '  layout(location = 0) in vec4 aVertexCoor_Distance;'#10+
   '  uniform mat4 uMVP;'#10+
   '  out vec4 VertexColor;'#10+
   '  out float Distance;'#10+
   'void main()'#10+
   '{'#10+
   '  gl_Position = uMVP*vec4(aVertexCoor_Distance.xy, 0.0, 1.0);'#10+
   '  Distance = aVertexCoor_Distance.z;'#10+
   '}';

  FRAGMENT_SHADER =
   '#version 330 core'#10+
   '  out vec4 FragColor;'#10+
   '  uniform vec4 uBeamColor;'#10+
   '  uniform vec4 uHaloColor;'#10+
   '  uniform vec4 uTintColor;'#10+
   '  uniform float uThreshold;'#10+
   '  uniform float uHaloAttenuation;'#10+
   '  uniform float uOpacity;'#10+
   '  in float Distance;'#10+
   'void main()'#10+
   '{'#10+
   '  vec4 col;'#10+
   '  if (Distance < uThreshold) {'#10+
   '     col = vec4(uBeamColor.rgb, uBeamColor.a*uOpacity);'#10+
   '  } else {'#10+
   '     col = vec4(uHaloColor.rgb, uHaloColor.a*pow((1-(Distance-uThreshold)), uHaloAttenuation)*uOpacity);'#10+
   '  }'#10+
   '  if (col.a == 0) discard;'#10+
   // Tint
   '  float a = col.a;'#10+
   '  float tintAlphaX2 = uTintColor.a*2;'#10+
   '  if (uTintColor.a > 0.5)'#10+
   '   {'#10+
   '     tintAlphaX2 = tintAlphaX2-1;'#10+
   //'     col = col * (1-tintAlphaX2) + uTintColor * tintAlphaX2;'#10+  // replace mode
   '     col = mix(col, uTintColor, tintAlphaX2);'#10+ // replace mode
   '   }'#10+
   '  else if (uTintColor.a > 0)'#10+
   '     col = col + uTintColor*tintAlphaX2;'#10+     // mix mode
   '  col.a = a;'#10+
   '  FragColor = col;'#10+
   '}';
private
  FLocMVP,
  FLocTintColor,
  FLocOpacity,
  FLocBeamColor,
  FLocHaloColor,
  FLocThreshold,
  FLocHaloAttenuation: GLint;
  FMVP: TOGLCMatrix;
  FBeamColorF, FHaloColorF,
  FTintF: TColorF;
  FThreshold, FHaloAttenuation, FOpacity: single;
protected
  procedure InitShaderCodeAndCallBack; override;
private
  procedure DefineVertexAttribPointer;
  procedure GetUniformLocation;
  procedure SetUniformValuesAndTexture;
public
  procedure Prepare(const aBeamColor, aHaloColor: TBGRAPixel;
                    const aThreshold, aHaloAttenuation, aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);

end;

{ TOGLCElectricalBeam }

TOGLCElectricalBeam = class(TSimpleSurfaceWithEffect)
private
  FBeamWidth, FHaloWidth, FThreshold: single;
  FPtTarget: TPointF;
  FRefreshTime,
  FRecomputePathTimeAccu: single;
  FAperture: single;
  FPath: TOGLCPath;
  FVertices: TArrayOfxydistance;
  FIndices: TArrayOfVertexIndex;
  FTargetInitialized: boolean;
  procedure SetBeamAperture(AValue: single);
  procedure SetHaloWidth(AValue: single);
  procedure RecomputePath;
  procedure DoComputeBeam;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
   procedure Update(const aElapsedTime: single); override;
//   procedure Draw(const aLayerPercentOpacity: single); override;
   procedure DoDraw; override;
public
  // the beam color
  BeamColor: TBGRAParam;
  // the color of the halo around the beam.
  HaloColor: TBGRAParam;
  // factor attenuation for the halo (default: 2)
  HaloAttenuation: single;

  Constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;

  procedure debugVertices;

  // Sets the beam target point expressed in scene coordinates.
  // The origin of the beam is the surface coordinates.
  procedure SetTargetPoint(aPtTarget: TPointF);

  procedure ComputeBeam;
  property BeamWidth: single read FBeamWidth write FBeamWidth;
  // Aperture is the max value in degree for the angle between two nodes, range is 0 to 45.
  property Aperture: single read FAperture write SetBeamAperture;

  // The width of the halo around the beam. The halo is applyed on each side the beam.
  property HaloWidth: single read FHaloWidth write SetHaloWidth;

  // For each slice of time (in seconds) the beam is recomputed.
  // This period is computed with this formula: RefreshTime+random*RefreshTime*0.5
  // If you don't want to re-compute periodically the beam, set it to negative value (default).
  // default value is -1.
  property RefreshTime: single read FRefreshTime write FRefreshTime;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCElectricalBeamRenderer }

procedure TOGLCElectricalBeamRenderer.InitShaderCodeAndCallBack;
begin
  FShaderName := 'TOGLCElectricalBeamRenderer';
  FVertexShaderCode := PChar(VERTEX_SHADER);
  FFragmentShaderCode := PChar(FRAGMENT_SHADER);
  FDefineVertexAttribPointer := @DefineVertexAttribPointer;
  FGetUniformLocation := @GetUniformLocation;
  FSetUniformValuesAndTexture := @SetUniformValuesAndTexture;
end;

procedure TOGLCElectricalBeamRenderer.DefineVertexAttribPointer;
begin
 glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Txydistance), NIL); // x y distance
 glEnableVertexAttribArray(0);
end;

procedure TOGLCElectricalBeamRenderer.GetUniformLocation;
begin
  with FShader do begin
    FLocMVP := GetUniform('uMVP');
    FLocTintColor := GetUniform('uTintColor');
    FLocOpacity := GetUniform('uOpacity');
    FLocBeamColor := GetUniform('uBeamColor');
    FLocHaloColor := GetUniform('uHaloColor');
    FLocThreshold := GetUniform('uThreshold');
    FLocHaloAttenuation := GetUniform('uHaloAttenuation');
  end;
end;

procedure TOGLCElectricalBeamRenderer.SetUniformValuesAndTexture;
begin
  glUniformMatrix4fv(FLocMVP, 1, GL_FALSE, @FMVP.Matrix[0,0]);
  glUniform4fv(FLocTintColor, 1, @FTintF);
  glUniform1f(FLocOpacity, FOpacity);
  glUniform4f(FLocBeamColor, FBeamColorF.r, FBeamColorF.g, FBeamColorF.b, FBeamColorF.a);
  glUniform4f(FLocHaloColor, FHaloColorF.r, FHaloColorF.g, FHaloColorF.b, FHaloColorF.a);
  glUniform1f(FLocThreshold, FThreshold);
  glUniform1f(FLocHaloAttenuation, FHaloAttenuation);
  FParentScene.TexMan.UnbindTexture;
end;

procedure TOGLCElectricalBeamRenderer.Prepare(const aBeamColor, aHaloColor: TBGRAPixel;
  const aThreshold, aHaloAttenuation, aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var forceFlush: boolean;
  cfB, cfH: TColorF;
begin
  cfB.InitFromBGRA(aBeamColor);
  cfH.InitFromBGRA(aHaloColor);
  forceFlush := not FMVP.EqualTo(FParentScene.MVPMatrix) or
                not FTintF.EqualTo(aComputedTint) or
                (FOpacity <> aOpacity) or
                not FBeamColorF.EqualTo(cfB) or
                not FHaloColorF.EqualTo(cfH) or
                (FThreshold <> aThreshold) or
                (FHaloAttenuation <> aHaloAttenuation);
  Batch_CheckIfNeedFlush(Self, ptTriangleStrip, NIL, 0, aBlendMode, forceFlush);
  FMVP.CopyFrom(FParentScene.MVPMatrix);
  FTintF.CopyFrom(aComputedTint);
  FOpacity := aOpacity;
  FBeamColorF.CopyFrom(cfB);
  FHaloColorF.CopyFrom(cfH);
  FThreshold := aThreshold;
  FHaloAttenuation := aHaloAttenuation;
end;


{ TOGLCElectricalBeam }

procedure TOGLCElectricalBeam.SetHaloWidth(AValue: single);
begin
  FHaloWidth := Max(0, AValue);
end;

procedure TOGLCElectricalBeam.SetBeamAperture(AValue: single);
begin
  FAperture := EnsureRange(AValue, 40, 80);
end;

procedure TOGLCElectricalBeam.RecomputePath;
var ptOrigin, ptTarget: TPointF;
   i, segmentCount: integer;
   segmentLength: single;
   polar, polar1: TPolarCoor;
   pt: TPointF;
begin
  ptOrigin := PointF(0,0);
  ptTarget := SceneToSurface(FPtTarget);
  polar := CartesianToPolar(ptOrigin, ptTarget);
  if polar.Distance < 2 then begin
    FPath := NIL;
    exit;
  end;

  if polar.Distance > 100 then segmentLength := 20
  else if polar.Distance > 40 then segmentLength := 10
  else if polar.Distance > 10 then segmentLength := 5
  else segmentLength := polar.Distance;
  segmentCount := Max(1, Round(polar.Distance/segmentLength));

  // construct the path
  SetLength(FPath, segmentCount+1);
  FPath[0] := ptOrigin;
  FPath[High(FPath)] := ptTarget;
  if segmentCount = 1 then exit;

  polar.Distance := 0;
pt := ptOrigin; //
  for i:=1 to High(FPath)-1 do begin
    polar.Distance := i * segmentLength;
//    pt := PolarToCartesian(ptOrigin, polar);    //
    polar1.Angle := polar.Angle + random(Round(FAperture)*10)*0.1 - FAperture*0.5;
    polar1.Distance := segmentLength;
    FPath[i] := PolarToCartesian(pt, polar1);
pt := FPath[i];  //
  end;


{  ang := polar.Angle;
  pt := ptOrigin;
  for i:=1 to High(FPath)-1 do begin
    polar.Distance := segmentLength;
    polar.Angle := ang + random(Round(FAperture)*10)*0.1 - FAperture*0.5;
    FPath[i] := PolarToCartesian(pt, polar);
    pt := FPath[i];
  end;    }

end;

procedure TOGLCElectricalBeam.DoComputeBeam;
const VERTEX_COUNT_FOR_FIRST_QUAD = 6;
      VERTEX_COUNT_FOR_OTHER_QUAD = 3;
      INDICE_PER_QUAD = 6;
var p: Pxydistance;
  pIndice: PVertexIndex;
  currentIndice: TVertexIndex;
  i, quadCount, primitiveRestartCount: Integer;
  quad1, quad2: TQuadF;
  width1: single;
  iterationCount: integer;
   { TODO : essayer avec ptTriangles }
   procedure InitVerticesAndIndicesForQuad1;
   begin
     if iterationCount = 0 then begin
       // path point pt1 and pt2
       p^.x := FPath[i].x; p^.y := FPath[i].y; p^.distance := 0;  inc(p);
       p^.x := FPath[i+1].x; p^.y := FPath[i+1].y; p^.distance := 0;  inc(p);
       // quad1 topleft, topright, bottomright, bottomleft
       p^.x := quad1[cTL].x; p^.y := quad1[cTL].y; p^.distance := 1;  inc(p);
       p^.x := quad1[cTR].x; p^.y := quad1[cTR].y; p^.distance := 1;  inc(p);
       p^.x := quad1[cBR].x; p^.y := quad1[cBR].y; p^.distance := 1;  inc(p);
       p^.x := quad1[cBL].x; p^.y := quad1[cBL].y; p^.distance := 1;  inc(p);

       // indices for triangle strip
       pIndice[0] := currentIndice+2;
       pIndice[1] := currentIndice+3;
       pIndice[2] := currentIndice+0;
       pIndice[3] := currentIndice+1;
       pIndice[4] := currentIndice+5;
       pIndice[5] := currentIndice+4;
       inc(currentIndice, VERTEX_COUNT_FOR_FIRST_QUAD);
     end else begin
       // OPTIMIZATION: here we don't repeat the 3 left vertices that are already defined in the previous quad (the 3 at right)
       // path point pt2
       p^.x := FPath[i+1].x; p^.y := FPath[i+1].y; p^.distance := 0;  inc(p);
       // quad1 topright, bottomright
       p^.x := quad1[cTR].x; p^.y := quad1[cTR].y; p^.distance := 1;  inc(p);
       p^.x := quad1[cBR].x; p^.y := quad1[cBR].y; p^.distance := 1;  inc(p);

       if iterationCount = 1 then begin
         // indices for triangle strip
         pIndice[0] := currentIndice-5;
         pIndice[1] := currentIndice;
         pIndice[2] := currentIndice-3;
         pIndice[3] := currentIndice+1;
         pIndice[4] := currentIndice+2;
         pIndice[5] := currentIndice-2;
       end else begin
         pIndice[0] := currentIndice-3;
         pIndice[1] := currentIndice;
         pIndice[2] := currentIndice-2;
         pIndice[3] := currentIndice+1;
         pIndice[4] := currentIndice+2;
         pIndice[5] := currentIndice-1;
       end;
       inc(currentIndice, VERTEX_COUNT_FOR_OTHER_QUAD);
     end;

     inc(pIndice, INDICE_PER_QUAD);
//     inc(iterationCount);
   end;
   procedure PushPrimitiveRestart;
   begin
     pIndice^ := PRIMITIVE_INDEX_RESTART_VALUE;
     inc(pIndice);
   end;

begin
  FVertices := NIL;
  FIndices := NIL;
  if (FBeamWidth <= 0) or (Length(FPath) < 2) then exit;

  width1 := FBeamWidth*0.5+FHaloWidth;
  FThreshold :=  (FBeamWidth*0.5)/width1;

  currentIndice := 0;
  iterationCount := 0;
  if Length(FPath) = 2 then begin
    // there are only 2 points
    SetLength(FVertices, VERTEX_COUNT_FOR_FIRST_QUAD);
    p := @FVertices[0];
    SetLength(FIndices, INDICE_PER_QUAD);
    pIndice := @FIndices[0];
    quad1 := LineSegmentToQuadF_PosMiddle(FPath[0], FPath[1], width1, width1);
    i := 0;
    InitVerticesAndIndicesForQuad1;
    exit;
  end;

  quadCount := Length(FPath)-1;
  primitiveRestartCount := quadCount - 1;
  SetLength(FVertices, VERTEX_COUNT_FOR_FIRST_QUAD * quadcount);
//  SetLength(FVertices, VERTEX_COUNT_FOR_FIRST_QUAD + (quadCount-1)*VERTEX_COUNT_FOR_OTHER_QUAD);
  SetLength(FIndices, quadCount*INDICE_PER_QUAD + primitiveRestartCount);

  // compute the previousTopLeft and previousBottomLeft
  quad1 := LineSegmentToQuadF_PosMiddle(FPath[0], FPath[1], width1, width1);

  p := @FVertices[0];
  pIndice := @FIndices[0];

  for i:=0 to High(FPath)-2 do begin
    quad2 := LineSegmentToQuadF_PosMiddle(FPath[i+1], FPath[i+2], width1, width1);
    AdjustCornerOnConsecutiveQuadF(quad1, quad2);
    InitVerticesAndIndicesForQuad1;
    PushPrimitiveRestart;
    Move(quad2, quad1, SizeOf(TQuadF));
  end;
  inc(i);
  InitVerticesAndIndicesForQuad1; // quad1 is equal to quad2

  dec(p);
  if p <> @FVertices[High(FVertices)] then
    raise exception.create('vertex buffer overload or underload');

  dec(pIndice);
  if pIndice <>@FIndices[High(FIndices)] then
    raise exception.create('index buffer overload or underload');
end;

function TOGLCElectricalBeam.GetWidth: integer;
begin
  Result := 1;
end;

function TOGLCElectricalBeam.GetHeight: integer;
begin
  Result := 1;
end;

procedure TOGLCElectricalBeam.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if Freeze then exit;

  BeamColor.OnElapse(aElapsedTime);

  if (FRefreshTime > 0) and FTargetInitialized then begin
    FRecomputePathTimeAccu := FRecomputePathTimeAccu - aElapsedTime;
    if FRecomputePathTimeAccu <= 0 then begin
      RecomputePath;
      DoComputeBeam;
      FRecomputePathTimeAccu := FRefreshTime + random*FRefreshTime*0.5;
    end;
  end;
end;

procedure TOGLCElectricalBeam.DoDraw;
var p: Pxydistance;
begin
  if (Length(FVertices) = 0) or (Length(FIndices) = 0) then exit;

  FParentScene.ElectricalBeamRenderer.Prepare(BeamColor.Value, HaloColor.Value, FThreshold,
                  HaloAttenuation, FComputedOpacity, FComputedTint, FBlendmode);
  FParentScene.ElectricalBeamRenderer.PushIndices(FIndices);
  p := FParentScene.ElectricalBeamRenderer.QueryVertex(Length(FVertices));
  Move(FVertices[0], p^, SizeOf(Txydistance)*Length(FVertices));
end;


constructor TOGLCElectricalBeam.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
  BeamColor := TBGRAParam.Create;
  BeamColor.Value := BGRA(100,0,255);
  FBeamWidth := 2.0;
  HaloColor := TBGRAParam.Create;
  HaloColor.Value := BGRA(255,0,255);
  HaloAttenuation := 2.0;
  HaloWidth := 3.0;
  FRefreshTime := -1;
  FAperture := 35;
end;

destructor TOGLCElectricalBeam.Destroy;
begin
  FreeAndNil(BeamColor);
  FreeAndNil(HaloColor);
  inherited Destroy;
end;

procedure TOGLCElectricalBeam.debugVertices;
var i: integer;
  s: string;
begin
  with FParentScene do begin
    LogMess('Vertices count: '+IntToStr(Length(FVertices)));
    for i:=0 to High(FVertices) do
     LogMess('('+FormatFloat('0.000',FVertices[i].x)+'  '+FormatFloat('0.000',FVertices[i].y)+')'+
             '  distance '+FormatFloat('0.000',FVertices[i].distance), 2);
    LogMess('Indices count: '+IntToStr(Length(FIndices)));
    s := '(';
    for i:=0 to High(FIndices) do s := s + ' '+FIndices[i].ToString;
    LogMess(s+' )', 2);
  end;
end;

procedure TOGLCElectricalBeam.SetTargetPoint(aPtTarget: TPointF);
begin
  FPtTarget := aPtTarget;
  FTargetInitialized := True;
end;

procedure TOGLCElectricalBeam.ComputeBeam;
begin
  if not FTargetInitialized then exit;
  RecomputePath;
  DoComputeBeam;
end;


{$endif oglcIMPLEMENTATION}




