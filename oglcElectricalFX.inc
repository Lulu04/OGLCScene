{$ifdef oglcINTERFACE}
type

{ TOGLCElectricalBeam }

TOGLCElectricalBeam=class(TOGLCPathDrawing)
private
  class var FShader: TOGLCShader;
  class var FLocBeamWidth: GLint;
  class var FLocBeamColor: GLint;
  class var FLocHaloColor: GLint;
  class var FLocHaloRadius: GLint;
  class var FLocHaloAttenuation: GLint;
  class var FInstanceCount: integer;
private
  FSavedp1, FSavedp2: TPointF;
private
  FPtOrigin,
  FPtTarget: TPointF;
  FRecomputePathTime,
  FRecomputePathTimeAccu: single;
  FShakerOffset: arrayOfTPointF;
  FShakerAmplitude: integer;
  FShakerTime,
  FShakerTimeAccu: single;
  FSegmentLength,
  FAperture: single;
  procedure SetShakerAmplitude(AValue: integer);
  procedure ComputeShakerOffsets;
protected
  procedure DoDraw( const pt1, pt2: TPointF; aLayerPercentOpacity: single); override;
  function GetPoint( aIndex: integer ): TPointF; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   // the beam color
   // use Tint property to set the color of the halo around the beam
  BeamColor: TBGRAParam;
  // the radius of the halo around the beam
  Radius     : single;
  // factor attenuation for the halo (default: 2)
  Attenuation: single;
  // uses Tint property to set the beam color
  Constructor Create;
  destructor Destroy; override;

  // constructs randomly the beam between the two points
  // aSegmentLength is the distance between two nodes in pixels
  // aAperture is the max value for the angle between two nodes
  procedure ComputeBeam( aPtOrigin, aPtTarget: TPointF; aSegmentLength:single=40; aAperture: single=35 );

  property LineWidth: single read FLineWidth write FLineWidth;
  property Path: TOGLCPath read FPath;
  // amplitude max for the shaker effect in pixel
  // if >0 each node of the beam moves randomly by max this value or less
  // 0= no shaker effect
  property ShakerAmplitude: integer read FShakerAmplitude write SetShakerAmplitude;
  property ShakerTime: single read FShakerTime write FShakerTime;

  // For each slice of time (in seconds) the beam will be recomputed
  // this period is computed as with this formula: PathRecomputeTime+random*PathRecomputeTime*0.5
  // If you don't want to re-compute periodically the beam, set it to negative value (default).
  // default value is -1
  property PathRecomputeTime: single read FRecomputePathTime write FRecomputePathTime;
end;

{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}
const

ELECTRICAL_BEAM_VERTEX_SHADER_PRG =
'varying float vRadiusInterpolation;'
+#10'void main() {'
+#10'    gl_Position = gl_Vertex;' //  mvp * gl_Vertex;'    // gl_ModelViewProjectionMatrix * gl_Vertex;'
+#10'    vRadiusInterpolation = gl_Color.rg;'  // distance between vertex and quad center
+#10'}';

ELECTRICAL_BEAM_FRAGMENT_SHADER_PRG = '#version 120'#10+
                          'varying float vRadiusInterpolation;'#10+
                          'uniform float beamWidth;'#10+
                          'uniform vec4 beamColor;'#10+
                          'uniform vec4 haloColor;'#10+
                          'uniform float haloRadius;'#10+
                          'uniform float haloAttenuation;'#10+
                          'vec4 color;'#10+
                          'void main() {'#10+

                          ' if (vRadiusInterpolation ==-9999) {'#10+
                          '   color = vec4(0,0,0,1);'#10+
                          ' } else {'#10+

                          ' float d = abs(vRadiusInterpolation);'#10+
                          ' if (d < beamWidth) {'#10+
                          '    color = beamColor;'#10+
                          ' } else {'#10+
                          '    float attenuation = 1 - (d / haloRadius);'#10+
                          '    color = vec4(haloColor.rgb, pow(attenuation, haloAttenuation));'#10+
                          ' }'#10+
                          ' }'#10+
                          ' gl_FragColor = color;'#10+
                          '}' ;


{ TOGLCElectricalBeam }

procedure TOGLCElectricalBeam.SetShakerAmplitude(AValue: integer);
begin
  if FShakerAmplitude=AValue then Exit;
  FShakerAmplitude:=AValue;
end;

procedure TOGLCElectricalBeam.ComputeShakerOffsets;
var
  i, v: Integer;
begin
 v:=ShakerAmplitude*100;
 for i:=1 to High(FShakerOffset)-1 do begin
   if ShakerAmplitude=0
   then FShakerOffset[i]:=PointF(0,0)
   else FShakerOffset[i]:=PointF(random(v)*0.01, random(v)*0.01);
 end;
end;

procedure TOGLCElectricalBeam.DoDraw(const pt1, pt2: TPointF; aLayerPercentOpacity: single);
var p2, p: TPointF;
  pol:TPolarCoor;
  A: single;
  mvp, oldmv: TnMat44;
begin
 pol:=CartesianToPolar(pt1, pt2);
 A:=pol.Angle;

 with FShader do begin
  Use;
  SetUniform1f( FLocBeamWidth, FLineWidth);
  SetUniform4f( FLocHaloColor, Tint.Red.Value/255, Tint.Green.Value/255, Tint.Blue.Value/255, Tint.Alpha.Value/255*aLayerPercentOpacity );
  SetUniform4f( FLocBeamColor, BeamColor.Red.Value/255, BeamColor.Green.Value/255, BeamColor.Blue.Value/255, BeamColor.Alpha.Value/255*aLayerPercentOpacity );
  SetUniform1f( FLocHaloRadius, Radius );
  SetUniform1f( FLocHaloAttenuation, Attenuation );
 end;

 oldmv := ngl_ModelViewMatrix;
 nglTranslatef( pt1.x+FSurfaceOrigin.x, pt1.y+FSurfaceOrigin.y, 0 );
// nglTranslatef( pt1.x, pt1.y, 0 );
 nglRotatef( A-90, 0, 0, 1 );               // opengl is counter clockwise
 mvp := ngl_ModelViewProjectionMatrix;
// FShader.SetUniformMatrix4( FLocMVP, @mvp[0,0]);

 p2 := PointF(0,pol.Distance);
 glBegin( GL_QUADS );
  if FSavedp1.x=-100000
   then p:=ApplyMatrix( @mvp, PointF(-Radius,0))
   else p:=FSavedp1;
  glColor4f(-Radius,0,0,0);
  glVertex2f( p.x, p.y );    //p1

  if FSavedp1.x=-100000
     then p:=ApplyMatrix( @mvp, PointF(Radius,0))
     else p:=FSavedp2;
  glColor4f(Radius,0,0,0);
  glVertex2f( p.x, p.y );  //p1

  p := ApplyMatrix( @mvp, p2+PointF(Radius,0));
  FSavedp2:=p;
  glColor4f(Radius,0,0,0);
  glVertex2f( p.x, p.y );  //p2

  p := ApplyMatrix( @mvp, p2-PointF(Radius,0));
  FSavedp1:=p;
  glColor4f(-Radius,0,0,0);
  glVertex2f( p.x, p.y );  //p2
 glEnd;
 FShader.Release;
{ DrawLine(p2.x-Radius, p2.y, p2.x+Radius, p2.y, BGRA(255,255,255,100),1);
 DrawLine(p2.x+Radius, p2.y, p1.x+Radius, p1.y, BGRA(255,255,255,100),1);
 DrawLine(p1.x+Radius, p1.y, p1.x-Radius, p1.y, BGRA(255,255,255,100),1);
 DrawLine(p1.x-Radius, p1.y, p2.x-Radius, p2.y, BGRA(255,255,255,100),1);  }

 // glPopMatrix;
// nglLoadIdentity;
 gnGL.mat[nGL_MODELVIEW]:=oldmv;

end;

function TOGLCElectricalBeam.GetPoint(aIndex: integer): TPointF;
begin
 Result := FPath.Pts[aIndex]+FShakerOffset[aIndex];
end;

procedure TOGLCElectricalBeam.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 if Freeze then exit;

 if FRecomputePathTime>=0 then begin
   FRecomputePathTimeAccu-=aElapsedTime;
   if FRecomputePathTimeAccu<=FRecomputePathTime then begin
     ComputeBeam( FPtOrigin, FPtTarget, FSegmentLength, FAperture);
     FRecomputePathTimeAccu:=FRecomputePathTime+random*FRecomputePathTime*0.5;//0;
   end;
 end;

 FShakerTimeAccu+=aElapsedTime;
 if FShakerTimeAccu>=FShakerTime then begin
   ComputeShakerOffsets;
   FShakerTimeAccu:=0;
 end;

 BeamColor.OnElapse( aElapsedTime );
end;

procedure TOGLCElectricalBeam.Draw(const aLayerPercentOpacity: single);
begin
 FSavedp1.x:=-100000;
 inherited Draw(aLayerPercentOpacity);
end;


constructor TOGLCElectricalBeam.Create;
begin
 inherited Create;
 Tint.Value:=BGRA(255,255,255);
 BeamColor:= TBGRAParam.Create;
 BeamColor.Value:=BGRAWhite;
 Attenuation:= 2.0;
 Radius:=20;
 FRecomputePathTime:=-1;

 if FInstanceCount=0 then begin
    FShader := TOGLCShader.Create( ELECTRICAL_BEAM_VERTEX_SHADER_PRG, ELECTRICAL_BEAM_FRAGMENT_SHADER_PRG );
    with FShader do begin
//     FLocMVP := GetUniform( 'mvp' );
     FLocBeamWidth := GetUniform( 'beamWidth' );
     FLocBeamColor := GetUniform( 'beamColor' );
     FLocHaloColor := GetUniform( 'haloColor' );
     FLocHaloRadius := GetUniform( 'haloRadius' );
     FLocHaloAttenuation := GetUniform( 'haloAttenuation' );
    end;
 end;
 inc(FInstanceCount);
end;

destructor TOGLCElectricalBeam.Destroy;
begin
 dec(FInstanceCount);
 if FInstanceCount=0 then begin
   FShader.Free;
   FShader:=NIL;
 end;
 BeamColor.Free;
 inherited Destroy;
end;

procedure TOGLCElectricalBeam.ComputeBeam(aPtOrigin, aPtTarget: TPointF; aSegmentLength: single; aAperture: single);
var nodes: ArrayOfTPointF;
  polar: TPolarCoor;
  p: TPointF;
  A, SegmentLength: single;
  nodeCount, i: Integer;
begin
 ReleaseCurrentPath;
 FPath:=TOGLCPath.Create;
 FPathOwner:=TRUE;

 FPtOrigin:=aPtOrigin;
 FPtTarget:=aPtTarget;
 FRecomputePathTimeAccu:=0;

 if aPtOrigin=aPtTarget then begin
   SetLength( FShakerOffset, 0);
   exit;
 end;

 if aAperture<0
    then aAperture:=0
    else if aAperture>45
           then aAperture:=45;
 FAperture:=aAperture;

 polar:=CartesianToPolar(aPtOrigin, aPtTarget);

 if aSegmentLength<=0 then aSegmentLength:=40;
 nodeCount:= round(polar.Distance/aSegmentLength);//nombre de point de controle
 if nodeCount=0 then begin
   nodeCount:=1;
   aSegmentLength:=polar.Distance/2;
 end;
 FSegmentLength:=aSegmentLength;

 SegmentLength:=polar.Distance/(nodeCount+1);

 SetLength(nodes,2+nodeCount);
 // set extremities
 nodes[0]:=aPtOrigin;
 nodes[nodeCount+1]:=aPtTarget;

 if nodeCount>0 then begin
   A:=polar.Angle;
   p:=aPtOrigin;
   for i:=1 to nodeCount do begin
    polar.Distance:=SegmentLength;//*i;
    polar.Angle:=A+(random(round(aAperture)*10)*0.1-aAperture*0.5);

    nodes[i]:=PolarToCartesian(p, polar);
    p:=nodes[i];
   end;
 end;

 FPath.Pts := nodes;

 // force offset for shaker to 0
 SetLength( FShakerOffset, Length(FPath.Pts));
 for i:=0 to High(FShakerOffset) do
   FShakerOffset[i]:=PointF(0,0);
end;


{$endif oglcIMPLEMENTATION}




