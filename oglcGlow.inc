{$ifdef oglcINTERFACE}
type
  { TOGLCGlow }

  TOGLCGlow = class( TSimpleSurfaceWithEffect )
   private
    class var FShaderGlowRefCount: cardinal;
    class var FShaderGlow: TOGLCShader;
    class var FLocGlowCenter: GLint;
    class var FLocGlowColor: GLint;
    class var FLocGlowRadius: GLint;
    class var FLocGlowAttenuation: GLint;
  protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
  public
   procedure Update ( const aElapsedTime : single ) ; override;
   procedure Draw( const aLayerPercentOpacity : single ) ; override;
  public
   Radius     : TFParam;
   Attenuation: TFParam;
   Constructor Create( aX, aY, aRadius: single; aColor: TBGRAPixel; aBlendMode: byte=FX_BLEND_ADD ) ;
   Destructor Destroy; override;
   // use Tint property to set the color
  end;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
const

GLOW_FRAGMENT_SHADER_PRG = '#version 120'#10+
                          'uniform vec2 lightCenter;'#10+
                          'uniform vec4 lightColor;'#10+
                          'uniform float lightRadius;'#10+
                          'uniform float powerAttenuation;'#10+
                          'void main() {'#10+
                          'float d = distance( lightCenter, gl_FragCoord.xy ) / lightRadius;'#10+
                          'float attenuation = 1 - d;'#10+
                          'vec4 color = vec4(attenuation, attenuation, attenuation, pow(attenuation, powerAttenuation)) * lightColor;'#10+
                          'gl_FragColor = color;'#10+
                          '}' ;

{ TOGLCGlow }

function TOGLCGlow.GetWidth: integer;
begin
 Result := round( Radius.Value * 2 );
end;

function TOGLCGlow.GetHeight: integer;
begin
 Result := round( Radius.Value * 2 );
end;

procedure TOGLCGlow.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 if Freeze then exit;
 Radius.OnElapse( aElapsedTime );
 Attenuation.OnElapse( aElapsedTime );
end;

procedure TOGLCGlow.Draw(const aLayerPercentOpacity: single);
var r: single;
    p: TPointF;
begin
 if not FVisible then exit;
// if not OpenGL_Version_3_0_Loaded then exit; // no shader available

 SetBlendMode( FBlendMode );

 r := Radius.Value * Scale.x.Value;
 p:= SurfaceToScene(PointF(0,0));

 with FShaderGlow do begin
  Use;
  SetUniform2f( FLocGlowCenter, p.x, FParentScene.Height - p.y);
  SetUniform4f( FLocGlowColor, Tint.Red.Value/255, Tint.Green.Value/255, Tint.Blue.Value/255, Tint.Alpha.Value/255*aLayerPercentOpacity );
  SetUniform1f( FLocGlowRadius, r );
  SetUniform1f( FLocGlowAttenuation, Attenuation.Value );
 end;

 glBegin( GL_QUADS ) ;
  glVertex2f( x.Value-r, y.Value-r );
  glVertex2f( x.Value+r, y.Value-r );
  glVertex2f( x.Value+r, y.Value+r );
  glVertex2f( x.Value-r, y.Value+r );
 glEnd ;

 FShaderGlow.Release;


 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value-Radius, Y.Value-Radius, Radius*2, Radius*2, BGRA(255,0,0));
 {$ENDIF}
// glPopMatrix;

end;

constructor TOGLCGlow.Create(aX, aY, aRadius: single; aColor: TBGRAPixel;
  aBlendMode: byte);
begin
 inherited Create;
 x.Value := aX;
 y.Value := aY;
 Radius := TFParam.Create;
 Radius.Value := aRadius;
 Attenuation := TFParam.Create;
 Attenuation.Value:= 2.0;
 Tint.Value:=aColor;
 FBlendMode := aBlendMode;

 if FShaderGlowRefCount=0 then begin
    FShaderGlow := TOGLCShader.Create( '', GLOW_FRAGMENT_SHADER_PRG );
    with FShaderGlow do begin
     Use;
     FLocGlowCenter := GetUniform( 'lightCenter' );
     FLocGlowColor := GetUniform( 'lightColor' );
     FLocGlowRadius := GetUniform( 'lightRadius' );
     FLocGlowAttenuation := GetUniform( 'powerAttenuation' );
     Release;
    end;
 end;
 inc(FShaderGlowRefCount);
end;

destructor TOGLCGlow.Destroy;
begin
 Radius.Free;
 Attenuation.Free;
 dec(FShaderGlowRefCount);
 if FShaderGlowRefCount = 0 then
   FShaderGlow.Free;
 inherited Destroy;
end;

{$endif oglcIMPLEMENTATION}

