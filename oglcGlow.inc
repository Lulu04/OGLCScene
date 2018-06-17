{$ifdef oglcINTERFACE}
type
  { TGlow }

  TGlow = class( TSimpleSurfaceWithEffect )
   private
    class var FLocGlowCenter: GLint;
    class var FLocGlowColor: GLint;
    class var FLocGlowRadius: GLint;
    class var FLocGlowAttenuation: GLint;
    class var FLocInitDone: boolean;
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
  end;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TGlow }

function TGlow.GetWidth: integer;
begin
 Result := round( Radius.Value * 2 );
end;

function TGlow.GetHeight: integer;
begin
 Result := round( Radius.Value * 2 );
end;

procedure TGlow.Update(const aElapsedTime: single);
begin
 inherited Update(aElapsedTime);
 Radius.OnElapse( aElapsedTime );
 Attenuation.OnElapse( aElapsedTime );
end;

procedure TGlow.Draw(const aLayerPercentOpacity: single);
var OldBlendMode : byte ;
  r: single;
begin
 if not FVisible then exit;
 if not OpenGL_Version_2_0_Loaded then exit; // no shader available

 OldBlendMode := _FglCurrentBlendMode;
 SetBlendMode( FBlendMode );

 r := Radius.Value * Scale.x.Value;

// glPushMatrix;
// glTranslatef( X, Y, 0);

 with ShaderManager do begin
  Use( FParentScene.FShaderGlow );
  SetUniform2f( FLocGlowCenter, x.Value, FParentScene.Height - y.Value);
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

 SetBlendMode( OldBlendMode );

 ShaderManager.UseNone;


 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value-Radius, Y.Value-Radius, Radius*2, Radius*2, BGRA(255,0,0));
 {$ENDIF}
// glPopMatrix;

end;

constructor TGlow.Create(aX, aY, aRadius: single; aColor: TBGRAPixel;
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

 if not FLocInitDone
   then begin
    FLocInitDone := TRUE;
    FLocGlowCenter := ShaderManager.GetUniform( 0, 'lightCenter' ) ;
    FLocGlowColor := ShaderManager.GetUniform( 0, 'lightColor' ) ;
    FLocGlowRadius := ShaderManager.GetUniform( 0, 'lightRadius' ) ;
    FLocGlowAttenuation := ShaderManager.GetUniform( 0, 'powerAttenuation' ) ;
end;
end;

destructor TGlow.Destroy;
begin
 Radius.Free;
 Attenuation.Free;
 inherited Destroy;
end;

{$endif oglcIMPLEMENTATION}

