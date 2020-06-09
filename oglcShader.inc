{$ifdef oglcINTERFACE}
type

{ TOGLCShader }

TOGLCShader = class
 private
  FProgramID,
  FFragmentID,
  FVertexID: GLUInt;
  procedure NotifyFragmentCompilationError( const aF: string );
  procedure NotifyVertexCompilationError( const aF: string );
  procedure NotifyLinkError;
  procedure NotifyGetUniformError( const s: string);
 public
  constructor Create(const AVertexCodeSource, AFragmentCodeSource: string);
  destructor Destroy; override;

  procedure Use; virtual;
  procedure Release;

  function GetUniform( const AName: string ): GLint;
  procedure SetUniform1f( AUniform: GLint; AValue: single );
  procedure SetUniform2f( AUniform: GLint; AValue1, AValue2: single );
  procedure SetUniform3f( AUniform: GLint; AValue1, AValue2, AValue3: single );
  procedure SetUniform4f( AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single );
  procedure SetUniform1i( AUniform: GLint; AValue: GLint );
  procedure SetUniformMatrix4( AUniform: GLint; AValue: PGLfloat);
end;


TOGLCPostFXEnum=( pfColorInversion, pfGrayscale, pfSepia, pfSharpen, pfBlur );
TOGLCPostFX= set of TOGLCPostFXEnum;

{ TOGLCPostProcessingFX }

TOGLCPostProcessingFX=class
private const
  // FX bit mask
  fxbmColorInversion=$01;
  fxbmGrayscale=$02;
  fxbmSepia=$04;
  fxbmSharpen=$08;
  fxbmBlur=$10;
  procedure ComputeFXBitMask;
private
  FFX: TOGLCPostFX;
  FFXBitMask: GLuint;
  FTint: TBGRAPixel;
  FTexture: PTexture;
  FColorAttachmentIndex: GLint;
  class var FShader: TOGLCShader;
  class var FLocFXFlag: GLint;
  class var FLocTint: GLint;
  class var FLocTexSlot: GLint;
  class var FLocTexWidth: GLint;
  class var FLocTexHeight: GLint;
  class var FCount: integer;
public
  constructor Create;
  destructor Destroy; override;

  procedure Use;
  procedure Release;

  procedure EnableFX( aFXs: TOGLCPostFX );
  procedure DisableFX( aFXs: TOGLCPostFX );
  procedure DisableAllFX;

  procedure SetParam( aTexture: PTexture; aColorAttachment: integer);
  property Tint: TBGRAPixel read FTint write FTint;
end;



{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
const
kVert ='varying vec4 vClr;'
+#10'void main() {'
+#10'    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;'
+#10'    vClr = gl_MultiTexCoord0;' //gl_Color;'
+#10'}';

kFrag = 'varying vec4 vClr;'
+#10'uniform sampler2D TexSlot;'
+#10'uniform int FXFlag;'
+#10'uniform vec4 TintColor;'
+#10'uniform int TexWidth, TexHeight;'
//+#10'uniform int TexHeight;'
+#10'void main() {'
+#10'  vec4 Col = texture(TexSlot, vClr.xy);'
// blur
+#10' if (FXFlag >= 16){'
+#10'   FXFlag = FXFlag-16;'
+#10'	float dx = 1.0f/TexWidth;//step on x'
+#10'	float dy = 1.0f/TexHeight;//step on y'
+#10'	vec3 sum = vec3(0,0,0);'
+#10'	for(int i = -3; i< 3; i++)'
+#10'	   for(int j = -3;j < 3; j++)'   //9
+#10'		sum += texture2D(TexSlot, vClr.xy + vec2(i * dx, j * dy)).rgb;'
+#10'	Col = vec4(vec3(sum/30), Col.a);'  //360
+#10' }'
// sharpen
+#10' if (FXFlag >= 8){'
+#10'   FXFlag = FXFlag-8;'
+#10'   float kernel[9] = float[]('
+#10'   -1, -1, -1,'
+#10'    -1,  9, -1,'
+#10'   -1, -1, -1'
+#10'   );'
+#10'	float dx = 1.0f/TexWidth;//step on x'
+#10'	float dy = 1.0f/TexHeight;//step on y'
+#10'   vec2 offsets[9] = vec2[]('
+#10'   vec2(-dx,  dy),' // top-left
+#10'   vec2( 0.0f,    dy),' // top-center
+#10'   vec2( dx,  dy),' // top-right
+#10'   vec2(-dx,  0.0f),'   // center-left
+#10'   vec2( 0.0f,    0.0f),'   // center-center
+#10'   vec2( dx,  0.0f),'   // center-right
+#10'   vec2(-dx, -dy),' // bottom-left
+#10'   vec2( 0.0f,   -dy),' // bottom-center
+#10'   vec2( dx, -dy)'  // bottom-right
+#10'   );'
+#10'   vec3 sampleTex[9];'
+#10'   for(int i = 0; i < 9; i++)'
+#10'      sampleTex[i] = vec3(texture2D(TexSlot, vClr.xy + offsets[i]));'
+#10'   sampleTex[4] = Col.rgb;'
+#10'   vec3 c = vec3(0.0);'
+#10'   for(int i = 0; i < 9; i++)'
+#10'      c += sampleTex[i] * kernel[i];'
+#10'   Col = vec4(c, Col.a);'
+#10' }'
// sepia
+#10' if (FXFlag >= 4){'
+#10'   FXFlag = FXFlag-4;'
+#10'   Col = vec4(0.393*Col.r + 0.769*Col.g + 0.189*Col.b,'
+#10'              0.349*Col.r + 0.686*Col.g + 0.168*Col.b,'
+#10'              0.272*Col.r + 0.534*Col.g + 0.131*Col.b, Col.a);'
+#10' }'
// grayscale
+#10' if (FXFlag >= 2){'
+#10'   FXFlag = FXFlag-2;'
+#10'   float average = 0.2126 * Col.r + 0.7152 * Col.g + 0.0722 * Col.b;'
+#10'   Col = vec4(average, average, average, Col.a);'
+#10' }'
// color inversion
+#10' if (FXFlag == 1){'
+#10'   Col = vec4(vec3(1.0 - Col), Col.a);'
+#10' }'
// Tint
+#10' Col = Col * (1-TintColor.a) + vec4( vec3(TintColor*TintColor.a), Col.a);'

+#10'  gl_FragColor = Col;'
+#10'}';

SCENE_POST_PROCESSING_VER_PRG =
                         '#version 330 core'#10+
                         'layout (location = 0) in vec2 aPos;'#10+
                         'layout (location = 1) in vec2 aTexCoords;'#10+
                         'out vec2 TexCoords;'#10+

                         'void main()'#10+
                         '{'#10+
                         '  gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);'#10+
                         '  TexCoords = aTexCoords;'#10+
                         '}';

// bit 0 = color inversion
// bit 1 = Grayscale
// bit 2 = Sharpen
// bit 3 = Blur effect
// always Apply Tint if Tint.alpha>0
SCENE_POST_PROCESSING_FRAG_PRG =
                          '#version 330 core'#10+
                          'out vec4 FragColor;'#10+
                          'in vec2 TexCoords;'#10+
                          'uniform sampler2D TexSlot;'#10+
                          'uniform vec4 TintColor;'#10+
                          'uniform int FXFlag;'#10+

                          'void main() {'#10+
                          'vec4 WorkC = texture(TexSlot, TexCoords);'#10+

                         // 'if ( FXFlag & 4 = 4 )'#10+
                         // ' {'#10+
                          // color inversion
                     //     '   WorkC = vec4(vec3(1.0 - WorkC), 1.0);'#10+
                         // ' }'#10+

                          'FragColor = WorkC;'#10+
                          '}' ;

{ TOGLCPostProcessingFX }

constructor TOGLCPostProcessingFX.Create;
begin
 if FCount=0 then begin
//   FShader := TOGLCShader.Create(SCENE_POST_PROCESSING_VER_PRG, SCENE_POST_PROCESSING_FRAG_PRG);
   FShader := TOGLCShader.Create(kVert, kFrag);
   with FShader do begin
     FLocTexSlot := GetUniform('TexSlot');
     FLocTint := GetUniform('TintColor');
     FLocFXFlag := GetUniform('FXFlag');
     FLocTexWidth := GetUniform('TexWidth');
     FLocTexHeight := GetUniform('TexHeight');
   end;
 end;
 inc(FCount);
 FFX:=[];
 ComputeFXBitMask;
 FTint:=BGRA(0,0,0,0);
end;

destructor TOGLCPostProcessingFX.Destroy;
begin
 dec(FCount);
 if FCount=0 then begin
   FShader.Free;
   FShader:=NIL;
 end;
 inherited Destroy;
end;

procedure TOGLCPostProcessingFX.Use;
begin
 glActiveTexture( GL_TEXTURE0+FColorAttachmentIndex );
 FShader.Use;

 FShader.SetUniform1i( FLocTexSlot, FColorAttachmentIndex);
 FShader.SetUniform1i( FLocFXFlag, FFXBitMask);
 FShader.SetUniform4f( FLocTint, FTint.red/255, FTint.green/255, FTint.blue/255, FTint.alpha/255);
 FShader.SetUniform1i( FLocTexWidth, FTexture^.TextureWidth);
 FShader.SetUniform1i( FLocTexHeight, FTexture^.TextureHeight);
end;

procedure TOGLCPostProcessingFX.Release;
begin
 FShader.Release;
end;

procedure TOGLCPostProcessingFX.ComputeFXBitMask;
begin
 FFXBitMask:=0;
 if pfColorInversion in FFX then FFXBitMask := FFXBitMask or fxbmColorInversion;
 if pfGrayscale in FFX then FFXBitMask := FFXBitMask or fxbmGrayscale;
 if pfSepia in FFX then FFXBitMask := FFXBitMask or fxbmSepia;
 if pfSharpen in FFX then FFXBitMask := FFXBitMask or fxbmSharpen;
 if pfBlur in FFX then FFXBitMask := FFXBitMask or fxbmBlur;
end;

procedure TOGLCPostProcessingFX.EnableFX(aFXs: TOGLCPostFX);
begin
 if pfColorInversion in aFXs then Include( FFX, pfColorInversion );
 if pfGrayscale in aFXs then Include( FFX, pfGrayscale);
 if pfSepia in aFXs then Include( FFX, pfSepia);
 if pfSharpen in aFXs then Include( FFX, pfSharpen);
 if pfBlur in aFXs then Include( FFX, pfBlur);
 ComputeFXBitMask;
end;

procedure TOGLCPostProcessingFX.DisableFX(aFXs: TOGLCPostFX);
begin
 if pfColorInversion in aFXs then Exclude( FFX, pfColorInversion );
 if pfGrayscale  in aFXs then Exclude( FFX, pfGrayscale);
 if pfSepia  in aFXs then Exclude( FFX, pfSepia);
 if pfSharpen  in aFXs then Exclude( FFX, pfSharpen);
 if pfBlur  in aFXs then Exclude( FFX, pfBlur);
 ComputeFXBitMask;
end;

procedure TOGLCPostProcessingFX.DisableAllFX;
begin
 FFX:=[];
 ComputeFXBitMask;
end;

procedure TOGLCPostProcessingFX.SetParam(aTexture: PTexture; aColorAttachment: integer);
begin
 FTexture:=aTexture;
 FColorAttachmentIndex:=aColorAttachment;
end;



{ TOGLCShader }

procedure TOGLCShader.NotifyFragmentCompilationError( const aF: string );
var s: string;
begin
 if Length(aF)>100
   then s := copy(aF,1,100)
   else s := aF;
 raise Exception.Create('FRAGMENT SHADER NOT COMPILED...'+LINEENDING+s);
end;

procedure TOGLCShader.NotifyVertexCompilationError( const aF: string );
var s: string;
begin
 if Length(aF)>100
   then s := copy(aF,1,100)
   else s := aF;
 raise Exception.Create('Vertex Shader not compiled...'+LINEENDING+s);
end;

procedure TOGLCShader.NotifyLinkError;
begin
 raise Exception.Create('Shader Program not linked...');
end;

procedure TOGLCShader.NotifyGetUniformError(const s: string);
begin
 raise Exception.Create('SHADER ERROR on GetUniform with '+s);
end;

constructor TOGLCShader.Create(const AVertexCodeSource, AFragmentCodeSource: string);
var R : GLint ;
    code:string;
begin
 FProgramID := 0;
 FVertexID := 0;
 FFragmentID := 0;
 if ( Length (AVertexCodeSource ) = 0 ) and
    ( Length (AFragmentCodeSource ) = 0 )
    then exit;

 FProgramID := glCreateProgram();

 if Length (AVertexCodeSource ) > 0 then begin
        FVertexID := glCreateShader( GL_VERTEX_SHADER );
        code := AVertexCodeSource;
        glShaderSource( FVertexID, 1, @code, NIL );
        glCompileShader( FVertexID );
        glGetShaderiv( FVertexID, GL_COMPILE_STATUS, @R );
        if R=GL_FALSE
          then NotifyVertexCompilationError( AVertexCodeSource );
        glAttachShader( FProgramID, FVertexID );
 end;

 if Length (AFragmentCodeSource ) > 0 then begin
         FFragmentID := glCreateShader( GL_FRAGMENT_SHADER );
         code := AFragmentCodeSource;
         glShaderSource( FFragmentID, 1, @code, NIL );
         glCompileShader( FFragmentID );
         glGetShaderiv( FFragmentID, GL_COMPILE_STATUS, @R );
         if R=GL_FALSE
           then NotifyFragmentCompilationError( AFragmentCodeSource );
         glAttachShader( FProgramID, FFragmentID );
 end;

 glLinkProgram( FProgramID );
 glGetProgramiv(FProgramID, GL_LINK_STATUS, @R);
 if R=GL_FALSE
   then NotifyLinkError;

 glValidateProgram( FProgramID );
 if FFragmentID<>0 then glDeleteShader( FFragmentID );
 if FVertexID<>0 then glDeleteShader( FVertexID );

end;

destructor TOGLCShader.Destroy;
begin
 if FFragmentID<>0 then glDeleteShader( FFragmentID );
 if FVertexID<>0 then glDeleteShader( FVertexID );
 if FProgramID<>0 then glDeleteProgram( FProgramID );
 inherited Destroy;
end;

procedure TOGLCShader.Use;
begin
 glUseProgram( FProgramID );
end;

procedure TOGLCShader.Release;
begin
 glUseProgram( 0 );
end;

function TOGLCShader.GetUniform(const AName: string): GLint;
begin
 Result := glGetUniformLocation( FProgramID, PChar(AName) );
 if Result=-1 then NotifyGetUniformError(AName);
end;

procedure TOGLCShader.SetUniform1f(AUniform: GLint; AValue: single);
begin
 glUniform1f( AUniform, AValue );
end;

procedure TOGLCShader.SetUniform2f(AUniform: GLint; AValue1, AValue2: single);
begin
 glUniform2f( AUniform, AValue1, AValue2 );
end;

procedure TOGLCShader.SetUniform3f(AUniform: GLint; AValue1, AValue2, AValue3: single);
begin
 glUniform3f( AUniform, AValue1, AValue2, AValue3 );
end;

procedure TOGLCShader.SetUniform4f(AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single);
begin
 glUniform4f( AUniform, AValue1, AValue2, AValue3, AValue4 );
end;

procedure TOGLCShader.SetUniform1i(AUniform: GLint; AValue: GLint);
begin
 glUniform1i( AUniform, AValue );
end;

procedure TOGLCShader.SetUniformMatrix4(AUniform: GLint; AValue: PGLfloat);
begin
 glUniformMatrix4fv(AUniform, 1, GL_FALSE, AValue);
end;

{$endif oglcIMPLEMENTATION}

