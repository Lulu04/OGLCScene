{$ifdef oglcINTERFACE}
type

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

{$endif oglcIMPLEMENTATION}

