{$ifdef oglcINTERFACE}
type

PShader = ^TShader;
TShader = record
             ProgramID,
             FragmentID,
             VertexID: GLUInt;
end;

const INVALID_SHADER_ID = -1;
type
TShaderID = integer;

{ TShaderManager }

TShaderManager = class
 private
  FList : TList;
  procedure NotifyFragmentCompilationError;
  procedure NotifyVertexCompilationError;
  procedure NotifyLinkError;
  function getShader( aIndex: integer ): PShader;
  procedure Clear;
 public
  constructor Create;
  Destructor Destroy; override;
  function ShaderAvailable: boolean;

  function CreateShader( const AVertexCodeSource, AFragmentCodeSource: string ): TShaderID; // return created shader ID
  procedure DeleteShader( AShader: TShaderID );

  procedure Use( AShader: TShaderID );
  procedure UseNone;

  function GetUniform( AShader: TShaderID; const AName ): GLint;
  procedure SetUniform1f( AUniform: GLint; AValue: single );
  procedure SetUniform2f( AUniform: GLint; AValue1, AValue2: single );
  procedure SetUniform3f( AUniform: GLint; AValue1, AValue2, AValue3: single );
  procedure SetUniform4f( AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single );

  procedure SetUniform1i( AUniform: GLint; AValue: GLint );
end;

var
 ShaderManager : TShaderManager=NIL;

const
 TEST_VERTEX_SHADER_PRG = '#version 130'#10+
                          'varying vec2 UV;'#10+
                          'void main() {'#10+
                          'gl_Position = gl_ProjectionMatrix * gl_Vertex;'#10 +
                          'UV = vec2(gl_MultiTexCoord0);'#10 +
                          '}';

 TEST_FRAGMENT_SHADER_PRG = '#version 130'#10+
                            'varying vec2 UV;'#10+
                            'uniform sampler2D Image;'#10+
                            'out vec4 FragmentColor;'#10+
                            'void main() {'#10+
                            'float TintFactor=0.1;'#10+
                            'vec4 ImageColor = texture2D( Image, UV );'#10+
                            'vec4 TintColor = vec4(1,0,0,1);'#10+
                            'gl_FragData[0] = ImageColor * (1-TintFactor) + TintColor * TintFactor;'#10+
                            '}' ;

 // sin
 SIN_VERTEX_SHADER_PRG = 'void main() {'+
                         'gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;'+
                         'gl_Position = ftransform();'+
                         'gl_FrontColor = gl_Color;'+
                         '}';
 SIN_FRAGMENT_SHADER_PRG = 'uniform sampler2D tex;'+
                           'void main() {'+
                           'vec4 pixel = texture2D(tex, gl_TexCoord[0].xy);'+
                           'gl_FragColor = vec4(0.0,1.0,0.0,1.0);'+//* pixel;'+ // gl_Color * pixel;
                           '}' ;
{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
{ TShaderManager }

procedure TShaderManager.NotifyFragmentCompilationError;
begin
 raise Exception.Create('Fragment Shader not compiled...');
end;

procedure TShaderManager.NotifyVertexCompilationError;
begin
 raise Exception.Create('Vertex Shader not compiled...');
end;

procedure TShaderManager.NotifyLinkError;
begin
 raise Exception.Create('Shader Program not linked...');
end;

function TShaderManager.getShader(aIndex: integer): PShader;
begin
 if (aIndex < 0) or (aIndex >= FList.Count)
   then Result := NIL
   else Result := PShader( FList.Items[aIndex] );
end;

procedure TShaderManager.Clear;
begin
 while FList.Count > 0 do DeleteShader( 0 );
end;

constructor TShaderManager.Create ;
begin
 FList := TList.Create;
end;

destructor TShaderManager.Destroy;
begin
 Clear;
 FreeAndNil( FList );
 inherited Destroy;
end;

function TShaderManager.ShaderAvailable: boolean;
begin
 Result := OpenGL_Version_2_0_Loaded;
end;

function TShaderManager.CreateShader(const AVertexCodeSource, AFragmentCodeSource: string): TShaderID;
var R : GLint ;
    code:string;
    o: PShader;
begin
 Result := INVALID_SHADER_ID;
 if not ShaderAvailable then exit;

 o := new(PShader);

 o^.ProgramID := 0;
 o^.VertexID := 0;
 o^.FragmentID := 0;
 if ( Length (AVertexCodeSource ) = 0 ) and
    ( Length (AFragmentCodeSource ) = 0 )
    then exit;

 o^.ProgramID := glCreateProgram();

 if Length (AVertexCodeSource ) > 0
  then begin
        o^.VertexID := glCreateShader( GL_VERTEX_SHADER ) ;
        code := AVertexCodeSource;
        glShaderSource( o^.VertexID, 1, @code, NIL ) ;
        glCompileShader( o^.VertexID ) ;
        glGetShaderiv( o^.VertexID, GL_COMPILE_STATUS, @R ) ;
        if R = GL_FALSE then NotifyVertexCompilationError;

        glAttachShader( o^.ProgramID, o^.VertexID ) ;
  end;

 if Length (AFragmentCodeSource ) > 0
   then begin
         o^.FragmentID := glCreateShader( GL_FRAGMENT_SHADER );
         code := AFragmentCodeSource;
         glShaderSource( o^.FragmentID, 1, @code, NIL );
         glCompileShader( o^.FragmentID );
         glGetShaderiv( o^.FragmentID, GL_COMPILE_STATUS, @R );
         if R = GL_FALSE then NotifyFragmentCompilationError;

         glAttachShader( o^.ProgramID, o^.FragmentID );
   end;

 glLinkProgram ( o^.ProgramID );
 glGetProgramiv(o^.ProgramID, GL_LINK_STATUS, @R);
 if R = GL_FALSE then NotifyLinkError;

 glValidateProgram ( o^.ProgramID );

 Result := TShaderID( FList.Add( o ) );
end;

procedure TShaderManager.DeleteShader(AShader: TShaderID);
var o: PShader;
begin
 if not ShaderAvailable then exit;
 o := GetShader( AShader );
 if o = NIL then exit;
 with o^ do
  begin
   glDeleteShader( FragmentID );
   glDeleteShader( VertexID );
   glDeleteProgram( ProgramID )
  end;
 Dispose( o );
 FList.Delete( AShader );
end;

procedure TShaderManager.Use(AShader: TShaderID);
var o: PShader;
begin
 if not ShaderAvailable then exit;
 o := getShader(AShader);
 if o = NIL
   then glUseProgram( 0 )
   else glUseProgram( o^.ProgramID );
end;

procedure TShaderManager.UseNone;
begin
 if ShaderAvailable then glUseProgram( 0 );
end;

function TShaderManager.GetUniform(AShader: TShaderID; const AName): GLint;
var o: PShader;
begin
 Result := 0;
 if not ShaderAvailable then exit;
 o := getShader( AShader);
 if o <> NIL
   then Result := glGetUniformLocation( o^.ProgramID, @AName );
end;

procedure TShaderManager.SetUniform1f(AUniform: GLint; AValue: single);
begin
 if ShaderAvailable then glUniform1f( AUniform, AValue );
end;

procedure TShaderManager.SetUniform2f(AUniform: GLint; AValue1, AValue2: single);
begin
 if ShaderAvailable then glUniform2f( AUniform, AValue1, AValue2 );
end;

procedure TShaderManager.SetUniform3f(AUniform: GLint; AValue1, AValue2, AValue3: single);
begin
 if ShaderAvailable then glUniform3f( AUniform, AValue1, AValue2, AValue3 );
end;

procedure TShaderManager.SetUniform4f(AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single);
begin
 if ShaderAvailable then glUniform4f( AUniform, AValue1, AValue2, AValue3, AValue4 );
end;

procedure TShaderManager.SetUniform1i(AUniform: GLint; AValue: GLint);
begin
 if ShaderAvailable then glUniform1i( AUniform, AValue );
end;
{$endif oglcIMPLEMENTATION}

