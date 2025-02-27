{$ifdef oglcINTERFACE}
type

{ TOGLCShader }

TOGLCShader = record
 private
  FParentScene: TOGLCScene;
  FProgramID: GLuint;
 public
  procedure Compile(aParentScene: TOGLCScene; const AVertexCodeSource, AFragmentCodeSource: string);
  procedure FreeID;

  // activates the shader
  procedure Use;
  // deactivates the shader
  procedure Release;
  // use this method to retrieve the location of a uniform.
  // an error message is logged in the parent scene in case of failure.
  function GetUniform(const AName: string): GLint;
  // return True if no error append in the compile process.
  function Ready: boolean;
  // the GL id of the shader
  property ID: GLuint read FProgramID;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCShader }

procedure TOGLCShader.Compile(aParentScene: TOGLCScene; const AVertexCodeSource, AFragmentCodeSource: string);
var R: GLint;
    FFragmentID, FVertexID: GLuint;
  procedure NotifyError(aPrg: GLUint; const aPrefix: string);
  var log_length: GLint=0;
      logBuf: TBytes;
      msg: string='';
  begin
    glGetShaderiv(aPrg, GL_INFO_LOG_LENGTH, @log_length);
    logBuf := NIL;
    if log_length > 2 then begin
      SetLength(logBuf, log_length);
      glGetShaderInfoLog(aPrg, log_length, @log_length, @logBuf[0]);
      msg := TEncoding.ANSI.GetAnsiString(logBuf);
    end;
    FParentScene.LogError('TOGLCShader.Compile: an error occured'+LineEnding+aPrefix+' '+msg);
    FParentScene.LogEmptyLine();
    {$ifdef SHADER_RAISE_EXCEPTION_ON_COMPILATION_ERROR}
    raise Exception.Create(aPrefix+' '+msg);
    {$endif}
  end;
begin
  FParentScene := aParentScene;
  FProgramID := 0;

  if (Length(AVertexCodeSource) = 0) and
     (Length(AFragmentCodeSource) = 0)
    then exit;

  FProgramID := glCreateProgram();

  if FProgramID = 0 then exit;

  FVertexID := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(FVertexID, 1, @AVertexCodeSource, NIL);
  glCompileShader(FVertexID);
  glGetShaderiv(FVertexID, GL_COMPILE_STATUS, @R);
  if R = GL_FALSE then begin
    NotifyError(FVertexID, 'VERTEX SHADER');
    glDeleteShader(FVertexID);
    glDeleteProgram(FProgramID);
    FProgramID := 0;
  end else glAttachShader(FProgramID, FVertexID);

  if FProgramID = 0 then exit;

  FFragmentID := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(FFragmentID, 1, @AFragmentCodeSource, NIL);
  glCompileShader(FFragmentID);
  glGetShaderiv(FFragmentID, GL_COMPILE_STATUS, @R);
  if R = GL_FALSE then begin
    NotifyError(FFragmentID, 'FRAGMENT SHADER');
    glDeleteShader(FFragmentID);
    glDeleteProgram(FProgramID);
    FProgramID := 0;
  end else glAttachShader(FProgramID, FFragmentID);

  if FProgramID = 0 then exit;

  glLinkProgram(FProgramID);
  glGetProgramiv(FProgramID, GL_LINK_STATUS, @R);
  if R = GL_FALSE then NotifyError(FProgramID, 'PROGRAM LINK ERROR');

  glValidateProgram(FProgramID);
  if FFragmentID <> 0 then glDeleteShader(FFragmentID);
  if FVertexID <> 0 then glDeleteShader(FVertexID);

  glGetError();
end;

procedure TOGLCShader.FreeID;
begin
  if FProgramID <> 0 then glDeleteProgram(FProgramID);
  FProgramID := 0;
end;

procedure TOGLCShader.Use;
begin
  if FProgramID <> FParentScene.FCurrentShaderIDInUse then
  begin
    glUseProgram(FProgramID);
    FParentScene.FCurrentShaderIDInUse := FProgramID;
  end;
end;

procedure TOGLCShader.Release;
begin
  glUseProgram(0);
  FParentScene.FCurrentShaderIDInUse := 0;
end;

function TOGLCShader.Ready: boolean;
begin
  Result := FProgramID <> 0;
end;

function TOGLCShader.GetUniform(const AName: string): GLint;
begin
  Result := glGetUniformLocation(FProgramID, PChar(AName));
  if Result = -1 then
    FParentScene.LogError('glGetUniformLocation error: '''+AName+''' not found');
end;

{$endif oglcIMPLEMENTATION}

