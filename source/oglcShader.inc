{$ifdef oglcINTERFACE}
type

{ TOGLCShader }

TOGLCShader = class
 private
  FParentScene: TOGLCScene;
  FProgramID,
  FFragmentID,
  FVertexID: GLuint;
  procedure NotifyGetUniformError(const s: string); inline;
  protected
  function GetUniform(const AName: string): GLint;
  procedure SetUniform1f(AUniform: GLint; AValue: single);
  procedure SetUniform2f(AUniform: GLint; AValue1, AValue2: single);
  procedure SetUniform3f(AUniform: GLint; AValue1, AValue2, AValue3: single);
  procedure SetUniform4f(AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single);
  procedure SetUniform1i(AUniform: GLint; AValue: GLint);
  procedure SetUniformMatrix4(AUniform: GLint; AValue: PGLfloat);
 public
  constructor Create(aParentScene: TOGLCScene; const AVertexCodeSource, AFragmentCodeSource: string);
  destructor Destroy; override;

  procedure Use;
  procedure Release;
  function Ready: boolean;
  property ID: GLuint read FProgramID;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCShader }

procedure TOGLCShader.NotifyGetUniformError(const s: string); inline;
begin
  raise Exception.Create('GetUniform error on '+s);
end;

constructor TOGLCShader.Create(aParentScene: TOGLCScene; const AVertexCodeSource, AFragmentCodeSource: string);
var R : GLint ;
    code:string;
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
    raise Exception.Create(aPrefix+' '+msg);
  end;
begin
  FParentScene := aParentScene;
  FProgramID := 0;
  FVertexID := 0;
  FFragmentID := 0;
  if (Length(AVertexCodeSource) = 0) and
     (Length(AFragmentCodeSource) = 0)
    then exit;

  FProgramID := glCreateProgram();

  if Length(AVertexCodeSource) > 0 then begin
    FVertexID := glCreateShader(GL_VERTEX_SHADER);
    code := AVertexCodeSource;
    glShaderSource(FVertexID, 1, @code, NIL);
    glCompileShader(FVertexID);
    glGetShaderiv(FVertexID, GL_COMPILE_STATUS, @R);
    if R = GL_FALSE then NotifyError(FVertexID, 'VERTEX SHADER');
    glAttachShader( FProgramID, FVertexID );
  end;

  if Length(AFragmentCodeSource) > 0 then begin
    FFragmentID := glCreateShader( GL_FRAGMENT_SHADER);
    code := AFragmentCodeSource;
    glShaderSource(FFragmentID, 1, @code, NIL);
    glCompileShader(FFragmentID );
    glGetShaderiv(FFragmentID, GL_COMPILE_STATUS, @R);
    if R = GL_FALSE then NotifyError(FFragmentID, 'FRAGMENT SHADER');
    glAttachShader(FProgramID, FFragmentID);
  end;

  glLinkProgram(FProgramID);
  glGetProgramiv(FProgramID, GL_LINK_STATUS, @R);
  if R = GL_FALSE then NotifyError(FProgramID, 'PROGRAM LINK ERROR');

  glValidateProgram(FProgramID);
  if FFragmentID <> 0 then glDeleteShader(FFragmentID);
  if FVertexID <> 0 then glDeleteShader(FVertexID);
end;

destructor TOGLCShader.Destroy;
begin
  if FFragmentID <> 0 then glDeleteShader(FFragmentID);
  if FVertexID <> 0 then glDeleteShader(FVertexID);
  if FProgramID <> 0 then glDeleteProgram(FProgramID);
  inherited Destroy;
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
  if Result = -1 then NotifyGetUniformError(AName);
end;

procedure TOGLCShader.SetUniform1f(AUniform: GLint; AValue: single);
begin
 glUniform1f(AUniform, AValue);
end;

procedure TOGLCShader.SetUniform2f(AUniform: GLint; AValue1, AValue2: single);
begin
  glUniform2f(AUniform, AValue1, AValue2);
end;

procedure TOGLCShader.SetUniform3f(AUniform: GLint; AValue1, AValue2, AValue3: single);
begin
  glUniform3f(AUniform, AValue1, AValue2, AValue3);
end;

procedure TOGLCShader.SetUniform4f(AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single);
begin
  glUniform4f(AUniform, AValue1, AValue2, AValue3, AValue4);
end;

procedure TOGLCShader.SetUniform1i(AUniform: GLint; AValue: GLint);
begin
  glUniform1i(AUniform, AValue);
end;

procedure TOGLCShader.SetUniformMatrix4(AUniform: GLint; AValue: PGLfloat);
begin
  glUniformMatrix4fv(AUniform, 1, GL_FALSE, AValue);
end;

{$endif oglcIMPLEMENTATION}

