{$ifdef oglcINTERFACE}
type

{ TOGLCShader }

TOGLCShader = class
 private
  class var FCurrentShaderIDInUse: GLuint;
 private
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
  constructor Create(const AVertexCodeSource, AFragmentCodeSource: string);
  destructor Destroy; override;

  procedure Use;
  class procedure Release;
  function Ready: boolean;
  property ID: GLuint read FProgramID;
end;


{ TVertexLayout }
PVertexLayout = ^TVertexLayout;
TVertexLayout = packed record
private const MAX_ATTRIBUTES = 8;
private type
  TAttribute = packed record
    Location: GLint;
    _Type: GLenum;
    Size: GLint;
    Normalized: GLboolean;
    Stride: GLsizei;
    PointerOffset: PGLvoid;
  end;
private
  FProgramID: GLuint;
  FAttributes: array of TAttribute;
  FStride, FAttributeCount: integer;
  function GetAttributes(index: integer): TAttribute;
public
  function Init(aShader: TOGLCShader): PVertexLayout;
  function AddFloatAttribute(const aAttribName: string; aFloatCount: Integer; aNormalized: Boolean): PVertexLayout;
  function Count: integer;
  property Attributes[index: integer]: TAttribute read GetAttributes; default;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCShader }

procedure TOGLCShader.NotifyGetUniformError(const s: string); inline;
begin
  raise Exception.Create('GetUniform error on '+s);
end;

constructor TOGLCShader.Create(const AVertexCodeSource, AFragmentCodeSource: string);
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
  if FProgramID <> FCurrentShaderIDInUse then
  begin
    glUseProgram(FProgramID);
    FCurrentShaderIDInUse := FProgramID;
  end;
end;

class procedure TOGLCShader.Release;
begin
  glUseProgram(0);
  FCurrentShaderIDInUse := 0;
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

{ TVertexLayout }

function TVertexLayout.GetAttributes(index: integer): TAttribute;
begin
  Result := FAttributes[index];
end;

function TVertexLayout.Init(aShader: TOGLCShader): PVertexLayout;
begin
  FProgramID := aShader.ID;
  FAttributes := NIL;
  FStride := 0;
  FAttributeCount := 0;
  Result := @Self;
end;

function TVertexLayout.AddFloatAttribute(const aAttribName: string; aFloatCount: Integer; aNormalized: Boolean): PVertexLayout;
var loc: GLint;
begin
  if (FAttributeCount = MAX_ATTRIBUTES) then
    raise Exception.Create('Too many attributes in vertex layout');

  loc := glGetAttribLocation(FProgramID, PChar(aAttribName));
  if loc < 0 then
    raise Exception.CreateFmt('Attribute "%s" not found in shader', [aAttribName]);

  SetLength(FAttributes, FAttributeCount+1);
  with FAttributes[FAttributeCount] do
  begin
    Location := loc;
    Size := GLint(aFloatCount);
    _Type := GL_FLOAT;
    if aNormalized then Normalized := GL_TRUE
      else Normalized := GL_FALSE;
    PointerOffset := PChar(FStride);
  end;
  inc(FAttributeCount);
  FStride := FStride + aFloatCount * SizeOf(single);
  Result := @Self;
end;

function TVertexLayout.Count: integer;
begin
  Result := Length(FAttributes);
end;


{$endif oglcIMPLEMENTATION}

