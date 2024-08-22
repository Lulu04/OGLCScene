{$ifdef oglcINTERFACE}
type

{ TGpuInfo }

TGpuInfo = record
  function RendererName: string;
  function Version: string;
  function TotalVideoRamKb: integer;
  function FreeVideoRamKb: integer;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TGpuInfo }

function TGpuInfo.RendererName: string;
begin
  if glGetString <> NIL then begin
    Result := glGetString(GL_RENDERER);
  end else Result := '';
end;

function TGpuInfo.Version: string;
begin
  if glGetString <> NIL then begin
    Result := glGetString(GL_VERSION);
  end else Result := '';
end;

// https://stackoverflow.com/questions/4552372/determining-available-video-memory
function TGpuInfo.TotalVideoRamKb: integer;
const GL_GPU_MEM_INFO_TOTAL_AVAILABLE_MEM_NVX = $9048;
      TEXTURE_FREE_MEMORY_ATI = $87FC;
  var m: array[0..3] of GLint;
begin
  Result := 0;
  if glGetIntegerv <> NIL then begin
    FillChar(m[0], Length(m)*SizeOf(GLInt), 0);
    glGetIntegerv(GL_GPU_MEM_INFO_TOTAL_AVAILABLE_MEM_NVX, @m[0]);
    glGetIntegerv(TEXTURE_FREE_MEMORY_ATI, @m[0]);
    glGetError();
    Result := m[0];
  end;
end;

// https://stackoverflow.com/questions/4552372/determining-available-video-memory
function TGpuInfo.FreeVideoRamKb: integer;
const GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = $9049;
      TEXTURE_FREE_MEMORY_ATI = $87FC;
  var m: array[0..3] of GLint;
begin
  Result := 0;
  if glGetIntegerv <> NIL then begin
    FillChar(m[0], Length(m)*SizeOf(GLInt), 0);

    // try with NVidia
    glGetIntegerv(GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX, @m[0]);
    if glGetError() = GL_NO_ERROR then Exit(m[0]);

    // try with ATI
    glGetIntegerv(TEXTURE_FREE_MEMORY_ATI, @m[0]);
    if glGetError() = GL_NO_ERROR then Exit(m[1]);
  end;
end;


{$endif oglcIMPLEMENTATION}

