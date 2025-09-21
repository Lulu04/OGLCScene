{$ifdef oglcINTERFACE}
type

{ TTexturedFont }

TTexturedFont = class{(TFPHashList)}
 private
  FParentScene: TOGLCScene;
  FAtlas: TOGLCTextureAtlas;
  FAtlasOwner: boolean;
  FTexCharArray: array of TTexturedChar;
  FFont: TFontDescriptor;
  function GetChar(aUTF8Char: string): PTexturedChar;
  procedure Init(aFont: TFontDescriptor);
  function GetTexture: PTexture;
  procedure InternalBuild(aPacker: TOGLCImagePacker; aPackerLoadedFromFile: boolean; const aCharSet: string;
        aFillTexture: TBGRABitmap);
 private
  class var FCharPrefix: integer; // used to  identify the char's texture into a loaded atlas
  class procedure NextCharPrefix;
  class function CharPrefix: string;
  class procedure ResetCharPrefix;
 public
  // This property affect the space between character
  //   value from 0.1 to 1 -> the characters are closest
  //   value equal to 1 -> normal font spacing
  //   value > 1 -> the characters are farther apart from each other
  CharSpacingCoeff: TBoundedFParam;
  Constructor Create(aParentScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap);
  constructor CreateFromImagePacker(aParentScene: TOGLCScene; aPacker: TOGLCImagePacker; aPackerLoadedFromFile: boolean;
     aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap);
  Destructor Destroy; override;

  function StringToCharIndexes(const aCaption: string): TTexturedCharIndexes;
  function CharIndexesToSize(const aIndexes: TTexturedCharIndexes): TSize;
  procedure DrawChar(aCharIdx: integer; aX, aY, aFlipIndex: integer;
                     const aClippingArea: PRect; const aModelViewMatrix: POGLCMatrix44;
                     const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
  procedure DrawChars(const aCharIdx: TTexturedCharIndexes; aX, aY, aFlipIndex: integer;
                      const aClippingArea: PRect; const aModelViewMatrix: POGLCMatrix44;
                      const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);

  // original size (equal to TBGRABitmap.TextSize())
  function GetCharWidth(const c: string): integer;
  function GetCharHeight(const c: string): integer;
  function GetTextWidth(const t: string): integer;
  function GetTextHeight(const t: string): integer;
  function GetCharSize(const c: string): TSize;
  // size with outline and shadow
  function GetDecoratedCharWidth(const c: string): integer;
  function GetDecoratedCharHeight(const c: string): integer;
  function GetDecoratedTextWidth(const t: string): integer;
  function GetDecoratedTextHeight(const t: string): integer;
  function GetDecoratedCharSize(const c: string): TSize;

  function IndexOf(const aUTF8Char: string): integer;
  function GetCharByIndex(aIndex: integer): PTexturedChar;
  property Chars[aUTF8Char: string]: PTexturedChar read GetChar;

  property Texture: PTexture read GetTexture;
  // a copy of the font descriptor used to construct this textured font.
  property Font: TFontDescriptor read FFont;
end;

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}

{ TTexturedFont }

constructor TTexturedFont.Create(aParentScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap);
begin
  FPArentScene := aParentScene;
  Init(aFont);
  FAtlasOwner := True;
  FAtlas := TOGLCTextureAtlas.Create(aParentScene);
  InternalBuild(FAtlas, False, aCharSet, aFillTexture);
  FAtlas.TryToPack;
  FAtlas.Build;
end;

constructor TTexturedFont.CreateFromImagePacker(aParentScene: TOGLCScene;
  aPacker: TOGLCImagePacker; aPackerLoadedFromFile: boolean;
  aFont: TFontDescriptor; const aCharSet: string;
  aFillTexture: TBGRABitmap);
begin
  FParentScene := aParentScene;
  Init(aFont);
  InternalBuild(aPacker, aPackerLoadedFromFile, aCharSet, aFillTexture);
  FAtlasOwner := False;
end;

destructor TTexturedFont.Destroy;
begin
  if FAtlasOwner then begin
    FAtlas.Free;
    FAtlas := NIL;
  end;
  FTexCharArray := NIL;
  CharSpacingCoeff.Free;
  CharSpacingCoeff := NIL;
  inherited Destroy;
end;

function TTexturedFont.IndexOf(const aUTF8Char: string): integer;
var i: integer;
begin
  if aUTF8Char <> '' then
    for i:=0 to Length(FTexCharArray)-1 do
      if aUTF8Char = FTexCharArray[i].Character then begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

procedure TTexturedFont.Init(aFont: TFontDescriptor);
begin
  FFont := aFont;
  FFont.Check;
  CharSpacingCoeff := CreateBoundedFParam(0.1, 100000, FALSE);
  CharSpacingCoeff.Value := 1.0;
end;

function TTexturedFont.GetChar(aUTF8Char: string): PTexturedChar;
var i: integer;
begin
  for i:=0 to High(FTexCharArray) do
    if FTexCharArray[i].Character = aUTF8Char then begin
      Result := @FTexCharArray[i];
      exit;
    end;
  Result := NIL;
end;

function TTexturedFont.GetTexture: PTexture;
begin
  Result := FAtlas.Texture;
end;


procedure TTexturedFont.InternalBuild(aPacker: TOGLCImagePacker; aPackerLoadedFromFile: boolean;
  const aCharSet: string; aFillTexture: TBGRABitmap);
var i: integer;
    temp: TBGRABitmap;
    s: string;
begin
  NextCharPrefix;

  temp := TBGRABitmap.Create(1, 1);
  FFont.SetFontRendererOn(temp);

  // build the font
  SetLength(FTexCharArray, UTF8LengthFast(aCharSet));
  for i:=0 to Length(FTexCharArray)-1 do
   begin
    s := UTF8Copy(aCharSet, i+1, 1);
    FFont.ResizeAndDrawStringOn(temp, s, aFillTexture);

    if aPackerLoadedFromFile then
      FTexCharArray[i].Texture := aPacker.RetrieveTextureByFileName(CharPrefix+s)
    else
      FTexCharArray[i].Texture := aPacker.Add(temp.Duplicate, CharPrefix+s);

    FTexCharArray[i].Character := s;
    FTexCharArray[i].DecoratedCharSize := Size(temp.Width, temp.Height);
    FTexCharArray[i].OriginalCharSize := temp.TextSize(s);
   end;
  temp.Free;
end;

class procedure TTexturedFont.NextCharPrefix;
begin
  inc(FCharPrefix);
end;

class function TTexturedFont.CharPrefix: string;
begin
  Result := FCharPrefix.ToString+'_';
end;

class procedure TTexturedFont.ResetCharPrefix;
begin
  FCharPrefix := 0;
end;

function TTexturedFont.StringToCharIndexes(const aCaption: string): TTexturedCharIndexes;
var i, charCount: Integer;
begin
  Result := NIL;
  charCount := UTF8Length(aCaption);
  if charCount = 0 then exit;
  SetLength(Result, charCount);
  for i:=1 to charCount do
    Result[i-1] := IndexOf(UTF8Copy(aCaption, i, 1));
end;

function TTexturedFont.CharIndexesToSize(const aIndexes: TTexturedCharIndexes): TSize;
var i: integer;
  ch: PTexturedChar;
  pixelCount, coefCharSpacing: single;
begin
  if Length(aIndexes) = 0 then Result.Create(0,0)
    else begin
      coefCharSpacing := CharSpacingCoeff.Value;
      pixelCount := 0;
      Result.cy := 0;
      for i:=0 to High(aIndexes) do begin
           ch := GetCharByIndex(aIndexes[i]);
           if ch <> NIL then with ch^ do begin
             if i = High(aIndexes) then pixelCount := pixelCount + DecoratedCharSize.cx * coefCharSpacing
               else pixelCount := pixelCount + OriginalCharSize.cx * coefCharSpacing;
             if Result.cy < DecoratedCharSize.cy then Result.cy := DecoratedCharSize.cy;
           end;
         end;
      Result.cx := Trunc(pixelCount);
    end;
end;

procedure TTexturedFont.DrawChar(aCharIdx: integer; aX, aY, aFlipIndex: integer; const aClippingArea: PRect;
  const aModelViewMatrix: POGLCMatrix44; const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
var tc: PTexturedChar;
  modelArea: TRect;
  texCoor: TQuadCoor;
begin
  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangles, FAtlas.Texture, 0, aBlendMode);
  tc := GetCharByIndex(aCharIdx);
  if tc <> NIL then begin
    modelArea := tc^.ModelArea;
    modelArea.Offset(Point(aX, aY));
    texCoor := tc^.Texture^.FramesCoord[0];
    if (aClippingArea <> NIL) and not CheckAndAdjust(aClippingArea^, modelArea, texCoor) then exit;
    FParentScene.TexturedMVTriangleRenderer.PushQuad3(modelArea, texCoor, aFlipIndex, aModelViewMatrix,
                                                     aOpacity, aComputedTint);
  end;
end;

procedure TTexturedFont.DrawChars(const aCharIdx: TTexturedCharIndexes; aX, aY, aFlipIndex: integer;
   const aClippingArea: PRect; const aModelViewMatrix: POGLCMatrix44; const aOpacity: single;
   const aComputedTint: TColorF; aBlendMode: byte);
var i, xx, yy: integer;
  tc: PTexturedChar;
  modelArea: TRect;
  texCoor: TQuadCoor;
  spaceCoef: single;
  flagDraw: Boolean;
begin
  if Length(aCharIdx) = 0 then exit;
  FParentScene.TexturedMVTriangleRenderer.Prepare(ptTriangles, FAtlas.Texture, 0, aBlendMode);
  xx := aX;
  yy := aY;
  spaceCoef := CharSpacingCoeff.Value;
  for i:=0 to High(aCharIdx) do begin
   tc := GetCharByIndex(aCharIdx[i]);
   if tc <> NIL then begin
     modelArea := tc^.ModelArea;
     modelArea.Offset(Point(xx, yy));
     texCoor := tc^.Texture^.FramesCoord[0];
     if aClippingArea <> NIL then flagDraw := CheckAndAdjust(aClippingArea^, modelArea, texCoor)
       else flagDraw := True;
     if flagDraw then
       FParentScene.TexturedMVTriangleRenderer.PushQuad3(modelArea, texCoor, aFlipIndex, aModelViewMatrix,
                                                        aOpacity, aComputedTint);
     xx := xx + Trunc(tc^.OriginalCharSize.cx * spaceCoef);
   end;
  end;
end;

function TTexturedFont.GetCharWidth(const c: string): integer;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := Round(FTexCharArray[i].OriginalCharSize.cx*CharSpacingCoeff.Value);
end;

function TTexturedFont.GetCharHeight(const c: string): integer;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].OriginalCharSize.cy ;
end;

function TTexturedFont.GetTextWidth(const t: string): integer;
var i: integer;
begin
  Result := 0;
  for i:=1 to UTF8Length(t) do Result += GetCharWidth(UTF8Copy( t, i, 1 ));
end;

function TTexturedFont.GetTextHeight(const t: string): integer;
var i, h: integer;
begin
  Result := 0;
  for i:=1 to UTF8Length(t) do begin
   h := GetCharHeight( UTF8Copy( t, i, 1 ));
   if Result<h then Result :=h;
  end;
end;

function TTexturedFont.GetCharSize(const c: string): TSize;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := Size(0,0)
    else Result := Size(Round(FTexCharArray[i].OriginalCharSize.cx*CharSpacingCoeff.Value), FTexCharArray[i].OriginalCharSize.cy);
end;

function TTexturedFont.GetDecoratedCharWidth(const c: string): integer;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].DecoratedCharSize.cx;
end;

function TTexturedFont.GetDecoratedCharHeight(const c: string): integer;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].DecoratedCharSize.cy ;
end;

function TTexturedFont.GetDecoratedTextWidth(const t: string): integer;
var i: integer;
begin
  Result := 0;
  for i:=1 to UTF8Length(t) do Result += GetDecoratedCharWidth( UTF8Copy( t, i, 1 ));
end;

function TTexturedFont.GetDecoratedTextHeight(const t: string): integer;
var i, h: integer;
begin
  Result := 0;
  for i:=1 to UTF8Length(t) do begin
   h := GetDecoratedCharHeight( UTF8Copy( t, i, 1 ));
   if Result<h then Result :=h;
  end;
end;

function TTexturedFont.GetDecoratedCharSize(const c: string): TSize;
var i: integer;
begin
  i := IndexOf( c );
  if i=-1
    then Result := Size(0,0)
    else Result := FTexCharArray[i].DecoratedCharSize ;
end;

function TTexturedFont.GetCharByIndex(aIndex: integer): PTexturedChar;
begin
  if (aIndex >= 0) and (aIndex < Length(FTexCharArray)) then
    Result := @FTexCharArray[aIndex]
  else Result := NIL;
end;

{$endif oglcIMPLEMENTATION}

