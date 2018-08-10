{$ifdef oglcINTERFACE}
const
  OGLC_DEFAULT_CHAR_SET = ' '+
                          '0123456789'+
                          'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                          'ÉéÈèÊêËëÁáÂâÀàÆæÇçÙùÛûÏïÎîÔôŒœŸÿ'+
                          'ÄäÖöÜüß'+
                          'Ññ'+
                          '?!,.;:/*-+=()[]@';
type

// construct an image with each character in the charset.

TTexturedChar = record
  Character: string;
  DecoratedCharSize,          // size of the character with outline and shadow
  OriginalCharSize: TSize;    // normal size of the character (as given by TBGRABitmap.TextSize())
  TexCoor: TTexCoor;          // texture coordinates for this character
end;
PTexturedChar=^TTexturedChar;

{ TTexturedFont }

TTexturedFont = class
  Constructor Create( aFont: TGuiFont; const aCharSet: string);
  Destructor Destroy; override;
 private
  FTexture: PTexture;
  FTexCharArray: array of TTexturedChar ;
  FFont: TGuiFont;
  function IndexOf( c: string ): integer;
  procedure BuildTexture( aFont: TGuiFont; const aCharSet: string );
  function GetTexturedChar( aChar: string ): PTexturedChar;
  procedure DrawChar( aChar: PTexturedChar; const aLoc: TPointF; const aLayerPercentOpacity: single;
                      aSetTextureParameters: boolean=TRUE);
 public
  CharSpaceCoeff: TBoundedFParam; // coeff affect the space between character
                                  // [0..1[ the characters are close together
                                  // 1  normal
                                  // >1   the characters are farther apart from each other
  procedure DrawText( aText: string; aLocation: TPointF; const aLayerPercentOpacity: single;
                      aSetTextureParameters: boolean=TRUE );
  function GetCharWidth( const c: string ): integer;
  function GetCharHeight( const c: string ): integer;
  function GetTextWidth( const t: string ): integer;
  function GetTextHeight( const t: string ): integer;
  function GetCharSize( const c: string ): TSize;
  property Texture: PTexture read FTexture;
end;


{ TFontManager }

TFontManager = class
 constructor Create;
 Destructor Destroy; override;
private
 FList: TList;
 function GetCount: integer;
 procedure Update( const aElapsedTime: single );
public
 function AddFont( aGuiFont: TGuiFont; const aCharSet: string=OGLC_DEFAULT_CHAR_SET): TTexturedFont;
 procedure DeleteFont( var aFont: TTexturedFont );
 procedure Clear;
 property Count: integer read GetCount;
end;

var
 FontManager : TFontManager ;


type

{ TFreeText }

TFreeText = class(TSimpleSurfaceWithEffect)
private
  FCaption: string;
  FTexturedFont: TTexturedFont;
  function GetCaption: string;
  procedure SetCaption(AValue: string);
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
  procedure Update( const aElapsedTime: single ); override;
  procedure Draw( const aLayerPercentOpacity: single );  override;
public
  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write FTexturedFont;
end;

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}
 { TFreeText }

 function TFreeText.GetCaption: string;
 begin
  Result := FCaption;
 end;

 procedure TFreeText.SetCaption(AValue: string);
 begin
  FCaption := AValue;
 end;

 function TFreeText.GetWidth: integer;
 begin
  Result := FTexturedFont.GetTextWidth( FCaption );
 end;

 function TFreeText.GetHeight: integer;
 begin
  Result := FTexturedFont.GetTextHeight( FCaption );
 end;

 procedure TFreeText.Update(const aElapsedTime: single);
 begin
  inherited Update(aElapsedTime);
 end;

 procedure TFreeText.Draw(const aLayerPercentOpacity: single);
 var AlphaTint: byte;
 begin
  if FCaption='' then exit;
  if FTexturedFont=NIL then exit;
  PushAndPrepareMatrix;
  SetBlendMode( FBlendMode );
  TextureManager.BindTexture( FTexturedFont.Texture );

  if Tint.alpha.Value < 255
    then begin
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
          glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
          FTexturedFont.DrawText( FCaption, PointF(0,0), Opacity.Value/255*aLayerPercentOpacity, FALSE );
         end;

  if Tint.alpha.Value <> 0
    then begin  //Apply Tint
          glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
          glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
          glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
          AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
          glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
          FTexturedFont.DrawText( FCaption, PointF(0,0), Opacity.Value/255*aLayerPercentOpacity, FALSE );
         end;

  DrawChildSurfaces( aLayerPercentOpacity*Opacity.pcValue );
  PopMatrix;
 end;

{ TTexturedFont }

  constructor TTexturedFont.Create(aFont: TGuiFont; const aCharSet: string);
 begin
  SetLength( FTexCharArray, 0 );
  BuildTexture( aFont, aCharSet );
  CharSpaceCoeff:= CreateBoundedFParam(0, 1000000, FALSE );
  CharSpaceCoeff.Value := 1.0;
 end;

 destructor TTexturedFont.Destroy;
 begin
  TextureManager.Delete( FTexture );
  SetLength( FTexCharArray, 0 );
  CharSpaceCoeff.Free;
  inherited Destroy;
 end;

 function TTexturedFont.IndexOf(c: string): integer;
 var i: integer;
 begin
  Result := -1;
  for i:=0 to Length(FTexCharArray)-1 do
   if c = FTexCharArray[i].Character
      then begin
            Result := i;
            exit ;
           end;
 end;


 procedure TTexturedFont.BuildTexture(aFont: TGuiFont; const aCharSet: string);
 var i, j, xx, yy, maxCharHeight : integer;
     ima, temp: TBGRABitmap;
     s1 : string;
     renderer: TBGRATextEffectFontRenderer;

 begin
  FFont := aFont;
  if FFont.FontHeight>150 then FFont.FontHeight:=150; // max FontHeight is 150

  renderer:= TBGRATextEffectFontRenderer.Create;
  with FFont do begin
    renderer.ShadowVisible := ShadowColor<>BGRAPixelTransparent;
    renderer.ShadowColor := ShadowColor;
    renderer.ShadowOffset.x := ShadowOffsetX;
    renderer.ShadowOffset.y := ShadowOffsetY;
    renderer.ShadowRadius := ShadowRadius;
    renderer.OutlineVisible := (OutLineColor<>BGRAPixelTransparent) and (OutLineWidth>0);
    renderer.OutlineColor := OutLineColor;
    renderer.OuterOutlineOnly := True;
    renderer.OutlineWidth := OutLineWidth;

    ima:= TBGRABitmap.Create(1024, 1024, BGRAPixelTransparent );
    ima.FontRenderer := renderer;
    ima.FontQuality:= fqFineAntialiasing;
    ima.FontName := FontName;
    ima.FontHeight := FontHeight;
    ima.FontStyle := Style;
  end;

  // build the font
  SetLength(FTexCharArray, UTF8Length(aCharSet));
  xx:=0;
  yy:=0;
  maxCharHeight:=0;
  for i:=0 to Length(FTexCharArray)-1 do
   begin
    s1 := UTF8Copy( aCharSet, i+1, 1 );
    temp := TextToBitmap( s1, FFont, NIL );

    if xx+temp.Width>ima.Width then begin
          // next line
          xx := 0;
          yy += maxCharHeight;
    end;

    ima.PutImage(xx, yy, temp, dmDrawWithTransparency);
    if maxCharHeight < temp.Height then maxCharHeight:=temp.Height;

    FTexCharArray[i].Character := s1;
    FTexCharArray[i].DecoratedCharSize := Size(temp.Width, temp.Height);
    FTexCharArray[i].OriginalCharSize := temp.TextSize(s1);
    // coordinates of the character on the whole image
    FTexCharArray[i].TexCoor[0].x := xx;
    FTexCharArray[i].TexCoor[0].y := yy;
    FTexCharArray[i].TexCoor[1].x := xx+temp.Width;
    FTexCharArray[i].TexCoor[1].y := yy;
    FTexCharArray[i].TexCoor[2].x := xx+temp.Width;
    FTexCharArray[i].TexCoor[2].y := yy+temp.Height;
    FTexCharArray[i].TexCoor[3].x := xx;
    FTexCharArray[i].TexCoor[3].y := yy+temp.Height;
    xx += temp.Width;
    temp.Free;
   end;

  // reduce the size of the image to remove the unused pixels
  if yy>0 then begin
     // reduce only verticaly because all width is occupied
     temp := TBGRABitmap.Create( ima.Width, yy+maxCharHeight );
  end else begin
     // reduce both horizontaly and verticaly
     temp := TBGRABitmap.Create( xx, yy+maxCharHeight );
  end;

  temp.PutImage( 0, 0, ima, dmSet );
  ima.Free;

  // transform texture coordinate in their equivalent range [0..1]
  for i:=0 to Length(FTexCharArray)-1 do
    with FTexCharArray[i] do begin
     for j:=0 to 3 do begin
       TexCoor[j].x := TexCoor[j].x/temp.Width;
       TexCoor[j].y := (temp.Height-TexCoor[j].y)/temp.Height;
     end;
    end;

  // create the texture from image
  FTexture := TextureManager.Add( temp );
  temp.Free;
 end;

 function TTexturedFont.GetTexturedChar(aChar: string): PTexturedChar;
 var i: integer;
 begin
  i := IndexOf( aChar );
  if i=-1
    then Result := NIL
    else Result := @FTexCharArray[i];
 end;

 procedure TTexturedFont.DrawChar(aChar: PTexturedChar; const aLoc: TPointF;
   const aLayerPercentOpacity: single; aSetTextureParameters: boolean);
 var
   quad : TTexCoor;
   tci  : PTextureCoordIndex;
   procedure DrawVertex;
    begin
     glBegin( GL_QUADS );
       glTexCoord2fv( @aChar^.TexCoor[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
       glTexCoord2fv( @aChar^.TexCoor[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
       glTexCoord2fv( @aChar^.TexCoor[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
       glTexCoord2fv( @aChar^.TexCoor[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
     glEnd;
    end;
 begin
//  if aChar^.Character=' ' then exit;
  quad[0] := aLoc;
  quad[1] := aLoc+PointF(aChar^.DecoratedCharSize.cx,0);
  quad[2] := aLoc+PointF(aChar^.DecoratedCharSize.cx,aChar^.DecoratedCharSize.cy);
  quad[3] := aLoc+PointF(0,aChar^.DecoratedCharSize.cy);

  tci := @FLIP_TEXCOORD[ 0 ];

  if aSetTextureParameters then begin
    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
    glColor4ub(255,255,255,round(255 * aLayerPercentOpacity));
  end;
  DrawVertex;
 end;

 procedure TTexturedFont.DrawText(aText: string; aLocation: TPointF;
   const aLayerPercentOpacity: single; aSetTextureParameters: boolean);
 var i: integer;
  tc: PTexturedChar;
 begin
  if aText='' then exit;
  if Length(FTexCharArray)=0 then exit;

  if aSetTextureParameters then TextureManager.BindTexture( FTexture );

  if FFont.ShadowOffsetX>=0 then begin // font shadow is on the right => put letter from left to right
    for i:=1 to UTF8Length(aText) do begin
     tc := GetTexturedChar( UTF8Copy(aText,i,1) );
     if tc <> NIL then begin
        DrawChar( tc, aLocation, aLayerPercentOpacity );
        aLocation.x+=tc^.OriginalCharSize.cx*CharSpaceCoeff.Value;
     end;
    end;
  end else begin  // font shadow is on the left => put letter from right to left
   for i:=1 to UTF8Length(aText) do begin
    tc := GetTexturedChar( UTF8Copy(aText,i,1) );
    if tc <> NIL then aLocation.x+=tc^.OriginalCharSize.cx*CharSpaceCoeff.Value;
    end;

   for i:=UTF8Length(aText) downto 1 do begin
    tc := GetTexturedChar( UTF8Copy(aText,i,1) );
    if tc <> NIL then begin
       aLocation.x-=tc^.OriginalCharSize.cx*CharSpaceCoeff.Value;
       DrawChar( tc, aLocation, aLayerPercentOpacity );
    end;
   end;
  end;
 end;

 function TTexturedFont.GetCharWidth(const c: string): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].DecoratedCharSize.cx ;
 end;

 function TTexturedFont.GetCharHeight(const c: string): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].DecoratedCharSize.cy ;
 end;

 function TTexturedFont.GetTextWidth(const t: string): integer;
 var i: integer;
 begin
  Result := 0;
  for i:=1 to UTF8Length(t) do Result += GetCharWidth( UTF8Copy( t, i, 1 ));
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
    else Result := FTexCharArray[i].DecoratedCharSize ;
 end;

 constructor TFontManager.Create;
 begin
  inherited Create;
  FList := TList.Create;
 end;

 { TFontManager }
 destructor TFontManager.Destroy;
 begin
  Clear;
  FList.Free;
  inherited Destroy;
 end;

 procedure TFontManager.Update(const aElapsedTime: single);
 var i: integer;
 begin
  for i:=0 to FList.Count-1 do
    TTexturedFont(FList.Items[i]).CharSpaceCoeff.OnElapse( aElapsedTime );
 end;

function TFontManager.GetCount: integer;
begin
 Result := FList.Count;
end;

  function TFontManager.AddFont(aGuiFont: TGuiFont; const aCharSet: string): TTexturedFont;
 begin
  Result := TTexturedFont.Create( aGuiFont, aCharSet );
  FList.Add( Result );
 end;

 procedure TFontManager.DeleteFont(var aFont: TTexturedFont);
 var i: integer;
 begin
  if aFont=NIL then exit;
  i := FList.IndexOf( Pointer(aFont) );
  if i<> -1 then FList.delete( i );
  aFont.Free;
  aFont := NIL;
 end;

 procedure TFontManager.Clear;
 begin
  while FList.Count>0 do begin
   TTexturedFont(FList.Items[0]).Free;
   FList.Delete(0);
  end;
 end;


{$endif oglcIMPLEMENTATION}

