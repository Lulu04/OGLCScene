{$ifdef oglcINTERFACE}
const
  OGLC_DEFAULT_CHAR_SET = ' '+
                          '0123456789'+
                          'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                          'ÉéÈèÊêËëÁáÂâÀàÆæÇçÙùÛûÏïÎîÔôŒœŸÿ'+
                          'ÄäÖöÜüß'+
                          'Ññ'+
                          '''"?!,.;:/*-+=()[]@#&';
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
  Constructor Create( aFont: TGuiFont; aFillTexture: TBGRABitmap; const aCharSet: string);
  Destructor Destroy; override;
 private
  FTexture: PTexture;
  FTexCharArray: array of TTexturedChar ;
  FFont: TGuiFont;
  function IndexOf( c: string ): integer;
  procedure BuildTexture( aFont: TGuiFont; aFillTexture: TBGRABitmap; const aCharSet: string );
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
  procedure DrawCenteredChar( aChar: string; aCenterLocation: TPointF; const aLayerPercentOpacity: single;
                      aSetTextureParameters: boolean=TRUE );
  // original size (equal to TBGRABitmap.TextSize())
  function GetCharWidth( const c: string ): integer;
  function GetCharHeight( const c: string ): integer;
  function GetTextWidth( const t: string ): integer;
  function GetTextHeight( const t: string ): integer;
  function GetCharSize( const c: string ): TSize;
  // size with outline and shadow
  function GetDecoratedCharWidth( const c: string ): integer;
  function GetDecoratedCharHeight( const c: string ): integer;
  function GetDecoratedTextWidth( const t: string ): integer;
  function GetDecoratedTextHeight( const t: string ): integer;
  function GetDecoratedCharSize( const c: string ): TSize;
  property Texture: PTexture read FTexture;
  property Font: TGuiFont read FFont;
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
 function AddFont( aGuiFont: TGuiFont; aFillTexture: TBGRABitmap; const aCharSet: string=OGLC_DEFAULT_CHAR_SET): TTexturedFont;
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
  procedure SetCaption(AValue: string); virtual;
  procedure SetTexturedFont(AValue: TTexturedFont); virtual;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
  procedure Draw( const aLayerPercentOpacity: single );  override;
public
  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
end;


TOGLCAlignment=(taTopLeft, taTopCenter, taTopRight,
                taCenterLeft, taCenterCenter, taCenterRight,
                taBottomLeft, taBottomCenter, taBottomRight);

{ TMultilineText }
// Draws a text in a rectangular area with horizontal and vertical align.
// The caption can be multiline: use #10 to separate the lines. Ex: 'line 1'#10'line 2'
TMultilineText=class(TFreeText)
private type
  TFreeTextWord=record
    Word: string;
    Location: TPointF;
    Width,
    LineIndex: integer;
    Visible: boolean;
  end;
  ArrayOfFreeTextWord = array of TFreeTextWord;
private
  FRect,
  FDrawingRect: TRect;
  FAlign: TOGLCAlignment;
  procedure DoVTopAlign;
  procedure DoVCenterAlign;
  procedure DoVBottomAlign;
  procedure DoHLeftAlign;
  procedure DoHCenterAlign;
  procedure DoHRightAlign;
  procedure DoAlign;
private
  FWords : ArrayOfFreeTextWord;
  FLineCount: integer;
  FDrawingWidth,
  FDrawingHeight: integer;
  procedure SetCaption(AValue: string); override;
  procedure SetTexturedFont(AValue: TTexturedFont); override;
  procedure DistributeTheWordsOnTheSurface;
  procedure SetAlign(AValue: TOGLCAlignment); virtual;
  procedure GetWordsIndexes( aLineIndex: integer; var iBegin, iEnd: integer );
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Draw( const aLayerPercentOpacity: single ); override;
public
  constructor Create( aFont: TTexturedFont; aRect: TRect );
  property Align: TOGLCAlignment read FAlign write SetAlign;
  property DrawingRect: TRect read FDrawingRect; // real size of the text area
end;

// TMaximizedText is an area where the text is displayed as large as possible and aligned.
// the font height is maximized and the corresponding textured font is then created
// Usefull for screen title for example
TMaximizedText = class( TMultilineText )
private
 FGuiFont: TGuiFont;
 FTemp: TBGRABitmap;
 FCharSet: string;
 procedure MakeMaximizedFont;
 function ComputeDrawingSize( aGuiFont: TGuiFont ): TSize;
 procedure SetCaption(AValue: string); override;
 procedure SetTexturedFont({%H-}AValue: TTexturedFont); override;
public
  // the font height field of 'aGuiFont' is maximized.
  constructor Create( aRect: TRect; aGuiFont: TGuiFont; const aCharSet: string=OGLC_DEFAULT_CHAR_SET );
  destructor destroy; override;

  function GetComputedGuiFont: TGuiFont;
  function GetComputedTexturedFont: TTexturedFont;
end;


 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}
 { TMaximizedText }

 procedure TMaximizedText.MakeMaximizedFont;
 var s: TSize;
 begin
  FGuiFont.FontHeight:=50; // arbitrary font height
  FTemp:= TBGRABitmap.Create(1,1);

  s := ComputeDrawingSize( FGuiFont );
  if ( s.cy < FRect.Height ) and ( s.cx < FRect.Width ) then begin
    // the drawing is smallest than the surface => we try to increase it
    repeat
      FGuiFont.FontHeight := FGuiFont.FontHeight+1;
      s := ComputeDrawingSize( FGuiFont );
    until ( s.cy > FRect.Height ) or ( s.cx > FRect.Width ) ;
    FGuiFont.FontHeight := FGuiFont.FontHeight-1;
  end else if (s.cy>FRect.Height) or (s.cx> FRect.Width) then begin
    // the drawing is larger than the surface => we decrease it
    repeat
      FGuiFont.FontHeight := FGuiFont.FontHeight-1;
      s := ComputeDrawingSize( FGuiFont );
    until ( s.cy <= FRect.Height ) and ( s.cx <= FRect.Width ) ;
  end;
  FTemp.Free;

  if FTexturedFont<>NIL then FontManager.DeleteFont( FTexturedFont );
  FTexturedFont := FontManager.AddFont( FGuiFont, FCharSet );
 end;

 function TMaximizedText.ComputeDrawingSize(aGuiFont: TGuiFont): TSize;
 var W: ArrayOfString;
     i, SpaceWidth, WordWidth, xx, yy: Integer;
 procedure NextLine;
 begin
  yy+=FTemp.FontHeight;
  xx:=0;
  inc(FLineCount);
  if i<>Length(W)-1 then Result.cy := Result.cy+FTemp.FontHeight;
 end;
 procedure PutWordOnCurrentLine;
 begin
  xx+=WordWidth+SpaceWidth;
 end;
 function CurrentLineIsEmpty: boolean;
 begin
  Result := xx=0;
 end;
 begin
   FTemp.FontHeight:=aGuiFont.FontHeight;
   FTemp.FontStyle:=aGuiFont.Style;
   FTemp.FontName:=aGuiFont.FontName;

   Result.cx := 0;
   Result.cy := 0;
   if FCaption ='' then exit;

   W := SplitTextToWordArray(FCaption, ' ');

   SpaceWidth := FTemp.TextSize(' ').cx;

   xx := 0;
   yy := 0;
   Result.cy := FTemp.FontHeight;
   i := 0;
   repeat
    WordWidth := FTemp.TextSize(W[i]).cx;

    if W[i]=#10 then begin
        NextLine;
        inc(i);
    end else if xx+WordWidth <= FRect.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(i);
    end else begin
         // the word don't fit
         if CurrentLineIsEmpty then begin
           PutWordOnCurrentLine;
           NextLine;
           inc(i);
         end else begin
              NextLine;
              PutWordOnCurrentLine;
              inc(i);
         end;
    end;
    if xx>Result.cx then Result.cx:=xx;
   until i=Length(W);
 end;

 procedure TMaximizedText.SetCaption(AValue: string);
 begin
  if FCaption=AValue then exit;
  FCaption := AValue;
  MakeMaximizedFont;
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

 procedure TMaximizedText.SetTexturedFont(AValue: TTexturedFont);
 begin
 // does nothing because it's no sense to manually change the font height in TMaximizedtext...
 end;

 constructor TMaximizedText.Create(aRect: TRect; aGuiFont: TGuiFont; const aCharSet: string);
 begin
  inherited Create( NIL, aRect );
  FGuiFont := aGuiFont;
  FCharSet:=aCharSet;
 end;

 destructor TMaximizedText.destroy;
begin
  if FTexturedFont<>NIl then FontManager.DeleteFont( FTexturedFont );
  inherited destroy;
end;

 function TMaximizedText.GetComputedGuiFont: TGuiFont;
 begin
  Result :=FGuiFont;
 end;

 function TMaximizedText.GetComputedTexturedFont: TTexturedFont;
 begin
  Result:=FTexturedFont;
 end;

 { TMultilineText }

 procedure TMultilineText.DistributeTheWordsOnTheSurface;
 var W: ArrayOfString;
   i, SpaceWidth, xx, yy: Integer;
   procedure NextLine;
   begin
    yy+=FTexturedFont.Font.FontHeight;
    xx:=0;
    inc(FLineCount);
   end;
   procedure PutWordOnCurrentLine;
   begin
    FWords[i].Location:=PointF(xx,yy);
    FWords[i].LineIndex:=FLineCount-1;
    xx+=FWords[i].Width+SpaceWidth;
    // the word is not visible if we are vertically out of bounds
    FWords[i].Visible := FWords[i].Location.y<FRect.Height;
   end;
   function CurrentLineIsEmpty: boolean;
   begin
    Result := xx=0;
   end;

 begin
   FLineCount := 0;
   FDrawingWidth := 0;
   FDrawingRect.Width:=0;
   FDrawingRect.Height:=0;
   if (FCaption ='') or (FTexturedFont=NIL) then begin
     SetLength(FWords,0);
     exit;
   end;

   W := SplitTextToWordArray(FCaption, ' ');
   SetLength(FWords, Length(W));

   SpaceWidth := FTexturedFont.GetCharWidth(' ');

   xx := 0;
   yy := 0;
   FLineCount := 1;
   i := 0;
   repeat
    FWords[i].Word := W[i];
    FWords[i].Width := FTexturedFont.GetTextWidth(FWords[i].Word);

    if FWords[i].Word=#10 then begin
        NextLine;
        inc(i);
    end else if xx+FWords[i].Width <= FRect.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(i);
    end else begin
         // the word don't fit
         if CurrentLineIsEmpty then begin
           PutWordOnCurrentLine;
           NextLine;
           inc(i);
         end else begin
              NextLine;
              PutWordOnCurrentLine;
              inc(i);
         end;
    end;
    if xx>FDrawingWidth then FDrawingWidth:=xx;
   until i=Length(FWords);
   FDrawingHeight := FTexturedFont.Font.FontHeight*FLineCount;
 end;

 procedure TMultilineText.SetAlign(AValue: TOGLCAlignment);
 begin
   if FAlign=AValue then Exit;
   FAlign:=AValue;
   DoAlign;
 end;

 procedure TMultilineText.SetCaption(AValue: string);
 begin
   if FCaption=AValue then exit;
   FCaption := AValue;
   DistributeTheWordsOnTheSurface;
   DoAlign;
 end;

 procedure TMultilineText.SetTexturedFont(AValue: TTexturedFont);
 begin
   if FTexturedFont=AValue then exit;
   FTexturedFont := AValue;
   DistributeTheWordsOnTheSurface;
   DoAlign;
 end;

 procedure TMultilineText.GetWordsIndexes(aLineIndex: integer; var iBegin,
   iEnd: integer);
 var i : integer;
 begin
  iBegin := -1;
  iEnd := -1;
  for i:=low(FWords) to high(FWords) do
   begin
    if FWords[i].LineIndex = aLineIndex then begin
         if iBegin = -1 then iBegin := i;
         iEnd := i;
    end else if iBegin <> -1 then exit;
   end;
 end;

 function TMultilineText.GetWidth: integer;
begin
  Result:=FRect.Width;
end;

 function TMultilineText.GetHeight: integer;
begin
  Result:=FRect.Height;
end;

 procedure TMultilineText.Draw(const aLayerPercentOpacity: single);
 var AlphaTint: byte;
  i: integer;
 begin
  if FCaption='' then exit;
  if FTexturedFont=NIL then exit;

  PushAndPrepareMatrix;
  SetBlendMode( FBlendMode );

//  TextureManager.DisableTextureUsage;
//  FillBox(0,0,Width, Height, BGRA(0,0,80,100));

  TextureManager.BindTexture( FTexturedFont.Texture );

  for i:=0 to Length(FWords)-1 do begin
    if FWords[i].Visible then begin
      if Tint.alpha.Value < 255 then begin
        glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
        glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
        FTexturedFont.DrawText( FWords[i].Word, FWords[i].Location, Opacity.Value/255*aLayerPercentOpacity, FALSE );
      end;

      if Tint.alpha.Value <> 0 then begin  //Apply Tint
        glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
        glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
        glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
        AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
        glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
        FTexturedFont.DrawText( FWords[i].Word, FWords[i].Location, Opacity.Value/255*aLayerPercentOpacity, FALSE );
      end;
    end;
  end;

  DrawChildSurfaces( aLayerPercentOpacity*Opacity.pcValue );
  PopMatrix;
 end;

 constructor TMultilineText.Create(aFont: TTexturedFont; aRect: TRect);
 begin
  inherited Create;
  FTexturedFont := aFont;
  FRect := aRect;
  FAlign := taCenterCenter;
 end;

 procedure TMultilineText.DoVTopAlign;
 var i, j, iBegin, iEnd: integer;
  yy: single;
 begin
  FDrawingRect.Top:=0;
  yy := 0;
  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes( i, iBegin{%H-}, iEnd{%H-} );
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy += FTexturedFont.Font.FontHeight;
  end;
 end;

 procedure TMultilineText.DoVCenterAlign;
 var i, j, iBegin, iEnd, h: integer;
  yy: single;
 begin
  h:=FLineCount*FTexturedFont.Font.FontHeight;
  if h=0 then exit;
  yy := ( FRect.Height - h )*0.5;
  FDrawingRect.Top:=round(yy);
  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes( i, iBegin{%H-}, iEnd{%H-} );
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy += FTexturedFont.Font.FontHeight;
  end;
 end;

 procedure TMultilineText.DoVBottomAlign;
 var i, j, iBegin, iEnd: integer;
  yy: single;
 begin
  yy := FRect.Height-FTexturedFont.Font.FontHeight;
  FDrawingRect.Top:=FRect.Height-FDrawingHeight;
  for i:=FLineCount-1 downto 0 do begin
    GetWordsIndexes( i, iBegin{%H-}, iEnd{%H-} );
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy -= FTexturedFont.Font.FontHeight;
  end;
 end;

 procedure TMultilineText.DoHLeftAlign;
 var i, j, w, iBegin, iEnd: integer;
 begin
  FDrawingRect.Left:=0;
  for i:=0 to FLineCount-1 do
   begin
    GetWordsIndexes( i, iBegin{%H-}, iEnd{%H-} );
    w := 0;
    for j:=iBegin to iEnd do
     begin
      FWords[j].Location.x := w;
      w += FWords[j].Width + FTexturedFont.GetCharWidth(' ');
     end;
   end;
 end;

 procedure TMultilineText.DoHCenterAlign;
 var i, j, iBegin, iEnd: integer;
  w: single;
 begin
  FDrawingRect.Left:=FRect.Width;
  for i:=0 to FLineCount-1 do
   begin
    GetWordsIndexes ( i, iBegin{%H-}, iEnd{%H-} );
    w := FWords[iEnd].Location.x + FWords[iEnd].Width - FWords[iBegin].Location.x; // total width of the line
    w := ( FRect.Width - w )*0.5;

    if FDrawingRect.Left>w then FDrawingRect.Left:=round(w);

    for j:=iBegin to iEnd do begin
      FWords[j].Location.x := w ;
      w := w + FWords[j].Width + FTexturedFont.GetCharWidth(' ');
    end;
   end;
 end;

 procedure TMultilineText.DoHRightAlign;
 var i, j, w, iBegin, iEnd: integer;
 begin
  FDrawingRect.Left:=FRect.Width;

  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes ( i, iBegin{%H-}, iEnd{%H-} );
    w := FRect.Width;
    for j:=iEnd downto iBegin do
     begin
      w -= FWords[j].Width;
      FWords[j].Location.x := w;
      if FDrawingRect.Left>w then FDrawingRect.Left:=w;
      w-=FTexturedFont.GetCharWidth(' ');
     end;
  end;

 end;

 procedure TMultilineText.DoAlign;
 begin
  case FAlign of
   taTopLeft, taCenterLeft, taBottomLeft: DoHLeftAlign;
   taTopRight, taCenterRight, taBottomRight: DoHRightAlign;
   taTopCenter, taCenterCenter, taBottomCenter: DoHCenterAlign;
  end;
  case FAlign of
   taTopLeft, taTopCenter, taTopRight: DoVTopAlign;
   taCenterLeft, taCenterCenter, taCenterRight: DoVCenterAlign;
   taBottomLeft, taBottomCenter, taBottomRight: DoVBottomAlign;
  end;
  FDrawingRect.Bottom:=FDrawingRect.Top+FDrawingHeight;
  FDrawingRect.Right:=FDrawingRect.Left+FDrawingWidth;
 end;


 { TFreeText }

 function TFreeText.GetCaption: string;
 begin
  Result := FCaption;
 end;

 procedure TFreeText.SetCaption(AValue: string);
 begin
  FCaption := AValue;
 end;

procedure TFreeText.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont=AValue then Exit;
  FTexturedFont:=AValue;
end;

 function TFreeText.GetWidth: integer;
 begin
  Result := FTexturedFont.GetTextWidth( FCaption );
 end;

 function TFreeText.GetHeight: integer;
 begin
  Result := FTexturedFont.GetTextHeight( FCaption );
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

  constructor TTexturedFont.Create(aFont: TGuiFont; aFillTexture: TBGRABitmap; const aCharSet: string);
 begin
  SetLength( FTexCharArray, 0 );
  BuildTexture( aFont, aFillTexture, aCharSet );
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


 procedure TTexturedFont.BuildTexture(aFont: TGuiFont; aFillTexture: TBGRABitmap; const aCharSet: string);
 var i, j, xx, yy, maxCharHeight : integer;
     ima, temp: TBGRABitmap;
     s1 : string;
     renderer: TBGRATextEffectFontRenderer;

 begin
  FFont := aFont;
  if FFont.FontHeight>150 then FFont.FontHeight:=150; // max FontHeight is 150

  renderer:= TBGRATextEffectFontRenderer.Create;
  with FFont do begin
    renderer.ShadowVisible := (ShadowColor<>BGRAPixelTransparent) and (ShadowRadius>0);
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
    temp := TextToBitmap( s1, FFont, aFillTexture );

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

 procedure TTexturedFont.DrawCenteredChar(aChar: string;
   aCenterLocation: TPointF; const aLayerPercentOpacity: single;
   aSetTextureParameters: boolean);
 var tc: PTexturedChar;
 begin
  if aChar='' then exit;
  if aSetTextureParameters then TextureManager.BindTexture( FTexture );

  tc := GetTexturedChar( aChar );
  if tc <> NIL then
    DrawChar( tc, aCenterLocation-PointF(tc^.OriginalCharSize.cx*0.5,tc^.OriginalCharSize.cy*0.5 ), aLayerPercentOpacity );
 end;

 function TTexturedFont.GetCharWidth(const c: string): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].OriginalCharSize.cx ;
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
    else Result := FTexCharArray[i].OriginalCharSize;
 end;

 function TTexturedFont.GetDecoratedCharWidth(const c: string): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i].DecoratedCharSize.cx ;
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
  Result := TTexturedFont.Create( aGuiFont, NIL, aCharSet );
  FList.Add( Result );
 end;

  function TFontManager.AddFont(aGuiFont: TGuiFont; aFillTexture: TBGRABitmap;
    const aCharSet: string): TTexturedFont;
  begin
   Result := TTexturedFont.Create( aGuiFont, aFillTexture, aCharSet );
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

