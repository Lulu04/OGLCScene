{$ifdef oglcINTERFACE}
type
{ TTexturedFont }

TTexturedFont = class
  Constructor Create;
  Destructor Destroy ; override ;
 private
  FTexCharArray : array of PTexture ;
  FCharList : ansistring ;
  FFontName       : string ;
  FFontHeight     : integer ;
  FStyle          : TFontStyles ;
  FFontColor      : TBGRAPixel ;
  FOutLineColor   : TBGRAPixel ;
  FOutLineWidth   : single ;
  FShadowColor    : TBGRAPixel ;
  FShadowOffsetX  : integer ;
  FShadowOffsetY  : integer ;
  FShadowRadius   : integer ;
  function IndexOf( c : char ): integer;
 public
  procedure SetCharList( s:ansistring ) ;
  procedure Build(FontName: string; FontHeight: integer;
                  Style: TFontStyles; FontColor: TBGRAPixel; OutLineColor: TBGRAPixel;
                  OutLineWidth: single; ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                  FontQuality: TBGRAFontQuality=fqFineAntialiasing );
  function GetCharTexture( c : char ) : PTexture ;
  function GetCharWidth( c : char ): integer ;
  function GetCharHeight( c : char ): integer ;
  function Text2Bitmap ( aCaption: UTF8String; aBackGround: TBGRABitmap=NIL ) : TBGRABitmap ;
end;


{ TFontManager }

TFontManager = class
 Constructor Create;
 Destructor Destroy ; override ;
private
 FList : TList ;
 FCharList : ansistring ;
public
 function AddFont( FontName:string; FontHeight:integer; Style:TFontStyles; FontColor : TBGRAPixel;
                   OutLineColor: TBGRAPixel; OutLineWidth:single;
                   ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                   FontQuality: TBGRAFontQuality=fqFineAntialiasing): TTexturedFont ;
 procedure DeleteFont( var aFont: TTexturedFont );
 procedure Clear ;

 property CharList : ansistring read FCharList write FCharList ;
end;

var
 FontManager : TFontManager ;

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}
 { TTexturedFont }

 constructor TTexturedFont.Create;
 begin
  SetLength( FTexCharArray, 0 );
 end;

 destructor TTexturedFont.Destroy;
 var i:integer;
 begin
  for i:=0 to Length( FTexCharArray )-1 do
   if FTexCharArray[i] <> NIL
      then TextureManager.Delete( FTexCharArray[i] );
  SetLength( FTexCharArray, 0 );
  inherited Destroy;
 end;

 function TTexturedFont.IndexOf(c: char): integer;
 var i : integer ;
 begin
  Result := -1 ;
  for i:=1 to Length( FCharList ) do
   if c = FCharList[i]
      then begin
            Result := i-1 ;
            exit ;
           end;
 end;

 function TTexturedFont.GetCharTexture(c: char): PTexture;
 var i: integer ;
 begin
  Result := NIL ;
  i := IndexOf( c );
  if i=-1
     then Result := NIL
     else Result := PTexture( FTexCharArray[IndexOf( c )] ) ;
 end;

 procedure TTexturedFont.SetCharList(s: ansistring);
 var i:integer;
 begin
  FCharList := s ;
  SetLength( FTexCharArray, UTF8Length( s ) );
  for i:=0 to Length( FTexCharArray )-1 do
    FTexCharArray[i] := NIL ;
 end;

 procedure TTexturedFont.Build(FontName: string; FontHeight: integer;
   Style: TFontStyles; FontColor: TBGRAPixel; OutLineColor: TBGRAPixel;
   OutLineWidth: single; ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
   FontQuality: TBGRAFontQuality);
 var i : integer;
     ima: TBGRABitmap;
     s1 : string ;
     renderer: TBGRATextEffectFontRenderer;
 begin
  FFontName := FontName ;
  FFontHeight := FontHeight ;
  FStyle := Style ;
  FFontColor := FontColor ;
  FOutLineColor := OutLineColor ;
  FOutLineWidth := OutLineWidth ;
  FShadowColor := ShadowColor ;
  FShadowOffsetX := ShadowOffsetX ;
  FShadowOffsetY := ShadowOffsetY ;
  FShadowRadius := ShadowRadius ;
 //
  renderer:= TBGRATextEffectFontRenderer.Create;
  renderer.ShadowVisible := ShadowColor<>BGRAPixelTransparent ;
  renderer.ShadowColor := ShadowColor;
  renderer.ShadowOffset.x := abs( ShadowOffsetX ) ;
  renderer.ShadowOffset.y := abs( ShadowOffsetY ) ;
  renderer.ShadowRadius := ShadowRadius ;
  renderer.OutlineVisible := (OutLineColor<>BGRAPixelTransparent) and (OutLineWidth>0) ;
  renderer.OutlineColor := OutLineColor;
  renderer.OuterOutlineOnly := True;
  renderer.OutlineWidth := OutLineWidth ;
  ima:= TBGRABitmap.Create(1,1);
  ima.FontRenderer := renderer ;
  ima.FontQuality:= FontQuality;
  ima.FontName := FontName ;
  ima.FontHeight := FontHeight ;
  ima.FontStyle := Style ;
  for i:=0 to Length( FTexCharArray )-1 do
   begin
    s1 := SysToUTF8( FCharList[i+1] );
    ima.SetSize( ima.TextSize( s1 ).cx + round(OutLineWidth)+abs( ShadowOffsetX )+ShadowRadius, ima.TextSize( s1 ).cy + round(OutLineWidth)+abs( ShadowOffsetY )+ShadowRadius );
    ima.Fill( BGRAPixelTransparent ) ;
    ima.TextOut(OutLineWidth/2, OutLineWidth/2, s1, FontColor ) ;
    FTexCharArray[i] := TextureManager.Add( ima ) ;
   end;
  ima.Free ;
 end;

 function TTexturedFont.GetCharWidth(c: char): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i]^.ImageWidth ;
 end;

 function TTexturedFont.GetCharHeight(c: char): integer;
 var i: integer;
 begin
  i := IndexOf( c );
  if i=-1
    then Result := 0
    else Result := FTexCharArray[i]^.ImageHeight ;
 end;

 function TTexturedFont.Text2Bitmap(aCaption: UTF8String; aBackGround: TBGRABitmap): TBGRABitmap;
 var w, r:single;
     renderer: TBGRATextEffectFontRenderer;
 begin
  renderer:= TBGRATextEffectFontRenderer.Create;
  renderer.ShadowVisible := FShadowColor<>BGRAPixelTransparent ;
  renderer.ShadowColor := FShadowColor;
  renderer.ShadowOffset.x := abs( FShadowOffsetX ) ;
  renderer.ShadowOffset.y := abs( FShadowOffsetY ) ;
  renderer.ShadowRadius := FShadowRadius ;
  renderer.OutlineVisible := (FOutLineColor<>BGRAPixelTransparent) and (FOutLineWidth>0) ;
  renderer.OutlineColor := FOutLineColor;
  renderer.OuterOutlineOnly := True;
  renderer.OutlineWidth := FOutLineWidth;
  Result := TBGRABitmap.Create( 1, 1 );
  Result.FontRenderer := renderer ;
  Result.FontQuality:= fqFineAntialiasing;
  Result.FontName := FFontName ;
  Result.FontHeight := FFontHeight;
  Result.FontStyle := FStyle;
  if aBackGround = NIL
     then begin
           Result.SetSize( Result.TextSize(aCaption).cx + round(FOutLineWidth)+abs( FShadowOffsetX )+FShadowRadius, Result.TextSize(aCaption).cy + round(FOutLineWidth)+abs( FShadowOffsetY )+FShadowRadius );
           Result.Fill( BGRAPixelTransparent ) ;
           Result.TextOut( FOutLineWidth/2, FOutLineWidth/2, aCaption, FFontColor ) ;
          end
     else begin
           Result.SetSize( Result.TextSize(aCaption+'  ').cx, Result.TextSize(aCaption+'  ').cy );
           if Result.Width > Result.Height
             then begin
                   w := Result.Height * 0.03 ;
                   r := Result.Height * 0.3 ;
                  end
             else begin
                   w := Result.Width * 0.03 ;
                   r := Result.Width * 0.3 ;
                  end;
           if w < 0.5 then w := 0.5 ;
           Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, nil, 0, aBackGround ) ;
           Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, BGRA(255,255,255), w ) ;
           Result.TextOut(Result.TextSize(' ').cx + FOutLineWidth/2, FOutLineWidth/2, aCaption, FFontColor ) ;
          end;
 end;




 { TFontManager }
 constructor TFontManager.Create;
 begin
  FList := TList.Create ;
  FCharList :='0123456789 ?!,.;:/*-+=()abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZéèàçùï[]@' ;
 end;

 destructor TFontManager.Destroy;
 begin
  Clear ;
  FList.Free ;
  inherited Destroy;
 end;

 function TFontManager.AddFont(FontName: string; FontHeight: integer;
                               Style: TFontStyles; FontColor: TBGRAPixel;
                               OutLineColor: TBGRAPixel; OutLineWidth: single;
                               ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                               FontQuality: TBGRAFontQuality): TTexturedFont;
 begin
  Result := TTexturedFont.Create;
  Result.SetCharList( FCharList );
  Result.Build( FontName, FontHeight, Style, FontColor, OutLineColor, OutLineWidth, ShadowColor, ShadowOffsetX, ShadowOffsetY, ShadowRadius, FontQuality );
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
  var i: integer;
 begin
  for i:=0 to FList.Count-1 do
   TTexturedFont( FList.Items[i] ).Free ;
  FList.Clear;
 end;


{$endif oglcIMPLEMENTATION}

