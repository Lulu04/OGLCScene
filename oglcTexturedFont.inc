{$ifdef oglcINTERFACE}
const
  ASCII_SYMBOL_CHARSET='!"#$%&''()*+,-./:;<=>?@[\]^_`{|}~';
  SIMPLELATIN_CHARSET=' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

  LATIN1_SUPP_CHARSET='ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ';
  LATIN1_SUPP_SYMBOL_CHARSET=' ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿';

  LATIN_EXTENDED_A_CHARSET='ĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞş'+
                           'ŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſ';

  LATIN_EXTENDED_B_CHARSET='ƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ'+
                           'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿ'+
                           'ɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏ';

  GREEK_AND_COPTIC_CHARSET='ͰͱͲͳʹ͵Ͷͷ'#$CD#$B8#$CD#$B9'ͺͻͼͽ;Ϳ΄΅Ά·ΈΉΊΌΎΏΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώϏϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿ';

  CYRILLIC_CHARSET='ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюяѐёђѓєѕіїјљњћќѝўџѠѡѢѣѤѥѦѧѨѩѪѫѬѭѮѯѰѱѲѳѴѵѶѷѸѹѺѻѼѽѾѿҀҁ҂҃҄҅҆҇҈҉ҊҋҌҍҎҏҐґҒғҔҕҖҗҘҙҚқҜҝҞҟҠҡҢңҤҥҦҧҨҩҪҫҬҭҮүҰұҲҳҴҵҶҷҸҹҺһҼҽҾҿӀӁӂӃӄӅӆӇӈӉӊӋӌӍӎӏӐӑӒӓӔӕӖӗӘәӚӛӜӝӞӟӠӡӢӣӤӥӦӧӨөӪӫӬӭӮӯӰӱӲӳӴӵӶӷӸӹӺӻӼӽӾӿ'+
                   'ԀԁԂԃԄԅԆԇԈԉԊԋԌԍԎԏԐԑԒԓԔԕԖԗԘԙԚԛԜԝԞԟԠԡԢԣԤԥԦԧԨԩԪԫԬԭԮԯ';

  ARMENIAN_CHARSET=#$D4#$B0'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕՖ'#$D5#$97#$D5#$98'ՙ՚՛՜՝՞՟'#$D5#$A0'աբգդեզէըթժիլխծկհձղճմյնշոչպջռսվտրցւփքօֆև'#$D6#$88'։֊'#$D6#$8B#$D6#$8C'֍֎֏';

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
  procedure InternalBuild(aPacker: TOGLCImagePacker; const aCharSet: string; aFillTexture: TBGRABitmap);
 public
  // This property affect the space between character
  //   value from 0.1 to 1 -> the characters are closest
  //   value equal to 1 -> normal font spacing
  //   value > 1 -> the characters are farther apart from each other
  CharSpacingCoeff: TBoundedFParam;
  Constructor Create(aParentScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap);
  constructor CreateFromImagePacker(aParentScene: TOGLCScene; aPacker: TOGLCImagePacker; aFont: TFontDescriptor;
                                    const aCharSet: string; aFillTexture: TBGRABitmap=NIL);
  Destructor Destroy; override;

  function StringToCharIndexes(const aCaption: string): TTexturedCharIndexes;
  function CharIndexesToSize(const aIndexes: TTexturedCharIndexes): TSize;
  procedure DrawChar(aCharIdx: integer; aX, aY, aFlipIndex: integer;
                     const aClippingArea: PUIClipArea; const aModelViewMatrix: POGLCMatrix44;
                     const aOpacity: single; const aComputedTint: TColorF; aBlendMode: byte);
  procedure DrawChars(const aCharIdx: TTexturedCharIndexes; aX, aY, aFlipIndex: integer;
                      const aClippingArea: PUIClipArea; const aModelViewMatrix: POGLCMatrix44;
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



{ TFreeText }
// A surface that draw a single line string.
// Instead of TUILabel, TFreeText is not clipped and mouse can't interact with it.

TFreeText = class(TSimpleSurfaceWithEffect)
private
  FCharIndexes: TTexturedCharIndexes;
  FCaption: string;
  FTexturedFont: TTexturedFont;
  FSize: TSize;
  FSizeComputationNeeded: boolean;
  procedure UpdateCharIndexesAndComputeSize;
  function GetCaption: string;
  procedure SetCaption(AValue: string); virtual;
  procedure SetTexturedFont(AValue: TTexturedFont); virtual;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene);
  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
end;


{ TFreeTextOnPathToFollow }

TFreeTextOnPathToFollow = class(TFreeText)
private
  FAutoRotate: boolean;
  FAngleAdjust: single;
public
  procedure Update(const aElapsedTime: single); override;
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
public
  // Distance traveled on the path
  DistanceTraveled: TFParam;

  constructor CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow);
  destructor Destroy; override;

  // 'TRUE' force the sprite to rotate on the right direction along the path. Default is TRUE.
  property AutoRotate: boolean read FAutoRotate write FAutoRotate;
  // Set this value to ajust the right orientation of the sprite. Default is '0'.
  property AngleAdjust: single read FAngleAdjust write FAngleAdjust;
end;


{ TFreeTextClockLabel }

TFreeTextClockLabel = class(TFreeText)
private
  FOnCountdownDone: TNotifyEvent;
  FTime: single;
  FPaused,
  FCountdown,
  FShowFractionalPart: boolean;
  procedure SetShowFractionalPart(AValue: boolean);
  procedure SetTime(AValue: single);
  procedure UpdateCaption;
public
  // the TexturedFont used by the inherited TFreeText must have the characters 0123456789:. included
  // Set aPaused to TRUE to pause the count/countdown at the begining
  constructor Create(aParentScene: TOGLCScene; aPaused: boolean);
  procedure Update(const aElapsedTime: single); override;
  // pause the clock
  procedure Pause;
  // run the clock
  procedure Run;
  // gives/sets the current value to count in seconds
  property Time: single read FTime write SetTime;
  // set to TRUE if you want to time countdown. Default is FALSE (counting time)
  property Countdown: boolean read FCountdown write FCountdown;
  // set to TRUE force the tenths and hundredths of seconds to be drawned. default is TRUE
  property ShowFractionalPart: boolean read FShowFractionalPart write SetShowFractionalPart;
  // event callback for countdown mode. Fired when timer reach 0.
  property OnCountdownDone: TNotifyEvent read FOnCountdownDone write FOnCountdownDone;
end;


{ TMultilineText }
// Draws a text in a rectangular area with horizontal and vertical align.
// The caption can be multiline: use #10 to separate the lines. Ex: 'line 1'#10'line 2'
TMultilineText = class(TFreeText)   // TSimpleSurfaceWithEffect
private type
  TFreeTextWord = record
    Word: string;
    CharIndexes: TTexturedCharIndexes;
    Location: TPoint;
    Width,
    LineIndex: integer;
  end;
  ArrayOfFreeTextWord = array of TFreeTextWord;
private
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
  FRect: TRect;
  FWords: ArrayOfFreeTextWord;
  FLineCount: integer;
  FDrawingWidth,
  FDrawingHeight: integer;
  procedure SetCaption(AValue: string); override;
  procedure SetTexturedFont(AValue: TTexturedFont); override;
  procedure DistributeTheWordsOnTheSurface;
  procedure SetAlign(AValue: TOGLCAlignment); virtual;
  procedure GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer);
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
  // Redefine the size of the area where the text is displayed.
  procedure SetSize(aWidth, aHeight: integer);
  property Align: TOGLCAlignment read FAlign write SetAlign;
  property DrawingRect: TRect read FDrawingRect; // real size of the text area
end;

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}

 { TMultilineText }

procedure TMultilineText.DistributeTheWordsOnTheSurface;
var W: TStringArray;
   iw, j, spaceWidth, xx, yy: Integer;
   procedure _NextLine;
   begin
    yy += FTexturedFont.Font.FontHeight;
    xx := 0;
    inc(FLineCount);
   end;
   procedure PutWordOnCurrentLine;
   begin
    FWords[j].Location := Point(xx, yy);
    FWords[j].LineIndex := FLineCount-1;
    xx += FWords[j].Width+spaceWidth;
    inc(j);
   end;
   procedure DoWordWrapOnCurrentLine;
   var ch: PTexturedChar;
      k, ww: integer;
   begin
     SetLength(FWords, Length(FWords)+1);
     ww := 0;
     for k:=0 to High(FWords[j].CharIndexes) do begin
       ch := FTexturedFont.GetCharByIndex(FWords[j].CharIndexes[k]);
       if ww + ch^.OriginalCharSize.cx > FRect.Width then break;
       ww := ww + ch^.OriginalCharSize.cx;
     end;
     FWords[j+1].Width := FWords[j].Width - ww;
     FWords[j+1].CharIndexes := Copy(FWords[j].CharIndexes, k, Length(FWords[j].CharIndexes)-k);
     FWords[j+1].LineIndex := FLineCount;
     FWords[j].Width := ww;
     FWords[j].Location := Point(xx, yy);
     FWords[j].LineIndex := FLineCount-1;
     xx := FWords[j+1].Width+ spaceWidth;
     FWords[j+1].Location := Point(0, yy);
     yy += FTexturedFont.Font.FontHeight;
     inc(FLineCount);
     inc(j);
   end;

   function CurrentLineIsEmpty: boolean;
   begin
    Result := xx = 0;
   end;

begin
   FLineCount := 0;
   FDrawingWidth := 0;
   FDrawingRect.Width := 0;
   FDrawingRect.Height := 0;
   FWords := NIL;
   if (FCaption = '') or (FTexturedFont = NIL) then exit;

   W := SplitTextToWordArray(FCaption, ' ');
   SetLength(FWords, Length(W));

   spaceWidth := FTexturedFont.GetCharWidth(' ');

   xx := 0;
   yy := 0;
   FLineCount := 1;
   iw := 0;
   j := 0;
   repeat
    FWords[j].Word := W[iw];
    FWords[j].CharIndexes := FTexturedFont.StringToCharIndexes(W[iw]);
    FWords[j].Width := FTexturedFont.GetTextWidth(FWords[j].Word);

    if FWords[j].Word = #10 then begin
        _NextLine;
        inc(iw);
    end else if xx+FWords[j].Width <= FRect.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(iw);
    end else begin
         // the word don't fit
         if CurrentLineIsEmpty then begin
           DoWordWrapOnCurrentLine;
           //_NextLine;
           inc(iw);
         end else begin
              _NextLine;
              DoWordWrapOnCurrentLine;
              inc(iw);
         end;
    end;
    if xx > FDrawingWidth then FDrawingWidth := xx;
   until iw = Length(FWords);

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
  AValue := StringReplace(AValue, #13#10, #10, [rfReplaceAll{, rfIgnoreCase}]);
  AValue := StringReplace(AValue, #13, #10, [rfReplaceAll{, rfIgnoreCase}]);
  if FCaption = AValue then exit;
  FCaption := AValue;
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

procedure TMultilineText.SetTexturedFont(AValue: TTexturedFont);
begin
   if FTexturedFont = AValue then exit;
   FTexturedFont := AValue;
   DistributeTheWordsOnTheSurface;
   DoAlign;
end;

procedure TMultilineText.GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer);
var i: integer;
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
  Result := FRect.Width;
end;

function TMultilineText.GetHeight: integer;
begin
  Result := FRect.Height;
end;

procedure TMultilineText.DoDraw;
var i: integer;
begin
  for i:=0 to Length(FWords)-1 do
    if FWords[i].Location.y < FRect.Height then
      FTexturedFont.DrawChars(FWords[i].CharIndexes, FWords[i].Location.x, FWords[i].Location.y, 0, NIL, NIL,
                               FComputedOpacity, FComputedTint, FBlendMode);
end;

constructor TMultilineText.Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
begin
  inherited Create(aParentScene);
  FTexturedFont := aFont;
  FAlign := taCenterCenter;
  FRect.Create(Point(0,0), aWidth, aHeight);
end;

procedure TMultilineText.SetSize(aWidth, aHeight: integer);
begin
  FRect.Create(Point(0,0), aWidth, aHeight);
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

procedure TMultilineText.DoVTopAlign;
var i, j, iBegin, iEnd: integer;
  yy: integer;
begin
  FDrawingRect.Top:=0;
  yy := 0;
  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes( i, iBegin, iEnd );
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy += FTexturedFont.Font.FontHeight; //FTexturedFont.Font.FontHeight*Scale.y.Value
  end;
end;

procedure TMultilineText.DoVCenterAlign;
var i, j, iBegin, iEnd, h: integer;
  yy: integer;
begin
  h := FLineCount*FTexturedFont.Font.FontHeight;
  if h = 0 then exit;
  yy := (FRect.Height - h) div 2;
  FDrawingRect.Top := round(yy);
  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes(i, iBegin, iEnd);
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy += FTexturedFont.Font.FontHeight;
  end;
end;

procedure TMultilineText.DoVBottomAlign;
var i, j, iBegin, iEnd: integer;
  yy: integer;
begin
  yy := FRect.Height-FTexturedFont.Font.FontHeight; //FRect.Height-FTexturedFont.Font.FontHeight*Scale.y.Value
  FDrawingRect.Top:=FRect.Height-FDrawingHeight;
  for i:=FLineCount-1 downto 0 do begin
    GetWordsIndexes( i, iBegin, iEnd );
    for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy -= FTexturedFont.Font.FontHeight;  //FTexturedFont.Font.FontHeight*Scale.y.Value
  end;
end;

procedure TMultilineText.DoHLeftAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  FDrawingRect.Left:=0;
  for i:=0 to FLineCount-1 do
   begin
    GetWordsIndexes( i, iBegin, iEnd );
    w := 0;
    for j:=iBegin to iEnd do
     begin
      FWords[j].Location.x := w;
      w += FWords[j].Width + FTexturedFont.GetCharWidth(' '); //*Scale.x.Value
     end;
   end;
end;

procedure TMultilineText.DoHCenterAlign;
var i, j, iBegin, iEnd: integer;
  w: integer;
begin
  FDrawingRect.Left:=FRect.Width;
  for i:=0 to FLineCount-1 do
   begin
    GetWordsIndexes ( i, iBegin, iEnd );
    w := 0;
    for j:=iBegin to iEnd do begin
     w+= FWords[j].Width;
     if i<>iEnd then w+=FTexturedFont.GetCharWidth(' ');
    end;

//    w := FWords[iEnd].Location.x + FWords[iEnd].Width - FWords[iBegin].Location.x; // total width of the line
    w := ( FRect.Width - w ) div 2;

    if FDrawingRect.Left>w then FDrawingRect.Left:=round(w);

    for j:=iBegin to iEnd do begin
      FWords[j].Location.x := w ;
      w += {(}FWords[j].Width + FTexturedFont.GetCharWidth(' '){)*Scale.x.Value};
    end;
   end;
end;

procedure TMultilineText.DoHRightAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  FDrawingRect.Left:=FRect.Width;

  for i:=0 to FLineCount-1 do begin
    GetWordsIndexes ( i, iBegin, iEnd );
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

procedure TFreeText.UpdateCharIndexesAndComputeSize;
begin
  if (FTexturedFont = NIL) or (Length(FCaption) = 0) then begin
    FCharIndexes := NIL;
    FSize.cx := 0;
    FSize.cy := 0;
    exit;
  end else begin
   FCharIndexes := FTexturedFont.StringToCharIndexes(FCaption);
   FSize := FTexturedFont.CharIndexesToSize(FCharIndexes);
  end;

  FSizeComputationNeeded := False;
end;

function TFreeText.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TFreeText.SetCaption(AValue: string);
begin
 if FCaption = AValue then exit;
  FCaption := AValue;
  UpdateCharIndexesAndComputeSize;
end;

procedure TFreeText.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont = AValue then Exit;
  FTexturedFont := AValue;
  UpdateCharIndexesAndComputeSize;
end;

function TFreeText.GetWidth: integer;
begin
  Result := FSize.cx;
end;

function TFreeText.GetHeight: integer;
begin
  Result := FSize.cy;
end;

procedure TFreeText.DoDraw;
begin
  if (FSize.cx = 0) or (FTexturedFont = NIL) then exit;
  FTexturedFont.DrawChars(FCharIndexes, 0, 0, FlipToIndex, NIL,
                             NIL, FComputedOpacity, FComputedTint, FBlendMode);
end;

constructor TFreeText.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
end;

 { TFreeTextOnPathToFollow }

procedure TFreeTextOnPathToFollow.Update(const aElapsedTime: single);
begin
   inherited Update(aElapsedTime);
   if not FFreeze then
     DistanceTraveled.OnElapse(aElapsedTime);
end;

procedure TFreeTextOnPathToFollow.Draw(const aLayerPercentOpacity: single);
var i, yOffset, flipIndex: integer;
  m: TOGLCMatrix;
  pos: TPointF;
  ang, dist, spacingCoef: single;
  ch: PTexturedChar;
begin
  if (FSize.cx = 0) or (FTexturedFont = NIL) then exit;

  FComputedOpacity := aLayerPercentOpacity*Opacity.Value*OneDiv255;
  if FComputedOpacity = 0 then exit;
  FComputedTint.InitFromTint(TintMode, Tint.Value);

  // render childs with negative ZOrder
  if (FChilds.Count > 0) and (TSimpleSurfaceWithEffect(FChilds[0]).FZOrderAsChild < 0) then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithNegativeZOrder(FComputedOpacity)
      else DrawChildSurfacesWithNegativeZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;

  // here model view matrix is set individually for each characters to draw.

  flipIndex := FlipToIndex;
  spacingCoef := FTexturedFont.CharSpacingCoeff.Value;
  dist := DistanceTraveled.Value;
  yOffset := FTexturedFont.Font.FontPixelMetric.Baseline;

  for i:=0 to High(FCharIndexes) do begin
   ch := FTexturedFont.GetCharByIndex(FCharIndexes[i]);
   if ch <> NIL then begin
     TOGLCPathToFollow(FParentSurface).GetPosAndAngle(dist, pos, ang);
     m.CopyFrom(FParentScene.ModelViewMatrix);
     with m do begin
       Translate(pos.x, pos.y);
       Scale(Self.Scale.x.Value, Self.Scale.y.Value);
       if FAutoRotate then Rotate(ang + FAngleAdjust);
     end;
     FTexturedFont.DrawChar(FCharIndexes[i], 0, -yOffset, flipIndex, NIL, @m.Matrix, FComputedOpacity, FComputedTint, FBlendMode);
     dist := dist + ch^.OriginalCharSize.cx * spacingCoef * Scale.x.Value;
   end;
  end;

  // render childs with positive ZOrder
  if FChilds.Count > 0 then begin
    PushAndPrepareSceneMV;
    if FChildsUseParentOpacity then DrawChildSurfacesWithPositiveZOrder(FComputedOpacity)
      else DrawChildSurfacesWithPositiveZOrder(aLayerPercentOpacity);
    PopSceneMV;
  end;
end;

procedure TFreeTextOnPathToFollow.DoDraw;
begin
end;

constructor TFreeTextOnPathToFollow.CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow);
begin
  inherited Create(aParentPathToFollow.ParentScene);
  DistanceTraveled := TFParam.Create;
  AutoRotate := TRUE;
  aParentPathToFollow.AddChild(Self);
end;

destructor TFreeTextOnPathToFollow.Destroy;
begin
  FreeAndNil(DistanceTraveled);
  inherited Destroy;
end;

 { TFreeTextClockLabel }

procedure TFreeTextClockLabel.SetTime(AValue: single);
begin
  if FTime = AValue then Exit;
  FTime := AValue;
  UpdateCaption;
end;

procedure TFreeTextClockLabel.SetShowFractionalPart(AValue: boolean);
begin
  if FShowFractionalPart = AValue then Exit;
  FShowFractionalPart := AValue;
  UpdateCaption;
end;

procedure TFreeTextClockLabel.UpdateCaption;
begin
  if FShowFractionalPart then
    Caption := SecondsToString(FTime, FShowFractionalPart)
  else
    Caption := IntToStr(Ceil(FTime));
end;

constructor TFreeTextClockLabel.Create(aParentScene: TOGLCScene; aPaused: boolean);
begin
  inherited Create(aParentScene);
  FPaused := aPaused;
  FShowFractionalPart := TRUE;
end;

procedure TFreeTextClockLabel.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  if FFreeze or FPaused then exit;

  if FCountdown then begin
    if FTime > 0 then begin
      FTime := FTime - aElapsedTime;
      if FTime < 0 then begin
        FTime := 0;
        if FOnCountdownDone <> NIL then FOnCountdownDone(self);
        FPaused := True;
      end;
    end;
  end else FTime := FTime + aElapsedTime;
  UpdateCaption;
end;

procedure TFreeTextClockLabel.Pause;
begin
  FPaused := TRUE;
end;

procedure TFreeTextClockLabel.Run;
begin
  FPaused := FALSE;
end;

{ TTexturedFont }

constructor TTexturedFont.Create(aParentScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap);
begin
  FPArentScene := aParentScene;
  Init(aFont);
  FAtlasOwner := True;
  FAtlas := TOGLCTextureAtlas.Create(aParentScene);
  InternalBuild(FAtlas, aCharSet, aFillTexture);
  FAtlas.TryToPack;
  FAtlas.Build;
end;

constructor TTexturedFont.CreateFromImagePacker(aParentScene: TOGLCScene; aPacker: TOGLCImagePacker; aFont: TFontDescriptor;
  const aCharSet: string; aFillTexture: TBGRABitmap);
begin
  FParentScene := aParentScene;
  Init(aFont);
  InternalBuild(aPacker, aCharSet, aFillTexture);
  FAtlasOwner := False;
end;

destructor TTexturedFont.Destroy;
begin
  if FAtlasOwner then FreeAndNil(FAtlas);
  FTexCharArray := NIL;
  FreeAndNil(CharSpacingCoeff);
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


procedure TTexturedFont.InternalBuild(aPacker: TOGLCImagePacker; const aCharSet: string; aFillTexture: TBGRABitmap);
var i: integer;
    temp: TBGRABitmap;
    s: string;
begin
  temp := TBGRABitmap.Create(1, 1);
  FFont.SetFontRendererOn(temp);

  // build the font
  SetLength(FTexCharArray, UTF8LengthFast(aCharSet));
  for i:=0 to Length(FTexCharArray)-1 do
   begin
    s := UTF8Copy(aCharSet, i+1, 1);
    FFont.ResizeAndDrawStringOn(temp, s, aFillTexture);
    FTexCharArray[i].Texture := aPacker.Add(temp.Duplicate);
    FTexCharArray[i].Character := s;
    FTexCharArray[i].DecoratedCharSize := Size(temp.Width, temp.Height);
    FTexCharArray[i].OriginalCharSize := temp.TextSize(s);
   end;
  temp.Free;
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

procedure TTexturedFont.DrawChar(aCharIdx: integer; aX, aY, aFlipIndex: integer; const aClippingArea: PUIClipArea;
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
    if (aClippingArea <> NIL) and not aClippingArea^.CheckAndAdjust(modelArea, texCoor) then exit;
    FParentScene.TexturedMVTriangleRenderer.PushQuad3(modelArea, texCoor, aFlipIndex, aModelViewMatrix,
                                                     aOpacity, aComputedTint);
  end;
end;

procedure TTexturedFont.DrawChars(const aCharIdx: TTexturedCharIndexes; aX, aY, aFlipIndex: integer;
   const aClippingArea: PUIClipArea; const aModelViewMatrix: POGLCMatrix44; const aOpacity: single;
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
     if aClippingArea <> NIL then flagDraw := aClippingArea^.CheckAndAdjust(modelArea, texCoor)
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

