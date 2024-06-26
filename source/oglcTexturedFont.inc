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



{ TFreeText }

// A surface that draw text. The text can be multiline. Lines are separated by #10.
// Instead of TUILabel, TFreeText is not clipped and mouse can't interact with it.
TFreeText = class(TSimpleSurfaceWithEffect)
private
  FCharIndexes: array of TTexturedCharIndexes;
  FCaptions: TStringArray;
  FTexturedFont: TTexturedFont;
  FSize: TSize;
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
// a text that moves along a TOGLCPathToFollow instance
TFreeTextOnPathToFollow = class(TFreeText)
private
  FAutoRotate: boolean;
  FAngleAdjust,
  FCoeffPositionOnPath: single;
  procedure SetCoeffPositionOnLine(AValue: single);
public
  procedure Update(const aElapsedTime: single); override;
  procedure Draw(const aLayerPercentOpacity: single);  override;
  procedure DoDraw; override;
public
  // Distance traveled on the path
  DistanceTraveled: TFParam;

  // Use this constructor to create the text associated with a TOGLCPathToFollow instance.
  constructor CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow; aZOrder: integer=0);
  destructor Destroy; override;

  // 'TRUE' force the sprite to rotate on the right direction along the path. Default is TRUE.
  property AutoRotate: boolean read FAutoRotate write FAutoRotate;
  // Set this value to ajust the right orientation of the sprite. Default is '0'.
  property AngleAdjust: single read FAngleAdjust write FAngleAdjust;
  // Allow to control the position of the sprite on the path. Range is from 0 to 1. Default value is 0.5
  //   0 -> the sprite is above the path
  // 0.5 -> the sprite is centered on the path
  //   1 -> the sprite is below the path
  property CoeffPositionOnPath: single read FCoeffPositionOnPath write SetCoeffPositionOnLine;
end;


{ TFreeTextClockLabel }
// A surface that can count/count down time and show the current time
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


{ TAlignedText }
// Draws a text in a rectangular area with horizontal and vertical align.
// The text can be multiline: use #10 to separate the lines. Ex: 'line 1'#10'line 2'
// Instead of TUITextArea, the text is not clipped and mouse can't interact with it.
TAlignedText = class(TSimpleSurfaceWithEffect)
private type
  TFreeTextWord = record
    Word: string;
    CharIndexes: TTexturedCharIndexes;
    Width,
    LineIndex: integer;
    Location: TPoint;
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
  FCaption: string;
  FTexturedFont: TTexturedFont;
  FRect: TRect;
  FWords: ArrayOfFreeTextWord;
  FLineCount: integer;
  FDrawingWidth,
  FDrawingHeight: integer;
  function GetCaption: string;
  procedure SetCaption(AValue: string);
  procedure SetTexturedFont(AValue: TTexturedFont);
  procedure DistributeTheWordsOnTheSurface;
  procedure SetAlign(AValue: TOGLCAlignment); virtual;
  function GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  procedure DoDraw; override;
public
  constructor Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
  // Redefine the size of the area where the text is displayed.
  procedure SetSize(aWidth, aHeight: integer);
  property Caption: string read GetCaption write SetCaption;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
  property Align: TOGLCAlignment read FAlign write SetAlign;
  property DrawingRect: TRect read FDrawingRect; // real size of the text area
end;

 {$endif oglcINTERFACE}
 {$ifdef oglcIMPLEMENTATION}

 { TAlignedText }

procedure TAlignedText.DistributeTheWordsOnTheSurface;
var W: TStringArray;
   iw, j, spaceWidth, xx, yy: Integer;
   flagLastWasLineEnding: boolean;
debugs: string;
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
      fullWord, partialWord: string;
   begin
     ww := 0;
     k := 0;
     fullWord := FWords[j].Word;
     repeat
       // fill the current line
      partialWord := '';
       repeat
         ch := FTexturedFont.GetCharByIndex(FWords[j].CharIndexes[k]);
         if ww + ch^.OriginalCharSize.cx > FRect.Width then break;
         ww := ww + ch^.OriginalCharSize.cx;
         partialWord := partialWord + ch^.Character;
         inc(k);
       until k = Length(FWords[j].CharIndexes);
       // if no letter is placed -> exit
       if partialWord = '' then exit;
       // register the partial line
       FWords[j].Width := ww;
       FWords[j].Location := Point(xx, yy);
       FWords[j].LineIndex := FLineCount-1;
       FWords[j].Word := partialWord;
       FWords[j].CharIndexes := TexturedFont.StringToCharIndexes(partialWord);
       // remove partial line from full word
       Delete(fullWord, 1, Length(partialWord));
       // add a new line if there remain some characters to position
       if fullWord <> '' then begin
         SetLength(FWords, Length(FWords)+1);
         _NextLine;
         inc(j);
         FWords[j].Word := fullWord;
         FWords[j].CharIndexes := TexturedFont.StringToCharIndexes(fullWord);
         FWords[j].Width := TexturedFont.CharIndexesToSize(FWords[j].CharIndexes).cx;
         FWords[j].LineIndex := FLineCount-1;
         FWords[j].Location := Point(xx, yy);
         k := 0;
         ww := 0;
       end;
     until fullWord = '';
     xx := FWords[j].Width + spaceWidth;
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
{FParentScene.LogDebug('TAlignedText.DistributeTheWordsOnTheSurface');
for xx:=0 to High(W) do FParentScene.LogDebug(''''+W[xx]+'''', 1); }
  // compute the number of word except #10
  xx := 0;
  for j:=0 to High(W) do if W[j] <> #10 then inc(xx);
  SetLength(FWords, xx);

  spaceWidth := FTexturedFont.GetCharWidth(' ');

  xx := 0;
  yy := 0;
  FLineCount := 1;
  iw := 0;
  j := 0;
  flagLastWasLineEnding := False;
  repeat
    FWords[j].Word := W[iw];
    FWords[j].CharIndexes := FTexturedFont.StringToCharIndexes(W[iw]);
    FWords[j].Width := FTexturedFont.GetTextWidth(FWords[j].Word);

    if FWords[j].Word = #10 then begin
      //if CurrentLineIsEmpty then dec(FLineCount) else flagLastWasLineEnding := True;
      //if CurrentLineIsEmpty then dec(FLineCount);
      flagLastWasLineEnding := True;
      _NextLine;
      inc(iw);
    end else if xx+FWords[j].Width <= FRect.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(iw);
      flagLastWasLineEnding := False;
    end else begin
         flagLastWasLineEnding := False;
         // the word don't fit
         if CurrentLineIsEmpty then begin
           DoWordWrapOnCurrentLine;
           inc(iw);
         end else begin
           _NextLine;
           DoWordWrapOnCurrentLine;
           inc(iw);
         end;
    end;
    if xx > FDrawingWidth then FDrawingWidth := xx;
  until iw = Length(W);

  if flagLastWasLineEnding then dec(FLineCount);

  FDrawingHeight := FTexturedFont.Font.FontHeight*FLineCount;

{// debug
  FParentScene.LogDebug('TAlignedText.DistributeTheWordsOnTheSurface');
  FParentScene.LogDebug('FLineCount='+FLineCount.ToString, 1);
  for j:=0 to High(FWords) do begin
   FParentScene.LogDebug('FWords['+j.ToString+'] = LineIndex:'+FWords[j].LineIndex.ToString+
                         '  Word:'''+FWords[j].Word+'''  Width:'+FWords[j].Width.ToString+
                         '  XY:('+FWords[j].Location.X.ToString+','+FWords[j].Location.Y.ToString+')', 1);
   debugs := '';
   for xx:=0 to High(FWords[j].CharIndexes) do
     debugs := debugs + FWords[j].CharIndexes[xx].ToString+' ';
   FParentScene.LogDebug('CharIndexes:'+debugs, 1);
  end; }
end;

procedure TAlignedText.SetAlign(AValue: TOGLCAlignment);
begin
   if FAlign=AValue then Exit;
   FAlign:=AValue;
   DoAlign;
end;

procedure TAlignedText.SetCaption(AValue: string);
begin
  AValue := StringReplace(AValue, #13#10, #10, [rfReplaceAll{, rfIgnoreCase}]);
  AValue := StringReplace(AValue, #13, #10, [rfReplaceAll{, rfIgnoreCase}]);
  if FCaption = AValue then exit;
  FCaption := AValue;
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

procedure TAlignedText.SetTexturedFont(AValue: TTexturedFont);
begin
   if FTexturedFont = AValue then exit;
   FTexturedFont := AValue;
   DistributeTheWordsOnTheSurface;
   DoAlign;
end;

function TAlignedText.GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
var i: integer;
begin
  iBegin := -1;
  iEnd := -1;
  for i:=low(FWords) to high(FWords) do begin
    if FWords[i].LineIndex = aLineIndex then begin
         if iBegin = -1 then iBegin := i;
         iEnd := i;
    end else if iBegin <> -1 then break;
   end;
  Result := (iBegin <> -1) and (iEnd <> -1);
end;

function TAlignedText.GetWidth: integer;
begin
  Result := FRect.Width;
end;

function TAlignedText.GetHeight: integer;
begin
  Result := FRect.Height;
end;

procedure TAlignedText.DoDraw;
var i: integer;
begin
  for i:=0 to Length(FWords)-1 do
    if FWords[i].Location.y < FRect.Height then
      FTexturedFont.DrawChars(FWords[i].CharIndexes, FWords[i].Location.x, FWords[i].Location.y, 0, NIL, NIL,
                               FComputedOpacity, FComputedTint, FBlendMode);
end;

constructor TAlignedText.Create(aParentScene: TOGLCScene; aFont: TTexturedFont; aWidth, aHeight: integer);
begin
  inherited Create;
  FParentScene := aParentScene;
  FTexturedFont := aFont;
  FAlign := taCenterCenter;
  FRect.Create(Point(0,0), aWidth, aHeight);
end;

procedure TAlignedText.SetSize(aWidth, aHeight: integer);
begin
  FRect.Create(Point(0,0), aWidth, aHeight);
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

procedure TAlignedText.DoVTopAlign;
var i, j, iBegin, iEnd: integer;
  yy: integer;
begin
  FDrawingRect.Top:=0;
  yy := 0;
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy +=  FTexturedFont.Font.FontHeight;
  end;
end;

procedure TAlignedText.DoVCenterAlign;
var i, j, iBegin, iEnd, h: integer;
  yy: integer;
begin
  h := FLineCount*FTexturedFont.Font.FontHeight;
  if h = 0 then exit;
  yy := (FRect.Height - h) div 2;
  FDrawingRect.Top := round(yy);
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy += FTexturedFont.Font.FontHeight;
  end;
end;

procedure TAlignedText.DoVBottomAlign;
var i, j, iBegin, iEnd: integer;
  yy: integer;
begin
  yy := FRect.Height-FTexturedFont.Font.FontHeight; //FRect.Height-FTexturedFont.Font.FontHeight*Scale.y.Value
  FDrawingRect.Top:=FRect.Height-FDrawingHeight;
  for i:=FLineCount-1 downto 0 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do FWords[j].Location.y := yy;
    yy -= FTexturedFont.Font.FontHeight;  //FTexturedFont.Font.FontHeight*Scale.y.Value
  end;
end;

procedure TAlignedText.DoHLeftAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  FDrawingRect.Left:=0;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
        FWords[j].Location.x := w;
        w += FWords[j].Width + FTexturedFont.GetCharWidth(' '); //*Scale.x.Value
      end;
   end;
end;

procedure TAlignedText.DoHCenterAlign;
var i, j, iBegin, iEnd: integer;
  w: integer;
begin
  FDrawingRect.Left:=FRect.Width;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
        w += FWords[j].Width;
        if i <> iEnd then w += FTexturedFont.GetCharWidth(' ');
      end;

//    w := FWords[iEnd].Location.x + FWords[iEnd].Width - FWords[iBegin].Location.x; // total width of the line
      w := ( FRect.Width - w ) div 2;

      if FDrawingRect.Left > w then FDrawingRect.Left := round(w);

      for j:=iBegin to iEnd do begin
        FWords[j].Location.x := w ;
        w += {(}FWords[j].Width + FTexturedFont.GetCharWidth(' '){)*Scale.x.Value};
      end;
   end;
end;

procedure TAlignedText.DoHRightAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  FDrawingRect.Left:=FRect.Width;

  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := FRect.Width;
      for j:=iEnd downto iBegin do begin
        w -= FWords[j].Width;
        FWords[j].Location.x := w;
        if FDrawingRect.Left>w then FDrawingRect.Left:=w;
        w-=FTexturedFont.GetCharWidth(' ');
      end;
    end;
end;

procedure TAlignedText.DoAlign;
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

function TAlignedText.GetCaption: string;
begin
  Result := FCaption;
end;


 { TFreeText }

procedure TFreeText.UpdateCharIndexesAndComputeSize;
var i: integer;
  s: TSize;
begin
  FSize.cx := 0;
  FSize.cy := 0;

  if (FTexturedFont = NIL) or (Length(FCaptions) = 0) then begin
    FCharIndexes := NIL;
    exit;
  end;

  SetLength(FCharIndexes, Length(FCaptions));
  for i:=0 to High(FCaptions) do begin
    FCharIndexes[i] := FTexturedFont.StringToCharIndexes(FCaptions[i]);

    s := FTexturedFont.CharIndexesToSize(FCharIndexes[i]);
    if FSize.cx < s.cx then FSize.cx := s.cx;
    FSize.cy := FSize.cy + s.cy;
  end;
end;

function TFreeText.GetCaption: string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(FCaptions) do begin
    Result := Result + FCaptions[i];
    if i < High(FCaptions) then Result := Result + #10;
  end;
end;

procedure TFreeText.SetCaption(AValue: string);
begin
  if AValue = '' then FCaptions := NIL
    else FCaptions := AdjustLineEnding(AValue).Split([#10]);
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
var i, yy: integer;
begin
  if (FSize.cx = 0) or (FTexturedFont = NIL) then exit;
  yy := 0;
  for i:=0 to High(FCaptions) do begin
    FTexturedFont.DrawChars(FCharIndexes[i], 0, yy, FlipToIndex, NIL,
                            NIL, FComputedOpacity, FComputedTint, FBlendMode);
    yy := yy + FTexturedFont.Font.FontHeight;
  end;
end;

constructor TFreeText.Create(aParentScene: TOGLCScene);
begin
  inherited Create;
  FParentScene := aParentScene;
end;

 { TFreeTextOnPathToFollow }

procedure TFreeTextOnPathToFollow.SetCoeffPositionOnLine(AValue: single);
begin
  FCoeffPositionOnPath := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TFreeTextOnPathToFollow.Update(const aElapsedTime: single);
begin
   inherited Update(aElapsedTime);
   if not FFreeze then
     DistanceTraveled.OnElapse(aElapsedTime);
end;

procedure TFreeTextOnPathToFollow.Draw(const aLayerPercentOpacity: single);
var i, j, yOffset, flipIndex: integer;
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
  yOffset := yOffset - Round(yOffset*FCoeffPositionOnPath);

  for i:=0 to High(FCharIndexes) do begin
     for j:=0 to High(FCharIndexes[i]) do begin
      ch := FTexturedFont.GetCharByIndex(FCharIndexes[i][j]);
      if ch <> NIL then begin
        TOGLCPathToFollow(FParentSurface).GetPosAndAngle(dist, pos, ang);
        m.CopyFrom(FParentScene.ModelViewMatrix);
        with m do begin
          Translate(pos.x, pos.y + FTexturedFont.Font.FontHeight*i);
          Scale(Self.Scale.x.Value, Self.Scale.y.Value);
          if FAutoRotate then Rotate(ang + FAngleAdjust);
        end;
        FTexturedFont.DrawChar(FCharIndexes[i][j], 0, -yOffset, flipIndex, NIL, @m.Matrix, FComputedOpacity, FComputedTint, FBlendMode);
        dist := dist + ch^.OriginalCharSize.cx * spacingCoef * Scale.x.Value;
      end;
    end;
    dist := DistanceTraveled.Value;
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

constructor TFreeTextOnPathToFollow.CreateAsChildOf(aParentPathToFollow: TOGLCPathToFollow; aZOrder: integer);
begin
  inherited Create(aParentPathToFollow.ParentScene);
  DistanceTraveled := TFParam.Create;
  AutoRotate := TRUE;
  FCoeffPositionOnPath := 0.0;
  aParentPathToFollow.AddChild(Self, aZOrder);
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

