{$ifdef oglcINTERFACE}
type


TLetterSprite = class(TSprite)
  PosOrigin: TPointF;
  PosOffset: TPointF;
end;


TLetterFreeText = class(TFreeText)
  PosOrigin: TPointF;
  PosOffset: TPointF;
end;


{ TLetterSpriteContainer }

TLetterSpriteContainer = class(TSpriteContainer)
private
  FLetterCount,
  FWidth, FHeight: integer;
  function GetLetterSprite(index: integer): TLetterSprite;
  function GetLetterFreeText(index: integer): TLetterFreeText;
  function GetPosOrigin(index: integer): TPointF;
protected
  function GetWidth: integer; override;
  function GetHeight: integer; override;
public
  constructor Create(aParentScene: TOGLCScene; const aString: string; aTexturedFont: TTexturedFont);
  constructor Create(aParentScene: TOGLCScene; const aString: string;
                     aFD: TFontDescriptor; aFillTexture: TBGRABitmap);

  property LettersSprite[index:integer]: TLetterSprite read GetLetterSprite;
  property LettersFreeText[index:integer]: TLetterFreeText read GetLetterFreeText;
  property LetterCount: integer read FLetterCount;
  property PosOrigin[index:integer]: TPointF read GetPosOrigin;
public // animation presets
  procedure FallThenDisappearToTop(aStayDuration: single; aMoveDuration: single=-1);
  // TO DO: another anim
  // letters comes from right or left then disappear on the other side
  // letters are zoomed in
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function TLetterSpriteContainer.GetLetterSprite(index: integer): TLetterSprite;
begin
  Result := TLetterSprite(Childs[index]);
end;

function TLetterSpriteContainer.GetLetterFreeText(index: integer): TLetterFreeText;
begin
 Result := TLetterFreeText(Childs[index]);
end;

function TLetterSpriteContainer.GetPosOrigin(index: integer): TPointF;
var o: TSimpleSurfaceWithEffect;
begin
  o := Childs[index];
  if o is TLetterSprite then Result := TLetterSprite(o).PosOrigin
    else Result := TLetterFreeText(o).PosOrigin;
end;

function TLetterSpriteContainer.GetWidth: integer;
begin
  Result := FWidth;
end;

function TLetterSpriteContainer.GetHeight: integer;
begin
  Result := FHeight;
end;

constructor TLetterSpriteContainer.Create(aParentScene: TOGLCScene;
      const aString: string; aTexturedFont: TTexturedFont);
var i, w: integer;
  letter: string;
  o: TLetterFreeText;
  xx: single;
begin
  inherited Create(aParentScene);

  FLetterCount := UTF8Length(aString);
  xx := 0;
  FWidth := 0;
  for i:=1 to FLetterCount do begin
    letter := UTF8Copy(aString, i, 1);
    o := TLetterFreeText.Create(aParentScene);
    o.TexturedFont := aTexturedFont;
    o.Caption := letter;
    o.PosOrigin := PointF(xx, 0);
    o.SetChildOf(Self, i);

    w := aTexturedFont.GetCharWidth(letter);
    xx := xx + w;
    FWidth := FWidth + w;
  end;
  FHeight := aTexturedFont.Font.FontHeight;
end;

constructor TLetterSpriteContainer.Create(aParentScene: TOGLCScene;
  const aString: string; aFD: TFontDescriptor; aFillTexture: TBGRABitmap);
var i, w: integer;
  letter: string;
  o: TLetterSprite;
  xx: single;
begin
  inherited Create(aParentScene);

  FLetterCount := UTF8Length(aString);
  xx := 0;
  FWidth := 0;
  for i:=1 to FLetterCount do begin
    letter := UTF8Copy(aString, i, 1);
    o := TLetterSprite.Create(aParentScene, aFD, letter, aFillTexture);
    o.PosOrigin := PointF(xx, 0);
    o.SetChildOf(Self, i);

    w := aFD.TextSize(letter).cx;
    xx := xx + w;
    FWidth := FWidth + w;
  end;
  FHeight := aFD.FontHeight;
end;

procedure TLetterSpriteContainer.FallThenDisappearToTop(aStayDuration: single; aMoveDuration: single);
var i: integer;
  o: TSimpleSurfaceWithEffect;
  sce: string;
  pOrigin: TPointF;
const delay: single=0.1;
begin
  if aMoveDuration = -1 then
    aMoveDuration := aStayDuration*0.1;

  for i:=0 to ChildCount-1 do begin
    o := Childs[i];
    pOrigin := PosOrigin[i];
    sce := 'SetCoor '+FormatFloatWithDot('0.0', pOrigin.x)+' '+
                      FormatFloatWithDot('0.0', -FParentScene.Height*0.5-o.Height)+#10;
    if i > 0 then
      sce := sce+'Wait '+ FormatFloatWithDot('0.0', i*delay)+#10;
    sce := sce+'MoveYTo '+FormatFloatWithDot('0.0', pOrigin.y)+' '+FormatFloatWithDot('0.00', aMoveDuration)+' idcStartFastEndSlow'#10+
               'Wait '+FormatFloatWithDot('0.0', delay*LetterCount+aStayDuration)+#10+
               'MoveYTo '+FormatFloatWithDot('0.0', -FParentScene.Height*0.5-o.Height)+' '+FormatFloatWithDot('0.00', aMoveDuration)+' idcStartSlowEndFast';
    o.AddAndPlayScenario(sce);
  end;
  KillDefered(2*aMoveDuration*LetterCount + aStayDuration);
end;

{$endif oglcIMPLEMENTATION}

