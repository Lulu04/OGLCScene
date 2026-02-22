{$ifdef oglcINTERFACE}
type

{ TAlignedTextDescriptor }

TAlignedTextDescriptor = record
private type
  TWord_ = record
    Letters: string;
    CharIndexes: TTexturedCharIndexes;
    Location: TPoint;
    Width,
    LineIndex: integer;
  end;
  ArrayOfWord_ = array of TWord_;
private
  FCaption: string;
  FAlign: TOGLCAlignment;
  FTexturedFont: TTexturedFont;
  FSpaceCharWidth: integer;
  FLineCount: integer;
  FReadyToRender: boolean;
private
  FCallbackSizeChanged: TOGLCEvent;
  FRect: TRect;
  function GetLine(index: integer): string;
  procedure InitDefault;
  procedure FreeObjects;
  procedure DistributeTheWordsOnTheSurface;
  function GetCaption: string;
  procedure SetAlign(AValue: TOGLCAlignment);
  procedure SetArea(AValue: TRect);
  procedure SetCaption(AValue: string);
  procedure SetTexturedFont(AValue: TTexturedFont);
  // return True if some words are found on the specified line index.
  // False means this line is empty.
  function GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
  procedure DoVTopAlign;
  procedure DoVCenterAlign;
  procedure DoVBottomAlign;
  procedure DoHLeftAlign;
  procedure DoHCenterAlign;
  procedure DoHRightAlign;
  procedure DoAlign;
  procedure ForceDistributeWordsAndAlign;
public
  Tint: TBGRAParam;
  DrawingRect: TRect; // real rect area of the text area. TopLeft is relative to Area.
  DrawingSize: TSize; // The real size of the text area
  Words: ArrayOfWord_; // an array that contains info on each word to render.

  property Area: TRect read FRect write SetArea;
  // Initialize this property with the text you want to see in the text area.
  // Separate the lines with #10 character or any kind of LineEnding.
  property Caption: string read GetCaption write SetCaption;
  property Align: TOGLCAlignment read FAlign write SetAlign;
  property TexturedFont: TTexturedFont read FTexturedFont write SetTexturedFont;
  property LineCount: integer read FLineCount;
  property Lines[index:integer]: string read GetLine;
  // fired when caption or Textured font change.
  property OnSizeChanged: TOGLCEvent read FCallbackSizeChanged write FCallbackSizeChanged;
  // True if the caption is <> '' and property TexturedFont is initialized
  property ReadyToRender: boolean read FReadyToRender;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TAlignedTextDescriptor }

procedure TAlignedTextDescriptor.InitDefault;
begin
  FillChar(Self, SizeOf(TAlignedTextDescriptor), 0);
  Tint := TBGRAParam.Create;
  Tint.Value := BGRA(220,220,220);
end;

function TAlignedTextDescriptor.GetLine(index: integer): string;
var i: integer;
begin
  Result := '';
  for i:=0 to High(Words) do begin
    if Words[i].LineIndex = index then
      if i = 0 then Result := Words[i].Letters
        else Result := ' ' + Words[i].Letters;
  end;
end;

procedure TAlignedTextDescriptor.FreeObjects;
begin
  Tint.Free;
  Tint := NIL;
end;

procedure TAlignedTextDescriptor.DistributeTheWordsOnTheSurface;
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
    Words[j].Location := Point(xx, yy);
    Words[j].LineIndex := FLineCount-1;
    xx += Words[j].Width+spaceWidth;
    inc(j);
   end;
   procedure DoWordWrapOnCurrentLine;
   var ch: PTexturedChar;
      k, ww: integer;
      fullWord, partialWord: string;
   begin
     ww := 0;
     k := 0;
     fullWord := Words[j].Letters;
     repeat
       // fill the current line
      partialWord := '';
       repeat
         ch := FTexturedFont.GetCharByIndex(Words[j].CharIndexes[k]);
         if ch <> NIL then begin
           if ww + ch^.OriginalCharSize.cx > Area.Width then break;
           ww := ww + ch^.OriginalCharSize.cx;
           partialWord := partialWord + ch^.Character;
         end;
         inc(k);
       until k = Length(Words[j].CharIndexes);
       // if no letter is placed -> exit
       if partialWord = '' then exit;
       // register the partial line
       Words[j].Width := ww;
       Words[j].Location := Point(xx, yy);
       Words[j].LineIndex := FLineCount-1;
       Words[j].Letters := partialWord;
       Words[j].CharIndexes := TexturedFont.StringToCharIndexes(partialWord);
       // remove partial line from full word
       Delete(fullWord, 1, Length(partialWord));
       // add a new line if there remain some characters to position
       if fullWord <> '' then begin
         SetLength(Words, Length(Words)+1);
         _NextLine;
         inc(j);
         Words[j].Letters := fullWord;
         Words[j].CharIndexes := TexturedFont.StringToCharIndexes(fullWord);
         Words[j].Width := TexturedFont.CharIndexesToSize(Words[j].CharIndexes).cx;
       {  Words[j].LineIndex := FLineCount-1;
         Words[j].Location := Point(xx, yy);  }
         k := 0;
         ww := 0;
       end;
     until fullWord = '';
     xx := Words[j].Width + spaceWidth;
     inc(j);
   end;

   function CurrentLineIsEmpty: boolean;
   begin
    Result := xx = 0;
   end;

begin
  FLineCount := 0;
  DrawingSize.cx := 0;
  DrawingSize.cy := 0;
  DrawingRect.Width := 0;
  DrawingRect.Height := 0;
  Words := NIL;
  FReadyToRender := (FCaption <> '') and (FTexturedFont <> NIL);
  if not FReadyToRender then exit;

  W := SplitTextToWordArray(FCaption, ' ');
  if Length(w) = 0 then exit;
  // compute the number of word except #10
  xx := 0;
  for j:=0 to High(W) do if W[j] <> #10 then inc(xx);
  SetLength(Words, xx);

  spaceWidth := FTexturedFont.GetCharWidth(' ');

  xx := 0;
  yy := 0;
  FLineCount := 1;
  iw := 0;
  j := 0;
  repeat

    if W[iw] = #10 then begin
      _NextLine;
      inc(iw);
    end else begin
      Words[j].Letters := W[iw];
      Words[j].CharIndexes := FTexturedFont.StringToCharIndexes(W[iw]);
      Words[j].Width := FTexturedFont.GetTextWidth(Words[j].Letters);

      if xx+Words[j].Width <= Area.Width then begin
      // the word fit
      PutWordOnCurrentLine;
      inc(iw);
      end else begin
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
    end;
    if xx > DrawingSize.cx then DrawingSize.cx := xx;
  until iw = Length(W);

  DrawingSize.cy := FTexturedFont.Font.FontHeight*FLineCount;
end;

function TAlignedTextDescriptor.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TAlignedTextDescriptor.SetAlign(AValue: TOGLCAlignment);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
  DoAlign;
end;

procedure TAlignedTextDescriptor.SetArea(AValue: TRect);
begin
  if FRect = AValue then Exit;
  FRect := AValue;
  DistributeTheWordsOnTheSurface;
  DoAlign;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged;
end;

procedure TAlignedTextDescriptor.SetCaption(AValue: string);
begin
  AValue := AdjustLineEnding(AValue);
  if FCaption = AValue then exit;
  FCaption := AValue;
  DistributeTheWordsOnTheSurface;
  DoAlign;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged;
end;

procedure TAlignedTextDescriptor.SetTexturedFont(AValue: TTexturedFont);
begin
  if FTexturedFont = AValue then exit;
  FTexturedFont := AValue;
  FSpaceCharWidth := FTexturedFont.GetCharWidth(' ');
  DistributeTheWordsOnTheSurface;
  DoAlign;
  if FCallbackSizeChanged <> NIL then FCallbackSizeChanged;
end;

function TAlignedTextDescriptor.GetWordsIndexes(aLineIndex: integer; out iBegin, iEnd: integer): boolean;
var i: integer;
begin
  iBegin := -1;
  iEnd := -1;
  for i:=low(Words) to high(Words) do begin
    if Words[i].LineIndex = aLineIndex then begin
      Result := True;
      if iBegin = -1 then iBegin := i;
      iEnd := i;
    end else if iBegin <> -1 then break;
   end;
  Result := (iBegin <> -1) and (iEnd <> -1);
end;

procedure TAlignedTextDescriptor.DoVTopAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  DrawingRect.Top := 0;
  yy := 0;
  yDelta := FTexturedFont.Font.FontHeight;
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do Words[j].Location.y := yy;
    yy := yy + yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoVCenterAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  if DrawingSize.cy = 0 then exit;
  yy := (Area.Height - DrawingSize.cy) div 2;
  yDelta := FTexturedFont.Font.FontHeight;
  DrawingRect.Top := round(yy);
  for i:=0 to FLineCount-1 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do Words[j].Location.y := yy;
    yy := yy + yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoVBottomAlign;
var i, j, iBegin, iEnd, yy, yDelta: integer;
begin
  yy := Area.Height - FTexturedFont.Font.FontHeight;
  yDelta := FTexturedFont.Font.FontHeight;
  DrawingRect.Top := Area.Height - DrawingSize.cy;
  for i:=FLineCount-1 downto 0 do begin
    if GetWordsIndexes(i, iBegin, iEnd) then
      for j:=iBegin to iEnd do Words[j].Location.y := yy;
    yy := yy - yDelta;
  end;
end;

procedure TAlignedTextDescriptor.DoHLeftAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  DrawingRect.Left:=0;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
        Words[j].Location.x := w;
        w := w + Words[j].Width + FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoHCenterAlign;
var i, j, iBegin, iEnd: integer;
  w: single;
begin
  DrawingRect.Left:=Area.Width;
  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := 0;
      for j:=iBegin to iEnd do begin
       w+= Words[j].Width;
       if i <> iEnd then w += FSpaceCharWidth;
      end;

      w := (Area.Width - w)*0.5;

      if DrawingRect.Left > w then DrawingRect.Left := Round(w);

      for j:=iBegin to iEnd do begin
        Words[j].Location.x := Round(w);
        w := w + Words[j].Width + FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoHRightAlign;
var i, j, w, iBegin, iEnd: integer;
begin
  DrawingRect.Left := Area.Width;

  for i:=0 to FLineCount-1 do
    if GetWordsIndexes(i, iBegin, iEnd) then begin
      w := Area.Width;
      for j:=iEnd downto iBegin do begin
        w := w - Words[j].Width;
        Words[j].Location.x := w;
        if DrawingRect.Left > w then DrawingRect.Left := w;
        w := w - FSpaceCharWidth;
      end;
    end;
end;

procedure TAlignedTextDescriptor.DoAlign;
var adjustedAlign: TOGLCAlignment;
begin
  if (FTexturedFont = NIL) or (FLineCount = 0) then exit;

  adjustedAlign := FAlign;
  // vertical center/bottom becomes vertical top if the vertical size of the text don't fit in the area
  if DrawingSize.cy > Area.Height then begin
    case FAlign of
      taCenterLeft, taBottomLeft: adjustedAlign := taTopLeft;
      taCenterCenter, taBottomCenter: adjustedAlign := taTopCenter;
      taCenterRight, taBottomRight: adjustedAlign := taTopRight;
    end;
  end;

  case adjustedAlign of
   taTopLeft, taCenterLeft, taBottomLeft: DoHLeftAlign;
   taTopRight, taCenterRight, taBottomRight: DoHRightAlign;
   taTopCenter, taCenterCenter, taBottomCenter: DoHCenterAlign;
  end;

  case adjustedAlign of
   taTopLeft, taTopCenter, taTopRight: DoVTopAlign;
   taCenterLeft, taCenterCenter, taCenterRight: DoVCenterAlign;
   taBottomLeft, taBottomCenter, taBottomRight: DoVBottomAlign;
  end;

  DrawingRect.Bottom := DrawingRect.Top + DrawingSize.cy;
  DrawingRect.Right := DrawingRect.Left + DrawingSize.cx;
end;

procedure TAlignedTextDescriptor.ForceDistributeWordsAndAlign;
begin
  DistributeTheWordsOnTheSurface;
  DoAlign;
end;

{$endif oglcIMPLEMENTATION}

