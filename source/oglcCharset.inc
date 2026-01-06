{$ifdef oglcINTERFACE}
{
  different charsets
}

type

{ TOGLCCharsets }

TOGLCCharsets = record
private
  FCount: integer;
  procedure AddRange(var charset: string; start, stop: QWord);
public
const
  NUMBER='0123456789';
  ASCII_SYMBOL='!"#$%&''()*+,-./:;<=>?@[\]^_`{|}~';
  ASCII_LETTER='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  ASCII_SPACE=' ';
  SIMPLELATIN=NUMBER + ASCII_SPACE + ASCII_LETTER;
  LATIN1_SUPPLEMENT_1='ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ';
  LATIN1_SUPPLEMENT_2=' ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿';

  procedure check;
  // This function scan the text and return only one occurence of the characters used
  // Usefull to construct small charset with only needed chars.
  // The function remove all kind of LineEnding.
  function TextToCharset(const aText: string): string;
  // Complete the given charset with the characters inside aText, avoiding redundant char. Return the modified charset.
  // The function remove all kind of LineEnding.
  function AddToCharset(const aCharSet, aText: string): string; overload;
  function AddToCharset(const aCharset: string; aText: TStringArray): string; overload;

  // return the list of the name of the available charsets
  function GetListOfCharsetNames: TStringArray;
  // return a charset from its name
  function GetByName(const aCharsetName: string): string;

  function LATIN_EXTENDED_A: string;
  function LATIN_EXTENDED_B: string;
  function GREEK_AND_COPTIC: string;
  function CYRILLIC: string;
  function CYRILLIC_SUPPLEMENT: string;
  function ARMENIAN: string;
  function HEBREW: string;
  function ARABIC: string;
  function SYRIAC: string;
  function ARABIC_SUPPLEMENT: string;
  function THAANA: string;
  function NKo: string;
  // India, Nepal
  function DEVANAGARI: string;
  function BENGALI: string;
  function GURMUKHI: string;
  function GUJARATI: string;
  function ORIYA: string;
  function TAMIL: string;
  function TELUGU: string;
  function KANNADA: string;
  function MALAYALAM: string;
  function SINHALA: string;
  function THAI: string;
  function LAO: string;
  function TIBETAN: string;
  function MYANMAR: string;
  function GEORGIAN: string;
  function HANGUL_JAMO: string;
  function ETHIOPIC: string;
  function ETHIOPIC_SUPPLEMENT: string;
  function CHEROKEE: string;
  function UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS: string;
  // to be continued...
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TOGLCCharsets.AddRange(var charset: string; start, stop: QWord);
  procedure AddByte(b: byte);
  begin
    charset[FCount] := chr(b);
    inc(FCount);
  end;
begin
  while start <= stop do begin
    if FCount >= Length(charset) then
      SetLength(charset, Length(charset) + 256);

    // add code point: utf8 support up to 6 bytes code point but the biggest code point of Unicode 6.0 takes only 4 bytes
    if ((start shr 40) and $FF) <> 0 then AddByte((start shr 40) and $FF);
    if ((start shr 32) and $FF) <> 0 then AddByte((start shr 32) and $FF);
    if ((start shr 24) and $FF) <> 0 then AddByte((start shr 24) and $FF);
    if ((start shr 16) and $FF) <> 0 then AddByte((start shr 16) and $FF);
    if ((start shr 8) and $FF) <> 0 then AddByte((start shr 8) and $FF);
    AddByte(start and $FF);

    Inc(start);
  end;
end;

procedure TOGLCCharsets.check;
var s: string;
begin
  s := '';
  FCount := 1;
  AddRange(s, $C680, $C6BF);
  AddRange(s, $C780, $C7BF);
  AddRange(s, $C880, $C8BF);
  AddRange(s, $C980, $C98F);
  SetLength(s, FCount-1);
  if s <> LATIN_EXTENDED_B then raise exception.create('pas bon');
end;

function TOGLCCharsets.TextToCharset(const aText: string): string;
begin
  Result := OGLCScene.AddToCharset('', aText);
end;

function TOGLCCharsets.AddToCharset(const aCharSet, aText: string): string;
begin
  Result := OGLCScene.AddToCharset(aCharSet, aText);
end;

function TOGLCCharsets.AddToCharset(const aCharset: string; aText: TStringArray): string;
begin
  Result := OGLCScene.AddToCharset(aCharset, aText);
end;

function TOGLCCharsets.GetListOfCharsetNames: TStringArray;
begin
  Result := ['NUMBER',
             'ASCII_SYMBOL',
             'ASCII_SPACE',
             'ASCII_LETTER',
             'LATIN1_SUPPLEMENT_1',
             'LATIN1_SUPPLEMENT_2',
             'LATIN_EXTENDED_A',
             'LATIN_EXTENDED_B',
             'GREEK_AND_COPTIC',
             'CYRILLIC',
             'CYRILLIC_SUPPLEMENT',
             'ARMENIAN',
             'HEBREW',
             'ARABIC',
             'SYRIAC',
             'ARABIC_SUPPLEMENT',
             'THAANA',
             'NKo',
             'DEVANAGARI',
             'BENGALI',
             'GURMUKHI',
             'GUJARATI',
             'ORIYA',
             'TAMIL',
             'TELUGU',
             'KANNADA',
             'MALAYALAM',
             'SINHALA',
             'THAI',
             'LAO',
             'TIBETAN',
             'MYANMAR',
             'GEORGIAN',
             'HANGUL_JAMO',
             'ETHIOPIC',
             'ETHIOPIC_SUPPLEMENT',
             'CHEROKEE',
             'UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS'];
end;

function TOGLCCharsets.GetByName(const aCharsetName: string): string;
begin
  case aCharsetName of
    'NUMBER': Result := NUMBER;
    'ASCII_SYMBOL': Result := ASCII_SYMBOL;
    'ASCII_SPACE': Result := ASCII_SPACE;
    'ASCII_LETTER': Result := ASCII_LETTER;
    'LATIN1_SUPPLEMENT_1': Result := LATIN1_SUPPLEMENT_1;
    'LATIN1_SUPPLEMENT_2': Result := LATIN1_SUPPLEMENT_2;
    'LATIN_EXTENDED_A': Result := LATIN_EXTENDED_A;
    'LATIN_EXTENDED_B': Result := LATIN_EXTENDED_B;
    'GREEK_AND_COPTIC': Result := GREEK_AND_COPTIC;
    'CYRILLIC': Result := CYRILLIC;
    'CYRILLIC_SUPPLEMENT': Result := CYRILLIC_SUPPLEMENT;
    'ARMENIAN': Result := ARMENIAN;
    'HEBREW': Result := HEBREW;
    'ARABIC': Result := ARABIC;
    'SYRIAC': Result := SYRIAC;
    'ARABIC_SUPPLEMENT': Result := ARABIC_SUPPLEMENT;
    'THAANA': Result := THAANA;
    'NKo': Result := NKo;
    'DEVANAGARI': Result := DEVANAGARI;
    'BENGALI': Result := BENGALI;
    'GURMUKHI': Result := GURMUKHI;
    'GUJARATI': Result := GUJARATI;
    'ORIYA': Result := ORIYA;
    'TAMIL': Result := TAMIL;
    'TELUGU': Result := TELUGU;
    'KANNADA': Result := KANNADA;
    'MALAYALAM': Result := MALAYALAM;
    'SINHALA': Result := SINHALA;
    'THAI': Result := THAI;
    'LAO': Result := LAO;
    'TIBETAN': Result := TIBETAN;
    'MYANMAR': Result := MYANMAR;
    'GEORGIAN': Result := GEORGIAN;
    'HANGUL_JAMO': Result := HANGUL_JAMO;
    'ETHIOPIC': Result := ETHIOPIC;
    'ETHIOPIC_SUPPLEMENT': Result := ETHIOPIC_SUPPLEMENT;
    'CHEROKEE': Result := CHEROKEE;
    'UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS': Result := UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS;
    else raise exception.create(' Charset '+aCharsetName+' is not available...');
  end;
end;

function TOGLCCharsets.LATIN_EXTENDED_A: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $C480, $C4BF);
  AddRange(Result, $C580, $C5BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.LATIN_EXTENDED_B: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $C680, $C6BF);
  AddRange(Result, $C780, $C7BF);
  AddRange(Result, $C880, $C8BF);
  AddRange(Result, $C980, $C98F);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.GREEK_AND_COPTIC: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $CDB0, $CDBF);
  AddRange(Result, $CE80, $CEBF);
  AddRange(Result, $CF80, $CFBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.CYRILLIC: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $D080, $D0BF);
  AddRange(Result, $D180, $D1BF);
  AddRange(Result, $D280, $D2BF);
  AddRange(Result, $D380, $D3BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.CYRILLIC_SUPPLEMENT: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $D480, $D4AF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ARMENIAN: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $D4B0, $D4BF);
  AddRange(Result, $D580, $D5BF);
  AddRange(Result, $D680, $D68F);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.HEBREW: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $D690, $D6BF);
  AddRange(Result, $D780, $D7BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ARABIC: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $D880, $D8BF);
  AddRange(Result, $D980, $D9BF);
  AddRange(Result, $DA80, $DABF);
  AddRange(Result, $DB80, $DBBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.SYRIAC: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $DC80, $DCBF);
  AddRange(Result, $DD80, $DD8F);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ARABIC_SUPPLEMENT: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $DD90, $DDBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.THAANA: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $DE80, $DEBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.NKo: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $DF80, $DFBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.DEVANAGARI: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0A480, $E0A4BF);
  AddRange(Result, $E0A580, $E0A5BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.BENGALI: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0A680, $E0A6BF);
  AddRange(Result, $E0A780, $E0A7BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.GURMUKHI: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0A880, $E0A8BF);
  AddRange(Result, $E0A980, $E0A9BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.GUJARATI: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0AA80, $E0AABF);
  AddRange(Result, $E0AB80, $E0ABBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ORIYA: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0AC80, $E0ACBF);
  AddRange(Result, $E0AD80, $E0ADBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.TAMIL: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0AE80, $E0AEBF);
  AddRange(Result, $E0AF80, $E0AFBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.TELUGU: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0B080, $E0B0BF);
  AddRange(Result, $E0B180, $E0B1BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.KANNADA: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0B280, $E0B2BF);
  AddRange(Result, $E0B380, $E0B3BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.MALAYALAM: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0B480, $E0B4BF);
  AddRange(Result, $E0B580, $E0B5BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.SINHALA: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0B680, $E0B6BF);
  AddRange(Result, $E0B780, $E0B7BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.THAI: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0B880, $E0B8BF);
  AddRange(Result, $E0B980, $E0B9BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.LAO: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0BA80, $E0BABF);
  AddRange(Result, $E0BB80, $E0BBBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.TIBETAN: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E0BC80, $E0BCBF);
  AddRange(Result, $E0BD80, $E0BDBF);
  AddRange(Result, $E0BE80, $E0BEBF);
  AddRange(Result, $E0BF80, $E0BFBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.MYANMAR: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E18080, $E180BF);
  AddRange(Result, $E18180, $E181BF);
  AddRange(Result, $E18280, $E1829F);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.GEORGIAN: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E182A0, $E182BF);
  AddRange(Result, $E18380, $E183BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.HANGUL_JAMO: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E18480, $E184BF);
  AddRange(Result, $E18580, $E185BF);
  AddRange(Result, $E18680, $E186BF);
  AddRange(Result, $E18780, $E187BF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ETHIOPIC: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E18880, $E188BF);
  AddRange(Result, $E18980, $E189BF);
  AddRange(Result, $E18A80, $E18ABF);
  AddRange(Result, $E18B80, $E18BBF);
  AddRange(Result, $E18C80, $E18CBF);
  AddRange(Result, $E18D80, $E18DBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.ETHIOPIC_SUPPLEMENT: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E18E80, $E18E9F);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.CHEROKEE: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E18EA0, $E18EBF);
  AddRange(Result, $E18F80, $E18FBF);
  SetLength(Result, FCount-1);
end;

function TOGLCCharsets.UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS: string;
begin
  Result := '';
  FCount := 1;
  AddRange(Result, $E19080, $E190BF);
  AddRange(Result, $E19180, $E191BF);
  AddRange(Result, $E19280, $E192BF);
  AddRange(Result, $E19380, $E193BF);
  AddRange(Result, $E19480, $E194BF);
  AddRange(Result, $E19580, $E195BF);
  AddRange(Result, $E19680, $E196BF);
  AddRange(Result, $E19780, $E197BF);
  AddRange(Result, $E19880, $E198BF);
  AddRange(Result, $E19980, $E199BF);
  SetLength(Result, FCount-1);
end;

{$endif oglcIMPLEMENTATION}

