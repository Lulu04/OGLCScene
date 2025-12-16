{$ifdef oglcINTERFACE}
type

{ TApp }

TApp = record
  // in case you use ALSound library to play audio.
  class function ALSoundLibrariesSubFolder: string; static;
  // return the path to application Data folder
  class function DataFolder: string; static;
  class function OSName: string; static;
end;


{ TOGLCSaveDirectory }
// Use this to create a directory in an appropriate place according to the OS to save your application/game data.
TOGLCSaveDirectory = class
private
  FCreated: boolean;
  FSaveFolder: string;
public
  // Create a folder named with what you specify in parameter 'aCompanyName' in appropriate
  // place according to the OS then create a sub-folder with the name of the application.
  // On Windows the folder is located in ProgramData. i.e. C:\ProgramData\LuluGame\FireAndWire
  // On Linux the folder is located in /username/
  // On MacOS the folder is located in /
  constructor CreateFolder(const aCompanyName: string);
  // True if the folder is well created and usable.
  property FolderCreated: boolean read FCreated;
  // returns the full path of the folder where you can create your save files, appended with a directory separator.
  // i.e. C:\ProgramData\LuluGame\FireAndWire\
  property SaveFolder: string read FSaveFolder;
end;


{ TProperties }

{ TProperties is used to save and load data in a string container. A data have a field name and a value, like 'Width|500'
  A char separator is used to separates each fields and values.
  When saving a string, to allow all possible characters, strings are encoded in hexadecimal(optionnal)

  SAVING DATA IN A STRING:
     function DataToString: string;
     var prop: TProperties;
     begin
       prop.Init('|');
       prop.Add('Name', FObjectName); // FObjectName is of type string and equal to 'Object1'
       prop.Add('Width', FWidth);     // FWidth is integer and = 120
       prop.Add('Height', FHeight);   // FHeight is integer and = 100
       prop.Add('IsValid', FIsValid); // FIsValid is boolean and = True
       Result := prop.PackedProperty; // return the string 'Name|Object1|Width|120|Height|100|IsValid|true'
     end;

  NOTE: in the above example, if the variable FObjectName is likely to contain the separator character,
        instead of prop.Add('Name', FObjectName);
        use        prop.Add('Name', FObjectName, True);
            -> the content of FObjectName is first converted in hexadecimal then pushed in the field 'Name'

  LOADING DATA FROM STRING
    procedure LoadFromString(const s: string);
    var prop: TProperties;
    begin
      prop.Split(s, '|');
      prop.IntegerValueOf('Width', FWidth, 10);  // 10 is a default value assigned to FWidth if the field 'Width' is not found
      prop.IntegerValueOf('Height', FHeight, 10);
      prop.BooleanValueOf('IsValid', FIsValid, False);
    end;

// Note: in ASCII table, there are four separator: $1C, $1D, $1E, $1F that can be used.
// $1C is used internally to encode string in hexa.
}
TProperties = record
private const HEADER_ENCODED_STRING: byte = $1C;
private
 FProperties: TStringArray;
 FSeparator: Char;
 function ValueIndexOf(const aName: string; out index: integer): boolean;
 procedure AddProp(const n, v: string);
 function GetPackedProperty: string;
 function ByteToHex(const AValue: Byte): string;
 function HexToByte(const AHexValue: string): integer;
 function EncodeInHexa(const s: string): string;
 function DecodeFromHexa(const s: string): string;
public // unpack section
 procedure SetEmpty;
 // search the section with the specified name in the specified TStringList and split the property.
 // Return True if the section is found.
 function SplitFrom(t: TStringList; const aSectionName: string; aSeparator: Char): boolean;
 // Split and return the number of 'blocks' found.
 function Split(const s: string; aSeparator: Char): integer;
 // Return True if the property is defined
 function PropertyIsPresent(const aPropertyName: string): boolean;
 // Return true if all names property in the passed array are presents.
 function CheckProperties(aPropertyNames: TStringArray): boolean;
 // Return True if aPropertyName is found and set its value to Value
 function StringValueOf(const aPropertyName: string; var Value: string; const aDefault: string): boolean;
 function BooleanValueOf(const aPropertyName: string; var Value: boolean; aDefault: boolean): boolean;
 function IntegerValueOf(const aPropertyName: string; var Value: integer; aDefault: integer): boolean;
 function Int64ValueOf(const aPropertyName: string; var Value: int64; aDefault: int64): boolean;
 function SingleValueOf(const aPropertyName: string; var Value: single; aDefault: single): boolean;
 //function DoubleValueOf(const aPropertyName: string; var Value: double; aDefault: single): boolean;
 function ByteValueOf(const aPropertyName: string; var Value: byte; aDefault: byte): boolean;
 function WordValueOf(const aPropertyName: string; var Value: word; aDefault: word): boolean;
 function BGRAPixelValueOf(const aPropertyName: string; var Value: TBGRAPixel; aDefault: TBGRAPixel): boolean;
 function PointValueOf(const aPropertyName: string; var Value: TPoint; aDefault: TPoint): boolean;
 function PointFValueOf(const aPropertyName: string; var Value: TPointF; aDefault: TPointF): boolean;

public // pack section
 procedure Init(aSeparator: char);
 procedure Add(const aPropertyName, aValue: string; aEncodeInHexa: boolean=False);
 procedure Add(const aPropertyName: string; aValue: boolean);
 procedure Add(const aPropertyName: string; aValue: integer);
 procedure Add(const aPropertyName: string; aValue: int64);
 procedure Add(const aPropertyName: string; aValue: single);
 procedure Add(const aPropertyName: string; aValue: TBGRAPixel);
 procedure Add(const aPropertyName: string; aValue: TPoint);
 procedure Add(const aPropertyName: string; aValue: TPointF; aFormat: string='0.000');
 property PackedProperty: string read GetPackedProperty;

public
 function ReplaceValue(const aPropertyName, aValue: string): boolean;
 function ReplaceValue(const aPropertyName: string; aValue: boolean): boolean;
 function ReplaceValue(const aPropertyName: string; aValue: integer): boolean;
 function ReplaceValue(const aPropertyName: string; aValue: int64): boolean;
 function ReplaceValue(const aPropertyName: string; aValue: single): boolean;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{$ifdef Darwin}
function AppBundleName: string;
begin
  Result := ApplicationName+'.app';
end;

function GetAppBundlePath: string;
var f, bundleName: string;
  i: SizeInt;
begin
  {$ifdef LCL}
  f := Application.Location;
  {$else}
  f := ExtractFilePath(ParamStr(0));
  {$endif}
  bundleName := '/'+AppBundleName;
  i := Pos(bundleName, f);
  if i > 0 then
    Result := ConcatPaths([copy(f, 1, i-1), bundleName])
  else
    Result := f;

  Result := IncludeTrailingPathDelimiter(Result);
end;
{$endif}

{ TApp }

class function TApp.ALSoundLibrariesSubFolder: string;
begin
 {$if defined(Windows) and defined(cpu386)}
  Result := 'i386-win32\';
 {$elseif defined(Windows) and defined(cpux86_64)}
  Result := 'x86_64-win64\';
 {$elseif defined(Linux) and defined(cpu386)}
  Result := 'i386-linux/';
 {$elseif defined(Linux) and defined(cpux86_64)}
  Result := 'x86_64-linux/';
 {$elseif defined(Darwin) and defined(cpux86_64)}
  Result := 'x86_64-macos/';
 {$else}
    {$error You can not compile this program for this platform !}
 {$endif}
end;

class function TApp.DataFolder: string;
begin
 {$if defined(Windows)}
   Result := IncludeTrailingPathDelimiter(Application.Location)+'Data\';
 {$elseif defined(Linux)}
   Result := IncludeTrailingPathDelimiter(Application.Location)+'Data/';
 {$elseif defined(Darwin)}
   Result := GetAppBundlePath;
   if DirectoryExists(Result) then
     // application use a bundle
     Result := Result + 'Contents/Resources/Data/'
   else begin
     // application don't use a bundle
     Result := ConcatPaths([ExtractFilePath(ParamStr(0)), 'Data']);
     Result := IncludeTrailingPathDelimiter(Result);
   end;
 {$else}
   {$error You can not compile this program on this platform, sorry !}
 {$endif}
end;

class function TApp.OSName: string;
begin
  Result := '';
  {$if defined(Windows) and defined(cpu386)}
    Result := 'i386-win32';
  {$elseif defined(Windows) and defined(cpux86_64)}
    Result := 'x86_64-win64';
  {$elseif defined(Linux) and defined(cpu386)}
    Result := 'i386-linux';
  {$elseif defined(Linux) and defined(cpux86_64)}
    Result := 'x86_64-linux';
  {$elseif defined(Darwin) and defined(cpux86_64)}
    Result := 'x86_64-macos';
  {$endif}
end;


var COMPANY_NAME: string;

function MyAppName: string;
begin
  Result := COMPANY_NAME;
end;

{ TOGLCSaveDirectory }

// create and return '\COMPANY_NAME\GameName\' folder in appropriate application data folder
// create the 2 folders if they don't exists
// example: Create folder 'LuluGame\' and sub-folder 'LuluGame\Firewire\'

constructor TOGLCSaveDirectory.CreateFolder(const aCompanyName: string);
var global: boolean;
  path: string;
begin
  COMPANY_NAME := aCompanyName;

  OnGetApplicationName := @MyAppName;
{$IFDEF WINDOWS}
  global := True;
{$ELSE}
  global := False;
{$ENDIF}
  path := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(global, True));
  OnGetApplicationName := NIL;
  path := IncludeTrailingPathDelimiter(path + ApplicationName);
  if not DirectoryExistsUTF8(path)
    then CreateDirUTF8(path);

  FCreated := DirectoryExistsUTF8(path);
  if FCreated then FSaveFolder := IncludeTrailingPathDelimiter(path)
    else FSaveFolder := '';
end;

{ TProperties }

function TProperties.ValueIndexOf(const aName: string; out index: integer): boolean;
var i: integer;
begin
  i := 0;
  while i < High(FProperties) do
  begin
     if FProperties[i] = aName then
     begin
       index := i+1;
       Result := True;
       exit;
     end;
     inc(i, 2);
  end;
  Result := False;
end;

procedure TProperties.AddProp(const n, v: string);
var i: integer;
begin
  i := Length(FProperties);
  SetLength(FProperties, i+2);
  FProperties[i] := n;
  FProperties[i+1] := v;
end;

function TProperties.GetPackedProperty: string;
var i: integer;
begin
  Result := '';
  i := 0;
  while i+1 < Length(FProperties) do begin
     if Length(Result) <> 0 then
       Result := Result + FSeparator;
     Result := Result + FProperties[i] + FSeparator + FProperties[i+1];
    inc(i, 2);
  end;
end;

function TProperties.ByteToHex(const AValue: Byte): string;
const CHex: array[0..15] of AnsiChar = '0123456789ABCDEF';
begin
  Result := CHex[AValue shr 4] + CHex[AValue and $0F];
end;

function TProperties.HexToByte(const AHexValue: string): integer;
begin
  TryStrToInt('$' + AHexValue, Result);
end;

function TProperties.EncodeInHexa(const s: string): string;
var i: integer;
  b: byte;
begin
  if s = '' then begin
    Result := '';
    exit;
  end;

  Result := chr(HEADER_ENCODED_STRING); // this header means the string is encoded in hexa
  for i:=1 to Length(s) do begin
    b := byte(s[i]);
    Result := Result + b.ToHexString(2);
  end;
end;

function TProperties.DecodeFromHexa(const s: string): string;
var k: integer;
begin
  Result := '';
  if Length(s) = 0 then exit;

  k := 2;
  while k < Length(s) do begin
    Result := Result + chr(HexToByte(s[k]+s[k+1]));
    inc(k, 2);
  end;
end;

procedure TProperties.SetEmpty;
begin
  FProperties := NIL;
end;

function TProperties.SplitFrom(t: TStringList; const aSectionName: string; aSeparator: Char): boolean;
var k: integer;
begin
  k := t.IndexOf(aSectionName);
  if (k = -1) or (k = t.Count-1) then begin
    SetEmpty;
    Result := False;
    exit;
  end;
  Split(t.Strings[k+1], aSeparator);
  Result := True;
end;

function TProperties.Split(const s: string; aSeparator: Char): integer;
begin
  FSeparator := aSeparator;
  FProperties := s.Split([aSeparator]);
  Result := Length(FProperties);
end;

function TProperties.PropertyIsPresent(const aPropertyName: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
end;

function TProperties.CheckProperties(aPropertyNames: TStringArray): boolean;
var i, j: integer;
begin
  for i:=0 to High(aPropertyNames) do
    if not ValueIndexOf(aPropertyNames[i], j) then
    begin
      Result := False;
      exit;
    end;
  Result := True;
end;

function TProperties.StringValueOf(const aPropertyName: string; var Value: string; const aDefault: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then begin
    Value := FProperties[i];
    if (Length(Value) > 0) and (Value[1] = chr(HEADER_ENCODED_STRING)) then
      Value := DecodeFromHexa(Value);
  end else Value := aDefault;
end;

function TProperties.BooleanValueOf(const aPropertyName: string; var Value: boolean; aDefault: boolean): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then Value := FProperties[i] = 'true'
    else Value := aDefault;
end;

function TProperties.IntegerValueOf(const aPropertyName: string; var Value: integer; aDefault: integer): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result and TryStrToInt(FProperties[i], v) then Value := v
    else Value := aDefault;
end;

function TProperties.Int64ValueOf(const aPropertyName: string; var Value: int64; aDefault: int64): boolean;
var i: integer;
  v: int64;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result and TryStrToInt64(FProperties[i], v) then Value := v
    else Value := aDefault;
end;

function TProperties.SingleValueOf(const aPropertyName: string; var Value: single; aDefault: single): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then Value := StringToSingle(FProperties[i])
    else Value := aDefault;
end;

function TProperties.ByteValueOf(const aPropertyName: string; var Value: byte; aDefault: byte): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result and TryStrToInt(FProperties[i], v) then Value := byte(v)
    else Value := aDefault;
end;

function TProperties.WordValueOf(const aPropertyName: string; var Value: word; aDefault: word): boolean;
var i, v: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result and TryStrToInt(FProperties[i], v) then Value := word(v)
    else Value := aDefault;
end;

function TProperties.BGRAPixelValueOf(const aPropertyName: string; var Value: TBGRAPixel; aDefault: TBGRAPixel): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then begin
    try
      Value := HexToBGRAPixel(FProperties[i]);
    except
      Value := aDefault;
    end;
  end else Value := aDefault;
end;

function TProperties.PointValueOf(const aPropertyName: string;
  var Value: TPoint; aDefault: TPoint): boolean;
var A: TStringArray;
  i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then begin
    try
      A := DecodeFromHexa(FProperties[i]).Split([' ']);
      Value.x := A[0].ToInteger;
      Value.y := A[1].ToInteger;
    except
      Value := aDefault;
    end;
  end else Value := aDefault;
end;

function TProperties.PointFValueOf(const aPropertyName: string;
  var Value: TPointF; aDefault: TPointF): boolean;
var A: TStringArray;
  i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then begin
    try
      A := DecodeFromHexa(FProperties[i]).Split([' ']);
      Value.x := StringToSingle(A[0]);
      Value.y := StringToSingle(A[1]);
    except
      Value := aDefault;
    end;
  end else Value := aDefault;
end;

procedure TProperties.Init(aSeparator: char);
begin
  FProperties := NIL;
  FSeparator := aSeparator;
end;

procedure TProperties.Add(const aPropertyName, aValue: string; aEncodeInHexa: boolean);
begin
  if aEncodeInHexa then AddProp(aPropertyName, EncodeInHexa(aValue))
    else AddProp(aPropertyName, aValue);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: boolean);
begin
  AddProp(aPropertyName, BoolToStr(aValue, 'true','false'));
end;

procedure TProperties.Add(const aPropertyName: string; aValue: integer);
begin
  AddProp(aPropertyName, aValue.ToString);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: int64);
begin
  AddProp(aPropertyName, aValue.ToString);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: single);
begin
  AddProp(aPropertyName, FormatFloatWithDot('0.0000', aValue));
end;

procedure TProperties.Add(const aPropertyName: string; aValue: TBGRAPixel);
begin
  AddProp(aPropertyName, BGRAPixelToHex(aValue));
end;

procedure TProperties.Add(const aPropertyName: string; aValue: TPoint);
var s: String;
begin
  s := EncodeInHexa(aValue.X.ToString + ' ' + aValue.Y.ToString);
  AddProp(aPropertyName, s);
end;

procedure TProperties.Add(const aPropertyName: string; aValue: TPointF; aFormat: string);
var s: String;
begin
  s := EncodeInHexa(FormatFloatWithDot(aFormat, aValue.x) + ' ' + FormatFloatWithDot(aFormat, aValue.y));
  AddProp(aPropertyName, s);
end;

function TProperties.ReplaceValue(const aPropertyName, aValue: string): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := aValue;
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: boolean): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := BoolToStr(aValue, 'true','false');
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: integer): boolean;
begin
  Result := ReplaceValue(aPropertyName, int64(aValue));
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: int64): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := aValue.ToString;
end;

function TProperties.ReplaceValue(const aPropertyName: string; aValue: single): boolean;
var i: integer;
begin
  Result := ValueIndexOf(aPropertyName, i);
  if Result then
    FProperties[i] := FormatFloatWithDot('0.000', aValue)
end;

{$endif oglcIMPLEMENTATION}

