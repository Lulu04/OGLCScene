unit u_ui_objectlist;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics,
  OGLCScene, BGRABitmap, BGRABitmapTypes, gvector;

type

TItemWithName = class
  _Name: string;
  function SaveToString: string; virtual; abstract;
  procedure LoadFromString(const data: string); virtual; abstract;
end;

{ TListOfItemWithName }

generic TListOfItemWithName<T> = class(specialize TVector<TItemWithName>)
  destructor Destroy; override;
  function AddEmpty: T; virtual; abstract;
  function IndexOf(const aName: string): integer;
  procedure Clear; reintroduce;
  function NameExists(const aName: string): boolean;
  function GetByName(const aName: string): T;
  function GetByIndex(aIndex: integer): T;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  // ex: [FONTBANK]
  function GetHeaderFile: string; virtual; abstract;
  // ex: FontBank.oglc
  function GetSaveFileName: string; virtual; abstract;

  function SaveToString: string;
  procedure LoadFromString(const data: string);

  procedure Save;
  procedure Load;
end;


{ TFontDescriptorItem }

TFontDescriptorItem = class(TItemWithName)
  FD: TFontDescriptor;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;

  // copy only FD, not the name
  procedure DuplicateTo(aItem: TFontDescriptorItem);

  function ToTextureFont(aAtlas: TAtlas; aCharSet: string; aFillTexture: TBGRABitmap=NIL): TTexturedFont;

  // return FTexturedFont_<_Name>
  function VariableNameForTexturedFont: string;
  // return fd<_Name>
  function VariableNameForFD: string;
  // return gradient<_Name>
  function VariableNameForFontGradient: string;
  function PascalCodeToDeclareVariables: string;
  function PascalCodeToInitializeVariables: string;
  function PascalCodeToAddTexturedFontToAtlas(aGenerateVariableName: boolean;
                                              const aCharSetName: string): string;
end;

{ TFontBank }

TFontBank = class(specialize TListOfItemWithName<TFontDescriptorItem>)
  function AddEmpty: TFontDescriptorItem; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
end;

TUIShapeStyle = (bsRectangle, bsRoundRect, bsEllipse, bsCustom);
TUIShapeDescriptor = class(TItemWithName)
  BodyShapeData,
  BackGradientData: string;   // '' for no back gradient
  procedure InitDefault;

  function ToUIPanel(aScene: TOGLCScene): TUIPanel;

  // use ; , and space
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;

end;

TUIShapeList = class(specialize TVector<TUIShapeDescriptor>);

TUIButtonDescriptor = record
  shape: TUIShapeDescriptor;
  //image: TTextureItem;
  font: TFontDescriptor;
end;


var
  FontBank: TFontBank;
  UIShapeList: TUIShapeList;


//TButtonItem

implementation

uses u_utils, u_common, u_project, LazFileUtils;

{ TFontDescriptorItem }

function TFontDescriptorItem.SaveToString: string;
begin
  Result := 'Name'+'&'+_Name+'&'+FD.SaveToString;
end;

procedure TFontDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
begin
  prop.Split(data, '&');
  prop.StringValueOf('Name', _Name, '');
  FD.LoadFromString(data);
end;

procedure TFontDescriptorItem.DuplicateTo(aItem: TFontDescriptorItem);
begin
  aItem.FD := FD;
end;

function TFontDescriptorItem.ToTextureFont(aAtlas: TAtlas; aCharSet: string; aFillTexture: TBGRABitmap): TTexturedFont;
begin
  Result := aAtlas.AddTexturedFont(FD, aCharSet, aFillTexture);
end;

function TFontDescriptorItem.VariableNameForTexturedFont: string;
begin
  Result := 'FTexturedFont_'+_Name;
end;

function TFontDescriptorItem.VariableNameForFD: string;
begin
  Result := 'fd'+_Name;
end;

function TFontDescriptorItem.VariableNameForFontGradient: string;
begin
  Result := 'gradient'+_Name;
end;

function TFontDescriptorItem.PascalCodeToDeclareVariables: string;
begin
  Result := '  fd'+_Name+': TFontDescriptor;';
  if FD.UseGradient then
    Result := Result + #10'  '+VariableNameForFontGradient+': TFontGradient';
end;

function TFontDescriptorItem.PascalCodeToInitializeVariables: string;
var s: string;
begin
  Result := '';
  if FD.UseGradient then begin
    Result := '  '+VariableNameForFontGradient+'.Create('+CodeGen.BGRAToPascal(FD.Gradient.Color1)+', '+
              CodeGen.BGRAToPascal(FD.Gradient.Color2)+', '+
              Codegen.BGRAGradientTypeToPascal(FD.Gradient.GradientType)+', '+
              CodeGen.PointFToPascal(FD.Gradient.Origin)+', '+
              CodeGen.PointFToPascal(FD.Gradient.D1)+', '+
              CodeGen.BooleanToPascal(FD.Gradient.GammaColorCorrection)+', '+
              CodeGen.BooleanToPascal(FD.Gradient.Sinus)+');'#10;
  end;

  if FD.UseGradient then
    s := '  fd'+_Name+'.Create('''+ FD.FontName+''', '+ FD.FontHeight.ToString+', '+
          CodeGen.FontStyleToPascal(FD.Style)+', '+VariableNameForFontGradient
  else
    s := '  fd'+_Name+'.Create('''+ FD.FontName+''', '+ FD.FontHeight.ToString+', '+
          CodeGen.FontStyleToPascal(FD.Style)+', '+CodeGen.BGRAToPascal(FD.FontColor);

  if FD.UseOutLine then
    s := s + ', '#10+CodeGen.BGRAToPascal(FD.OutLineColor)+', '+FormatFloatWithDot('0.0', FD.OutLineWidth);

  if FD.UseShadow then
    s := s + ', '#10+CodeGen.BGRAToPascal(FD.ShadowColor)+', '+
         FD.ShadowOffsetX.ToString+', '+ FD.ShadowOffsetY.ToString+', '+
         FD.ShadowRadius.ToString;

  Result := Result + s + ');';
end;

function TFontDescriptorItem.PascalCodeToAddTexturedFontToAtlas(
  aGenerateVariableName: boolean; const aCharSetName: string): string;
begin
  // FVar := aAtlas .AddTexturedFont(FScene, aFD: TFontDescriptor, aCharSet: string, aFillTexture);
  if aGenerateVariableName then Result := VariableNameForTexturedFont+' := '
    else Result := '';
  Result := Result + 'aAtlas.AddTexturedFont(FScene, '+VariableNameForFD+', '+aCharSetName+', NIL);';
end;

{ TFontBank }

function TFontBank.AddEmpty: TFontDescriptorItem;
begin
  Result := TFontDescriptorItem.Create;
  Result.FD.CreateDefault;
  PushBack(Result);
end;

function TFontBank.GetHeaderFile: string;
begin
  Result := '[FONT BANK]';
end;

function TFontBank.GetSaveFileName: string;
begin
  Result := 'FontBank.oglc';
end;

{ TListOfItemWithName }

destructor TListOfItemWithName.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TListOfItemWithName.IndexOf(const aName: string): integer;
var i: integer;
begin
  Result := -1;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^._Name = aName then exit(i);
end;

procedure TListOfItemWithName.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do
      Mutable[i]^.Free;
  inherited Clear;
end;

function TListOfItemWithName.NameExists(const aName: string): boolean;
var i: Integer;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if GetByIndex(i)._Name = aName then exit(True);
end;

function TListOfItemWithName.GetByName(const aName: string): T;
var i: integer;
begin
  i := IndexOf(aName);
  if i = -1 then Result := NIL
    else Result := T(Mutable[i]^);
end;

function TListOfItemWithName.GetByIndex(aIndex: integer): T;
begin
  Result := T(Mutable[aIndex]^);
end;

procedure TListOfItemWithName.DeleteByIndex(aIndex: integer);
begin
  Mutable[aIndex]^.Free;
  Erase(aIndex);
end;

procedure TListOfItemWithName.DeleteByName(const aName: string);
var i: integer;
begin
  i := IndexOf(aName);
  if i <> -1 then DeleteByIndex(i);
end;

function TListOfItemWithName.SaveToString: string;
var prop: TProperties;
  i: integer;
begin
  prop.Init('|');
  prop.Add('Count', integer(Size).ToString);
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Item'+i.ToString, Mutable[i]^.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TListOfItemWithName.LoadFromString(const data: string);
var prop: TProperties;
  c, i: integer;
  s: string;
  o: T;
begin
  Clear;
  prop.Split(data, '|');
  c := 0;
  s := '';
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;
  for i:=0 to c-1 do begin
    if prop.StringValueOf('Item'+i.ToString, s, '') then begin
      o := AddEmpty;
      TItemWithName(o).LoadFromString(s);
    end;
  end;
end;

procedure TListOfItemWithName.Save;
var sl: TStringList;
 filename: string;
begin
  filename := Project.Config.TargetLazarusProject.GetFolderGameHelperFiles + GetSaveFileName;
  FScene.LogInfo('Saving '+GetSaveFileName+' to '+filename, 1);
  sl := TStringList.Create;
  try
    try
      sl.Add(GetHeaderFile);
      sl.Add(SaveToString);
      sl.SaveToFile(filename);
      FScene.LogInfo('success', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
     end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TListOfItemWithName.Load;
var sl: TStringList;
  filename: string;
  k: LongInt;
begin
  Clear;
  filename := Project.Config.TargetLazarusProject.GetFolderGameHelperFiles + GetSaveFileName;
  if not FileExistsUTF8(filename) then begin
    FScene.LogInfo(GetSaveFileName+' not found', 1);
    exit;
  end;

  FScene.LogInfo('Found '+GetSaveFileName+', loading...', 1);
  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(filename);
      k := sl.IndexOf(GetHeaderFile);
      if k = -1 then begin
        FScene.LogError('Header '+GetHeaderFile+' not found in file !', 2);
        exit;
      end;
      if k = sl.Count-1 then begin
        FScene.LogError('no data found after the header in file !', 2);
        exit;
      end;
      LoadFromString(sl.Strings[k+1]);
      FScene.LogInfo('success', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
     end;
    end;
  finally
    sl.Free;
  end;
end;

{ TUIShapeDescriptor }

procedure TUIShapeDescriptor.InitDefault;
var o: TBodyShape;
begin
  Self := Default(TUIShapeDescriptor);
  o.InitDefault(NIL);
  BodyShapeData := o.SaveToString;
  BackGradientData := '';   // '' for no gradient
end;

function TUIShapeDescriptor.ToUIPanel(aScene: TOGLCScene): TUIPanel;
begin
  Result := TUIPanel.Create(aScene);
  Result.BodyShape.LoadFromString(BodyShapeData);

  if BackGradientData <> '' then
    Result.BackGradient.LoadGradientDataFromString(BackGradientData);
end;

function TUIShapeDescriptor.SaveToString: string;
var prop: TProperties;
begin
  prop.Init(';');
  prop.Add('Name', _Name);
  prop.Add('BodyShapeData', BodyShapeData);
  if BackGradientData <> '' then
    prop.Add('GradientData', BackGradientData);
  Result := prop.PackedProperty;
end;

procedure TUIShapeDescriptor.LoadFromString(const data: string);
var prop: TProperties;
begin
  InitDefault;
  prop.Split(data, ';');
  prop.StringValueOf('Name', _Name, 'noname');
  prop.StringValueOf('BodyShapeData', BodyShapeData, BodyShapeData);
  prop.StringValueOf('GradientData', BackGradientData, '');
end;

end.

