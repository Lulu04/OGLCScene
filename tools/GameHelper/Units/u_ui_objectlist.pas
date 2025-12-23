unit u_ui_objectlist;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls,
  OGLCScene, BGRABitmap, BGRABitmapTypes, gvector;

type

TItemWithName = class
  _Name: string;
  procedure InitDefault; virtual; abstract;
  procedure DuplicateTo(aItem: Pointer); virtual; abstract;
  function SaveToString: string; virtual; abstract;
  procedure LoadFromString(const data: string); virtual; abstract;
end;


//TSimpleNameObjectHash<T> = class(TSimpleHash<TNameObjectHashBucket<T>>)

{ TListOfItemWithName }

generic TListOfItemWithName<T: TItemWithName> = class(specialize TVector<T>)
  destructor Destroy; override;
  function AddEmpty: T; virtual; abstract;
  function IndexOf(const aName: string): integer;
  procedure Clear; reintroduce;
  function NameExists(const aName: string): boolean;
  function GetByName(const aName: string): T;
  function GetByIndex(aIndex: integer): T;
  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);
public
  // return True if the item is renamed
  function DoRenameByName(const aOldName: string;
                          out aNewName: string;
                          aSaveBankIfDone: boolean): boolean;
  // return True if the deletion is done
  function DoDeleteByName(const aName: string; aSaveBankIfDone: boolean): boolean;
  // return True if the duplication is done
  function DoDuplicateByName(const aSrcName: string;
                             out aDuplicateName: string;
                             aSaveBankIfDone: boolean): boolean;
  // check if the passed name exists in the list.
  // If yes, ask to the user if s/he want to replace its content, return True if yes
  // If there isn't an item with this name the function return True
  function AskUserToReplaceExistingItem(const aName: string): boolean;

  procedure FillComboBoxWithNames(aCB: TComboBox);

  // ex: font
  function GetItemReadableName: string; virtual; abstract;
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
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;

  // copy only FD, not the name
  procedure DuplicateTo(aItem: Pointer); override;

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
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;

  procedure GetAtlasWithTexturedFont(const aFontDescriptorName, aCaption: string;
                                     out aAtlas: TAtlas;
                                     out aTexturedFont: TTexturedFont;
                                     aFillTexture: TBGRABitmap=NIL);
end;




{ TPanelDescriptorItem }

TPanelDescriptorItem = class(TItemWithName)
  IsModal,
  DarkenBG: boolean;
  DarknessColor: TBGRAPixel;
  ScenarioOnShow,
  ScenarioOnHide: string; // the content of the scenario
  textures: string;
  surfaces: string;
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;
  procedure DuplicateTo(aItem: Pointer); override;
end;

{ TPanelBank }

TPanelBank = class(specialize TListOfItemWithName<TPanelDescriptorItem>)
  function AddEmpty: TPanelDescriptorItem; override;
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
end;


TUIButtonDescriptor = record
  //image: TTextureItem;
  //font: TFontDescriptor;
end;


var
  FontBank: TFontBank;
  PanelBank: TPanelBank;


//TButtonItem

implementation

uses u_utils, u_common, u_project, u_resourcestring, LazFileUtils, Dialogs,
  Controls;

{ TFontDescriptorItem }

procedure TFontDescriptorItem.InitDefault;
begin
  FD.CreateDefault;
end;

function TFontDescriptorItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('&');
  prop.Add('Name', _Name);
  prop.Add('Data', FD.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TFontDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
  s: string;
begin
  s := '';
  prop.Split(data, '&');
  prop.StringValueOf('Name', _Name, '???');
  prop.StringValueOf('Data', s, '');
  FD.LoadFromString(s);
end;

procedure TFontDescriptorItem.DuplicateTo(aItem: Pointer);
begin
  TFontDescriptorItem(aItem).FD := FD;
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
  Result.InitDefault;
  PushBack(Result);
end;

function TFontBank.GetItemReadableName: string;
begin
  Result := sFont;
end;

function TFontBank.GetHeaderFile: string;
begin
  Result := '[FONT BANK]';
end;

function TFontBank.GetSaveFileName: string;
begin
  Result := 'FontBank.oglc';
end;

procedure TFontBank.GetAtlasWithTexturedFont(const aFontDescriptorName, aCaption: string;
  out aAtlas: TAtlas; out aTexturedFont: TTexturedFont;
  aFillTexture: TBGRABitmap);
var item: TFontDescriptorItem;
begin
  item := GetByName(aFontDescriptorName);
  if item <> NIL then begin
    FScene.MakeCurrent;
    aAtlas := FScene.CreateAtlas;
    aAtlas.Spacing := 2;
    aTexturedFont := aAtlas.AddTexturedFont(item.FD, TextToCharset(aCaption), aFillTexture);
    aAtlas.TryToPack;
    aAtlas.Build;
  end else begin
    aAtlas := NIL;
    aTexturedFont := NIL;
    FScene.LogError('TFontBank.GetAtlasWithTexturedFont: aFontDescriptorName "'+aFontDescriptorName+'" not found in FontBank');
  end;
end;

{ TPanelDescriptorItem }

procedure TPanelDescriptorItem.InitDefault;
begin
  IsModal := False;
  DarkenBG := False;
  DarknessColor := BGRA(0,0,0,100);
  ScenarioOnShow := '';
  ScenarioOnHide := '';

  textures := '';
  surfaces := '';
end;

function TPanelDescriptorItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('!');
  prop.Add('Name', _Name);
  prop.Add('IsModal', IsModal);
  prop.Add('DarkenBG', DarkenBG);
  if DarkenBG then prop.Add('DarknessColor', DarknessColor);
  if ScenarioOnShow <> '' then prop.Add('ScenarioOnShow', ScenarioOnShow);
  if ScenarioOnHide <> '' then prop.Add('ScenarioOnHide', ScenarioOnHide);
  if textures <> '' then prop.Add('Textures', textures);
  if surfaces <> '' then prop.Add('Surfaces', surfaces);
  Result := prop.PackedProperty;
end;

procedure TPanelDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
begin
  prop.Split(data, '!');
  prop.StringValueOf('Name', _Name, '???');
  //prop.StringValueOf('BodyShape', BodyShape, '');
  prop.BooleanValueOf('IsModal', IsModal, False);
  prop.BooleanValueOf('DarkenBG', DarkenBG, False);
  prop.BGRAPixelValueOf('DarknessColor', DarknessColor, BGRA(0,0,0,100));
  prop.StringValueOf('ScenarioOnShow', ScenarioOnShow, '');
  prop.StringValueOf('ScenarioOnHide', ScenarioOnHide, '');
  prop.StringValueOf('Textures', textures, '');
  prop.StringValueOf('Surfaces', surfaces, '');
end;

procedure TPanelDescriptorItem.DuplicateTo(aItem: Pointer);
var dst: TPanelDescriptorItem;
begin
  dst := TPanelDescriptorItem(aItem);
  dst.IsModal := IsModal;
  dst.DarkenBG := DarkenBG;
  dst.DarknessColor := DarknessColor;
  dst.ScenarioOnShow := ScenarioOnShow;
  dst.ScenarioOnHide := ScenarioOnHide;
  dst.textures := textures;
  dst.surfaces := surfaces;
end;

{ TPanelBank }

function TPanelBank.AddEmpty: TPanelDescriptorItem;
begin
  Result := TPanelDescriptorItem.Create;
  Result.InitDefault;
  PushBack(Result);
end;

function TPanelBank.GetItemReadableName: string;
begin
  Result := sPanel;
end;

function TPanelBank.GetHeaderFile: string;
begin
  Result := '[PANELBANK]';
end;

function TPanelBank.GetSaveFileName: string;
begin
  Result := 'PanelBank.oglc';
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

function TListOfItemWithName.DoRenameByName(const aOldName: string;
  out aNewName: string; aSaveBankIfDone: boolean): boolean;
var item: TItemWithName;
  newName: string;
begin
  Result := False;

  item := GetByName(aOldName);
  if item = NIL then exit;
  newName := Trim(InputBox('', sEnterTheNewName, aOldName));
  if newName = aOldName then exit;
  if NameExists(newName) then begin
    ShowMessage(Format(sAxxxNamedAlreadyExists, [GetItemReadableName, newName]));
    exit;
  end;

  // rename the item in the bank
  item._Name := newName;
  if aSaveBankIfDone then Save;

  aNewName := newName;
  Result := True;
end;

function TListOfItemWithName.DoDeleteByName(const aName: string;
  aSaveBankIfDone: boolean): boolean;
var item: TItemWithName;
begin
  Result := False;

  item := GetByName(aName);
  if item = NIL then exit;

  if QuestionDlg('',Format(sDeleteThisxxx, [GetItemReadableName]), mtWarning,
                 [mrOk, sDelete, mrCancel, sCancel], 0) = mrCancel then exit;

  // delete in bank
  DeleteByName(aName);
  if aSaveBankIfDone then Save;
  Result := True;
end;

function TListOfItemWithName.DoDuplicateByName(const aSrcName: string; out
  aDuplicateName: string; aSaveBankIfDone: boolean): boolean;
var srcItem, dstItem: TItemWithName;
  k: integer;
begin
  Result := False;

  // retrieve the source item
  srcItem := GetByName(aSrcName);
  if srcItem = NIL then exit;

  // create an unique name for the new item
  k := 0;
  repeat
    inc(k);
    if k < 100 then aDuplicateName := aSrcName+'_'+Format('%.2d', [k])
      else aDuplicateName := aSrcName+'_'+k.ToString;
  until not NameExists(aDuplicateName);

  // add the new item in the bank and save
  dstItem := AddEmpty;
  srcItem.DuplicateTo(dstItem);
  dstItem._Name := aDuplicateName;
  if aSaveBankIfDone then Save;

  Result := True;
end;

function TListOfItemWithName.AskUserToReplaceExistingItem(const aName: string): boolean;
begin
  if NameExists(aName) then
    if QuestionDlg('', Format(sAxxxAlreadyExistsWouldYouLikeToReplaceIt, [GetItemReadableName, aName]),
                   mtWarning, [mrOk, sReplace, mrCancel, sCancel], 0) <> mrOk then exit(False);
  Result := True;
end;

procedure TListOfItemWithName.FillComboBoxWithNames(aCB: TComboBox);
var i: SizeUInt;
begin
  aCB.Clear;
  if Size > 0 then
    for i:=0 to Size-1 do
      aCB.Items.Add(Mutable[i]^._Name);
end;

function TListOfItemWithName.SaveToString: string;
var prop: TProperties;
  i: integer;
begin
  prop.Init('{');
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
  prop.Split(data, '{');
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
      FScene.LogInfo('success, '+Size.ToString+' item(s)', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
     end;
    end;
  finally
    sl.Free;
  end;
end;

end.

