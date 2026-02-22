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
  UseCharsetPreset: boolean;
  CharsetPresets: TStringArray; // the list of used charsets
  TextToCharSet: string; // the text from which construct the charset
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;

  // copy only FD, not the name
  procedure DuplicateTo(aItem: Pointer); override;

  // return texturedfont<_Name>
  function VariableNameForTexturedFont: string;
  // return  charset<_Name>
  function VariableNameForCharset: string;
  // return fontdescriptor<_Name>
  function VariableNameForFD: string;
  // return gradient<_Name>
  function VariableNameForFontGradient: string;

  // declare the fd variable and if needed the gradient variable (without 'var')
  function PascalCodeToInitializeGradientVariable: string;
  function PascalCodeToInitializeFDVariable: string;
  function PascalCodeToInitializeCharsetVariable: string;
  function PascalCodeToAddTexturedFontToAtlas(aGenerateVariableName: boolean): string;
end;

{ TFontBank }

TFontBank = class(specialize TListOfItemWithName<TFontDescriptorItem>)
  function AddEmpty: TFontDescriptorItem; override;
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
  procedure Clear; reintroduce;
  function GetFirstAvailableFontName: string;

  procedure GetAtlasWithTexturedFont(const aFontDescriptorName, aCaption: string;
                                     out aAtlas: TAtlas;
                                     out aTexturedFont: TTexturedFont;
                                     aFillTexture: TBGRABitmap=NIL);
end;




{ TPanelDescriptorItem }

TPanelDescriptorItem = class(TItemWithName)
  ChildClippingEnabled,
  IsModal,
  DarkenBG: boolean;
  DarknessColor: TBGRAPixel;
  OnShowScenarioName,
  OnHideScenarioName: string; // the content of the scenario
  textures: string;
  surfaces: string;
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;
  procedure DuplicateTo(aItem: Pointer); override;

  // i.e. for a panel named 'Pause' return 'u_panel_pause'
  function GetUnitName: string;
  function GetUnitFullFilename: string;
  procedure ExportPanelToPascalUnit;
end;

{ TPanelBank }

TPanelBank = class(specialize TListOfItemWithName<TPanelDescriptorItem>)
  function AddEmpty: TPanelDescriptorItem; override;
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
end;


{ TPathDescriptorItem }

TPathDescriptorItem = class(TItemWithName)
  PathData: string;
  UseSpline: boolean;
  SplineStyle: TSplineStyle;
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;
  procedure DuplicateTo(aItem: Pointer); override;

  // aItemData is the string returned by SaveToString
  function ItemDataToExpandedPath(const aItemData: string; aWidth, aHeight: integer): TOGLCPath;

  // return oglcpath<_Name>
  function VariableName: string;
  // return oglcpath<_Name> := '<PathData>';
  function PascalCodeToInitializeVariable: string;
end;

{ TPathBank }

TPathBank = class(specialize TListOfItemWithName<TPathDescriptorItem>)
  function AddEmpty: TPathDescriptorItem; override;
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
end;


{ TScenarioDescriptorItem }

TScenarioDescriptorItem = class(TItemWithName)
  ScenarioData: string;
  procedure InitDefault; override;
  function SaveToString: string; override;
  procedure LoadFromString(const data: string); override;
  procedure DuplicateTo(aItem: Pointer); override;

  // return scenario<_Name>
  function VariableName: string;
  // return scenario<_Name> := '<ScenarioData>';
  function PascalCodeToInitializeVariable: string;
end;

{ TPathBank }

{ TScenarioBank }

TScenarioBank = class(specialize TListOfItemWithName<TScenarioDescriptorItem>)
  function AddEmpty: TScenarioDescriptorItem; override;
  function GetItemReadableName: string; override;
  function GetHeaderFile: string; override;
  function GetSaveFileName: string; override;
end;


var
  FontBank: TFontBank;
  PanelBank: TPanelBank;
  PathBank: TPathBank;
  ScenarioBank: TScenarioBank;

//TButtonItem

implementation

uses u_utils, u_common, u_project, u_resourcestring, u_texture_list,
  u_surface_list, LazFileUtils, Dialogs, Controls;

{ TFontDescriptorItem }

procedure TFontDescriptorItem.InitDefault;
begin
  FD.Create(FontBank.GetFirstAvailableFontName, 12, [], BGRA(255,255,255));

  UseCharsetPreset := True;
  CharsetPresets := ['NUMBER', 'ASCII_SPACE', 'ASCII_LETTER'];
  TextToCharSet := '';
end;

function TFontDescriptorItem.SaveToString: string;
var prop: TProperties;
  s: string;
begin
  s := '';
  prop.Init('&');
  prop.Add('Name', _Name);
  prop.Add('Data', FD.SaveToString);
  prop.Add('UseCharset', UseCharsetPreset);
  if Length(CharsetPresets) > 0 then begin
    s := s.Join(' ', CharsetPresets);
    prop.Add('Charsets', s);
  end;
  if TextToCharSet <> '' then prop.Add('TextToCharSet', TextToCharSet, True);

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
  prop.BooleanValueOf('UseCharset', UseCharsetPreset, True);
  prop.StringValueOf('Charsets', s, '');
  CharsetPresets := s.Split([' ']);
  prop.StringValueOf('TextToCharSet', TextToCharSet, '');
end;

procedure TFontDescriptorItem.DuplicateTo(aItem: Pointer);
begin
  FD.DuplicateTo(@TFontDescriptorItem(aItem).FD);
  TFontDescriptorItem(aItem).UseCharsetPreset := UseCharsetPreset;
  TFontDescriptorItem(aItem).CharsetPresets := Copy(CharsetPresets, 0, Length(CharsetPresets));
  TFontDescriptorItem(aItem).TextToCharSet := Copy(TextToCharSet, 1, Length(TextToCharSet));
end;

function TFontDescriptorItem.VariableNameForTexturedFont: string;
begin
  Result := 'texturedfont'+_Name;
end;

function TFontDescriptorItem.VariableNameForCharset: string;
begin
  Result := 'charset'+_Name;
end;

function TFontDescriptorItem.VariableNameForFD: string;
begin
  Result := 'fontdescriptor'+_Name;
end;

function TFontDescriptorItem.VariableNameForFontGradient: string;
begin
  Result := 'gradient'+_Name;
end;

function TFontDescriptorItem.PascalCodeToInitializeGradientVariable: string;
begin
  Result := '';
  if FD.UseGradient then
    Result := '  '+VariableNameForFontGradient+'.Create('+CodeGen.BGRAToPascal(FD.Gradient.Color1)+', '+
              CodeGen.BGRAToPascal(FD.Gradient.Color2)+', '+
              Codegen.BGRAGradientTypeToPascal(FD.Gradient.GradientType)+', '+
              CodeGen.PointFToPascal(FD.Gradient.Origin)+', '+
              CodeGen.PointFToPascal(FD.Gradient.D1)+', '+
              CodeGen.BooleanToPascal(FD.Gradient.GammaColorCorrection)+', '+
              CodeGen.BooleanToPascal(FD.Gradient.Sinus)+');';
end;

function TFontDescriptorItem.PascalCodeToInitializeFDVariable: string;
begin
  Result := '  '+VariableNameForFD+'.Create('''+ FD.FontName+''', '+
                 //FD.FontHeight.ToString+', '+
                 'Round('+FD.FontHeight.ToString+'/SCREEN_HEIGHT_AT_DESIGN_TIME*FScene.Height), '+
                 CodeGen.FontStyleToPascal(FD.Style)+', ';

  if FD.UseGradient then Result := Result + VariableNameForFontGradient
    else Result := Result + CodeGen.BGRAToPascal(FD.FontColor);

  if FD.UseOutLine then
    Result := Result + ', '+CodeGen.BGRAToPascal(FD.OutLineColor)+', '+FormatFloatWithDot('0.0', FD.OutLineWidth);

  if FD.UseShadow then
    Result := Result + ', '+CodeGen.BGRAToPascal(FD.ShadowColor)+', '+
         FD.ShadowOffsetX.ToString+', '+ FD.ShadowOffsetY.ToString+', '+
         FD.ShadowRadius.ToString;

  Result := Result + ');';
end;

function TFontDescriptorItem.PascalCodeToInitializeCharsetVariable: string;
var i: integer;
begin
  Result := '  '+VariableNameForCharset+' := ';
  if UseCharsetPreset then begin
    for i:=0 to High(CharsetPresets) do begin
      Result := Result + 'FScene.Charsets.'+CharsetPresets[i];
      if i < High(CharsetPresets) then Result := Result + '+';
    end;
    Result := Result+';';
  end else begin
    //Result := Result + 'FScene.Charsets.TextToCharset('''+TextToCharSet+''');';
    Result := Result + CodeGen.TextToPascal(FScene.Charsets.TextToCharset(TextToCharSet), TRUE)+';';
  end;
end;

function TFontDescriptorItem.PascalCodeToAddTexturedFontToAtlas(aGenerateVariableName: boolean): string;
var charset: string;
  i: integer;
begin
  // FVar := aAtlas .AddTexturedFont(FScene, aFD: TFontDescriptor, aCharSet: string, aFillTexture);
  if aGenerateVariableName then Result := '  '+VariableNameForTexturedFont+' := '
    else Result := '  ';

  if UseCharsetPreset and (Length(CharsetPresets) > 0) then begin
    charset := '';
    for i:=0 to High(CharsetPresets) do begin
      charset := charset + 'FScene.Charsets.'+CharsetPresets[i];
      if i < High(CharsetPresets) then charset := charset + '+';
    end;
  end
  else if TextToCharSet <> '' then charset := 'FScene.Charsets.TextToCharset('''+TextToCharSet+''')'
  else charset := 'FScene.Charsets.NUMBER+FScene.Charsets.ASCII_SPACE+FScene.Charsets.ASCII_LETTER';

  Result := Result + 'aAtlas.AddTexturedFont(FScene, '+VariableNameForFD+', '+charset+', NIL);';
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

procedure TFontBank.Clear;
begin
  inherited Clear;
  FScene.FontManager.ClearProjectFonts;
end;

function TFontBank.GetFirstAvailableFontName: string;
begin
  if FScene.FontManager.ProjectFontCount > 0 then Result := FScene.FontManager.ProjectFontName[0]
    else if FScene.FontManager.SystemFontCount > 0 then Result := FScene.FontManager.SystemFontName[0]
      else Result := '';
end;

procedure TFontBank.GetAtlasWithTexturedFont(const aFontDescriptorName, aCaption: string;
  out aAtlas: TAtlas; out aTexturedFont: TTexturedFont;
  aFillTexture: TBGRABitmap);
var item: TFontDescriptorItem;
  s: string;
begin
  item := GetByName(aFontDescriptorName);
  if item <> NIL then begin
    FScene.MakeCurrent;
    aAtlas := FScene.CreateAtlas;
    aAtlas.Spacing := 2;
    if aCaption = '' then s := ' ' else s := aCaption;
    aTexturedFont := aAtlas.AddTexturedFont(item.FD, TextToCharset(s), aFillTexture);
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
  ChildClippingEnabled := True;
  IsModal := False;
  DarkenBG := False;
  DarknessColor := BGRA(0,0,0,100);
  OnShowScenarioName := '';
  OnHideScenarioName := '';

  textures := '';
  surfaces := '';
end;

function TPanelDescriptorItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('!');
  prop.Add('Name', _Name);
  prop.Add('Clipping', ChildClippingEnabled);
  prop.Add('IsModal', IsModal);
  prop.Add('DarkenBG', DarkenBG);
  if DarkenBG then prop.Add('DarknessColor', DarknessColor);
  if OnShowScenarioName <> '' then prop.Add('OnShowScenarioName', OnShowScenarioName);
  if OnHideScenarioName <> '' then prop.Add('OnHideScenarioName', OnHideScenarioName);
  if textures <> '' then prop.Add('Textures', textures);
  if surfaces <> '' then prop.Add('Surfaces', surfaces);
  Result := prop.PackedProperty;
end;

procedure TPanelDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
begin
  prop.Split(data, '!');
  prop.StringValueOf('Name', _Name, '???');
  prop.BooleanValueOf('Clipping', ChildClippingEnabled, True);
  //prop.StringValueOf('BodyShape', BodyShape, '');
  prop.BooleanValueOf('IsModal', IsModal, False);
  prop.BooleanValueOf('DarkenBG', DarkenBG, False);
  prop.BGRAPixelValueOf('DarknessColor', DarknessColor, BGRA(0,0,0,100));
  prop.StringValueOf('OnShowScenarioName', OnShowScenarioName, '');
  prop.StringValueOf('OnHideScenarioName', OnHideScenarioName, '');
  prop.StringValueOf('Textures', textures, '');
  prop.StringValueOf('Surfaces', surfaces, '');
end;

procedure TPanelDescriptorItem.DuplicateTo(aItem: Pointer);
var dst: TPanelDescriptorItem;
begin
  dst := TPanelDescriptorItem(aItem);
  dst.ChildClippingEnabled := ChildClippingEnabled;
  dst.IsModal := IsModal;
  dst.DarkenBG := DarkenBG;
  dst.DarknessColor := DarknessColor;
  dst.OnShowScenarioName := Copy(OnShowScenarioName, 1, Length(OnShowScenarioName));
  dst.OnHideScenarioName := Copy(OnHideScenarioName, 1, Length(OnHideScenarioName));
  dst.textures := Copy(textures, 1, Length(textures));
  dst.surfaces := Copy(surfaces, 1, Length(surfaces));
end;

function TPanelDescriptorItem.GetUnitName: string;
begin
  Result := 'u_panel_' + LowerCase(_Name);
end;

function TPanelDescriptorItem.GetUnitFullFilename: string;
begin
  Result := Project.Config.TargetLazarusProject.GetFolderUnitsSprites + GetUnitName + '.pas';
end;

procedure TPanelDescriptorItem.ExportPanelToPascalUnit;
var t: TStringList;
  textureList: TTextureList;
  surfaceList: TSurfaceList;
  i: integer;
  s, sx, sy: string;
  rootItem, current, _parent: PSurfaceDescriptor;
  nameUnit, nameClass, panelClassType: string;
  fontItem: TFontDescriptorItem;
begin
  if (textures = '') or (surfaces = '') then exit;
  // create each needed instance
  textureList := TTextureList.Create;
  surfaceList := TSurfaceList.Create;
  surfaceList.Textures := textureList;
  t := TStringList.Create;

  try
    textureList.LoadFromString(textures);
    surfaceList.LoadFromString(surfaces, False);

    nameClass := 'TPanel'+_Name;
    nameUnit := GetUnitName;
    rootItem := surfaceList.GetRootItem;
    if IsModal then begin
      panelClassType := 'TUIModalPanel';
      rootItem^.classtype := TUIModalPanel;
    end else begin
      panelClassType := 'TUIPanelWithEffects';
      rootItem^.classtype := TUIPanelWithEffects;
    end;

    if DarkenBG then rootItem^.BGDarkenColor := DarknessColor
      else rootItem^.BGDarkenColor := BGRA(0,0,0,0);


    // header
    t.Add('{');
    CodeGen.AddFileGeneratedByGameHelper(t);
    t.Add('');
    t.AddText('  DON''T EDIT THIS UNIT because it may be overwritten by the tool -> your code will be lost !'#10+
              '  Instead, in another unit add "'+nameUnit+'" in the uses clause'#10+
              '  and add your code in a class inherited from '+nameClass+'.');
    t.Add('');
    t.Add('  Usage:');
    if surfaceList.UseTexture then
      t.Add('    - add the panel textures to your atlas with class method '+nameClass+'.LoadTexture().');
    t.AddText('    - create an instance of the panel'#10+
              '      i.e. F'+_Name+' := TCustom'+_Name+'.Create(aLAYER_INDEX);'#10+
              '    or if the panel is a child of another surface:'#10+
              '           F'+_Name+' := TCustom'+_Name+'.Create(-1);'#10+
              '           F'+_Name+'.SetChildOf(parentSurface, zordervalue);'#10+
              '    - initialize the panel coordinates'#10+
              '           F'+_Name+'.Setcoordinates(...);'#10+
              '}');
    t.Add('');
    CodeGen.AddInterface(t, nameUnit);

    // class declaration
    t.Add(nameClass+' = class('+panelClassType+')');

    // textures declaration
    if textureList.Size > 0 then begin
      t.AddText('private');
      s := '  class var ';
      for i:=0 to textureList.Size-1 do begin
        s := s + textureList.Mutable[i]^.name;
        if i < textureList.Size-1 then s := s +', ';
        if (i mod 4 = 0) and (i > 0) and (i < textureList.Size-1) then s := s + #10'  ';
      end;
      s := s + ': PTexture;';
      t.AddText(s);
    end;

    // child variables declaration
    t.Add('public');
    s := '';
    for i:=0 to surfaceList.Size-1 do begin
      current := surfaceList.Mutable[i];
      if current = rootItem then continue;
      s := s + '  '+current^.name+': '+current^.classtype.ClassName+';';
      if i < surfaceList.Size-1 then s := s +#10;
    end;
    t.AddText(s);

    // methods declaration
    if textureList.Size > 0 then
      CodeGen.AddDeclarationOfClassLoadTexture(t);
    //CodeGen.AddDeclarationOfSurfaceConstructor(t);
    t.Add('  constructor Create;');

    t.Add('end;');   // end of class declaration
    t.Add('');
    // implementation
    CodeGen.AddImplementation(t);
    t.Add('{ '+nameClass+' }');
    t.Add('');

    // procedure load Texture
    if textureList.Size > 0 then begin
      t.AddText('class procedure '+nameClass+'.LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
                'var texFolder: string;'#10+
                'begin'#10+
                '  texFolder := u_common.TexturesFolder;');
      for i:=0 to textureList.Size-1 do
        textureList.Mutable[i]^.PascalCodeToRetrieveOrAddTextureToAtlas(t, '  ', True);
        //t.Add('  '+textureList.Mutable[i]^.PascalCodeToAddTextureToAtlas(True));
      t.AddText('end;'#10+#10);
    end;

    // constructor
    t.AddText('constructor '+nameClass+'.Create;'#10+
              'begin'#10);
    case panelClassType of
      'TUIPanel', 'TUIPanelWithEffects': begin
        t.Add('  inherited Create(FScene);');
        if not ChildClippingEnabled then
          t.Add('  ChildClippingEnabled := False;');
      end;
      'TUIModalPanel': begin
        if DarkenBG then t.Add('  inherited Create(FScene, '+DarknessColor.alpha.ToString+');')
          else t.Add('  inherited Create(FScene, 0);');
        if not ChildClippingEnabled then
          t.Add('  ChildClippingEnabled := False;');
      end;
      'TUILabel': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TUIImage': begin
        if rootItem^.textureName = '' then s := 'NIL'
          else s := rootItem^.textureName;
        t.Add('  inherited Create(FScene, '+s+', '+rootItem^.width.ToString+', '+rootItem^.height.ToString+');');
      end;
      'TUIButton': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TUICheck': begin
        fontItem := FontBank.GetByName(rootItem^.FontDescriptorName);
        if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
          else s := 'NIL';
        t.Add('  inherited Create(FScene, '+CodeGen.TextToPascal(rootItem^.Caption)+', '+s+');');
      end;
      'TUIRadio': begin
        fontItem := FontBank.GetByName(rootItem^.FontDescriptorName);
        if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
          else s := 'NIL';
        t.Add('  inherited Create(FScene, '+CodeGen.TextToPascal(rootItem^.Caption)+', '+s+');');
      end;
      'TUIScrollBar': begin
        t.Add('  inherited Create(FScene, '+CodeGen.UIOrientationToPascal(rootItem^.uiOrientation)+');');
      end;
      'TUIProgressBar': begin
        t.Add('  inherited Create(FScene, '+CodeGen.UIOrientationToPascal(rootItem^.uiOrientation)+');');
      end;
      'TUIScrollBox': begin
        t.Add('  inherited Create(FScene, '+CodeGen.BooleanToPascal(rootItem^.scrollboxUseVScrollbar)+', '+
                                            CodeGen.BooleanToPascal(rootItem^.scrollboxUseHScrollbar)+');');
      end;
      'TUIListBox': begin
        fontItem := FontBank.GetByName(rootItem^.FontDescriptorName);
        if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
          else s := 'NIL';
        t.Add('  inherited Create(FScene, '+s+');');
      end;
      'TUITextArea': begin
        t.Add('  inherited Create(FScene);');
      end

      else raise exception.create('forgot to implement '+panelClassType);
    end;
    {t.AddText('  if aLayerIndex <> -1 then'#10+
              '    FScene.Add(Self, aLayerIndex);'#10);}
    if rootItem <> NIL then begin
      if (rootItem^.x <> 0.0) or (rootItem^.y <> 0.0) then
        t.Add('  SetCoordinate(ScaleWF('+FormatFloatWithDot('0.00', rootItem^.x)+'), ScaleHF('+
                                 FormatFloatWithDot('0.00', rootItem^.y)+'));');

      CodeGen.CommonPropertiesToPascalCode(t, rootItem, '  ');
      CodeGen.ExtraPropertiesToPascalCode(t, rootItem, '  ');
    end;
    t.Add('');
    // creating childs
    for i:=0 to surfaceList.Size-1 do begin
      current := surfaceList.Mutable[i];
      // copy values to variable for easier access
      //current^.DuplicateValuesToTemporaryVariables;

      if (rootItem <> NIL) and (current = rootItem) then continue;

      case current^.classtype.ClassName of
        'TUIPanel': t.Add('  '+current^.name+' := TUIPanel.Create(FScene);');
        'TUILabel': t.Add('  '+current^.name+' := TUILabel.Create(FScene);');
        'TUIImage': begin
          if current^.textureName = '' then s := 'NIL'
            else s := current^.textureName;
          t.Add('  '+current^.name+' := TUIImage.Create(FScene, '+s+', '+current^.width.ToString+', '+current^.height.ToString+');');
        end;
        'TUIButton': begin
          t.Add('  '+current^.name+' := TUIButton.Create(FScene);');
        end;
        'TUICheck': begin
          fontItem := FontBank.GetByName(current^.FontDescriptorName);
          if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
            else s := 'NIL';
          t.Add('  '+current^.name+' := TUICheck.Create(FScene, '+CodeGen.TextToPascal(current^.Caption)+', '+s+');');
        end;
        'TUIRadio': begin
          fontItem := FontBank.GetByName(current^.FontDescriptorName);
          if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
            else s := 'NIL';
          t.Add('  '+current^.name+' := TUIRadio.Create(FScene, '+CodeGen.TextToPascal(current^.Caption)+', '+s+');');
        end;
        'TUIScrollBar': begin
          t.Add('  '+current^.name+' := TUIScrollBar.Create(FScene, '+CodeGen.UIOrientationToPascal(current^.uiOrientation)+');');
        end;
        'TUIProgressBar': begin
          t.Add('  '+current^.name+' := TUIProgressBar.Create(FScene, '+CodeGen.UIOrientationToPascal(current^.uiOrientation)+');');
        end;
        'TUIScrollBox': begin
          t.Add('  '+current^.name+' := TUIScrollBox.Create(FScene, '+CodeGen.BooleanToPascal(current^.scrollboxUseVScrollbar)+', '+
                                              CodeGen.BooleanToPascal(current^.scrollboxUseHScrollbar)+');');
        end;
        'TUIListBox': begin
          fontItem := FontBank.GetByName(current^.FontDescriptorName);
          if fontItem <> NIL then s := fontItem.VariableNameForTexturedFont
            else s := 'NIL';
          t.Add('  '+current^.name+' := TUIListBox.Create(FScene, '+s+');');
        end;
        'TUITextArea': begin
          t.Add('  '+current^.name+' := TUITextArea.Create(FScene);');
        end
        else raise exception.create('forgot to implement '+current^.classtype.ClassName);
      end;

      // set child dependency and values
      _parent := surfaceList.GetItemByID(current^.parentID);
      if rootItem = NIL then begin
        if current^.parentID = -1 then s := 'Self'
          else s := _parent^.name;
      end else begin
        if current^.parentID = rootItem^.id then s := 'Self'
          else s := _parent^.name;
      end;
      t.AddText('  with '+current^.name+' do begin'#10+
                '    SetChildOf('+s+', '+current^.zOrder.ToString+');');

      // to keep right proportion, coordinates must be relative to
      // the width and height of the parent or current
      if (current^.x <> 0) or (current^.y <> 0) then begin
        if _parent^.classtype = TSpriteContainer then begin
          //sx := CodeGen.FormatXCoorRelativeToParentWidth(current^.x/current^.width, current^.name);
          //sy := CodeGen.FormatYCoorRelativeToParentHeight(current^.y/current^.height, current^.name);
          sx := 'ScaleWF('+FormatFloatWithDot('0.000', current^.x)+')';
          sy := 'ScaleHF('+FormatFloatWithDot('0.000', current^.y)+')';
        end else begin
          if _parent^.IsRoot then begin
            sx := CodeGen.FormatXCoorRelativeToParentWidth(current^.x/_parent^.width, 'Self');
            sy := CodeGen.FormatYCoorRelativeToParentHeight(current^.y/_parent^.height, 'Self');
          end else begin
            sx := CodeGen.FormatXCoorRelativeToParentWidth(current^.x/_parent^.width, _parent^.name);
            sy := CodeGen.FormatYCoorRelativeToParentHeight(current^.y/_parent^.height, _parent^.name);
          end;
        end;
        t.Add('    SetCoordinate('+sx+', '+sy+');');
      end;

      CodeGen.CommonPropertiesToPascalCode(t, current, '    ');
      CodeGen.ExtraPropertiesToPascalCode(t, current, '    ');
      t.Add('  end;');
      if i < surfaceList.Size-1 then t.Add('');
    end;//for

    t.Add('end;');
    t.Add('');

    t.Add('end.');

    t.SaveToFile(GetUnitFullFilename);
  finally
    textureList.Clear;
    textureList.Free;
    surfaceList.Free;
    t.Free;
  end;
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

{ TPathDescriptorItem }

procedure TPathDescriptorItem.InitDefault;
begin
  PathData := '';
  UseSpline := True;
  SplineStyle := ssCrossingWithEnds;
end;

function TPathDescriptorItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('!');
  prop.Add('Name', _Name);
  prop.Add('UseSpline', UseSpline);
  if UseSpline then prop.Add('SplineStyle', ord(SplineStyle));
  prop.Add('Path', PathData);
  Result := prop.PackedProperty;
end;

procedure TPathDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
  v: integer;
begin
  v := 0;
  prop.Split(data, '!');
  prop.StringValueOf('Name', _Name, '???');
  prop.BooleanValueOf('UseSpline', UseSpline, True);
  prop.IntegerValueOf('SplineStyle', v, Ord(ssCrossingWithEnds));
  SplineStyle := TSplineStyle(v);
  prop.StringValueOf('Path', PathData, '');
end;

procedure TPathDescriptorItem.DuplicateTo(aItem: Pointer);
begin
  TPathDescriptorItem(aItem).UseSpline := UseSpline;
  TPathDescriptorItem(aItem).SplineStyle := SplineStyle;
  TPathDescriptorItem(aItem).PathData := Copy(PathData, 1, Length(PathData));
end;

function TPathDescriptorItem.ItemDataToExpandedPath(const aItemData: string;
  aWidth, aHeight: integer): TOGLCPath;
begin
  LoadFromString(aItemData);

  Result := NIL;
  Result.LoadFromString(PathData);
  Result.ExpandNormalizedTo(aWidth, aHeight);
  if UseSpline then
    Result.ConvertToSpline(SplineStyle);
end;

function TPathDescriptorItem.VariableName: string;
begin
  Result := 'oglcpath'+_Name;
end;

function TPathDescriptorItem.PascalCodeToInitializeVariable: string;
var s: string;
begin
  WriteStr(s, SplineStyle);
  Result := '  '+VariableName+'.LoadNormalizedFromStringAndExpand('''+PathData+''', FScene.Width, FScene.Height, '+CodeGen.BooleanToPascal(UseSpline)+', '+s+');';
end;

{ TPathBank }

function TPathBank.AddEmpty: TPathDescriptorItem;
begin
  Result := TPathDescriptorItem.Create;
  Result.InitDefault;
  PushBack(Result);
end;

function TPathBank.GetItemReadableName: string;
begin
  Result := sPath;
end;

function TPathBank.GetHeaderFile: string;
begin
  Result := '[PATHBANK]';
end;

function TPathBank.GetSaveFileName: string;
begin
  Result := 'PathBank.oglc';
end;

{ TScenarioDescriptorItem }

procedure TScenarioDescriptorItem.InitDefault;
begin
  ScenarioData := '';
end;

function TScenarioDescriptorItem.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('!');
  prop.Add('Name', _Name);
  prop.Add('Data', ScenarioData, True);
  Result := prop.PackedProperty;
end;

procedure TScenarioDescriptorItem.LoadFromString(const data: string);
var prop: TProperties;
begin
  prop.Split(data, '!');
  prop.StringValueOf('Name', _Name, '???');
  prop.StringValueOf('Data', ScenarioData, '');
end;

procedure TScenarioDescriptorItem.DuplicateTo(aItem: Pointer);
begin
  TScenarioDescriptorItem(aItem).ScenarioData := Copy(ScenarioData, 1, Length(ScenarioData));
end;

function TScenarioDescriptorItem.VariableName: string;
begin
  Result := 'scenario'+_Name;
end;

function TScenarioDescriptorItem.PascalCodeToInitializeVariable: string;
begin
  Result := '  '+VariableName+' := '+CodeGen.TextToPascal(ScenarioData, True)+';';
end;

{ TScenarioBank }

function TScenarioBank.AddEmpty: TScenarioDescriptorItem;
begin
  Result := TScenarioDescriptorItem.Create;
  Result.InitDefault;
  PushBack(Result);
end;

function TScenarioBank.GetItemReadableName: string;
begin
  Result := sScenario;
end;

function TScenarioBank.GetHeaderFile: string;
begin
  Result := '[SCENARIOBANK]';
end;

function TScenarioBank.GetSaveFileName: string;
begin
  Result := 'ScenarioBank.oglc';
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
 i: integer;
begin
  filename := Project.Config.TargetLazarusProject.GetFolderGameHelperFiles + GetSaveFileName;
  FScene.LogInfo('Saving '+GetSaveFileName+' to '+filename, 1);
  sl := TStringList.Create;
  try
    try
      sl.Add(GetHeaderFile);
      //sl.Add(SaveToString);

      if Size > 0 then
        for i:=0 to Size-1 do
          sl.Add(Mutable[i]^.SaveToString);

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
  k: Integer;
  o: T;
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

   {   if k = sl.Count-1 then begin
        FScene.LogError('no data found after the header in file !', 2);
        exit;
      end;
      LoadFromString(sl.Strings[k+1]); }

      if k = sl.Count-1 then begin
        FScene.LogInfo('file is empty', 2);
        exit;
      end;
      repeat
        inc(k);
        o := AddEmpty;
        TItemWithName(o).LoadFromString(sl.Strings[k]);
      until k = sl.Count-1;


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

