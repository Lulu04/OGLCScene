unit u_spritebank;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector,
  u_undo_redo;

type

{ TSpriteCodeGenerationOptions }

TSpriteCodeGenerationOptions = record
  useapplysymetrywhenflip,
  overrideProcessMessage,
  overrideUpdate: boolean;
  procedure InitDefault;
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;

{ TSpriteBankItem }

PSpriteBankItem = ^TSpriteBankItem;
TSpriteBankItem = record
  name,
  textures,
  surfaces,
  collisionbodies,
  postures,
  codeoptions: string;
  procedure InitDefault;
  // copy all fields except name
  procedure DuplicateTo(aItem: PSpriteBankItem);
  function SaveToString: string;
  procedure LoadFromString(const s: string);

  // i.e. for a sprite named 'Car' return 'u_sprite_car'
  function GetUnitName: string;
  function GetUnitFullFilename: string;
  procedure ExportSpriteToPascalUnit;
end;

{ TSpriteBank }

TSpriteBank = class(specialize TVector<TSpriteBankItem>)
  function AddEmpty: PSpriteBankItem;
  // case insensitive
  function SpriteNameExists(const aName: string): boolean;
  // search is case insensitive
  function GetItemByName(const aName: string): PSpriteBankItem;
  function GetItemByIndex(aindex: SizeUInt): PSpriteBankItem;

  procedure DeleteByIndex(aIndex: integer);
  procedure DeleteByName(const aName: string);

  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
  procedure SaveToPath(const aPath: string);
  procedure LoadFromPath(const aPath: string);

  procedure FillComboBox(aCB: TComboBox);
end;

var
  SpriteBank: TSpriteBank;

type
TSpriteBankUndoRedoActionType = (uratSpriteBankUndefined,
                           uratSpriteBankDeleteItem, // user delete a sprite
                           uratSpriteBankRenameItem, // user rename a sprite
                           uratSpriteBankAddItem     // user duplicate a sprite
                          );
TSpriteBankUndoRedoItem = record
  action: TSpriteBankUndoRedoActionType;
  data: TSpriteBankItem;
  newData: TSpriteBankItem;
  oldName: string;
end;

{ TSpriteBankUndoRedoManager }

TSpriteBankUndoRedoManager = class(specialize TGenericUndoRedoManager<TSpriteBankUndoRedoItem>)
  procedure ProcessUndo(var aItem: TSpriteBankUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TSpriteBankUndoRedoItem); override;

  procedure AddActionDeleteSprite(aIndex: integer);
  procedure AddActionRenameSprite(aIndex: integer; const aOldName: string);
  procedure AddActionDuplicateSprite(aIndexNewItem: integer);
end;

implementation

uses form_main, u_texture_list, u_surface_list, u_collisionbody_list,
  u_posture_list, u_project, u_common, u_utils, u_connection_to_ide,
  u_ui_objectlist, BGRABitmapTypes, LazFileUtils;

{ TSpriteCodeGenerationOptions }

procedure TSpriteCodeGenerationOptions.InitDefault;
begin
  useapplysymetrywhenflip := False;
  overrideProcessMessage := False;
  overrideUpdate := False;
end;

function TSpriteCodeGenerationOptions.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('UseApplySymetryWhenFlip',  useapplysymetrywhenflip);
  prop.Add('OverrideProcessMessage', overrideProcessMessage);
  prop.Add('OverrideUpdate', overrideUpdate);
  Result := prop.PackedProperty;
end;

procedure TSpriteCodeGenerationOptions.LoadFromString(const s: string);
var prop: TProperties;
begin
  prop.Split(s, '|');
  prop.BooleanValueOf('UseApplySymetryWhenFlip', useapplysymetrywhenflip, False);
  prop.BooleanValueOf('OverrideProcessMessage', overrideProcessMessage, False);
  prop.BooleanValueOf('OverrideUpdate', overrideUpdate, False);
end;

{ TSpriteBankItem }

procedure TSpriteBankItem.InitDefault;
begin
  //FillChar(Self, SizeOf(TSpriteBankItem), 0);
  Self := Default(TSpriteBankItem);
end;

procedure TSpriteBankItem.DuplicateTo(aItem: PSpriteBankItem);
begin
  aItem^.textures := textures;
  aItem^.surfaces := surfaces;
  aItem^.collisionbodies := collisionbodies;
  aItem^.postures := postures;
  aItem^.codeoptions := codeoptions;
end;

function TSpriteBankItem.SaveToString: string;
begin
  Result := name+'#'+textures+'#'+surfaces+'#'+collisionbodies+'#'+postures+
            '#'+codeoptions;
end;

procedure TSpriteBankItem.LoadFromString(const s: string);
var A: TStringArray;
begin
  A := s.Split(['#']);
  name := A[0];
  textures := A[1];
  surfaces := A[2];
  if Length(A) = 4 then collisionbodies := A[3]
    else collisionbodies := '';
  if Length(A) = 5 then postures := A[4]
    else postures := '';
  if Length(A) = 6 then codeoptions := A[5]
    else codeoptions := '';
end;

function TSpriteBankItem.GetUnitName: string;
begin
  Result := UNIT_NAME_PREFIX[ulSprites] + LowerCase(name);
end;

function TSpriteBankItem.GetUnitFullFilename: string;
begin
  Result := Project.Config.TargetLazarusProject.GetFolderUnitsSprites + GetUnitName + '.pas';
end;

procedure TSpriteBankItem.ExportSpriteToPascalUnit;
var t: TStringList;
  textureList: TTextureList;
  surfaceList: TSurfaceList;
  bodyList: TBodyItemList;
  postureList: TPostureList;
  codeop: TSpriteCodeGenerationOptions;
  i, j, c: integer;
  s, sx, sy: string;
  rootItem, current, _parent, surfItem: PSurfaceDescriptor;
  bodyItem: PBodyItem;
  pt: TPostureValues;
  pf: TPointF;
  nameUnit, nameClass, spriteClassType: string;
  fontItem: TFontDescriptorItem;
begin
  if (textures = '') or (surfaces = '') then exit;
  // create each needed instance
  textureList := TTextureList.Create;
  surfaceList := TSurfaceList.Create;
  surfaceList.Textures := textureList;
  bodyList := TBodyItemList.Create;
  postureList := TPostureList.Create;
  t := TStringList.Create;

  try
    textureList.LoadFromString(textures);
    surfaceList.LoadFromString(surfaces, False);
    bodyList.LoadFromString(collisionbodies);
    postureList.LoadFromString(postures);
    codeop.LoadFromString(codeoptions);


    nameClass := 'T'+name;
    nameUnit := GetUnitName;
    rootItem := surfaceList.GetRootItem;
    spriteClassType := rootItem^.classtype.ClassName; //rootItem^.surface.ClassName;

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
      t.Add('    - add the sprite textures to your atlas with class method '+nameClass+'.LoadTexture().');
    t.AddText('    - create an instance of the sprite'#10+
              '      i.e. F'+name+' := TCustom'+name+'.Create(aLAYER_INDEX);'#10+
              '    or if the sprite is a child of another:'#10+
              '           F'+name+' := TCustom'+name+'.Create(-1);'#10+
              '           F'+name+'.SetChildOf(parentSprite, zordervalue);'#10+
              '    - initialize the sprite coordinates'#10+
              '           F'+name+'.Setcoordinates(...);'#10+
              '}');
    t.Add('');
    CodeGen.AddInterface(t, nameUnit);

    // class declaration
    t.Add(nameClass+' = class('+spriteClassType+')');

    // textures declaration
    if textureList.Size > 0 then begin
      t.AddText('private');
      s := '  class var ';
      for i:=0 to textureList.Size-1 do begin
        s := s + textureList.Mutable[i]^.name;
        if i < textureList.Size-1 then s := s +', ';
        if (i mod 4 = 0) and (i > 0) and (i < textureList.Size-1) then s := s + #10'  ';
      end;
      s := s + ': PTexture;'#10+
           '  class var FAdditionnalScale: single;';
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

    // flipH and flipV for ApplySymmetryWhenFlip
    if codeop.useapplysymetrywhenflip then begin
      t.Add('protected');
      CodeGen.AddDeclarationOfProtectedFlipHAndFlipVForApplySymmetryWhenFlip(t);
    end;

    // methods declaration
//    t.Add('public');
    if textureList.Size > 0 then
      CodeGen.AddDeclarationOfClassLoadTexture(t);
    CodeGen.AddDeclarationOfSurfaceConstructor(t);
    if codeop.overrideProcessMessage then
      CodeGen.AddDeclarationOfProcessMessage(t);
    if codeop.overrideUpdate then
      CodeGen.AddDeclarationOfUpdate(t);

    // methods declaration for posture
    if postureList.Size > 0 then begin
      s := 'public'#10;
      for i:=0 to postureList.Size-1 do begin
        s := s+'  procedure Posture_'+postureList.Mutable[i]^.name+'(aDuration: single=0.5; aVelocityCurve: word=idcSinusoid);';
        if i < postureList.Size-1 then s := s+#10;
      end;
      t.AddText(s);
    end;


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
                '  FAdditionnalScale := AdditionnalScale;'#10+
                '  texFolder := u_common.TexturesFolder;');
      for i:=0 to textureList.Size-1 do
        t.Add('  '+textureList.Mutable[i]^.PascalCodeToAddTextureToAtlas(True));
      t.AddText('end;'#10+#10);
    end;

    // procedure SetFlipH and SetFlipV
    if codeop.useapplysymetrywhenflip then begin
      t.AddText('procedure '+nameClass+'.SetFlipH(AValue: boolean);'#10+
                'begin'#10+
                '  inherited SetFlipH(AValue);');
      s := '';
      for i:=0 to surfaceList.Size-1 do begin
        current := surfaceList.Mutable[i];
        if (rootItem <> NIL) and (current = rootItem) then continue;
        if current^.flipH then t.Add('  '+current^.name+'.FlipH := not AValue;')
          else t.Add('  '+current^.name+'.FlipH := AValue;');
        if current^.FlipV then s := s+'  '+current^.name+'.FlipV := not AValue;'
          else s := s+'  '+current^.name+'.FlipV := AValue;';
        if i < surfaceList.Size-1 then s := s + #10;
      end;
      t.AddText('end;'#10+#10+
                'procedure '+nameClass+'.SetFlipV(AValue: boolean);'#10+
                'begin'#10+
                '  inherited SetFlipV(AValue);'#10+
                s+#10+
                'end;');
      t.Add('');
    end;

    // constructor
    t.AddText('constructor '+nameClass+'.Create(aLayerIndex: integer);'#10+
              'begin'#10);
    case spriteClassType of
      'TSpriteContainer': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TSprite', 'TSpriteWithElasticCorner', 'TTiledSprite', 'TPolarSprite', 'TScrollableSprite': begin
        t.Add('  inherited Create('+rootItem^.textureName + ', False);');
      end;
      'TShapeOutline': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TGradientRectangle': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TQuad4Color': begin
        t.Add('  inherited Create(FScene);');
        CodeGen.ExtraPropertiesToPascalCode(t, rootItem, '  ');
      end;
      'TDeformationGrid': begin
        t.Add('  inherited Create('+rootItem^.textureName + ', False);');
      end;
      'TOGLCPathToFollow': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TSpriteOnPath': begin
        raise exception.create('TSpriteOnPath can not be the root surface');
      end;
      'TFreeTextOnPath': begin
        raise exception.create('TFreeTextOnPath can not be the root surface');
      end;
      'TFreeText': begin
        t.Add('  inherited Create(FScene);');
      end;
      'TFreeTextClock': begin
        t.Add('  inherited Create(FScene, '+CodeGen.BooleanToPascal(rootItem^.ClockStartPaused)+');');
      end;
      'TFreeTextAligned': begin
        fontItem := FontBank.GetByName(rootItem^.FontDescriptorName);
        t.Add('  inherited Create(FScene, '+fontItem.VariableNameForTexturedFont+
              ', ScaleW('+rootItem^.width.ToString+'), ScaleH('+rootItem^.height.ToString+'));');
      end
      else raise exception.create('forgot to implement '+spriteClassType);
    end;
    t.AddText('  if aLayerIndex <> -1 then'#10+
              '    FScene.Add(Self, aLayerIndex);'#10);
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
        'TSpriteContainer': t.Add('  '+current^.name+' := TSpriteContainer.Create(FScene)');
        'TSprite': t.Add('  '+current^.name+' := TSprite.Create('+current^.textureName+', False);');
        'TPolarSprite': t.Add('  '+current^.name+' := TPolarSprite.Create('+current^.textureName+', False);');
        'TQuad4Color': t.Add('  '+current^.name+' := TQuad4Color.Create(FScene);');
        'TGradientRectangle': t.Add('  '+current^.name+' := TGradientRectangle.Create(FScene);');
        'TDeformationGrid': t.Add('  '+current^.name+' := TDeformationGrid.Create('+current^.textureName+', False);');
        'TOGLCPathToFollow': t.Add('  '+current^.name+' := TOGLCPathToFollow.Create(FScene);');
        'TSpriteOnPath': begin
          surfItem := surfaceList.GetItemByID(current^.parentID);
          t.Add('  '+current^.name+' := TSpriteOnPath.CreateAsChildOf('+surfItem^.name+', '+current^.textureName+', False, '+current^.zOrder.ToString+');');
        end;
        'TFreeText': t.Add('  '+current^.name+' := TFreeText.Create(FScene);');
        'TFreeTextClock': t.Add('  '+current^.name+' := TFreeTextClock.Create(FScene, '+CodeGen.BooleanToPascal(current^.ClockStartPaused)+');');
        'TFreeTextAligned': begin
          fontItem := FontBank.GetByName(current^.FontDescriptorName);
          t.Add('  '+current^.name+' := TFreeTextAligned.Create(FScene, '+fontItem.VariableNameForTexturedFont+
              ', ScaleW('+current^.width.ToString+'), ScaleH('+current^.height.ToString+'));');
        end;
        'TFreeTextOnPath': begin
          surfItem := surfaceList.GetItemByID(current^.parentID);
          t.Add('  '+current^.name+' := TFreeTextOnPath.CreateAsChildOf('+surfItem^.name+', '+current^.zOrder.ToString+');');
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
          sx := CodeGen.FormatXCoorRelativeToParentWidth(current^.x/current^.width, current^.name);
          sy := CodeGen.FormatYCoorRelativeToParentHeight(current^.y/current^.height, current^.name);
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
      if codeop.useapplysymetrywhenflip then
        t.Add('    ApplySymmetryWhenFlip := True;');
      t.Add('  end;');
      if i < surfaceList.Size-1 then t.Add('');
    end;//for

    t.Add('');
    // create collision bodies
    if bodyList.Size > 0 then begin
      t.Add('  // Collision body');
      for i:=0 to bodyList.Size-1 do begin
        bodyItem := bodyList.Mutable[i];
        // to keep right proportion, coordinates must be relative to
        // the width and height
        case bodyItem^.BodyType of
          _btPoint: begin
            pf := bodyItem^.ItemDescriptor.pt;
            s := '  CollisionBody.AddPoint(';
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     ')';
            t.Add(s);
          end;
          _btLine: begin
            pf := bodyItem^.ItemDescriptor.pt1;
            s := '  CollisionBody.AddLine(';
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     '), ';
            pf := bodyItem^.ItemDescriptor.pt2;
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     '));';
            t.Add(s);

  {          t.Add('  CollisionBody.AddLine('+PointFToPascal(bodyItem^.ItemDescriptor.pt1)+'*FAdditionalScale, '+
                                      PointFToPascal(bodyItem^.ItemDescriptor.pt2)+'*FAdditionalScale);');  }
          end;
          _btCircle: begin
            pf := bodyItem^.ItemDescriptor.center;
            s := '  CollisionBody.AddCircle(';
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     '), ';
            s := s + CodeGen.FormatXCoorRelativeToParentWidth(bodyItem^.ItemDescriptor.radius/rootItem^.width{surface.Width}, '')+');';
            t.Add(s);

          //  t.Add('  CollisionBody.AddCircle('+PointFToPascal(bodyItem^.ItemDescriptor.center)+'*FAdditionalScale, '+
          //                              FormatFloatWithDot('0.00', bodyItem^.ItemDescriptor.radius)+'*FAdditionalScale);');
          end;
          _btRect: begin
            pf := bodyItem^.ItemDescriptor.rect.TopLeft;
            s := '  CollisionBody.AddRect(';
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     '), ';
            pf := bodyItem^.ItemDescriptor.rect.BottomRight;
            s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                     ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                     '));';
            t.Add(s);

           // t.Add('  CollisionBody.AddRect(RectF('+PointFToPascal(bodyItem^.ItemDescriptor.rect.TopLeft)+'*FAdditionalScale, '+
           //                                 PointFToPascal(bodyItem^.ItemDescriptor.rect.BottomRight)+'*FAdditionalScale));');
          end;
          _btPolygon: begin
            if Length(bodyItem^.ItemDescriptor.pts) = 0 then continue;
            s := '  CollisionBody.AddPolygon([';
            c := 0;
            for j:=0 to High(bodyItem^.ItemDescriptor.pts) do begin
              pf := bodyItem^.ItemDescriptor.pts[j];
              s := s + 'PointF('+CodeGen.FormatXCoorRelativeToParentWidth(pf.x/rootItem^.width{surface.Width}, '')+
                       ', '+CodeGen.FormatYCoorRelativeToParentHeight(pf.y/rootItem^.height{surface.Height}, '')+
                       ')';
              //s := s + PointFToPascal(bodyItem^.ItemDescriptor.pts[j])+'*FAdditionalScale';
              if j < High(bodyItem^.ItemDescriptor.pts) then s := s+', ';
              inc(c);
              if (c mod 2 = 0) and (j > 0) and (j < Length(bodyItem^.ItemDescriptor.pts)-1) then s := s + #10'    ';
            end;
            s := s+']);';
            t.AddText(s);
          end;
        end;//case
      end;
    end;

    t.Add('end;');
    t.Add('');

    // method ProcessMessage()
    if codeop.overrideProcessMessage then
      CodeGen.AddImplementationOfProcessMessage(t, nameClass);

    // method update()
    if codeop.overrideUpdate then
      CodeGen.AddImplementationOfUpdate(t, nameClass);

    // methods for postures
    if postureList.Size > 0 then begin
      for i:=0 to postureList.Size-1 do begin
        t.AddText('procedure '+nameClass+'.Posture_'+postureList.Mutable[i]^.name+'(aDuration: single; aVelocityCurve: word);'#10+
                  'begin');
        for j:=0 to surfaceList.Size-1 do begin
          current := surfaceList.Mutable[j];
          if current = rootItem then continue;
          _parent := surfaceList.GetItemByID(current^.parentID);
          // copy values to variable for easier access
          //current^.DuplicateValuesToTemporaryVariables;

          // to keep right proportion, coordinates must be relative to
          // the width and height of the parent or current
          pt := postureList.Mutable[i]^.Values[j];
          if _parent^.classtype = TSpriteContainer then begin
            sx := CodeGen.FormatXCoorRelativeToParentWidth(pt.x/current^.width, current^.name);
            sy := CodeGen.FormatYCoorRelativeToParentHeight(pt.y/current^.height, current^.name);
          end else begin
            if _parent^.IsRoot then begin
              sx := CodeGen.FormatXCoorRelativeToParentWidth(pt.x/_parent^.width,'Self');
              sy := CodeGen.FormatYCoorRelativeToParentHeight(pt.y/_parent^.height, 'Self');
            end else begin
              sx := CodeGen.FormatXCoorRelativeToParentWidth(pt.x/_parent^.width, _parent^.name);
              sy := CodeGen.FormatYCoorRelativeToParentHeight(pt.y/_parent^.height, _parent^.name);
            end;
          end;
          t.Add('  '+current^.name+'.MoveTo('+sx+', '+sy+', aDuration, aVelocityCurve);');

          t.Add(CodeGen.Generate_AngleChangeTo(postureList.Mutable[i]^.Values[j].angle, current^.name));
          t.Add(CodeGen.Generate_ScaleChangeTo(postureList.Mutable[i]^.Values[j].scalex, postureList.Mutable[i]^.Values[j].scaley, current^.name));
        end;
        t.Add('end;');
        t.Add('');
      end;
    end;

    t.Add('end.');

    t.SaveToFile(GetUnitFullFilename);
  finally
    textureList.Clear;
    textureList.Free;
    surfaceList.Free;
    bodyList.free;
    postureList.Free;
    t.Free;
  end;
end;

{ TSpriteBank }

function TSpriteBank.AddEmpty: PSpriteBankItem;
var o: TSpriteBankItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TSpriteBank.SpriteNameExists(const aName: string): boolean;
begin
  Result := GetItemByName(aName) <> NIL;
end;

function TSpriteBank.GetItemByName(const aName: string): PSpriteBankItem;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if CompareText(Mutable[i]^.name, aName) = 0 then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSpriteBank.GetItemByIndex(aindex: SizeUInt): PSpriteBankItem;
begin
  Result := Mutable[aindex];
end;

procedure TSpriteBank.DeleteByIndex(aIndex: integer);
begin
  Erase(aIndex);
end;

procedure TSpriteBank.DeleteByName(const aName: string);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then begin
      DeleteByIndex(i);
      exit;
    end;
end;

procedure TSpriteBank.SaveTo(t: TStringList);
var i: SizeUInt;
begin
  t.Add('[SPRITE BANK]');
  t.Add(integer(Size).ToString);
  if Size > 0 then
    for i:=0 to Size-1 do begin
      t.Add(Mutable[i]^.name);
      t.Add(Mutable[i]^.textures);
      t.Add(Mutable[i]^.surfaces);
      t.Add(Mutable[i]^.collisionbodies);
      t.Add(Mutable[i]^.postures);
    end;
end;

procedure TSpriteBank.LoadFrom(t: TStringList);
var k, c, i: integer;
  o: TSpriteBankItem;
begin
  Clear;
  k := t.IndexOf('[SPRITE BANK]');
  if (k = -1) or (k = t.Count-1) then exit;

  inc(k);
  c := t.Strings[k].ToInteger;

  if c = 0 then exit;
  for i:=0 to c-1 do begin
    inc(k);
    o.name := t.Strings[k];
    inc(k);
    o.textures := t.Strings[k];
    inc(k);
    o.surfaces := t.Strings[k];
    inc(k);
    o.collisionbodies := t.Strings[k];
    inc(k);
    o.postures := t.Strings[k];
    PushBack(o);
  end;
end;

procedure TSpriteBank.SaveToPath(const aPath: string);
var t: TStringList;
  filename: string;
begin
  t := TStringList.Create;
  filename := aPath + 'SpriteBank.oglc';
  FScene.LogInfo('Saving Sprite Bank to '+filename, 1);
  try
    try
      SaveTo(t);
      t.SaveToFile(filename);
      FScene.LogInfo('done', 2);
    except
      On E :Exception do begin
        FScene.LogError('TSpriteBank.Save: exception occur');
        FScene.logError(E.Message, 1);
     end;
    end;
  finally
    t.Free;
  end;
end;

procedure TSpriteBank.LoadFromPath(const aPath: string);
var t: TStringList;
  filename: string;
begin
  Clear;
  filename := aPath + 'SpriteBank.oglc';
  if not FileExistsUTF8(filename) then begin
    FScene.LogInfo('No Sprite Bank found', 1);
    exit;
  end;

  FScene.LogInfo('Found Sprite Bank, loading...', 1);
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(filename);
      LoadFrom(t);
      FScene.LogInfo('success, '+Size.ToString+' item(s)', 2);
    except
      On E :Exception do begin
        FScene.logError(E.Message, 2);
      end;
    end;
  finally
    t.Free;
  end;
end;

procedure TSpriteBank.FillComboBox(aCB: TComboBox);
var i: SizeUInt;
begin
  aCB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aCB.Items.Add(Mutable[i]^.name);
end;

{ TSpriteBankUndoRedoManager }

procedure TSpriteBankUndoRedoManager.ProcessUndo(var aItem: TSpriteBankUndoRedoItem);
var o: PSpriteBankItem;
  i: integer;
begin
  case aItem.action of
    uratSpriteBankDeleteItem: begin
      o := SpriteBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.textures := aItem.data.textures;
      o^.surfaces := aItem.data.surfaces;
      o^.collisionbodies := aItem.data.collisionbodies;
      o^.postures := aItem.data.postures;
      FrameToolsSpriteBank.LB.ItemIndex := FrameToolsSpriteBank.LB.Items.Add(o^.name);
    end;

    uratSpriteBankRenameItem: begin
      o := SpriteBank.GetItemByName(aItem.data.name);
      if o = NIL then begin raise exception.Create('bug: name not found in SpriteBank'); exit; end;
      o^.name := aItem.oldName;
      i := FrameToolsSpriteBank.LB.ItemIndex;
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Strings[i] := aItem.oldName;
    end;

    uratSpriteBankAddItem: begin
      SpriteBank.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Delete(i);
    end;
  end;
end;

procedure TSpriteBankUndoRedoManager.ProcessRedo(var aItem: TSpriteBankUndoRedoItem);
var i: Integer;
  o: PSpriteBankItem;
begin
  case aItem.action of
    uratSpriteBankAddItem: begin
      o := SpriteBank.AddEmpty;
      o^.name := aItem.data.name;
      o^.textures := aItem.data.textures;
      o^.surfaces := aItem.data.surfaces;
      o^.collisionbodies := aItem.data.collisionbodies;
      o^.postures := aItem.data.postures;
      FrameToolsSpriteBank.LB.ItemIndex := FrameToolsSpriteBank.LB.Items.Add(o^.name);
    end;

    uratSpriteBankDeleteItem: begin
      SpriteBank.DeleteByName(aItem.data.name);
      i := FrameToolsSpriteBank.LB.Items.IndexOf(aItem.data.name);
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Delete(i);
    end;

    uratSpriteBankRenameItem: begin
      o := SpriteBank.GetItemByName(aItem.oldName);
      if o = NIL then begin raise exception.Create('bug: oldName not found in SpriteBank'); exit; end;
      o^.name := aItem.data.name;
      i := FrameToolsSpriteBank.LB.ItemIndex;
      if i <> -1 then FrameToolsSpriteBank.LB.Items.Strings[i] := aItem.data.name;
    end;
  end;
end;

procedure TSpriteBankUndoRedoManager.AddActionDeleteSprite(aIndex: integer);
var o: TSpriteBankUndoRedoItem;
begin
  o := Default(TSpriteBankUndoRedoItem);
  o.action := uratSpriteBankDeleteItem;
  o.data.name := SpriteBank.Mutable[aIndex]^.name;
  o.data.textures := SpriteBank.Mutable[aIndex]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndex]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndex]^.collisionbodies;
  o.data.postures := SpriteBank.Mutable[aIndex]^.postures;
  AddItem(o);
end;

procedure TSpriteBankUndoRedoManager.AddActionRenameSprite(aIndex: integer; const aOldName: string);
var o: TSpriteBankUndoRedoItem;
begin
  o := Default(TSpriteBankUndoRedoItem);
  o.action := uratSpriteBankRenameItem;
  o.data.name := SpriteBank.Mutable[aIndex]^.name;
  o.data.textures := SpriteBank.Mutable[aIndex]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndex]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndex]^.collisionbodies;
  o.data.postures := SpriteBank.Mutable[aIndex]^.postures;
  o.oldName := aOldName;
  AddItem(o);
end;

procedure TSpriteBankUndoRedoManager.AddActionDuplicateSprite(aIndexNewItem: integer);
var o: TSpriteBankUndoRedoItem;
begin
  o := Default(TSpriteBankUndoRedoItem);
  o.action := uratSpriteBankAddItem;
  o.data.name := SpriteBank.Mutable[aIndexNewItem]^.name;
  o.data.textures := SpriteBank.Mutable[aIndexNewItem]^.textures;
  o.data.surfaces := SpriteBank.Mutable[aIndexNewItem]^.surfaces;
  o.data.collisionbodies := SpriteBank.Mutable[aIndexNewItem]^.collisionbodies;
  o.data.postures:= SpriteBank.Mutable[aIndexNewItem]^.postures;
  AddItem(o);
end;

end.

