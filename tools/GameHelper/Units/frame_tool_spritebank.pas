unit frame_tool_spritebank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Dialogs, Menus, u_screen_spritebank, u_surface_list, u_texture_list,
  u_collisionbody_list, u_posture_list, u_spritebank;

type

  { TFrameToolSpriteBank }

  TFrameToolSpriteBank = class(TFrame)
    BDuplicate: TSpeedButton;
    BRename: TSpeedButton;
    BUndo: TSpeedButton;
    BRedo: TSpeedButton;
    CBShowCollisionBody: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    LB: TListBox;
    MIRedo: TMenuItem;
    MIUndo: TMenuItem;
    Panel8: TPanel;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    MIDuplicate: TMenuItem;
    MIDelete: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    SD1: TSaveDialog;
    BEdit: TSpeedButton;
    BExportToPascalUnit: TSpeedButton;
    BDelete: TSpeedButton;
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure CBShowCollisionBodyChange(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure BExportToPascalUnitClick(Sender: TObject);
  private
    procedure FillLB;
    procedure ShowSprite(aIndex: integer);
    procedure UpdateWidgetState;
  private
    FUndoRedoManager: TBankUndoRedoManager;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    function Textures: TTextureList;
    function Surfaces: TSpriteBankSurfaceList;
    function Bodies: TBodyItemList;
    function Postures: TPostureList;
  end;

implementation

uses LCLType, Graphics, form_main, u_project, OGLCScene, BGRABitmap,
  BGRABitmapTypes;

{$R *.lfm}

{ TFrameToolSpriteBank }

procedure TFrameToolSpriteBank.LBSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  UpdateWidgetState;
  ShowSprite(LB.ItemIndex);

  i := LB.ItemIndex;
  if i = -1 then Edit1.Text := ''
    else Edit1.Text := 'T'+SpriteBank.Mutable[i]^.name;
end;

procedure TFrameToolSpriteBank.BExportToPascalUnitClick(Sender: TObject);
var t: TStringlist;
  i, j, c: integer;
  s, sw, sh, sx, sy, texFilename: string;
  rootItem, current, _parent: PSurfaceDescriptor;
  bodyItem: PBodyItem;
  p: SizeInt;
  bodyList: TBodyItemList;
  pt: TPostureValues;
  pf: TPointF;
  function GetClassName: string;
  begin
    Result := Trim(Edit1.Text);
  end;
  function GetClassType: string;
  begin
    Result := rootItem^.surface.ClassName;
  end;
  function PointFToString(const aX, aY: single): string; overload;
  begin
    Result := 'PointF('+FormatFloatWithDot('0.00', aX)+', '+FormatFloatWithDot('0.00', aY)+')';
  end;
  function PointFToString(const aPt: TPointF): string; overload;
  begin
    Result := 'PointF('+FormatFloatWithDot('0.00', aPt.x)+', '+FormatFloatWithDot('0.00', aPt.y)+')';
  end;
  function Generate_AngleChangeTo(aAngle: single; const aSurfaceName: string): string;
  begin
    Result :='  '+aSurfaceName+'.Angle.ChangeTo(';
    if aAngle = 0 then Result := Result+'0'
      else Result := Result+FormatFloatWithDot('0.000', aAngle);
    Result := Result+', aDuration, idcSinusoid);';
  end;
  function Generate_ScaleChangeTo(aScaleX, aScaleY: single; const aSurfaceName: string): string;
  begin
    Result := '  '+aSurfaceName+'.Scale.ChangeTo(PointF(';
    if aScaleX = 1.0 then Result := Result+'1.0'
      else Result := Result+FormatFloatWithDot('0.000', aScaleX);
    Result := Result + ', ';
    if aScaleY = 1.0 then Result := Result+'1.0'
      else Result := Result+FormatFloatWithDot('0.000', aScaleY);
    Result := Result + '), aDuration, idcSinusoid);';
  end;
  function FormatXCoorRelativeToParentWidth(aX: single; const aSurfaceName: string): string;
  begin
    if aX = 0 then Result := '0'
      else begin
        if aSurfaceName <> '' then
          Result := FormatFloatWithDot('0.000', aX)+'*'+aSurfaceName+'.Width'
        else
          Result := FormatFloatWithDot('0.000', aX)+'*'+'Width';
      end;
  end;
  function FormatYCoorRelativeToParentHeight(aY: single; const aSurfaceName: string): string;
  begin
    if aY = 0 then Result := '0'
      else begin
        if aSurfaceName <> '' then
          Result := FormatFloatWithDot('0.000', aY)+'*'+aSurfaceName+'.Height'
        else
          Result := FormatFloatWithDot('0.000', aY)+'*'+'Height';
      end;
  end;

begin
  if LB.ItemIndex = -1 then exit;
  rootItem := Surfaces.GetRootItem;

  s := Trim(Edit1.Text);
  if Length(s) < 2 then exit;
  if s[1] <> 'T' then s := 'T'+s;
  SD1.FileName := Copy(s, 2, Length(Edit1.Text));
  if not SD1.Execute then exit;

  t := TStringlist.Create;

  t.AddText('unit '+ ChangeFileExt(ExtractFilename(SD1.FileName), '')+';'#10+
            #10+
            '{$mode ObjFPC}{$H+}'#10+
            #10+
            'interface'#10+
            #10+
            'uses'#10+
            '  Classes, SysUtils,'#10+
            '  OGLCScene, BGRABitmap, BGRABitmapTypes;'#10+
            #10+
            'type'#10+
            #10+
            GetClassName+' = class('+GetClassType+')');

  // textures declaration
  t.AddText('private'#10+
            '  class var FAdditionalScale: single;');
  s := '  class var ';
  for i:=0 to Textures.Size-1 do begin
    s := s + Textures.Mutable[i]^.name;
    if i < Textures.Size-1 then s := s +', ';
    if (i mod 4 = 0) and (i > 0) and (i < Textures.Size-1) then s := s + #10'  ';
  end;
  s := s + ': PTexture;';
  t.AddText(s);

  // child variables declaration
  t.Add('private');
  s := '';
  for i:=0 to Surfaces.Size-1 do begin
    current := Surfaces.Mutable[i];
    if current = rootItem then continue;
    s := s + '  '+current^.name+': '+current^.classtype.ClassName+';';
    if i < Surfaces.Size-1 then s := s +#10;
  end;
  t.AddText(s);

  // flipH and flipV for ApplySymmetryWhenFlip
  if CheckBox1.Checked then
    t.AddText('protected'#10+
              '  procedure SetFlipH(AValue: boolean); override;'#10+
              '  procedure SetFlipV(AValue: boolean); override;');

  // oglc methods declaration
  if CheckBox2.Checked then s := '  procedure ProcessMessage(UserValue: TUserMessageValue); override;'#10
    else s := '';
  t.AddText('public'#10+
            '  // aAdditionalScale is used to scale the collision bodies'#10+
            '  class procedure LoadTexture(aAtlas: TOGLCTextureAtlas; aAdditionalScale: single=1.0);'#10+
            '  constructor Create(aLayerIndex: integer=-1);'#10+
            s);

  // methods declaration for posture
  if Postures.Size > 0 then begin
    s := 'public'#10;
    for i:=0 to Postures.Size-1 do begin
      s := s+'  procedure Posture_'+Postures.Mutable[i]^.name+'(aDuration: single=0.5);';
      if i < Postures.Size-1 then s := s+#10;
    end;
    t.AddText(s);
  end;


  t.Add('end;');   // end of class declaration
  t.Add('');
  t.AddText('implementation'#10+
            #10+
            '{ '+GetClassName+' }'#10+
            #10);

  // procedure load Texture
  t.AddText('class procedure '+GetClassName+'.LoadTexture(aAtlas: TOGLCTextureAtlas; aAdditionalScale: single);'#10+
            'var dataFolder: string;'#10+
            'begin'#10+
            '  FAdditionalScale := aAdditionalScale;'#10+
            '  dataFolder := FScene.App.DataFolder;');
  for i:=0 to Textures.Size-1 do begin
    // texture filename must be relative to application Data folder
    texFilename := Textures.Mutable[i]^.filename;
    p := texFilename.LastIndexOf(DirectorySeparator+'Data'+DirectorySeparator);
    texFilename := texFilename.Remove(0, p+6);
    texFilename := 'dataFolder+'''+texFilename+'''';

    s := '  '+Textures.Mutable[i]^.name + ' := ';
    if ExtractFileExt(Textures.Mutable[i]^.filename) = '.svg' then begin
      if Textures.Mutable[i]^.width = -1 then sw := '-1'
        else sw := 'ScaleW('+Textures.Mutable[i]^.width.ToString+')';
      if Textures.Mutable[i]^.height = -1 then sh := '-1'
        else sh := 'ScaleH('+Textures.Mutable[i]^.height.ToString+')';

      if Textures.Mutable[i]^.isMultiFrame then
        s := s + '(aAtlas.AddMultiFrameImageFromSVG('+texFilename+
           ', '+sw+', '+sh+
           ', '+(Textures.Mutable[i]^.width div Textures.Mutable[i]^.frameWidth).ToString+
           ', '+(Textures.Mutable[i]^.height div Textures.Mutable[i]^.frameHeight).ToString+
           ', 0);'
      else
        s := s + 'aAtlas.AddFromSVG('+texFilename+', '+sw+', '+sh+');';
    end else begin
      if Textures.Mutable[i]^.isMultiFrame then
        s := s + 'aAtlas.AddMultiFrameImage('+texFilename+
        ', '+(Textures.Mutable[i]^.width div Textures.Mutable[i]^.frameWidth).ToString+
        ', '+(Textures.Mutable[i]^.height div Textures.Mutable[i]^.frameHeight).ToString+');'
      else
        s := s + 'aAtlas.Add('+texFilename+');';
    end;
    t.Add(s);
  end;
  t.AddText('end;'#10+
            #10);

  // procedure SetFlipH and SetFlipV
  if CheckBox1.Checked then begin
    t.AddText('procedure '+GetClassName+'.SetFlipH(AValue: boolean);'#10+
              'begin'#10+
              '  inherited SetFlipH(AValue);');
    s := '';
    for i:=0 to Surfaces.Size-1 do begin
      current := Surfaces.Mutable[i];
      if (rootItem <> NIL) and (current = rootItem) then continue;
      t.Add('  '+current^.name+'.FlipH := AValue;');
      s := s+'  '+current^.name+'.FlipV := AValue;';
      if i < Surfaces.Size-1 then s := s + #10;
    end;
    t.AddText('end;'#10+#10+
              'procedure '+GetClassName+'.SetFlipV(AValue: boolean);'#10+
              'begin'#10+
              '  inherited SetFlipV(AValue);'#10+
              s+#10+
              'end;');
    t.Add('');
  end;

  // constructor
  t.AddText('constructor '+GetClassName+'.Create(aLayerIndex: integer);'#10+
            'begin'#10);
  case GetClassType of
    'TSpriteContainer': begin
      s := '  inherited Create(FScene);';
    end;
    'TSprite', 'TSpriteWithElasticCorner', 'TTiledSprite',
    'TPolarSprite', 'TScrollableSprite': begin
      s := '  inherited Create('+rootItem^.textureName + ', False);';
    end;
    'TShapeOutline': begin
      s := '  inherited Create(FScene);';
    end;
    'TGradientRectangle': begin
      s := '  inherited Create(FScene);';
    end;
    'TQuad4Color': begin
      s := '  inherited Create(FScene);'#10+
           '  SetSize(...);';
    end;
    'TDeformationGrid': begin
      s := '  inherited Create('+rootItem^.textureName + ', False);';
    end;
    else raise exception.create('forgot to implement!');
  end;
  t.Add(s);
  t.AddText('  if aLayerIndex <> -1 then'#10+
            '    FScene.Add(Self, aLayerIndex);'#10);
  if rootItem <> NIL then begin
    if (rootItem^.x <> 0.0) or (rootItem^.y <> 0.0) then
      t.Add('  SetCoordinate('+FormatFloatWithDot('0.00', rootItem^.x)+', '+
                               FormatFloatWithDot('0.00', rootItem^.y)+');');
    if (rootItem^.pivotX <> 0.5) or (rootItem^.pivotY <> 0.5) then
      t.Add('  Pivot := '+PointFToString(rootItem^.pivotX, rootItem^.pivotY)+';');
    if rootitem^.angle <> 0.0 then
      t.Add('  Angle.Value := '+FormatFloatWithDot('0.00', rootItem^.angle)+';');
    if (rootItem^.scaleX <> 1.0) or (rootItem^.scaleY <> 1.0) then
      t.Add('  Scale.Value := '+PointFToString(rootItem^.scaleX, rootItem^.scaleY)+';');
    t.Add('');
  end else t.Add('');

  // creating childs
  for i:=0 to Surfaces.Size-1 do begin
    current := Surfaces.Mutable[i];
    // copy values to variable for easier access
    current^.DuplicateValuesToTemporaryVariables;

    if (rootItem <> NIL) and (current = rootItem) then continue;

    if current^.classtype = TSpriteContainer then begin
      s := '  '+current^.name+' := TSpriteContainer.Create(FScene)'#10;

    end else
    if current^.classtype = TSprite then begin
      s := '  '+current^.name+' := TSprite.Create('+current^.textureName+', False);';
    end else
    if current^.classtype = TDeformationGrid then begin
      s := '  '+current^.name+' := TDeformationGrid.Create('+current^.textureName+', False);';
    end else raise exception.create('forgot to implement!');
    t.Add(s);
    // set child dependency and values
    _parent := Surfaces.GetItemByID(current^.parentID);
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
        sx := FormatXCoorRelativeToParentWidth(current^.x/current^.surface.Width, current^.name);
        sy := FormatYCoorRelativeToParentHeight(current^.y/current^.surface.Height, current^.name);
      end else begin
        if _parent^.IsRoot then begin
          sx := FormatXCoorRelativeToParentWidth(current^.x/_parent^.surface.Width, 'Self');
          sy := FormatYCoorRelativeToParentHeight(current^.y/_parent^.surface.Height, 'Self');
        end else begin
          sx := FormatXCoorRelativeToParentWidth(current^.x/_parent^.surface.Width, _parent^.name);
          sy := FormatYCoorRelativeToParentHeight(current^.y/_parent^.surface.Height, _parent^.name);
        end;
      end;
      t.Add('    SetCoordinate('+sx+', '+sy+');');
    end;

    if (current^.pivotX <> 0.5) or (current^.pivotY <> 0.5) then
      t.Add('    Pivot := '+PointFToString(current^.pivotX, current^.pivotY)+';');
    if current^.angle <> 0 then
      t.Add('    Angle.Value := '+FormatFloatWithDot('0.00', current^.angle)+';');
    if (current^.scaleX <> 1.0) or (current^.scaleY <> 1.0) then
      t.Add('    Scale.Value := '+PointFToString(current^.scaleX, current^.scaleY)+';');
    if CheckBox1.Checked then
      t.Add('    ApplySymmetryWhenFlip := True;');
    t.Add('  end;');
    if i < Surfaces.Size-1 then t.Add('');
  end;//for

  // create collision bodies
  bodyList := TBodyItemList.Create;
  i := FrameToolsSpriteBank.LB.ItemIndex;
  bodyList.LoadFromString(SpriteBank.GetItemByName(FrameToolsSpriteBank.LB.Items.Strings[i])^.collisionbodies);

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
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale';
          t.Add(s);
        end;
        _btLine: begin
          pf := bodyItem^.ItemDescriptor.pt1;
          s := '  CollisionBody.AddLine(';
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale, ';
          pf := bodyItem^.ItemDescriptor.pt2;
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale);';
          t.Add(s);

{          t.Add('  CollisionBody.AddLine('+PointFToString(bodyItem^.ItemDescriptor.pt1)+'*FAdditionalScale, '+
                                    PointFToString(bodyItem^.ItemDescriptor.pt2)+'*FAdditionalScale);');  }
        end;
        _btCircle: begin
          pf := bodyItem^.ItemDescriptor.center;
          s := '  CollisionBody.AddCircle(';
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale, ';
          s := s + FormatXCoorRelativeToParentWidth(bodyItem^.ItemDescriptor.radius, '')+'*FAdditionalScale);';
          t.Add(s);

        //  t.Add('  CollisionBody.AddCircle('+PointFToString(bodyItem^.ItemDescriptor.center)+'*FAdditionalScale, '+
        //                              FormatFloatWithDot('0.00', bodyItem^.ItemDescriptor.radius)+'*FAdditionalScale);');
        end;
        _btRect: begin
          pf := bodyItem^.ItemDescriptor.rect.TopLeft;
          s := '  CollisionBody.AddRect(';
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale, ';
          pf := bodyItem^.ItemDescriptor.rect.BottomRight;
          s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                   ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                   ')*FAdditionalScale);';
          t.Add(s);

         // t.Add('  CollisionBody.AddRect(RectF('+PointFToString(bodyItem^.ItemDescriptor.rect.TopLeft)+'*FAdditionalScale, '+
         //                                 PointFToString(bodyItem^.ItemDescriptor.rect.BottomRight)+'*FAdditionalScale));');
        end;
        _btPolygon: begin
          if Length(bodyItem^.ItemDescriptor.pts) = 0 then continue;
          s := '  CollisionBody.AddPolygon([';
          c := 0;
          for j:=0 to High(bodyItem^.ItemDescriptor.pts) do begin
            pf := bodyItem^.ItemDescriptor.pts[j];
            s := s + 'PointF('+FormatXCoorRelativeToParentWidth(pf.x/rootItem^.surface.Width, '')+
                     ', '+FormatYCoorRelativeToParentHeight(pf.y/rootItem^.surface.Height, '')+
                     ')*FAdditionalScale';
            //s := s + PointFToString(bodyItem^.ItemDescriptor.pts[j])+'*FAdditionalScale';
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
  bodyList.Free;

  t.Add('end;');
  t.Add('');

  // method ProcessMessage()
  if CheckBox2.Checked then begin
    t.AddText('procedure '+GetClassName+'.ProcessMessage(UserValue: TUserMessageValue);'#10+
              'begin'#10+
              '  case UserValue of'#10+
              '    0: begin'#10+
              '    end;'#10+
              '  end;//case'#10+
              'end;');
    t.Add('');
  end;


  // methods for postures
  if Postures.Size > 0 then begin
    for i:=0 to Postures.Size-1 do begin
      t.AddText('procedure '+GetClassName+'.Posture_'+Postures.Mutable[i]^.name+'(aDuration: single);'#10+
                'begin');
      for j:=0 to Surfaces.Size-1 do begin
        current := Surfaces.Mutable[j];
        if current = rootItem then continue;
        _parent := Surfaces.GetItemByID(current^.parentID);
        // copy values to variable for easier access
        current^.DuplicateValuesToTemporaryVariables;

        // to keep right proportion, coordinates must be relative to
        // the width and height of the parent or current
        pt := Postures.Mutable[i]^.Values[j];
        if _parent^.classtype = TSpriteContainer then begin
          sx := FormatXCoorRelativeToParentWidth(pt.x/current^.surface.Width, current^.name);
          sy := FormatYCoorRelativeToParentHeight(pt.y/current^.surface.Height, current^.name);
        end else begin
          if _parent^.IsRoot then begin
            sx := FormatXCoorRelativeToParentWidth(pt.x/_parent^.surface.Width,'Self');
            sy := FormatYCoorRelativeToParentHeight(pt.y/_parent^.surface.Height, 'Self');
          end else begin
            sx := FormatXCoorRelativeToParentWidth(pt.x/_parent^.surface.Width, _parent^.name);
            sy := FormatYCoorRelativeToParentHeight(pt.y/_parent^.surface.Height, _parent^.name);
          end;
        end;
        t.Add('  '+current^.name+'.MoveTo('+sx+', '+sy+', aDuration, idcSinusoid);');

        t.Add(Generate_AngleChangeTo(Postures.Mutable[i]^.Values[j].angle, current^.name));
        t.Add(Generate_ScaleChangeTo(Postures.Mutable[i]^.Values[j].scalex, Postures.Mutable[i]^.Values[j].scaley, current^.name));
      end;
      t.Add('end;');
      t.Add('');
    end;
  end;

  t.Add('end.');
  try
    t.SaveToFile(SD1.FileName);
    ShowMessage('Pascal unit created');
  finally
    t.Free;
  end;
end;

procedure TFrameToolSpriteBank.LBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LB.ItemIndex := LB.GetIndexAtXY(X, Y);
end;

procedure TFrameToolSpriteBank.BEditClick(Sender: TObject);
var i: integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  LB.ItemIndex := -1;
  FormMain.EditSpriteInSpriteBank(LB.Items.Strings[i]);
end;

procedure TFrameToolSpriteBank.BDeleteClick(Sender: TObject);
var oldName, newName: string;
  i, k: integer;
  item, newSprite: PSpriteBankItem;
begin
  i := LB.ItemIndex;

  if Sender = BDelete then begin
    if i = -1 then exit;
    if QuestionDlg('','Delete this sprite ?', mtWarning,
                   [mrOk, 'Delete', mrCancel, 'Cancel'], 0) = mrCancel then exit;
    FUndoRedoManager.AddActionDeleteSprite(i);
    SpriteBank.DeleteByIndex(i);
    LB.Items.Delete(i);
    ScreenSpriteBank.ClearView;
    Project.SetModified;
    UpdateWidgetState;
  end;

  if Sender = BDuplicate then begin
    if i = -1 then exit;
    oldName := LB.Items.Strings[i];
    k := 0;
    repeat
      inc(k);
      if k < 100 then newName := oldName+'_'+Format('%.2d', [k])
        else newName := oldName+'_'+k.ToString;
    until not SpriteBank.SpriteNameExists(newName);
    item := SpriteBank.GetItemByName(oldName);
    if item = NIL then exit;
    newSprite := SpriteBank.AddEmpty;
    newSprite^.name := newName;
    newSprite^.textures := item^.textures;
    newSprite^.surfaces := item^.surfaces;
    newSprite^.collisionbodies := item^.collisionbodies;
    newSprite^.postures := item^.postures;
    LB.ItemIndex := LB.Items.Add(newName);
    FUndoRedoManager.AddActionDuplicateSprite(LB.ItemIndex);
    Project.SetModified;
  end;

  if Sender = BRename then begin
    if i = -1 then exit;
    oldName := LB.Items.Strings[i];
    newName := InputBox('', 'Enter the new name:', oldName);
    if newName = oldName then exit;
    SpriteBank.GetItemByName(LB.Items.Strings[i])^.name := newName;
    FUndoRedoManager.AddActionRenameSprite(i, oldName);
    LB.Items.Strings[i] := newName;
    Project.SetModified;
  end;

  if Sender = BUndo then begin
    FUndoRedoManager.Undo;
    Project.SetModified;
    UpdateWidgetState;
  end;

  if Sender = BRedo then begin
    FUndoRedoManager.Redo;
    Project.SetModified;
    UpdateWidgetState;
  end;
end;

procedure TFrameToolSpriteBank.CBShowCollisionBodyChange(Sender: TObject);
begin
  ShowSprite(LB.ItemIndex);
end;

procedure TFrameToolSpriteBank.FillLB;
var i: SizeUInt;
begin
  LB.Clear;
  if SpriteBank.Size = 0 then exit;
  for i:=0 to SpriteBank.Size-1 do
    LB.Items.Add(SpriteBank.Mutable[i]^.name);
end;

procedure TFrameToolSpriteBank.ShowSprite(aIndex: integer);
begin
  ScreenSpriteBank.ClearView;
  if aIndex <> -1 then
    ScreenSpriteBank.ShowSprite(aIndex);
end;

procedure TFrameToolSpriteBank.UpdateWidgetState;
begin
  BDuplicate.Enabled := LB.ItemIndex <> -1;
  BRename.Enabled := BDuplicate.Enabled;
  BDelete.Enabled := BDuplicate.Enabled;
  BUndo.Enabled := FUndoRedoManager.CanUndo;
  BRedo.Enabled := FUndoRedoManager.CanRedo;
end;

constructor TFrameToolSpriteBank.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FUndoRedoManager := TBankUndoRedoManager.Create;
end;

destructor TFrameToolSpriteBank.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TFrameToolSpriteBank.OnShow;
begin
  FillLB;
  UpdateWidgetState;
end;

function TFrameToolSpriteBank.Textures: TTextureList;
begin
  Result := ScreenSpriteBank.Textures;
end;

function TFrameToolSpriteBank.Surfaces: TSpriteBankSurfaceList;
begin
  Result := ScreenSpriteBank.Surfaces;
end;

function TFrameToolSpriteBank.Bodies: TBodyItemList;
begin
  Result := ScreenSpriteBank.Bodies;
end;

function TFrameToolSpriteBank.Postures: TPostureList;
begin
  Result := ScreenSpriteBank.Postures
end;

end.

