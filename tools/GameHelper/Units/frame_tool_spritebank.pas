unit frame_tool_spritebank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, gvector, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Dialogs, u_screen_spritebank, u_surface_list, u_texture_list,
  u_collisionbody_list;

type

  { TFrameToolSpriteBank }

  TFrameToolSpriteBank = class(TFrame)
    Edit1: TEdit;
    Label1: TLabel;
    LB: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    SD1: TSaveDialog;
    BEdit: TSpeedButton;
    BExportToPascalUnit: TSpeedButton;
    procedure BEditClick(Sender: TObject);
    procedure LBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
    procedure BExportToPascalUnitClick(Sender: TObject);
  private
    procedure FillLB;
    procedure ShowSprite(aIndex: integer);
  public
    procedure OnShow;

    function Textures: TTextureList;
    function Surfaces: TSpriteBankSurfaceList;
    function Bodies: TBodyItemList;
  end;

implementation

uses LCLType, Graphics, u_spritebank, u_common, form_main, OGLCScene,
  BGRABitmap, BGRABitmapTypes;

{$R *.lfm}

{ TFrameToolSpriteBank }

procedure TFrameToolSpriteBank.LBSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  ShowSprite(LB.ItemIndex);

  i := LB.ItemIndex;
  if i = -1 then Edit1.Text := ''
    else Edit1.Text := 'T'+SpriteBank.Mutable[i]^.name;
end;

procedure TFrameToolSpriteBank.BExportToPascalUnitClick(Sender: TObject);
var t: TStringlist;
  i, j, c: integer;
  s, sw, sh, sx, sy: string;
  rootItem, current, _parent: PSurfaceDescriptor;
  xx, yy: single;
  bodyItem: PBodyItem;
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

begin
  if LB.ItemIndex = -1 then exit;
  rootItem := Surfaces.GetRootItem;

  s := Trim(Edit1.Text);
  if Length(s) < 2 then exit;
  if s[1] <> 'T' then s := 'T'+s;
  SD1.FileName := Copy(s, 2, Length(Edit1.Text));
  if not SD1.Execute then exit;

  t := TStringlist.Create;

  t.AddText('unit '+ Trim(Edit1.Text)+';'#10+
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
  t.Add('private'#10);
  t.Add('  class var FAdditionalScale: single;');
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

  // methods
  t.AddText('public'#10+
            '  // aAdditionalScale is used to scale the collision bodies'#10+
            '  class procedure LoadTexture(aAtlas: TOGLCTextureAtlas; aAdditionalScale: single=1.0);'#10+
            '  constructor Create(aLayerIndex: integer=-1);'#10+
            'end;'#10+
            #10+
            'implementation'#10+
            #10+
            '{ '+GetClassName+' }'#10+
            #10);

  // procedure load Texture
  t.AddText('class procedure '+GetClassName+'.LoadTexture(aAtlas: TOGLCTextureAtlas; aAdditionalScale: single);'#10+
            'begin'#10+
            '  FAdditionalScale := aAdditionalScale;');
  for i:=0 to Textures.Size-1 do begin
    s := '  '+Textures.Mutable[i]^.name + ' := ';
    if ExtractFileExt(Textures.Mutable[i]^.filename) = '.svg' then begin
      if Textures.Mutable[i]^.width = -1 then sw := '-1'
        else sw := 'ScaleW('+Textures.Mutable[i]^.width.ToString+')';
      if Textures.Mutable[i]^.height = -1 then sh := '-1'
        else sh := 'ScaleH('+Textures.Mutable[i]^.height.ToString+')';

      if Textures.Mutable[i]^.isMultiFrame then
        s := s + '(aAtlas.AddMultiFrameImageFromSVG('''+Textures.Mutable[i]^.filename+''''+
           ', '+sw+', '+sh+
           ', '+(Textures.Mutable[i]^.width div Textures.Mutable[i]^.frameWidth).ToString+
           ', '+(Textures.Mutable[i]^.height div Textures.Mutable[i]^.frameHeight).ToString+
           ', 0);'
      else
        s := s + 'aAtlas.AddFromSVG('''+Textures.Mutable[i]^.filename+''''+', '+sw+', '+sh+');';
    end else begin
      if Textures.Mutable[i]^.isMultiFrame then
        s := s + 'aAtlas.AddMultiFrameImage('''+Textures.Mutable[i]^.filename+''''+
        ', '+(Textures.Mutable[i]^.width div Textures.Mutable[i]^.frameWidth).ToString+
        ', '+(Textures.Mutable[i]^.height div Textures.Mutable[i]^.frameHeight).ToString+');'
      else
        s := s + 'aAtlas.Add('''+Textures.Mutable[i]^.filename+''');';
    end;
    t.Add(s);
  end;
  t.AddText('end;'#10+
            #10);

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
           //#10;
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
        xx := current^.x/current^.surface.Width;
        yy := current^.y/current^.surface.Height;
        sx := FormatFloatWithDot('0.000', xx)+'*'+current^.name+'.Width';
        sy := FormatFloatWithDot('0.000', yy)+'*'+current^.name+'.Height';
      end else begin
        xx := current^.x/_parent^.surface.Width;
        yy := current^.y/_parent^.surface.Height;
        sx := FormatFloatWithDot('0.000', xx)+'*'+_parent^.name+'.Width';
        sy := FormatFloatWithDot('0.000', yy)+'*'+_parent^.name+'.Height';
      end;
      t.Add('    SetCoordinate('+sx+', '+sy+');');
    end;

    if (current^.pivotX <> 0.5) or (current^.pivotY <> 0.5) then
      t.Add('    Pivot := '+PointFToString(current^.pivotX, current^.pivotY)+';');
    if current^.angle <> 0 then
      t.Add('    Angle.Value := '+FormatFloatWithDot('0.00', current^.angle)+';');
    if (current^.scaleX <> 1.0) or (current^.scaleY <> 1.0) then
      t.Add('    Scale.Value := '+PointFToString(current^.scaleX, current^.scaleY)+';');
    t.Add('  end;');
    if i < Surfaces.Size-1 then t.Add('');
  end;//for

  // create collision bodies
  if Bodies.Size > 0 then begin
    t.Add('  // Collision body adjusted with the additional scale value');
    for i:=0 to Bodies.Size-1 do begin
      bodyItem := Bodies.Mutable[i];
      case bodyItem^.BodyType of
        _btPoint: t.Add('  CollisionBody.AddPoint('+PointFToString(bodyItem^.ItemDescriptor.pt)+'*FAdditionalScale);');
        _btLine: t.Add('  CollisionBody.AddLine('+PointFToString(bodyItem^.ItemDescriptor.pt1)+'*FAdditionalScale, '+
                                    PointFToString(bodyItem^.ItemDescriptor.pt2)+'*FAdditionalScale);');
        _btCircle: t.Add('  CollisionBody.AddCircle('+PointFToString(bodyItem^.ItemDescriptor.center)+'*FAdditionalScale, '+
                                      FormatFloatWithDot('0.00', bodyItem^.ItemDescriptor.radius)+'*FAdditionalScale);');
        _btRect: t.Add('  CollisionBody.AddRect(RectF('+PointFToString(bodyItem^.ItemDescriptor.rect.TopLeft)+'*FAdditionalScale, '+
                                          PointFToString(bodyItem^.ItemDescriptor.rect.BottomRight)+'*FAdditionalScale));');
        _btPolygon: begin
          if Length(bodyItem^.ItemDescriptor.pts) = 0 then continue;
          s := '  CollisionBody.AddPolygon([';
          c := 0;
          for j:=0 to High(bodyItem^.ItemDescriptor.pts) do begin
            s := s + PointFToString(bodyItem^.ItemDescriptor.pts[j])+'*FAdditionalScale';
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
begin
  if LB.ItemIndex = -1 then exit;
  FormMain.EditSpriteInSpriteBank(LB.Items.Strings[LB.ItemIndex]);
end;

procedure TFrameToolSpriteBank.FillLB;
var i: SizeUInt;
begin
  LB.Clear;
  if SpriteBank.Size = 0 then exit;
  for i:=0 to SpriteBank.Size do
    LB.Items.Add(SpriteBank.Mutable[i]^.name);
end;

procedure TFrameToolSpriteBank.ShowSprite(aIndex: integer);
begin
  ScreenSpriteBank.ClearView;
  if aIndex <> -1 then
    ScreenSpriteBank.ShowSprite(aIndex);
end;

procedure TFrameToolSpriteBank.OnShow;
begin
  FillLB;
//  if LB.Count > 0 then
//    LB.ItemIndex := 0;
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

end.

