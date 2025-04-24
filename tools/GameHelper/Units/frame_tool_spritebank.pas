unit frame_tool_spritebank;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Dialogs,
  Types, gvector, u_screen_spritebank, u_surface_list, u_texture_list;

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
  end;

implementation

uses LCLType, Graphics, u_spritebank, u_common, form_main, OGLCScene;

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
  i: integer;
  s, sw, sh: string;
  rootItem, current: PSurfaceDescriptor;
  function GetClassName: string;
  begin
    Result := Trim(Edit1.Text);
  end;
  function GetClassType: string;
  begin
    Result := rootItem^.surface.ClassName;
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
  s := '  class var ';
  for i:=0 to Textures.Size-1 do begin
    s := s + Textures.Mutable[i]^.name;
    if i < Textures.Size-1 then s := s +', ';
    if (i mod 4 = 0) and (i > 0) and (i < Textures.Size-1) then s := s + #10;
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
            '  class procedure LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
            '  constructor Create(aLayerIndex: integer=-1);'#10+
            'end;'#10+
            #10+
            'implementation'#10+
            #10+
            '{ '+GetClassName+' }'#10+
            #10);

  // procedure load Texture
  t.AddText('class procedure '+GetClassName+'.LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
            'begin'#10);
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
  if rootItem <> NIL then
    t.AddText('  SetCoordinate('+FormatFloatWithDot('0.00', rootItem^.x)+', '+
                                 FormatFloatWithDot('0.00', rootItem^.y)+');'#10+
              '  Pivot := PointF('+FormatFloatWithDot('0.00', rootItem^.pivotX)+', '+
                                   FormatFloatWithDot('0.00', rootItem^.pivotY)+');'#10+
              '  Angle.Value := '+FormatFloatWithDot('0.00', rootItem^.angle)+';'#10+
              '  Scale.Value := PointF('+FormatFloatWithDot('0.00', rootItem^.scaleX)+', '+
                                         FormatFloatWithDot('0.00', rootItem^.scaleY)+');'#10#10)
  else t.Add('');

  // creating childs
  for i:=0 to Surfaces.Size-1 do begin
    current := Surfaces.Mutable[i];
    // copy values to variable
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
    if rootItem = NIL then begin
      if current^.parentID = -1 then s := 'Self'
        else s := Surfaces.GetItemByID(current^.parentID)^.name;
    end else begin
      if current^.parentID = rootItem^.id then s := 'Self'
        else s := Surfaces.GetItemByID(current^.parentID)^.name;
    end;
    t.AddText('  with '+current^.name+' do begin'#10+
              '    SetChildOf('+s+', '+current^.zOrder.ToString+');'#10+
              '    SetCoordinate('+FormatFloatWithDot('0.00', current^.x)+', '+
                                   FormatFloatWithDot('0.00', current^.y)+');'#10+
              '    Pivot := PointF('+FormatFloatWithDot('0.00', current^.pivotX)+', '+
                                     FormatFloatWithDot('0.00', current^.pivotY)+');'#10+
              '    Angle.Value := '+FormatFloatWithDot('0.00', current^.angle)+';'#10+
              '    Scale.Value := PointF('+FormatFloatWithDot('0.00', current^.scaleX)+', '+
                                     FormatFloatWithDot('0.00', current^.scaleY)+');'#10+
              '  end;');
    if i < Surfaces.Size-1 then t.Add('');
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

end.

