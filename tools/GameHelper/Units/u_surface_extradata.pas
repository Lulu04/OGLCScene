unit u_surface_extradata;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, Types,
  OGLCScene,BGRABitmap, BGRABitmapTypes;

type

{ TSpriteExtra }
// for TSprite, TSpriteWithElasticCorner, TTiledSprite, TScrollableSprite
TSpriteExtra = record
  TextureName: string;   // the filename+extension without path
  procedure SaveTo(const prop: TProperties);
  procedure LoadFrom(const prop: TProperties);
end;

TPolarSpriteExtra = record
 TextureName: string;   // the filename+extension without path
 PolarCenter: TPointF;
 PolarDistance, PolarAngle: single;
 procedure SaveTo(const prop: TProperties);
 procedure LoadFrom(const prop: TProperties);
end;


{ TShapeOutlineDefinition }

TShapeOutlineDefinition = class(TShapeOutline)
  Shape: TOGLCPath;
  procedure SaveTo(const prop: TProperties);
  procedure LoadFrom(const prop: TProperties);
end;

TGradientRectangleDefinition = class(TGradientRectangle)

end;

TQuad4ColorDefinition = class(TQuad4Color)
  TLColor, TRColor, BRColor, BLColor: TBGRAPixel;
  TL, TR, BR, BL: TPointF;
//  procedure SaveTo(const prop: TProperties);
//  procedure LoadFrom(const prop: TProperties);
end;

TDeformationGridDefinition = class(TDeformationGrid)
end;

TSpriteContainerDefinition = class(TSpriteContainer)
end;

implementation

uses u_common;

{ TSpriteExtra }

procedure TSpriteExtra.SaveTo(const prop: TProperties);
begin
  prop.Add('TextureName', TextureName);
end;

procedure TSpriteExtra.LoadFrom(const prop: TProperties);
begin
  prop.StringValueOf('TextureName', TextureName, '');
end;

{ TPolarSpriteExtra }

procedure TPolarSpriteExtra.SaveTo(const prop: TProperties);
begin
  prop.Add('TextureName', TextureName);
  prop.Add('PolarCenterX', PolarCenter.x);
  prop.Add('PolarCenterY', PolarCenter.y);
  prop.Add('PolarAngle', PolarAngle);
  prop.Add('PolarDistance', PolarDistance);
end;

procedure TPolarSpriteExtra.LoadFrom(const prop: TProperties);
var v: single=0;
begin
  prop.StringValueOf('TextureName', TextureName, '');
  prop.SingleValueOf('PolarCenterX', v, 0);
  PolarCenter.x := v;
  prop.SingleValueOf('PolarCenterY', v, 0);
  PolarCenter.y := v;
  prop.SingleValueOf('PolarAngle', PolarAngle, 0);
  prop.SingleValueOf('PolarDistance', PolarDistance, 0);
end;

{ TShapeOutlineDefinition }

procedure TShapeOutlineDefinition.SaveTo(const prop: TProperties);
var i: integer;
begin
  prop.Add('ShapeNodeCount', Length(Shape));
  for i:=0 to High(Shape) do
    prop.Add('ShapeN'+i.ToString, FormatFloatWithDot('0.000', Shape[i].x)+' '+
                                  FormatFloatWithDot('0.000', Shape[i].y));
end;

procedure TShapeOutlineDefinition.LoadFrom(const prop: TProperties);
var i, c: integer;
  s: string='';
  A: TStringArray;
begin
  c := 0;
  prop.IntegerValueOf('ShapeNodeCount', c, 0);
  Shape := NIL;
  if c = 0 then exit;
  SetLength(Shape, c);
  for i:=0 to c-1 do begin
    prop.StringValueOf('ShapeN'+i.ToString, s, '');
    A := s.Split([' ']);
    Shape[i].x := StringToSingle(A[0]);
    Shape[i].y := StringToSingle(A[1]);
  end;
end;


end.

