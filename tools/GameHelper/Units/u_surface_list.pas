unit u_surface_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, StdCtrls,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_texture_list, u_ui_handle;

type

classOfSimpleSurfaceWithEffect = class of TSimpleSurfaceWithEffect;
TSurfaceList = class;
{ TSurfaceDescriptor }

TSurfaceDescriptor = record
private
  HandleManager: TUIHandleManager;
  FAngleOrigin: single; // used to rotate the surface
  function GetPivot: TPointF;
  function GetSelected: boolean;
  procedure SetPivot(AWorldCoor: TPointF);
  procedure SetSelected(AValue: boolean);
public
  ParentList: TSurfaceList;
  id: integer;           // unique ID
  parentID: integer;     // parentID = -1 means the surface is child of the TSpriteContainer root.
  name: string;          // by default same as textureName
  surface: TSimpleSurfaceWithEffect;
  textureName: string;   // the filename+extension without path
  classtype: classOfSimpleSurfaceWithEffect;
  // temporary variables used when loading
  pivotX, pivotY, angle, x, y, scaleX, scaleY: single;
  zOrder: integer;
  procedure InitDefault;
  procedure KillSurface;
  procedure CreateSurface;
  procedure SetChildDependency;
  procedure SetValuesFromTemporaryVariables; // sets x, y, pivot,...
  procedure DuplicateValuesToTemporaryVariables;

  function IsOverRotateHandle(aWorldPt: TPointF): boolean;
  function IsOverPivotHandle(aWorldPt: TPointF): boolean;
  procedure AddOffsetToPivot(aOffset: TPointF); // in world coordinates
  procedure UpdateHandlePosition;
  procedure ToogleSelectedAndRotatedHandle;

  procedure SaveCurrentAngleBeforeRotation;
  procedure ComputeAngle(aReferencePointInWorld: TPointF; aUseIncrement: boolean);

  property Selected: boolean read GetSelected write SetSelected;
  property Pivot: TPointF read GetPivot write SetPivot;

  function SaveToString: string;
  procedure LoadFromString(const s: string);

end;
PSurfaceDescriptor = ^TSurfaceDescriptor;
ArrayOfPSurfaceDescriptor = array of PSurfaceDescriptor;

{ TSurfaceList }

TSurfaceList = class(specialize TVector<TSurfaceDescriptor>)
private
  FID: integer;
  function NextID: integer;
  function GetSortedSurfaceFromNearestTofurthest: ArrayOfPSurfaceDescriptor;
public
  procedure Clear; reintroduce;
  function AddEmpty: PSurfaceDescriptor;

  function GetItemByID(aID: integer): PSurfaceDescriptor;
  function GetItemIndexByID(aID: integer): integer;
  function GetByIndex(aIndex: integer): PSurfaceDescriptor;
  function GetItemBySurface(aSurface: TSimpleSurfaceWithEffect): PSurfaceDescriptor;

  // Returns the items located at the specified position.
  // Items are sorted from nearest to furthest away.
  function GetItemsAt(aX, aY: integer): ArrayOfPSurfaceDescriptor;

  function NameExists(const aName: string): boolean;
  function TextureNameisUsedByASurface(const aTextureName: string): boolean;

  procedure DeleteItemByID(aID: integer);

  procedure SelectNone;

  function SaveToString: string;
  procedure LoadFromString(const s: string);
  procedure SaveTo(t:TStringList);
  procedure LoadFrom(t:TStringList);

  // items are the ID of each TSurfaceDescriptor
  procedure FillComboBox(aCB: TComboBox);
  procedure FillListBox(aLB: TListBox);

  function Textures: TTextureList; virtual; abstract;
end;



implementation

uses u_common, u_screen_spritebuilder, u_utils;

{ TSurfaceDescriptor }

function TSurfaceDescriptor.GetSelected: boolean;
begin
  Result := HandleManager.IsVisible;
end;

function TSurfaceDescriptor.GetPivot: TPointF;
begin
  Result := surface.Pivot;
end;

procedure TSurfaceDescriptor.SetPivot(AWorldCoor: TPointF);
var p :TPointF;
begin
  p := surface.SceneToSurface(AWorldCoor);
  p.x := p.x / surface.Width;
  p.y := p.y / surface.Height;
  surface.Pivot.x := p.x;
  surface.Pivot.y := p.y;
  HandleManager.ShowPivotHandle(surface);
end;

procedure TSurfaceDescriptor.SetSelected(AValue: boolean);
begin
  if AValue then HandleManager.ToogleSelectedAndRotatedHandle(surface)
    else HandleManager.HideAll;
end;

procedure TSurfaceDescriptor.InitDefault;
begin
  Surface := NIL;
  id := -1;
  parentID := -1;
  textureName := '';
  classtype := TSimpleSurfaceWithEffect;
  HandleManager.InitDefault;
end;

procedure TSurfaceDescriptor.KillSurface;
begin
  if surface <> NIL then surface.Kill;
  surface := NIL;
  HandleManager.KillSurfaces;
end;

procedure TSurfaceDescriptor.CreateSurface;
var texItem: PTextureItem;
  tex: PTexture;
  path: TOGLCPath;
begin
  texItem := ParentList.Textures.GetItemByName(textureName);
  if texItem <> NIL then tex := texItem^.texture else tex := NIL;

  if classType = TSprite then
    surface := TSprite.Create(tex, False)
  else
  if classType = TDeformationGrid then
    surface := TDeformationGrid.Create(tex, False)
  else
  if classType = TSpriteContainer then
    surface := TSpriteContainer.Create(FScene)
  else raise exception.create('forgot to implement!');

  // collision for selection
  surface.CollisionBody.AddPolygon([PointF(0,0), PointF(surface.Width, 0),
                                    PointF(surface.Width, surface.Height), PointF(0, surface.Height)]);

  UIHandle.CreateUIHandles(@HandleManager);
end;

procedure TSurfaceDescriptor.SetChildDependency;
var parentItem: PSurfaceDescriptor;
begin
  if parentID = -1 then surface.SetChildOf(FContainer, zOrder)
  else begin
      parentItem := ParentList.GetItemByID(parentID);
      if parentItem = NIL
        then raise exception.create('parent with ID='+parentID.ToString+' not found !')
        else begin
          surface.SetChildOf(parentItem^.surface, zOrder);
        end;
  end;
end;

procedure TSurfaceDescriptor.SetValuesFromTemporaryVariables;
begin
  surface.X.Value := x;
  surface.Y.Value := y;
  surface.Pivot.x := pivotX;
  surface.Pivot.y := pivotY;
  surface.Scale.x.Value := scaleX;
  surface.Scale.y.Value := scaleY;
  surface.Angle.Value := angle;
end;

procedure TSurfaceDescriptor.DuplicateValuesToTemporaryVariables;
begin
  pivotX := surface.Pivot.x;
  pivotY := surface.Pivot.y;
  angle := surface.Angle.Value;
  x := surface.X.Value;
  y := surface.Y.Value;
  scaleX := surface.Scale.X.Value;
  scaleY := surface.Scale.Y.Value;
  zOrder := surface.ZOrderAsChild;
end;

function TSurfaceDescriptor.IsOverRotateHandle(aWorldPt: TPointF): boolean;
begin
  Result := HandleManager.IsOverRotateHandle(aWorldPt);
end;

function TSurfaceDescriptor.IsOverPivotHandle(aWorldPt: TPointF): boolean;
begin
  Result := HandleManager.IsOverPivotHandle(aWorldPt);
end;

procedure TSurfaceDescriptor.SaveCurrentAngleBeforeRotation;
begin
  FAngleOrigin := surface.Angle.Value;
end;

procedure TSurfaceDescriptor.ComputeAngle(aReferencePointInWorld: TPointF;
  aUseIncrement: boolean);
var ppivot: TPointF;
  a: single;
begin
  with surface do
    ppivot := SurfaceToScene(PointF(Width*Pivot.x, Height*Pivot.y));

  a := FAngleOrigin + CartesianToPolar(ppivot, aReferencePointInWorld).Angle;
  if aUseIncrement then
    a := Trunc(a / 15) * 15;
  surface.Angle.Value := a;
end;

procedure TSurfaceDescriptor.AddOffsetToPivot(aOffset: TPointF);
var ppivot: TPointF;
begin
  with surface do
    ppivot := SurfaceToScene(PointF(Width*Pivot.x, Height*Pivot.y));
  ppivot := ppivot + aOffset;
  Pivot := ppivot;
end;

procedure TSurfaceDescriptor.UpdateHandlePosition;
begin
  HandleManager.UpdateHandlePosition(surface);
end;

procedure TSurfaceDescriptor.ToogleSelectedAndRotatedHandle;
begin
  HandleManager.ToogleSelectedAndRotatedHandle(surface);
end;

function TSurfaceDescriptor.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('~');
  prop.Add('ID', id);
  prop.Add('ParentID', parentID);
  prop.Add('ZOrder', surface.ZOrderAsChild);
  prop.Add('Name', name);
  prop.Add('Classtype', classtype.ClassName);
  prop.Add('TextureName', textureName);
  prop.Add('PivotX', surface.Pivot.x);
  prop.Add('PivotY', surface.Pivot.y);
  prop.Add('Angle', surface.Angle.Value);
  prop.Add('X', surface.X.Value);
  prop.Add('Y', surface.Y.Value);
  prop.Add('ScaleX', surface.Scale.X.Value);
  prop.Add('ScaleY', surface.Scale.Y.Value);

  Result := prop.PackedProperty;
end;

procedure TSurfaceDescriptor.LoadFromString(const s: string);
var prop: TProperties;
  s1: string;
begin
  s1 := '';
  InitDefault;
  prop.Split(s, '~');
  prop.IntegerValueOf('ID', id, id);
  prop.IntegerValueOf('ParentID', parentID, parentID);
  prop.IntegerValueOf('ZOrder', zOrder, zOrder);
  prop.StringValueOf('Name', name, 'noname');
  prop.StringValueOf('Classtype', s1, 'TSimpleSurfaceWithEffect');
  case s1 of
    'TSprite': classtype := TSprite;
    'TDeformationGrid': classtype := TDeformationGrid;
    'TSpriteContainer': classtype := TSpriteContainer;
    else exception.create('forgot to implement!');
  end;
  prop.StringValueOf('TextureName', textureName, textureName);

  prop.SingleValueOf('PivotX', pivotX, 0.5);
  prop.SingleValueOf('PivotY', pivotY, 0.5);
  prop.SingleValueOf('Angle', angle, 0.0);
  prop.SingleValueOf('X', x, 0.0);
  prop.SingleValueOf('Y', y, 0.0);
  prop.SingleValueOf('ScaleX', scaleX, 0.0);
  prop.SingleValueOf('ScaleY', scaleY, 0.0);
end;

{ TSurfaceList }

function TSurfaceList.NextID: integer;
begin
  inc(FID);
  Result := FID;
end;

function TSurfaceList.GetSortedSurfaceFromNearestTofurthest: ArrayOfPSurfaceDescriptor;
  procedure AddToResult(item: PSurfaceDescriptor);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := item;
  end;
  procedure CheckRecursive(aSurface: TSimpleSurfaceWithEffect);
  var i: integer;
    o: TSimpleSurfaceWithEffect;
    item: PSurfaceDescriptor;
  begin
    for i:=aSurface.ChildCount-1 downto 0 do begin
      o := aSurface.Childs[i];
      if o.ChildCount > 0 then CheckRecursive(o);
      item := GetItemBySurface(o);
      if item <> NIL then AddToResult(item);
    end;
  end;
begin
  Result := NIL;
  if Size = 0 then exit;
  CheckRecursive(FContainer);
end;

procedure TSurfaceList.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do
      Mutable[i]^.KillSurface;
  inherited Clear;
  FID := 0;
end;

function TSurfaceList.AddEmpty: PSurfaceDescriptor;
var o: TSurfaceDescriptor;
begin
  o.InitDefault;
  o.ParentList := Self;
  o.id := NextID;
  PushBack(o);
  Result := Mutable[Size-1];
end;

function TSurfaceList.GetItemByID(aID: integer): PSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;

  for i:=0 to Size-1 do
    if Mutable[i]^.id = aID then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSurfaceList.GetItemIndexByID(aID: integer): integer;
var i: SizeUInt;
begin
  Result := -1;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.id = aID then begin
      Result := i;
      exit;
    end;
end;

function TSurfaceList.GetByIndex(aIndex: integer): PSurfaceDescriptor;
begin
  Result := NIL;
  if Size = 0 then exit;
  Result := Mutable[aIndex];
end;

function TSurfaceList.GetItemBySurface(aSurface: TSimpleSurfaceWithEffect): PSurfaceDescriptor;
var i: SizeUInt;
begin
  Result := NIL;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.surface = aSurface then begin
      Result := Mutable[i];
      exit;
    end;
end;

function TSurfaceList.GetItemsAt(aX, aY: integer): ArrayOfPSurfaceDescriptor;
var collisionPoint: TOGLCBodyItem;
  sorted: ArrayOfPSurfaceDescriptor;
  i: integer;
  o: TSimpleSurfaceWithEffect;
begin
  Result := NIL;
  if Size = 0 then exit;

  collisionPoint.BodyType := _btPoint;
  collisionPoint.pt := PointF(aX, aY);

  sorted := GetSortedSurfaceFromNearestTofurthest;
  for i:=0 to High(sorted) do begin
    o := sorted[i]^.surface;
    o.CollisionBody.SetSurfaceToWordMatrix(o.GetMatrixSurfaceToWorld);
    if o.CollisionBody.CheckCollisionWith(collisionPoint) then begin
      SetLength(Result, length(Result)+1);
      Result[High(Result)] := sorted[i];
    end;
  end;
end;

function TSurfaceList.NameExists(const aName: string): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.name = aName then exit(True);
end;

function TSurfaceList.TextureNameisUsedByASurface(const aTextureName: string): boolean;
var i: SizeUInt;
begin
  Result := False;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.textureName = aTextureName then exit(True);
end;

procedure TSurfaceList.DeleteItemByID(aID: integer);
var i: integer;
begin
  i := GetItemIndexByID(aID);
  if i <> -1 then Self.Erase(i);
end;

procedure TSurfaceList.SelectNone;
var i: SizeUInt;
begin
  if Size > 0 then
   for i:=0 to Size-1 do
     Mutable[i]^.Selected := False;
end;

function TSurfaceList.SaveToString: string;
var prop: TProperties;
  i: SizeUInt;
begin
  prop.Init('|');
  prop.Add('Count', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Surface'+i.ToString, Mutable[i]^.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TSurfaceList.LoadFromString(const s: string);
var prop: TProperties;
  i: SizeUInt;
  c: integer;
  s1: string;
  o: TSurfaceDescriptor;
begin
  Clear;
  prop.Split(s, '|');
  c := 0;
  s1 := '';
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;

  for i:=0 to c-1 do
    if prop.StringValueOf('Surface'+i.ToString, s1, '') then begin
      o.ParentList := Self;
      o.LoadFromString(s1);
      PushBack(o);
    end;

  // update FID
  for i:=0 to Size-1 do
    with Mutable[i]^ do
      if FID < id then FID := id;

  // create the surfaces and init its values
  for i:=0 to Size-1 do
    with Mutable[i]^ do begin
      CreateSurface;
      SetValuesFromTemporaryVariables;
    end;

  // create the childs dependencies
  for i:=0 to Size-1 do
    with Mutable[i]^ do
      SetChildDependency;
end;

const SPRITE_BUILDER_SURFACES_SECTION = '[SPRITE_BUILDER_SURFACES]';
procedure TSurfaceList.SaveTo(t: TStringList);
begin
  t.Add(SPRITE_BUILDER_SURFACES_SECTION);
  t.Add(SaveToString);
end;

procedure TSurfaceList.LoadFrom(t: TStringList);
var k: integer;
begin
  Clear;
  k := t.IndexOf(SPRITE_BUILDER_SURFACES_SECTION);
  if (k = -1) or (k = t.Count-1) then exit;
  LoadFromString(t.Strings[k+1]);
end;

procedure TSurfaceList.FillComboBox(aCB: TComboBox);
var i: SizeUInt;
begin
  aCB.Clear;
  aCB.Items.Add('Root sprite container');
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aCB.Items.Add(Mutable[i]^.id.ToString);
end;

procedure TSurfaceList.FillListBox(aLB: TListBox);
var i: SizeUInt;
begin
  aLB.Clear;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    aLB.Items.Add(Mutable[i]^.name);
end;

end.

