unit u_collisionbody_list;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  OGLCScene, gvector,
  BGRABitmap, BGRABitmapTypes,
  u_undo_redo;

type

{ TUINodeHandle }

TUINodeHandle = record
private
  FSprite: TSprite;
  function GetSelected: boolean;
  procedure SetSelected(AValue: boolean);
public
  procedure InitDefault;
  procedure CreateSprite;
  procedure KillSprite;
  function IsOver(aWorldPt: TPointF): boolean;
  procedure UpdatePosition(aWorldPt: TPointF);
  property Selected: boolean read GetSelected write SetSelected;
end;
PUINodeHandle = ^TUINodeHandle;
ArrayOfPUIPointHandle = array of PUINodeHandle;
ArrayOfUIPointHandle = array of TUINodeHandle;

{ TBodyItem }

TBodyItem = record
private
  function PointFToString(const aPt: TPointF): string;
  function StringToPointF(const s: string): TPointF;
  function GetBodyType: TOGLCBodyItemType;
  procedure SetBodyType(AValue: TOGLCBodyItemType);
  procedure UpdateNodesPosition;
public
  Outline: TShapeOutline;
  ItemDescriptor: TOGLCBodyItem;
  Pts: ArrayOfUIPointHandle;
  ParentSurface: TSimpleSurfaceWithEffect;
  PolygonIsClosed: boolean;
  procedure InitDefault;
  procedure CreateSprites;
  procedure KillSprites;

  property BodyType: TOGLCBodyItemType read GetBodyType write SetBodyType;
  procedure UpdateAsLine(aWorldPt1, aWorldPt2: TPointF);
  procedure UpdateAsCircle(aWorldCenter, aWorldPtRadius: TPointF);
  procedure UpdateAsRectangle(aWorldTopLeft, aWorldBottomRight: TPointF);
  procedure UpdateAsPolygon(aWorldPt: TPointF);

  procedure CloseThePolygon;

  function IsOverTheFirstNode(aWorldPt: TPointF): boolean;

  procedure SelectAllNodes;
  procedure UnselectAllNodes;
  function GetNodesAt(aWorldPt: TPointF): ArrayOfPUIPointHandle;
  procedure UpdateSelectedNodePosition(aWolrdOffset: TPointF);
  function SomeNodesAreSelected: boolean;
  function AllNodesAreSelected: boolean;
  function GetNodeSelectedCount: integer;

  procedure UpdateNodeSelectedFrom(aNode: PUINodeHandle; aSelectState: boolean);

  procedure DeleteSelectedNodes;

  // faire attention: on sauve les valeurs en %[0..1] de la hauteur et largeur du sprite
  // les coordonnées des éléments sont en local space du sprite
  function SaveToString: string;
  procedure LoadFromString(const s: string);
end;
PBodyItem = ^TBodyItem;

{ TBodyItemList }

TBodyItemList = class(specialize TVector<TBodyItem>)
  destructor Destroy; override;

  procedure Clear; reintroduce;
  function AddEmpty: PBodyItem;

  procedure UpdateNodesPosition;
  procedure UnselectAllNodes;
  function GetItemAndNodesAt(aWorldPt: TPointF; out aBody: PBodyItem): ArrayOfPUIPointHandle;

  function SelectedNodeBelongToTheSameShape: boolean;

  function SelectedNodesBelongToSinglePolygon(out aPolygon: PBodyItem): boolean;
  //function SelectedNodes

  procedure DeleteShapeWithSelectedNode;
  procedure DeleteItem(aItem: PBodyItem);

  procedure SetParentSurface(aSurface: TSimpleSurfaceWithEffect);

  function SaveToString: string;
  procedure LoadFromString(const s: string);
  procedure SaveTo(t: TStringList);
  procedure LoadFrom(t: TStringList);
end;


type
TCBodyUndoRedoActionType = (
                           cburatUndefined
                           );
TCBodyUndoRedoItem = record
  action: TCBodyUndoRedoActionType;
  ItemDescriptor: TOGLCBodyItem;
end;

{ TTextureUndoRedoManager }

{ TCBodyUndoRedoManager }

TCBodyUndoRedoManager = class(specialize TGenericUndoRedoManager<TCBodyUndoRedoItem>)
  ParentCBodylist: TBodyItemList;
  procedure ProcessUndo(var aItem: TCBodyUndoRedoItem); override;
  procedure ProcessRedo(var aItem: TCBodyUndoRedoItem); override;
end;

implementation

uses u_common, u_ui_atlas, BGRAPath;

{ TUINodeHandle }

function TUINodeHandle.GetSelected: boolean;
begin
  Result := FSprite.Frame = 2;
end;

procedure TUINodeHandle.SetSelected(AValue: boolean);
begin
  if AValue then FSprite.Frame := 2
    else FSprite.Frame := 1;
end;

procedure TUINodeHandle.InitDefault;
begin
  FillChar(Self, SizeOf(TUINodeHandle), 0);
end;

procedure TUINodeHandle.CreateSprite;
begin
  FSprite := FScene.AddSprite(texHandlePathNode, False, LAYER_COLLISION_BODY);
  FSprite.Frame := 1;
end;

procedure TUINodeHandle.KillSprite;
begin
  if FSprite <> NIL then FSprite.Kill;
  FSprite := NIL;
end;

function TUINodeHandle.IsOver(aWorldPt: TPointF): boolean;
var r: TRectF;
begin
  r := FSprite.GetRectAreaInWorldSpace(False);
  Result := FScene.Collision.PointRectF(aWorldPt, r);
end;

procedure TUINodeHandle.UpdatePosition(aWorldPt: TPointF);
begin
  FSprite.SetCenterCoordinate(aWorldPt);
end;

{ TBodyItem }

function TBodyItem.GetBodyType: TOGLCBodyItemType;
begin
  Result := ItemDescriptor.BodyType;
end;

function TBodyItem.PointFToString(const aPt: TPointF): string;
begin
  Result := FormatFloatWithDot('0.0000', aPt.x)+' '+FormatFloatWithDot('0.0000', aPt.y);
end;

procedure TBodyItem.SetBodyType(AValue: TOGLCBodyItemType);
begin
  ItemDescriptor.BodyType := AValue;
end;

function TBodyItem.StringToPointF(const s: string): TPointF;
var A: TStringArray;
begin
  A := s.Split([' ']);
  Result := PointF(StringToSingle(A[0]), StringToSingle(A[1]));
end;

procedure TBodyItem.InitDefault;
begin
  Self := Default(TBodyItem);
end;

procedure TBodyItem.CreateSprites;
begin
  if Outline <> NIL then exit;
  Outline := TShapeOutline.Create(FScene);
  FScene.Add(Outline, LAYER_COLLISION_BODY);
  Outline.Antialiasing := False;
  Outline.LineWidth := 2.0;
  Outline.LineColor := BGRA(255,255,50);

  Pts := NIL;
end;

procedure TBodyItem.KillSprites;
var i: integer;
begin
  Outline.Kill;
  Outline := NIL;
  for i:=0 to High(Pts) do
    Pts[i].KillSprite;
end;

procedure TBodyItem.UpdateNodesPosition;
var p1, p2: TPointF;
  ps, path: ArrayOfTPointF;
  r: single;
  i: integer;
begin
  case ItemDescriptor.BodyType of
    _btLine: begin
      p1 := ParentSurface.SurfaceToScene(ItemDescriptor.pt1);
      p2 := ParentSurface.SurfaceToScene(ItemDescriptor.pt2);
      Outline.SetShapeLine(p1, p2);
      Pts[0].UpdatePosition(p1);
      Pts[1].UpdatePosition(p2);
    end;
    _btCircle: begin
      p1 := ParentSurface.SurfaceToScene(ItemDescriptor.center);
      p2 := ParentSurface.SurfaceToScene(ItemDescriptor.pt1);
      r := Distance(p1, p2); // Max(5, (p2.x - p1.x)); //Distance(p1, p2);
      path := ComputeEllipse(0, 0, r, r, 0.4);
      OutLine.SetShapeCustom(p1.x, p1.y, path);
      Pts[0].UpdatePosition(p1);
      Pts[1].UpdatePosition(p2);
    end;
    _btRect: begin
      p1 := ParentSurface.SurfaceToScene(ItemDescriptor.rect.TopLeft);
      p2 := ParentSurface.SurfaceToScene(ItemDescriptor.rect.BottomRight);
      Outline.SetShapeRectangle(p1.x, p1.y, Round(p2.x-p1.x), Round(p2.y-p1.y));
      Pts[0].UpdatePosition(p1);
      Pts[1].UpdatePosition(p2);
    end;
    _btPolygon: begin
      ps := NIL;
      SetLength(ps, Length(ItemDescriptor.pts));
      for i:=0 to High(ps) do
        ps[i] := ParentSurface.SurfaceToScene(ItemDescriptor.pts[i]);
      //if PolygonIsClosed then ps.ClosePath;
      //ps.ForceTopLeftToOrigin;
   //   p1 := ParentSurface.SurfaceToScene(PointF(0, 0)); //ItemDescriptor.pts[0]);
      p1 := ParentSurface.GetXY;
      OutLine.SetShapeCustom(p1.x, p1.y, ps); // ItemDescriptor.pts); //ps);
      for i:=0 to High(Pts) do
        Pts[i].UpdatePosition(ps[i]);
    end
    else raise exception.create('forgot to implement!');
  end;
end;

procedure TBodyItem.UpdateAsLine(aWorldPt1, aWorldPt2: TPointF);
begin
  CreateSprites;
  if Pts = NIL then begin
    SetLength(Pts, 2);
    Pts[0].InitDefault;
    Pts[0].CreateSprite;
    Pts[1].InitDefault;
    Pts[1].CreateSprite;
  end;

  ItemDescriptor.pt1 := ParentSurface.SceneToSurface(aWorldPt1);
  ItemDescriptor.pt2 := ParentSurface.SceneToSurface(aWorldPt2);
  UpdateNodesPosition;
end;

procedure TBodyItem.UpdateAsCircle(aWorldCenter, aWorldPtRadius: TPointF);
var localPtRadius: TPointF;
begin
  CreateSprites;
  if Pts = NIL then begin
    SetLength(Pts, 2);
    Pts[0].InitDefault;
    Pts[0].CreateSprite;
    Pts[1].InitDefault;
    Pts[1].CreateSprite;
  end;

  localPtRadius := ParentSurface.SceneToSurface(aWorldPtRadius);

  ItemDescriptor.center := ParentSurface.SceneToSurface(aWorldCenter);
  ItemDescriptor.radius := Distance(ItemDescriptor.center, localPtRadius);
  // we keep the local point for radius into pt1
  ItemDescriptor.pt1 := localPtRadius;

  UpdateNodesPosition;
end;

procedure TBodyItem.UpdateAsRectangle(aWorldTopLeft, aWorldBottomRight: TPointF);
begin
  CreateSprites;
  if Pts = NIL then begin
    SetLength(Pts, 2);
    Pts[0].InitDefault;
    Pts[0].CreateSprite;
    Pts[1].InitDefault;
    Pts[1].CreateSprite;
  end;

  ItemDescriptor.rect.TopLeft := ParentSurface.SceneToSurface(aWorldTopLeft);
  ItemDescriptor.rect.BottomRight := ParentSurface.SceneToSurface(aWorldBottomRight);
  UpdateNodesPosition;
end;

procedure TBodyItem.UpdateAsPolygon(aWorldPt: TPointF);
var i: integer;
begin
  CreateSprites;
  SetLength(Pts, Length(Pts)+1);
  Pts[High(Pts)].CreateSprite;
  Pts[High(Pts)].UpdatePosition(aWorldPt);

  i := Length(ItemDescriptor.pts);
  SetLength(ItemDescriptor.pts, i+1);
  ItemDescriptor.pts[i] := ParentSurface.SceneToSurface(aWorldPt);
  UpdateNodesPosition;
end;

procedure TBodyItem.CloseThePolygon;
begin
  PolygonIsClosed := True;
  UpdateAsPolygon(ParentSurface.SurfaceToScene(itemDescriptor.pts[0]));
end;

function TBodyItem.IsOverTheFirstNode(aWorldPt: TPointF): boolean;
begin
  if Length(Pts) = 0 then exit(False)
    else Result := Pts[0].IsOver(aWorldPt);
end;

procedure TBodyItem.SelectAllNodes;
var i: integer;
begin
  for i:=0 to High(Pts) do
    Pts[i].Selected := True;
end;

procedure TBodyItem.UnselectAllNodes;
var i: integer;
begin
  for i:=0 to High(Pts) do
    Pts[i].Selected := False;
end;

function TBodyItem.GetNodesAt(aWorldPt: TPointF): ArrayOfPUIPointHandle;
var i: integer;
begin
  Result := NIL;
  if Length(Pts) = 0 then exit;
  for i:=0 to High(Pts) do
    if Pts[i].IsOver(aWorldPt) then begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := @Pts[i];
    end;
end;

procedure TBodyItem.UpdateSelectedNodePosition(aWolrdOffset: TPointF);
var i: integer;
begin
  if Length(Pts) = 0 then exit;

  case ItemDescriptor.BodyType of
    _btLine: begin
      if Pts[0].Selected then ItemDescriptor.pt1 := ItemDescriptor.pt1 + aWolrdOffset;
      if Pts[1].Selected then ItemDescriptor.pt2 := ItemDescriptor.pt2 + aWolrdOffset;
      UpdateNodesPosition;
    end;
    _btCircle: begin
      if Pts[0].Selected then begin
        ItemDescriptor.center := ItemDescriptor.center + aWolrdOffset;
        ItemDescriptor.pt1 := ItemDescriptor.pt1 + aWolrdOffset;
      end
      else
      if Pts[1].Selected then begin
        ItemDescriptor.pt1 := ItemDescriptor.pt1 + aWolrdOffset;
        ItemDescriptor.pt1.y := ItemDescriptor.center.y;
      end;
      UpdateNodesPosition;
    end;
    _btRect: begin
      if Pts[0].Selected then ItemDescriptor.rect.TopLeft := ItemDescriptor.rect.TopLeft + aWolrdOffset;
      if Pts[1].Selected then ItemDescriptor.rect.BottomRight := ItemDescriptor.rect.BottomRight + aWolrdOffset;
      UpdateNodesPosition;
    end;
    _btPolygon: begin
      for i:=0 to High(Pts) do
        if Pts[i].Selected then
          ItemDescriptor.pts[i] := ItemDescriptor.pts[i] + aWolrdOffset;
      if PolygonIsClosed then ItemDescriptor.pts[High(Pts)] := ItemDescriptor.pts[0];
      UpdateNodesPosition;
    end;
  end;//case
end;

function TBodyItem.SomeNodesAreSelected: boolean;
begin
  Result := GetNodeSelectedCount <> 0;
end;

function TBodyItem.AllNodesAreSelected: boolean;
begin
  Result := GetNodeSelectedCount = Length(Pts);
end;

function TBodyItem.GetNodeSelectedCount: integer;
var i: Integer;
begin
  Result := 0;
  for i:=0 to High(Pts) do
    if Pts[i].Selected then inc(Result);
end;

procedure TBodyItem.UpdateNodeSelectedFrom(aNode: PUINodeHandle; aSelectState: boolean);
var origin, left, right: integer;
  function NextIndex(aIndex: integer): integer;
  begin
    Result := aIndex + 1;
    if Result = Length(Pts) then Result := 0;
  end;
  function PreviousIndex(aIndex: integer): integer;
  begin
    Result := aIndex - 1;
    if Result = -1 then Result := High(Pts);
  end;
begin
  if aNode^.Selected <> aSelectState then begin
    aNode^.Selected := aSelectState;
    exit;
  end;

  if aSelectState and AllNodesAreSelected then exit;
  if not aSelectState and not SomeNodesAreSelected then exit;

  // retrieve the current node index
  for origin:=0 to High(Pts) do
    if @Pts[origin] = aNode then break;

  if aSelectState then begin
    if not aNode^.Selected then begin
      aNode^.Selected := True;
      exit;
    end;
    right := NextIndex(origin);
    left := PreviousIndex(origin);
    repeat
      if right <> origin then begin
        if not Pts[right].Selected then begin
          Pts[right].Selected := True;
          exit;
        end;
        right := NextIndex(right);
      end;
      if left <> origin then begin
        if not Pts[left].Selected then begin
          Pts[left].Selected := True;
          exit;
        end;
        left := PreviousIndex(left);
      end;
    until (right = origin) and (left = origin);
  end else begin
     right := origin + Length(Pts) div 2;
     if right >= Length(Pts) then right := right - Length(Pts);
     left := origin - Length(Pts) div 2;
     if left < 0 then left := left + Length(Pts);
     repeat
       if right <> origin then begin
         if Pts[right].Selected then begin
           Pts[right].Selected := False;
           exit;
         end;
         right := PreviousIndex(right);
       end;
       if left <> origin then begin
         if Pts[left].Selected then begin
           Pts[left].Selected := False;
           exit;
         end;
         left := NextIndex(left);
       end;
     until (right = origin) and (left = origin);
     Pts[origin].Selected := False;
  end;
end;

procedure TBodyItem.DeleteSelectedNodes;
var i: integer;
begin
  if itemDescriptor.BodyType <> _btPolygon then
    raise exception.create('node can be deleted only on polygon!');

  // delete the selected nodes
  for i:=High(Pts) downto 0 do begin
    if Pts[i].Selected then begin
      Pts[i].KillSprite;
      Delete(Pts, i, 1);
      Delete(ItemDescriptor.pts, i, 1);
    end;
  end;

  // reconstruct the polygon
  UpdateNodesPosition;
end;

function TBodyItem.SaveToString: string;
var prop: TProperties;
  s: string;
  i: integer;
begin
  prop.Init('~');
  prop.Add('Type', Ord(ItemDescriptor.BodyType));
  case ItemDescriptor.BodyType of
    _btPoint: begin
      prop.Add('pt', PointFToString(ItemDescriptor.pt));
    end;
    _btLine: begin
      prop.Add('pt1', PointFToString(ItemDescriptor.pt1));
      prop.Add('pt2', PointFToString(ItemDescriptor.pt2));
    end;
    _btCircle: begin
      prop.Add('center', PointFToString(ItemDescriptor.center));
      prop.Add('radius', ItemDescriptor.radius);
    end;
    _btRect: begin
      prop.Add('tl', PointFToString(ItemDescriptor.rect.TopLeft));
      prop.Add('br', PointFToString(ItemDescriptor.rect.BottomRight));
    end;
    _btPolygon: begin
      prop.Add('closed', PolygonIsClosed);
      prop.Add('count', Length(ItemDescriptor.pts));
      s := '';
      for i:=0 to High(ItemDescriptor.pts) do begin
        s := s + PointFToString(ItemDescriptor.pts[i]);
        if i < High(ItemDescriptor.pts) then s := s + ' ';
      end;
      prop.Add('pts', s);
    end;
    else raise exception.create('forgot to implement!');
  end;

  Result := prop.PackedProperty;
end;

procedure TBodyItem.LoadFromString(const s: string);
var prop: TProperties;
  s1: string;
  c, vi, i: integer;
  vs: single;
  A: TStringArray;
begin
  s1 := '';
  vi := 0;
  vs := 0.0;
  c := 0;
  InitDefault;
  prop.Split(s, '~');
  prop.IntegerValueOf('Type', vi, vi);
  ItemDescriptor.BodyType := TOGLCBodyItemType(vi);
  CreateSprites;

  case ItemDescriptor.BodyType of
    _btPoint: begin
      if prop.StringValueOf('pt', s1, '') then
        ItemDescriptor.pt := StringToPointF(s1);
      SetLength(Pts, 1);
    end;
    _btLine: begin
      if prop.StringValueOf('pt1', s1, '') then
        ItemDescriptor.pt1 := StringToPointF(s1);
      if prop.StringValueOf('pt2', s1, '') then
        ItemDescriptor.pt2 := StringToPointF(s1);
      SetLength(Pts, 2);
    end;
    _btCircle: begin
      if prop.StringValueOf('center', s1, '') then
        ItemDescriptor.center := StringToPointF(s1);
      if prop.SingleValueOf('radius', vs, 0) then
        ItemDescriptor.radius := vs;
      SetLength(Pts, 2);
    end;
    _btRect: begin
      if prop.StringValueOf('tl', s1, '') then
        ItemDescriptor.rect.TopLeft := StringToPointF(s1);
      if prop.StringValueOf('br', s1, '') then
        ItemDescriptor.rect.BottomRight := StringToPointF(s1);
      SetLength(Pts, 2);
    end;
    _btPolygon: begin
      prop.BooleanValueOf('closed', PolygonIsClosed, False);
      prop.IntegerValueOf('count', c, 0);
      if c > 0 then begin
        SetLength(Pts, c);
        SetLength(ItemDescriptor.pts, c);
        prop.StringValueOf('pts', s1, '');
        A := s1.Split([' ']);
        for i:=0 to c-1 do
          ItemDescriptor.pts[i] := PointF(StringToSingle(A[i*2]), StringToSingle(A[i*2+1]));
      end;
    end;
    else raise exception.create('forgot to implement!');
  end;

  for i:=0 to High(Pts) do begin
    Pts[i].InitDefault;
    Pts[i].CreateSprite;
  end;
end;

{ TBodyItemList }

destructor TBodyItemList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBodyItemList.Clear;
var i: SizeUInt;
begin
  if Size > 0 then
    for i:=0 to Size-1 do
      Mutable[i]^.KillSprites;
  inherited Clear;
end;

function TBodyItemList.AddEmpty: PBodyItem;
var o: TBodyItem;
begin
  o.InitDefault;
  PushBack(o);
  Result := Mutable[Size-1];
end;

procedure TBodyItemList.UpdateNodesPosition;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Mutable[i]^.UpdateNodesPosition;
end;

procedure TBodyItemList.UnselectAllNodes;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Mutable[i]^.UnselectAllNodes;
end;

function TBodyItemList.SelectedNodeBelongToTheSameShape: boolean;
var i: SizeUInt;
  c: integer;
begin
  if Size = 0 then exit(False);
  c := 0;
  for i:=0 to Size-1 do
    if Mutable[i]^.SomeNodesAreSelected then inc(c);
  Result := c = 1;
end;

function TBodyItemList.SelectedNodesBelongToSinglePolygon(out aPolygon: PBodyItem): boolean;
var i: SizeUInt;
  c: integer;
begin
  Result := False;
  aPolygon := NIL;
  if Size = 0 then exit;

  c := 0;
  for i:=0 to Size-1 do
    if Mutable[i]^.SomeNodesAreSelected then begin
      if Mutable[i]^.BodyType <> _btPolygon then exit(False)
        else begin
          inc(c);
          aPolygon := Mutable[i];
        end;
    end;
  Result := c = 1;
  if not Result then aPolygon := NIL;
end;

procedure TBodyItemList.DeleteShapeWithSelectedNode;
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i]^.SomeNodesAreSelected then begin
      Mutable[i]^.KillSprites;
      Erase(i);
      exit;
    end;
end;

procedure TBodyItemList.DeleteItem(aItem: PBodyItem);
var i: SizeUInt;
begin
  aItem^.KillSprites;
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    if Mutable[i] = aItem then begin
      Erase(i);
      exit;
    end;
end;

procedure TBodyItemList.SetParentSurface(aSurface: TSimpleSurfaceWithEffect);
var i: SizeUInt;
begin
  if Size = 0 then exit;
  for i:=0 to Size-1 do
    Mutable[i]^.ParentSurface := aSurface;
  UpdateNodesPosition;
end;

function TBodyItemList.GetItemAndNodesAt(aWorldPt: TPointF; out aBody: PBodyItem): ArrayOfPUIPointHandle;
var i: SizeUInt;
begin
  Result := NIL;
  aBody := NIL;
  if Size = 0 then exit;

  for i:=0 to Size-1 do begin
    Result := Mutable[i]^.GetNodesAt(aWorldPt);
    if Length(Result) > 0 then begin
      aBody := Mutable[i];
      exit;
    end;
  end;
end;

function TBodyItemList.SaveToString: string;
var prop: TProperties;
  i: SizeUInt;
begin
  prop.Init('|');
  prop.Add('Count', integer(Size));
  if Size > 0 then
    for i:=0 to Size-1 do
      prop.Add('Body'+i.ToString, Mutable[i]^.SaveToString);
  Result := prop.PackedProperty;
end;


procedure TBodyItemList.LoadFromString(const s: string);
var prop: TProperties;
  i: SizeUInt;
  c: integer;
  s1: string;
  o: TBodyItem;
begin
  Clear;
  prop.Split(s, '|');
  c := 0;
  s1 := '';
  prop.IntegerValueOf('Count', c, 0);
  if c = 0 then exit;

  for i:=0 to c-1 do
    if prop.StringValueOf('Body'+i.ToString, s1, '') then begin
      o.LoadFromString(s1);
      PushBack(o);
    end;
end;

const SPRITE_BUILDER_COLLISION_SECTION = '[SPRITE_BUILDER_BODY_COLLISION]';
procedure TBodyItemList.SaveTo(t: TStringList);
begin
  t.Add(SPRITE_BUILDER_COLLISION_SECTION);
  t.Add(SaveToString);
end;

procedure TBodyItemList.LoadFrom(t: TStringList);
var k: integer;
begin
  Clear;
  k := t.IndexOf(SPRITE_BUILDER_COLLISION_SECTION);
  if (k = -1) or (k = t.Count-1) then exit;
  LoadFromString(t.Strings[k+1]);
end;

{ TCBodyUndoRedoManager }

procedure TCBodyUndoRedoManager.ProcessUndo(var aItem: TCBodyUndoRedoItem);
begin

end;

procedure TCBodyUndoRedoManager.ProcessRedo(var aItem: TCBodyUndoRedoItem);
begin

end;

end.

