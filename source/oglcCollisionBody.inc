{$ifdef oglcINTERFACE}
{
 To manage the collision between surfaces, a surface have a list of primitive shape
   ((quad), rectangle, circle, line, point) to best describe its collide areas.
 The coordinates of the shapes are relative to the top/left of the surface.
}
type

TOGLCBodyItemType = (_btRect, _btCircle, _btLine, _btPoint, _btPolygon);

{ TOGLCBodyItem }

TOGLCBodyItem = packed record
 BodyType: TOGLCBodyItemType;
{ case TOGLCBodyItemType of
   _btRect: (rect: TRectF);
   _btCircle: (center: TPointF; radius: single);
   _btLine: (pt1, pt2: TPointF);
   _btPoint: (pt: TPointF);   }
  rect: TRectF;      //_btRect
  center: TPointF; radius: single;  //_btCircle
  pt1, pt2: TPointF; //_btLine
  pt: TPointF;       //_btPoint
  pts: array of TPointF; //_btPolygon
 // to transform model coordinates to word coordinates
 function GetTransformed(const aMatrix: TOGLCMatrix): TOGLCBodyItem;
end;
POGLCBodyItem = ^TOGLCBodyItem;

{ TOGLCCollisionBody }
{ To manage the collision between two surfaces, a surface have a list of primitive shape
   (polygon, rectangle, circle, line, point) to best describe its collide areas.
 The coordinates of the shapes are relative to the top/left of the surface.
 To use this feature:
  1) add some shape to best describe the collision body.
  2) to test a collision, call SetSurfaceToWordMatrix() to with a matrix that
     transform surface to world coordinates, then call one of the two CheckCollisionWith()

 EXAMPLE:     // we create a sprite with a collision body
              mySprite1 := TSprite.Create(myTexture, False);
              mySprite1.CollisionBody.AddPolygon(...);
              mySprite1.CollisionBody.AddCircle(...);
              // we create another sprite with a collision body
              mySprite2 := TSprite.Create(myTexture, False);
              mySprite2.CollisionBody.AddCircle(...);
              ...
              to check a collision
              ...
               mySprite1.CollisionBody.SetSurfaceToWordMatrix(mySprite1.GetMatrixSurfaceSpaceToScene);
               mySprite2.CollisionBody.SetSurfaceToWordMatrix(mySprite2.GetMatrixSurfaceSpaceToScene);
               if mySurface1.CollisionBody.CheckCollisionWith(mySprite2) then begin
                 ...code when collision occurs...
               end;
}
TOGLCCollisionBody = class
private type TBodyItems = class(specialize TVector<TOGLCBodyItem>);
private
  FParentSurface: TSimpleSurfaceWithEffect;
  FItems: TBodyItems;
  FSurfaceToWordMatrix: TOGLCMatrix;
  function GetTransformedItem(const aItem: TOGLCBodyItem): TOGLCBodyItem;
public
  constructor Create(aParentSurface: TSimpleSurfaceWithEffect);
  destructor Destroy; override;

  // Define a rectangular area for non rotated surface.
  // Collision check is faster than a polygon.
  // If you rotate the surface, use instead AddPolygon().
  procedure AddRect(const aRect: TRectF);
  procedure AddPolygon(const aPts: ArrayOfTPointF);
  procedure AddCircle(const aCenter: TPointF; aRadius: single);
  procedure AddLine(const aPt1, aPt2: TPointF);
  procedure AddPoint(const aPt: TPointF);

  // Before check for a collision, you must initialize the matrix
  // to convert local coordinates to world coordinates.
  // You can take this matrix with function MySurface.GetMatrixSurfaceToWorld
  procedure SetSurfaceToWordMatrix(const aMatrix: TOGLCMatrix);
  function CheckCollisionWith(aSurface: TSimpleSurfaceWithEffect): boolean; overload;
  function CheckCollisionWith(const aBodyItem: TOGLCBodyItem): boolean; overload;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCBodyItem }

function TOGLCBodyItem.GetTransformed(const aMatrix: TOGLCMatrix): TOGLCBodyItem;
var p: TPointF;
begin
  Result.BodyType := BodyType;
  case BodyType of
    _btPolygon: Result.pts := Copy(aMatrix.Transform(pts), 0, Length(pts));
    _btRect: Result.rect := aMatrix.Transform(rect);
    _btCircle: begin
      Result.center := aMatrix.Transform(center);
      p := aMatrix.Transform(center + PointF(0,-radius));
      Result.radius := Distance(Result.center, p);
    end;
    _btLine: begin
      Result.pt1 := aMatrix.Transform(pt1);
      Result.pt2 := aMatrix.Transform(pt2);
    end;
    _btPoint: Result.pt := aMatrix.Transform(pt);
  end;
end;

{ TOGLCCollisionBody }

procedure TOGLCCollisionBody.AddRect(const aRect: TRectF);
var o: TOGLCBodyItem;
begin
  o.BodyType := _btRect;
  o.rect := aRect;
  FItems.PushBack(o);
end;

procedure TOGLCCollisionBody.AddCircle(const aCenter: TPointF; aRadius: single);
var o: TOGLCBodyItem;
begin
  o.BodyType := _btCircle;
  o.center := aCenter;
  o.radius := aRadius;
  FItems.PushBack(o);
end;

procedure TOGLCCollisionBody.AddLine(const aPt1, aPt2: TPointF);
var o: TOGLCBodyItem;
begin
  o.BodyType := _btLine;
  o.pt1 := aPt1;
  o.pt2 := aPt2;
  FItems.PushBack(o);
end;

procedure TOGLCCollisionBody.AddPoint(const aPt: TPointF);
var o: TOGLCBodyItem;
begin
  o.BodyType := _btPoint;
  o.pt := aPt;
  FItems.PushBack(o);
end;

procedure TOGLCCollisionBody.AddPolygon(const aPts: ArrayOfTPointF);
var o: TOGLCBodyItem;
begin
  o.BodyType := _btPolygon;
  o.pts := aPts;
  FItems.PushBack(o);
end;

function TOGLCCollisionBody.CheckCollisionWith(aSurface: TSimpleSurfaceWithEffect): boolean;
var item: TOGLCBodyItem;
begin
  Result := False;
  // sets the 2 matrix
  //FSurfaceToWordMatrix.CopyFrom(FParentSurface.GetMatrixSurfaceSpaceToScene);
  //aSurface.FCollisionBody.FSurfaceToWordMatrix.CopyFrom(aSurface.GetMatrixSurfaceSpaceToScene);

  for item in aSurface.FCollisionBody.FItems do
    if CheckCollisionWith(aSurface.FCollisionBody.GetTransformedItem(item)) then exit(True);
end;

procedure TOGLCCollisionBody.SetSurfaceToWordMatrix(const aMatrix: TOGLCMatrix);
begin
  FSurfaceToWordMatrix.CopyFrom(aMatrix);
end;

function TOGLCCollisionBody.CheckCollisionWith(const aBodyItem: TOGLCBodyItem): boolean;
var item: TOGLCBodyItem;
  i: integer;
begin
  Result := False;
  // sets the matrix
  //FSurfaceToWordMatrix.CopyFrom(FParentSurface.GetMatrixSurfaceSpaceToScene);

  for i:=0 to FItems.Size-1 do begin
    // transform model coordinates to word coordinates
    item := FItems.Mutable[i]^.GetTransformed(FSurfaceToWordMatrix);
    case item.BodyType of
      _btPolygon: begin
        case aBodyItem.BodyType of
          _btPolygon: Result := Collision.PolygonPolygon(item.pts, aBodyItem.pts);
          _btRect: Result := Collision.PolygonRectF(item.pts, aBodyItem.rect);
          _btCircle: Result := Collision.PolygonCircle(item.pts, aBodyItem.center, aBodyItem.radius);
          _btLine: Result := Collision.PolygonLine(item.pts, aBodyItem.pt1, aBodyItem.pt2);
          _btPoint: Result := Collision.PolygonPoint(item.pts, aBodyItem.pt);
        end;
      end;
      _btRect: begin
        case aBodyItem.BodyType of
          _btPolygon: Result := Collision.PolygonRectF(aBodyItem.pts, item.rect);
          _btRect: Result := Collision.RectFRectF(item.rect, aBodyItem.rect);
          _btCircle: Result := Collision.CircleRectF(aBodyItem.center, aBodyItem.radius, item.rect) or
                               Collision.CircleContainRectF(aBodyItem.center, aBodyItem.radius, item.rect);
          _btLine: Result := Collision.LineRectF(aBodyItem.pt1, aBodyItem.pt2, item.rect);
          _btPoint: Result := Collision.PointRectF(aBodyItem.pt, item.rect);
        end;
      end;
      _btCircle: begin
        case aBodyItem.BodyType of
          _btPolygon: Result := Collision.PolygonCircle(aBodyItem.pts, item.center, item.radius);
          _btRect: Result := Collision.CircleRectF(item.center, item.radius, aBodyItem.rect) or
                             Collision.CircleContainRectF(item.center, item.radius, aBodyItem.rect);
          _btCircle: Result := Collision.CircleCircle(item.center, item.radius, aBodyItem.center, aBodyItem.radius);
          _btLine: Result := Collision.LineCircle(aBodyItem.pt1, aBodyItem.pt2, item.center, item.radius);
          _btPoint: Result := Collision.PointCircle(aBodyItem.pt, item.center, item.radius);
        end;
      end;
      _btLine: begin
        case aBodyItem.BodyType of
          _btPolygon: Result := Collision.PolygonLine(aBodyItem.pts, item.pt1, item.pt2);
          _btRect: Result := Collision.LineRectF(item.pt1, item.pt2, aBodyItem.rect);
          _btCircle: Result := Collision.LineCircle(item.pt1, item.pt2, aBodyItem.center, aBodyItem.radius);
          _btLine: Result := Collision.LineLine(item.pt1, item.pt2, aBodyItem.pt1, aBodyItem.pt2);
          _btPoint: Result := Collision.LinePoint(item.pt1, item.pt2, aBodyItem.pt);
        end;
      end;
      _btPoint: begin
        case aBodyItem.BodyType of
          _btPolygon: Result := Collision.PolygonPoint(aBodyItem.pts, item.pt);
          _btRect: Result := Collision.PointRectF(item.pt, aBodyItem.rect);
          _btCircle: Result := Collision.PointCircle(item.pt, aBodyItem.center, aBodyItem.radius);
          _btLine: Result := Collision.LinePoint(aBodyItem.pt1, aBodyItem.pt2, item.pt);
          _btPoint: Result := item.pt = aBodyItem.pt;
        end;
      end;
    end;

    if Result then exit;
  end;
end;

function TOGLCCollisionBody.GetTransformedItem(const aItem: TOGLCBodyItem): TOGLCBodyItem;
begin
  Result := aItem.GetTransformed(FSurfaceToWordMatrix);
end;

constructor TOGLCCollisionBody.Create(aParentSurface: TSimpleSurfaceWithEffect);
begin
  inherited Create;
  FItems := TBodyItems.Create;
  FParentSurface := aParentSurface;
end;

destructor TOGLCCollisionBody.Destroy;
begin
  FItems.Free;
  FItems := NIL;
  inherited Destroy;
end;


{$endif oglcIMPLEMENTATION}

