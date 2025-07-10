{$ifdef oglcINTERFACE}
{
   Make an explosion effect with a texture.
   To do this, the texture is splitted in multiple rectangular parts. Each part
   is associated to a TPolarSprite.

   aScene: the parent scene.
   aLayerIndex: if <> -1, each created sprite will be added to this layer
   aTexture: texture used to perform the effect. Can be a part of another.
   aRowCount, aColumCount: defines the number of divisions of the texture
   aCenter: coordinate of the center of the explosion in world coordinates
   aCenterVariation: variation applyed randomly to distribute the sprite around the center
   AScaleValue: scale value applyed on each piece
   aRotationMin, aRotationMax: a constant rotation is added to each part, randomly calculated from this range
   aDistance: distance traveled by each piece of texture (in pixels)
   aDistanceVariation: random value between [0..aDistanceVariation] added to aDistance
   aDuration: time to travel this distance
   aCurveID: velocity curve applyed on each piece
   aChangeOpacityToZero: if true the opacity of each created sprite will change to 0.
   aAutoKillPart: if true a KillDefered(aDuration) is applyed on each sprite created.
}
function ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
                        aTexture: PTexture; aRowCount, aColumCount: integer;
                        aCenter: TPointF; aCenterVariation: single;
                        AScaleValue: TPointF;
                        aRotationMin, aRotationMax: single;
                        aDistance, aDistanceVariation: single;
                        aDuration: single;
                        aCurveID: word = idcStartFastEndSlow;
                        aChangeOpacityToZero: boolean = True;
                        aAutoKillPart: boolean=True): TArrayOfArrayOfTPolarSprite;

{
   Make an explosion effect with a texture.
   To do this, the texture is splitted in multiple rectangular parts. Each part
   is associated to a TPolarSprite and sets child of the specified surface.

   aSurface: the parent surface for the created sprites.
   aZOrder: the child Z order for the created sprites.
   aTexture: texture used to perform the effect. Can be a part of another.
   aRowCount, aColumCount: defines the number of divisions of the texture
   aCenter: coordinate of the center of the explosion in surface coordinates.
   aCenterVariation: variation applyed randomly to distribute the sprite around the center
   AScaleValue: scale value applyed on each piece
   aRotationMin, aRotationMax: a constant rotation is added to each part, randomly calculated from this range
   aDistance: distance traveled by each piece of texture (in pixels)
   aDistanceVariation: random value between [0..aDistanceVariation] added to aDistance
   aDuration: time to travel this distance
   aCurveID: velocity curve applyed on each piece
   aChangeOpacityToZero: if true the opacity of each created sprite will change to 0.
   aAutoKillPart: if true a KillDefered(aDuration) is applyed on each sprite created.
} function ExplodeTextureToChildOf(aSurface: TSimpleSurfaceWithEffect; aZOrder: integer;
                                   aTexture: PTexture; aRowCount, aColumCount: integer;
                                   aCenter: TPointF; aCenterVariation: single;
                                   AScaleValue: TPointF;
                                   aRotationMin, aRotationMax: single;
                                   aDistance, aDistanceVariation: single;
                                   aDuration: single;
                                   aCurveID: word = idcStartFastEndSlow;
                                   aChangeOpacityToZero: boolean = True;
                                   aAutoKillPart: boolean=True): TArrayOfArrayOfTPolarSprite;

{
   Make an explosion effect with a texture.
   To do this, the texture is splitted in multiple rectangular parts. Each part
   is associated to a TPolarSprite.
   In this version, you can control the angle to spread the parts.

   aScene: the parent scene.
   aLayerIndex: if <> -1, each created sprite will be added to this layer
   aTexture: texture used to perform the effect. Can be a part of another.
   aRowCount, aColumCount: defines the number of divisions of the texture
   aTopLeft: the top left coordinate of the sprite to explode in world coordinates
   AScaleValue: scale value applyed on each piece
   aRotationMin, aRotationMax: a constant rotation is added to each part, randomly calculated from this range
   aSpreadAngleMin, aSpreadAngleMax: define the cone to spread the parts. Angle 0 is to the right, CW.
   aDistance: distance traveled by each piece of texture (in pixels)
   aDistanceVariation: random value between [0..aDistanceVariation] added to aDistance
   aDuration: time to travel this distance
   aCurveID: velocity curve applyed on each piece
   aChangeOpacityToZero: if true the opacity of each created sprite will change to 0.
   aAutoKillPart: if true a KillDefered(aDuration) is applyed on each sprite created.
}
function ExplodeTextureDirectionnal(aScene: TOGLCScene; aLayerIndex: integer;
                        aTexture: PTexture; aRowCount, aColumCount: integer;
                        aTopLeft: TPointF;
                        AScaleValue: TPointF;
                        aRotationMin, aRotationMax: single;
                        aSpreadAngleMin, aSpreadAngleMax: single;
                        aDistance, aDistanceVariation: single;
                        aDuration: single;
                        aCurveID: word = idcStartFastEndSlow;
                        aChangeOpacityToZero: boolean = True;
                        aAutoKillPart: boolean=True): TArrayOfArrayOfTPolarSprite;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
  aTexture: PTexture; aRowCount, aColumCount: integer; aCenter: TPointF;
  aCenterVariation: single; AScaleValue: TPointF; aRotationMin,
  aRotationMax: single; aDistance, aDistanceVariation: single;
  aDuration: single; aCurveID: word; aChangeOpacityToZero: boolean;
  aAutoKillPart: boolean): TArrayOfArrayOfTPolarSprite;
var r: TRect;
  ro, co: Integer;
  cellw,cellh: single;
  TP: TPointF;
  tex: PTexture;
begin
 aColumCount := EnsureRange(aColumCount, 2, aTexture^.TextureWidth);
 aRowCount := EnsureRange(aRowCount, 2, aTexture^.TextureHeight);

 Result := NIL;
 SetLength(Result, aRowCount, aColumCount);

 cellw := aTexture^.FrameWidth/aColumCount;
 cellh := aTexture^.FrameHeight/aRowCount;

 TP := aTexture^.GetTopLeftCoor(0);

 for ro:=0 to aRowCount-1 do
   for co:=0 to aColumCount-1 do begin

     r := Rect(round(cellw*co + TP.x),
               round(cellh*ro + TP.y),
               round(cellw*co+cellw+TP.x), round(cellh*ro+cellh+TP.y));

     tex := new(PTexture);
     tex^.TakePartOf(aTexture, r);

     Result[ro][co] := TPolarSprite.Create(tex, True);
     with Result[ro][co] do begin
       Polar.Center.Value := aCenter;
       Polar.CartesianValue := aCenter+PointF(random*aCenterVariation-aCenterVariation*0.5, random*aCenterVariation-aCenterVariation*0.5);
       Polar.Distance.ChangeTo(aDistance+random*aDistanceVariation, aDuration, aCurveID);
       if aAutoKillPart then KillDefered(aDuration);
       Scale.Value := AScaleValue;
       Angle.AddConstant(aRotationMin+ (aRotationMax-aRotationMin)*Random);
       if aChangeOpacityToZero then Opacity.ChangeTo(0, aDuration, idcStartSlowEndFast);
     end;

     if aLayerIndex <> -1 then aScene.Add(Result[ro][co], aLayerIndex);
   end;
end;

function ExplodeTextureToChildOf(aSurface: TSimpleSurfaceWithEffect;
  aZOrder: integer; aTexture: PTexture; aRowCount, aColumCount: integer;
  aCenter: TPointF; aCenterVariation: single; AScaleValue: TPointF;
  aRotationMin, aRotationMax: single; aDistance, aDistanceVariation: single;
  aDuration: single; aCurveID: word; aChangeOpacityToZero: boolean;
  aAutoKillPart: boolean): TArrayOfArrayOfTPolarSprite;
var r: TRect;
  ro, co: Integer;
  cellw,cellh: single;
  TP: TPointF;
  tex: PTexture;
begin
 aColumCount := EnsureRange(aColumCount, 2, aTexture^.TextureWidth);
 aRowCount := EnsureRange(aRowCount, 2, aTexture^.TextureHeight);

 Result := NIL;
 SetLength(Result, aRowCount, aColumCount);

 cellw := aTexture^.FrameWidth/aColumCount;
 cellh := aTexture^.FrameHeight/aRowCount;

 TP := aTexture^.GetTopLeftCoor(0);

 for ro:=0 to aRowCount-1 do
   for co:=0 to aColumCount-1 do begin

     r := Rect(round(cellw*co + TP.x),
               round(cellh*ro + TP.y),
               round(cellw*co+cellw+TP.x), round(cellh*ro+cellh+TP.y));

     tex := new(PTexture);
     tex^.TakePartOf(aTexture, r);

     Result[ro][co] := TPolarSprite.Create(tex, True);
     with Result[ro][co] do begin
       Polar.Center.Value := aCenter;
       Polar.CartesianValue := aCenter+PointF(random*aCenterVariation-aCenterVariation*0.5, random*aCenterVariation-aCenterVariation*0.5);
       Polar.Distance.ChangeTo(aDistance+random*aDistanceVariation, aDuration, aCurveID);
       if aAutoKillPart then KillDefered(aDuration);
       Scale.Value := AScaleValue;
       Angle.AddConstant(aRotationMin+ (aRotationMax-aRotationMin)*Random);
       if aChangeOpacityToZero then Opacity.ChangeTo(0, aDuration, idcStartSlowEndFast);
       aSurface.AddChild(Result[ro][co], aZOrder);
     end;
   end;
end;

function ExplodeTextureDirectionnal(aScene: TOGLCScene; aLayerIndex: integer;
  aTexture: PTexture; aRowCount, aColumCount: integer; aTopLeft: TPointF;
  AScaleValue: TPointF; aRotationMin, aRotationMax: single; aSpreadAngleMin,
  aSpreadAngleMax: single; aDistance, aDistanceVariation: single;
  aDuration: single; aCurveID: word; aChangeOpacityToZero: boolean;
  aAutoKillPart: boolean): TArrayOfArrayOfTPolarSprite;
var r: TRect;
  ro, co: Integer;
  cellw,cellh: single;
  TP, deltaCell, deltaMiddleCell, curPos: TPointF;
  tex: PTexture;
begin
 aColumCount := EnsureRange(aColumCount, 2, aTexture^.TextureWidth);
 aRowCount := EnsureRange(aRowCount, 2, aTexture^.TextureHeight);

 Result := NIL;
 SetLength(Result, aRowCount, aColumCount);

 cellw := aTexture^.FrameWidth/aColumCount;
 cellh := aTexture^.FrameHeight/aRowCount;

 TP := aTexture^.GetTopLeftCoor(0);
 deltaCell := PointF(aTexture^.FrameWidth div aColumCount, aTexture^.FrameHeight div aRowCount);
 deltaMiddleCell := deltaCell * 0.5;

 aTopLeft := aTopLeft + deltaMiddleCell;
 for ro:=0 to aRowCount-1 do begin
   curPos := aTopLeft;
   for co:=0 to aColumCount-1 do begin

     r := Rect(round(cellw*co + TP.x),
               round(cellh*ro + TP.y),
               round(cellw*co+cellw+TP.x), round(cellh*ro+cellh+TP.y));

     tex := new(PTexture);
     tex^.TakePartOf(aTexture, r);

     Result[ro][co] := TPolarSprite.Create(tex, True);
     with Result[ro][co] do begin
       Polar.Center.Value := curPos;
       Polar.Angle.Value := Random * (aSpreadAngleMax - aSpreadAngleMin) + aSpreadAngleMin;
       Polar.Distance.ChangeTo(aDistance+random*aDistanceVariation, aDuration, aCurveID);
       if aAutoKillPart then KillDefered(aDuration);
       Scale.Value := AScaleValue;
       Angle.AddConstant(aRotationMin+ (aRotationMax-aRotationMin)*Random);
       if aChangeOpacityToZero then Opacity.ChangeTo(0, aDuration, idcStartSlowEndFast);
     end;

     if aLayerIndex <> -1 then aScene.Add(Result[ro][co], aLayerIndex);
     curPos.x := curPos.x + deltaCell.x;
   end;
   aTopLeft.y := aTopLeft.y + deltaCell.y;
 end;
end;

{$endif oglcIMPLEMENTATION}

