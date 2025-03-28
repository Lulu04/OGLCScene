{$ifdef oglcINTERFACE}
{
   Make an explosion effect with a texture.
   To do this, the texture is 'cut' in multiple rectangular parts. Each part
   is associated to a TPolarSprite.

   aScene: the parent scene.
   aLayerIndex: if <> -1, each created sprite will be added to this layer
   aTexture: texture used to perform the effect. Can be a part of another.
   aRowCount, aColumCount: defines the number of divisions of the texture
   aTopLeft: coordinates of the top left piece
   aCenter: coordinate of the center of the explosion
   aCenterVariation: variation applyed randomly on the center coordinates
   AScaleValue: scale value applyed on each piece
   aAngleMin, aAngleMax: a constant rotation is added to each part, randomly calculated from this range
   aDistance: distance traveled by each piece of texture (in pixels)
   aDistanceVariation: random value between [0..aDistanceVariation] added to aDistance
   aAutoKillPart: if true a KillDefered is applyed on each sprite created.
   aDuration: time to travel this distance
   aCurveID: velocity curve applyed on each piece
}
function ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
                        aTexture: PTexture; aRowCount, aColumCount: integer;
                        aTopLeft: TPointF;
                        aCenter: TPointF; aCenterVariation: single;
                        AScaleValue: TPointF;
                        aAngleMin, aAngleMax: single;
                        aDistance, aDistanceVariation: single;
                        aAutoKillPart: boolean;
                        aDuration: single;
                        aCurveID: word = idcStartFastEndSlow): TArrayOfArrayOfTPolarSprite;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

function ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
                         aTexture: PTexture; aRowCount, aColumCount: integer;
                         aTopLeft: TPointF;
                         aCenter: TPointF; aCenterVariation: single;
                         AScaleValue: TPointF;
                         aAngleMin, aAngleMax: single;
                         aDistance, aDistanceVariation: single;
                         aAutoKillPart: boolean;
                         aDuration: single;
                         aCurveID: word): TArrayOfArrayOfTPolarSprite;
var r: TRect;
  ro, co: Integer;
  cellw,cellh, xx, yy: single;
  TP: TPointF;
  tex: PTexture;
begin
 aColumCount := EnsureRange(aColumCount, 2, aTexture^.TextureWidth);
 aRowCount := EnsureRange(aRowCount, 2, aTexture^.TextureHeight);

 Result := NIL;
 SetLength(Result, aRowCount, aColumCount);

 cellw := aTexture^.FrameWidth/aColumCount;
 cellh := aTexture^.FrameHeight/aRowCount;

 TP := PointF(aTexture^.TextureWidth*aTexture^.FramesCoord[0][cTL].x,
              aTexture^.TextureHeight-aTexture^.TextureHeight*aTexture^.FramesCoord[0][cTR].y);

 yy := aTopLeft.y;
 for ro:=0 to aRowCount-1 do begin
   xx := aTopLeft.x;
   for co:=0 to aColumCount-1 do begin

   r := Rect(round(cellw*co + TP.x),
             round(cellh*ro + TP.y),
             round(cellw*co+cellw+TP.x), round(cellh*ro+cellh+TP.y));

   //tex := aScene.TexMan.GivePart(aTexture, r);
   tex := new(PTexture);
   tex^.TakePartOf(aTexture, r);

   Result[ro][co] := TPolarSprite.Create(tex, True);
   with Result[ro][co] do begin
     Polar.Center.Value := aCenter+PointF(random*aCenterVariation-aCenterVariation*0.5, random*aCenterVariation-aCenterVariation*0.5);
     Polar.CartesianValue := PointF(xx,yy);
     Polar.Distance.ChangeTo(aDistance+random*aDistanceVariation, aDuration, aCurveID);
   if aAutoKillPart then KillDefered(aDuration);
     Scale.Value := AScaleValue;
     Angle.AddConstant(aAngleMin+ (aAngleMax-aAngleMin)*Random);
     Opacity.ChangeTo(0, aDuration, idcStartSlowEndFast);
   end;

   if aLayerIndex <> -1 then aScene.Add(Result[ro][co], aLayerIndex);
   xx += cellw;
   end;
   yy += cellh;
 end;
end;

{$endif oglcIMPLEMENTATION}

