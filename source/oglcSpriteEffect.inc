{$ifdef oglcINTERFACE}
{
   Make an explosion effect with a texture.
   To do this, the texture is 'cut' in multiple rectangular parts. Each part
   is associated to a TPolarSprite.

   aScene, aLayerIndex: scene and layer where the texture parts will be created
   aTexture: texture used to perform the effect. Can be a part of another.
   aRowCount, aColumCount: defines the number of divisions of the texture
   aTopLeft: coordinates of the top left piece
   aCenter: coordinate of the center of the explosion
   aCenterVariation: variation applyed randomly on the center coordinates
   AScaleValue: scale value applyed on each piece
   aDistance: distance traveled by each piece of texture (in pixels)
   aDistanceVariation: random value between [0..aDistanceVariation] added to aDistance
   aDuration: time to travel this distance
   aCurveID: velocity curve applyed on each piece
}
procedure ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
                         aTexture: PTexture; aRowCount, aColumCount: integer;
                         aTopLeft: TPointF;
                         aCenter: TPointF; aCenterVariation: single;
                         AScaleValue: TPointF;
                         aDistance, aDistanceVariation: single;
                         aDuration: single;
                         aCurveID: word = idcStartFastEndSlow);

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure ExplodeTexture(aScene: TOGLCScene; aLayerIndex: integer;
                         aTexture: PTexture; aRowCount, aColumCount: integer;
                         aTopLeft: TPointF;
                         aCenter: TPointF; aCenterVariation: single;
                         AScaleValue: TPointF;
                         aDistance, aDistanceVariation: single;
                         aDuration: single;
                         aCurveID: word);
var s: array of TArrayOfTPolarSprite;
  r: TRect;
  ro, co: Integer;
  cellw,cellh, xx, yy: single;
  TP: TPointF;
  tex: PTexture;
begin
 aColumCount := EnsureRange(aColumCount, 2, aTexture^.TextureWidth);
 aRowCount := EnsureRange(aRowCount, 2, aTexture^.TextureHeight);

 s := NIL;
 SetLength(s, aRowCount, aColumCount);

 cellw := aTexture^.TextureWidth/aColumCount;
 cellh := aTexture^.TextureHeight/aRowCount;

 TP := PointF(aTexture^.TextureWidth*aTexture^.FramesCoord[0][cTL].x,
              aTexture^.TextureHeight-aTexture^.TextureHeight*aTexture^.FramesCoord[0][cTR].y);
 yy := aTopLeft.y;
 for ro:=0 to aRowCount-1 do begin
   xx := aTopLeft.x;
   for co:=0 to aColumCount-1 do begin

   r := Rect(round(cellw*co + TP.x),
             round(cellh*ro + TP.y),
             round(cellw*co+cellw+TP.x), round(cellh*ro+cellh+TP.y));

   tex := aScene.TexMan.GivePart(aTexture, r);
   s[ro][co] := TPolarSprite.Create(tex, True);
   s[ro][co].Polar.Center.Value := aCenter+PointF(random*aCenterVariation-aCenterVariation*0.5, random*aCenterVariation-aCenterVariation*0.5);
   s[ro][co].Polar.CartesianValue := PointF(xx,yy);
   s[ro][co].Polar.Distance.ChangeTo(aDistance+random*aDistanceVariation, aDuration, aCurveID);
   s[ro][co].KillDefered(aDuration);
   s[ro][co].Scale.Value := AScaleValue;
   s[ro][co].Opacity.ChangeTo(0, aDuration, idcStartSlowEndFast);

   aScene.Add(s[ro][co], aLayerIndex);
   xx += cellw;
   end;
   yy += cellh;
 end;
end;

{$endif oglcIMPLEMENTATION}

