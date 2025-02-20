{$ifdef oglcINTERFACE}
type

TTintMode = (tmMixColor,       // Tint color is mixed with the actual fragment color.
             tmReplaceColor);  // Tint color replace the actual fragment color.

type
  ////////////////
  // we redefine TColorF as 4xsingle
{ TColorF }

TColorF = packed record
  r,g,b,a : single;
  procedure SetAsTransparent;
  procedure Init(aR, aG, aB, aA: byte); overload;
  procedure Init(aR, aG, aB: byte); overload;
  procedure CopyFrom(const aColorF: TColorF); inline;
  procedure InitFromBGRA(const aColor: TBGRAPixel); overload;
  procedure InitFromBGRA(const aColor: TBGRAPixel; aAlphaCoef: single); overload;
  procedure InitFromTint(const aTintMode: TTintMode; const aColor: TBGRAPixel); inline;
  function ToBGRA: TBGRAPixel;
  procedure MultByFactor(aV: single);
  function EqualTo(const c: TColorF): boolean;
end;
TQuadColorF = array[0..3] of TColorF;

type

//-------------------------------
// types for vertex attributes
Txyuv = packed record
  x, y: single; // vertex coordinates
  u, v: single; // texture coordinates
end;
Pxyuv = ^Txyuv;
TArrayOfxyuv = array of Txyuv;

Txycolor = packed record
  x, y: single;   // vertex coordinates
  c: TColorF;     // color
end;
Pxycolor = ^Txycolor;
TArrayOfxycolor = array of Txycolor;

// used by Glow shader
Txyattenuationcolor = packed record
  x, y: single;        // vertex coordinates
  attenuation: single; // attenuation
  c: TColorF;          // color
end;
Pxyattenuationcolor = ^Txyattenuationcolor;

// used by Line Renderer
TLineVertexAttributes = packed record
  x, y: single;               // vertex coordinates
  distance, threshold: single;   // distance and threshold
  c: TColorF;                    // color
end;
PLineVertexAttributes = ^TLineVertexAttributes;
TArrayOfLineVertexAttributes = array of TLineVertexAttributes;

// used by Electrical Beam Renderer
Txydistance = packed record
  x, y: single;  // vertex coordinates
  distance: single;
end;
Pxydistance = ^Txydistance;
TArrayOfxydistance = array of Txydistance;

Txycolormatrix = packed record
  x, y: single;    // vertex coordinates
  c: TColorF;      // color
  mv: TOGLCMatrix; // model view matrix
end;
Pxycolormatrix = ^Txycolormatrix;

Txyopacitycolortintmatrix = packed record
  x, y, opacity: single;    // vertex coordinates and opacity
  c: TColorF;      // color
  tint: TColorF;   // tint
  mv: TOGLCMatrix; // model view matrix
end;
Pxyopacitycolortintmatrix= ^Txyopacitycolortintmatrix;
TArrayOfxyopacitycolortintmatrix = array of Txyopacitycolortintmatrix;

Txyuvtintopacitymatrix = packed record
  x, y: single;    // vertex coordinates
  u, v: single;    // texture coordinates
  tint: TColorF;      // tint
  opacity: single; // global opacity
  mv: TOGLCMatrix; // model view matrix
end;
Pxyuvtintopacitymatrix = ^Txyuvtintopacitymatrix;

// vertex indice
TVertexIndex = uint16; // can be replaced by uint32
PVertexIndex = ^TVertexIndex;
TArrayOfVertexIndex = array of TVertexIndex;

// primitive restart
const OPENGL_TYPE_USED_FOR_INDICE = GL_UNSIGNED_SHORT;
      PRIMITIVE_INDEX_RESTART_VALUE: uint16  = $FFFF;

  GL_POINTS = GL.GL_POINTS;
  GL_LINES = GL.GL_LINES;
  GL_LINE_LOOP = GL.GL_LINE_LOOP;
  GL_LINE_STRIP = GL.GL_LINE_STRIP;
  GL_TRIANGLES = GL.GL_TRIANGLES;
  GL_TRIANGLE_STRIP = GL.GL_TRIANGLE_STRIP;
  GL_TRIANGLE_FAN = GL.GL_TRIANGLE_FAN;

type
  TPrimitiveType = (ptNone=-1,
                    ptPoints = GL.GL_POINTS,
                    ptLines  = GL.GL_LINES,
                    ptLineLoop = GL.GL_LINE_LOOP,
                    ptLineStrip = GL.GL_LINE_STRIP,
                    ptTriangles = GL.GL_TRIANGLES,
                    ptTriangleStrip = GL.GL_TRIANGLE_STRIP,
                    ptTriangleFan = GL.GL_TRIANGLE_FAN);
  TLineType = TPrimitiveType.ptLines..TPrimitiveType.ptLineStrip; // GL_LINES..GL_LINE_STRIP;
  TTriangleType = TPrimitiveType.ptTriangles..TPrimitiveType.ptTriangleFan; //  GL_TRIANGLES..GL_TRIANGLE_FAN;

type
TOGLCCorner = (cTL=0,   // top left
               cTR,     // top right
               cBR,     // bottom right
               cBL);    // bottom left
POGLCCorner = ^TOGLCCorner;
TQuadCornerIndexes = array[ 0..3 ] of TOGLCCorner;
PQuadCornerIndexes = ^TQuadCornerIndexes;

// quad definition for texture coordinates or other quadrilateral area
TQuadCoor = array[TOGLCCorner] of TPointF;
PQuadCoor = ^TQuadCoor;

{ TQuadCoorHelper }

TQuadCoorHelper = type helper for TQuadCoor
  procedure SetAsEmpty; inline;
  procedure SetAsRectangle(aWidth, aHeight: single); inline;
  procedure SetAsRectangle(aX, aY, aWidth, aHeight: single); inline;
  procedure CopyFrom(const aQuad: TQuadCoor); inline;
  procedure AddOffset(const aOffsets: TQuadParamF); inline;
  procedure Translate(dx, dy: single);
end;

// indexes to flip a texture
const FLIP_INDEXES: array[ 0..3 ] of TQuadCornerIndexes = (
               (cBL, cTL, cBR, cTR), // normal
               (cBR, cTR, cBL, cTL), // flip H
               (cTL, cBL, cTR, cBR), // flip V
               (cTR, cBR, cTL, cBL));// flip HV




/////////////////////
//  FONTS DESCRIPTOR
type

{ TFontGradient }

TFontGradient = record
  Color1,
  Color2: TBGRAPixel;
  GradientType: TGradientType;
  Origin, D1: TPointF;
  GammaColorCorrection,
  Sinus: boolean;
  procedure Create(aColor1, aColor2: TBGRAPixel; AGradientType: TGradientType; AOrigin, aD1: TPointF;
                   aGammaColorCorrection: boolean = True; aSinus: Boolean = False);
  function ToBGRAGradientScanner: TBGRAGradientScanner;
end;

const MAXFONTHEIGHT = 150;
type
{ TFontDescriptor }

TFontDescriptor = record
  FontName: string;
  FontHeight: integer;
  Style: TFontStyles;
  OutLineColor: TBGRAPixel;
  OutLineWidth: single;
  ShadowColor: TBGRAPixel;
  ShadowOffsetX,
  ShadowOffsetY,
  ShadowRadius: integer;
  UseGradient: boolean;
  FontColor: TBGRAPixel;
  Gradient: TFontGradient;
  FontPixelMetric: TFontPixelMetric;
  // set the default font (arial, white)
  procedure CreateDefault;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles; aFontColor: TBGRAPixel); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                  aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth: single); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                  aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth: single;
                  aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                   aFontGradient: TFontGradient;
                   aOutLineColor: TBGRAPixel; aOutLineWidth: single;
                   aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer); overload;

  procedure Check;
  function StringToBitmap(const aStr: string; aFillTexture: TBGRABitmap): TBGRABitmap;
  // if the font use outline or shadow, aBitmap must be initialized with a font renderer with method SetFontRendererOn(...).
  // aBitmap is resized to contain the string.
  procedure ResizeAndDrawStringOn(aBitmap: TBGRABitmap; const aStr: string; aFillTexture: TBGRABitmap);

  procedure SetFontRendererOn(aBitmap: TBGRABitmap);
  function UseShadow: boolean;
  function UseOutLine: boolean;
  // gives the additionnal size computed from outline width and shadow parameters.
  function DecorationSize: TSize;
end;
PFontDescriptor = ^TFontDescriptor;


TOGLCAlignment = (taTopLeft, taTopCenter, taTopRight,
                  taCenterLeft, taCenterCenter, taCenterRight,
                  taBottomLeft, taBottomCenter, taBottomRight);


TTexturedCharIndexes = array of integer;

TOGLCLinePosition = (lpMiddle,   // A line of 6 pixels width will have 3 pixel outside and 3 inside.
                     lpInside);  // The line is inside.


///////////////////
// POST PROCESSING
type
// available effects applicable on each layer
TOGLCPostProcessingFXEnum = (pfTint,             // apply tint
                             pfColorInversion,   // inverts rgb component: new red = 1-red
                             pfGrayscale,        // changes colors to gray scale
                             pfSepia,            // color effect
                             pfSharpen,          // accentuates edges
                             pfBlur,             // a simple blur effect with 3 selectable levels
                             pfUnderWater,       // simulates deformations that can be seen under water
                             pfLightBlur);
TOGLCPostProcessingFX = set of TOGLCPostProcessingFXEnum;

POGLCPostProcessingParameters = ^TOGLCPostProcessingParameters;
TOGLCPostProcessingParameters = record
private const
  // FX bit mask
  fxbmTint=$01;
  fxbmColorInversion=$02;
  fxbmGrayscale=$04;
  fxbmSepia=$08;
  fxbmSharpen=$10;
  fxbmBlur=$20;
  fxbmUnderWater=$40;
  fxbmLightBlur=$80;
private
  FFX: TOGLCPostProcessingFX;
  procedure ComputeFXBitMask;
  procedure InitDefault;
private
  FXBitMask: GLuint;
  FUnderWaterAmplitude, FUnderWaterDensity, FUnderWaterTimeMultiplicator: single;
  FBlurAmount: GLint;
public
  // default is tmReplaceColor
  TintMode: TTintMode;
  // the color used by pfTint effect
  Tint: TBGRAPixel;
  procedure Enable(aFXs: TOGLCPostProcessingFX);
  procedure Disable(aFXs: TOGLCPostProcessingFX);
  procedure DisableAll;

  // aAmplitude: the waves amplitude [1..100]   default is 10
  // aDensity: the waves density [1..100]    default value is 25
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);

  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParams(aAmount: integer);
end;


type
///////////////////////
// Generic chained list
{ TOGLCGList }

generic TOGLCGList<T> = class
  Constructor Create;
  Destructor Destroy; override;
private
  type PT = ^T;
private
  FInstance0: PT;
  FCount: integer;
  function CreateInstance: PT;
  function AddInstanceToList: PT;
  procedure Append(aItem: PT);
  procedure InsertBefore(aItemToInsert, aItemInList: PT);
  procedure InsertAfter(aItemToInsert, aItemInList: PT);
  procedure MoveBefore(aItemToMove, aItemInList: PT);
  procedure MoveAfter(aItemToMove, aItemInList: PT);
  procedure DoDelete(aItem: PT);
  procedure Extract(aItem: PT);
  function FirstItem: PT;
  function LastItem: PT;
public
  procedure Clear;
  property Count: integer read FCount;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCPostProcessingParameters }

procedure TOGLCPostProcessingParameters.ComputeFXBitMask;
begin
  FXBitMask := 0;
  if pfTint in FFX then FXBitMask := FXBitMask or fxbmTint;
  if pfColorInversion in FFX then FXBitMask := FXBitMask or fxbmColorInversion;
  if pfGrayscale in FFX then FXBitMask := FXBitMask or fxbmGrayscale;
  if pfSepia in FFX then FXBitMask := FXBitMask or fxbmSepia;
  if pfSharpen in FFX then FXBitMask := FXBitMask or fxbmSharpen;
  if pfBlur in FFX then FXBitMask := FXBitMask or fxbmBlur;
  if pfUnderWater in FFX then FXBitMask := FXBitMask or fxbmUnderWater;
  if pfLightBlur in FFX then FXBitMask := FXBitMask or fxbmLightBlur;
end;

procedure TOGLCPostProcessingParameters.InitDefault;
begin
  FFX := [];
  ComputeFXBitMask;
  Tint := BGRA(0,0,0,0);
  TintMode := tmReplaceColor;

  SetUnderWaterParams(10, 25, 1.0);
  SetBlurParams(4);
end;

procedure TOGLCPostProcessingParameters.Enable(aFXs: TOGLCPostProcessingFX);
begin
  FFX := FFX + aFXs;
  ComputeFXBitMask;
end;

procedure TOGLCPostProcessingParameters.Disable(aFXs: TOGLCPostProcessingFX);
begin
  FFX := FFX - aFXs;
  ComputeFXBitMask;
end;

procedure TOGLCPostProcessingParameters.DisableAll;
begin
  FFX := [];
  ComputeFXBitMask;
end;

procedure TOGLCPostProcessingParameters.SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);
begin
  FUnderWaterAmplitude := EnsureRange(aAmplitude, 1, 100) * 0.001; // 0.1 to 0.001
  FUnderWaterDensity := EnsureRange(aDensity, 1, 1000);
  if aTimeMultiplicator < 0.1 then FUnderWaterTimeMultiplicator := 0.1
    else FUnderWaterTimeMultiplicator := aTimeMultiplicator;
end;

procedure TOGLCPostProcessingParameters.SetBlurParams(aAmount: integer);
begin
  FBlurAmount := EnsureRange(aAmount, 1, 4);
end;

{ TFontGradient }

procedure TFontGradient.Create(aColor1, aColor2: TBGRAPixel;
  AGradientType: TGradientType; AOrigin, aD1: TPointF;
  aGammaColorCorrection: boolean; aSinus: Boolean);
begin
  Color1 := aColor1;
  Color2 := aColor2;
  GradientType := AGradientType;
  Origin := AOrigin;
  D1 := aD1;
  GammaColorCorrection := aGammaColorCorrection;
  Sinus := aSinus;
end;

function TFontGradient.ToBGRAGradientScanner: TBGRAGradientScanner;
begin
  Result := TBGRAGradientScanner.Create(Color1, Color2, GradientType, Origin, D1,
                                        GammaColorCorrection, Sinus);
end;

{ TFontDescriptor }

procedure TFontDescriptor.CreateDefault;
begin
  Create('Arial', 12, [], BGRAWhite);
end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontColor: TBGRAPixel);
begin
  Create(aFontName, aFontHeight, aStyle, aFontColor, BGRAPixelTransparent, 0, BGRAPixelTransparent,0, 0, 0);
end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontColor, aOutLineColor: TBGRAPixel;
  aOutLineWidth: single);
begin
  Create(aFontName, aFontHeight, aStyle, aFontColor, aOutLineColor, aOutLineWidth, BGRAPixelTransparent,0, 0, 0);
end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontColor, aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX,
  aShadowOffsetY, aShadowRadius: integer);
var ima: TBGRABitmap;
begin
  if aFontName=''
    then FontName :='default'
    else FontName := aFontName;
  FontHeight := aFontHeight;
  Style := aStyle;
  FontColor := aFontColor;
  UseGradient := FALSE;
  OutLineColor := aOutLineColor;
  OutLineWidth := aOutLineWidth;
  ShadowColor := aShadowColor;
  ShadowOffsetX := aShadowOffsetX;
  ShadowOffsetY := aShadowOffsetY;
  ShadowRadius := aShadowRadius;

  ima := TBGRABitmap.Create(1,1);
  SetFontRendererOn(ima);
  FontPixelMetric := ima.FontPixelMetric;
  ima.Free;
end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; aFontGradient: TFontGradient; aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX,
  aShadowOffsetY, aShadowRadius: integer);
var ima: TBGRABitmap;
begin
  if aFontName=''
    then FontName :='default'
    else FontName := aFontName;
  FontName := aFontName;
  FontHeight := aFontHeight;
  Style := aStyle;
  FontColor := BGRAPixelTransparent;
  Gradient := aFontGradient;
  UseGradient := TRUE;
  OutLineColor := aOutLineColor;
  OutLineWidth := aOutLineWidth;
  ShadowColor := aShadowColor;
  ShadowOffsetX := aShadowOffsetX;
  ShadowOffsetY := aShadowOffsetY;
  ShadowRadius := aShadowRadius;

  ima := TBGRABitmap.Create(1,1);
  SetFontRendererOn(ima);
  FontPixelMetric := ima.FontPixelMetric;
  ima.Free;
end;

procedure TFontDescriptor.Check;
begin
  // max FontHeight is 150
  if FontHeight > MAXFONTHEIGHT then FontHeight := MAXFONTHEIGHT;
end;

function TFontDescriptor.StringToBitmap(const aStr: string; aFillTexture: TBGRABitmap): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(1, 1);
  SetFontRendererOn(Result);
  ResizeAndDrawStringOn(Result, aStr, aFillTexture);
end;

procedure TFontDescriptor.ResizeAndDrawStringOn(aBitmap: TBGRABitmap; const aStr: string; aFillTexture: TBGRABitmap);
var xx,yy:single;
    ts: TSize;
    _ShadowOffsetX, _ShadowOffsetY, _ShadowRadius: integer;
    _OutLineWidth: single;
   grad: TBGRAGradientScanner;
begin
  // size of the rendered font. try to take into account shadow and outline.
  ts := aBitmap.TextSize(aStr)+DecorationSize;

  // to avoid a division by 0
  if ts.cx = 0 then ts.cx := 1;
  if ts.cy = 0 then ts.cy := 1;

  aBitmap.SetSize(ts.cx, ts.cy);

  // force shadow param. to 0 if there is no shadow
  if UseShadow then begin
    _ShadowOffsetX := ShadowOffsetX;
    _ShadowOffsetY := ShadowOffsetY;
    _ShadowRadius := ShadowRadius;
  end else begin
     _ShadowOffsetX := 0;
     _ShadowOffsetY := 0;
     _ShadowRadius := 0;
  end;
  // force outline param. to 0 if there is no outline
  if UseOutLine
    then _OutLineWidth := OutLineWidth
    else _OutLineWidth := 0;

  xx := _OutlineWidth*0.5+_ShadowRadius;
  yy := xx;
  if _ShadowOffsetX < 0 then xx += Abs(_ShadowOffsetX);
  if _ShadowOffsetY < 0 then yy += Abs(_ShadowOffsetY);

  xx := 0;
  yy := 0;

  aBitmap.Fill(BGRAPixelTransparent);
  if aFillTexture <> NIL
    then aBitmap.TextOut(xx, yy, aStr, aFillTexture)
    else if UseGradient then begin
           grad := Gradient.ToBGRAGradientScanner;
           aBitmap.TextOut(xx, yy, aStr, grad);
           grad.Free;
         end else aBitmap.TextOut(xx, yy, aStr, FontColor);
end;

procedure TFontDescriptor.SetFontRendererOn(aBitmap: TBGRABitmap);
var renderer: TBGRATextEffectFontRenderer;
begin
  if aBitmap = NIL then exit;
  if UseShadow or UseOutLine then begin
    renderer := TBGRATextEffectFontRenderer.Create;
    with renderer do begin
      ShadowVisible := UseShadow;
      ShadowColor := Self.ShadowColor;
      ShadowOffset.x := ShadowOffsetX;
      ShadowOffset.y := ShadowOffsetY;
      ShadowRadius := Self.ShadowRadius;
      OutlineVisible := UseOutLine;
      OutlineColor := Self.OutLineColor;
      OuterOutlineOnly := True;
      OutlineWidth := Self.OutLineWidth;
    end;
    aBitmap.FontRenderer := renderer;
  end;

  aBitmap.FontQuality := fqFineAntialiasing; //fqFineAntialiasing    fqSystemClearType
  aBitmap.FontName := FontName;
  aBitmap.FontFullHeight := FontHeight;
  aBitmap.FontStyle := Style;
end;

function TFontDescriptor.UseShadow: boolean;
begin
   Result := (ShadowColor.alpha <> 0) and (ShadowRadius > 0);
end;

function TFontDescriptor.UseOutLine: boolean;
begin
  Result := (OutLineColor.alpha <> 0) and (OutLineWidth > 0);
end;

function TFontDescriptor.DecorationSize: TSize;
var
  _ShadowOffsetX, _ShadowOffsetY, _ShadowRadius: Integer;
  _OutLineWidth: Single;
begin
  // force shadow param. to 0 if there is no shadow
  if UseShadow then begin
    _ShadowOffsetX := ShadowOffsetX;
    _ShadowOffsetY := ShadowOffsetY;
    _ShadowRadius := ShadowRadius;
  end else begin
     _ShadowOffsetX := 0;
     _ShadowOffsetY := 0;
     _ShadowRadius := 0;
  end;
  // force outline param. to 0 if there is no outline
  if UseOutLine
    then _OutLineWidth := OutLineWidth
    else _OutLineWidth := 0;

  Result.cx := round(_OutlineWidth)+abs( _ShadowOffsetX )+_ShadowRadius{*2};
  Result.cy := round(_OutlineWidth)+abs( _ShadowOffsetY )+_ShadowRadius{*2};
end;


{ TOGLCGList }

constructor TOGLCGList.Create;
begin
  New(FInstance0);
  FInstance0^.Prev := NIL;
  FInstance0^.Next := NIL;
  FCount := 0;
end;

destructor TOGLCGList.Destroy;
begin
  Clear;
  Dispose(FInstance0);
  inherited Destroy;
end;

function TOGLCGList.CreateInstance: PT;
begin
  New(Result);
end;

function TOGLCGList.AddInstanceToList: PT;
begin
  New(Result);
  Append(Result);
end;

procedure TOGLCGList.Append(aItem: PT);
var p: PT;
begin
  p := FInstance0;
  while p^.Next <> NIL do p := p^.Next;

  p^.Next := aItem;
  aItem^.Next := NIL;
  aItem^.Prev := p;
  inc(FCount);
end;

procedure TOGLCGList.InsertBefore(aItemToInsert, aItemInList: PT);
var previousItemInList: PT;
begin
  previousItemInList := aItemInList^.Prev;
  previousItemInList^.Next := aItemToInsert;

  aItemToInsert^.Prev := previousItemInList;
  aItemToInsert^.Next := aItemInList;

  aItemInList^.Prev := aItemToInsert;
  inc(FCount);
end;

procedure TOGLCGList.InsertAfter(aItemToInsert, aItemInList: PT);
var nextItemInList: PT;
begin
  nextItemInList := aItemInList^.Next;

  if nextItemInList = NIL then begin
    Append(aItemToInsert);
    exit;
  end;

  nextItemInList^.Prev := aItemToInsert;

  aItemToInsert^.Next := nextItemInList;
  aItemToInsert^.Prev := aItemInList;

  aItemInList^.Next := aItemToInsert;
  inc(FCount);
end;

procedure TOGLCGList.MoveBefore(aItemToMove, aItemInList: PT);
begin
  Extract(aItemToMove);
  InsertBefore(aItemToMove, aItemInList);
end;

procedure TOGLCGList.MoveAfter(aItemToMove, aItemInList: PT);
begin
  Extract(aItemToMove);
  InsertAfter(aItemToMove, aItemInList);
end;

procedure TOGLCGList.DoDelete(aItem: PT);
var p, n:PT;
begin
  if aItem^.Next <> NIL then
  begin
    p := aItem^.Prev;
    n := aItem^.Next;
    p^.Next := n;
    n^.Prev := p;
  end else begin
    p := aItem^.Prev;
    p^.Next := NIL;
  end;
  Dispose(aItem);
  dec(FCount);
end;

procedure TOGLCGList.Extract(aItem: PT);
begin
  if aItem^.Prev <> NIL then aItem^.Prev^.Next := aItem^.Next;
  if aItem^.Next <> NIL then aItem^.Next^.Prev := aItem^.Prev;

  aItem^.Prev := NIL;
  aItem^.Next := NIL;
  dec(FCount);
end;

function TOGLCGList.FirstItem: PT;
begin
  Result := FInstance0^.Next;
end;

function TOGLCGList.LastItem: PT;
var p: PT;
begin
  p := FInstance0;
  while p^.Next <> NIL do p := p^.Next;
  if p = FInstance0 then p := NIL;
  Result := p;
end;

procedure TOGLCGList.Clear;
begin
  while FInstance0^.Next <> NIL do DoDelete(FInstance0^.Next);
end;

{ TQuadCoorHelper }

procedure TQuadCoorHelper.SetAsEmpty;
begin
  FillChar(Self, SizeOf(TPointF)*4, 0);
end;

procedure TQuadCoorHelper.SetAsRectangle(aWidth, aHeight: single);
begin
  FillChar(Self, SizeOf(TPointF)*4, 0);
  Self[cTL] := PointF(0, 0);
  Self[cTR].x := aWidth;
  Self[cBR].x := aWidth;
  Self[cBR].y := aHeight;
  Self[cBL].y := aHeight;
end;

procedure TQuadCoorHelper.SetAsRectangle(aX, aY, aWidth, aHeight: single);
begin
  Self[cTL].x := aX;
  Self[cTL].y := aY;
  Self[cTR].x := aX+aWidth;
  Self[cTR].y := aY;
  Self[cBR].x := aX+aWidth;
  Self[cBR].y := aY+aHeight;
  Self[cBL].x := aX;
  Self[cBL].y := aY+aHeight;
end;

procedure TQuadCoorHelper.CopyFrom(const aQuad: TQuadCoor);
begin
  Move(aQuad, Self, SizeOf(TQuadCoor));
end;

procedure TQuadCoorHelper.AddOffset(const aOffsets: TQuadParamF);
begin
  Self[cTL] := Self[cTL] + aOffsets.TopLeft.Value;
  Self[cTR] := Self[cTR] + aOffsets.TopRight.Value;
  Self[cBR] := Self[cBR] + aOffsets.BottomRight.Value;
  Self[cBL] := Self[cBL] + aOffsets.BottomLeft.Value;
end;

procedure TQuadCoorHelper.Translate(dx, dy: single);
begin
  Self[cTL].x := Self[cTL].x + dx;  Self[cTL].y := Self[cTL].y + dy;
  Self[cTR].x := Self[cTR].x + dx;  Self[cTR].y := Self[cTR].y + dy;
  Self[cBR].x := Self[cBR].x + dx;  Self[cBR].y := Self[cBR].y + dy;
  Self[cBL].x := Self[cBL].x + dx;  Self[cBL].y := Self[cBL].y + dy;
end;

{ TColorF }

procedure TColorF.SetAsTransparent;
begin
  FillChar(Self, SizeOf(TColorF), 0);
end;

procedure TColorF.Init(aR, aG, aB, aA: byte);
begin
  r := aR*OneDiv255;
  g := aG*OneDiv255;
  b := aB*OneDiv255;
  a := aA*OneDiv255;
end;

procedure TColorF.Init(aR, aG, aB: byte);
begin
  Init(aR, aG, aB, 255);
end;

procedure TColorF.CopyFrom(const aColorF: TColorF);
begin
  Move(aColorF, Self, SizeOf(TColorF));
end;

procedure TColorF.InitFromBGRA(const aColor: TBGRAPixel);
begin
  r := aColor.red*OneDiv255;
  g := aColor.green*OneDiv255;
  b := aColor.blue*OneDiv255;
  a := aColor.alpha*OneDiv255;
end;

procedure TColorF.InitFromBGRA(const aColor: TBGRAPixel; aAlphaCoef: single);
begin
  r := aColor.red*OneDiv255;
  g := aColor.green*OneDiv255;
  b := aColor.blue*OneDiv255;
  a := aColor.alpha*OneDiv255*aAlphaCoef;
end;

procedure TColorF.InitFromTint(const aTintMode: TTintMode; const aColor: TBGRAPixel);
const _128div255: single = 0.5019607; // 128/255;
      OneDiv255Div2: single = 0.0019607; // 1/255/2
begin
  r := aColor.red*OneDiv255;
  g := aColor.green*OneDiv255;
  b := aColor.blue*OneDiv255;
  a := aColor.alpha*OneDiv255Div2; //(aColor.alpha shr 1)*OneDiv255;      // alpha/2/255
  if aTintMode = tmReplaceColor then
    a := a + _128div255;
end;

function TColorF.ToBGRA: TBGRAPixel;
begin
  Result.red := Round(r*255);
  Result.green := Round(g*255);
  Result.blue := Round(b*255);
  Result.alpha := Round(a*255);
end;

procedure TColorF.MultByFactor(aV: single);
begin
  r := r * aV;
  g := g * aV;
  b := b * aV;
  a := a * aV;
end;

function TColorF.EqualTo(const c: TColorF): boolean;
begin
  Result := (r = c.r) and (g = c.g) and (b = c.g) and (a = c.a);
end;

{$endif oglcIMPLEMENTATION}

