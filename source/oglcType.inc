{$ifdef oglcINTERFACE}
type

TArrayOfInteger = array of integer;
TProcOfObject = procedure() of object;

TTintMode = (tmMixColor,       // Tint color is mixed with the actual fragment color.
             tmReplaceColor);  // Tint color replace the actual fragment color.

type
  ////////////////
  // we redefine TColorF as 4xsingle
{ TColorF }

TColorF = packed record
private
  function GetAlphaByte: byte;
  function GetBlueByte: byte;
  function GetGreenByte: byte;
  function GetRedByte: byte;
  procedure SetAlphaByte(AValue: byte);
  procedure SetBlueByte(AValue: byte);
  procedure SetGreenByte(AValue: byte);
  procedure SetRedByte(AValue: byte);
public
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
  property RedByte: byte read GetRedByte write SetRedByte;
  property GreenByte: byte read GetGreenByte write SetGreenByte;
  property BlueByte: byte read GetBlueByte write SetBlueByte;
  property AlphaByte: byte read GetAlphaByte write SetAlphaByte;
end;
TQuadColorF = array[0..3] of TColorF;

type

//-------------------------------
// types for vertex attributes
Txy = packed record
  x, y: single; // vertex coordinates
end;
Pxy = ^Txy;
TArrayOfxy = array of Txy;

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



/////////////////////
//  FREE TYPE FONT
type

{ TOGLCFontManager }

TOGLCFontManager = class
private
  FFontCollection: TFreeTypeFontCollection;
private // system fonts
  FSystemFontFilenames,
  FSystemFontNames: TStringArray;
  function GetSystemFontName(index: integer): string;
  procedure ScanSystemFonts;
private // project fonts
  FProjectFontFilenames,
  FProjectFontNames: TStringArray;
  function GetProjectFontName(index: integer): string;
private
  function SystemFontIndexOf(aFontName: string): integer;
  function ProjectFontIndexOf(aFontName: string): integer;
private
  procedure ListFontFamily(AFamilyName: string);
public
  constructor Create;
  destructor Destroy; override;

  function SystemFontCount: integer;
  property SystemFontName[index:integer]: string read GetSystemFontName;

  procedure ScanProjectFont(const aProjectFontDirectory: string);
  procedure ClearProjectFonts;
  function ProjectFontCount: integer;
  property ProjectFontName[index:integer]: string read GetProjectFontName;

  function GetFreeTypeFont(aFontName: string): TFreeTypeFont;

  procedure FontHaveBoldOrItalic(const aFontName: string; out CanBold, CanItalic: boolean);

  property FontCollection: TFreeTypeFontCollection read FFontCollection;
end;


/////////////////////
//  FONT DESCRIPTOR
type

{ TFontGradient }

TFontGradient = record
  Color1,
  Color2: TBGRAPixel;
  GradientType: TGradientType;
  Origin, D1: TPointF;
  GammaColorCorrection,
  Sinus: boolean;
  procedure Create(aColor1, aColor2: TBGRAPixel; AGradientType: TGradientType;
                   AOrigin, aD1: TPointF;
                   aGammaColorCorrection: boolean = True; aSinus: Boolean = False);
  function ToBGRAGradientScanner: TBGRAGradientScanner;
  // utils for Game Helper
  function SaveToString: string;
  procedure LoadFromString(const data: string);
end;

const MAXFONTHEIGHT = 150;
type
{ TFontDescriptor }
PFontDescriptor = ^TFontDescriptor;
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
  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles; aFontColor: TBGRAPixel); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                  aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth: single); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                  aFontColor, aOutLineColor: TBGRAPixel; aOutLineWidth: single;
                  aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                   const aFontGradient: TFontGradient); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                   const aFontGradient: TFontGradient;
                   aOutLineColor: TBGRAPixel; aOutLineWidth: single); overload;

  procedure Create(const aFontName: string; aFontHeight: integer; aStyle: TFontStyles;
                   const aFontGradient: TFontGradient;
                   aOutLineColor: TBGRAPixel; aOutLineWidth: single;
                   aShadowColor: TBGRAPixel; aShadowOffsetX, aShadowOffsetY, aShadowRadius: integer); overload;

  procedure DuplicateTo(aDest: PFontDescriptor);
  // After creating a font descriptor, call this method to compute the font height so that the passed string will
  // fit in the passed rect with a maximum size.
  procedure ComputeMaxHeightFor(const aString: string; const aRect: TRect);

  function GetFreeTypeFont: TFreeTypeFont;

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
public  // utils for Game Helper
  function SaveToString: string;
  procedure LoadFromString(const data: string);
end;


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
TOGLCPostProcessingFXEnum = (ppTint,             // apply tint
                             ppColorInversion,   // inverts rgb component: new red = 1-red
                             ppGrayscale,        // changes colors to gray scale
                             ppSepia,            // color effect
                             ppSharpen,          // accentuates edges
                             ppBlur,             // a simple blur effect with 4 selectable levels
                             ppUnderWater,       // simulates under water deformations
                             ppPixelize,         // pixel effect
                             ppDreamVision,      // color effect like in a dream
                             ppShockWave,        // an effect to simulate a shock wave
                             ppBloom,            // makes bright areas of the scene appear to glow
                             ppCustomRenderer);  // to select a custom renderer. All other effects will be ignored
TOGLCPostProcessingFX = set of TOGLCPostProcessingFXEnum;

TOGLCBaseRendererForPostProcessing = class;

{ TOGLCPostProcessingParameters }

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
  fxbmPixelize=$80;
  fxbmDreamVision=$100;
  fxbmShockWave=$200;
  fxbmBloom=$400;
  fxbmUserRenderer=$800;
private
  FFX: TOGLCPostProcessingFX;
  procedure ComputeFXBitMask;
  procedure InitDefault;
private
  FXBitMask: GLuint;
  FUnderWaterAmplitude, FUnderWaterDensity, FUnderWaterTimeMultiplicator: single;
  FBlurAmount: GLint;
  FPixelizeAmount: GLfloat;
  FShockWaveCenterU, FShockWaveCenterV, FShockWaveTimeMultiplicator: single;
  FShockWaveColorF: TColorF;
  FBloomThreshold, FBloomIntensity: single;
  FUserRenderer: TOGLCBaseRendererForPostProcessing;
public
  // default is tmReplaceColor
  TintMode: TTintMode;
  // the color used by pfTint effect
  Tint: TBGRAPixel;
  procedure Enable(aFXs: TOGLCPostProcessingFX);
  procedure Disable(aFXs: TOGLCPostProcessingFX);
  procedure DisableAll;

  procedure SetTintParams(aTintMode: TTintMode; const aTint: TBGRAPixel);

  // aAmplitude: the waves amplitude [1..100]   default is 10
  // aDensity: the waves density [1..100]    default value is 25
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast)
  procedure SetUnderWaterParams(aAmplitude, aDensity, aTimeMultiplicator: single);

  // aAmount range is 1 soft   .....    4 full
  procedure SetBlurParams(aAmount: integer);

  // aAmount range is 0.01 to 1.0
  procedure SetPixelizeParams(aAmount: single);

  // aCenterU, aCenterV are the coordinates of the center of the wave, expressed in range [0..1]
  // aCenterV sets to 0 mean the bottom of the screen (openGL coordinates system). Default value is 0.5
  // aTimeMultiplicator mini is 0.1 (slow)  -->  1.0 normal speed(default)  --->  greater than 1.0 (fast) Default is 0.3
  // aColor is the tint color of the wave. Default is transparent
  procedure SetShockWaveParams(aCenterU, aCenterV, aTimeMultiplicator: single; const aColor: TBGRAPixel);

  // aThreshold range is [0..1] default value is 0.5
  // if the luminosity of a pixel is greater than this threshold the blur is applied
  // aIntensity is the intensity of the computed blureed color. range is > 0.  default is 1.0
  procedure SetBloomParams(aThreshold, aIntensity: single);

  // specify a custom renderer to use when the effect 'pfUserRenderer' is enabled.
  procedure UseCustomRenderer(aUserRenderer: TOGLCBaseRendererForPostProcessing);
end;

type
//////////////////////////

{ TSmoothPointF }
// this module allow to change smoothly coordinates (TPointF) from its current value to a wanted value.
// see oglccamera.inc for an example
TSmoothPointF = record
  MovesAmount: single;
  Speed: single;  // default is 0.05
  procedure InitDefault;
  procedure Update(var aCurrentPos: TPointF; const aWantedPos: TPointF; aElapsedTime: single);
end;

{ TSmoothSingle }
// this module allow to smoothly change a quantity (single) from its current value to a wanted value.
TSmoothSingle = record
  MovesAmount: single;
  Speed: single;  // default is 0.05
  procedure InitDefault;
  procedure Update(var aCurrent: single; const aWanted: single; aElapsedTime: single);
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

{ TSmoothPointF }

procedure TSmoothPointF.InitDefault;
begin
  MovesAmount := 0.0;
  Speed := 0.05;
end;

procedure TSmoothPointF.Update(var aCurrentPos: TPointF; const aWantedPos: TPointF; aElapsedTime: single);
var delta: TPointF;
  deltaIsZero: boolean;
begin
  delta := aWantedPos - aCurrentPos;

  deltaIsZero := (delta.x = 0) and (delta.y = 0);
  if deltaIsZero
    then MovesAmount := Max(MovesAmount-AElapsedTime*Speed, 0.0) // decelerates
    else MovesAmount := Min(MovesAmount+AElapsedTime*Speed, 1.0); // accelerates

  if (MovesAmount <> 0) and not deltaIsZero then
    aCurrentPos := aCurrentPos + delta * MovesAmount;
end;

{ TSmoothSingle }

procedure TSmoothSingle.InitDefault;
begin
  MovesAmount := 0.0;
  Speed := 0.05;
end;

procedure TSmoothSingle.Update(var aCurrent: single; const aWanted: single; aElapsedTime: single);
var delta: single;
begin
  delta := aWanted - aCurrent;
  if delta = 0.0
    then MovesAmount := Max(MovesAmount-AElapsedTime*Speed, 0.0) // decelerates
    else MovesAmount := Min(MovesAmount+AElapsedTime*Speed, 1.0); // accelerates
  if (MovesAmount <> 0) and (delta <> 0.0) then
    aCurrent := aCurrent + delta * MovesAmount;
end;

{ TOGLCPostProcessingParameters }

procedure TOGLCPostProcessingParameters.ComputeFXBitMask;
begin
  // ppCustomRenderer ignore all others effects
  if ppCustomRenderer in FFX then begin
    FXBitMask := fxbmUserRenderer;
    exit;
  end;

  FXBitMask := 0;
  if ppTint in FFX then FXBitMask := FXBitMask or fxbmTint;
  if ppColorInversion in FFX then FXBitMask := FXBitMask or fxbmColorInversion;
  if ppGrayscale in FFX then FXBitMask := FXBitMask or fxbmGrayscale;
  if ppSepia in FFX then FXBitMask := FXBitMask or fxbmSepia;
  if ppSharpen in FFX then FXBitMask := FXBitMask or fxbmSharpen;
  if ppBlur in FFX then FXBitMask := FXBitMask or fxbmBlur;
  if ppUnderWater in FFX then FXBitMask := FXBitMask or fxbmUnderWater;
  if ppPixelize in FFX then FXBitMask := FXBitMask or fxbmPixelize;
  if ppDreamVision in FFX then FXBitMask := FXBitMask or fxbmDreamVision;
  if ppShockWave in FFX then FXBitMask := FXBitMask or fxbmShockWave;
  if ppBloom in FFX then FXBitMask := FXBitMask or fxbmBloom;
end;

procedure TOGLCPostProcessingParameters.InitDefault;
begin
  FFX := [];
  ComputeFXBitMask;
  Tint := BGRA(0,0,0,0);
  TintMode := tmReplaceColor;

  SetUnderWaterParams(10, 25, 1.0);
  SetBlurParams(4);
  SetPixelizeParams(0.5);
  SetShockWaveParams(0.5, 0.5, 0.3, BGRA(0,0,0,0));
  SetBloomParams(0.5, 1.0);
  FUserRenderer := NIL;
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

procedure TOGLCPostProcessingParameters.SetTintParams(aTintMode: TTintMode; const aTint: TBGRAPixel);
begin
  TintMode := aTintMode;
  Tint := aTint;
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

procedure TOGLCPostProcessingParameters.SetPixelizeParams(aAmount: single);
begin
  FPixelizeAmount := EnsureRange(aAmount, 0.01, 1.0);
end;

procedure TOGLCPostProcessingParameters.SetShockWaveParams(aCenterU, aCenterV,
  aTimeMultiplicator: single; const aColor: TBGRAPixel);
begin
  FShockWaveCenterU := EnsureRange(aCenterU, 0, 1);
  FShockWaveCenterV := EnsureRange(aCenterV, 0, 1);
  FShockWaveTimeMultiplicator := Max(aTimeMultiplicator, 0.01);
  FShockWaveColorF.InitFromBGRA(aColor);
end;

procedure TOGLCPostProcessingParameters.SetBloomParams(aThreshold, aIntensity: single);
begin
  FBloomThreshold := EnsureRange(aThreshold, 0, 1);
  FBloomIntensity := Max(0.01, aIntensity);
end;

procedure TOGLCPostProcessingParameters.UseCustomRenderer(aUserRenderer: TOGLCBaseRendererForPostProcessing);
begin
  FUserRenderer := aUserRenderer;
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

function TFontGradient.SaveToString: string;
var prop: TProperties;
begin
  prop.Init(' ');
  prop.Add('Color1', Color1);
  prop.Add('Color2', Color2);
  prop.Add('Type', integer(Ord(GradientType)));
  prop.Add('OriginX', Origin.x);
  prop.Add('OriginY', Origin.Y);
  prop.Add('D1X', D1.x);
  prop.Add('D1Y', D1.Y);
  prop.Add('GammaCorrect', GammaColorCorrection);
  prop.Add('Sinus', Sinus);
  Result := prop.PackedProperty;
end;

procedure TFontGradient.LoadFromString(const data: string);
var prop: TProperties;
  v: integer;
  vs: single;
begin
  if data = '' then exit;
  v := 0;
  vs := 0;
  prop.Split(data, ' ');
  prop.BGRAPixelValueOf('Color1', Color1, BGRA(255,255,0));
  prop.BGRAPixelValueOf('Color2', Color2, BGRA(0,255,255));
  prop.IntegerValueOf('Type', v, 0);
  GradientType := TGradientType(v);
  prop.SingleValueOf('OriginX', vs, 0);
  Origin.x := vs;
  prop.SingleValueOf('OriginY', vs, 0);
  Origin.y := vs;
  prop.SingleValueOf('D1X', vs, 0);
  D1.x := vs;
  prop.SingleValueOf('D1Y', vs, 0);
  D1.y := vs;
  prop.BooleanValueOf('GammaCorrect', GammaColorCorrection, True);
  prop.BooleanValueOf('Sinus', Sinus, False);
end;

{ TFontDescriptor }

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
  FontName := aFontName;
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
  aStyle: TFontStyles; const aFontGradient: TFontGradient);
begin
  Create(aFontName, aFontHeight, aStyle, aFontGradient,
         BGRAPixelTransparent, 0.0,
         BGRAPixelTransparent, 0, 0, 0);

end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; const aFontGradient: TFontGradient;
  aOutLineColor: TBGRAPixel; aOutLineWidth: single);
begin
  Create(aFontName, aFontHeight, aStyle, aFontGradient,
         aOutLineColor, aOutLineWidth,
         BGRAPixelTransparent, 0, 0, 0);
end;

procedure TFontDescriptor.Create(const aFontName: string; aFontHeight: integer;
  aStyle: TFontStyles; const aFontGradient: TFontGradient; aOutLineColor: TBGRAPixel;
  aOutLineWidth: single; aShadowColor: TBGRAPixel; aShadowOffsetX,
  aShadowOffsetY, aShadowRadius: integer);
var ima: TBGRABitmap;
begin
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

procedure TFontDescriptor.DuplicateTo(aDest: PFontDescriptor);
begin
  aDest^.FontName := Copy(FontName, 1, Length(FontName));
  aDest^.FontHeight := FontHeight;
  aDest^.Style := Style;
  aDest^.OutLineColor := OutLineColor;
  aDest^.OutLineWidth := OutLineWidth;
  aDest^.ShadowColor := ShadowColor;
  aDest^.ShadowOffsetX := ShadowOffsetX;
  aDest^.ShadowOffsetY := ShadowOffsetY;
  aDest^.ShadowRadius := ShadowRadius;
  aDest^.UseGradient := UseGradient;
  aDest^.FontColor := FontColor;
  aDest^.Gradient := Gradient;
  aDest^.FontPixelMetric := FontPixelMetric;
end;

procedure TFontDescriptor.ComputeMaxHeightFor(const aString: string; const aRect: TRect);
var ima: TBGRABitmap;
  siz: TSize;
  familyItem: TCustomFamilyCollectionItem;
  font: TFreeTypeFont;
  dsx, dsy: integer;
begin
  ima := TBGRABitmap.Create(1, 1);
  SetFontRendererOn(ima);
  ima.FontFullHeight := 1;

  dsx := DecorationSize.cx;
  dsy := DecorationSize.cy;

  familyItem := FontCollection.Family[FontName];
  if familyItem <> NIL then begin
    font := familyItem.Font[0].Createfont;
    try
      font.SizeInPixels := 1;
      repeat
        font.SizeInPixels := font.SizeInPixels + 1;
        siz.cx := Round(font.TextWidth(aString)) + dsx;
        siz.cy := Round(font.TextHeight(aString)) + dsy;
      until (siz.cx > aRect.Width) or (siz.cy > aRect.Height);
      FontHeight := Round(font.SizeInPixels);
    finally
      font.Free;
    end;
  end else begin
    repeat
      ima.FontFullHeight := ima.FontFullHeight + 1;
      siz := ima.TextSize(aString);
    until (siz.cx > aRect.Width) or (siz.cy > aRect.Height);
    FontHeight := ima.FontFullHeight - 1;
  end;
  Check;
  ima.Free;
end;

function TFontDescriptor.GetFreeTypeFont: TFreeTypeFont;
var familyItem: TCustomFamilyCollectionItem;
begin
  familyItem := FontCollection.Family[FontName];
  if familyItem <> NIL then Result := familyItem.Font[0].Createfont
    else Result := NIL;
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
 {   _ShadowOffsetX, _ShadowOffsetY, _ShadowRadius: integer;
    _OutLineWidth: single; }
   grad: TBGRAGradientScanner;
   familyItem: TCustomFamilyCollectionItem;
   font: TFreeTypeFont;
begin
  familyItem := FontCollection.Family[FontName];
  if familyItem <> NIL then begin
    font := familyItem.Font[0].Createfont;
    try
      font.SizeInPixels := FontHeight;
      ts.cx := Round(font.TextWidth(aStr));
      ts.cy := Round(font.TextHeight(aStr));
    finally
      font.Free;
    end;
  end else ts := aBitmap.TextSize(aStr);

  // size of the rendered font. try to take into account shadow and outline.
  ts := ts + DecorationSize;

  // to avoid a division by 0
  if ts.cx = 0 then ts.cx := 1;
  if ts.cy = 0 then ts.cy := 1;

  aBitmap.SetSize(ts.cx, ts.cy);

{  // force shadow param. to 0 if there is no shadow
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
  if _ShadowOffsetY < 0 then yy += Abs(_ShadowOffsetY);  }

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
var renderer: TBGRAFreeTypeFontRenderer;
begin
  if aBitmap = NIL then exit;

  renderer := TBGRAFreeTypeFontRenderer.Create;
  aBitmap.FontRenderer := renderer;
  aBitmap.FontName := FontName;
  aBitmap.FontFullHeight:= FontHeight;
  aBitmap.FontQuality := fqFineClearTypeRGB;
  aBitmap.FontStyle := Style;
  if UseShadow then begin
    renderer.ShadowVisible := True;
    renderer.ShadowColor := Self.ShadowColor;
    renderer.ShadowOffset := Point(ShadowOffsetX, ShadowOffsetY);
    renderer.ShadowRadius := Self.ShadowRadius;
    renderer.ShadowQuality := rbFast;
  end;
  if UseOutLine then begin
    renderer.OutlineVisible := True;
    renderer.OutlineColor := Self.OutLineColor;
    renderer.OuterOutlineOnly := True;
  end;

{  end else begin
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
  end;  }

//  aBitmap.FontQuality := fqFineAntialiasing; //fqFineAntialiasing    fqSystemClearType
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

function TFontDescriptor.SaveToString: string;
var prop: TProperties;
  s: string;
begin
  prop.Init('#');
  prop.Add('FontName', FontName, True);
  prop.Add('FontHeight', FontHeight);
  s := '';
  if fsBold in Style then s := s+'B';
  if fsItalic in Style then s := s+'I';
  if fsUnderline in Style then s := s+'U';
  if fsStrikeOut in Style then s := s+'S';
  prop.Add('Style', s);
  prop.Add('OutLineColor', OutLineColor);
  prop.Add('OutLineWidth', OutLineWidth);
  prop.Add('ShadowColor', ShadowColor);
  prop.Add('ShadowOffsetX', ShadowOffsetX);
  prop.Add('ShadowOffsetY', ShadowOffsetY);
  prop.Add('ShadowRadius', ShadowRadius);
  prop.Add('UseGradient', UseGradient);
  prop.Add('FontColor', FontColor);
  if UseGradient then prop.Add('GradientData', Gradient.SaveToString);
  Result := prop.PackedProperty;
end;

procedure TFontDescriptor.LoadFromString(const data: string);
var prop: TProperties;
  s: string;
begin
  s := '';
  prop.Split(data, '#');
  prop.StringValueOf('FontName', FontName, '');
  prop.IntegerValueOf('FontHeight', FontHeight, 12);
  prop.StringValueOf('Style', s, '');
  Style := [];
  if s.Contains('B') then Include(Style, fsBold);
  if s.Contains('I') then Include(Style, fsItalic);
  if s.Contains('U') then Include(Style, fsUnderline);
  if s.Contains('S') then Include(Style, fsStrikeOut);
  prop.BGRAPixelValueOf('OutLineColor', OutLineColor, OutLineColor);
  prop.SingleValueOf('OutLineWidth', OutLineWidth, OutLineWidth);
  prop.BGRAPixelValueOf('ShadowColor', ShadowColor, ShadowColor);
  prop.IntegerValueOf('ShadowOffsetX', ShadowOffsetX, ShadowOffsetX);
  prop.IntegerValueOf('ShadowOffsetY', ShadowOffsetY, ShadowOffsetY);
  prop.IntegerValueOf('ShadowRadius', ShadowRadius, ShadowRadius);
  prop.BooleanValueOf('UseGradient', UseGradient, UseGradient);
  if UseGradient then begin
    prop.StringValueOf('GradientData', s, '');
    Gradient.LoadFromString(s);
  end;
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

{ TColorF }

function TColorF.GetAlphaByte: byte;
begin
  Result := Round(a*255);
end;

function TColorF.GetBlueByte: byte;
begin
  Result := Round(b*255);
end;

function TColorF.GetGreenByte: byte;
begin
  Result := Round(g*255);
end;

function TColorF.GetRedByte: byte;
begin
  Result := Round(r*255);
end;

procedure TColorF.SetAlphaByte(AValue: byte);
begin
  a := AValue/255;
end;

procedure TColorF.SetBlueByte(AValue: byte);
begin
  b := AValue/255;
end;

procedure TColorF.SetGreenByte(AValue: byte);
begin
  g := AValue/255;
end;

procedure TColorF.SetRedByte(AValue: byte);
begin
  r := AValue/255;
end;

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

{ TOGLCFontManager }

function TOGLCFontManager.GetSystemFontName(index: integer): string;
begin
  Result := FSystemFontNames[index];
end;

// from https://forum.lazarus.freepascal.org/index.php?topic=32835.0
procedure TOGLCFontManager.ScanSystemFonts;
var i: integer;
  item: TCustomFontCollectionItem;
  filesFound: TStringList;
  procedure ScanFolder(const aPath: string);
  var t: TStringList;
    j: integer;
  begin
    if not DirectoryExistsUTF8(aPath) then exit;
    t := FindAllFiles(aPath, '*.ttf', True);
    for j:=0 to t.Count-1 do
      filesFound.Add(t.Strings[j]);
    t.Free;
  end;

begin
  FFontCollection.Clear;
  // scan the system font directories
  filesFound := TStringList.Create;
  {$ifdef windows}
   ScanFolder(SHGetFolderPathUTF8(20));  // CSIDL_FONTS = 20
  {$endif}
  {$ifdef linux}
   ScanFolder('/usr/share/fonts/truetype/');
   ScanFolder('/usr/share/cups/fonts/');
   ScanFolder('/usr/local/lib/X11/fonts/');
   ScanFolder(GetUserDir + '.fonts/');
  {$endif}
  {$ifdef darwin}
   ScanFolder('/System/Library/Fonts/');
  {$endif}

  // retrieve the filename and the font name
  FSystemFontNames := NIL;
  FSystemFontFilenames := NIL;
  if filesFound.Count = 0 then exit;

  SetLength(FSystemFontFilenames, filesFound.Count);
  SetLength(FSystemFontNames, filesFound.Count);
  FFontCollection.BeginUpdate;
  try
    for i:=0 to filesFound.Count-1 do begin
      try
        item := FFontCollection.AddFile(filesFound.Strings[i]);
      except
        item := NIL;
      end;
      if item <> NIL then begin
        FSystemFontNames[i] := item.Family.FamilyName;
        FSystemFontFilenames[i] := filesFound.Strings[i];
      end;
    end;
  finally
    FFontCollection.EndUpdate;
  end;
  filesFound.Free;
end;

function TOGLCFontManager.GetProjectFontName(index: integer): string;
begin
  Result := FProjectFontNames[index];
end;

function TOGLCFontManager.SystemFontIndexOf(aFontName: string): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to High(FSystemFontNames) do
    if CompareText(aFontName, FSystemFontNames[i]) = 0 then exit(i);
end;

function TOGLCFontManager.ProjectFontIndexOf(aFontName: string): integer;
var i: integer;
begin
  Result := -1;
  for i:=0 to High(FProjectFontNames) do
    if CompareText(aFontName, FProjectFontNames[i]) = 0 then exit(i);
end;

procedure TOGLCFontManager.ListFontFamily(AFamilyName: string);
var
  familyItem: TCustomFamilyCollectionItem;
  i: Integer;
  font: TFreeTypeFont;
begin
  WriteLn('Font family "', AFamilyName, '":');
  familyItem := FontCollection.Family[AFamilyName];
  if familyItem = nil then
    WriteLn('  not found.')
  else begin
  WriteLn('  Font count: ', familyItem.FontCount);
  for i:=0 to familyItem.FontCount-1 do begin
    WriteLn('  Font #', i+1, ':');
    WriteLn('                Filename: ', familyItem.Font[i].Filename);
    WriteLn('                  styles: ', familyItem.Font[i].Styles);
    WriteLn('                    bold: ', BoolToStr(familyItem.Font[i].Bold, 'true', 'false'));
    WriteLn('                  italic: ', BoolToStr(familyItem.Font[i].Italic, 'true', 'false'));
    font := familyItem.Font[i].Createfont;
    try
      font.SizeInPoints := 12;
      WriteLn('     TextWidth(''M'')@12pt: ', FloatToStr(font.TextWidth('M')));
      font.SizeInPoints := 48;
      WriteLn('     TextWidth(''M'')@48pt: ', FloatToStr(font.TextWidth('M')));
    finally
      font.Free;
    end;
    WriteLn;
  end;
  end;
  WriteLn('----------------------------------------------------------------------');
end;

constructor TOGLCFontManager.Create;
begin
  FFontCollection := TFreeTypeFontCollection.Create;
  ScanSystemFonts;
  SetDefaultFreeTypeFontCollection(FFontCollection);
end;

destructor TOGLCFontManager.Destroy;
begin
  SetDefaultFreeTypeFontCollection(NIL);
  FreeAndNil(FFontCollection);
  inherited Destroy;
end;

procedure TOGLCFontManager.ScanProjectFont(const aProjectFontDirectory: string);
var i: integer;
  item: TCustomFontCollectionItem;
  filesFound: TStringList;
begin
  FFontCollection.Clear;
  ScanSystemFonts;

  filesFound := FindAllFiles(aProjectFontDirectory, '*.ttf', False);
  FProjectFontNames := NIL;
  FProjectFontFilenames := NIL;
  if filesFound.Count > 0 then begin
    FFontCollection.BeginUpdate;
    try
      SetLength(FProjectFontNames, filesFound.Count);
      SetLength(FProjectFontFilenames, filesFound.Count);
      for i:=0 to filesFound.Count-1 do begin
        FProjectFontFilenames[i] := filesFound.Strings[i];
        item := FFontCollection.AddFile(filesFound.Strings[i]);
        FProjectFontNames[i] := item.Family.FamilyName;
      end;
    finally
      FFontCollection.EndUpdate;
    end;
  end;

  filesFound.Free;
end;

procedure TOGLCFontManager.ClearProjectFonts;
begin
  FProjectFontNames := NIL;
  FProjectFontFilenames := NIL;

  ScanSystemFonts;
end;

function TOGLCFontManager.SystemFontCount: integer;
begin
  Result := length(FSystemFontNames);
end;

function TOGLCFontManager.ProjectFontCount: integer;
begin
  Result := Length(FProjectFontNames);
end;

function TOGLCFontManager.GetFreeTypeFont(aFontName: string): TFreeTypeFont;
var familyItem: TCustomFamilyCollectionItem;
begin
  familyItem := FontCollection.Family[aFontName];
  if familyItem = NIL then exit(NIL);
  Result := familyItem.Font[0].Createfont;
end;

procedure TOGLCFontManager.FontHaveBoldOrItalic(const aFontName: string; out
  CanBold, CanItalic: boolean);
var i: Integer;
  item: TCustomFontCollectionItem;
begin
  CanBold := False;
  CanItalic := False;
  item := NIL;

  // try with system fonts
  i := SystemFontIndexOf(aFontName);
  if i <> -1 then begin
    item := FFontCollection.FontFile[FSystemFontFilenames[i]];
    if item <> NIL then begin
      CanBold := item.Bold;
      CanItalic := item.Italic;
    end;
    exit;
  end;

  // try with project fonts
  i := ProjectFontIndexOf(aFontName);
  if i <> -1 then begin
    item := FFontCollection.FontFile[FProjectFontFilenames[i]];
    if item <> NIL then begin
      CanBold := item.Bold;
      CanItalic := item.Italic;
    end;
  end;
end;

{$endif oglcIMPLEMENTATION}

