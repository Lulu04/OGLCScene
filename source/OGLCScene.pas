{
  **************************************************************************
 *                                                                          *
 *  This file is the main part of OGLCScene library.                        *
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenGL 3.3 core and FreePascal/Lazarus,     *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

 written by Lulu  2017 - 2025
}

{
  File 'oglcParticle.inc' contains the code for the particle emitter.
  This code is a modified version from ZENGL version 0.3.8 written by Andrey Kemka
}

unit OGLCScene;
{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}


//{$define DEBUG_MODE_ON}  // uncomment to draw a red rectangle around all surfaces.

{$define USE_glcorearb}  // uncomment to use the glcorearb.pas header for OpenGL written by Chris Rorden.
                         // comment to use fpc header GL and GLExt

// Compiling in debug mode raise exception when a shader compilation error occurs.
// In release mode, shader compilation error only add a message in the scene log file.
{$ifopt D+}
  {$define SHADER_RAISE_EXCEPTION_ON_COMPILATION_ERROR}
{$endif}

{$ifdef USE_glcorearb}
  {$MACRO ON}
  {$define GL:=glcorearb}
{$endif}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, Controls, lcltype, LazFileUtils,
  types, {contnrs,}
  OpenGLContext,
  {$ifdef USE_glcorearb}glcorearb,{$else} GLExt, GL,{$endif}
  DynLibs,
  lazutf8,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRAPath, BGRAGradientScanner, BGRAFreeType, BGRASVG,//EasyLazFreeType,
  math,
  strutils, gvector;


type
TOGLCScene = class;
TTexturedFont = class;

// simple callback
TOGLCEvent = procedure of object;

{ in the scene, each surface can play scenarios. A scenario is an external text file
  that contain the actions to execute.
  Below you can find all the available actions to build your scenario
}

// SCENARIO
TIDScenario = integer;
const
     // Boolean value
     acTRUE                 = 'TRUE';
     acFALSE                = 'FALSE';
     // Available actions for surface   (suffix: I for integer F for float)
     acLoop                 =  'Loop'    ; // Loop
     acLabel                =  'Label'   ; // Label UserLabelName
     acGotoLabel            =  'Goto'    ; // Goto UserLabelName
     acWait                 =  'Wait'    ; // Wait secondF

     acKill                 =  'Kill'    ; // Kill
     acFreeze               =  'Freeze'  ; // Freeze TRUE/FALSE
     acVisible              =  'Visible' ; // Visible TRUE/FALSE

     acFlipH                =  'FlipH'       ; // FlipH TRUE/FALSE
     acFlipV                =  'FlipV'       ; // FlipV TRUE/FALSE
     acToggleFlipH          =  'ToggleFlipH' ; // ToggleFlipH
     acToggleFlipV          =  'ToggleFlipV' ; // ToggleFlipV

     acOpacity              =  'Opacity' ;       // Opacity NewOpacityI
     acOpacityChange        =  'OpacityChange' ; // OpacityChange NewOpacityI DurationF CurveID

     acAnimate              =  'Animate' ; // Animate StartFrameIndexI EndFrameIndexI FramePerSecondF
     acIncFrame             =  'IncFrame'; // IncFrame
     acDecFrame             =  'DecFrame'; // DecFrame
     acSetFrame             =  'SetFrame'; // SetFrame FrameIndexI

     acTint                 =  'Tint'    ; // Tint redI greenI blueI alphaI
     acTintChange           =  'TintChange' ; // TintChange RedI GreenI BlueI AlphaI DurationF CurveID
     acTintRedChange        =  'TintRedChange' ;   // TintRedChange NewRedValueI DurationF CurveID
     acTintGreenChange      =  'TintGreenChange' ; // TintGreenChange NewGreenValueI DurationF CurveID
     acTintBlueChange       =  'TintBlueChange' ;  // TintBlueChange NewBlueValueI DurationF CurveID
     acTintAlphaChange      =  'TintAlphaChange' ; // TintAlphaChange NewAlphaValueI DurationF CurveID

     acAngle                = 'Angle'    ; // Angle NewAngleF
     acRotate               = 'Rotate'   ; // Rotate AnglePerSecondF
     acRotateTo             = 'RotateTo' ; // RotateTo AngleF DurationF CurveID
     acRotationAroundAxis   = 'RotationAroundAxis'    ; // RotationAroundAxis XAxisF YAxisF AnglePerSecondF SelfRotate

     acScale                = 'Scale'          ; // Scale HVValueF      // [1]= normal size, [0..1[->reduced, ]1..inf[->enlarged
     acScaleChange          = 'ScaleChange'    ; // ScaleChange HVNewValueF DurationF CurveID
     acScaleH               = 'ScaleH'         ; // ScaleH HValueF      // to set scale value on horizontaly axis
     acScaleHChange         = 'ScaleHChange'   ; // ScaleHChange HNewValueF DurationF CurveID
     acScaleV               = 'ScaleV'         ; // ScaleV VValueF      // to set scale value on verticaly axis
     acScaleVChange         = 'ScaleVChange'   ; // ScaleVChange VNewValueF DurationF CurveID


     acBlink                = 'Blink'    ; // Blink NumberOfBlinkI(-1 for infinite) aVisibleTimeF aInvisibleTimeF
     acStopBlink            = 'StopBlink'; // StopBlink

     acMoveTo               = 'MoveTo'         ; // MoveTo XF YF DurationF CurveID
     acMoveXTo              = 'MoveXTo'        ; // MoveXTo XF DurationF CurveID
     acMoveYTo              = 'MoveYTo'        ; // MoveYTo YF DurationF CurveID

     acMoveCenterTo         = 'MoveCenterTo'   ; // MoveCenterTo XcenterF YCenterF DurationF CurveID
     acMoveXCenterTo        = 'MoveXCenterTo'  ; // MoveXCenterTo XF DurationF CurveID
     acMoveYCenterTo        = 'MoveYCenterTo'  ; // MoveYCenterTo YF DurationF CurveID

     acMoveRelative         = 'MoveRelative'   ; // MoveRelative DeltaXF DeltaYF DurationF CurveID
     acMoveXRelative        = 'MoveXRelative'  ; // MoveXRelative DeltaXF DurationF CurveID
     acMoveYRelative        = 'MoveYRelative'  ; // MoveYRelative DeltaYF DurationF CurveID

     acSetCoor              = 'SetCoor'        ; // SetCoor XF YF
     acSetCenterCoor        = 'SetCenterCoor'  ; // SetCenterCoor XF YF

     acCenterOnScene        = 'CenterOnScene'  ; // CenterOnScene

     // Other actions
     acPostMessage          = 'PostMessage'; // PostMessage ValueI DelayF

const
  // available predefined curves IDentificator
  idcLinear = 0;
  idcStartFastEndSlow = 1;
  idcStartSlowEndFast = 2;
  idcSinusoid = 3;
  idcSinusoid2 = 4;
  idcBouncy = 5;
  idcSpring = 6;
  idcExtend = 7;
  idcExtend2 = 8;
  idc5Steps = 9;
  idcDrop = 10;
  idcSlowAtMiddle = 11;
  idcPauseAtMiddle = 12;
  idcSpring2 = 13;
  idcSingleRebound = 14;

// Blend mode
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD    = $01;
  FX_BLEND_MULT   = $02;
  FX_NOBLEND      = $03;


type

TOGLCCamera = class;

{$define oglcINTERFACE}
{$I oglcVersion.inc}
{$I oglcLogFile.inc}
{$I oglcVelocityCurve.inc}
{$I oglcCharset.inc}
{$I oglcMath.inc}
{$I gl_core_matrix.inc}
{$I oglcType.inc}
{$I oglcAppSaveUtils.inc}
{$I oglcShader.inc}
{$I oglcMessage.inc}
{$I oglcTexture.inc}
{$I oglcTextureAtlas.inc}
{$I oglcTimer.inc}
{$I oglcLayer.inc}
{$I oglcCollisionBody.inc}
{$I oglcSurface.inc}
{$I oglcPath.inc}
{$I oglcCamera.inc}
{$I oglcBorderAndFill.inc}
{$I oglcMultiRendererFactory.inc}
{$I oglcRendererToTexture.inc}
{$I oglcSurfaceWithProceduralShader.inc}
{$I oglcPostProcessing.inc}
{$I oglcSpriteTemplate.inc}
{$I oglcDecorManager.inc}
{$I oglcScreenTemplate.inc}
{$I oglcFXExplodeTexture.inc}
{$I oglcFXScrolledSprite.inc}
{$I oglcElectricalFX.inc}
{$I oglcGlow.inc}
{$I oglcDeformationGrid.inc}
{$I oglcSlideShow.inc}
{$I oglcUIClipping.inc}
{$I oglcTileEngine.inc}
{$I oglcParticle.inc}
{$I oglcTexturedFont.inc}
{$I oglcAlignText.inc}
{$I oglcFreeText.inc}
{$I oglcUITheme.inc}
{$I oglcUI.inc}
{$I oglcUIScrollable.inc}
{$I oglcUIPanelWithEffects.inc}
{$I oglcUIModalPanel.inc}
{$I oglcReusableSurfaceContainer.inc}
{$I oglcUtils.inc}
{$I oglcEnvironment.inc}
{$I oglcGpuInfo.inc}
{$undef oglcINTERFACE}

type




{ TOGLCContext }

TOGLCContext = class(TLayerList)
 protected
  FOGLC: TOpenGLControl;
  FOpenGLLibLoaded: boolean;
  FAspectRatio: single;
  function GetMouseCoorOnViewPort: TPoint;
 private
  const DEFAULT_DESIGN_PPI = 96;
  var FDesignPPI: integer;
  FPPIScaleFactor: single;
 private
  FPreviousOnResize: TNotifyEvent;
  FPreviousOnMouseDown, FPreviousOnMouseUp: TMouseEvent;
  FPreviousOnMouseMove: TMouseMoveEvent;
  FPreviousOnMouseWheel: TMouseWheelEvent;

  FGLInitialized,
  FNeedToResizeViewPort: boolean;
  FKeyState: array[0..255] of boolean;
  FKeyPressed: array[0..255] of boolean;
  FLastKeyDown: Byte;
  FLastUTF8CharEntered: string;
  FMouseManager: TMouseManager;
  FTextureManager: TTextureManager;
  FTimerManager: TTimerManager;
  function GetKeyToString(index: byte): string;
  function GetLastUTF8CharEntered: string;
  function GetMouseButtonState(btn: TMouseButton): boolean;
  function GetUserPressAKey: boolean;
  procedure ProcessOnResize(Sender:TObject);
  procedure ProcessOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  procedure ProcessOnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  function GetKeyState(index: byte): boolean;
  function GetKeyPressed(index: byte): boolean;
  function GetViewPortHeight: integer;
  function GetViewPortWidth: integer;
  function GetViewPortTopLeft: TPointF;
  function GetViewPortCenter: TPointF;
  procedure UpdateViewPortSize; virtual;
  procedure SetDesignPPI(AValue: integer);
  procedure SetSystemMouseCursorVisible(AValue: boolean);
  function MakeContextCurrent: boolean;
 public
  App: TApp;
  Gpu: TGpuInfo;
  constructor Create(aOGLContext: TOpenGLControl; aAspectRatio: single); virtual;
  destructor Destroy; override;

  // Allow to change the scene aspect ratio at runtime.
  // Warning: the coordinates of the existing surfaces on the scene are not modified.
  procedure ChangeAspectRatioTo(aAspectRatio: single);

  // must be called from main windows to ensure keyboard input
  procedure ProcessOnKeyDown(var Key: Word; Shift: TShiftState);
  // must be called from main windows to ensure keyboard input
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
  // must be called from main windows to ensure keyboard UTF8Char input
  procedure ProcessOnUTF8KeyPress(var UTF8Key: TUTF8Char);

  procedure ClearKeysState;
  property LastKeyDown: byte read FLastKeyDown;
  // Return True if user press a key on the keyboard.
  property UserPressAKey: boolean read GetUserPressAKey;
  // Index is a LCLType.VK_xx key code. Return TRUE if Key is currently pressed.
  property KeyState[index:byte]: boolean read GetKeyState;
  // Index is a LCLType.VK_xx key code. Return TRUE if a key was pressed then released
  property KeyPressed[index: byte]: boolean read GetKeyPressed;
  // Index is a LCLType.VK_xx key code. Return a string representation of the key
  property KeyToString[index: byte]: string read GetKeyToString;
  // Return the last UTF8 character entered by player since the last call of this property.
  // Empty string is returned if the player haven't pressed a key that generate a displayable character.
  // Reading this property reset it to an empty string.
  property LastUTF8CharEntered: string read GetLastUTF8CharEntered;

  property MouseButtonState[btn: TMouseButton]: boolean read GetMouseButtonState;

  function ScaleDesignToScene(ASize: integer): integer;
  function ScaleDesignToSceneF(ASize: single): single;
  // Set here the design PPI for your app. This value will affects the value returned by
  // methods ScaleDesignToScene() and ScaleDesignToSceneF().
  property DesignPPI: integer read FDesignPPI write SetDesignPPI;
  // True if the OpenGL library was successfully loaded
  property OpenGLLibLoaded: boolean read FOpenGLLibLoaded;
end;


{ TOGLCScene }
TOGLCScene = class(TOGLCContext)
 private
  FWaitLoopCountBeforeRunning: integer;
  FLog: TLog;
  FCurrentScreen,
  FScreenRequested: TScreenTemplate;
  FFadeBlackColorOnNewScreen: boolean;
 private // renderer batch
  FRendererBatch: TRendererBatchDescriptor;
  FCurrentShaderIDInUse: GLuint;
 private
  FSmoothLineRenderer: TOGLCSmoothLineRenderer;
  FTileRenderer: TOGLCTileRenderer;
  FGlowRenderer: TOGLCGlowRenderer;
  FElectricalBeamRenderer: TOGLCElectricalBeamRenderer;
  FTexturedTriangleRenderer: TOGLCTexturedTriangleRenderer;
  FTexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer;
  FThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer;
  FThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer;
  FFastLineRenderer: TOGLCFastLineRenderer;
  FParticleRenderer: TOGLCParticleRenderer;
  FNoFilterTextureMVTriangleRenderer: TOGLCNoFilterTextureMVTriangleRenderer;
  FTexturedTriangleRendererWithFX: TOGLCTexturedTriangleRendererWithFX;
  FScrollableTextureRenderer: TOGLCScrollableTextureRenderer;
  function GetFastLineRenderer: TOGLCFastLineRenderer;
  function GetGlowRenderer: TOGLCGlowRenderer;
  function GetElectricalBeamRenderer: TOGLCElectricalBeamRenderer;
  function GetParticleRenderer: TOGLCParticleRenderer;
  function GetSmoothLineRenderer: TOGLCSmoothLineRenderer;
  function GetTexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer;
  function GetTexturedTriangleRenderer: TOGLCTexturedTriangleRenderer;
  function GetTexturedTriangleRendererWithFX: TOGLCTexturedTriangleRendererWithFX;
  function GetThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer;
  function GetThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer;
  function GetTileRenderer: TOGLCTileRenderer;
  function GetNoFilterTexturedMVTriangleRenderer: TOGLCNoFilterTextureMVTriangleRenderer;
  function GetScrollableTextureRenderer: TOGLCScrollableTextureRenderer;
  procedure CreateRenderers;
  procedure DestroyRenderers;
 private
  FModalPanelList: TModalPanelList;
 private
  procedure UpdateViewPortSize; override;
  procedure Draw;
  procedure UpDate(const aElapsedTime:single);
 private
  // FPS
  FFPSCounter: integer;
  FFPS: integer;
  FTickOriginUpdate, FTickAccu: QWord;
  FUpdatePeriod: integer;
  FExecuteDuringLoop: boolean;
  procedure SetUpdatePeriod(AValue: integer);
  procedure CallBackTimerFPS;
 private
  FGlobalFadeColor: TBGRAParam;
  FBackgroundColorF: TColorF;
  FScreenFadeTime: single;
  procedure SetBackgroundColor (aColor:TBGRAPixel);
 private
  // CallBack
  FOnBeforePaint,
  FOnAfterPaint,
  FOnLoadCommonData,
  FOnFreeCommonData : TOGLCEvent;
  FCommonDataLoaded: boolean;
  procedure SetLayerCount(AValue: integer); override;
 private
  FCameraList: TList;
  function GetBackgroundColor: TBGRAPixel;
 private
  FStencilClipping: TStencilClipping;
 private
  FBlendIsEnabled: boolean;
  FCurrentBlendMode: byte;
  procedure SetBlendMode(AValue: byte);
 private
  FPostProcessingEngine: TOGLCPostProcessingEngine;
 private
  FLoadingMessageSprite: TSprite;
 public
  // Post processing effects can be applyed on layers.
  // this property offers a bunch of tools to facilitate post-processing control.
  PostProcessing: TOGLCPostProcessingUtils;
  // This property offers a bunch of functions to test collision between different shapes.
  Collision: TCollisionFunctions;
  // This property offers a bunch of charset that you can use while creating your font texture.
  // Use one or add several charset to match your language.
  Charsets: TOGLCCharsets;

  // Aspect ratio can be any value >= 1.0 ex. 4/3, 16/9. The TOpenGLControl is resized to respect the aspect ratio
  // and centered on its parent.
  // If you set aspect ratio to -1 the TOpenGLControl is resized to fit its parent client area.
  constructor Create(aOGLContext: TOpenGLControl; aAspectRatio: single=-1); override;
  Destructor Destroy; override;

  // define aCallback if you want to receive messages generated by the scene into
  // your own procedure.
  procedure CreateLogFile(const aFilename: string; aDeletePrevious: boolean; aCallback: TOGLCLogCallback=NIL; aCallbackData: pointer=NIL);
  procedure LogEmptyLine(aSeparator: string='');
  procedure LogMess(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False);
  procedure LogInfo(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False);
  procedure LogWarning(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False);
  procedure LogError(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False);
  procedure LogDebug(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False);
  procedure LogStartMeasuringTime(const aMsg: string; aMarginCount: integer=0);
  procedure LogStopMeasuringTime(const aMsg: string; aMarginCount: integer=0);

  function GetRectArea: TRect;

  function MakeCurrent: boolean;
  // call DoLoop on Application idle process. It update and draw the scene
  procedure DoLoop;
  procedure ExecuteDuring( aTimeInSecond:single ); Deprecated;

  procedure RunScreen(aScreen: TScreenTemplate; aFadeWithBlackColor: boolean=TRUE);
  // the requested screen after a call to RunScreen()
  property RequestedScreen: TScreenTemplate read FScreenRequested;
  // the current running screen
  property CurrentScreen: TScreenTemplate read FCurrentScreen;
  // The duration of the fade when a new screen is started with RunScreen().
  // default value is 0.5s
  property ScreenFadeTime: single read FScreenFadeTime write FScreenFadeTime;

  // Add a surface to a layer
  procedure Add(aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0);
  // insert a surface to a layer at specified index
  procedure Insert(aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  // remove a surface from the layer, the surface is not freed
  procedure RemoveSurfaceFromLayer(aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer);
  procedure RemoveSurfaceFromItsLayer(aSurface: TSimpleSurfaceWithEffect);
  // remove a surface from its current layer and add it to the new layer index
  procedure MoveSurfaceToLayer(aSurface: TSimpleSurfaceWithEffect; aNewLayerIndex: integer);
  function GetSurfaceByIndex(aLayerIndex, aSurfaceIndex: integer): TSimpleSurface;

  // Global scene color fade in and out
  procedure ColorFadeIn(const aColor: TBGRAPixel; const aDurationInSecond: single);
  procedure ColorFadeOut(const aDurationInSecond: single);

  property FPS: integer read FFPS;
  // set the update period in ms. Default value is 16ms.
  // set to 0 to force update on each rendered frame (animation will not be smooth)
  property UpdatePeriod: integer read FUpdatePeriod write SetUpdatePeriod;

  property Width: integer read GetViewPortWidth;
  property Height: integer read GetViewPortHeight;
  property TopLeft: TPointF read GetViewPortTopLeft;
  property Center: TPointF read GetViewPortCenter;
  property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor;
  property BlendMode: byte read FCurrentBlendMode write SetBlendMode;
  // Callback
  property OnBeforePaint: TOGLCEvent read FOnBeforePaint write FOnBeforePaint;
  property OnAfterPaint: TOGLCEvent read FOnAfterPaint write FOnAfterPaint;
  // use this callback to load your global ressources
  property OnLoadCommonData: TOGLCEvent read FOnLoadCommonData write FOnLoadCommonData;
  // use this callback to free your global ressources
  property OnFreeCommonData: TOGLCEvent read FOnFreeCommonData write FOnFreeCommonData;

  property Mouse: TMouseManager read FMouseManager;
  property TexMan: TTextureManager read FTextureManager;
  property Timer: TTimerManager read FTimerManager;

  property StencilClipping: TStencilClipping read FStencilClipping;
 public
  ProjectionMatrix,        // projection matrix
  ModelViewMatrix: TOGLCMatrix;   // Model View matrix
  function MVPMatrix: TOGLCMatrix; // Model View Projection matrix

  // a renderer is instancied only when you use it.
  property SmoothLineRenderer: TOGLCSmoothLineRenderer read GetSmoothLineRenderer;
  property TileRenderer: TOGLCTileRenderer read GetTileRenderer; // is instancied only when a TOGLCTileMap is instancied
  property GlowRenderer: TOGLCGlowRenderer read GetGlowRenderer; // is instancied only when a TOGLCGlow is instancied
  property ElectricalBeamRenderer: TOGLCElectricalBeamRenderer read GetElectricalBeamRenderer;
  property ParticleRenderer: TOGLCParticleRenderer read GetParticleRenderer;
  property FastLineRenderer: TOGLCFastLineRenderer read GetFastLineRenderer;
  property ThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer read GetThreeColorTriangleRenderer;
  property ThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer read GetThreeColorMVTriangleRenderer;
  property TexturedTriangleRenderer: TOGLCTexturedTriangleRenderer read GetTexturedTriangleRenderer;
  property TexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer read GetTexturedMVTriangleRenderer;
  property NoFilterTexturedMVTriangleRenderer: TOGLCNoFilterTextureMVTriangleRenderer read GetNoFilterTexturedMVTriangleRenderer;
  property TexturedTriangleRendererWithFX: TOGLCTexturedTriangleRendererWithFX read GetTexturedTriangleRendererWithFX;
  property ScrollableTextureRenderer: TOGLCScrollableTextureRenderer read GetScrollableTextureRenderer;
  // Flush the batch renderer system
  procedure FlushRenderer;
 public // convenience functions
  function CreateAtlas: TOGLCTextureAtlas;
  function CreateTexturedFont(aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap): TTexturedFont;
  // Create a camera for this scene. You can assign the camera to one or several layer.
  // Don't forget to free it with KillCamera(...).
  function CreateCamera: TOGLCCamera;
  // unassign the camera on all layers, and free it.
  // After this method, aCamera is set to NIL.
  procedure KillCamera(var aCamera: TOGLCCamera);

  function CreateTileEngine(aLayer: integer=0): TTileEngine;

  function CreateSprite(aTexture: PTexture; aTextureOwner: boolean; aLayer: integer=0): TSprite; deprecated 'use AddSprite instead';
  function AddSprite(aTexture: PTexture; aTextureOwner: boolean; aLayer: integer=0): TSprite; overload;
  function AddSprite(aTexture: PTexture; aTextureOwner: boolean; aLayerIndex: integer; aX, aY: single): TSprite; overload;
  function Add_FreeText(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TFreeText;

  function Add_UILabel(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUILabel;
  function Add_UIButton(const aCaption: string; aFont: TTexturedFont; aTexture: PTexture; aLayer: integer=0): TUIButton; overload;
  function Add_UICheck(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUICheck; overload;
  function Add_UIRadio(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUIRadio;
  function Add_UIPanel(aLayer: integer=0): TUIPanel;
  function Add_UIProgressBar(aOrientation: TUIOrientation; aLayer: integer=0): TUIProgressBar;
  function Add_UIScrollBar(aOrientation: TUIOrientation; aLayer: integer=0): TUIScrollBar;
  function Add_UIListBox(aFont: TTexturedFont; aLayer: integer=0): TUIListBox;
  function Add_UITextArea(aLayer: integer=0): TUITextArea;
  function Add_UIScrollBox(aUseVScrollBar, aUseHScrollBar: boolean; aLayer: integer=0): TUIScrollBox;
  function Add_ModalPanel: TUIModalPanel;
end;
POGLCScene = ^TOGLCScene;


implementation
uses LclIntf;   // for function GetKeyState()

{ TOGLCContext }

function TOGLCContext.GetMouseCoorOnViewPort: TPoint;
begin
  Result := FOGLC.ScreenToClient(Controls.Mouse.CursorPos)
end;

procedure TOGLCContext.ProcessOnResize(Sender: TObject);
begin
  FNeedToResizeViewPort := TRUE;
  if FPreviousOnResize <> NIL then
    FPreviousOnResize(Sender);
end;

function TOGLCContext.GetMouseButtonState(btn: TMouseButton): boolean;
begin
  Result := FMouseManager.ButtonState[btn];
end;

function TOGLCContext.GetKeyToString(index: byte): string;
begin
  case index of
    VK_LBUTTON: Result := 'L-BUTTON';
    VK_RBUTTON: Result := 'R-BUTTON';
    VK_CANCEL: Result := 'CANCEL';
    VK_MBUTTON: Result := 'M-BUTTON';
    VK_XBUTTON1: Result := 'XBUTTON1';
    VK_XBUTTON2: Result := 'XBUTTON2';
    VK_BACK: Result := 'BACK';
    VK_TAB: Result := 'TAB';
    VK_CLEAR: Result := 'CLEAR';
    VK_RETURN: Result := 'RETURN';
    VK_SHIFT: Result := 'SHIFT';
    VK_CONTROL: Result := 'CONTROL';
    VK_MENU: Result := {$ifdef Darwin}'OPTION';{$else}'ALT';{$endif}
    VK_PAUSE: Result := 'PAUSE';
    VK_CAPITAL: Result := 'CAPSLOCK';
    VK_KANA: Result := 'KANA';
    //VK_HANGUL: Result := 'HANGUL';
    VK_JUNJA: Result := 'JUNJA';
    VK_FINAL: Result := 'FINAL';
    VK_HANJA: Result := 'HANJA';
    //VK_KANJI: Result := 'KANJI';
    VK_ESCAPE: Result := 'ESC';
    VK_CONVERT: Result := 'CONVERT';
    VK_NONCONVERT: Result := 'NONCONVERT';
    VK_ACCEPT: Result := 'ACCEPT';
    VK_MODECHANGE: Result := 'MODECHANGE';
    VK_SPACE: Result := 'SPACE';
    VK_PRIOR: Result := 'PRIOR';
    VK_NEXT: Result := 'NEXT';
    VK_END: Result := 'END';
    VK_HOME: Result := 'HOME';
    VK_LEFT: Result := 'LEFT';
    VK_UP: Result := 'UP';
    VK_RIGHT: Result := 'RIGHT';
    VK_DOWN: Result := 'DOWN';
    VK_SELECT: Result := 'SELECT';
    VK_PRINT: Result := 'PRINT';
    VK_EXECUTE: Result := 'EXECUTE';
    VK_SNAPSHOT: Result := 'SNAPSHOT';
    VK_INSERT: Result := 'INSERT';
    VK_DELETE: Result := 'DELETE';
    VK_HELP: Result := 'HELP';
    VK_0..VK_9, VK_A..VK_Z: Result := chr(index);
    VK_LWIN: Result := {$ifdef Darwin}'L-CMD';{$else}'L-WIN';{$endif}
    VK_RWIN: Result := {$ifdef Darwin}'R-CMD';{$else}'R-WIN';{$endif}
    VK_APPS: Result := 'APPS';
    VK_SLEEP: Result := 'SLEEP';
    VK_NUMPAD0..VK_NUMPAD9: Result := 'NUMPAD'+intToStr(index-VK_NUMPAD0);
    VK_MULTIPLY: Result := 'MULTIPLY';
    VK_ADD: Result := 'ADD';
    VK_SEPARATOR: Result := 'SEPARATOR';
    VK_SUBTRACT: Result := 'SUBTRACT';
    VK_DECIMAL: Result := 'DECIMAL';
    VK_DIVIDE: Result := 'DIVIDE';
    VK_F1..VK_F24: Result := 'F'+intToStr(index-VK_F1+1);
    VK_NUMLOCK: Result := 'NUMLOCK';
    VK_SCROLL: Result := 'SCROLL';
    VK_LSHIFT: Result := 'L-SHIFT';
    VK_RSHIFT: Result := 'R-SHIFT';
    VK_LCONTROL: Result := 'L-CONTROL';
    VK_RCONTROL: Result := 'R-CONTROL';
    VK_LMENU: Result := {$ifdef Darwin}'L-OPTION';{$else}'L-ALT';{$endif}
    VK_RMENU: Result := {$ifdef Darwin}'R-OPTION';{$else}'R-ALT';{$endif}
    VK_BROWSER_BACK: Result := 'BROWSER_BACK';
    VK_BROWSER_FORWARD: Result := 'BROWSER_FORWARD';
    VK_BROWSER_REFRESH: Result := 'BROWSER_REFRESH';
    VK_BROWSER_STOP: Result := 'BROWSER_STOP';
    VK_BROWSER_SEARCH: Result := 'BROWSER_SEARCH';
    VK_BROWSER_FAVORITES: Result := 'BROWSER_FAVORITES';
    VK_BROWSER_HOME: Result := 'BROWSER_HOME';
    VK_VOLUME_MUTE: Result := 'VOLUME_MUTE';
    VK_VOLUME_DOWN: Result := 'VOLUME_DOWN';
    VK_VOLUME_UP: Result := 'VOLUME_UP';
    VK_MEDIA_NEXT_TRACK: Result := 'MEDIA_NEXT_TRACK';
    VK_MEDIA_PREV_TRACK: Result := 'MEDIA_PREV_TRACK';
    VK_MEDIA_STOP: Result := 'MEDIA_STOP';
    VK_MEDIA_PLAY_PAUSE: Result := 'MEDIA_PLAY_PAUSE';
    VK_LAUNCH_MAIL: Result := 'LAUNCH_MAIL';
    VK_LAUNCH_MEDIA_SELECT: Result := 'LAUNCH_MEDIA_SELECT';
    VK_LAUNCH_APP1: Result := 'LAUNCH_APP1';
    VK_LAUNCH_APP2: Result := 'LAUNCH_APP2';
    VK_OEM_1: Result := 'OEM_1';
    VK_OEM_PLUS: Result := 'OEM_PLUS';
    VK_OEM_COMMA: Result := 'OEM_COMMA';
    VK_OEM_MINUS: Result := 'OEM_MINUS';
    VK_OEM_PERIOD: Result := 'OEM_PERIOD';
    VK_OEM_2: Result := 'OEM_2';
    VK_OEM_3: Result := 'OEM_3';
    VK_OEM_4: Result := 'OEM_4';
    VK_OEM_5: Result := 'OEM_5';
    VK_OEM_6: Result := 'OEM_6';
    VK_OEM_7: Result := 'OEM_7';
    VK_OEM_8: Result := 'OEM_8';
    VK_OEM_102: Result := 'OEM_102';
    VK_PROCESSKEY: Result := 'PROCESSKEY';
    VK_ATTN: Result := 'ATTN';
    VK_CRSEL: Result := 'CRSEL';
    VK_EXSEL: Result := 'EXSEL';
    VK_EREOF: Result := 'EREOF';
    VK_PLAY: Result := 'PLAY';
    VK_ZOOM: Result := 'ZOOM';
    VK_NONAME: Result := 'NONAME';
    VK_PA1: Result := 'PA1';
    VK_OEM_CLEAR: Result := 'OEM_CLEAR';
    else Result := 'unknow';
  end;
end;

function TOGLCContext.GetLastUTF8CharEntered: string;
begin
  Result := FLastUTF8CharEntered;
  FLastUTF8CharEntered := '';
end;

function TOGLCContext.GetUserPressAKey: boolean;
var i: integer;
begin
  Result := False;
  for i:=0 to High(FKeyState) do
    if FKeyState[i] then exit(True);
end;

procedure TOGLCContext.ProcessOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseManager.ProcessOnMouseDown(Button, Shift, X, Y);
  if FPreviousOnMouseDown <> NIL then FPreviousOnMouseDown(FOGLC, Button, Shift, X, Y);
end;

procedure TOGLCContext.ProcessOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseManager.ProcessOnMouseUp(Button, Shift, X, Y);
  if FPreviousOnMouseUp <> NIL then FPreviousOnMouseUp(FOGLC, Button, Shift, X, Y);
end;

procedure TOGLCContext.ProcessOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouseManager.ProcessOnMouseMove(Shift, X, Y);
  if FPreviousOnMouseMove <> NIL then FPreviousOnMouseMove(FOGLC, Shift, X, Y);
end;

procedure TOGLCContext.ProcessOnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FMouseManager.ProcessOnMouseWheel(Shift, WheelDelta, MousePos, Handled);
  if FPreviousOnMouseWheel <> NIL then FPreviousOnMouseWheel(FOGLC, Shift, WheelDelta, MousePos, Handled);
end;

function TOGLCContext.GetKeyState(index: byte): boolean;
begin
  Result := FKeyState[index];
end;

function TOGLCContext.GetKeyPressed(index: byte): boolean;
begin
  Result := FKeyPressed[index];
  FKeyPressed[index] := FALSE;
end;

function TOGLCContext.GetViewPortHeight: integer;
begin
  Result := FOGLC.Height;
end;

function TOGLCContext.GetViewPortWidth: integer;
begin
  Result := FOGLC.Width;
end;

function TOGLCContext.GetViewPortTopLeft: TPointF;
begin
  Result := PointF(FOGLC.Left, FOGLC.Top);
end;

function TOGLCContext.GetViewPortCenter: TPointF;
begin
  Result := PointF(FOGLC.Width*0.5, FOGLC.Height*0.5);
end;

procedure TOGLCContext.UpdateViewPortSize;
var p: TWinControl;
    xx, yy, w, h: integer;
begin
  p := FOGLC.Parent;
  if FAspectRatio = -1 then begin
    w := p.ClientWidth;
    h := p.ClientHeight;
  end else begin
    if p.ClientWidth > p.ClientHeight then begin
      w := Round(p.ClientHeight*FAspectRatio);
      if w > p.ClientWidth then w := p.ClientWidth;
      h := Trunc(w/FAspectRatio);
    end else begin
     h := Round(p.ClientWidth/FAspectRatio);
     if h > p.ClientHeight then h := p.ClientHeight;
     w := Trunc(h*FAspectRatio);
    end;
  end;
  xx := (p.ClientWidth-w) div 2;
  yy := (p.ClientHeight-h) div 2;
  FOGLC.SetBounds(xx, yy, w, h);
end;

procedure TOGLCContext.SetDesignPPI(AValue: integer);
begin
  if AValue <= 0 then exit;
  FDesignPPI := AValue;
  FPPIScaleFactor := Screen.PixelsPerInch/AValue;  // from FOGLC.Scale96ToScreen();
end;

procedure TOGLCContext.SetSystemMouseCursorVisible(AValue: boolean);
begin
  if FOGLC = NIL then exit;
  case AValue of
    True: FOGLC.Cursor := crDefault;
    False: FOGLC.Cursor := crNone;
  end;
end;

constructor TOGLCContext.Create(aOGLContext: TOpenGLControl; aAspectRatio: single);
begin
  inherited Create;
  FGLInitialized := FALSE;
  FOGLC := aOGLContext;
  FAspectRatio := aAspectRatio;

  FPreviousOnResize := FOGLC.OnResize;
  FPreviousOnMouseDown := FOGLC.OnMouseDown;
  FPreviousOnMouseUp := FOGLC.OnMouseUp;
  FPreviousOnMouseMove := FOGLC.OnMouseMove;
  FPreviousOnMouseWheel := FOGLC.OnMouseWheel;

  // enable stencil 8bits for UI clipping
  FOGLC.StencilBits := 8;
  FOGLC.OpenGLMajorVersion := 3;
  FOGLC.OpenGLMinorVersion := 3;

  FOGLC.OnResize := @ProcessOnResize;
  FOGLC.OnMouseDown := @ProcessOnMouseDown;
  FOGLC.OnMouseUp := @ProcessOnMouseUp;
  FOGLC.OnMouseMove := @ProcessOnMouseMove;
  FOGLC.OnMouseWheel := @ProcessOnMouseWheel;

  ClearKeysState;
  DesignPPI := DEFAULT_DESIGN_PPI;

  // load OpenGL
  FOGLC.Visible := False;
  if FOGLC.MakeCurrent
    then FOpenGLLibLoaded := Load_GL_version_3_3_CORE
    else FOpenGLLibLoaded := False;
end;

destructor TOGLCContext.Destroy;
begin
  FTextureManager.Free;
  FTextureManager := NIL;
  FMouseManager.Free;
  FMouseManager := NIL;
  inherited Destroy;
end;

procedure TOGLCContext.ChangeAspectRatioTo(aAspectRatio: single);
begin
  FAspectRatio := aAspectRatio;
  UpdateViewPortSize;
end;

function TOGLCContext.MakeContextCurrent: boolean;
begin
  Result := FOGLC.MakeCurrent(False);
end;

procedure TOGLCContext.ProcessOnKeyDown(var Key: Word; Shift: TShiftState);
var key2: Word;
begin
  if Key <= $FF then begin
    key2 := 0;
    if Key = VK_SHIFT then begin
      if lclintf.GetKeyState(VK_LSHIFT) < 0 then key2 := VK_LSHIFT
      else
      if lclintf.GetKeyState(VK_RSHIFT) < 0 then key2 := VK_RSHIFT;
    end
    else if Key = VK_CONTROL then begin
      if lclintf.GetKeyState(VK_LCONTROL) < 0 then key2 := VK_LCONTROL
      else
      if lclintf.GetKeyState(VK_RCONTROL) < 0 then key2 := VK_RCONTROL;
    end;
    if key2 <> 0 then begin
      FKeyState[byte(Key2)] := TRUE;
      FLastKeyDown := byte(Key2);
    end else FLastKeyDown := byte(Key);

    FKeyState[byte(Key)] := TRUE;
  end;
end;

procedure TOGLCContext.ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
var key2: Word;
begin
  if Key <= $FF then begin
    key2 := 0;
    if Key = VK_SHIFT then begin
      if FKeyState[VK_LSHIFT] and (lclintf.GetKeyState(VK_LSHIFT) >= 0) then key2 := VK_LSHIFT
      else
      if FKeyState[VK_RSHIFT] and (lclintf.GetKeyState(VK_RSHIFT) >= 0) then key2 := VK_RSHIFT;
    end
    else if Key = VK_CONTROL then begin
      if FKeyState[VK_LCONTROL] and (lclintf.GetKeyState(VK_LCONTROL) >= 0) then key2 := VK_LCONTROL
      else
      if FKeyState[VK_RCONTROL] and (lclintf.GetKeyState(VK_RCONTROL) >= 0) then key2 := VK_RCONTROL;
    end;
    if key2 <> 0 then begin
      if FKeyState[byte(key2)] then
        FKeyPressed[byte(key2)] := TRUE;
      FKeyState[byte(key2)] := FALSE;
    end;

    if FKeyState[byte(Key)] then
      FKeyPressed[byte(Key)] := TRUE;

    FKeyState[byte(Key)] := FALSE;
  end;
end;

procedure TOGLCContext.ProcessOnUTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  FLastUTF8CharEntered := UTF8Key;
  UTF8Key := '';
end;

procedure TOGLCContext.ClearKeysState;
begin
  FillChar(FKeyState, sizeof(FKeyState), FALSE);
  FillChar(FKeyPressed, sizeof(FKeyPressed), FALSE);
end;

function TOGLCContext.ScaleDesignToScene(ASize: integer): integer;
begin
  Result := Round(ASize*FPPIScaleFactor);
end;

function TOGLCContext.ScaleDesignToSceneF(ASize: single): single;
begin
  Result := ASize*FPPIScaleFactor;
end;

{$define oglcIMPLEMENTATION}
{$I oglcLogFile.inc}
{$I oglcVelocityCurve.inc}
{$I oglcCharset.inc}
{$I oglcMath.inc}
{$I gl_core_matrix.inc}
{$I oglcType.inc}
{$I oglcAppSaveUtils.inc}
{$I oglcShader.inc}
{$I oglcMessage.inc}
{$I oglcTexture.inc}
{$I oglcTextureAtlas.inc}
{$I oglcTimer.inc}
{$I oglcLayer.inc}
{$I oglcCollisionBody.inc}
{$I oglcSurface.inc}
{$I oglcPath.inc}
{$I oglcCamera.inc}
{$I oglcMultiRendererFactory.inc}
{$I oglcPostProcessing.inc}
{$I oglcRendererToTexture.inc}
{$I oglcSurfaceWithProceduralShader.inc}
{$I oglcBorderAndFill.inc}
{$I oglcSpriteTemplate.inc}
{$I oglcDecorManager.inc}
{$I oglcScreenTemplate.inc}
{$I oglcFXExplodeTexture.inc}
{$I oglcFXScrolledSprite.inc}
{$I oglcElectricalFX.inc}
{$I oglcGlow.inc}
{$I oglcDeformationGrid.inc}
{$I oglcSlideShow.inc}
{$I oglcUIClipping.inc}
{$I oglcTileEngine.inc}
{$I oglcParticle.inc}
{$I oglcUtils.inc}
{$I oglcUITheme.inc}
{$I oglcUI.inc}
{$I oglcUIScrollable.inc}
{$I oglcUIPanelWithEffects.inc}
{$I oglcUIModalPanel.inc}
{$I oglcTexturedFont.inc}
{$I oglcAlignText.inc}
{$I oglcFreeText.inc}
{$I oglcReusableSurfaceContainer.inc}
{$I oglcEnvironment.inc}
{$I oglcGpuInfo.inc}
{$undef oglcIMPLEMENTATION}

{ TOGLCScene }

constructor TOGLCScene.Create(aOGLContext: TOpenGLControl; aAspectRatio: single);
begin
  if aAspectRatio <> -1 then begin
    aOGLContext.Align := alNone;
    if aAspectRatio < 1.0 then aAspectRatio := 4/3;
  end;
  inherited Create(aOGLContext, aAspectRatio);

  FBackgroundColorF.Init(0,0,0,1);

  CreateVelocityCurveList;

  FOnBeforePaint := NIL;
  FOnAfterPaint := NIL;
  FOnLoadCommonData := NIL;
  FOnFreeCommonData := NIL;
  FCommonDataLoaded := FALSE;

  FGlobalFadeColor:= TBGRAParam.Create;
  FGlobalFadeColor.Value := BGRA(0,0,0,0);
  FScreenFadeTime:= 0.5;

  FTextureManager := TTextureManager.Create(Self);

  FTimerManager := TTimerManager.Create;
  FTimerManager.Add( @CallBackTimerFPS, 1000 );

  FMouseManager := TMouseManager.Create(Self);

  FStencilClipping.InitDefault;

  FExecuteDuringLoop := FALSE;

  FCurrentScreen := NIL;
  FScreenRequested := NIL;

  FCameraList := TList.Create;

  FUpdatePeriod := 16;

  FWaitLoopCountBeforeRunning := 5;

  FModalPanelList := TModalPanelList.Create;

  FCurrentBlendMode := $FF;

  PostProcessing.FParentScene := Self;
end;

destructor TOGLCScene.Destroy;
var i: Integer;
begin
  if FCurrentScreen <> NIL then begin
    FCurrentScreen.FreeObjects;
    if FCurrentScreen.FreeWhenLeave then FCurrentScreen.Free;
    FCurrentScreen := NIL;
  end;

  if (FOnFreeCommonData <> NIL) and FOpenGLLibLoaded then FOnFreeCommonData;
  FCommonDataLoaded := FALSE;

  FPostProcessingEngine.Free;
  FPostProcessingEngine := NIL;

  for i:=0 to FCameraList.Count-1 do
   TOGLCCamera(FCameraList.Items[0]).Free;
  FCameraList.Free;
  FCameraList := NIL;

  FGlobalFadeColor.Free;
  FGlobalFadeColor := NIL;

  SetLayerCount(0);
  FTimerManager.Free;
  FTimerManager := NIL;
  FModalPanelList.Free;  // FreeAndNil(FModalPanelList) NOT WORKING HERE
  FModalPanelList := NIL;
  FTextureManager.Free;
  FTextureManager := NIL;
  DestroyRenderers;

  FLoadingMessageSprite.Free;
  FLoadingMessageSprite := NIL;

  FreeVelocityCurveList;

  if FLog <> NIL then begin
    FLog.Add('Peace');
    FLog.Free;
    FLog := NIL;
  end;

  inherited Destroy;
end;

procedure TOGLCScene.CreateLogFile(const aFilename: string; aDeletePrevious: boolean;
  aCallback: TOGLCLogCallback; aCallbackData: pointer);
begin
  FLog := TLog.Create(aFilename, aCallback, aCallbackData);
  if aDeletePrevious then FLog.DeleteLogFile;
  if FOpenGLLibLoaded then begin
    FLog.Mess('Renderer: ' + Gpu.RendererName + '    version '+ Gpu.Version);
    FLog.Mess('Total video ram: ' + Gpu.TotalVideoRamKb.ToString + ' Kb' +
              '    Free video ram: ' + Gpu.FreeVideoRamKb.ToString + ' Kb');
    FLog.Mess('Start application "'+ApplicationName+'" on platform '+App.OSName, 0, True);
    FLog.AddEmptyLine;
  end else begin
    FLog.Error('OpenGL library is not loaded !');
  end;
end;

procedure TOGLCScene.LogEmptyLine(aSeparator: string);
begin
 if FLog <> NIL then FLog.AddEmptyLine(aSeparator);
end;

procedure TOGLCScene.LogMess(const aMsg: string; aMarginCount: integer; aShowTime: boolean);
begin
  if FLog <> NIL then FLog.Mess(aMsg, aMarginCount, aShowTime);
end;

procedure TOGLCScene.LogInfo(const aMsg: string; aMarginCount: integer; aShowTime: boolean);
begin
  if FLog <> NIL then FLog.Info(aMsg, aMarginCount, aShowTime);
end;

procedure TOGLCScene.LogWarning(const aMsg: string; aMarginCount: integer; aShowTime: boolean);
begin
  if FLog <> NIL then FLog.Warning(aMsg, aMarginCount, aShowTime);
end;

procedure TOGLCScene.LogError(const aMsg: string; aMarginCount: integer; aShowTime: boolean);
begin
  if FLog <> NIL then FLog.Error(aMsg, aMarginCount, aShowTime);
end;

procedure TOGLCScene.LogDebug(const aMsg: string; aMarginCount: integer; aShowTime: boolean);
begin
  if FLog <> NIL then FLog.Debug(aMsg, aMarginCount, aShowTime);
end;

procedure TOGLCScene.LogStartMeasuringTime(const aMsg: string; aMarginCount: integer);
begin
  if FLog <> NIL then FLog.StartMeasuringTime(aMsg, aMarginCount);
end;

procedure TOGLCScene.LogStopMeasuringTime(const aMsg: string; aMarginCount: integer);
begin
  if FLog <> NIL then FLog.StopMeasuringTime(aMsg, aMarginCount);
end;

procedure TOGLCScene.DoLoop;
var tick, delta: QWord;
    ElapsedTime: single;
begin
  if not FOpenGLLibLoaded then exit;
  if not MakeCurrent then exit;

  if not FGLInitialized then begin
    FOGLC.Visible := True;
 {   FOpenGLLibLoaded := Load_GL_version_3_3_CORE;
    if not FOpenGLLibLoaded
      then raise Exception.Create('Cannot load OpenGL 3.3 core...')
      {$ifdef USE_glcorearb};{$else}else Load_GL_EXT_blend_func_separate;{$endif}   }
    SetBlendMode(FX_BLEND_NORMAL);
    //glEnable(GL_POLYGON_SMOOTH);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(PRIMITIVE_INDEX_RESTART_VALUE);

    glGetError();

    UpdateViewPortSize;
    CreateRenderers;

    FGLInitialized := TRUE;

    FPostProcessingEngine := TOGLCPostProcessingEngine.Create(Self);

    FTickOriginUpdate := GetTickCount64;
    FTickAccu := 0;

    glClearColor(FBackgroundColorF.r, FBackgroundColorF.g, FBackgroundColorF.b, FBackgroundColorF.a);
    glClear(GL_COLOR_BUFFER_BIT);
    FOGLC.SwapBuffers;
    glGetError();
  end;

  if FNeedToResizeViewPort then begin
    UpdateViewPortSize;
    FNeedToResizeViewPort := FALSE;
  end;

  // wait a little before loading common data and run a screen
  // because under some Linux system, it take some time to finalize the main window size.
  if FWaitLoopCountBeforeRunning > 0 then begin
    dec(FWaitLoopCountBeforeRunning);
    if FWaitLoopCountBeforeRunning = 0 then UpdateViewPortSize
      else exit;
  end;

  if not FCommonDataLoaded and (FOnLoadCommonData <> NIL) then begin
    FOnLoadCommonData;
    FCommonDataLoaded := TRUE;
  end;

  FTimerManager.ProcessTimer;

  tick := GetTickCount64;
  delta := tick - FTickOriginUpdate;

  if FUpdatePeriod < 1 then begin
    ElapsedTime := delta * 0.001;
    UpDate(ElapsedTime);
  end else begin
    FTickAccu := FTickAccu + delta;
    ElapsedTime := FUpdatePeriod*0.001;
    if FTickAccu > FUpdatePeriod then begin
      UpDate(ElapsedTime);
      FTickAccu := FTickAccu - FUpdatePeriod;
    end;
  end;
  FTickOriginUpdate := tick;


  Draw;
  glFlush;
  glFinish;
  FOGLC.SwapBuffers;
  inc(FFPSCounter);

 // if needed, start the next screen
 if FScreenRequested <> NIL then begin
   Mouse.MousePoolEnabled := False; // disable mouse pooling

   if FFadeBlackColorOnNewScreen and
     (FGlobalFadeColor.State = psNO_CHANGE) and
     (FGlobalFadeColor.Value <> BGRABlack)
     then ColorFadeIn(BGRABlack, FScreenFadeTime);

   if FGlobalFadeColor.State = psNO_CHANGE then begin
     // empty modal panel list
     FModalPanelList.FreeAll;

     if FCurrentScreen <> NIL then begin
       FCurrentScreen.FreeObjects;
       if FCurrentScreen.FreeWhenLeave
         then FCurrentScreen.Free
         else FCurrentScreen.ClearMessageList;
     end;

     // show loading message sprite
     FLoadingMessageSprite := FScreenRequested.GetLoadingMessageSprite;
     if FLoadingMessageSprite <> NIL then begin
       FLoadingMessageSprite.ParentScene := Self;
       FLoadingMessageSprite.CenterOnScene;
       Draw;
       glFlush;
       glFinish;
       FOGLC.SwapBuffers;
     end;

     FCurrentScreen := FScreenRequested;
     FScreenRequested := NIL;
     FCurrentScreen.ClearMessageList;
     FCurrentScreen.CreateObjects;
     ClearKeysState;
     ColorFadeOut(FScreenFadeTime);
     Mouse.MousePoolEnabled := True;

     FLoadingMessageSprite.Free;
     FLoadingMessageSprite := NIL;
   end;
 end;
end;

procedure TOGLCScene.ExecuteDuring(aTimeInSecond: single);
var torig: QWord;
begin
 if FExecuteDuringLoop then exit;
 torig := GetTickCount64;
 FExecuteDuringLoop := TRUE;
 repeat
  DoLoop;
  Application.ProcessMessages;
 until ( GetTickCount64 - torig ) * 0.001 >= aTimeInSecond ;
 FExecuteDuringLoop := FALSE;
end;

procedure TOGLCScene.RunScreen(aScreen: TScreenTemplate; aFadeWithBlackColor: boolean);
begin
 FScreenRequested := aScreen;
 FFadeBlackColorOnNewScreen := aFadeWithBlackColor;
end;

procedure TOGLCScene.CreateRenderers;
begin
  FTexturedMVTriangleRenderer := TOGLCTexturedMVTriangleRenderer.Create(Self, True);
end;

procedure TOGLCScene.DestroyRenderers;
begin
  FreeAndNil(FFastLineRenderer);
  FreeAndNil(FSmoothLineRenderer);
  FreeAndNil(FThreeColorTriangleRenderer);
  FreeAndNil(FThreeColorMVTriangleRenderer);
  FreeAndNil(FTexturedMVTriangleRenderer);
  FreeAndNil(FTexturedTriangleRenderer);
  FreeAndNil(FNoFilterTextureMVTriangleRenderer);
  FreeAndNil(FTexturedTriangleRendererWithFX);
  FreeAndNil(FScrollableTextureRenderer);

  FreeAndNil(FTileRenderer);
  FreeAndNil(FGlowRenderer);
  FreeAndNil(FParticleRenderer);
  FreeAndNil(FElectricalBeamRenderer);
end;

procedure TOGLCScene.UpdateViewPortSize;
var err: glEnum;
begin
  inherited UpdateViewPortSize;
  ProjectionMatrix.Ortho(0, Width, Height, 0, 0.0, 1.0);
  //ProjectionMatrix.Ortho(FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
  ModelViewMatrix.LoadIdentity;
{
 // Matrix for legacy OpenGL
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 glOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity();  }

  MakeContextCurrent;
  glViewport(0, 0, Width, Height);
  err := glGetError();
  if err <> GL_NO_ERROR then
    LogError('TOGLCScene.UpdateViewPortSize: GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));
end;

{procedure TOGLCScene.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 case Button of
  mbLeft: MouseManager.LeftButton:=TRUE;
  mbRight: MouseManager.RightButton:=TRUE;
  mbMiddle: MouseManager.MiddleButton:=TRUE;
 end;
 MouseManager.FMousePos.x:=X;
 MouseManager.FMousePos.y:=Y;
end;

procedure TOGLCScene.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 case Button of
  mbLeft: MouseManager.LeftButton:=FALSE;
  mbRight: MouseManager.RightButton:=FALSE;
  mbMiddle: MouseManager.MiddleButton:=FALSE;
 end;
 MouseManager.FMousePos.x:=X;
 MouseManager.FMousePos.y:=Y;
end; }

procedure TOGLCScene.Add( aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
 AddSurfaceToLayer(aSurface, aLayerIndex );
 aSurface.SetParentLayer( Layer[aLayerIndex] );
 aSurface.SetParentScene( Self );
end;

procedure TOGLCScene.Insert( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
 InsertSurfaceToLayer( aSurfaceIndex, aSurface, aLayerIndex);
 aSurface.SetParentLayer( Layer[aLayerIndex] );
 aSurface.SetParentScene( Self );
end;

procedure TOGLCScene.RemoveSurfaceFromLayer(aSurface: TSimpleSurfaceWithEffect;
  aLayerIndex: integer);
begin
 Layer[aLayerIndex].Remove(aSurface);
end;

procedure TOGLCScene.RemoveSurfaceFromItsLayer(aSurface: TSimpleSurfaceWithEffect);
begin
  if aSurface.ParentLayer = NIL then exit;
  aSurface.ParentLayer.Remove(aSurface);
end;

procedure TOGLCScene.MoveSurfaceToLayer(aSurface: TSimpleSurfaceWithEffect; aNewLayerIndex: integer);
var i: integer;
begin
  if (aNewLayerIndex < 0) or (aNewLayerIndex >= LayerCount) or (aSurface = NIL) then exit;
  if aSurface.FParentLayer = Layer[aNewLayerIndex] then exit;

  if aSurface.FParentLayer = NIL then begin
    Add(aSurface, aNewLayerIndex);
    exit;
  end;

  // push the 'ChangeLayer' request in list. it will be processed in the next scene update
  i := Length(FListSurfaceChangeLayer);
  SetLength(FListSurfaceChangeLayer, i+1);
  FListSurfaceChangeLayer[i].Surface := aSurface;
  FListSurfaceChangeLayer[i].NewLayerIndex := aNewLayerIndex;
end;

function TOGLCScene.GetSurfaceByIndex(aLayerIndex, aSurfaceIndex: integer ): TSimpleSurface;
begin
 Result := Layer[aLayerIndex].Surface[aSurfaceIndex];
end;

procedure TOGLCScene.ColorFadeIn(const aColor: TBGRAPixel; const aDurationInSecond: single);
begin
 if aDurationInSecond = 0
   then FGlobalFadeColor.Value := aColor
   else FGlobalFadeColor.ChangeTo(aColor, aDurationInSecond);
end;

procedure TOGLCScene.ColorFadeOut(const aDurationInSecond: single);
begin
 if aDurationInSecond=0
   then FGlobalFadeColor.Alpha.Value := 0
   else FGlobalFadeColor.Alpha.ChangeTo(0, aDurationInSecond);
end;

function TOGLCScene.MVPMatrix: TOGLCMatrix;
begin
  Result.Matrix := MultMat(ModelViewMatrix.Matrix, ProjectionMatrix.Matrix);
end;

procedure TOGLCScene.FlushRenderer;
begin
  FTexturedMVTriangleRenderer.Batch_Flush;
end;

function TOGLCScene.CreateAtlas: TOGLCTextureAtlas;
begin
  Result := TOGLCTextureAtlas.Create(Self);
end;

function TOGLCScene.CreateTexturedFont(aFont: TFontDescriptor; const aCharSet: string;
  aFillTexture: TBGRABitmap): TTexturedFont;
begin
  Result := TTexturedFont.Create(Self, aFont, aCharSet, aFillTexture);
end;

function TOGLCScene.CreateCamera: TOGLCCamera;
begin
  Result := TOGLCCamera.Create;
  Result.FParentScene := self;
  Result.AutoFollow.Init(Self, Result);
  FCameraList.Add(Result);
end;

function TOGLCScene.CreateTileEngine(aLayer: integer): TTileEngine;
begin
  Result := TTileEngine.Create(Self);
  Add(Result, aLayer);
end;

function TOGLCScene.CreateSprite(aTexture: PTexture; aTextureOwner: boolean; aLayer: integer): TSprite;
begin
  Result := TSprite.Create(aTexture, aTextureOwner);
  Add(Result, aLayer);
end;

function TOGLCScene.AddSprite(aTexture: PTexture; aTextureOwner: boolean; aLayer: integer): TSprite;
begin
  Result := TSprite.Create(aTexture, aTextureOwner);
  Add(Result, aLayer);
end;

function TOGLCScene.AddSprite(aTexture: PTexture; aTextureOwner: boolean;
  aLayerIndex: integer; aX, aY: single): TSprite;
begin
  Result := AddSprite(aTexture, aTextureOwner, aLayerIndex);
  Result.SetCoordinate(aX, aY);
end;

function TOGLCScene.Add_FreeText(const aCaption: string; aFont: TTexturedFont; aLayer: integer): TFreeText;
begin
  Result := TFreeText.Create(Self);
  Result.TexturedFont := aFont;
  Result.Caption := aCaption;
  Add(Result, aLayer);
end;

procedure TOGLCScene.KillCamera(var aCamera: TOGLCCamera);
begin
  if aCamera = NIL then exit;
  aCamera.Unassign;
  FCameraList.Remove(aCamera);
  aCamera.Free;
  aCamera := NIL;
end;

function TOGLCScene.Add_UILabel(const aCaption: string; aFont: TTexturedFont;
  aLayer: integer): TUILabel;
begin
  Result := TUILabel.Create(Self);
  Result.Caption := aCaption;
  Result.TexturedFont := aFont;
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIButton(const aCaption: string; aFont: TTexturedFont; aTexture: PTexture; aLayer: integer): TUIButton;
begin
  Result := TUIButton.Create(Self, aCaption, aFont, aTexture);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UICheck(const aCaption: string; aFont: TTexturedFont;aLayer: integer): TUICheck;
begin
  Result := TUICheck.Create(Self, aCaption, aFont);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIRadio(const aCaption: string; aFont: TTexturedFont; aLayer: integer): TUIRadio;
begin
  Result := TUIRadio.Create(Self, aCaption, aFont);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIPanel(aLayer: integer): TUIPanel;
begin
  Result := TUIPanel.Create(Self);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIProgressBar(aOrientation: TUIOrientation; aLayer: integer): TUIProgressBar;
begin
  Result := TUIProgressBar.Create(Self, aOrientation);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIScrollBar(aOrientation: TUIOrientation; aLayer: integer): TUIScrollBar;
begin
  Result := TUIScrollBar.Create(Self, aOrientation);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIListBox(aFont: TTexturedFont; aLayer: integer): TUIListBox;
begin
  Result := TUIListBox.Create(Self, aFont);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UITextArea(aLayer: integer): TUITextArea;
begin
  Result := TUITextArea.Create(Self);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_UIScrollBox(aUseVScrollBar, aUseHScrollBar: boolean; aLayer: integer): TUIScrollBox;
begin
  Result := TUIScrollBox.Create(Self, aUseVScrollBar, aUseHScrollBar);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_ModalPanel: TUIModalPanel;
begin
  Result := TUIModalPanel.Create(Self);
end;

procedure TOGLCScene.Draw;
var L, i: integer;
    o: TSimpleSurfaceWithEffect;
    err: glEnum;
begin
  // set background color
  glClearColor(FBackgroundColorF.r, FBackgroundColorF.g, FBackgroundColorF.b, FBackgroundColorF.a);
  glClear(GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // delete all surface with FKill=true
  for L:=0 to LayerCount-1 do
    for i:=Layer[L].FList.Count-1 downto 0 do begin
      o := Layer[L].Surface[i];
      if o.FKill then begin
        Layer[L].Delete(i);
        o.Free;
      end;
    end;

  // Before paint CallBack
  if Assigned(FOnBeforePaint) then FOnBeforePaint;

  // Render all Layers
  for L:=LayerCount-1 downto 0 do
    Layer[L].Draw;
  err := glGetError();
  if err <> GL_NO_ERROR then
    LogError('TOGLCScene.Draw: Render LAYERS give GL ERROR $'+IntToHex(err, 4)+' '+GLErrorToString(err));

  // render an eventual remains in the batch renderer process
  FTexturedMVTriangleRenderer.Batch_Flush;
  // flush the post processing engine
  FPostProcessingEngine.Flush;

  // After paint CallBack
  if Assigned (FOnAfterPaint) then FOnAfterPaint;

  // Render Modal panels
  if FModalPanelList.OneModalIsVisible then begin
    ModelViewMatrix.LoadIdentity;
    FModalPanelList.Draw;
  end;

  // Render mouse cursor
  ModelViewMatrix.LoadIdentity;
  Mouse.Draw;

  // Scene global fade
  if FGlobalFadeColor.Alpha.Value > 0
    then FillBox(Self, 0, 0, GetViewPortWidth, GetViewPortHeight, FGlobalFadeColor.Value);

  // Loading message sprite
  if FLoadingMessageSprite <> NIL then
    FLoadingMessageSprite.Draw(1.0);

  // render an eventual remains in the batch renderer process
  FTexturedMVTriangleRenderer.Batch_Flush;
end;

procedure TOGLCScene.UpDate(const aElapsedTime: single);
var i: integer;
begin
  // Process 'Surface change layer' request
  for i:=0 to length(FListSurfaceChangeLayer)-1 do
   with FListSurfaceChangeLayer[i] do begin
     Surface.FParentLayer.Remove(Surface);
     Layer[NewLayerIndex].Add(Surface);
     Surface.FParentLayer := Layer[NewLayerIndex];
   end;
  FListSurfaceChangeLayer := NIL;

  Mouse.PrepareBeforeUpdate;

  FModalPanelList.FreeKilledPanels;
  // update Modal Panel or layer
  if FModalPanelList.OneModalIsVisible then FModalPanelList.Update(aElapsedTime)
  else begin
    // update camera objects
    for i:=0 to FCameraList.Count-1 do
      TOGLCCamera(FCameraList.Items[i]).Update(aElapsedTime);
    // update all layers
    for i:=LayerCount-1 downto 0 do Layer[i].Update(aElapsedTime);
    // update current screen
    if FCurrentScreen <> NIL then FCurrentScreen.Update(aElapsedTime);
  end;

  Mouse.UpDate(aElapsedTime);

  // Scene global fade
  FGlobalFadeColor.OnElapse(aElapsedTime);
end;

procedure TOGLCScene.CallBackTimerFPS;
begin
 FFPS := FFPSCounter;
 FFPSCounter := 0;
end;

procedure TOGLCScene.SetBackgroundColor(aColor: TBGRAPixel);
begin
 FBackgroundColorF.InitFromBGRA(aColor);
end;

function TOGLCScene.GetRectArea: TRect;
begin
 Result.Create( Point(0,0), Width,  Height );
end;

function TOGLCScene.MakeCurrent: boolean;
begin
  Result := MakeContextCurrent;
end;

procedure TOGLCScene.SetLayerCount(AValue: integer);
var i: integer ;
begin
 inherited SetLayerCount(AValue);
 for i:=0 to GetLayerCount-1 do
  begin
   Layer[i].FParentScene := Self;
  end;
end;

function TOGLCScene.GetBackgroundColor: TBGRAPixel;
begin
 Result := FBackgroundColorF.ToBGRA;
end;

procedure TOGLCScene.SetBlendMode(AValue: byte);
var src : LongWord;
    dst : LongWord;
    procedure EnableglBlend; inline;
    begin
     if not FBlendIsEnabled
       then glEnable(GL_BLEND);
     FBlendIsEnabled := True;
    end;
    procedure DisableglBlend; inline;
    begin
     if FBlendIsEnabled
       then glDisable(GL_BLEND);
     FBlendIsEnabled := False;
    end;
begin
 if FCurrentBlendMode = AValue then exit;
 case AValue of

   FX_BLEND_ADD: begin
       src := GL_SRC_ALPHA;
       dst := GL_ONE;
       EnableglBlend;
   end;

   FX_BLEND_MULT: begin
       src := GL_ZERO;
       dst := GL_SRC_COLOR;
       EnableglBlend;
   end;

   FX_NOBLEND: DisableglBlend

   else begin  // normal blend
      src := GL_SRC_ALPHA;
      dst := GL_ONE_MINUS_SRC_ALPHA;
      EnableglBlend;
   end;
 end;
 {$ifdef USE_glcorearb}
 glBlendFuncSeparate({%H-}src, {%H-}dst, GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
 {$else}
 {if not FBlendFuncSeparateInitialized then
   FBlendFuncSeparateInitialized := Load_GL_EXT_blend_func_separate;  }
 if Assigned(glBlendFuncSeparate)
   then glBlendFuncSeparate({%H-}src, {%H-}dst, GL_ONE, GL_ONE_MINUS_SRC_ALPHA)
   else glBlendFunc(src, dst);
 {$endif}
 FCurrentBlendMode := AValue;
end;

function TOGLCScene.GetFastLineRenderer: TOGLCFastLineRenderer;
begin
  if FFastLineRenderer = NIL then FFastLineRenderer := TOGLCFastLineRenderer.Create(Self, True);
  Result := FFastLineRenderer;
end;

function TOGLCScene.GetGlowRenderer: TOGLCGlowRenderer;
begin
  if FGlowRenderer = NIL then FGlowRenderer := TOGLCGlowRenderer.Create(Self, True);
  Result := FGlowRenderer;
end;

function TOGLCScene.GetElectricalBeamRenderer: TOGLCElectricalBeamRenderer;
begin
  if FElectricalBeamRenderer = NIL then
    FElectricalBeamRenderer := TOGLCElectricalBeamRenderer.Create(Self, True);
  Result := FElectricalBeamRenderer;
end;

function TOGLCScene.GetParticleRenderer: TOGLCParticleRenderer;
begin
  if FParticleRenderer = NIL then
    FParticleRenderer := TOGLCParticleRenderer.Create(Self, True);
  Result := FParticleRenderer;
end;

function TOGLCScene.GetSmoothLineRenderer: TOGLCSmoothLineRenderer;
begin
  if FSmoothLineRenderer = NIL then FSmoothLineRenderer := TOGLCSmoothLineRenderer.Create(Self, True);
  Result := FSmoothLineRenderer;
end;

function TOGLCScene.GetTexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer;
begin
  if FTexturedMVTriangleRenderer = NIL then FTexturedMVTriangleRenderer := TOGLCTexturedMVTriangleRenderer.Create(Self, True);
  Result := FTexturedMVTriangleRenderer;
end;

function TOGLCScene.GetTexturedTriangleRenderer: TOGLCTexturedTriangleRenderer;
begin
  if FTexturedTriangleRenderer = NIL then FTexturedTriangleRenderer := TOGLCTexturedTriangleRenderer.Create(Self, True);
  Result := FTexturedTriangleRenderer;
end;

function TOGLCScene.GetTexturedTriangleRendererWithFX: TOGLCTexturedTriangleRendererWithFX;
begin
  if FTexturedTriangleRendererWithFX = NIL then FTexturedTriangleRendererWithFX := TOGLCTexturedTriangleRendererWithFX.Create(Self, True);
  Result := FTexturedTriangleRendererWithFX
end;

function TOGLCScene.GetThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer;
begin
  if FThreeColorMVTriangleRenderer = NIL then FThreeColorMVTriangleRenderer := TOGLCThreeColorMVTriangleRenderer.Create(Self, True);
  Result := FThreeColorMVTriangleRenderer;
end;

function TOGLCScene.GetThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer;
begin
  if FThreeColorTriangleRenderer = NIL then FThreeColorTriangleRenderer := TOGLCThreeColorTriangleRenderer.Create(Self, True);
  Result := FThreeColorTriangleRenderer;
end;

function TOGLCScene.GetTileRenderer: TOGLCTileRenderer;
begin
  if FTileRenderer = NIL then FTileRenderer := TOGLCTileRenderer.Create(Self, False);
  Result := FTileRenderer;
end;

function TOGLCScene.GetNoFilterTexturedMVTriangleRenderer: TOGLCNoFilterTextureMVTriangleRenderer;
begin
  if FNoFilterTextureMVTriangleRenderer = NIL then
    FNoFilterTextureMVTriangleRenderer := TOGLCNoFilterTextureMVTriangleRenderer.Create(Self, True);
  Result := FNoFilterTextureMVTriangleRenderer;
end;

function TOGLCScene.GetScrollableTextureRenderer: TOGLCScrollableTextureRenderer;
begin
  if FScrollableTextureRenderer = NIL then
    FScrollableTextureRenderer := TOGLCScrollableTextureRenderer.Create(Self, True);
  Result := FScrollableTextureRenderer;
end;

procedure TOGLCScene.SetUpdatePeriod(AValue: integer);
begin
  FUpdatePeriod := AValue;
end;

end.

