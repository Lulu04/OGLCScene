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

 written by Lulu  2017 - 2024
}

{
  File 'oglcParticle.inc' contains the code for the particle emitter.
  This code is a modified version from ZENGL version 0.3.8 written by Andrey Kemka
}

unit OGLCScene;
{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}


//{$DEFINE DEBUG_MODE_ON}
{$define CHECKGLERROR_ON}  // call 'CheckGLError' to raise an exception if an gl error occurs

interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, Controls, lcltype, LazFileUtils,
  types, {contnrs,}
  OpenGLContext, {glcorearb,} GLExt, GL,
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
TDelayCallBack = procedure( UserValue: word ) of object;

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

     acOpacity              =  'Opacity' ;       // Opacity NewOpacity
     acOpacityChange        =  'OpacityChange' ; // OpacityChange NewOpacity Duration CurveID

     acAnimate              =  'Animate' ; // Animate StartFrameIndex EndFrameIndex FramePerSecond(single)
     acIncFrame             =  'IncFrame'; // IncFrame
     acDecFrame             =  'DecFrame'; // DecFrame
     acSetFrame             =  'SetFrame'; // SetFrame ImageIndex

     acTint                 =  'Tint'    ; // Tint red green blue alpha
     acTintChange           =  'TintChange' ; // TintChange Red Green Blue Alpha Duration CurveID
     acTintRedChange        =  'TintRedChange' ;   // TintRedChange NewRedValue Duration CurveID
     acTintGreenChange      =  'TintGreenChange' ; // TintGreenChange NewGreenValue Duration CurveID
     acTintBlueChange       =  'TintBlueChange' ;  // TintBlueChange NewBlueValue Duration CurveID
     acTintAlphaChange      =  'TintAlphaChange' ; // TintAlphaChange NewAlphaValue Duration CurveID

     acAngle                = 'Angle'    ; // Angle NewAngle
     acRotate               = 'Rotate'   ; // Rotate AnglePerSecond
     acRotateTo             = 'RotateTo' ; // RotateTo Angle Duration CurveID
     acRotationAroundAxis   = 'RotationAroundAxis'    ; // RotationAroundAxis XAxis YAxis AnglePerSecond SelfRotate

     acScale                = 'Scale'          ; // Scale HVValue      // [1]= normal size, [0..1[->reduced, ]1..inf[->enlarged
     acScaleChange          = 'ScaleChange'    ; // ScaleChange HVNewValue Duration CurveID
     acScaleH               = 'ScaleH'         ; // ScaleH HValue      // to set scale value for horizontaly axis
     acScaleHChange         = 'ScaleHChange'   ; // ScaleHChange HNewValue Duration CurveID
     acScaleV               = 'ScaleV'         ; // ScaleV VValue      // to set scale value for verticaly axis
     acScaleVChange         = 'ScaleVChange'   ; // ScaleVChange VNewValue Duration CurveID


     acBlink                = 'Blink'    ; // Blink NumberOfBlink(-1 for infinite) aVisibleTime aInvisibleTime
     acStopBlink            = 'StopBlink'; // StopBlink

     acMoveTo               = 'MoveTo'         ; // MoveTo X Y Duration CurveID
     acMoveXTo              = 'MoveXTo'        ; // MoveXTo X Duration CurveID
     acMoveYTo              = 'MoveYTo'        ; // MoveYTo Y Duration CurveID

     acMoveCenterTo         = 'MoveCenterTo'   ; // MoveCenterTo Xcenter YCenter Duration CurveID
     acMoveXCenterTo        = 'MoveXCenterTo'  ; // MoveXCenterTo X Duration CurveID
     acMoveYCenterTo        = 'MoveYCenterTo'  ; // MoveYCenterTo Y Duration CurveID

     acMoveRelative         = 'MoveRelative'   ; // MoveRelative DeltaX DeltaY Duration CurveID
     acMoveXRelative        = 'MoveXRelative'  ; // MoveXRelative DeltaX Duration CurveID
     acMoveYRelative        = 'MoveYRelative'  ; // MoveYRelative DeltaY Duration CurveID

     acSetCoor              = 'SetCoor'        ; // SetCoor X Y
     acSetCenterCoor        = 'SetCenterCoor'  ; // SetCenterCoor X Y  (single)

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
{$I oglcLogFile.inc}
{$I oglcVelocityCurve.inc}
{$I gl_core_matrix.inc}
{$I oglcType.inc}
{$I oglcAppSaveUtils.inc}
{$I oglcShader.inc}
{$I oglcMessage.inc}
{$I oglcScreenTemplate.inc}
{$I oglcTexture.inc}
{$I oglcTextureAtlas.inc}
{$I oglcTimerDelay.inc}
{$I oglcShaderFXPostProcessing.inc}
{$I oglcRenderToTexture.inc}
{$I oglcLayer.inc}
{$I oglcSurface.inc}
{$I oglcPath.inc}
{$I oglcCamera.inc}
{$I oglcBorderAndFill.inc}
{$I oglcMultiRendererFactory.inc}
{$I oglcSpriteTemplate.inc}
{$I oglcSpriteEffect.inc}
{$I oglcElectricalFX.inc}
{$I oglcGlow.inc}
{$I oglcDeformationGrid.inc}
{$I oglcSlideShow.inc}
{$I oglcUIClipping.inc}
{$I oglcTileMap.inc}
{$I oglcParticle.inc}
{$I oglcTexturedFont.inc}
{$I oglcUITheme.inc}
{$I oglcUI.inc}
{$I oglcUIScrollable.inc}
{$I oglcUIModalPanel.inc}
{$I oglcUtils.inc}
{$I oglcEnvironment.inc}
{$undef oglcINTERFACE}

type




{ TOGLCContext }

TOGLCContext = class(TLayerList)
 protected
  FOGLC: TOpenGLControl;
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
  FKeyPressedCount: integer;
  FLastKeyDown: Byte;
  FMouseManager: TMouseManager;
  FTextureManager: TTextureManager;
  function GetKeyToString(index: byte): string;
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
  constructor Create(aOGLContext: TOpenGLControl; aAspectRatio: single); virtual;
  destructor Destroy; override;

  // call from main windows (because TOpenGLControl don't handle key events)
  procedure ProcessOnKeyDown(var Key: Word; Shift: TShiftState);
  procedure ProcessOnKeyUp(var Key: Word; Shift: TShiftState);
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

  property MouseButtonState[btn: TMouseButton]: boolean read GetMouseButtonState;

  function ScaleDesignToScene(ASize: integer): integer;
  function ScaleDesignToSceneF(ASize: single): single;
  // Set here the design PPI for your app. This value will affects the value returned by
  // methods ScaleDesignToScene() and ScaleDesignToSceneF().
  property DesignPPI: integer read FDesignPPI write SetDesignPPI;
end;


{ TOGLCScene }
TOGLCScene = class(TOGLCContext)
 private
  FWaitLoopCountBeforeRunning: integer;
  FLog: TLog;
  FCurrentScreen,
  FScreenRequested: TScreenTemplate;
  FIsChangingScreen,
  FDoBlackScreenOnNewScreen: boolean;
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
  function GetFastLineRenderer: TOGLCFastLineRenderer;
  function GetGlowRenderer: TOGLCGlowRenderer;
  function GetSmoothLineRenderer: TOGLCSmoothLineRenderer;
  function GetTexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer;
  function GetTexturedTriangleRenderer: TOGLCTexturedTriangleRenderer;
  function GetThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer;
  function GetThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer;
  function GetTileRenderer: TOGLCTileRenderer;
  procedure CreateRenderers;
  procedure DestroyRenderers;
 private
  FModalPanelList: TModalPanelList;
 private
  procedure UpdateViewPortSize; override;
  procedure Draw;
  procedure UpDate(const DT:single);
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
{ public
  FRendererForPostProcessing: TOGLCRenderToTexture;
  FPostProcessingShader: TOGLCPostProcessingFX;   }
 public
  constructor Create(aOGLContext: TOpenGLControl; aAspectRatio: single); override;
  Destructor Destroy; override;

  procedure CreateLogFile(const aFilename: string; aDeletePrevious: boolean; aCallback: TOGLCLogCallback=NIL; aCallbackData: pointer=NIL);
  procedure LogEmptyLine(aSeparator: string=''); inline;
  procedure LogMess(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False); inline;
  procedure LogInfo(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False); inline;
  procedure LogWarning(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False); inline;
  procedure LogError(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False); inline;
  procedure LogDebug(const aMsg: string; aMarginCount: integer=0; aShowTime: boolean=False); inline;
  procedure LogStartMeasuringTime(const aMsg: string; aMarginCount: integer=0); inline;
  procedure LogStopMeasuringTime(const aMsg: string; aMarginCount: integer=0); inline;

  function GetRectArea: TRect;

  function MakeCurrent: boolean;
  // call DoLoop on Application idle process. It update and draw the scene
  procedure DoLoop;
  procedure ExecuteDuring( aTimeInSecond:single ); Deprecated;

  procedure RunScreen(aScreen: TScreenTemplate; DoBlackScreen: boolean=TRUE);
  property CurrentScreen: TScreenTemplate read FCurrentScreen;
  property ScreenFadeTime: single read FScreenFadeTime write FScreenFadeTime;

  // Add a surface to a layer
  procedure Add(aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0);
  procedure Insert(aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0); // insert a surface to a layer
  procedure RemoveSurfaceFromLayer(aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer); // remove a surface from the layer (but don't free it)
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
  // Callback
  property OnBeforePaint: TOGLCEvent read FOnBeforePaint write FOnBeforePaint;
  property OnAfterPaint: TOGLCEvent read FOnAfterPaint write FOnAfterPaint;
  // use this callback to load your global ressources
  property OnLoadCommonData: TOGLCEvent read FOnLoadCommonData write FOnLoadCommonData;
  // use this callback to free them
  property OnFreeCommonData: TOGLCEvent read FOnFreeCommonData write FOnFreeCommonData;

  property Mouse: TMouseManager read FMouseManager;
  property TexMan: TTextureManager read FTextureManager;

  property StencilClipping: TStencilClipping read FStencilClipping;
 public
  ProjectionMatrix,        // projection matrix
  ModelViewMatrix: TOGLCMatrix;   // Model View matrix
  function MVPMatrix: TOGLCMatrix; // Model View Projection matrix

  // a renderer is instancied only when you use it.
  property SmoothLineRenderer: TOGLCSmoothLineRenderer read GetSmoothLineRenderer;
  property TileRenderer: TOGLCTileRenderer read GetTileRenderer; // is instancied only when a TOGLCTileMap is instancied
  property GlowRenderer: TOGLCGlowRenderer read GetGlowRenderer; // is instancied only when a TOGLCGlow is instancied
  property FastLineRenderer: TOGLCFastLineRenderer read GetFastLineRenderer;
  property ThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer read GetThreeColorTriangleRenderer;
  property ThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer read GetThreeColorMVTriangleRenderer;
  property TexturedTriangleRenderer: TOGLCTexturedTriangleRenderer read GetTexturedTriangleRenderer;
  property TexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer read GetTexturedMVTriangleRenderer;

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

  function CreateSprite(aTexture: PTexture; aTextureOwner: boolean; aLayer: integer=0): TSprite;

  function Add_UILabel(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUILabel;

{  function Add_UITextArea(const aText: string;
                            aX, aY, aWidth, aHeight: integer;
                            aFont: TTexturedFont;
                            aAlignment: TOGLCAlignment;
                            aLayer: integer=0): TUITextArea;  }

  function Add_UIButton(const aCaption: string; aFont: TTexturedFont; aTexture: PTexture; aLayer: integer=0): TUIButton; overload;
  function Add_UICheck(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUICheck; overload;
  function Add_UIRadio(const aCaption: string; aFont: TTexturedFont; aLayer: integer=0): TUIRadio;
  function Add_UIProgressBar(aOrientation: TUIOrientation; aLayer: integer=0): TUIProgressBar;
  function Add_ModalPanel: TUIModalPanel;
end;
POGLCScene = ^TOGLCScene;

// this global variable represents the current context
var
    Scene: TOGLCScene;
    TexMan: TTextureManager;
    MouseMan: TMouseManager;
    ElapsedTime: single;

implementation
uses LclIntf;


procedure CheckGLError; inline;
{$ifdef CHECKGLERROR_ON} var errorCode: GLenum; {$endif}
begin
 {$ifdef CHECKGLERROR_ON}
  errorCode := glGetError();
  if errorCode <> GL_NO_ERROR then
    raise exception.Create('error code $'+IntToHex(errorCode, 4)+' appears: '+GLErrorToString(errorCode));
 {$endif}
end;

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

function TOGLCContext.GetUserPressAKey: boolean;
begin
  Result := FKeyPressedCount > 0;
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
  if p.ClientWidth > p.ClientHeight then begin
    w := Round(p.ClientHeight*FAspectRatio);
    if w > p.ClientWidth then w := p.ClientWidth;
    h := Trunc(w/FAspectRatio);
  end else begin
   h := Round(p.ClientWidth/FAspectRatio);
   if h > p.ClientHeight then h := p.ClientHeight;
   w := Trunc(h*FAspectRatio);
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

  FOGLC.OnResize := @ProcessOnResize;
  FOGLC.OnMouseDown := @ProcessOnMouseDown;
  FOGLC.OnMouseUp := @ProcessOnMouseUp;
  FOGLC.OnMouseMove := @ProcessOnMouseMove;
  FOGLC.OnMouseWheel := @ProcessOnMouseWheel;

  ClearKeysState;
  DesignPPI := DEFAULT_DESIGN_PPI;
end;

destructor TOGLCContext.Destroy;
begin
  FreeAndNil(FTextureManager);
  FreeAndNil(FMouseManager);
  inherited Destroy;
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
      if not FKeyState[byte(Key2)] then inc(FKeyPressedCount);
      FKeyState[byte(Key2)] := TRUE;
      FLastKeyDown := byte(Key2);
    end else FLastKeyDown := byte(Key);

    if not FKeyState[byte(Key)] then inc(FKeyPressedCount);
    FKeyState[byte(Key)] := TRUE;
    //FLastKeyDown := byte(Key);
    Key := 0;
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
      if FKeyState[byte(key2)] then begin
        FKeyPressed[byte(key2)] := TRUE;
        dec(FKeyPressedCount);
      end;
      FKeyState[byte(key2)] := FALSE;
    end;

    if FKeyState[byte(Key)] then begin
      FKeyPressed[byte(Key)] := TRUE;
      dec(FKeyPressedCount);
    end;
    FKeyState[byte(Key)] := FALSE;
    Key := 0;
  end;
if FKeyPressedCount < 0 then raise exception.create('FKeyPressedCount is < 0');
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyState[VK_LCONTROL] := FALSE
  else if ssShift in Shift then FKeyState[VK_LSHIFT] := FALSE
   else if ssAlt in Shift then FKeyState[VK_MENU] := FALSE
    else}
end;

procedure TOGLCContext.ProcessOnUTF8KeyPress(var UTF8Key: TUTF8Char);
begin
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
{$I gl_core_matrix.inc}
{$I oglcType.inc}
{$I oglcAppSaveUtils.inc}
{$I oglcShader.inc}
{$I oglcMessage.inc}
{$I oglcScreenTemplate.inc}
{$I oglcTexture.inc}
{$I oglcTextureAtlas.inc}
{$I oglcTimerDelay.inc}
{$I oglcShaderFXPostProcessing.inc}
{$I oglcRenderToTexture.inc}
{$I oglcLayer.inc}
{$I oglcSurface.inc}
{$I oglcPath.inc}
{$I oglcCamera.inc}
{$I oglcMultiRendererFactory.inc}
{$I oglcBorderAndFill.inc}
{$I oglcSpriteTemplate.inc}
{$I oglcSpriteEffect.inc}
{$I oglcElectricalFX.inc}
{$I oglcGlow.inc}
{$I oglcDeformationGrid.inc}
{$I oglcSlideShow.inc}
{$I oglcUIClipping.inc}
{$I oglcTileMap.inc}
{$I oglcParticle.inc}
{$I oglcUtils.inc}
{$I oglcUITheme.inc}
{$I oglcUI.inc}
{$I oglcUIScrollable.inc}
{$I oglcUIModalPanel.inc}
{$I oglcTexturedFont.inc}
{$I oglcEnvironment.inc}
{$undef oglcIMPLEMENTATION}

{ TOGLCScene }

constructor TOGLCScene.Create(aOGLContext: TOpenGLControl; aAspectRatio: single);
begin
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
  FScreenFadeTime:= 1.0;

  FTextureManager := TTextureManager.Create(Self);

  TimerManager := TTimerManager.Create;
  TimerManager.Add( @CallBackTimerFPS, 1000 );

  DelayManager := TDelayManager.Create;

  FMouseManager := TMouseManager.Create(Self);

  FStencilClipping.InitDefault;

  FExecuteDuringLoop := FALSE;
  FIsChangingScreen := FALSE;

  FCurrentScreen := NIL;
  FScreenRequested := NIL;

  FCameraList := TList.Create;

  FUpdatePeriod := 16;

  FWaitLoopCountBeforeRunning := 5;

  FModalPanelList := TModalPanelList.Create;
end;

destructor TOGLCScene.Destroy;
var i: Integer;
begin
  if FCurrentScreen <> NIL then begin
    FCurrentScreen.FreeObjects;
    if FCurrentScreen.FreeWhenLeave then FCurrentScreen.Free;
    FCurrentScreen := NIL;
  end;

  if FOnFreeCommonData <> NIL then FOnFreeCommonData;
  FCommonDataLoaded := FALSE;

{ if FRendererForPostProcessing<>NIL then FRendererForPostProcessing.Free;
 FPostProcessingShader.Free;  }

  for i:=0 to FCameraList.Count-1 do
   TOGLCCamera(FCameraList.Items[0]).Free;
  FreeAndNil(FCameraList);

  FreeAndNil(FGlobalFadeColor);

  SetLayerCount(0);
  FreeAndNil(TimerManager);
  FreeAndNil(DelayManager);
  FreeAndNil(FTextureManager);
  DestroyRenderers;

  FModalPanelList.Free;  // FreeAndNil(FModalPanelList) NOT WORKING HERE
  FModalPanelList := NIL;


  FreeVelocityCurveList;

  if FLog <> NIL then begin
    FLog.Add('Peace');
    FreeAndNil(FLog);
  end;

  inherited Destroy;
end;

procedure TOGLCScene.CreateLogFile(const aFilename: string; aDeletePrevious: boolean;
  aCallback: TOGLCLogCallback; aCallbackData: pointer);
begin
  FLog := TLog.Create(aFilename, aCallback, aCallbackData);
  if aDeletePrevious then FLog.DeleteLogFile;
  FLog.Mess('Begin '+ApplicationName+' on platform '+App.OSName, 0, True);
  FLog.AddEmptyLine;
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
begin
  if not MakeCurrent then exit;

  if not FGLInitialized then begin
    if not Load_GL_version_3_3_CORE // Load_GL_version_3_0 // Load_GL_VERSION_3_3_CORE
      then raise Exception.Create('Cannot load OpenGL 3.3 core...');
    SetBlendMode(FX_BLEND_NORMAL);
    //glEnable(GL_POLYGON_SMOOTH);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(PRIMITIVE_INDEX_RESTART_VALUE);

    CheckGLError;

    UpdateViewPortSize;
    CreateRenderers;

    FGLInitialized := TRUE;

{   FRendererForPostProcessing:= TOGLCRenderToTexture.Create(Self, GetSceneWidth, GetSceneHeight, 0);
   if not FRendererForPostProcessing.Ready
     then Showmessage('FBO for scene post processing not ready !...');
   FPostProcessingShader:= TOGLCPostProcessingFX.Create;
   FPostProcessingShader.SetParam(FRendererForPostProcessing.RenderedTexture, 0);  }

    FTickOriginUpdate := GetTickCount64;
    FTickAccu := 0;

    glClearColor(FBackgroundColorF.r, FBackgroundColorF.g, FBackgroundColorF.b, FBackgroundColorF.a);
    glClear(GL_COLOR_BUFFER_BIT);
    FOGLC.SwapBuffers;
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

  TimerManager.ProcessTimer;
  DelayManager.ProcessDelay;

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

   FIsChangingScreen := TRUE;
   if FDoBlackScreenOnNewScreen and
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
       FCurrentScreen := FScreenRequested;
       FScreenRequested := NIL;
       FCurrentScreen.ClearMessageList;
       FCurrentScreen.CreateObjects;
       ClearKeysState;
       if FDoBlackScreenOnNewScreen then
         ColorFadeOut(FScreenFadeTime);
       FIsChangingScreen := FALSE;
       Mouse.MousePoolEnabled := True;
   end;
 end;

{ // compute pause to achieve the wanted frame rate
 Application.ProcessMessages;
 if FUpdatePeriod > 0 then begin
   delta := GetTickCount64 - FTickOriginFrame;
   if (delta > 0) and (delta < Trunc(1000/FUpdatePeriod)) then begin            //FMonitorRefreshPeriod
     delta := Trunc(1000/FUpdatePeriod)-delta;
     sleep(delta);
   end;
 end;
 FTickOriginFrame := GetTickCount64;  }
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

procedure TOGLCScene.RunScreen(aScreen: TScreenTemplate; DoBlackScreen: boolean);
begin
 if FIsChangingScreen then exit;
 FScreenRequested := aScreen;
 FDoBlackScreenOnNewScreen := DoBlackScreen;
end;

procedure TOGLCScene.CreateRenderers;
begin
  FFastLineRenderer := TOGLCFastLineRenderer.Create(Self);
  FThreeColorTriangleRenderer := TOGLCThreeColorTriangleRenderer.Create(Self);
  FThreeColorMVTriangleRenderer := TOGLCThreeColorMVTriangleRenderer.Create(Self);
  FTexturedTriangleRenderer := TOGLCTexturedTriangleRenderer.Create(Self);
  FTexturedMVTriangleRenderer := TOGLCTexturedMVTriangleRenderer.Create(Self);
  FSmoothLineRenderer := TOGLCSmoothLineRenderer.Create(Self);
end;

procedure TOGLCScene.DestroyRenderers;
begin
  FreeAndNil(FFastLineRenderer);
  FreeAndNil(FThreeColorTriangleRenderer);
  FreeAndNil(FThreeColorMVTriangleRenderer);
  FreeAndNil(FTexturedMVTriangleRenderer);
  FreeAndNil(FTexturedTriangleRenderer);
  FreeAndNil(FSmoothLineRenderer);
  FreeAndNil(FTileRenderer);
  FreeAndNil(FGlowRenderer);

  FreeAndNil(ParticleRenderer);
  FreeAndNil(FElectricalBeamRenderer);
end;

procedure TOGLCScene.UpdateViewPortSize;
begin
 inherited UpdateViewPortSize;

 ProjectionMatrix.Ortho(0, FOGLC.Width, FOGLC.Height, 0, 0.0, 1.0);
 //ProjectionMatrix.Ortho(FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
 ModelViewMatrix.LoadIdentity; // can be used to resize the view
                               // to fit high dpi screen ?
{
 // Matrix for legacy OpenGL
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 glOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity();  }

  glViewport(0, 0, FOGLC.Width, FOGLC.Height);
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

procedure TOGLCScene.KillCamera(var aCamera: TOGLCCamera);
var i: integer;
begin
  for i:=0 to LayerCount-1 do
    if Layer[i].Camera = aCamera then Layer[i].Camera := NIL;
  FCameraList.Remove( aCamera );
  aCamera.Free;
  aCamera := NIL;
end;

function TOGLCScene.Add_UILabel(const aCaption: string; aFont: TTexturedFont;
  aLayer: integer): TUILabel;
begin
  Result := TUILabel.Create(Self, aCaption, aFont);
  Add(Result, aLayer);
end;

{function TOGLCScene.Add_UITextArea(const aText: string; aX, aY, aWidth,
  aHeight: integer; aFont: TTexturedFont; aAlignment: TOGLCAlignment; aLayer: integer): TUITextArea;
begin
 Result := TUITextArea.Create(Self, aText, aX, aY, aWidth, aHeight, aFont, aAlignment);
 Add(Result, aLayer);
end; }

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

function TOGLCScene.Add_UIProgressBar(aOrientation: TUIOrientation; aLayer: integer): TUIProgressBar;
begin
  Result := TUIProgressBar.Create(Self, aOrientation);
  Add(Result, aLayer);
end;

function TOGLCScene.Add_ModalPanel: TUIModalPanel;
begin
  Result := TUIModalPanel.Create(Self);
end;

procedure TOGLCScene.Draw;
var L, i: integer;
    o: TSimpleSurfaceWithEffect;
begin
{ if FPostProcessingShader.FFX<>[]
   then FRendererForPostProcessing.Bind;  }

  // delete all surface with FKill=true
  for L:=0 to LayerCount-1 do
    for i:=Layer[L].FList.Count-1 downto 0 do begin
      o := Layer[L].Surface[i];
      if o.FKill then begin
        Layer[L].Delete(i);
        o.Free;
      end;
    end;

  glClearColor(FBackgroundColorF.r, FBackgroundColorF.g, FBackgroundColorF.b, FBackgroundColorF.a);
  glClear(GL_COLOR_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // Before paint CallBack
  if Assigned(FOnBeforePaint) then FOnBeforePaint;

  // Render all Layers
  for L:=LayerCount-1 downto 0 do
    Layer[L].Draw;

  // Scene global fade
  if FGlobalFadeColor.Alpha.Value > 0
    then FillBox(Self, 0, 0, GetViewPortWidth, GetViewPortHeight, FGlobalFadeColor.Value);

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

  // render an eventual remains in the batch renderer process
  FTexturedMVTriangleRenderer.Batch_Flush;

{ if FPostProcessingShader.FFX<>[] then begin
   // draw post-processing frame buffer on screen
  FRendererForPostProcessing.Unbind;

   SetBlendMode(FX_BLEND_NORMAL);
   //DrawTexture( FRendererForPostProcessing.RenderedTexture, 0, 0, 0, 0, 255, BGRA(0,0,0,0));
  FPostProcessingShader.SetParam(FRendererForPostProcessing.RenderedTexture, 0);
  FPostProcessingShader.Use;
  glActiveTexture(GL_TEXTURE0);
  TextMan.BindTexture( FRendererForPostProcessing.RenderedTexture, 0 );

  glBegin( GL_TRIANGLE_STRIP );
    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].y, 1.0);
    glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,0 );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].y, 1.0);
    glVertex2f( 0,0 );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].y, 1.0);
    glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,FRendererForPostProcessing.RenderedTexture^.TextureHeight );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].y, 1.0);
    glVertex2f( 0,FRendererForPostProcessing.RenderedTexture^.TextureHeight );
  glEnd;



{
  glBegin( GL_TRIANGLES );
   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].y, 1.0);
   glVertex2f( 0,0 );

   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].y, 1.0);
   glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,0 );

   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].y, 1.0);
   glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,FRendererForPostProcessing.RenderedTexture^.TextureHeight );

   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].y, 1.0);
   glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,FRendererForPostProcessing.RenderedTexture^.TextureHeight );

   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].y, 1.0);
   glVertex2f( 0,FRendererForPostProcessing.RenderedTexture^.TextureHeight );

   glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0] );
   //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].y, 1.0);
   glVertex2f( 0,0 );

  glEnd;
}


{
  glBegin( GL_QUADS );
    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][0].y, 1.0);
    glVertex2f( 0,0 );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][1].y, 1.0);
    glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,0 );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][2].y, 1.0);
    glVertex2f( FRendererForPostProcessing.RenderedTexture^.TextureWidth,FRendererForPostProcessing.RenderedTexture^.TextureHeight );

    glTexCoord2fv( @FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3] );
    //glColor3f(FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].x, FRendererForPostProcessing.RenderedTexture^.FramesCoord[0][3].y, 1.0);
    glVertex2f( 0,FRendererForPostProcessing.RenderedTexture^.TextureHeight );
  glEnd;
}

  FPostProcessingShader.Release;
//  glActiveTexture(GL_TEXTURE0);
 end;   }
end;

procedure TOGLCScene.UpDate(const DT: single);
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

  // update Modal Panel or layer
  if FModalPanelList.OneModalIsVisible then FModalPanelList.Update(ElapsedTime)
  else begin
  //if not FModalPanelList.Update(ElapsedTime) then begin
    // update camera objects
    for i:=0 to FCameraList.Count-1 do
      TOGLCCamera(FCameraList.Items[i]).Update(ElapsedTime);
    // update all layers
    for i:=LayerCount-1 downto 0 do Layer[i].Update(DT);
    // update current screen
    if FCurrentScreen <> NIL then FCurrentScreen.Update(DT);
  end;

  Mouse.UpDate(DT);

  // Scene global fade
  FGlobalFadeColor.OnElapse(DT);
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
  if Result then begin
    OGLCScene.Scene := Self;
    OGLCScene.TexMan := TexMan;
    OGLCScene.MouseMan := Mouse;
  end;
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

function TOGLCScene.GetFastLineRenderer: TOGLCFastLineRenderer;
begin
  if FFastLineRenderer = NIL then FFastLineRenderer := TOGLCFastLineRenderer.Create(Self);
  Result := FFastLineRenderer;
end;

function TOGLCScene.GetGlowRenderer: TOGLCGlowRenderer;
begin
  if FGlowRenderer = NIL then FGlowRenderer := TOGLCGlowRenderer.Create(Self);
  Result := FGlowRenderer;
end;

function TOGLCScene.GetSmoothLineRenderer: TOGLCSmoothLineRenderer;
begin
  if FSmoothLineRenderer = NIL then FSmoothLineRenderer := TOGLCSmoothLineRenderer.Create(Self);
  Result := FSmoothLineRenderer;
end;

function TOGLCScene.GetTexturedMVTriangleRenderer: TOGLCTexturedMVTriangleRenderer;
begin
  if FTexturedMVTriangleRenderer = NIL then FTexturedMVTriangleRenderer := TOGLCTexturedMVTriangleRenderer.Create(Self);
  Result := FTexturedMVTriangleRenderer;
end;

function TOGLCScene.GetTexturedTriangleRenderer: TOGLCTexturedTriangleRenderer;
begin
  if FTexturedTriangleRenderer = NIL then FTexturedTriangleRenderer := TOGLCTexturedTriangleRenderer.Create(Self);
  Result := FTexturedTriangleRenderer;
end;

function TOGLCScene.GetThreeColorMVTriangleRenderer: TOGLCThreeColorMVTriangleRenderer;
begin
  if FThreeColorMVTriangleRenderer = NIL then FThreeColorMVTriangleRenderer := TOGLCThreeColorMVTriangleRenderer.Create(Self);
  Result := FThreeColorMVTriangleRenderer;
end;

function TOGLCScene.GetThreeColorTriangleRenderer: TOGLCThreeColorTriangleRenderer;
begin
  if FThreeColorTriangleRenderer = NIL then FThreeColorTriangleRenderer := TOGLCThreeColorTriangleRenderer.Create(Self);
  Result := FThreeColorTriangleRenderer;
end;

function TOGLCScene.GetTileRenderer: TOGLCTileRenderer;
begin
  if FTileRenderer = NIL then FTileRenderer := TOGLCTileRenderer.Create(Self);
  Result := FTileRenderer;
end;

procedure TOGLCScene.SetUpdatePeriod(AValue: integer);
begin
  FUpdatePeriod := AValue;
end;

end.

