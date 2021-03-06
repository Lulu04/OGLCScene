{
  **************************************************************************
 *                                                                          *
 *  This file is the main part of OGLCScene library.                        *
 *                                                                          *
 *  See the file LICENSE included in this distribution,                     *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This software is distributed in the hope of being useful                *
 *  for learning purposes about OpenGL and Lazarus,                         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
  **************************************************************************

 written by Lulu  2017 - 2018

}
unit OGLCScene;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, Controls,
  types,
  OpenGLContext, GLExt, GL,
  DynLibs,
  lazutf8,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRAPath, BGRAGradientScanner,
  BGRASVG,
  VelocityCurve,
  math,
  strutils;


//{$DEFINE DEBUG_MODE_ON}

type
TOGLCScene = class;

// Callback for OGLCScene.OnBeforePaint and OGLCScene.OnAfterPaint
TOGLCEvent = procedure of object;
TDelayCallBack = procedure( UserValue: word ) of object;
TMessageCallBack = procedure( UserValue: word ) of object;

{ in the scene, each surface can play scenarios. A scenario is an external text file
  that contain the actions to execute.
  Below you can find all the available actions to build your scenario
}

// SCENARIO
TIDScenario = integer;
const
     // Boolean value
     acTRUE                 = 'TRUE'     ;
     acFALSE                = 'FALSE'    ;
     // Available actions for sprite
     acLoop                 =  'Loop'    ; // Loop
     acLabel                =  'Label'   ; // Label UserLabelName
     acGotoLabel            =  'Goto'    ; // Goto UserLabelName
     acWait                 =  'Wait'    ; // Wait second

     acKill                 =  'Kill'    ; // Kill
     acFreeze               =  'Freeze'  ; // Freeze TRUE/FALSE
     acVisible              =  'Visible' ; // Visible TRUE/FALSE

     acFlipH                =  'FlipH'       ; // FlipH TRUE/FALSE
     acFlipV                =  'FlipV'       ; // FlipV TRUE/FALSE
     acToggleFlipH          =  'ToggleFlipH' ; // ToggleFlipH
     acToggleFlipV          =  'ToggleFlipV' ; // ToggleFlipV

     acOpacity              =  'Opacity' ;       // Opacity NewOpacity
     acOpacityChange        =  'OpacityChange' ; // OpacityChange NewOpacity Duration Curve

     acAnimate              =  'Animate' ; // Animate StartFrameIndex EndFrameIndex FramePerSecond(single)
     acIncFrame             =  'IncFrame'; // IncFrame
     acDecFrame             =  'DecFrame'; // DecFrame
     acSetFrame             =  'SetFrame'; // SetFrame ImageIndex

     acTint                 =  'Tint'    ; // Tint red green blue alpha
     acTintChange           =  'TintChange' ; // TintChange Red Green Blue Alpha Duration Curve
     acTintRedChange        =  'TintRedChange' ;   // TintRedChange NewRedValue Duration Curve
     acTintGreenChange      =  'TintGreenChange' ; // TintGreenChange NewGreenValue Duration Curve
     acTintBlueChange       =  'TintBlueChange' ;  // TintBlueChange NewBlueValue Duration Curve
     acTintAlphaChange      =  'TintAlphaChange' ; // TintAlphaChange NewAlphaValue Duration Curve

     acSkew                 = 'Skew'     ; // Skew XValue YValue Duration Curve

     acAngle                = 'Angle'    ; // Angle NewAngle
     acRotate               = 'Rotate'   ; // Rotate AnglePerSecond
     acRotateTo             = 'RotateTo' ; // RotateTo Angle Duration Curve
     acRotationAroundAxis   = 'RotationAroundAxis'    ; // RotationAroundAxis XAxis YAxis AnglePerSecond SelfRotate

     acScale                = 'Scale'          ; // Scale HVValue      // [1]= normal size, [0..1[->reduced, ]1..inf[->enlarged
     acScaleChange          = 'ScaleChange'    ; // ScaleChange HVNewValue Duration Curve
     acScaleH               = 'ScaleH'         ; // ScaleH HValue      // to set scale value for horizontaly axis
     acScaleHChange         = 'ScaleHChange'   ; // ScaleHChange HNewValue Duration Curve
     acScaleV               = 'ScaleV'         ; // ScaleV VValue      // to set scale value for verticaly axis
     acScaleVChange         = 'ScaleVChange'   ; // ScaleVChange VNewValue Duration Curve


     acBlink                = 'Blink'    ; // Blink NumberOfBlink(-1 for infinite) aVisibleTime aInvisibleTime
     acStopBlink            = 'StopBlink'; // StopBlink

     acMoveTo               = 'MoveTo'         ; // MoveTo X Y Duration Curve
     acMoveXTo              = 'MoveXTo'        ; // MoveXTo X Duration Curve
     acMoveYTo              = 'MoveYTo'        ; // MoveYTo Y Duration Curve

     acMoveCenterTo         = 'MoveCenterTo'   ; // MoveCenterTo Xcenter YCenter Duration Curve
     acMoveXCenterTo        = 'MoveXCenterTo'  ; // MoveXCenterTo X Duration Curve
     acMoveYCenterTo        = 'MoveYCenterTo'  ; // MoveYCenterTo Y Duration Curve

     acMoveRelative         = 'MoveRelative'   ; // MoveRelative DeltaX DeltaY Duration Curve
     acMoveXRelative        = 'MoveXRelative'  ; // MoveXRelative DeltaX Duration Curve
     acMoveYRelative        = 'MoveYRelative'  ; // MoveYRelative DeltaY Duration Curve

     acSetCoor              = 'SetCoor'        ; // SetCoor X Y
     acSetCenterCoor        = 'SetCenterCoor'  ; // SetCenterCoor X Y  (single)

     acCenterOnScene        = 'CenterOnScene'  ; // CenterOnScene

     acEnableGui            = 'EnableGui';  // EnableGui
     acDisableGui           = 'DisableGui'; // DisableGui

     // Available actions for user event
     acSendEvent            = 'SendEvent' ; // SendEvent UserEvent


// Blend mode
const
     FX_BLEND_NORMAL = $00;
     FX_BLEND_ADD    = $01;
     FX_BLEND_MULT   = $02;
     FX_NOBLEND      = $03;

     FX_COLOR_MIX    = $00;
     FX_COLOR_SET    = $01;

type
ArrayOfString = array of string;

TOGLCCamera = class;

TOGLCAlignment=(taTopLeft, taTopCenter, taTopRight,
                taCenterLeft, taCenterCenter, taCenterRight,
                taBottomLeft, taBottomCenter, taBottomRight);

{$define oglcINTERFACE}
{$I gl_core_matrix.inc }
{$I oglcListType.inc }
{$I oglcMessage.inc }
{$I oglcTexture.inc }
{$I oglcTimerDelay.inc }
{$I oglcShader.inc }
{$I oglcRenderToTexture.inc }
{$I oglcLayer.inc }
{$I oglcSurface.inc }
{$I oglcPath.inc }
{$I oglcCamera.inc }
{$I oglcSpriteTemplate.inc }
{$I oglcSpriteEffect.inc }
{$I oglcElectricalFX.inc }
{$I oglcGlow.inc }
{$I oglcDeformationGrid.inc }
{$I oglcSlideShow.inc }
{$I oglcTileMap.inc }
{$I oglcParticle.inc }
{$I oglcPathToDraw.inc }
{$I oglcGUI.inc }
{$I oglcUtils.inc }
{$I oglcTexturedFont.inc }
{$I oglcEnvironment.inc }
{$undef oglcINTERFACE}

type

// abstract class to manage one stage of your game application

{ TStageSkeleton }

TStageSkeleton = class
private
 FMessageList: TMessageList;
public
 constructor Create;
 destructor Destroy; override;
public
 FreeWhenLeave: boolean; // set to true to free automatically the stage when the application leave it

 // post a message to the stage. the message will be processed later in the Update method
 // the delay allows to process the message only after a lapse of time (in seconds)
 procedure AddMessage( UserValue: word; aDelay: single=0 );
 // override to process the messages received
 procedure ProcessMessage( {%H-}UserValue: word ); virtual;

 // override to load texture, create sprite, gui object, sound, initialization, etc...
 procedure LoadData; virtual; abstract;
 // override to free all that need to be freed at the end of this stage
 procedure FreeData; virtual; abstract;
 // override to update your stuff according time. don't forget to call inherited.
 procedure Update( AElapsedTime: single ); virtual;
end;


{ TOGLCScene }
TOGLCScene = class (TLayerList)
 protected
  FOGLC: TOpenGLControl;
 private
  FOGLCOnResize : TNotifyEvent;
  FOGLCOnClick : TNotifyEvent;
  FFlagMouseLeftClicked: boolean;
  FGLInitialized,
  FNeedToResizeViewPort: boolean;
  FKeyMap : array[0..255] of boolean; // TRUE if Key is actualy pressed
  FKeyPressed: array[0..255] of boolean; // TRUE if a key was pressed then released
  FCurrentStage,
  FStageRequested: TStageSkeleton;
  FIsChangingStage,
  FDoBlackScreenOnNewStage: boolean;
  procedure NewOnResize( Sender:TObject );
  procedure NewOnClick( Sender:TObject );
  procedure UpdateViewPortSize;
  procedure Draw;
  procedure UpDate ( const DT:single );
 private
  FTickOrigin : QWord;
  // FPS
  FFPSCounter : integer;
  FFPS        : integer;
  FMonitorRefreshRate: integer;
  FExecuteDuringLoop: boolean;
  procedure CallBackTimerFPS;
 private
  FGlobalFadeColor: TBGRAParam;
  FBackgroundColorF: TColorF;
  FFadeTimeForStageChange: single;
  function GetSceneHeight: integer;
  function GetSceneWidth: integer;
  procedure SetBackgroundColor ( aColor:TBGRAPixel );
 private
  // CallBack
  FOnBeforePaint,
  FOnAfterPaint,
  FOnLoadCommonData,
  FOnFreeCommonData : TOGLCEvent;
  FCommonDataLoaded: boolean;
  function GetKeyMap(index: byte): boolean;
  procedure SetLayerCount(AValue: integer ); override ;
 private
  FCamera: TOGLCCamera;
  FCameraList: TList;
  function GetBackgroundColor: TBGRAPixel;
  function GetKeyPressed(index: byte): boolean;
  function GetSceneCenter: TPointF;
  procedure SetKeyMap(index: byte; AValue: boolean);
{ public
  FRendererForPostProcessing: TOGLCRenderToTexture;
  FPostProcessingShader: TOGLCPostProcessingFX;   }
 public

  Constructor Create( aOGLContext: TOpenGLControl );
  Destructor Destroy; override;
  function GetRectArea: TRect;
  // called from main windows
  procedure ProcessKeyDown( Key: Word; {%H-}Shift: TShiftState);
  procedure ProcessKeyUp( Key: Word; {%H-}Shift: TShiftState);
  procedure ProcessMouseDown( Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseUp( Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);

  // call DoLoop on Application idle process. It update and draw the scene
  procedure DoLoop;
  procedure ExecuteDuring( aTimeInSecond:single ); Deprecated;

  procedure LaunchStage( AStage: TStageSkeleton; DoBlackScreen: boolean=TRUE );
  property CurrentStage: TStageSkeleton read FCurrentStage;
  property FadeTimeForStageChange: single read FFadeTimeForStageChange write FFadeTimeForStageChange;

  // Add a surface to a layer
  procedure Add( aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  procedure Insert ( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0); // insert a surface to a layer
  procedure RemoveSurfaceFromLayer( aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer); // remove a surface from the layer (but don't free it)
  function GetSurfaceByIndex( aLayerIndex, aSurfaceIndex: integer): TSimpleSurface;

  // Global scene color fade in and out
  procedure ColorFadeIn( const aColor: TBGRAPixel; const aDurationInSecond: single );
  procedure ColorFadeOut( const aDurationInSecond: single );

  property Width: integer read GetSceneWidth;
  property Height: integer read GetSceneHeight;
  property Center: TPointF read GetSceneCenter;
  property FPS: integer read FFPS;
  // set the monitor refresh rate
  // default value is 60Hz
  // set to 0 to force real time between swap buffer
  // set a value different from 0 to force fixed time. example 60Hz force time to 1/60=0.01666 sec
  property MonitorRefreshRate: integer read FMonitorRefreshRate write FMonitorRefreshRate;

  property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor ;
  // Callback
  property OnBeforePaint: TOGLCEvent read FOnBeforePaint write FOnBeforePaint;
  property OnAfterPaint: TOGLCEvent read FOnAfterPaint write FOnAfterPaint;
  // use this callback to load your global ressources
  property OnLoadCommonData: TOGLCEvent read FOnLoadCommonData write FOnLoadCommonData;
  // use this callback to free them
  property OnFreeCommonData: TOGLCEvent read FOnFreeCommonData write FOnFreeCommonData;
 public
  procedure ClearKeysState;
  property Key[index:byte]: boolean read GetKeyMap write SetKeyMap;
  property KeyPressed[index:byte]: boolean read GetKeyPressed;

  property Camera: TOGLCCamera read FCamera;
 public
  // convenience functions
  function CreateCamera: TOGLCCamera;
  // unassign the camera on all layers, and free it.
  // After this method, aCamera is set to NIL.
  procedure KillCamera( var aCamera: TOGLCCamera );

  function Add_GuiLabel( const aCaption: string; aFont:TGuiFont; aBackGround: TBGRABitmap; aLayer: integer ): TGuiLabel;

  function Add_GuiTextArea( const aText: string;
                            aX, aY, aWidth, aHeight: integer;
                            aFont: TGuiFont;
                            aHorizAlign: TAlignment; aVerticalAlign: TTextLayout;
                            aLayer: integer ): TGuiTextArea;

  function Add_GuiButton( const aCaption: string; aFont:TGuiFont; aBackGround: TBGRABitmap; aLayer: integer ): TGuiButton; overload;
  function Add_GuiButton( aImage : TBGRABitmap; aLayer: integer ): TGuiButton; overload;
  function Add_GuiButton( const aFilename : string; aLayer: integer ): TGuiButton; overload;

  function Add_GuiCheck( const aCaption: string; aFont:TGuiFont; aBackGround: TBGRABitmap; aLayer: integer ): TGuiCheck; overload;
  function Add_GuiCheck( aImage : TBGRABitmap; aLayer: integer ): TGuiCheck; overload;
  function Add_GuiCheck( const aFilename : string; aLayer: integer ): TGuiCheck; overload;

  function Add_GuiRadio( const aCaption: string; aFont:TGuiFont; aBackGround: TBGRABitmap; aLayer: integer ): TGuiRadio;

  function Add_GuiProgressBar( aX, aY: single; aWidth, aHeight: integer; aLayer: integer ): TGuiProgressBar;
end;
POGLCScene = ^TOGLCScene ;

implementation

{ TStageSkeleton }

constructor TStageSkeleton.Create;
begin
 FMessageList:= TMessageList.Create;
end;

destructor TStageSkeleton.Destroy;
begin
 FMessageList.Free;
  inherited Destroy;
end;

procedure TStageSkeleton.AddMessage(UserValue: word; aDelay: single);
begin
 FMessageList.Add( UserValue, @Self.ProcessMessage, aDelay );
end;

procedure TStageSkeleton.ProcessMessage(UserValue: word);
begin
 // override to process the received message
end;

procedure TStageSkeleton.Update(AElapsedTime: single);
begin
 FMessageList.ProcessMessages(AElapsedTime);
end;

{$define oglcIMPLEMENTATION}
{$I gl_core_matrix.inc }
{$I oglcListType.inc }
{$I oglcMessage.inc }
{$I oglcTexture.inc }
{$I oglcTimerDelay.inc }
{$I oglcShader.inc }
{$I oglcRenderToTexture.inc }
{$I oglcLayer.inc }
{$I oglcSurface.inc }
{$I oglcPath.inc }
{$I oglcCamera.inc }
{$I oglcSpriteTemplate.inc }
{$I oglcSpriteEffect.inc }
{$I oglcElectricalFX.inc }
{$I oglcGlow.inc }
{$I oglcDeformationGrid.inc }
{$I oglcSlideShow.inc }
{$I oglcTileMap.inc }
{$I oglcParticle.inc }
{$I oglcPathToDraw.inc }
{$I oglcUtils.inc }
{$I oglcGUI.inc }
{$I oglcTexturedFont.inc }
{$I oglcEnvironment.inc }
{$undef oglcIMPLEMENTATION}

{ TOGLCScene }

constructor TOGLCScene.Create(aOGLContext: TOpenGLControl);
begin
 inherited Create;

 // fix decimal separator to dot to avoid exception when converting floating point values from scenario's files
 SysUtils.DefaultFormatSettings.DecimalSeparator := '.';

 FGLInitialized := FALSE;
 FOGLC := aOGLContext;

 FOGLCOnResize := FOGLC.OnResize;
 FOGLC.OnResize := @NewOnResize;

 FOGLCOnClick := FOGLC.OnClick;
 FOGLC.OnClick := @NewOnClick;
 FFlagMouseLeftClicked:=FALSE;

 ClearKeysState;

 FBackgroundColorF[1]:= 0;
 FBackgroundColorF[2]:= 0;
 FBackgroundColorF[3]:= 0;
 FBackgroundColorF[4]:= 1;

 FOnBeforePaint := NIL;
 FOnAfterPaint := NIL;
 FOnLoadCommonData := NIL;
 FOnFreeCommonData := NIL;
 FCommonDataLoaded := FALSE;

 FGlobalFadeColor:= TBGRAParam.Create;
 FGlobalFadeColor.Value := BGRA(0,0,0,0);
 FFadeTimeForStageChange:= 1.0;


 TextureManager := TTextureManager.Create;

 TimerManager := TTimerManager.Create;
 TimerManager.Add( @CallBackTimerFPS, 1000 );

 DelayManager := TDelayManager.Create;

 MouseManager := TMouseManager.Create;
 MouseManager.FParentScene := Self;

 FontManager := TFontManager.Create;

 FExecuteDuringLoop := FALSE;
 FIsChangingStage := FALSE;

 FCurrentStage := NIL;
 FStageRequested := NIL;

 FCameraList := TList.Create;
 FCamera := CreateCamera;

 FMonitorRefreshRate := 60;
end;

destructor TOGLCScene.Destroy;
var i: Integer;
begin
 if FCurrentStage <> NIL then begin
   FCurrentStage.FreeData;
   if FCurrentStage.FreeWhenLeave then FCurrentStage.Free;
   FCurrentStage := NIL;
 end;

 if FOnFreeCommonData <> NIL then FOnFreeCommonData;
 FCommonDataLoaded := FALSE;

{ if FRendererForPostProcessing<>NIL then FRendererForPostProcessing.Free;
 FPostProcessingShader.Free;  }

 for i:=0 to FCameraList.Count-1 do
  TOGLCCamera(FCameraList.Items[0]).Free;
 FCameraList.Free;

 FreeAndNil( FGlobalFadeColor );

 SetLayerCount(0);
 FontManager.Free;
 TimerManager.Free;
 DelayManager.Free;
 MouseManager.Free;
 TextureManager.Free;

 inherited Destroy;
end;

procedure TOGLCScene.DoLoop;
var t: QWord;
    sec: single;
    i: Integer;
begin
 if not FOGLC.MakeCurrent() then exit;

 if not FGLInitialized then begin
   if not Load_GL_version_3_0
     then raise Exception.Create('Cannot load OpenGL 3.0...');

   SetBlendMode( FX_BLEND_NORMAL ) ;
   glEnable(GL_POLYGON_SMOOTH or GL_LINE_SMOOTH);

   UpdateViewPortSize;

   FGLInitialized := TRUE;

{   FRendererForPostProcessing:= TOGLCRenderToTexture.Create(Self, GetSceneWidth, GetSceneHeight, 0);
   if not FRendererForPostProcessing.Ready
     then Showmessage('FBO for scene post processing not ready !...');
   FPostProcessingShader:= TOGLCPostProcessingFX.Create;
   FPostProcessingShader.SetParam(FRendererForPostProcessing.RenderedTexture, 0);  }

   FTickOrigin := GetTickCount64;
 end;

 if FNeedToResizeViewPort then begin
   UpdateViewPortSize;
   FNeedToResizeViewPort:=FALSE;
 end;

 if not FCommonDataLoaded and ( FOnLoadCommonData <> NIL ) then begin
   FOnLoadCommonData;
   FCommonDataLoaded := TRUE;
 end;

 TimerManager.ProcessTimer;
 DelayManager.ProcessDelay;

 t := GetTickCount64;
 if FMonitorRefreshRate<1
   then sec := ( t - FTickOrigin ) * 0.001
   else sec := 1/FMonitorRefreshRate;
 FTickOrigin := t;

 // update camera objects
 for i:=0 to FCameraList.Count-1 do
   TOGLCCamera(FCameraList.Items[i]).Update( sec );

 UpDate( sec );

 Draw;
 glFlush;
 FOGLC.SwapBuffers;
 glFinish;
 inc( FFPSCounter );

 FFlagMouseLeftClicked := FALSE;

 // if needed, launch the next stage
 if FStageRequested <> NIL then begin

   FIsChangingStage := TRUE;
   if FDoBlackScreenOnNewStage and
     (FGlobalFadeColor.State=psNO_CHANGE) and
     (FGlobalFadeColor.Value<>BGRABlack)
     then ColorFadeIn( BGRABlack, FFadeTimeForStageChange );

   if FGlobalFadeColor.State=psNO_CHANGE then begin
       if FCurrentStage <> NIL then begin
         FCurrentStage.FreeData;
         if FCurrentStage.FreeWhenLeave
           then FCurrentStage.Free;
       end;
       FCurrentStage := FStageRequested;
       FStageRequested := NIL;
       FCurrentStage.LoadData;
       ClearKeysState;
       if FDoBlackScreenOnNewStage then
         ColorFadeOut( FFadeTimeForStageChange );
       FIsChangingStage := FALSE;
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

procedure TOGLCScene.LaunchStage(AStage: TStageSkeleton; DoBlackScreen: boolean);
begin
 if FIsChangingStage then exit;
 FStageRequested := AStage;
 FDoBlackScreenOnNewStage := DoBlackScreen;
end;

procedure TOGLCScene.NewOnResize(Sender: TObject);
begin
 FNeedToResizeViewPort := TRUE;
 if FOGLCOnResize <> NIL then FOGLCOnResize( Sender );
end;

procedure TOGLCScene.NewOnClick(Sender: TObject);
begin
 FFlagMouseLeftClicked := TRUE;
 if FOGLCOnClick <> NIL then FOGLCOnClick( Sender );
end;

procedure TOGLCScene.UpdateViewPortSize;
begin
 // matrix emulator for OpenGL core
 gnGL.mat[nGL_PROJECTION] := IdentityHmgMatrix;
 gnGL.mat[nGL_MODELVIEW] := IdentityHmgMatrix;
 nglMatrixMode(nGL_PROJECTION);
 nglOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
 nglMatrixMode(nGL_MODELVIEW);

 // Matrix for legacy OpenGL
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 glOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);
 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity();

 glViewport(0, 0, GetSceneWidth, GetSceneHeight );
end;

procedure TOGLCScene.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyMap[VK_LCONTROL] := TRUE
  else if ssShift in Shift then FKeyMap[VK_LSHIFT] := TRUE
   else if ssAlt in Shift then FKeyMap[VK_MENU] := TRUE
    else} FKeyMap[byte(Key)] := TRUE;
end;

procedure TOGLCScene.ProcessKeyUp(Key: Word; Shift: TShiftState);
begin
 if FKeyMap[byte(Key)] then FKeyPressed[byte(Key)]:=TRUE;
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyMap[VK_LCONTROL] := FALSE
  else if ssShift in Shift then FKeyMap[VK_LSHIFT] := FALSE
   else if ssAlt in Shift then FKeyMap[VK_MENU] := FALSE
    else} FKeyMap[byte(Key)] := FALSE ;
end;

procedure TOGLCScene.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 case Button of
  mbLeft: MouseManager.LeftButton:=TRUE;
  mbRight: MouseManager.RightButton:=TRUE;
  mbMiddle: MouseManager.MiddleButton:=TRUE;
 end;
 MouseManager.FMousePos.x:=X;
 MouseManager.FMousePos.y:=Y;
end;

procedure TOGLCScene.ProcessMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 case Button of
  mbLeft: MouseManager.LeftButton:=FALSE;
  mbRight: MouseManager.RightButton:=FALSE;
  mbMiddle: MouseManager.MiddleButton:=FALSE;
 end;
 MouseManager.FMousePos.x:=X;
 MouseManager.FMousePos.y:=Y;
end;

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
   else FGlobalFadeColor.ChangeTo( aColor, aDurationInSecond );
end;

procedure TOGLCScene.ColorFadeOut(const aDurationInSecond: single);
begin
 if aDurationInSecond=0
   then FGlobalFadeColor.Alpha.Value := 0
   else FGlobalFadeColor.Alpha.ChangeTo( 0, aDurationInSecond );
end;

procedure TOGLCScene.ClearKeysState;
begin
 FillChar( FKeyMap, sizeof(FKeyMap), FALSE );
 FillChar( FKeyPressed, sizeof(FKeyPressed), FALSE );
end;

function TOGLCScene.CreateCamera: TOGLCCamera;
begin
 Result := TOGLCCamera.Create;
 Result.FParentScene := self;
 FCameraList.Add( Result );
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

function TOGLCScene.Add_GuiLabel(const aCaption: string; aFont: TGuiFont;
  aBackGround: TBGRABitmap; aLayer: integer): TGuiLabel;
begin
 Result := TGuiLabel.Create( aCaption, aFont, aBackGround );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiTextArea(const aText: string; aX, aY, aWidth,
  aHeight: integer; aFont: TGuiFont; aHorizAlign: TAlignment;
  aVerticalAlign: TTextLayout; aLayer: integer): TGuiTextArea;
begin
 Result := TGuiTextArea.Create( aText, aX, aY, aWidth, aHeight, aFont, aHorizAlign, aVerticalAlign );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiButton(const aCaption: string; aFont: TGuiFont;
  aBackGround: TBGRABitmap; aLayer: integer): TGuiButton;
begin
 Result := TGuiButton.Create( aCaption, aFont, aBackGround );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiButton(aImage: TBGRABitmap; aLayer: integer): TGuiButton;
begin
 Result := TGuiButton.Create( aImage );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiButton(const aFilename: string; aLayer: integer ): TGuiButton;
begin
 Result := TGuiButton.Create( aFilename );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiCheck(const aCaption: string; aFont: TGuiFont;
  aBackGround: TBGRABitmap; aLayer: integer): TGuiCheck;
begin
 Result := TGuiCheck.Create( aCaption, aFont, aBackground );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiCheck(aImage: TBGRABitmap; aLayer: integer
  ): TGuiCheck;
begin
 Result := TGuiCheck.Create( aImage );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiCheck(const aFilename: string; aLayer: integer
  ): TGuiCheck;
begin
 Result := TGuiCheck.Create( aFilename );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiRadio(const aCaption: string; aFont: TGuiFont;
  aBackGround: TBGRABitmap; aLayer: integer): TGuiRadio;
begin
 Result := TGuiRadio.Create( aCaption, aFont, aBackground );
 Add( Result, aLayer );
end;

function TOGLCScene.Add_GuiProgressBar(aX, aY: single; aWidth,
  aHeight: integer; aLayer: integer): TGuiProgressBar;
begin
 Result := TGuiProgressBar.Create( aX, aY, aWidth, aHeight );
 Add( Result, aLayer );
end;

procedure TOGLCScene.Draw;
var L, i: integer;
    o: TSimpleSurfaceWithEffect;
begin
{ if FPostProcessingShader.FFX<>[]
   then FRendererForPostProcessing.Bind;  }

 glClearColor(FBackgroundColorF[1], FBackgroundColorF[2], FBackgroundColorF[3], FBackgroundColorF[4]);
 glClear( GL_COLOR_BUFFER_BIT );

 // set camera view
 FCamera.Use;

 // delete all surface with FKill=true
 for L:=0 to LayerCount-1 do
  begin
   for i:=Layer[L].FList.Count-1 downto 0 do
    begin
     o := Layer[L].Surface[i];
     if o.FKill then begin
       Layer[L].Delete( i );
       o.Free;
     end;
    end;
  end;

 // Before paint CallBack
 if Assigned (FOnBeforePaint) then FOnBeforePaint;

 // Render all Layers
 for L:=LayerCount-1 downto 0 do Layer[L].Draw;


 // Scene global fade
 if FGlobalFadeColor.Alpha.Value > 0
   then FillBox( 0, 0, GetSceneWidth, GetSceneHeight, FGlobalFadeColor.Value );

 // Render mouse cursor
 MouseManager.Draw;

 FCamera.Release;
 // After paint CallBack
 if Assigned (FOnAfterPaint) then FOnAfterPaint;

{ if FPostProcessingShader.FFX<>[] then begin
   // draw post-processing frame buffer on screen
  FRendererForPostProcessing.Unbind;

   SetBlendMode(FX_BLEND_NORMAL);
   //DrawTexture( FRendererForPostProcessing.RenderedTexture, 0, 0, 0, 0, 255, BGRA(0,0,0,0));
  FPostProcessingShader.SetParam(FRendererForPostProcessing.RenderedTexture, 0);
  FPostProcessingShader.Use;
  glActiveTexture(GL_TEXTURE0);
  TextureManager.BindTexture( FRendererForPostProcessing.RenderedTexture );

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
 // camera update
 FCamera.Update( DT );

 // Do 'Surface change layer' request
 for i:=0 to length(FListSurfaceChangeLayer)-1 do
  with FListSurfaceChangeLayer[i] do
   begin
    Surface.FParentLayer.Delete( Surface.FIndex );
    Surface.FIndex := Layer[NewLayerIndex].Add( Surface );
    Surface.FParentLayer := Layer[NewLayerIndex];
   end;
 SetLength( FListSurfaceChangeLayer, 0 );

 // Update all layers
 for i:=LayerCount-1 downto 0 do Layer[i].Update( DT );
 // if any, update current stage
 if FCurrentStage <> NIL then FCurrentStage.Update( DT );

 MouseManager.UpDate( DT );
 FontManager.Update( DT );

 // Scene global fade
 FGlobalFadeColor.OnElapse( DT );
end;

procedure TOGLCScene.CallBackTimerFPS;
begin
 FFPS := FFPSCounter;
 FFPSCounter := 0;
end;

function TOGLCScene.GetSceneHeight: integer;
begin
 Result := FOGLC.Height;
end;

function TOGLCScene.GetSceneWidth: integer;
begin
 Result := FOGLC.Width;
end;

procedure TOGLCScene.SetBackgroundColor(aColor: TBGRAPixel);
begin
 FBackgroundColorF := ColorF( aColor.red/255, aColor.green/255, aColor.blue/255, aColor.alpha/255);
end;

function TOGLCScene.GetKeyMap(index: byte): boolean;
begin
 Result := FKeyMap[index];
end;

procedure TOGLCScene.SetKeyMap(index: byte; AValue: boolean);
begin
  FKeyMap[index]:=AValue;
end;

function TOGLCScene.GetRectArea: TRect;
begin
 Result.Create( Point(0,0), Width,  Height );
end;

procedure TOGLCScene.SetLayerCount(AValue: integer);
var i:integer ;
begin
 inherited SetLayerCount(AValue);
 for i:=0 to GetLayerCount-1 do
  begin
   Layer[i].FParentScene := Self;
  end;
end;

function TOGLCScene.GetSceneCenter: TPointF;
begin
 Result := PointF( Width*0.5, Height*0.5 );
end;

function TOGLCScene.GetKeyPressed(index: byte): boolean;
begin
 Result := FKeyPressed[index];
 FKeyPressed[index]:=FALSE;
end;

function TOGLCScene.GetBackgroundColor: TBGRAPixel;
begin
 Result := BGRA( round(FBackgroundColorF[1]*255),
                 round(FBackgroundColorF[2]*255),
                 round(FBackgroundColorF[3]*255),
                 round(FBackgroundColorF[4]*255));
end;

end.

