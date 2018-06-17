{
  ****************************************************************************
 *                                                                          *
 *  This file is part of OGLCScene library which is distributed             *
 *  under the modified LGPL.                                                *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ****************************************************************************

 written by Lulu - 2017

}
unit OGLCScene;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, Controls,
  types,
  OpenGLContext, GLExt, GL,
  lazutf8,
  BGRABitmapTypes, BGRABitmap, BGRATextFX, BGRAPath,
  VelocityCurve,
 // UTF8utils,
  math,
  strutils;


//{$DEFINE DEBUG_MODE_ON}

type

TOGLCScene = class;

// Callback for OGLCScene.OnBeforePaint and OGLCScene.OnAfterPaint
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


{$define oglcINTERFACE}
{$I oglcListType.inc }
{$I oglcMessage.inc }
{$I oglcTexture.inc }
{$I oglcTexturedFont.inc }
{$I oglcTimerDelay.inc }
{$I oglcShader.inc }
{$I oglcRenderToTexture.inc }
{$I oglcLayer.inc }
{$I oglcSurface.inc }
{$I oglcSpriteTemplate.inc }
{$I oglcGlow.inc }
{$I oglcDeformationGrid.inc }
{$I oglcSlideShow.inc }
{$I oglcTileMap.inc }
{$I oglcParticle.inc }
{$I oglcPathToDraw.inc }
{$I oglcGUI.inc }
{$I oglcUtils.inc }
{$I oglcEnvironment.inc }
{$undef oglcINTERFACE}

type

// abstract class to manage one stage of your game application
TStageSkeleton = class
 procedure LoadData; virtual; abstract;  // override to load texture, create sprite, gui object, sound, initialization, etc...
 procedure FreeData; virtual; abstract;  // override to free all that need to be freed at the end of this stage
 procedure Update( AElapsedTime: single ); virtual; abstract; // override to update your stuff according time
end;

{ TOGLCCamera }

TOGLCCamera = class
 FParentScene: TOGLCScene;
 Constructor Create;
 Destructor Destroy; override;
 procedure Update( const AElapsedTime: single );
public
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;
  procedure Reset( const aSeconds: single ); // reset camera to initial value in specified time in seconds
  procedure Use;
  procedure Release;
end;

{ TOGLCScene }
TOGLCScene = class (TLayerList)
 protected
  FOGLC: TOpenGLControl;
 private
  FOGLCOnResize : TNotifyEvent;
  FOGLCOnClick : TNotifyEvent;
  FFlagMouseLeftClicked: boolean;
  FGLInitialized : boolean;
  FKeyMap : array[0..255] of boolean; // TRUE if Key is pressed
  FCurrentStage,
  FStageRequested: TStageSkeleton;
  FIsChangingStage: boolean;
  FDoBlackScreenOnNewStage: boolean;
  procedure NewOnResize ( Sender:TObject );
  procedure NewOnClick ( Sender:TObject );
  procedure Draw;
  procedure UpDate ( const DT:single );
 protected
  FShaderGlow: TShaderID;
 private
  FTickOrigin : QWord;
  // FPS
  FFPSCounter : integer;
  FFPS        : integer;
  FExecuteDuringLoop: boolean;
  procedure CallBackTimerFPS ;
 private
  FGlobalFadeColor: TBGRAParam;
  FBackgroundColor : TBGRAPixel;
  function GetSceneHeight: integer; inline ;
  function GetSceneWidth: integer; inline ;
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
  function GetSceneCenter: TPointF;
 public

  Constructor Create ( aOGLContext: TOpenGLControl ) ;
  Destructor Destroy; override;
  function GetRectArea: TRect;
  // called from main windows
  procedure ProcessKeyDown( Key: Word; {%H-}Shift: TShiftState);
  procedure ProcessKeyUp( Key: Word; {%H-}Shift: TShiftState);
  procedure ProcessMouseDown( Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  procedure ProcessMouseUp( Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);

  // call DoLoop on Application idle process. It update and draw the scene
  procedure DoLoop;
  procedure ExecuteDuring( aTimeInSecond:single );

  procedure LaunchStage( AStage: TStageSkeleton; DoBlackScreen: boolean=TRUE );
  property CurrentStage: TStageSkeleton read FCurrentStage;

  // Surface
  procedure Add ( aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  procedure Insert ( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  function GetSurfaceByIndex( aLayerIndex, aSurfaceIndex: integer): TSimpleSurface;

  // Global scene color fade in and out
  procedure ColorFadeIn ( const aColor : TBGRAPixel ; const aDurationInSecond : single );
  procedure ColorFadeOut ( const aDurationInSecond : single );

  property Width: integer read GetSceneWidth;
  property Height: integer read GetSceneHeight;
  property Center: TPointF read GetSceneCenter;
  property FPS: integer read FFPS;
  property BackgroundColor: TBGRAPixel read FBackgroundColor write SetBackgroundColor ;
  // Callback
  property OnBeforePaint: TOGLCEvent read FOnBeforePaint write FOnBeforePaint;
  property OnAfterPaint: TOGLCEvent read FOnAfterPaint write FOnAfterPaint;
  // use this callback to load your global ressources
  property OnLoadCommonData: TOGLCEvent read FOnLoadCommonData write FOnLoadCommonData;
  // use this callback to free them
  property OnFreeCommonData: TOGLCEvent read FOnFreeCommonData write FOnFreeCommonData;
 public
  procedure ClearKeysState;
  property Key[index:byte]: boolean read GetKeyMap ;

  property Camera: TOGLCCamera read FCamera;
 public
  // convenience functions
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

var

OpenGL_Version_2_0_Loaded: boolean = FALSE;

{ TOGLCCamera }

constructor TOGLCCamera.Create;
begin
  LookAt := TPointFParam.Create;
  Scale := TPointFParam.Create;
  Scale.Value := PointF(1,1);
  Angle := TFParam.Create;
end;

destructor TOGLCCamera.Destroy;
begin
 FreeAndNil( LookAt );
 FreeAndNil( Scale );
 FreeAndNil( Angle );
 inherited Destroy;
end;

procedure TOGLCCamera.Update(const AElapsedTime: single);
begin
 LookAt.OnElapse( AElapsedTime );
 Scale.OnElapse( AElapsedTime );
 Angle.OnElapse( AElapsedTime );
end;

procedure TOGLCCamera.Reset(const aSeconds: single);
begin
 LookAt.ChangeTo( PointF(0,0), aSeconds );
 Scale.ChangeTo( PointF(1,1), aSeconds );
 Angle.ChangeTo( 0, aSeconds );
end;

procedure TOGLCCamera.Use;
begin
 glPushMatrix;
 glTranslatef( FParentScene.Width * 0.5, FParentScene.Height * 0.5 , 0 );
 //glTranslatef( X.Value + Width * 0.5 * Scale.x.Value, Y.Value + Height * 0.5 * Scale.y.Value, 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -FParentScene.Width * 0.5 + LookAt.x.Value, -FParentScene.Height * 0.5 + LookAt.y.Value, 0 );
end;

procedure TOGLCCamera.Release;
begin
 glPopMatrix;
end;

{$define oglcIMPLEMENTATION}
{$I oglcListType.inc }
{$I oglcMessage.inc }
{$I oglcTexture.inc }
{$I oglcTexturedFont.inc }
{$I oglcTimerDelay.inc }
{$I oglcShader.inc }
{$I oglcRenderToTexture.inc }
{$I oglcLayer.inc }
{$I oglcSurface.inc }
{$I oglcSpriteTemplate.inc }
{$I oglcGlow.inc }
{$I oglcDeformationGrid.inc }
{$I oglcSlideShow.inc }
{$I oglcTileMap.inc }
{$I oglcParticle.inc }
{$I oglcPathToDraw.inc }
{$I oglcUtils.inc }
{$I oglcGUI.inc }
{$I oglcEnvironment.inc }
{$undef oglcIMPLEMENTATION}

{ TOGLCScene }

constructor TOGLCScene.Create(aOGLContext: TOpenGLControl);
begin
 inherited Create;

 // fix decimal separator to dot to avoid exception when converting floating point values from scenario's files
 SysUtils.FormatSettings.DecimalSeparator := '.';

 FGLInitialized := FALSE;
 FOGLC := aOGLContext;

 FOGLCOnResize := FOGLC.OnResize;
 FOGLC.OnResize := @NewOnResize;

 FOGLCOnClick := FOGLC.OnClick;
 FOGLC.OnClick := @NewOnClick;
 FFlagMouseLeftClicked:=FALSE;

 ClearKeysState;

 FBackgroundColor := BGRABlack;

 FOnBeforePaint := NIL;
 FOnAfterPaint := NIL;
 FOnLoadCommonData := NIL;
 FOnFreeCommonData := NIL;
 FCommonDataLoaded := FALSE;

 FGlobalFadeColor:= TBGRAParam.Create;
 FGlobalFadeColor.Value := BGRA(0,0,0,0);

 TextureManager := TTextureManager.Create;

 TimerManager := TTimerManager.Create;
 TimerManager.Add ( @CallBackTimerFPS, 1000 );

 DelayManager := TDelayManager.Create;

 MouseManager := TMouseManager.Create;
 MouseManager.FParentScene := Self;

 FontManager := TFontManager.Create;

 FExecuteDuringLoop := FALSE;
 FIsChangingStage := FALSE;

 FCurrentStage := NIL;
 FStageRequested := NIL;

 FCamera := TOGLCCamera.Create;
 FCamera.FParentScene := self;
end;

destructor TOGLCScene.Destroy;
begin
 if FCurrentStage <> NIL then FCurrentStage.FreeData;
 FCurrentStage := NIL;

 if FOnFreeCommonData <> NIL then FOnFreeCommonData;
 FCommonDataLoaded := FALSE;

 FreeAndNil( FGlobalFadeColor );

 SetLayerCount(0);
 FontManager.Free;
 TimerManager.Free;
 DelayManager.Free;
 MouseManager.Free;
 TextureManager.Free;
 if ShaderManager <> NIL then FreeAndNil( ShaderManager );

 FreeAndNil( FCamera );

 inherited Destroy;
end;

procedure TOGLCScene.DoLoop;
var t: QWord;
begin
 if not FOGLC.MakeCurrent() then exit;

 if not FGLInitialized then begin

   SetBlendMode( FX_BLEND_NORMAL ) ;
   glEnable(GL_POLYGON_SMOOTH or GL_LINE_SMOOTH);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glViewport(0, 0, GetSceneWidth, GetSceneHeight );
   FGLInitialized := TRUE;
   SetBackgroundColor( FBackgroundColor );

   OpenGL_Version_2_0_Loaded := Load_GL_version_2_0;
   if not OpenGL_Version_2_0_Loaded
    then Showmessage('Cannot load OpenGL 2.0, shader not available...');

   if ShaderManager = NIL
     then begin
           ShaderManager := TShaderManager.Create;
           FShaderGlow := ShaderManager.CreateShader( '', GLOW_FRAGMENT_SHADER_PRG );
     end;

   FTickOrigin := GetTickCount64;
 end;

 if not FCommonDataLoaded and ( OnLoadCommonData <> NIL ) then begin
   FOnLoadCommonData;
   FCommonDataLoaded := TRUE;
 end;

 TimerManager.ProcessTimer;

 t := GetTickCount64;
 while GetTickCount64 - FTickOrigin < 16 do
  begin
   sleep(1);
   Application.ProcessMessages;
  end;
 UpDate( ( t - FTickOrigin ) * 0.001 );
 FTickOrigin := t;

 Draw;
 FOGLC.SwapBuffers;
 inc( FFPSCounter );

 FFlagMouseLeftClicked := FALSE ;

 // if needed, go to the next stage
 if (FStageRequested <> NIL) and not FExecuteDuringLoop and not FIsChangingStage then
 begin
   FIsChangingStage := TRUE;
   if FDoBlackScreenOnNewStage then
   begin
     ColorFadeIn( BGRABlack, 1.0 );
     ExecuteDuring( 1.0 );
   end;
   if FCurrentStage <> NIL then FCurrentStage.FreeData;
   FCurrentStage := FStageRequested;
   FStageRequested := NIL;
   FCurrentStage.LoadData;
   ClearKeysState;
   if FDoBlackScreenOnNewStage
     then begin
       ColorFadeOut( 1.0 );
       ExecuteDuring( 1.0 );
     end;
   FIsChangingStage := FALSE;
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

procedure TOGLCScene.LaunchStage(AStage: TStageSkeleton; DoBlackScreen: boolean=TRUE);
begin
 if FIsChangingStage then exit;
 FStageRequested := AStage;
 FDoBlackScreenOnNewStage := DoBlackScreen;
end;


procedure TOGLCScene.NewOnResize(Sender: TObject);
begin
 if FOGLCOnResize <> NIL then FOGLCOnResize( Sender );
end;

procedure TOGLCScene.NewOnClick(Sender: TObject);
begin
 FFlagMouseLeftClicked := TRUE;
 if FOGLCOnClick <> NIL then FOGLCOnClick( Sender );
end;

procedure TOGLCScene.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyMap[VK_LCONTROL] := TRUE
  else if ssShift in Shift then FKeyMap[VK_LSHIFT] := TRUE
   else if ssAlt in Shift then FKeyMap[VK_MENU] := TRUE
    else} FKeyMap[byte(Key)] := TRUE ;
end;

procedure TOGLCScene.ProcessKeyUp(Key: Word; Shift: TShiftState);
begin
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

procedure TOGLCScene.Add( aSurface : TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
 AddSurfaceToLayer(aSurface, aLayerIndex );
 aSurface.SetParentLayer( Layer[aLayerIndex] );
 aSurface.SetParentScene( Self );
end;

procedure TOGLCScene.Insert( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer) ;
begin
 InsertSurfaceToLayer( aSurfaceIndex, aSurface, aLayerIndex);
 aSurface.SetParentLayer( Layer[aLayerIndex] );
 aSurface.SetParentScene( Self );
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
 FillChar( FKeyMap, sizeof(FKeyMap), FALSE ) ;
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
 // set camera view
 FCamera.Use;

 glClear( GL_COLOR_BUFFER_BIT );

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

 // After paint CallBack
 if Assigned (FOnAfterPaint) then FOnAfterPaint;

 // Scene global fade
 if FGlobalFadeColor.Alpha.Value > 0
   then FillBox( 0, 0, GetSceneWidth, GetSceneHeight, FGlobalFadeColor.Value );

 // Render mouse cursor
 MouseManager.Draw;

// glDisable( GL_BLEND );

 FCamera.Release;
end;

procedure TOGLCScene.UpDate(const DT: single);
var i: integer;
begin
 // camera update
 FCamera.Update( DT );

 // Mouse update
// if FMouseManager.FSprite <> NIL
//   then FMouseManager.FSprite.Update( DT );

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
 Result := FOGLC.Width ;
end;

procedure TOGLCScene.SetBackgroundColor(aColor: TBGRAPixel);
begin
 FBackgroundColor := aColor;
 glClearColor(FBackgroundColor.red/255, FBackgroundColor.green/255, FBackgroundColor.blue/255, FBackgroundColor.alpha/255);
end;

function TOGLCScene.GetKeyMap(index: byte): boolean;
begin
 Result := FKeyMap[index];
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


end.

