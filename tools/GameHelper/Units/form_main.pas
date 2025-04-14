unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, Menus, OpenGLContext, OGLCScene, u_common, Types;

{
 RIGHT MOUSE button: Move view
 LEFT MOUSE button: select item, move item
 Mouse wheel : shift view vertically
 Mouse wheel + shift : shift view horizontally
 Mouse wheel + control: zoom in/out
}

type
{  TMouseState = ( msIdle,
                  msMoveView
                );  }
  { TFormMain }

  TFormMain = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    Label3: TLabel;
    OGL: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    ToolBar1: TToolBar;
    BNewScreen: TToolButton;
    BLoadScreen: TToolButton;
    BSaveScreen: TToolButton;
    ToolButton1: TToolButton;
    BScreenOptions: TToolButton;
    procedure BLoadScreenClick(Sender: TObject);
    procedure BNewScreenClick(Sender: TObject);
    procedure BSaveScreenClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OGLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
{    FMouseState: TMouseState;
    FClickOrigin: TPoint;
    procedure DoLoopMoveView; }
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    procedure UpdateWidgets;
  end;

var
  FormMain: TFormMain;

implementation
uses u_screen_spritebuilder, u_project, u_app_pref, u_screen_template,
  u_spritebank, u_ui_handle, BGRABitmap, BGRABitmapTypes;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Project := TProject.Create;

  FScene := TOGLCScene.Create(OGL, -1);
  FScene.DesignPPI := 96;
  FScene.LayerCount := LAYER_COUNT;
  //FScene.BackgroundColor := BGRA(0,200,255);
  FScene.ScreenFadeTime := 0;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
  FreeAndNil(Project);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := Project.DoUserPromptToSaveProject;
end;

procedure TFormMain.BNewScreenClick(Sender: TObject);
begin
  Project.New;
end;

procedure TFormMain.BSaveScreenClick(Sender: TObject);
begin
  Project.Save;
  if Project.IsReady then AppPref.LastProjectFilename := Project.Filename;
end;

procedure TFormMain.BLoadScreenClick(Sender: TObject);
begin
  Project.Load;
  if Project.IsReady then AppPref.LastProjectFilename := Project.Filename;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  FScene.ProcessOnKeyDown(Key, Shift);
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  FScene.ProcessOnKeyUp(Key, Shift);
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.OGLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseDown(Button, Shift, X, Y);
end;

procedure TFormMain.OGLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'X:'+X.ToString+'  Y:'+Y.ToString;

  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseMove(Shift, X, Y);
end;

procedure TFormMain.OGLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseUp(Button, Shift, X, Y);
end;

procedure TFormMain.OGLMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (FScene = NIL) or (FScene.CurrentScreen = NIL) then exit;
  TCustomScreenTemplate(FScene.CurrentScreen).ProcessMouseWheel(Shift, WheelDelta, MousePos, Handled);
end;

procedure TFormMain.LoadCommonData;
begin
  ScreenSpriteBuilder := TScreenSpriteBuilder.Create;
  ScreenSpriteBuilder.Initialize;

  UIHandle.InitDefault;
  UIHandle.TargetLayer := LAYER_UI;
  UIHandle.CreateAtlas;

  SpriteBank := TSpriteBank.Create;

  // load last project
  if AppPref.LastProjectFilename <> ''
    then Project.Load(AppPref.LastProjectFilename);

  FScene.RunScreen(ScreenSpriteBuilder);
end;

procedure TFormMain.FreeCommonData;
begin
  UIHandle.FreeAtlas;

  SpriteBank.Free;
  SpriteBank := NIL;

  ScreenSpriteBuilder.Finalize;
  FreeAndNil(ScreenSpriteBuilder);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

procedure TFormMain.UpdateWidgets;
begin

end;


end.

