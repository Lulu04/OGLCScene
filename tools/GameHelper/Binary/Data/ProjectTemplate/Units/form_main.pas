unit form_main;

{$mode objfpc}{$H+}
{$I project_config.cfg}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, OpenGLContext, LCLType;

type

  { TFormMain }

  TFormMain = class(TForm)
    OpenGLControl1: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public

  end;

var
  FormMain: TFormMain;

implementation
uses OGLCScene, u_common, screen_1;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef MAXIMIZE_SCENE_ON_MONITOR}
  FScene := TOGLCScene.Create(OpenGLControl1, SCREEN_WIDTH_AT_DESIGN_TIME/SCREEN_HEIGHT_AT_DESIGN_TIME);
 {$ifdef WINDOWED_MODE}
  WindowState := wsMaximized;
 {$else}
  WindowState := wsFullScreen;
 {$endif}
{$else}
  ClientWidth := SCREEN_WIDTH_AT_DESIGN_TIME;
  ClientHeight := SCREEN_HEIGHT_AT_DESIGN_TIME;
  FScene := TOGLCScene.Create(OpenGLControl1, -1);
  WindowState := wsNormal;
{$endif}
  FScene.DesignPPI := SCREEN_PPI_AT_DESIGN_TIME;
  FScene.LayerCount := LAYER_COUNT;
  FScene.ScreenFadeTime := 0.5;
  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  FScene.ProcessOnUTF8KeyPress(UTF8Key);
end;

procedure TFormMain.LoadCommonData;
begin
  FScene.CreateLogFile(Application.Location+'scene.log', True, NIL, NIL);

  // create your game screens here
  Screen1 := TScreen1.Create;

  FScene.RunScreen(Screen1);
end;

procedure TFormMain.FreeCommonData;
begin
  // free your game screens here
  FreeAndNil(Screen1);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

end.

