unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, OpenGLContext, OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    Label3: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
    procedure FormResize(Sender: TObject);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    procedure ResizeScene;
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
uses u_screen_title, u_project, u_app_pref, form_tools, BGRABitmap,
  BGRABitmapTypes, Math;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Project := TProject.Create;

  FScene := TOGLCScene.Create(OpenGLControl1, 4/3);
  FScene.DesignPPI := 96;
  FScene.LayerCount := LAYER_COUNT;
  FScene.BackgroundColor := BGRA(0,200,255);
  FScene.ScreenFadeTime := 0;

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
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  ResizeScene;
end;

procedure TFormMain.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Label1.Caption := 'X:'+X.ToString+'  Y:'+Y.ToString;
end;

procedure TFormMain.ResizeScene;
var v: integer;
begin

  if ClientWidth < ClientHeight then begin
    v := ClientWidth;     // h/w
    Panel4.Width := Round(ClientWidth / Project.SceneProportion);
    Panel4.Height := ClientWidth;
  end else begin
    v := ClientHeight-Panel1.Height;
    Panel4.Width := Round(v / Project.SceneProportion);
    Panel4.Height := v; // Round(ClientHeight * Project.SceneProportion);
  end;
end;

procedure TFormMain.LoadCommonData;
begin
  ScreenTitle := TScreenTitle.Create;
  FScene.RunScreen(ScreenTitle);

  if AppPref.LastProjectFilename <> '' then Project.Load(AppPref.LastProjectFilename);
end;

procedure TFormMain.FreeCommonData;
begin
  // free common data here
  FreeAndNil(ScreenTitle);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

procedure TFormMain.UpdateWidgets;
begin
  // current screen
  Label3.Caption := FormTools.LBScreen.GetSelectedText;
end;


end.

