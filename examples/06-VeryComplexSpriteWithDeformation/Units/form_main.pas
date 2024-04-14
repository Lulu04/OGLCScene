unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, BGRABitmap, BGRABitmapTypes, OpenGLContext, OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
  end;

var
  FormMain: TFormMain;

implementation
uses screen_demo;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FScene := TOGLCScene.Create(OpenGLControl1, 4/3);
  FScene.DesignPPI := 96;  // this project was made with a 96ppi monitor
                           // This affect FScene.ScaleDesignToScene() method;
  FScene.LayerCount := LAYER_COUNT;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled := FALSE;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  Caption := Format('OGLCScene - Very complex sprite with deformation   -   (%d,%d) - %d FPS', [FScene.Width, FScene.Height, FScene.FPS]);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;

  if Sender = TrackBar1 then ScreenDemo.LittleRed.SetWindSpeed(TrackBar1.Position*0.01);

  if Sender = CheckBox1 then ScreenDemo.LittleRed.ToogleFlipH;

  if Sender = CheckBox2 then ScreenDemo.LittleRed.ToogleFlipV;

  if Sender = CheckBox3 then ScreenDemo.LittleRed.ShowOrigin := CheckBox3.Checked;
end;

procedure TFormMain.LoadCommonData;
begin
  // we create the single screen of our example
  ScreenDemo := TScreenDemo.Create;
  FScene.RunScreen(ScreenDemo, False);
end;

procedure TFormMain.FreeCommonData;
begin
  FreeAndNil(ScreenDemo);
end;

procedure TFormMain.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;


end.

