unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, StdCtrls,
  ComCtrls, BGRABitmap, BGRABitmapTypes, OpenGLContext, OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
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
uses screen_demo, LazFileUtils;
{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  fontDirectory: String;
begin
  FScene := TOGLCScene.Create(OpenGLControl1, 4/3);
  FScene.DesignPPI := 96;  // this project was made with a 96ppi monitor
                           // This affect FScene.ScaleDesignToScene() method;
  FScene.LayerCount := LAYER_COUNT;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  // scan the font provided with the demo
  fontDirectory := CleanAndExpandDirectory(Application.Location+'..'+PathDelim+'Data'+PathDelim+'Fonts'+PathDelim);
  FScene.FontManager.ScanProjectFont(fontDirectory);

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

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FScene.OpenGLLibLoaded then
    ShowMessage('ERROR: OpenGL library could not be loaded...'+LineEnding+
                'Check if your system is compatible with OpenGL 3.3 core'+LineEnding+
                'and if the library is well installed on your computer');
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  Caption := Format('OGLCScene - Sprite and Text on Path   -   (%d,%d) - %d FPS', [FScene.Width, FScene.Height, FScene.FPS]);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;

  if Sender = TrackBar1 then
    ScreenDemo.SpriteOnPath.CoeffPositionOnPath := TrackBar1.Position*0.01;

  if Sender = TrackBar2 then
    ScreenDemo.TextOnPath.CoeffPositionOnPath := TrackBar2.Position*0.01;

  if Sender = CheckBox1 then begin
    ScreenDemo.SpriteOnPath.AutoRotate := CheckBox1.Checked;
    if not CheckBox1.Checked then ScreenDemo.SpriteOnPath.Angle.Value := 0;
  end;

  if Sender = CheckBox2 then begin
    ScreenDemo.TextOnPath.AutoRotate := CheckBox2.Checked;
    if not CheckBox2.Checked then ScreenDemo.TextOnPath.Angle.Value := 0;
  end;
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

