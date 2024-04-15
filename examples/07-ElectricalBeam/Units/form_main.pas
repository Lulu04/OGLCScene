unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, StdCtrls,
  ComCtrls, BGRABitmap, BGRABitmapTypes, OpenGLContext, OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
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
  FScene := TOGLCScene.Create(OpenGLControl1, 16/9);
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
  Caption := Format('OGLCScene - Electrical Beam   -   (%d,%d) - %d FPS', [FScene.Width, FScene.Height, FScene.FPS]);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
var c: TBGRAPixel;
begin
  if Sender = TrackBar5 then begin
    Label7.Caption := 'RefreshTime '+FormatFloat('0.00', TrackBar5.Position*0.01)+'s';
    ScreenDemo.Beam.RefreshTime := TrackBar5.Position*0.01;
  end;

  if Sender = TrackBar1 then begin
    Label1.Caption := 'Aperture '+TrackBar1.Position.ToString+'°';
    ScreenDemo.Beam.Aperture := TrackBar1.Position;
  end;

  if Sender = TrackBar2 then begin
    Label2.Caption := 'BeamWidth '+TrackBar2.Position.ToString+'px';
    ScreenDemo.Beam.BeamWidth := TrackBar2.Position;
  end;

  if Sender = ColorButton1 then begin
    ScreenDemo.Beam.BeamColor.ChangeTo(ColorToBGRA(ColorButton1.ButtonColor), 2);
  end;

  if Sender = TrackBar3 then begin
    Label4.Caption := 'HaloWidth '+TrackBar3.Position.ToString+'px';
    ScreenDemo.Beam.HaloWidth := TrackBar3.Position;
  end;

  if (Sender = ColorButton2) or (Sender = TrackBar4) then begin
    Label6.Caption := 'HaloColor.Alpha '+TrackBar4.Position.ToString;
    c := ColorToBGRA(ColorButton2.ButtonColor);
    c.alpha := TrackBar4.Position;
    ScreenDemo.Beam.HaloColor.Value := c;
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

