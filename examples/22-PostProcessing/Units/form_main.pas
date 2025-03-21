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
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    BStartShockWave: TSpeedButton;
    TBThreshold: TTrackBar;
    TBIntensity: TTrackBar;
    Timer1: TTimer;
    TBPixelize: TTrackBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
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
  FScene := TOGLCScene.Create(OpenGLControl1, -1);
  FScene.DesignPPI := 96;  // this project was made with a 96ppi monitor
                           // This affect FScene.ScaleDesignToScene() method;
  FScene.LayerCount := LAYER_COUNT;
  FScene.CreateLogFile(Application.Location+'scene.log', True);

  FScene.OnLoadCommonData := @LoadCommonData;
  FScene.OnFreeCommonData := @FreeCommonData;

  Application.OnIdle := @ProcessApplicationIdle;

  BStartShockWave.Caption := 'Start ShockWave effect'+LineEnding+'on layer stars';
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

procedure TFormMain.CheckBox1Change(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;

  if Sender = CheckBox1 then begin
    TBPixelize.Enabled := CheckBox1.Checked;
    Label3.Enabled := CheckBox1.Checked;
    if CheckBox1.Checked
      then FScene.Layer[LAYER_SHIP].PostProcessing.Enable([ppPixelize])
      else FScene.Layer[LAYER_SHIP].PostProcessing.Disable([ppPixelize]);
  end;

  if Sender = TBPixelize then begin
    FScene.Layer[LAYER_SHIP].PostProcessing.SetPixelizeParams(TBPixelize.Position*0.01);
  end;

  if Sender = CheckBox2 then
    if CheckBox2.Checked
      then begin
        FScene.PostProcessing.EnableFXOnAllLayers([ppDreamVision])
      end else FScene.PostProcessing.DisableFXOnAllLayers([ppDreamVision]);

  if Sender = CheckBox3 then begin
    TBThreshold.Enabled := CheckBox3.Checked;
    TBIntensity.Enabled := CheckBox3.Checked;
    Label4.Enabled := CheckBox3.Checked;
    Label5.Enabled := CheckBox3.Checked;
    if CheckBox3.Checked
      then FScene.PostProcessing.EnableFXOnAllLayers([ppBloom])
      else FScene.PostProcessing.DisableFXOnAllLayers([ppBloom]);
  end;

  if (Sender = TBThreshold) or (Sender = TBIntensity) then begin
    Label4.Caption := 'threshold '+FormatFloat('0.0', TBThreshold.Position*0.01);
    Label5.Caption := 'intensity '+FormatFloat('0.0', TBIntensity.Position*0.01);
    FScene.PostProcessing.SetBloomParamsOnAllLayers(TBThreshold.Position*0.01, TBIntensity.Position*0.01);
  end;

  if Sender = BStartShockWave then begin
    // because the ShockWave effect need time counted from zero -> we have to reset the shader time variable.
    FScene.PostProcessing.ResetTimeVariable;
    // enable (start) the ShockWave effect on the stars layer.
    FScene.PostProcessing.EnableFXOnLayers([ppShockWave], [LAYER_STARS]);
    // or
    //FScene.Layer[LAYER_STARS].PostProcessing.Enable([pfShockWave]);
  end;
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
  Caption := Format('OGLCScene - Scene layers   -   (%d,%d) - %d FPS  -  %d sprites',
  [FScene.Width, FScene.Height, FScene.FPS, FScene.SurfaceCount]);
end;

procedure TFormMain.LoadCommonData;
begin
  // we create and run the single screen of our example
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

