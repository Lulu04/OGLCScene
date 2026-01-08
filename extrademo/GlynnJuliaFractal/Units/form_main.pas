unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  Menus, StdCtrls, Spin,
  BGRABitmap, BGRABitmapTypes, OpenGLContext,
  OGLCScene, u_common;

type

  { TFormMain }

  TFormMain = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    rgColorModes: TRadioGroup;
    seAlpha: TSpinEdit;
    seAlphaExp: TSpinEdit;
    seCoeffB: TFloatSpinEdit;
    seCoeffG: TFloatSpinEdit;
    seCoeffR: TFloatSpinEdit;
    seIterations: TSpinEdit;
    seLightness: TFloatSpinEdit;
    seMinhueOffset: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure seIterationsChange(Sender: TObject);
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public

  end;

var
  FormMain: TFormMain;

implementation
uses screen_demo, u_procedural_glynnjulia, Graphics;
{$R *.lfm}


{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  rgColorModes.ItemIndex:= 0;
  FScene := TOGLCScene.Create(OpenGLControl1, -1);
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

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.seIterationsChange(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;
  // send the parameters to the renderer
  ScreenDemo.GlynnJuliaRenderer.Params.Iteration := seIterations.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.MinHueOffset := seMinhueOffset.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.ColorMode := TGlynnJuliaColorMode(rgColorModes.ItemIndex);
  ScreenDemo.GlynnJuliaRenderer.Params.Alpha := seAlpha.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.AlphaGlowSoftness := seAlphaExp.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.CoeffR := seCoeffR.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.CoeffG := seCoeffG.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.CoeffB := seCoeffB.Value;
  ScreenDemo.GlynnJuliaRenderer.Params.myLightness := seLightness.Value;
end;

procedure TFormMain.LoadCommonData;
begin
  // create and run the demo screen
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

