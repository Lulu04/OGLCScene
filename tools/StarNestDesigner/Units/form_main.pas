unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, Menus, Spin,
  BGRABitmap, BGRABitmapTypes, OpenGLContext,
  OGLCScene, u_common,
  u_export_to_string, u_presetmanager, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    CBFlipV: TCheckBox;
    FSEOpacityThreshold: TFloatSpinEdit;
    FSEBrightness: TFloatSpinEdit;
    FSEDarkmatter: TFloatSpinEdit;
    FSEDistFading: TFloatSpinEdit;
    FSESaturation: TFloatSpinEdit;
    FSEStepSize: TFloatSpinEdit;
    FSEZoom: TFloatSpinEdit;
    FSETile: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label25: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel2: TPanel;
    BExport: TSpeedButton;
    BPreset: TSpeedButton;
    BImportFromForum: TSpeedButton;
    ScrollBox1: TScrollBox;
    SEVolStep: TSpinEdit;
    SEIteration: TSpinEdit;
    TBGlobalOpacity: TTrackBar;
    TBSpeed: TTrackBar;
    TBAngle: TTrackBar;
    procedure BExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TBCloudsFragmentationChange(Sender: TObject);
  private
    FPresetManager: TPresetManager;
    FLoadingPreset: boolean;
    FLastSelectedPresetName: string;
    procedure PresetToWidget(const A: TStringArray);
    function WidgetToPreset: string;
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
  public
    procedure SetPresetNameOnTileForm(const aName: string);
    function GetLastSelectedPresetNameWithoutSpace: string;
    function GetLastSelectedPresetNameWithSpace: string;
    property PresetManager: TPresetManager read FPresetManager;
  end;

var
  FormMain: TFormMain;

implementation
uses screen_demo, form_importfromforum, Graphics, Clipbrd, Math;
{$R *.lfm}

const TITLE_PREFIX = 'OGLCScene - StarNest designer';

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

  FPresetManager := TPresetManager.Create(Self);
  FPresetManager.Init1('Cloud presets', BPreset, Application.Location+'StarNest.preset');
  FPresetManager.Init2(@PresetToWidget, @WidgetToPreset);

  Caption := TITLE_PREFIX;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FScene.Free;
  FScene := NIL;
end;

procedure TFormMain.BExportClick(Sender: TObject);
begin
  if Sender = BExport then
    FormCopyToClipboard.ShowModal;

  if Sender = BImportFromForum then
    FormImportPreset.ShowModal;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TFormMain.TBCloudsFragmentationChange(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;

  Label6.Caption := FormatFloat('0.00', TBSpeed.Position*0.01);
  Label13.Caption := TBAngle.Position.ToString+'°';

  if FLoadingPreset then exit;

  ScreenDemo.Stars.Params^.VolSteps := SEVolStep.Value;
  ScreenDemo.Stars.Params^.Iterations := SEIteration.Value;
  ScreenDemo.Stars.Params^.StepSize := FSEStepSize.Value;
  ScreenDemo.Stars.Params^.Zoom := FSEZoom.Value;
  ScreenDemo.Stars.Params^.Tile := FSETile.Value;
  ScreenDemo.Stars.Params^.Brightness := FSEBrightness.Value;
  ScreenDemo.Stars.Params^.Darkmatter := FSEDarkmatter.Value;
  ScreenDemo.Stars.Params^.DistFading := FSEDistFading.Value;
  ScreenDemo.Stars.Params^.Saturation := FSESaturation.Value;

  ScreenDemo.Stars.Opacity.Value := TBGlobalOpacity.Position;
  ScreenDemo.Stars.ScrollingSpeed.Value := TBSpeed.Position*0.01;
  ScreenDemo.Stars.ScrollingAngle.Value := TBAngle.Position;
  ScreenDemo.Stars.OpacityThreshold := FSEOpacityThreshold.Value;
  ScreenDemo.Stars.FlipV := CBFlipV.Checked;
end;

procedure TFormMain.PresetToWidget(const A: TStringArray);
begin
  ScreenDemo.Stars.Params^.LoadParamsFromString(A[0]);

  FLoadingPreset := True;

  SEIteration.Value := ScreenDemo.Stars.Params^.Iterations;
  SEVolStep.Value := ScreenDemo.Stars.Params^.VolSteps;
  FSEStepSize.Value := ScreenDemo.Stars.Params^.StepSize;
  FSEZoom.Value := ScreenDemo.Stars.Params^.Zoom;
  FSETile.Value := ScreenDemo.Stars.Params^.Tile;
  FSEBrightness.Value := ScreenDemo.Stars.Params^.Brightness;
  FSEDarkmatter.Value := ScreenDemo.Stars.Params^.Darkmatter;
  FSEDistFading.Value := ScreenDemo.Stars.Params^.DistFading;
  FSESaturation.Value := ScreenDemo.Stars.Params^.Saturation;

  FLoadingPreset := False;
end;

function TFormMain.WidgetToPreset: string;
begin
  Result := ScreenDemo.Stars.Params^.SaveParamsToString;
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

procedure TFormMain.SetPresetNameOnTileForm(const aName: string);
begin
  Caption := TITLE_PREFIX + ' - ' + aName;
  FLastSelectedPresetName := aName;
end;

function TFormMain.GetLastSelectedPresetNameWithoutSpace: string;
begin
  Result := FLastSelectedPresetName.Replace(' ', '_');
end;

function TFormMain.GetLastSelectedPresetNameWithSpace: string;
begin
  Result := FLastSelectedPresetName;
end;


end.

