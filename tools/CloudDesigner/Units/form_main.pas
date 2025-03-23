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
    CBCloudsColor1: TColorButton;
    CBAddRelief: TCheckBox;
    FSEHTranslation: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label33: TLabel;
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
    TBCloudsChange: TTrackBar;
    TBCloudsFragmentation: TTrackBar;
    TBFadeRight: TTrackBar;
    TBFadeLeft: TTrackBar;
    TBFadeTop: TTrackBar;
    TBCloudsOpacity: TTrackBar;
    TBDensity: TTrackBar;
    TBFadeBottom: TTrackBar;
    procedure BExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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

const TITLE_PREFIX = 'OGLCScene - Clouds designer';

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
  FPresetManager.Init1('Cloud presets', BPreset, Application.Location+'Cloud.preset');
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

procedure TFormMain.OpenGLControl1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var w, m: integer;
begin
  if ScreenDemo = NIL then exit;

  // changes the planet's size according to the mouse wheel
  w := ScreenDemo.Clouds.Width;
  if WheelDelta > 0 then begin
    m := Min(FScene.Width, FScene.Height);
    w := Round(w + w/4);
    if w > m then w := m;
  end else begin
    m := Min(FScene.Width, FScene.Height) div 20;
    w := Round(w - w/4);
    if w < m then w := m;
  end;
  ScreenDemo.Clouds.SetSize(w, w);
  ScreenDemo.Clouds.CenterOnScene;

  Handled := True;
end;

procedure TFormMain.TBCloudsFragmentationChange(Sender: TObject);
begin
  if ScreenDemo = NIL then exit;

  // update values in labels
  Label5.Caption := TBCloudsOpacity.Position.ToString;
  Label2.Caption := FormatFloat('0.0', TBCloudsFragmentation.Position*0.1);
  Label26.Caption := FormatFloat('0.00', TBCloudsChange.Position/TBCloudsChange.Max);
  Label9.Caption := FormatFloat('0.0', TBFadeTop.Position/TBFadeTop.Max);
  Label11.Caption := FormatFloat('0.0', TBFadeBottom.Position/TBFadeBottom.Max);
  Label13.Caption := FormatFloat('0.0', TBFadeRight.Position/TBFadeRight.Max);
  Label15.Caption := FormatFloat('0.0', TBFadeLeft.Position/TBFadeLeft.Max);

  if FLoadingPreset then exit;

  ScreenDemo.Clouds.Color := ColorToBGRA(CBCloudsColor1.ButtonColor);
  ScreenDemo.Clouds.Fragmentation := TBCloudsFragmentation.Position*0.1;
  ScreenDemo.Clouds.Transformation := TBCloudsChange.Position/TBCloudsChange.Max;
  ScreenDemo.Clouds.Density := TBDensity.Position/TBDensity.Max;
  ScreenDemo.Clouds.Relief := CBAddRelief.Checked;
  ScreenDemo.Clouds.FadeTop := TBFadeTop.Position/TBFadeTop.Max;
  ScreenDemo.Clouds.FadeBottom := TBFadeBottom.Position/TBFadeBottom.Max;
  ScreenDemo.Clouds.FadeRight := TBFadeRight.Position/TBFadeRight.Max;
  ScreenDemo.Clouds.FadeLeft := TBFadeLeft.Position/TBFadeLeft.Max;

  ScreenDemo.Clouds.TranslationSpeed := FSEHTranslation.Value;
  ScreenDemo.Clouds.Opacity.Value := TBCloudsOpacity.Position;
end;

procedure TFormMain.PresetToWidget(const A: TStringArray);
begin
  ScreenDemo.Clouds.LoadParamsFromString(A[0]);

  FLoadingPreset := True;

  CBCloudsColor1.ButtonColor := ScreenDemo.Clouds.Color.ToColor;
  TBCloudsFragmentation.Position := Round(ScreenDemo.Clouds.Fragmentation*10);
  TBCloudsChange.Position := Round(ScreenDemo.Clouds.Transformation*TBCloudsChange.Max);
  TBDensity.Position := Round(ScreenDemo.Clouds.Density*TBDensity.Max);
  CBAddRelief.Checked := ScreenDemo.Clouds.Relief;
  FSEHTranslation.Value := ScreenDemo.Clouds.TranslationSpeed;
  TBFadeTop.Position := Round(ScreenDemo.Clouds.FadeTop*TBFadeTop.Max);
  TBFadeBottom.Position := Round(ScreenDemo.Clouds.FadeBottom*TBFadeBottom.Max);
  TBFadeRight.Position := Round(ScreenDemo.Clouds.FadeRight*TBFadeRight.Max);
  TBFadeLeft.Position := Round(ScreenDemo.Clouds.FadeLeft*TBFadeLeft.Max);

  FLoadingPreset := False;
end;

function TFormMain.WidgetToPreset: string;
begin
  Result := ScreenDemo.Clouds.SaveParamsToString;
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

