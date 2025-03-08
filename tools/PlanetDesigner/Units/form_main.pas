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
    BSwapCloudsColor1: TSpeedButton;
    CBCloudsColor1: TColorButton;
    CBCloudsColor2: TColorButton;
    CBColorGround1: TColorButton;
    CBColorGround2: TColorButton;
    CBColorHalo: TColorButton;
    CBColorOcean: TColorButton;
    FSERotationSpeed: TFloatSpinEdit;
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
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel2: TPanel;
    BSwapGroundColor: TSpeedButton;
    RBCloudsBlendAdd: TRadioButton;
    RadioButton2: TRadioButton;
    BExport: TSpeedButton;
    BPreset: TSpeedButton;
    BImportFromForum: TSpeedButton;
    BDefaultRotationSpeed: TSpeedButton;
    ScrollBox1: TScrollBox;
    TBCloudsColDistrib: TTrackBar;
    TBCloudsChange: TTrackBar;
    TBGroundAmount: TTrackBar;
    TBGroundColDistrib: TTrackBar;
    TBHaloSize: TTrackBar;
    TBShadowAmount: TTrackBar;
    TBCloudsFragmentation: TTrackBar;
    TBCloudsOpacity: TTrackBar;
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

const TITLE_PREFIX = 'OGLCScene - Planet designer';

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
  FPresetManager.Init1('Planet presets', BPreset, Application.Location+'Planet.preset');
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
  w := ScreenDemo.Planet.Width;
  if WheelDelta > 0 then begin
    m := Min(FScene.Width, FScene.Height);
    w := Round(w + w/4);
    if w > m then w := m;
  end else begin
    m := Min(FScene.Width, FScene.Height) div 20;
    w := Round(w - w/4);
    if w < m then w := m;
  end;
  ScreenDemo.Planet.SetSize(w, w);
  ScreenDemo.Planet.CenterOnScene;

  Handled := True;
end;

procedure TFormMain.TBCloudsFragmentationChange(Sender: TObject);
var vf: single;
  col: TColor;
begin
  if ScreenDemo = NIL then exit;

  // update values in labels
  Label17.Caption := FormatFloat('0.00', TBGroundColDistrib.Position*0.01);
  Label24.Caption := FormatFloat('0.00', TBGroundAmount.Position/TBGroundAmount.Max);
  Label32.Caption := FormatFloat('0.00', TBCloudsColDistrib.Position/TBCloudsColDistrib.Max);
  Label5.Caption := TBCloudsOpacity.Position.ToString;
  Label2.Caption := FormatFloat('0.0', TBCloudsFragmentation.Position*0.1);
  Label26.Caption := FormatFloat('0.00', TBCloudsChange.Position/TBCloudsChange.Max);
  Label13.Caption := FormatFloat('0.00', 1.0 - (TBHaloSize.Position/TBHaloSize.Max));
  Label15.Caption := FormatFloat('0.00', TBShadowAmount.Position/TBShadowAmount.Max);


  if FLoadingPreset then exit;

  if (Sender = CBColorGround1) or (Sender = CBColorGround2) or
     (Sender = TBGroundColDistrib) or (Sender = TBGroundAmount) then begin
    ScreenDemo.Planet.ColorGround1 := ColorToBGRA(CBColorGround1.ButtonColor);
    ScreenDemo.Planet.ColorGround2 := ColorToBGRA(CBColorGround2.ButtonColor);
    ScreenDemo.Planet.GroundColorDistribution := TBGroundColDistrib.Position*0.01;

    ScreenDemo.Planet.GroundAmount := TBGroundAmount.Position/TBGroundAmount.Max;
  end;
  if Sender = BSwapGroundColor then begin
    col := CBColorGround1.ButtonColor;
    CBColorGround1.ButtonColor := CBColorGround2.ButtonColor;
    CBColorGround2.ButtonColor := col;
  end;

  if Sender = CBColorOcean then
    ScreenDemo.Planet.ColorOcean := ColorToBGRA(CBColorOcean.ButtonColor);

  if (Sender = CBCloudsColor1) or (Sender = CBCloudsColor2) or
     (Sender = TBCloudsColDistrib) or (Sender = TBCloudsOpacity) or
     (Sender = TBCloudsFragmentation) or (Sender = TBCloudsChange) or
     (Sender = RBCloudsBlendAdd) then begin
    ScreenDemo.Planet.ColorClouds1 := ColorToBGRA(CBCloudsColor1.ButtonColor);
    ScreenDemo.Planet.ColorClouds2 := ColorToBGRA(CBCloudsColor2.ButtonColor);

    ScreenDemo.Planet.CloudsColorDistribution := TBCloudsColDistrib.Position/TBCloudsColDistrib.Max;

    ScreenDemo.Planet.CloudsOpacity := TBCloudsOpacity.Position;

    ScreenDemo.Planet.CloudsFragmentation := TBCloudsFragmentation.Position*0.1;

    ScreenDemo.Planet.CloudsTransformation := TBCloudsChange.Position/TBCloudsChange.Max;

    if RBCloudsBlendAdd.Checked then vf := 1.0
      else vf := -1.0;
    ScreenDemo.Planet.CloudsBlendMode := vf;
  end;
  if Sender = BSwapCloudsColor1 then begin
    col := CBCloudsColor1.ButtonColor;
    CBCloudsColor1.ButtonColor := CBCloudsColor2.ButtonColor;
    CBCloudsColor2.ButtonColor := col;
  end;

  if (Sender = CBColorHalo) or (Sender = TBHaloSize) then begin
    ScreenDemo.Planet.ColorHalo := ColorToBGRA(CBColorHalo.ButtonColor);

    ScreenDemo.Planet.HaloSize := 1.0 - (TBHaloSize.Position/TBHaloSize.Max);
  end;

  if Sender = TBShadowAmount then
    ScreenDemo.Planet.ShadowAmount := TBShadowAmount.Position/TBShadowAmount.Max;

  if Sender = FSERotationSpeed then
    ScreenDemo.Planet.RotationSpeed := FSERotationSpeed.Value;
  if Sender = BDefaultRotationSpeed then begin
    FSERotationSpeed.Value := 0.0035;
    ScreenDemo.Planet.RotationSpeed := FSERotationSpeed.Value;
  end;
end;

procedure TFormMain.PresetToWidget(const A: TStringArray);
begin
  ScreenDemo.Planet.LoadParamsFromString(A[0]);

  FLoadingPreset := True;

  CBColorGround1.ButtonColor := ScreenDemo.Planet.ColorGround1.ToColor;
  CBColorGround2.ButtonColor := ScreenDemo.Planet.ColorGround2.ToColor;
  TBGroundColDistrib.Position := Round(ScreenDemo.Planet.GroundColorDistribution*TBGroundColDistrib.Max);
  TBGroundAmount.Position := Round(ScreenDemo.Planet.GroundAmount*TBGroundAmount.Max);

  CBColorOcean.ButtonColor := ScreenDemo.Planet.ColorOcean.ToColor;

  CBCloudsColor1.ButtonColor := ScreenDemo.Planet.ColorClouds1.ToColor;
  CBCloudsColor2.ButtonColor := ScreenDemo.Planet.ColorClouds2.ToColor;
  TBCloudsColDistrib.Position := Round(ScreenDemo.Planet.CloudsColorDistribution*TBCloudsColDistrib.Max);
  TBCloudsOpacity.Position := ScreenDemo.Planet.CloudsOpacity;
  TBCloudsFragmentation.Position := Round(ScreenDemo.Planet.CloudsFragmentation*10);
  TBCloudsChange.Position := Round(ScreenDemo.Planet.CloudsTransformation*TBCloudsChange.Max);
  RBCloudsBlendAdd.Checked := ScreenDemo.Planet.CloudsBlendMode = 1.0;

  CBColorHalo.ButtonColor := ScreenDemo.Planet.ColorHalo.ToColor;
  TBHaloSize.Position := Round((1.0 - ScreenDemo.Planet.HaloSize) * TBHaloSize.Max);

  TBShadowAmount.Position := Round(ScreenDemo.Planet.ShadowAmount*100);

  FSERotationSpeed.Value := ScreenDemo.Planet.RotationSpeed;

  FLoadingPreset := False;
end;

function TFormMain.WidgetToPreset: string;
begin
  Result := ScreenDemo.Planet.SaveParamsToString;
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

