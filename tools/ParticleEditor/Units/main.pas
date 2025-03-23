unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, Graphics, LCLType,
  StdCtrls, ComCtrls, ExtCtrls, Menus, Spin, ExtDlgs,
  BGRABitmap, BGRABitmapTypes,
  OpenGLContext,
  common,
  OGLCScene,
  screen_Home, Frame_CurveEdit, frame_ShowColor, Types;

type

  TMouseState = ( msIdle,
                  msMoveView
                );

  { TForm_Principale }

  TForm_Principale = class(TForm)
    BLoadTexture: TButton;
    BResetZoom: TSpeedButton;
    Button10: TButton;
    BResetSize: TButton;
    Button2: TButton;
    BChangeBG: TButton;
    BShoot: TButton;
    BResetSpin: TButton;
    BResetAngularVelocity: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CD1: TColorDialog;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CBEmitterType: TComboBox;
    CBColorMode: TComboBox;
    CBBlend: TComboBox;
    CheckBox4: TCheckBox;
    EditFrameWidth: TEdit;
    EditFrameHeight: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    AngularVelocityCurve: TFrameEditCurve;
    ColorCurve: TFrameEditCurve;
    Edit5: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    FrameShowColor1: TFrameShowColor;
    Label31: TLabel;
    Label32: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    OPD1: TOpenPictureDialog;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel9: TPanel;
    RB30: TRadioButton;
    RB31: TRadioButton;
    RB3: TRadioButton;
    RB32: TRadioButton;
    RB33: TRadioButton;
    ScrollBox1: TScrollBox;
    SizeCurve: TFrameEditCurve;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Panel8: TPanel;
    BCenterView: TSpeedButton;
    SpinCurve: TFrameEditCurve;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Panel7: TPanel;
    SE3: TSpinEdit;
    SE4: TSpinEdit;
    Splitter1: TSplitter;
    TBParticleSpin: TTrackBar;
    TBParticleSpinVariation: TTrackBar;
    TBParticleInitialSize: TTrackBar;
    TBParticleInitialSizeVariation: TTrackBar;
    TBParticleAngularVelocity: TTrackBar;
    TBParticleAngularVelocityVariation: TTrackBar;
    TB20: TTrackBar;
    Timer1: TTimer;
    VelocityCurve: TFrameEditCurve;
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
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MIOpen: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MenuItem5: TMenuItem;
    MIQuit: TMenuItem;
    OD1: TOpenDialog;
    OD2: TOpenDialog;
    OGL: TOpenGLControl;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    RB1: TRadioButton;
    RB2: TRadioButton;
    SD1: TSaveDialog;
    SEStartFrame: TSpinEdit;
    SEEndFrame: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TBEmitterLife: TTrackBar;
    TBParticleVelocity: TTrackBar;
    TBParticleVelocityVariation: TTrackBar;
    TBDirection: TTrackBar;
    TBSpread: TTrackBar;
    TBParticleCount: TTrackBar;
    TBParticleLife: TTrackBar;
    TBParticleLifeVariation: TTrackBar;
    TBParticleAngle: TTrackBar;
    TBParticleAngleVariation: TTrackBar;
    TB9: TTrackBar;
    procedure BCenterViewClick(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure BResetSizeClick(Sender: TObject);
    procedure BLoadTextureClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BChangeBGClick(Sender: TObject);
    procedure BShootClick(Sender: TObject);
    procedure BResetZoomClick(Sender: TObject);
    procedure BResetSpinClick(Sender: TObject);
    procedure BResetAngularVelocityClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CBColorModeChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CBEmitterTypeChange(Sender: TObject);
    procedure EditFrameWidthEditingDone(Sender: TObject);
    procedure Edit3EditingDone(Sender: TObject);
    procedure Edit5EditingDone(Sender: TObject);
    procedure Edit7EditingDone(Sender: TObject);
    procedure Edit9EditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure OGLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RB1Change(Sender: TObject);
    procedure SEStartFrameChange(Sender: TObject);
    procedure SE3Change(Sender: TObject);
    procedure TBParticleVelocityChange(Sender: TObject);
    procedure TBParticleSpinChange(Sender: TObject);
    procedure TBParticleInitialSizeChange(Sender: TObject);
    procedure TBParticleAngularVelocityChange(Sender: TObject);
    procedure TBEmitterLifeChange(Sender: TObject);
    procedure TB20Change(Sender: TObject);
    procedure TBParticleCountChange(Sender: TObject);
    procedure TBParticleLifeChange(Sender: TObject);
    procedure TBParticleAngleChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMouseState: TMouseState;
    FClickOrigin: TPoint;
    FViewOffset: TPointF;
    FZoom: single;
    procedure DoLoopMoveView;
  private
    FInitializing: boolean;
    procedure UpdateWidgets;
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure SetWindowTitle(aModified: boolean);
    procedure LoadFile(const aFile: string);
    procedure ProcessVelocityCurveChange(p: PPointF; count: integer);
    procedure ProcessSpinCurveChange(p: PPointF; count: integer);
    procedure ProcessSizeCurveChange(p: PPointF; count: integer);
    procedure ProcessAngularVelocityCurveChange(p: PPointF; count: integer);
  private
    procedure ProcessColorCurveChange(p: PPointF; count: integer);
    procedure ProcessColorCurveDelete(aIndex: integer);
    procedure ProcessColorCurvePointDblClick( PointIndex: integer);
    procedure ProcessColorCurveAddPoint(PointIndex: integer; P: TPointF);
    procedure SetZoom(AValue: single);
  public
    function GetVelocityValue: integer;
    procedure SetVelocityValue(aValue: integer);
    procedure LoadExample;

    property Zoom: single read FZoom write SetZoom;
  end;

var
  Form_Principale: TForm_Principale;

implementation
uses Math;

{$R *.lfm}

{ TForm_Principale }

procedure TForm_Principale.FormCreate(Sender: TObject);
begin
 FScene := TOGLCScene.Create(OGL, 4/3);
 FScene.LayerCount := LAYER_COUNT;

 FScene.OnLoadCommonData := @LoadCommonData;
 FScene.OnFreeCommonData := @FreeCommonData;

 Application.OnIdle := @ProcessApplicationIdle;

 FZoom := 1.0;
end;

procedure TForm_Principale.FormDestroy(Sender: TObject);
begin
 FScene.Free;
 FScene := NIL;
end;

procedure TForm_Principale.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 FScene.ColorFadeIn(BGRABlack, 0.5 );
 //FScene.ExecuteDuring(0.5);
end;

procedure TForm_Principale.FormCloseQuery(Sender: TObject; var CanClose: boolean );
var r:TModalResult;
begin
 CanClose:=TRUE;
 if FModified then begin
  r := MessageDlg('','Do you want save before quit ?', mtWarning, [mbYes,mbNo,mbCancel],'');
  if r=mrCancel then CanClose := FALSE;
  if r=mrYes then MISaveClick(NIL);
 end;
end;

// load texture
procedure TForm_Principale.BLoadTextureClick(Sender: TObject);
var ima: TBGRABitmap;
  w, h: integer;
begin
 if not OD2.Execute then exit;
 if PE.FParticleParam.Texture <> NIL
     then FScene.TexMan.Delete(PE.FParticleParam.Texture);
 PE.FParticleParam.Texture := NIL;

 ima:= TBGRABitmap.Create( OD2.FileName );
 w := ima.Width;
 h := ima.Height;
 EditFrameWidth.Text := w.ToString;
 EditFrameHeight.Text := h.ToString;
 ima.Free;

 SEStartFrame.Value := 0;
 SEEndFrame.Value := 0;
 PE.FParticleParam.StartFrame := 0;
 PE.FParticleParam.EndFrame := 0;

 PE.FParticleParam.Texture := FScene.TexMan.Add(OD2.FileName, w, h);
 Label4.Caption := ExtractFileName(OD2.FileName);
 SetWindowTitle(True);
end;

// Direction -45°
procedure TForm_Principale.Button10Click(Sender: TObject);
begin
  TBDirection.Position := TBDirection.Position-45;
end;

procedure TForm_Principale.BCenterViewClick(Sender: TObject);
begin
  FViewOffset := PointF(0,0); //FScene.Center.Round;
  HomeScreen.ApplyViewOffset(FViewOffset);
end;

// Size reset curve
procedure TForm_Principale.BResetSizeClick(Sender: TObject);
begin
  with PE.FParticleParam do begin
   SetLength( ArraySize, 2);
   ArraySize[0].Life:=0.0;
   ArraySize[0].Value:= 1.0;
   ArraySize[1].Life:=1.0;
   ArraySize[1].Value:= 1.0;
  end;

  SizeCurve.Clear;
  SizeCurve.AddPoint(0.0, 0.25);
  SizeCurve.AddPoint(1.0, 0.25);
end;

procedure TForm_Principale.Button2Click(Sender: TObject);
begin
  PE.Clear;
end;

// change background color or image
procedure TForm_Principale.BChangeBGClick(Sender: TObject);
var p: PTexture;
begin
 if RB1.Checked then begin
   if not CD1.Execute then exit;
   FBackGroundColor.SetAllColorsTo( ColorToBGRA( CD1.Color ));
 end;

 if RB2.Checked then begin
  if not OPD1.Execute then exit;
  if FBackgroundImage <> NIL then FBackgroundImage.Kill;
  p := FScene.TexMan.Add(OPD1.FileName);
  FBackgroundImage := TSprite.Create(p, TRUE);
  FScene.Add(FBackgroundImage, LAYER_BACKGROUND);
  FBackgroundImage.CenterOnScene;
 end;
end;

// shoot !
procedure TForm_Principale.BShootClick(Sender: TObject);
begin
  PE.Shoot;
end;

//camera zoom reset
procedure TForm_Principale.BResetZoomClick(Sender: TObject);
begin
  Zoom := 1.0;
  HomeScreen.ApplyViewZoom(Zoom);
end;

// Spin reset
procedure TForm_Principale.BResetSpinClick(Sender: TObject);
begin
  TBParticleSpin.Position := 0;
  SetWindowTitle(TRUE);
end;

//Angular velocity reset
procedure TForm_Principale.BResetAngularVelocityClick(Sender: TObject);
begin
  TBParticleAngularVelocity.Position := 0;
  TBParticleAngularVelocityVariation.Position := 0;
end;

// Direction 180°
procedure TForm_Principale.Button8Click(Sender: TObject);
begin
  TBDirection.Position := 180;
end;

// Direction +45°
procedure TForm_Principale.Button9Click(Sender: TObject);
begin
  TBDirection.Position := TBDirection.Position+45;
end;

// Color mode/Blend
procedure TForm_Principale.CBColorModeChange(Sender: TObject);
begin
  if Sender = CheckBox4 then begin
    FrameShowColor1.ShowAlpha := CheckBox4.Checked;
    FrameShowColor1.UpdateColor(PE.FParticleParam.ArrayColor);
    exit;
  end;

  PE.TintMode := TTintMode(CBColorMode.ItemIndex);
  PE.BlendMode := CBBlend.ItemIndex;
  SetWindowTitle(True);
end;

procedure TForm_Principale.CheckBox1Change(Sender: TObject);
begin
  PE.LoopMode := CheckBox1.Checked;
  BShoot.Visible := not CheckBox1.Checked;
  Label1.Visible := not CheckBox1.Checked;
  Label10.Visible := not CheckBox1.Checked;
  TBEmitterLife.Visible := not CheckBox1.Checked;
  SetWindowTitle(True);
end;

procedure TForm_Principale.CheckBox2Change(Sender: TObject);
begin
  if FInitializing then exit;
  PE.ParticlesPosRelativeToEmitterPos := CheckBox2.Checked;
  SetWindowTitle(True);
end;

// Show/hide emitter shape
procedure TForm_Principale.CheckBox3Change(Sender: TObject);
begin
  PE.DrawEmitterShape := CheckBox3.Checked;
end;

procedure TForm_Principale.CBEmitterTypeChange(Sender: TObject);
begin
  UpdateWidgets;
{ Panel12.Visible := CBEmitterType.ItemIndex = 1;   // line
 Panel11.Visible := CBEmitterType.ItemIndex = 2;   // rectangle
 Panel13.Visible := (CBEmitterType.ItemIndex = 3) or  // circle, inner circle and outer circle
                    (CBEmitterType.ItemIndex = 5) or
                    (CBEmitterType.ItemIndex = 6);
 Panel14.Visible := (CBEmitterType.ItemIndex = 4); // ring  }

  case CBEmitterType.ItemIndex of
   0: PE.SetEmitterTypePoint;
   1: PE.SetEmitterTypeLine(TBDirection.Position, StringToSingle(Edit5.Text));
   2: PE.SetEmitterTypeRectangle(strtoint(Edit3.Text), strtoint(Edit4.Text));
   3: PE.SetEmitterTypeCircle(StringToSingle(Edit7.Text));
   4: PE.SetEmitterTypeRing( StringToSingle(Edit9.Text), StringToSingle(Edit8.Text));
   5: PE.SetEmitterTypeInnerCircle(StringToSingle(Edit7.Text));
   6: PE.SetEmitterTypeOuterCircle(StringToSingle(Edit7.Text));
  end;
end;

// Texture Width and height
procedure TForm_Principale.EditFrameWidthEditingDone(Sender: TObject);
begin
  if strtoint(EditFrameWidth.Text) < 1 then EditFrameWidth.Text := '1';
  if strtoint(EditFrameHeight.Text) < 1 then EditFrameHeight.Text := '1';

  if PE.FParticleParam.Texture <> NIL
    then FScene.TexMan.Delete(PE.FParticleParam.Texture);
  PE.FParticleParam.Texture := FScene.TexMan.Add(OD2.FileName, strtoint(EditFrameWidth.Text), strtoint(EditFrameHeight.Text));
end;

// Rectangle size
procedure TForm_Principale.Edit3EditingDone(Sender: TObject);
begin
  PE.SetEmitterTypeRectangle(strtoint(Edit3.Text), strtoint(Edit4.Text));
  SetWindowTitle(True);
end;

// Line length
procedure TForm_Principale.Edit5EditingDone(Sender: TObject);
begin
  PE.SetEmitterTypeLine(TBDirection.Position, StringToSingle(Edit5.Text));
end;

// circle radius
procedure TForm_Principale.Edit7EditingDone(Sender: TObject);
begin
  PE.SetEmitterTypeCircle(StringToSingle(Edit7.Text));
end;

// ring small radius/big radius
procedure TForm_Principale.Edit9EditingDone(Sender: TObject);
begin
  PE.SetEmitterTypeRing(StringToSingle(Edit9.Text), StringToSingle(Edit8.Text));
end;


procedure TForm_Principale.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //FScene.ProcessOnKeyDown(Key, Shift);
end;

procedure TForm_Principale.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if BShoot.Visible and (Key = VK_F1)
    then PE.Shoot;
//    else FScene.ProcessOnKeyUp(Key, Shift);
end;

procedure TForm_Principale.MIOpenClick(Sender: TObject);
var r: TModalResult;
begin
  if FModified then begin
   r := MessageDlg('','Do you want save before load another project ?', mtWarning, [mbYes,mbNo,mbCancel],'');
   if r = mrCancel then exit;
   if r = mrYes then MISaveClick(NIL);
  end;

  if not OD1.Execute then exit;
  LoadFile(OD1.FileName);
  FProjectName := ExtractFileName(OD1.FileName);
  SetWindowTitle(False);
end;

// save
procedure TForm_Principale.MISaveClick(Sender: TObject);
var s: TStringList;
  t: string;
  i: integer;
begin
  if not SD1.Execute then exit;
  FProjectName := ExtractFileName( SD1.FileName );
  s := TStringList.Create;
  s.Add('E_Coor');
  s.Add('0 0');

  s.Add('E_GlobalOpacity');
  s.Add(round(PE.Opacity.Value).ToString);

  s.Add('E_Direction');
  s.Add(TBDirection.Position.ToString);

  s.Add('E_Spread');
  s.Add(TBSpread.Position.ToString);

  s.Add('E_Life');
  s.Add( FormatFloat('0.000', TBEmitterLife.Position*0.001));

  s.Add('E_LoopMode');
  if PE.LoopMode
    then s.Add('TRUE')
    else s.Add('FALSE');

  s.Add('E_Emission');
  s.Add(TBParticleCount.Position.ToString);

  s.Add('E_Gravity');
  s.Add(SE3.Value.ToString+' '+SE4.Value.ToString);

  s.Add('E_Type');
  case CBEmitterType.ItemIndex of
   0: s.Add('0');
   1: s.Add('1 '+ Edit5.Text);
   2: s.Add('2 '+ Edit3.Text+' '+Edit4.Text);
   3: s.Add('3 '+ Edit7.Text);
   4: s.Add('4 '+ Edit9.Text+' '+Edit8.Text);
   5: s.Add('5 '+ Edit7.Text);
   6: s.Add('6 '+ Edit7.Text);
  end;

  s.Add('P_ParticlesPositionsRelativeToEmitterPosition');
  if PE.ParticlesPosRelativeToEmitterPos
    then s.Add('TRUE')
    else s.Add('FALSE');

  s.Add('P_Texture');
  s.Add( Label4.Caption );
  s.Add( EditFrameWidth.Text+' '+EditFrameHeight.Text );

  s.Add('P_Frame');
  s.Add( SEStartFrame.Value.ToString+' '+SEEndFrame.Value.ToString);

  s.Add('P_Life');
  s.Add( FormatFloat('0.000', TBParticleLife.Position*0.001)+' '+FormatFloat('0.000', TBParticleLifeVariation.Position*0.001) );

  s.Add('P_ColorMode');
  s.Add(CBColorMode.ItemIndex.ToString);

  s.Add('P_BlendMode');
  s.Add(CBBlend.ItemIndex.ToString);

  s.Add('P_Color');
  with PE.FParticleParam do begin
   t := Length(ArrayColor).ToString;
   for i:=0 to Length(ArrayColor)-1 do
     t+=' ' + FormatFloat('0.00000', ArrayColor[i].Life) + ' ' +
        ArrayColor[i].C.red.ToString + ' ' +
        ArrayColor[i].C.green.ToString + ' ' +
        ArrayColor[i].C.blue.ToString + ' ' +
        ArrayColor[i].C.alpha.ToString;
  end;
  s.Add(t);

  s.Add('P_Velocity');
  t := GetVelocityValue.ToString+' '+TBParticleVelocityVariation.Position.ToString;
  with PE.FParticleParam do begin
   t += ' '+Length(ArrayVelocity).ToString;
   for i:=0 to Length(ArrayVelocity)-1 do
     t+=' ' + FormatFloat('0.00000', ArrayVelocity[i].Life) + ' ' +
              FormatFloat('0.00000',ArrayVelocity[i].Value);
  end;
  s.Add(t);

  s.Add('P_AVelocity');
  t := TBParticleAngularVelocity.Position.ToString+' '+TBParticleAngularVelocityVariation.Position.ToString;
  with PE.FParticleParam do begin
   t += ' '+Length(ArrayAVelocity).ToString;
   for i:=0 to Length(ArrayAVelocity)-1 do
     t+=' ' + FormatFloat('0.00000', ArrayAVelocity[i].Life) + ' ' +
              FormatFloat('0.00000',ArrayAVelocity[i].Value);
  end;
  s.Add(t);

  s.Add('P_Size');
  t := FormatFloat('0.00000', TBParticleInitialSize.Position/100)+' '+
       FormatFloat('0.00000', TBParticleInitialSizeVariation.Position/100);
  with PE.FParticleParam do begin
   t += ' '+inttostr(Length(ArraySize));
   for i:=0 to Length(ArraySize)-1 do
     t+=' ' + FormatFloat('0.00000', ArraySize[i].Life) + ' ' +
              FormatFloat('0.00000',ArraySize[i].Value);
  end;
  s.Add(t);

  s.Add('P_StartAngle');
  s.Add(TBParticleAngle.Position.ToString+' '+TBParticleAngleVariation.Position.ToString);

  s.Add('P_Spin');
  t := TBParticleSpin.Position.ToString+' '+TBParticleSpinVariation.Position.ToString;
  with PE.FParticleParam do begin
   t += ' '+Length(ArraySpin).ToString;
   for i:=0 to Length(ArraySpin)-1 do
     t+=' ' + FormatFloat('0.00000', ArraySpin[i].Life) + ' ' +
              FormatFloat('0.00000',ArraySpin[i].Value);
  end;
  s.Add(t);
  try
    s.SaveToFile(SD1.FileName);
  finally
    s.Free;
  end;
  SetWindowTitle(False);
end;

procedure TForm_Principale.MIQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm_Principale.OGLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FClickOrigin := Point(X, Y);
  if (Button = mbLeft) and (FMouseState = msIdle) then DoLoopMoveView;
end;

procedure TForm_Principale.OGLMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPointF;
begin
  if (Button = mbRight) and (FMouseState = msIdle) then begin
    if (HomeScreen <> NIL) and (PE <> NIL) then begin
      p := HomeScreen.FCamera.ControlToWorld(X, Y);
      PE.SetCoordinate(p);
    end;
  end;

  if (Button = mbLeft) and (FMouseState = msMoveView) then
    FMouseState := msIdle;
end;

procedure TForm_Principale.OGLMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var p1, p2, p: TPointF;
begin
  p1 := HomeScreen.FCamera.ControlToWorld(MousePos.x, MousePos.y);

  if WheelDelta < 0 then
    Zoom := Zoom - Zoom*0.2
  else
    Zoom := Zoom + Zoom*0.2;
  HomeScreen.ApplyViewZoom(Zoom);

  // moves the view
  p2 := HomeScreen.FCamera.ControlToWorld(MousePos.x, MousePos.y);
  p := p2 - p1;
  FViewOffset := FViewOffset - p;

  HomeScreen.ApplyViewOffset(FViewOffset);

  Handled := TRUE;
end;

// radio button 'Color/image' for background
procedure TForm_Principale.RB1Change(Sender: TObject);
begin
  FBackGroundColor.Visible := RB1.Checked;
  if FBackgroundImage <> NIL then FBackgroundImage.Visible := RB2.Checked;
  FBackGroundRainbow.Visible := RB3.Checked;
end;

// frame index
procedure TForm_Principale.SEStartFrameChange(Sender: TObject);
begin
  if FInitializing then exit;
  PE.FParticleParam.StartFrame := SEStartFrame.Value;
  PE.FParticleParam.EndFrame := SEEndFrame.Value;
  SetWindowTitle(True);
end;

// Gravity
procedure TForm_Principale.SE3Change(Sender: TObject);
begin
  if FInitializing then exit;
  PE.Gravity.Value := PointF(SE3.Value, SE4.Value);
  SetWindowTitle(True);
end;

// Velocity/Variation
procedure TForm_Principale.TBParticleVelocityChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.Velocity := GetVelocityValue;
  PE.FParticleParam.VelocityVariation := TBParticleVelocityVariation.Position;
  SetWindowTitle(True);
end;

// Spin/Variation
procedure TForm_Principale.TBParticleSpinChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.Spin:= TBParticleSpin.Position;
  PE.FParticleParam.SpinVariation := TBParticleSpinVariation.Position;

  SetWindowTitle(True);
end;

// Size/Variation
procedure TForm_Principale.TBParticleInitialSizeChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.Size := TBParticleInitialSize.Position/100;
  PE.FParticleParam.SizeVariation := TBParticleInitialSizeVariation.Position/100;

  SetWindowTitle(True);
end;

// Angular Velocity/Variation
procedure TForm_Principale.TBParticleAngularVelocityChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.AVelocity:=TBParticleAngularVelocity.Position;
  PE.FParticleParam.AVelocityVariation:=TBParticleAngularVelocityVariation.Position;
  SetWindowTitle(True);
end;

// Emitter life, Direction, Spread
procedure TForm_Principale.TBEmitterLifeChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.EmitterLife := TBEmitterLife.Position*0.001;
  PE.Direction.Value := TBDirection.Position;
  PE.Spread.Value := TBSpread.Position;

  SetWindowTitle(True);
end;

procedure TForm_Principale.TB20Change(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.Opacity.pcValue := TB20.Position*0.01;
  SetWindowTitle(True);
end;

// Emission
procedure TForm_Principale.TBParticleCountChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.ParticlesToEmit.Value := TBParticleCount.Position;
  SetWindowTitle(True);
end;

// Life
procedure TForm_Principale.TBParticleLifeChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.Life := TBParticleLife.Position*0.001;
  PE.FParticleParam.LifeVariation := TBParticleLifeVariation.Position*0.001;
  SetWindowTitle(True);
end;

// Start Angle
procedure TForm_Principale.TBParticleAngleChange(Sender: TObject);
begin
  UpdateWidgets;
  if FInitializing then exit;

  PE.FParticleParam.StartAngle := TBParticleAngle.Position;
  PE.FParticleParam.StartAngleVariation := TBParticleAngleVariation.Position;
  SetWindowTitle(True);
end;

procedure TForm_Principale.Timer1Timer(Sender: TObject);
begin
  if PE <> NIL then Label32.Caption := PE.ParticlesCount.ToString;
end;

procedure TForm_Principale.DoLoopMoveView;
var current, delta: TPoint;
begin
  FMouseState := msMoveView;
  OGL.Cursor := crSizeAll;

  repeat
    current := OGL.ScreenToClient(Mouse.CursorPos);
    delta := current - FClickOrigin;
    if (delta.x <> 0) or (delta.y <> 0) then begin
      delta.x := Round(delta.x / FZoom);
      delta.y := Round(delta.y / FZoom);
      FViewOffset := FViewOffset - PointF(delta);
      HomeScreen.ApplyViewOffset(FViewOffset);

      FClickOrigin := current;
      FScene.DoLoop;
    end;
    Application.ProcessMessages;
  until FMouseState = msIdle;
  OGL.Cursor := crDefault;
  Caption := 'FViewOffset = '+FViewOffset.x.ToString+','+FViewOffset.y.ToString+
             '  Zoom = '+FormatFloat('0.00', Zoom);
end;

procedure TForm_Principale.UpdateWidgets;
var texWidth: integer;
begin
  // Life
  Label17.Caption := FormatFloat('0.000', TBParticleLife.Position*0.001)+' sec';
  Label20.Caption := FormatFloat('0.000', TBParticleLifeVariation.Position*0.001)+' sec';

  // Emission
  Label16.Caption := TBParticleCount.Position.ToString+' particles/sec';

  // global opacity
  Label55.Caption := 'Global opacity '+TB20.Position.ToString+'%';

  // Emitter life, Direction, Spread
  Label10.Caption := TBEmitterLife.Position.ToString + ' ms';
  Label11.Caption := TBDirection.Position.ToString + ' Deg';
  Label12.Caption := TBSpread.Position.ToString + ' Deg';

  // Start Angle
  Label21.Caption := TBParticleAngle.Position.ToString+' deg';
  Label24.Caption := TBParticleAngleVariation.Position.ToString+' deg';

  // Angular Velocity/Variation
  Label46.Caption := FormatFloat('0.000', TBParticleAngularVelocity.Position)+' Deg/sec';
  Label48.Caption := FormatFloat('0.000', TBParticleAngularVelocityVariation.Position)+' Deg/sec';

  // Size/Variation
  texWidth := strtoint(EditFrameWidth.Text);   // texture width
  Label42.Caption := FormatFloat('0.00', TBParticleInitialSize.Position*0.01)+' = '+
                                 Trunc(TBParticleInitialSize.Position*0.01*texWidth).ToString+' px';
  Label44.Caption := FormatFloat('0.00', TBParticleInitialSizeVariation.Position*0.01)+' = '+
                                 Trunc(TBParticleInitialSizeVariation.Position*0.01*texWidth).ToString+' px';

  // Spin/Variation
  Label38.Caption := TBParticleSpin.Position.ToString+' Deg/sec';
  Label40.Caption := TBParticleSpinVariation.Position.ToString+' Deg/sec';

  // Velocity/Variation
  Label34.Caption := GetVelocityValue.ToString+' px/sec';
  Label36.Caption := TBParticleVelocityVariation.Position.ToString+' px/sec';

  // emitter type
  Panel12.Visible := CBEmitterType.ItemIndex = 1;   // line
  Panel11.Visible := CBEmitterType.ItemIndex = 2;   // rectangle
  Panel13.Visible := (CBEmitterType.ItemIndex = 3) or  // circle, inner circle and outer circle
                     (CBEmitterType.ItemIndex = 5) or
                     (CBEmitterType.ItemIndex = 6);
  Panel14.Visible := (CBEmitterType.ItemIndex = 4); // ring
end;

procedure TForm_Principale.LoadExample;
begin
  LoadFile('Examples'+DirectorySeparator+'Smoke.par');
  OD2.FileName := Label4.Caption;
end;

procedure TForm_Principale.LoadCommonData;
begin
  DoubleBuffered:=TRUE;
  // load common data here
  HomeScreen := THomeScreen.Create;
  FScene.RunScreen(HomeScreen);

  FViewOffset := PointF(0,0); // FScene.Center.Round;

  VelocityCurve.OnCurveChange := @ProcessVelocityCurveChange;
  VelocityCurve.SetLegendMinMax('-200%','+200%');
  VelocityCurve.AddHorizAxis([0.25, 0.5, 0.75]);

  SpinCurve.OnCurveChange := @ProcessSpinCurveChange;
  SpinCurve.SetLegendMinMax('-200%','+200%');
  SpinCurve.AddHorizAxis([0.25, 0.5, 0.75]);

  SizeCurve.OnCurveChange := @ProcessSizeCurveChange;
  SizeCurve.SetLegendMinMax('0%', '+400%');
  SizeCurve.AddHorizAxis([0.25, 0.5, 0.75]);

  AngularVelocityCurve.OnCurveChange := @ProcessAngularVelocityCurveChange;
  AngularVelocityCurve.SetLegendMinMax('-200%', '+200%');
  AngularVelocityCurve.AddHorizAxis([0.25, 0.5, 0.75]);

  ColorCurve.OnCurveChange := @ProcessColorCurveChange;
  ColorCurve.OnDeleteCallBack := @ProcessColorCurveDelete;
  ColorCurve.OnPointDblClick := @ProcessColorCurvePointDblClick;
  ColorCurve.OnAddPoint := @ProcessColorCurveAddPoint;
  ColorCurve.SetLegendMinMax('alpha 0','alpha 255');
end;

procedure TForm_Principale.FreeCommonData;
begin
  // free common data here
  HomeScreen.Free;

  VelocityCurve.FreeData;
  SpinCurve.FreeData;
  SizeCurve.FreeData;
  AngularVelocityCurve.FreeData;
  ColorCurve.FreeData;
  FrameShowColor1.FreeData;
end;

procedure TForm_Principale.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FScene.DoLoop;
  Done := FALSE;
end;

procedure TForm_Principale.SetWindowTitle(aModified: boolean);
var txt: string;
begin
  FModified := aModified;
  txt := 'OGLC Particles Editor - '+FProjectName;
  if FModified then txt +='*';
  Caption := txt;
end;

procedure TForm_Principale.LoadFile(const aFile: string);
var i: integer;
begin
  PE.LoadFromFile(aFile, NIL);
  PE.CenterOnScene;
  FInitializing := True;

  // emitter loop mode
  CheckBox1.Checked := PE.LoopMode;

  // emitter life
  TBEmitterLife.Position := Round(PE.EmitterLife*1000);

  // emitter direction
  TBDirection.Position := Round(PE.Direction.Value);

  // emitter spread
  TBSpread.Position := Round(PE.Spread.Value);

  // emitter emission
  TBParticleCount.Position := Round(PE.ParticlesToEmit.Value);
  Label16.Caption := TBParticleCount.Position.ToString+' particles/sec';

  // global opacity
  TB20.Position := Round(PE.Opacity.Value/255*TB20.Max);

  // emitter type
  CBEmitterType.ItemIndex := Ord(PE.EmitterType);
  case PE.EmitterType of
    etPoint:;  // point
    etLine: Edit5.Text := Trunc(PE.EmitterParam.LineSize).ToString; // SplittedText[1];  // line length
    etRectangle: begin
      Edit3.Text := PE.Width.ToString;   //rectangle width height
      Edit4.Text := PE.Height.ToString;
    end;
    etCircle, etInnerCircle, etOuterCircle: Edit7.Text := FormatFloat('0.0', PE.EmitterParam.SmallRadius);// SplittedText[1]; // circle radius
    etRing: begin
      Edit9.Text := FormatFloat('0.0', PE.EmitterParam.SmallRadius);   // ring smallRadius bigRadius
      Edit8.Text := FormatFloat('0.0', PE.EmitterParam.BigRadius);
    end;
  end;//case
  CBEmitterTypeChange(NIL);

  // particle pos relative to emitter pos
  CheckBox2.Checked := PE.ParticlesPosRelativeToEmitterPos;

  // emitter gravity
  SE3.Value := Round(PE.Gravity.x.Value);
  SE4.Value := Round(PE.Gravity.y.Value);

  // texture
  Label4.Caption := PE.FParticleParam.Texture^.Filename;
  EditFrameWidth.Text := PE.FParticleParam.Texture^.FrameWidth.ToString;
  EditFrameHeight.Text := PE.FParticleParam.Texture^.FrameHeight.ToString;

  // texture frame
  SEStartFrame.Value := PE.FParticleParam.StartFrame;
  SEEndFrame.Value := PE.FParticleParam.EndFrame;

  // particle life
  TBParticleLife.Position := Round(PE.FParticleParam.Life*1000);
  TBParticleLifeVariation.Position := Round(PE.FParticleParam.LifeVariation);

  // particle velocity
  SetVelocityValue(Trunc(PE.FParticleParam.Velocity));
  //TBParticleVelocity.Position := Trunc(PE.FParticleParam.Velocity);
  TBParticleVelocityVariation.Position := Trunc(PE.FParticleParam.VelocityVariation);
  VelocityCurve.Clear;
  for i:=0 to High(PE.FParticleParam.ArrayVelocity) do
    VelocityCurve.AddPoint(PE.FParticleParam.ArrayVelocity[i].Life,
                           (PE.FParticleParam.ArrayVelocity[i].Value+2)/4, False);

  // particle spin
  TBParticleSpin.Position := Trunc(PE.FParticleParam.Spin);
  TBParticleSpinVariation.Position := Trunc(PE.FParticleParam.SpinVariation);
  SpinCurve.Clear;
  for i:=0 to High(PE.FParticleParam.ArraySpin) do
    SpinCurve.AddPoint(PE.FParticleParam.ArraySpin[i].Life,
                       (PE.FParticleParam.ArraySpin[i].Value+2)/4, False);

  // particle size
  TBParticleInitialSize.Position := Trunc(PE.FParticleParam.Size*100);
  TBParticleInitialSizeVariation.Position := Trunc(PE.FParticleParam.SizeVariation*100);
  SizeCurve.Clear;
  for i:=0 to High(PE.FParticleParam.ArraySize) do
    SizeCurve.AddPoint(PE.FParticleParam.ArraySize[i].Life,
                       PE.FParticleParam.ArraySize[i].Value/4, False);

  // particle angular velocity
  TBParticleAngularVelocity.Position := Trunc(PE.FParticleParam.AVelocity);
  TBParticleAngularVelocityVariation.Position := Trunc(PE.FParticleParam.AVelocityVariation);
  AngularVelocityCurve.Clear;
  for i:=0 to High(PE.FParticleParam.ArrayAVelocity) do
    AngularVelocityCurve.AddPoint(PE.FParticleParam.ArrayAVelocity[i].Life,
                                  (PE.FParticleParam.ArrayAVelocity[i].Value+2)/4, False);

  // particle color mode;
  CBColorMode.ItemIndex := Ord(PE.TintMode);

  // particle blend
  CBBlend.ItemIndex := PE.BlendMode;

  // particle colors
  ColorCurve.Clear;
  for i:=0 to High(PE.FParticleParam.ArrayColor) do
    ColorCurve.AddPoint(PE.FParticleParam.ArrayColor[i].Life,
                        PE.FParticleParam.ArrayColor[i].C.alpha/255, False);
  FrameShowColor1.UpdateColor(PE.FParticleParam.ArrayColor);

  PE.ParticlesPosRelativeToEmitterPos := CheckBox2.Checked;

  FInitializing := False;
  SetWindowTitle(False);
end;

procedure TForm_Principale.ProcessVelocityCurveChange(p: PPointF; count: integer);
var i: integer;
begin
  if FInitializing then exit;

  if P[0].x <> 0 then raise exception.create('first point is not at life 0');
  if P[count-1].x <> 1.0 then raise exception.create('last point is not at life 1');

  if count < Length(PE.FParticleParam.ArrayVelocity) then PE.Clear;

  SetLength(PE.FParticleParam.ArrayVelocity, count);
  i := 0;
  while i < count do begin
    PE.FParticleParam.ArrayVelocity[i].Life := p^.x;
    PE.FParticleParam.ArrayVelocity[i].Value := p^.y*4-2;
    inc(p);
    inc(i);
  end;

  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessSpinCurveChange(p: PPointF; count: integer);
var i: integer;
begin
  if FInitializing then exit;

  if P[0].x <> 0 then raise exception.create('first point is not at life 0');
  if P[count-1].x <> 1.0 then raise exception.create('last point is not at life 1');

  if count < Length(PE.FParticleParam.ArraySpin) then PE.Clear;

  SetLength(PE.FParticleParam.ArraySpin, count);
  i := 0;
  while i < count do begin
    PE.FParticleParam.ArraySpin[i].Life := p^.x;
    PE.FParticleParam.ArraySpin[i].Value := p^.y*4-2;
    inc(p);
    inc(i);
  end;

  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessSizeCurveChange(p: PPointF; count: integer);
var i: integer;
begin
  if FInitializing then exit;

  if P[0].x <> 0 then raise exception.create('first point is not at life 0');
  if P[count-1].x <> 1.0 then raise exception.create('last point is not at life 1');

  if count < Length(PE.FParticleParam.ArraySize) then PE.Clear;

  SetLength(PE.FParticleParam.ArraySize, count);
  i := 0;
  while i < count do begin
    PE.FParticleParam.ArraySize[i].Life := p^.x;
    PE.FParticleParam.ArraySize[i].Value := p^.y*4;
    inc(p);
    inc(i);
  end;

  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessAngularVelocityCurveChange(p: PPointF; count: integer);
var i: integer;
begin
  if FInitializing then exit;

  if P[0].x <> 0 then raise exception.create('first point is not at life 0');
  if P[count-1].x <> 1.0 then raise exception.create('last point is not at life 1');

  if count < Length(PE.FParticleParam.ArrayAVelocity) then PE.Clear;

  SetLength(PE.FParticleParam.ArrayAVelocity, count);
  i := 0;
  while i < count do begin
    PE.FParticleParam.ArrayAVelocity[i].Life := p^.x;
    PE.FParticleParam.ArrayAVelocity[i].Value := p^.y*4-2;
    inc(p);
    inc(i);
  end;

  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessColorCurveChange(p: PPointF; count: integer);
var i: integer;
begin
  if FInitializing then exit;

  if P[0].x <> 0 then raise exception.create('first point is not at life 0');
  if P[count-1].x <> 1.0 then raise exception.create('last point is not at life 1');

  if count < Length(PE.FParticleParam.ArrayColor) then PE.Clear;

  if Length(PE.FParticleParam.ArrayColor) <> count then
    SetLength(PE.FParticleParam.ArrayColor, count);
  i := 0;
  while i < count do begin
    PE.FParticleParam.ArrayColor[i].Life := p^.x;
    PE.FParticleParam.ArrayColor[i].C.alpha := Round(p^.y*255);
    inc(p);
    inc(i);
  end;
  if PE.FParticleParam.ArrayColor[0].Life <> 0.0 then
    raise exception.create('first item life value must be 0.0');
  if PE.FParticleParam.ArrayColor[High(PE.FParticleParam.ArrayColor)].Life <> 1.0 then
    raise exception.create('last item life value must be 0.0');

  FrameShowColor1.UpdateColor(PE.FParticleParam.ArrayColor);
  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessColorCurveDelete(aIndex: integer);
var i: integer;
begin
  for i:=aIndex to High(PE.FParticleParam.ArrayColor)-1 do
    PE.FParticleParam.ArrayColor[i].C := PE.FParticleParam.ArrayColor[i+1].C;
end;

procedure TForm_Principale.ProcessColorCurvePointDblClick(PointIndex: integer);
var c: TBGRAPixel;
begin
  CD1.Color := PE.FParticleParam.ArrayColor[PointIndex].C.ToColor;
  if not CD1.Execute then exit;
  c := ColorToBGRA(CD1.Color);

  PE.FParticleParam.ArrayColor[PointIndex].C.red := c.red;
  PE.FParticleParam.ArrayColor[PointIndex].C.green := c.green;
  PE.FParticleParam.ArrayColor[PointIndex].C.blue := c.blue;
  ColorCurve.Invalidate;
  FrameShowColor1.UpdateColor(PE.FParticleParam.ArrayColor);
  SetWindowTitle(True);
end;

procedure TForm_Principale.ProcessColorCurveAddPoint(PointIndex: integer; P: TPointF);
var i: integer;
begin
  // insert new color in particle emitter
  with PE.FParticleParam do begin
   SetLength(ArrayColor, Length(PE.FParticleParam.ArrayColor)+1);
   if Length(ArrayColor) > 1 then begin
     if PointIndex<Length(ArrayColor)-1 then
       for i:=Length(ArrayColor)-2 downto PointIndex do begin
         ArrayColor[i+1].Life := ArrayColor[i].Life;
         ArrayColor[i+1].C := ArrayColor[i].C;
       end;
   end;
   ArrayColor[PointIndex].Life := P.x;
   ArrayColor[PointIndex].C := ColorToBGRA(CD1.Color, round(255*P.y));

   FrameShowColor1.UpdateColor(ArrayColor);
  end;
  SetWindowTitle(True);
end;

procedure TForm_Principale.SetZoom(AValue: single);
begin
  FZoom := EnsureRange(AValue, 0.1, 5.0);
end;

function TForm_Principale.GetVelocityValue: integer;
begin
  Result := TBParticleVelocity.Position;
  if RB31.Checked then Result := Result*2;
  if RB32.Checked then Result := Result*5;
  if RB33.Checked then Result := Result*10;
end;

procedure TForm_Principale.SetVelocityValue(aValue: integer);
begin
  if AValue > 1024*5 then begin
   TBParticleVelocity.Position := AValue div 10;
   RB33.Checked := TRUE;
  end else if AValue > 1024*2 then begin
   TBParticleVelocity.Position := AValue div 5;
   RB32.Checked := TRUE;
  end else if AValue > 1024 then begin
   TBParticleVelocity.Position := AValue div 2;
   RB31.Checked := TRUE;
  end else begin
   TBParticleVelocity.Position := AValue;
   RB30.Checked := TRUE;
  end;
end;


end.

