unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Buttons, Graphics,
  StdCtrls, ComCtrls, ExtCtrls, Menus, Spin, ExtDlgs,
  BGRABitmap, BGRABitmapTypes,
  OpenGLContext,
  common,
  OGLCScene,
  screen_Home, Frame_CurveEdit, frame_ShowColor;

type

  { TForm_Principale }

  TForm_Principale = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CD1: TColorDialog;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    CBColorMode: TComboBox;
    CBBlend: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    AngularVelocityCurve: TFrame1;
    ColorCurve: TFrame1;
    Edit5: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Frame2_1: TFrame2;
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
    Panel9: TPanel;
    RB3: TRadioButton;
    SizeCurve: TFrame1;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Panel8: TPanel;
    SpinCurve: TFrame1;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Panel7: TPanel;
    SE3: TSpinEdit;
    SE4: TSpinEdit;
    TB12: TTrackBar;
    TB13: TTrackBar;
    TB14: TTrackBar;
    TB15: TTrackBar;
    TB16: TTrackBar;
    TB17: TTrackBar;
    TBZoom: TTrackBar;
    TB20: TTrackBar;
    VelocityCurve: TFrame1;
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
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LBJeux: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OD1: TOpenDialog;
    OD2: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
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
    SE1: TSpinEdit;
    SE2: TSpinEdit;
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
    TB1: TTrackBar;
    TB10: TTrackBar;
    TB11: TTrackBar;
    TB2: TTrackBar;
    TB3: TTrackBar;
    TB4: TTrackBar;
    TB5: TTrackBar;
    TB6: TTrackBar;
    TB7: TTrackBar;
    TB8: TTrackBar;
    TB9: TTrackBar;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CBColorModeChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1EditingDone(Sender: TObject);
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
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RB1Change(Sender: TObject);
    procedure SE1Change(Sender: TObject);
    procedure SE3Change(Sender: TObject);
    procedure TB10Change(Sender: TObject);
    procedure TB12Change(Sender: TObject);
    procedure TB14Change(Sender: TObject);
    procedure TB16Change(Sender: TObject);
    procedure TB1Change(Sender: TObject);
    procedure TB20Change(Sender: TObject);
    procedure TB4Change(Sender: TObject);
    procedure TB5Change(Sender: TObject);
    procedure TB7Change(Sender: TObject);
    procedure TBZoomChange(Sender: TObject);
  private
    procedure LoadCommonData;
    procedure FreeCommonData;
    procedure ProcessApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure SetWindowTitle( Modified: boolean );
    procedure LoadFile( const aFile: string );
    procedure ParticleEngineParamToUserScreenParam;
    procedure RestartParticleEngine;
    procedure ProcessVelocityCurveChange( p: psingle; count: integer );
    procedure ProcessSpinCurveChange( p: psingle; count: integer );
    procedure ProcessSizeCurveChange( p: psingle; count: integer );
    procedure ProcessAngularVelocityCurveChange( p: psingle; count: integer );
  private
    procedure ProcessColorCurveChange( p: psingle; count: integer );
    Procedure ProcessColorCurvePointDblClick( PointIndex: integer );
    procedure ProcessColorCurveAddPoint( PointIndex: integer; P: TPointF );
    procedure ProcessColorCurveDeletePoint( PointIndex: integer );
  public
  end;

var
  Form_Principale: TForm_Principale;

implementation
{$R *.lfm}

{ TForm_Principale }

procedure TForm_Principale.FormCreate(Sender: TObject);
begin
 FScene := TOGLCScene.Create ( OpenGLControl1 ) ;
 FScene.LayerCount := LAYER_COUNT;

 FScene.OnLoadCommonData := @LoadCommonData;
 FScene.OnFreeCommonData := @FreeCommonData;

 Application.OnIdle := @ProcessApplicationIdle;
end;

procedure TForm_Principale.FormDestroy(Sender: TObject);
begin
 FScene.Free;
 FScene := NIL;
end;

procedure TForm_Principale.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 FScene.ColorFadeIn(BGRABlack, 0.5 );
 FScene.ExecuteDuring(0.5);
end;

procedure TForm_Principale.FormCloseQuery(Sender: TObject; var CanClose: boolean );
var r:TModalResult;
begin
 CanClose:=TRUE;
 if FModified then begin
  r := MessageDlg('','Do you want save before quit ?', mtWarning, [mbYes,mbNo,mbCancel],'');
  if r=mrCancel then CanClose := FALSE;
  if r=mrYes then MenuItem3Click(NIL);
 end;
end;

// load texture
procedure TForm_Principale.Button1Click(Sender: TObject);
var ima: TBGRABitmap;
begin
 if not OD2.Execute then exit;
 if FPEngine.FParticleParam.Texture <> NIL
     then TextureManager.Delete( FPEngine.FParticleParam.Texture );

 ima:= TBGRABitmap.Create( OD2.FileName );
 Edit1.Text:=inttostr( ima.Width );
 Edit2.Text:=inttostr( ima.Height );
 ima.Free;

 SE1.Value:=0;
 SE2.Value:=0;
 FPEngine.FParticleParam.StartFrame:=0;
 FPEngine.FParticleParam.EndFrame:=0;


 FPEngine.FParticleParam.Texture := TextureManager.Add( OD2.FileName, strtoint(Edit1.Text), strtoint(Edit2.Text) );
 Label4.Caption:=ExtractFileName( OD2.FileName );
 SetWindowTitle( TRUE );
end;

// Direction -45°
procedure TForm_Principale.Button10Click(Sender: TObject);
begin
 TB2.Position := TB2.Position-45;
end;

// Size reset curve
procedure TForm_Principale.Button11Click(Sender: TObject);
begin
 with FPEngine.FParticleParam do begin
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
 FPEngine.Clear;
end;

// change background color or image
procedure TForm_Principale.Button3Click(Sender: TObject);
var p: PTexture;
begin
 if RB1.Checked then begin
   if not CD1.Execute then exit;
   FBackGroundColor.SetAllColorsTo( ColorToBGRA( CD1.Color ));
 end;

 if RB2.Checked then begin
  if not OPD1.Execute then exit;
  if FBackgroundImage<>NIL then FBackgroundImage.Kill;
  p := TextureManager.Add( OPD1.FileName );
  FBackgroundImage := TSprite.Create( p, TRUE );
  FScene.Add( FBackgroundImage, LAYER_BACKGROUND);
  FBackgroundImage.CenterOnScene;
 end;
end;

// shoot !
procedure TForm_Principale.Button4Click(Sender: TObject);
begin
 FPEngine.Shoot;
end;

//camera zoom reset
procedure TForm_Principale.Button5Click(Sender: TObject);
begin
 TBZoom.Position:=1000;
end;

// Spin reset
procedure TForm_Principale.Button6Click(Sender: TObject);
begin
 TB12.Position:=0;
 SetWindowTitle( TRUE );
end;

//Angular velocity reset
procedure TForm_Principale.Button7Click(Sender: TObject);
begin
 TB16.Position:=0;
 TB17.Position:=0;
end;

// Direction 180°
procedure TForm_Principale.Button8Click(Sender: TObject);
begin
TB2.Position := 180;
end;

// Direction +45°
procedure TForm_Principale.Button9Click(Sender: TObject);
begin
 TB2.Position := TB2.Position+45;
end;

// Color mode/Blend
procedure TForm_Principale.CBColorModeChange(Sender: TObject);
begin
 FPEngine.FParticleParam.ColorMode := CBColorMode.ItemIndex;
 FPEngine.FParticleParam.BlendMode := CBBlend.ItemIndex;
 SetWindowTitle( TRUE );
end;

procedure TForm_Principale.CheckBox1Change(Sender: TObject);
begin
 FPEngine.LoopMode := CheckBox1.Checked;
 Button4.Visible := not CheckBox1.Checked;
 Label1.Visible := not CheckBox1.Checked;
 Label10.Visible := not CheckBox1.Checked;
 TB1.Visible := not CheckBox1.Checked;
 SetWindowTitle( TRUE );
end;

procedure TForm_Principale.CheckBox2Change(Sender: TObject);
begin
 FPEngine.ParticlesPositionsRelativeToEmitterPosition := CheckBox2.Checked;
 SetWindowTitle( TRUE );
end;

// Show/hide emitter shape
procedure TForm_Principale.CheckBox3Change(Sender: TObject);
begin
 FDrawEmitterShape := CheckBox3.Checked;
end;

procedure TForm_Principale.ComboBox1Change(Sender: TObject);
begin
 Panel12.Visible := ComboBox1.ItemIndex=1;   // line
 Panel11.Visible := ComboBox1.ItemIndex=2;   // rectangle
 Panel13.Visible := (ComboBox1.ItemIndex=3) or  // circle, inner circle and outer circle
                    (ComboBox1.ItemIndex=5) or
                    (ComboBox1.ItemIndex=6);
 Panel14.Visible := (ComboBox1.ItemIndex=4); // ring

 case ComboBox1.ItemIndex of
  0: FPEngine.SetEmitterTypePoint;
  1: FPEngine.SetEmitterTypeLine( TB2.Position, strtofloat(Edit5.Text));
  2: FPEngine.SetEmitterTypeRectangle( strtoint(Edit3.Text), strtoint(Edit4.Text) );
  3: FPEngine.SetEmitterTypeCircle( strtofloat(Edit7.Text) );
  4: FPEngine.SetEmitterTypeRing( strtofloat(Edit9.Text), strtofloat(Edit8.Text) );
  5: FPEngine.SetEmitterTypeInnerCircle( strtofloat(Edit7.Text) );
  6: FPEngine.SetEmitterTypeOuterCircle( strtofloat(Edit7.Text) );
 end;
end;

// Texture Width and height
procedure TForm_Principale.Edit1EditingDone(Sender: TObject);
begin
 if strtoint(Edit1.Text)<1 then Edit1.Text:='1';
 if strtoint(Edit2.Text)<1 then Edit2.Text:='1';

 if FPEngine.FParticleParam.Texture <> NIL
     then TextureManager.Delete( FPEngine.FParticleParam.Texture );
 FPEngine.FParticleParam.Texture := TextureManager.Add( OD2.FileName, strtoint(Edit1.Text), strtoint(Edit2.Text) );
end;

// Rectangle size
procedure TForm_Principale.Edit3EditingDone(Sender: TObject);
begin
 FPEngine.SetEmitterTypeRectangle( strtoint(Edit3.Text), strtoint(Edit4.Text) );
 SetWindowTitle( TRUE );
end;

// Line length
procedure TForm_Principale.Edit5EditingDone(Sender: TObject);
begin
 FPEngine.SetEmitterTypeLine( TB2.Position, strtofloat(Edit5.Text));
end;

// circle radius
procedure TForm_Principale.Edit7EditingDone(Sender: TObject);
begin
 FPEngine.SetEmitterTypeCircle( strtofloat(Edit7.Text));
end;

// ring small radius/big radius
procedure TForm_Principale.Edit9EditingDone(Sender: TObject);
begin
 FPEngine.SetEmitterTypeRing( strtofloat(Edit9.Text), strtofloat(Edit8.Text));
end;


procedure TForm_Principale.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 FScene.ProcessKeyDown( Key, Shift );
end;

procedure TForm_Principale.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 FScene.ProcessKeyUp( Key, Shift );
end;

procedure TForm_Principale.MenuItem2Click(Sender: TObject);
var r: TModalResult;
begin
 if FModified then begin
  r := MessageDlg('','Do you want save before load another project ?', mtWarning, [mbYes,mbNo,mbCancel],'');
  if r=mrCancel then exit;
  if r=mrYes then MenuItem3Click(NIL);
 end;

 if not OD1.Execute then exit;
 LoadFile( OD1.FileName );
 FProjectName := ExtractFileName( OD1.FileName );
 SetWindowTitle( FALSE );
end;

// save
procedure TForm_Principale.MenuItem3Click(Sender: TObject);
var s: TStringList;
  t: string;
  i,Texwidth: integer;
begin
 if not SD1.Execute then exit;
 FProjectName := ExtractFileName( SD1.FileName );
 s := TStringList.Create;
 s.Add('E_Coor');
 s.Add('0 0');

 s.Add('E_GlobalOpacity');
 s.Add( inttostr(round(FPEngine.Opacity.Value)) );

 s.Add('E_Direction');
 s.Add( inttostr(TB2.Position));

 s.Add('E_Spread');
 s.Add( inttostr(TB3.Position));

 s.Add('E_Life');
 s.Add( FormatFloat('0.000', TB1.Position*0.001));

 s.Add('E_LoopMode');
 if FPEngine.LoopMode
   then s.Add('TRUE')
   else s.Add('FALSE');

 s.Add('E_Emission');
 s.Add( inttostr(TB4.Position));

 s.Add('E_Gravity');
 s.Add( inttostr(SE3.Value)+' '+inttostr(SE4.Value) );

 s.Add('E_Type');
 case ComboBox1.ItemIndex of
  0: s.Add('0');
  1: s.Add('1 '+ Edit5.Text);
  2: s.Add('2 '+ Edit3.Text+' '+Edit4.Text);
  3: s.Add('3 '+ Edit7.Text);
  4: s.Add('4 '+ Edit9.Text+' '+Edit8.Text);
  5: s.Add('5 '+ Edit7.Text);
  6: s.Add('6 '+ Edit7.Text);
 end;

 s.Add('P_ParticlesPositionsRelativeToEmitterPosition');
 if FPEngine.ParticlesPositionsRelativeToEmitterPosition
   then s.Add('TRUE')
   else s.Add('FALSE');

 s.Add('P_Texture');
 s.Add( Label4.Caption );
 s.Add( Edit1.Text+' '+Edit2.Text );

 s.Add('P_Frame');
 s.Add( inttostr(SE1.Value)+' '+inttostr(SE2.Value) );

 s.Add('P_Life');
 s.Add( FormatFloat('0.000', TB5.Position*0.001)+' '+FormatFloat('0.000', TB6.Position*0.001) );

 s.Add('P_ColorMode');
 s.Add( inttostr(CBColorMode.ItemIndex) );

 s.Add('P_BlendMode');
 s.Add( inttostr(CBBlend.ItemIndex) );

 s.Add('P_Color');
 with FPEngine.FParticleParam do begin
  t := inttostr(Length(ArrayColor));
  for i:=0 to Length(ArrayColor)-1 do
    t+=' ' + FormatFloat('0.00000', ArrayColor[i].Life) + ' ' +
       inttostr(ArrayColor[i].C.red) + ' ' +
       inttostr(ArrayColor[i].C.green) + ' ' +
       inttostr(ArrayColor[i].C.blue) + ' ' +
       inttostr(ArrayColor[i].C.alpha);
 end;
 s.Add( t );

 s.Add('P_Velocity');
 t := inttostr( TB10.Position )+' '+inttostr( TB11.Position );
 with FPEngine.FParticleParam do begin
  t += ' '+inttostr(Length(ArrayVelocity));
  for i:=0 to Length(ArrayVelocity)-1 do
    t+=' ' + FormatFloat('0.00000', ArrayVelocity[i].Life) + ' ' +
             FormatFloat('0.00000',ArrayVelocity[i].Value);
 end;
 s.Add( t );

 s.Add('P_AVelocity');
 t := inttostr( TB16.Position )+' '+inttostr( TB17.Position );
 with FPEngine.FParticleParam do begin
  t += ' '+inttostr(Length(ArrayAVelocity));
  for i:=0 to Length(ArrayAVelocity)-1 do
    t+=' ' + FormatFloat('0.00000', ArrayAVelocity[i].Life) + ' ' +
             FormatFloat('0.00000',ArrayAVelocity[i].Value);
 end;
 s.Add( t );

 s.Add('P_Size');
 TexWidth := strtoint(Edit1.Text);
 t := FormatFloat('0.00000', TB14.Position/TexWidth )+' '+
      FormatFloat('0.00000', TB15.Position/TexWidth );
 with FPEngine.FParticleParam do begin
  t += ' '+inttostr(Length(ArraySize));
  for i:=0 to Length(ArraySize)-1 do
    t+=' ' + FormatFloat('0.00000', ArraySize[i].Life) + ' ' +
             FormatFloat('0.00000',ArraySize[i].Value);
 end;
 s.Add( t );

 s.Add('P_StartAngle');
 s.Add( inttostr(TB7.Position)+' '+inttostr(TB8.Position));

 s.Add('P_Spin');
 t := inttostr( TB12.Position )+' '+inttostr( TB13.Position );
 with FPEngine.FParticleParam do begin
  t += ' '+inttostr(Length(ArraySpin));
  for i:=0 to Length(ArraySpin)-1 do
    t+=' ' + FormatFloat('0.00000', ArraySpin[i].Life) + ' ' +
             FormatFloat('0.00000',ArraySpin[i].Value);
 end;
 s.Add( t );

 try
  s.SaveToFile(SD1.FileName);
 finally
  s.Free;
 end;
 SetWindowTitle( FALSE );
end;

procedure TForm_Principale.MenuItem6Click(Sender: TObject);
begin
 Close;
end;

procedure TForm_Principale.OpenGLControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FPEngine.SetCoordinate(X,Y);
end;

// radio button 'Color/image' for background
procedure TForm_Principale.RB1Change(Sender: TObject);
begin
 FBackGroundColor.Visible := RB1.Checked;
 if FBackgroundImage<>NIL then FBackgroundImage.Visible := RB2.Checked;
 FBackGroundRainbow.Visible := RB3.Checked;
end;

// frame index
procedure TForm_Principale.SE1Change(Sender: TObject);
var p: pinteger;
begin
 p := @FPEngine.FParticleParam.StartFrame;
 p^ := SE1.Value;
 p := @FPEngine.FParticleParam.EndFrame;
 p^ := SE2.Value;
 SetWindowTitle( TRUE );
end;

// Gravity
procedure TForm_Principale.SE3Change(Sender: TObject);
begin
 FPEngine.Gravity.Value := PointF(SE3.Value, SE4.Value);
 SetWindowTitle( TRUE );
end;

// Velocity/Variation
procedure TForm_Principale.TB10Change(Sender: TObject);
var p: psingle;
begin
 Label34.Caption:=inttostr(TB10.Position)+' px/sec';
 p := @FPEngine.FParticleParam.Velocity;
 p^ := TB10.Position;

 Label36.Caption:=inttostr(TB11.Position)+' px/sec';
 p := @FPEngine.FParticleParam.VelocityVariation;
 p^ := TB11.Position;
 SetWindowTitle( TRUE );
end;

// Spin/Variation
procedure TForm_Principale.TB12Change(Sender: TObject);
begin
 Label38.Caption := inttostr(TB12.Position)+' Deg/sec';
 Label40.Caption := inttostr(TB13.Position)+' Deg/sec';
 FPEngine.FParticleParam.Spin:= TB12.Position;
 FPEngine.FParticleParam.SpinVariation := TB13.Position;
 SetWindowTitle( TRUE );
end;

// Size/Variation
procedure TForm_Principale.TB14Change(Sender: TObject);
var i:integer;
begin
 i := strtoint(Edit1.Text);
 Label42.Caption := inttostr(TB14.Position)+' pixels';
 Label44.Caption := inttostr(TB15.Position)+' pixels';

 FPEngine.FParticleParam.Size:=TB14.Position/i;
 FPEngine.FParticleParam.SizeVariation:=TB15.Position/i;
 {
  i := strtoint(Edit1.Text);
  Label42.Caption := inttostr(TB14.Position)+'% = '+FormatFloat('0.00',TB14.Position*0.01*i)+' pixels';
  Label44.Caption := inttostr(TB15.Position)+'% = '+FormatFloat('0.00',TB15.Position*0.01*i)+' pixels';

  FPEngine.FParticleParam.Size:=TB14.Position*0.01;
  FPEngine.FParticleParam.SizeVariation:=TB15.Position*0.01;
 }
 SetWindowTitle( TRUE );
end;

// Angular Velocity/Variation
procedure TForm_Principale.TB16Change(Sender: TObject);
begin
 Label46.Caption := FormatFloat('0.000', TB16.Position)+' Deg/sec';
 Label48.Caption := FormatFloat('0.000', TB17.Position)+' Deg/sec';

 FPEngine.FParticleParam.AVelocity:=TB16.Position;
 FPEngine.FParticleParam.AVelocityVariation:=TB17.Position;
 SetWindowTitle( TRUE );
end;

// Emitter life, Direction, Spread
procedure TForm_Principale.TB1Change(Sender: TObject);
begin
 Label10.Caption := inttostr( TB1.Position ) + ' ms';
 FPEngine.EmitterLife := TB1.Position*0.001;

 Label11.Caption := inttostr ( TB2.Position ) + ' Deg';
 FPEngine.Direction.Value := TB2.Position;

 Label12.Caption := inttostr ( TB3.Position ) + ' Deg';
 FPEngine.Spread.Value := TB3.Position;

 SetWindowTitle( TRUE );
end;

procedure TForm_Principale.TB20Change(Sender: TObject);
begin
 Label55.Caption := 'Global opacity '+inttostr(TB20.Position)+'%';
 FPEngine.Opacity.pcValue := TB20.Position*0.01;
 SetWindowTitle( TRUE );
end;

// Emission
procedure TForm_Principale.TB4Change(Sender: TObject);
begin
 SetWindowTitle( TRUE );
 Label16.Caption := inttostr( TB4.Position )+' particles/sec';
 FPEngine.ParticlesToEmit.Value := TB4.Position;
end;

// Life
procedure TForm_Principale.TB5Change(Sender: TObject);
var p: psingle;
begin
 Label17.Caption := FormatFloat('0.000', TB5.Position*0.001)+' sec';
 Label20.Caption := FormatFloat('0.000', TB6.Position*0.001)+' sec';
 p := @FPEngine.FParticleParam.Life;
 p^ := TB5.Position*0.001;
 p := @FPEngine.FParticleParam.LifeVariation;
 p^ := TB6.Position*0.001;
 SetWindowTitle( TRUE );
end;

// Start Angle
procedure TForm_Principale.TB7Change(Sender: TObject);
var p: psingle;
begin
 Label21.Caption := inttostr( TB7.Position )+' deg';
 Label24.Caption := inttostr( TB8.Position )+' deg';
 p := @FPEngine.FParticleParam.StartAngle;
 p^ := TB7.Position;
 p := @FPEngine.FParticleParam.StartAngleVariation;
 p^ := TB8.Position;
 SetWindowTitle( TRUE );
end;

// camera zoom
procedure TForm_Principale.TBZoomChange(Sender: TObject);
var s:single;
begin
 s := TBZoom.Position*0.001;
 FScene.Camera.Scale.Value := PointF(s,s);
 Label32.Caption:=FormatFloat('0.00',s);
end;

procedure TForm_Principale.LoadCommonData;
begin
 self.DoubleBuffered:=TRUE;
 // load common data here
 HomeScreen := THomeScreen.Create;
 FScene.LaunchStage( HomeScreen );

 FPEngine := TParticleEmitter.Create;
 FScene.Add( FPEngine, LAYER_PARTICLE );

 VelocityCurve.OnCurveChange:=@ProcessVelocityCurveChange;
 VelocityCurve.SetLegendMinMax('-200%','+200%');
 VelocityCurve.AddHorizAxis([0.25, 0.5, 0.75]);

 SpinCurve.OnCurveChange:=@ProcessSpinCurveChange;
 SpinCurve.SetLegendMinMax('-200%','+200%');
 SpinCurve.AddHorizAxis([0.25, 0.5, 0.75]);

 SizeCurve.OnCurveChange:=@ProcessSizeCurveChange;
 SizeCurve.SetLegendMinMax('0%', '+400%');
 SizeCurve.AddHorizAxis([0.25, 0.5, 0.75]);

 AngularVelocityCurve.OnCurveChange:=@ProcessAngularVelocityCurveChange;
 AngularVelocityCurve.SetLegendMinMax('-200%', '+200%');
 AngularVelocityCurve.AddHorizAxis([0.25, 0.5, 0.75]);

 ColorCurve.OnCurveChange:=@ProcessColorCurveChange;
 ColorCurve.OnPointDblClick:=@ProcessColorCurvePointDblClick;
 ColorCurve.OnAddPoint:=@ProcessColorCurveAddPoint;
 ColorCurve.OnDeletePoint:=@ProcessColorCurveDeletePoint;
 ColorCurve.SetLegendMinMax('alpha 0','alpha 255');

 LoadFile('Examples'+DirectorySeparator+'Smoke.par');
 OD2.FileName := Label4.Caption;

 ParticleEngineParamToUserScreenParam;
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
 Frame2_1.FreeData;
end;

procedure TForm_Principale.ProcessApplicationIdle(Sender: TObject; var Done: Boolean);
begin
 FScene.DoLoop;
 Done := FALSE;
end;

procedure TForm_Principale.SetWindowTitle( Modified: boolean );
var txt: string;
begin
 FModified := Modified;
 txt := 'OGLC Particles Editor - '+FProjectName;
 if FModified then txt +='*';
 Caption := txt;
end;

procedure TForm_Principale.LoadFile(const aFile: string);
var s: TStringList;
  k,i: integer;
  SplittedText: ArrayOfString;
begin
 s := TStringList.Create;
 try
  s.LoadFromFile(aFile);

  k := s.IndexOf('E_LoopMode');
  CheckBox1.Checked := s.Strings[k+1]='TRUE';

  k := s.IndexOf('E_Life');
  TB1.Position := round(strtoFloat( s.Strings[k+1] )*1000);

  k := s.IndexOf('E_Direction');
  TB2.Position := strtoint( s.Strings[k+1] );

  k := s.IndexOf('E_Spread');
  TB3.Position := strtoint( s.Strings[k+1] );

  k := s.IndexOf('E_Emission');
  TB4.Position := strtoint( s.Strings[k+1] );
  Label16.Caption := s.Strings[k+1]+' particles/sec';

  k := s.IndexOf('E_GlobalOpacity');
  if k<>-1
    then TB20.Position:=strtoint(s.Strings[k+1])
    else TB20.Position:=100;

  k := s.IndexOf('E_Type');
  if k<>-1 then begin
   SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
   ComboBox1.ItemIndex:=strtoint(SplittedText[0]);
   case strtoint( SplittedText[0] ) of
    0:;  // point
    1: Edit5.Text := SplittedText[1];  // line length
    2: begin
     Edit3.Text := SplittedText[1];   //rectangle width height
     Edit4.Text := SplittedText[2];
    end;
    3,5,6: Edit7.Text := SplittedText[1]; // circle radius
    4: begin
     Edit9.Text := SplittedText[1];   // ring smallRadius bigRadius
     Edit8.Text := SplittedText[2];
    end;
   end;//case
  end else ComboBox1.ItemIndex:=0;
  ComboBox1Change(NIL);

  k := s.IndexOf('P_Texture');
  Label4.Caption := s.Strings[k+1];
  SplittedText := SplitLineToStringArray( s.Strings[k+2], ' ' );
  Edit1.Text := SplittedText[0];
  Edit2.Text := SplittedText[1];

  k := s.IndexOf('P_Frame');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  SE1.Value := strtoint( SplittedText[0] );
  SE2.Value := strtoint( SplittedText[1] );

  k := s.IndexOf('P_Life');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  TB5.Position := round(strtofloat( SplittedText[0] )*1000);
  TB6.Position := round(strtofloat( SplittedText[1] )*1000);

  k := s.IndexOf('P_Velocity');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  TB10.Position := strtoint(SplittedText[0]);
  TB11.Position := strtoint(SplittedText[1]);
  VelocityCurve.Clear;
  k := strtoint(SplittedText[2]);  // point count
  i := 3;
  while k>0 do begin
   VelocityCurve.AddPoint(StrToFloat(SplittedText[i]), (StrToFloat(SplittedText[i+1])+2)/4, FALSE);
   inc(i,2);
   dec(k);
  end;

  k := s.IndexOf('P_Spin');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  TB12.Position := strtoint(SplittedText[0]);
  TB13.Position := strtoint(SplittedText[1]);
  SpinCurve.Clear;
  k := strtoint(SplittedText[2]);  // point count
  i := 3;
  while k>0 do begin
   SpinCurve.AddPoint(StrToFloat(SplittedText[i]), (StrToFloat(SplittedText[i+1])+2)/4, FALSE);
   inc(i,2);
   dec(k);
  end;

  k := s.IndexOf('P_Size');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  TB14.Position := round(StrToFloat(SplittedText[0])*100);
  TB15.Position := round(StrToFloat(SplittedText[1])*100);
  SizeCurve.Clear;
  k := strtoint(SplittedText[2]);  // point count
  i := 3;
  while k>0 do begin
   SizeCurve.AddPoint(StrToFloat(SplittedText[i]), StrToFloat(SplittedText[i+1])/4, FALSE);
   inc(i,2);
   dec(k);
  end;

  k := s.IndexOf('P_AVelocity');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  TB16.Position := round(StrToFloat(SplittedText[0]));
  TB17.Position := round(StrToFloat(SplittedText[1]));
  AngularVelocityCurve.Clear;
  k := strtoint(SplittedText[2]);  // point count
  i := 3;
  while k>0 do begin
   AngularVelocityCurve.AddPoint(StrToFloat(SplittedText[i]), (StrToFloat(SplittedText[i+1])+2)/4, FALSE);
   inc(i,2);
   dec(k);
  end;

  k := s.IndexOf('P_ColorMode');
  CBColorMode.ItemIndex := strtoint(s.Strings[k+1]);

  k := s.IndexOf('P_BlendMode');
  CBBlend.ItemIndex := strtoint(s.Strings[k+1]);

  k := s.IndexOf('P_Color');
  SplittedText := SplitLineToStringArray( s.Strings[k+1], ' ' );
  k := strtoint(SplittedText[0]);  // point count
  ColorCurve.Clear;
  i := 1;
  while k>0 do begin
   ColorCurve.AddPoint(StrToFloat(SplittedText[i]), StrToFloat(SplittedText[i+4])/255, FALSE);
   inc(i,5);
   dec(k);
  end;

 finally
  s.Free;
 end;
 SetWindowTitle( FALSE );
 FPEngine.LoadFromFile(aFile);
 FPEngine.CenterOnScene;
 Frame2_1.UpdateColor( FPEngine.FParticleParam.ArrayColor );
end;

procedure TForm_Principale.ParticleEngineParamToUserScreenParam;
begin
 FPEngineLocked := TRUE;  // disable particle engine refresh


 FPEngineLocked := FALSE;
end;

procedure TForm_Principale.RestartParticleEngine;
begin
 if FPEngineLocked then exit;
end;

procedure TForm_Principale.ProcessVelocityCurveChange(p: psingle; count: integer);
var i: integer;
begin
 SetLength( FPEngine.FParticleParam.ArrayVelocity, count );
 i := 0;
 while i<count do
  begin
   FPEngine.FParticleParam.ArrayVelocity[i].Life:=p^;
   inc(p);
   FPEngine.FParticleParam.ArrayVelocity[i].Value:=p^*4-2;
   inc(p);
   inc(i);
  end;
end;

procedure TForm_Principale.ProcessSpinCurveChange(p: psingle; count: integer);
var i: integer;
begin
 SetLength( FPEngine.FParticleParam.ArraySpin, count );
 i := 0;
 while i<count do
  begin
   FPEngine.FParticleParam.ArraySpin[i].Life:=p^;
   inc(p);
   FPEngine.FParticleParam.ArraySpin[i].Value:=p^*4-2;
   inc(p);
   inc(i);
  end;
end;

procedure TForm_Principale.ProcessSizeCurveChange(p: psingle; count: integer);
var i: integer;
begin
 SetLength( FPEngine.FParticleParam.ArraySize, count );
 i := 0;
 while i<count do
  begin
   FPEngine.FParticleParam.ArraySize[i].Life:=p^;
   inc(p);
   FPEngine.FParticleParam.ArraySize[i].Value:=p^*4;
   inc(p);
   inc(i);
  end;
end;

procedure TForm_Principale.ProcessAngularVelocityCurveChange(p: psingle;
  count: integer);
var i: integer;
begin
 SetLength( FPEngine.FParticleParam.ArrayAVelocity, count );
 i := 0;
 while i<count do
  begin
   FPEngine.FParticleParam.ArrayAVelocity[i].Life:=p^;
   inc(p);
   FPEngine.FParticleParam.ArrayAVelocity[i].Value:=p^*4-2;
   inc(p);
   inc(i);
  end;
end;

procedure TForm_Principale.ProcessColorCurveChange(p: psingle; count: integer );
var i: integer;
begin
 SetLength( FPEngine.FParticleParam.ArrayColor, count );
 i := 0;
 while i<count do
  begin
   FPEngine.FParticleParam.ArrayColor[i].Life:=p^;
   inc(p);
   FPEngine.FParticleParam.ArrayColor[i].C.alpha := round(p^*255);
   inc(p);
   inc(i);
  end;
 Frame2_1.UpdateColor( FPEngine.FParticleParam.ArrayColor );
end;

procedure TForm_Principale.ProcessColorCurvePointDblClick(PointIndex: integer);
var c: TBGRAPixel;
begin
 if not CD1.Execute then exit;
 c := ColorToBGRA( CD1.Color );

 FPEngine.FParticleParam.ArrayColor[PointIndex].C.red := c.red;
 FPEngine.FParticleParam.ArrayColor[PointIndex].C.green := c.green;
 FPEngine.FParticleParam.ArrayColor[PointIndex].C.blue := c.blue;
 ColorCurve.Invalidate;
 Frame2_1.UpdateColor( FPEngine.FParticleParam.ArrayColor );
end;

procedure TForm_Principale.ProcessColorCurveAddPoint(PointIndex: integer;
  P: TPointF);
var i: integer;
begin
 // insert new color in particle emitter
 with FPEngine.FParticleParam do begin
  SetLength( ArrayColor, Length(FPEngine.FParticleParam.ArrayColor)+1 );
  if Length(ArrayColor)>1 then begin
        if PointIndex<Length(ArrayColor)-1
          then for i:=Length(ArrayColor)-2 downto PointIndex do begin
                ArrayColor[i+1].Life := ArrayColor[i].Life;
                ArrayColor[i+1].C := ArrayColor[i].C;
          end;
    end;
  ArrayColor[PointIndex].Life := P.x;
  ArrayColor[PointIndex].C := ColorToBGRA( CD1.Color, round(255*P.y) );

  Frame2_1.UpdateColor( ArrayColor );
 end;
end;

procedure TForm_Principale.ProcessColorCurveDeletePoint(PointIndex: integer);
var i:integer;
begin
 with FPEngine.FParticleParam do begin
  for i:=PointIndex to Length(ArrayColor)-2 do begin
   ArrayColor[i].Life := ArrayColor[i+1].Life;
   ArrayColor[i].C := ArrayColor[i+1].C;
  end;
  SetLength( ArrayColor, Length(ArrayColor)-1 );

  Frame2_1.UpdateColor( ArrayColor );
 end;

end;


end.

