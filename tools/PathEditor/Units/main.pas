unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ExtDlgs, Buttons, Menus, Arrow, Spin,
  BGRABitmap, BGRABitmapTypes,
  Frame_CurveEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Arrow1: TArrow;
    Arrow2: TArrow;
    Arrow3: TArrow;
    Arrow4: TArrow;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Button1: TButton;
    CBActivateSplineMode: TCheckBox;
    CBShowGrid: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OPD1: TOpenPictureDialog;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    RBAspectRatio: TRadioGroup;
    SD1: TSaveDialog;
    BHelp: TSpeedButton;
    SEScreenWidth: TSpinEdit;
    SEScreenHeight: TSpinEdit;
    TBGrid: TTrackBar;
    TBScale: TTrackBar;
    TBOpacity: TTrackBar;
    Timer1: TTimer;
    procedure Arrow2Click(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBActivateSplineModeChange(Sender: TObject);
    procedure CBShowGridChange(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure RBAspectRatioSelectionChanged(Sender: TObject);
    procedure TBOpacityChange(Sender: TObject);
    procedure TBScaleChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    function GetScaleFactor: single;
    procedure ComputeBackgroundImageScaled;
    procedure DrawGraphBackground( BG: TBGRABitmap );
    procedure SetSizeAndPositionOfCurveFrame;
  private
    FrameEditCurve: TFrameEditCurve;
    FLoading: boolean;
    procedure ProcessMouseOverNode(P: PPointF);
    procedure ProcessNodeDoubleClick(P: PPointF);
  public

  end;

var
  Form1: TForm1;

implementation
uses LCLType, form_help, form_entercoordinates;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
 // force decimal separator to '.'
 SysUtils.FormatSettings.DecimalSeparator := '.';

 FrameEditCurve := TFrameEditCurve.Create(Self);
 FrameEditCurve.Parent := Panel2;
 FrameEditCurve.ShowNodes := True;
 FrameEditCurve.OnMouseOverNode := @ProcessMouseOverNode;
 FrameEditCurve.OnPointDblClick := @ProcessNodeDoubleClick;
 FrameEditCurve.SetGridSize(TBGrid.Position);
 FrameEditCurve.ShowGrid := CBShowGrid.Checked;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_BACK: FrameEditCurve.DeleteLastPoint;  // delete the last point
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetSizeAndPositionOfCurveFrame;
end;

procedure TForm1.RBAspectRatioSelectionChanged(Sender: TObject);
begin
  SetSizeAndPositionOfCurveFrame;
end;

// load image from disk
procedure TForm1.Button1Click(Sender: TObject);
begin
{ if not OPD1.Execute then exit;
 if FBackgroundImageNotScaled<>NIL then FBackgroundImageNotScaled.Free;
 FBackgroundImageNotScaled := TBGRABitmap.Create( OPD1.FileName );

 ComputeBackgroundImageScaled;  }
end;

procedure TForm1.CBActivateSplineModeChange(Sender: TObject);
begin
  if FLoading then exit;
  if CBActivateSplineMode.Checked then
    FrameEditCurve.ActivateSplineMode(TSplineStyle(ComboBox1.ItemIndex))
  else
    FrameEditCurve.DeactivateSplineMode;
end;

procedure TForm1.CBShowGridChange(Sender: TObject);
begin
  FrameEditCurve.ShowGrid := CBShowGrid.Checked;
  FrameEditCurve.SetGridSize(TBGrid.Position);
  Label1.Caption := 'size: ' + TBGrid.Position.ToString;
end;

// Clear All
procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if QuestionDlg('', 'Delete all nodes ?', mtWarning, [mrOk, 'Ok', mrCancel, 'Cancel'], 0) = mrOk then
    FrameEditCurve.Clear;
end;

// Global up arrow
procedure TForm1.Arrow2Click(Sender: TObject);
var delta: integer;
begin
  if CBShowGrid.Checked
    then delta := TBGrid.Position
    else delta := 1;

  if Sender = Arrow2
    then FrameEditCurve.MoveCurve(0, -delta/FrameEditCurve.Height)
  else if Sender = Arrow1
    then FrameEditCurve.MoveCurve(0, delta/FrameEditCurve.Height)
  else if Sender = Arrow3
    then FrameEditCurve.MoveCurve(delta/FrameEditCurve.Width, 0)
  else FrameEditCurve.MoveCurve(-delta/FrameEditCurve.Width, 0);
end;

procedure TForm1.BHelpClick(Sender: TObject);
begin
  FormHelp.Show;
end;

// Open an existing drawing
procedure TForm1.BitBtn2Click(Sender: TObject);
begin
 if not OD1.Execute then exit;
 SD1.FileName := OD1.FileName;
 FrameEditCurve.LoadFromFile(OD1.FileName);
 FLoading := True;
 CBActivateSplineMode.Checked := FrameEditCurve.SplineModeActivated;
 ComboBox1.ItemIndex := Ord(FrameEditCurve.SplineStyle);
 FLoading := False;
end;

// Save current drawing
procedure TForm1.BitBtn3Click(Sender: TObject);
begin
 if not SD1.Execute then exit;
 FrameEditCurve.SaveToFile(SD1.FileName);
end;

// Show/hide points
procedure TForm1.CheckBox4Change(Sender: TObject);
begin
 FrameEditCurve.ShowNodes := CheckBox4.Checked;
end;

// Image opacity
procedure TForm1.TBOpacityChange(Sender: TObject);
begin
{ Label5.Caption:='opacity: '+inttostr(TBOpacity.Position);
 PB.Invalidate;  }
end;

// Image scale
procedure TForm1.TBScaleChange(Sender: TObject);
begin
 Label2.Caption := 'scale factor: '+FormatFloat('0.00', GetScaleFactor);
 ComputeBackgroundImageScaled;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Label6.Caption:='Point count : '+FrameEditCurve.NodeCount.ToString;
end;

function TForm1.GetScaleFactor: single;
begin
 Result := TBScale.Position*0.1;
end;

procedure TForm1.ComputeBackgroundImageScaled;
var ima: TBGRABitmap;
  xx,yy: integer;
begin
{ ima := FBackgroundImageNotScaled.Resample(round(FBackgroundImageNotScaled.Width*GetScaleFactor),
                                           round(FBackgroundImageNotScaled.Height*GetScaleFactor)) as TBGRABitmap;
 xx := (FBackgroundImageScaled.Width-ima.Width) div 2;
 yy := (FBackgroundImageScaled.Height-ima.Height) div 2;
 FBackgroundImageScaled.Fill( BGRAPixelTRansparent );
 FBackgroundImageScaled.PutImage( xx, yy, ima, dmDrawWithTransparency );
 PB.Invalidate; }
end;

procedure TForm1.DrawGraphBackground(BG: TBGRABitmap);
begin
{ BG.Fill(BGRA(50,20,20));
 if CheckBox3.Checked then
   BG.PutImage(0, 0, FBackgroundImageScaled, dmDrawWithTransparency, TBOpacity.Position ); }
end;

procedure TForm1.SetSizeAndPositionOfCurveFrame;
var w, h, x, y: integer;
begin
 case RBAspectRatio.ItemIndex of
   0: begin
     w := Panel1.Left - 1;
     h := Trunc(w / (4/3));
     x := 0;
     y := (ClientRect.Height - h) div 2;
     FrameEditCurve.SetBounds(x, y, w, h);
     SEScreenWidth.Value := 1024;
     SEScreenHeight.Value := 768;
   end;
   1: begin
     w := Panel1.Left - 1;
     h := Trunc(w / (16/9));
     x := 0;
     y := (ClientRect.Height - h) div 2;
     FrameEditCurve.SetBounds(x, y, w, h);
     SEScreenWidth.Value := 1280;
     SEScreenHeight.Value := 720;
   end;
   2: begin
     FrameEditCurve.SetBounds(0, 0, Panel1.Left - 1, ClientRect.Height);
     SEScreenWidth.Value := 1024;
     SEScreenHeight.Value := 1024;
   end;
 end;
end;

procedure TForm1.ProcessMouseOverNode(P: PPointF);
begin
  Label12.Caption := Round(SEScreenWidth.Value*P^.x).ToString;
  Label13.Caption := Round(SEScreenHeight.Value*P^.y).ToString;
end;

procedure TForm1.ProcessNodeDoubleClick(P: PPointF);
begin
  FormEnterCoordinate := TFormEnterCoordinate.Create(NIL);
  FormEnterCoordinate.SetScreenResolution(SEScreenWidth.Value, SEScreenHeight.Value);
  FormEnterCoordinate.SEX.Value := Round(P^.x*SEScreenWidth.Value);
  FormEnterCoordinate.SEY.Value := Round(P^.y*SEScreenHeight.Value);
  if FormEnterCoordinate.ShowModal = mrOk then
    P^ := FormEnterCoordinate.GetResultPoint;
  FormEnterCoordinate.Free;
end;

end.

