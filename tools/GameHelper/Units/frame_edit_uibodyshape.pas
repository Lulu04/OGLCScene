unit frame_edit_uibodyshape;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs, Spin,
  Buttons,
  OGLCScene, BGRABitmap, BGRABitmapTypes;

type

  { TFrameEditUIBodyShape }

  TFrameEditUIBodyShape = class(TFrame)
    CBFill: TColorButton;
    CBBorderBlend: TComboBox;
    CBFillBlend: TComboBox;
    CBBorderVisible: TCheckBox;
    CBBorder: TColorButton;
    CBFillVisible: TCheckBox;
    ComboBox1: TComboBox;
    FSEBorderWidth: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    RBFillSingleColor: TRadioButton;
    RBFillGradient: TRadioButton;
    SEBorderAlpha: TSpinEdit;
    SEFillAlpha: TSpinEdit;
    BEditGradient: TSpeedButton;
    procedure CBBorderVisibleChange(Sender: TObject);
    procedure CBFillVisibleChange(Sender: TObject);
    procedure BEditGradientClick(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FTargetSurface: TUIClickableWithBodyShape;
    FInitializing: boolean;
    procedure ProcessGradientChange(Sender: TObject);
    function GetBorderBlendMode: byte;
    function GetBorderColor: TBGRAPixel;
    function GetBorderWidth: single;
    function GetBorderIsVisible: boolean;
    procedure SetBorderBlendMode(AValue: byte);
    procedure SetBorderColor(AValue: TBGRAPixel);
    procedure SetBorderWidth(AValue: single);
    procedure SetBorderIsVisible(AValue: boolean);

  public
    property BorderIsVisible: boolean read GetBorderIsVisible write SetBorderIsVisible;
    property BorderColor: TBGRAPixel read GetBorderColor write SetBorderColor;
    property BorderWidth: single read GetBorderWidth write SetBorderWidth;
    property BorderBlendMode: byte read GetBorderBlendMode write SetBorderBlendMode;

    procedure UpdateFromSurface(aSurface: TUIClickableWithBodyShape);

    property TargetSurface: TUIClickableWithBodyShape read FTargetSurface write FTargetSurface;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses form_editgradient;

{$R *.lfm}

{ TFrameEditUIBodyShape }

procedure TFrameEditUIBodyShape.CBBorderVisibleChange(Sender: TObject);
begin
  Label3.Enabled := CBBorderVisible.Checked;
  CBBorder.Enabled := CBBorderVisible.Checked;
  Label4.Enabled := CBBorderVisible.Checked;
  SEBorderAlpha.Enabled := CBBorderVisible.Checked;
  Label5.Enabled := CBBorderVisible.Checked;
  FSEBorderWidth.Enabled := CBBorderVisible.Checked;
  Label6.Enabled := CBBorderVisible.Checked;
  CBBorderBlend.Enabled := CBBorderVisible.Checked;
  Label10.Enabled := CBBorderVisible.Checked;
  ComboBox1.Enabled := CBBorderVisible.Checked;


  if FInitializing then exit;

  if Assigned(FTargetSurface) then begin
    FTargetSurface.BodyShape.Border.Visible := CBBorderVisible.Checked;
    FTargetSurface.BodyShape.Border.Color := ColorToBGRA(CBBorder.ButtonColor, SEBorderAlpha.Value);
    FTargetSurface.BodyShape.Border.Width := FSEBorderWidth.Value;
    FTargetSurface.BodyShape.Border.BlendMode := byte(CBBorderBlend.ItemIndex);
    FTargetSurface.BodyShape.Border.LinePosition := TOGLCLinePosition(ComboBox1.ItemIndex);
  end;

  if Assigned(FOnChange) then FOnchange(Self);
end;

procedure TFrameEditUIBodyShape.CBFillVisibleChange(Sender: TObject);
begin
  RBFillSingleColor.Enabled := CBFillVisible.Checked;
  RBFillGradient.Enabled := CBFillVisible.Checked;
  BEditGradient.Enabled := CBFillVisible.Checked and RBFillGradient.Checked;
  Label7.Enabled := CBFillVisible.Checked and RBFillSingleColor.Checked;
  CBFill.Enabled := Label7.Enabled;
  Label8.Enabled := Label7.Enabled;
  SEFillAlpha.Enabled := Label7.Enabled;
  Label9.Enabled := CBFillVisible.Checked;
  CBFillBlend.Enabled := Label9.Enabled;

  if FInitializing then exit;

  if Assigned(FTargetSurface) then begin
    FTargetSurface.BodyShape.Fill.Visible := CBFillVisible.Checked and RBFillSingleColor.Checked;
    FTargetSurface.BodyShape.Fill.Color := ColorToBGRA(CBFill.ButtonColor, SEFillAlpha.Value);
    FTargetSurface.BodyShape.Fill.BlendMode := byte(CBFillBlend.ItemIndex);

    FTargetSurface.BackGradient.Visible := CBFillVisible.Checked and RBFillGradient.Checked;
    if FTargetSurface.BackGradient.Visible and not FTargetSurface.BackGradient.IsInitialized then
      FTargetSurface.BackGradient.CreateHorizontal([BGRA(100,50,255,100), BGRA(255,0,255,200), BGRA(50,0,255,100)], [0.0, 0.5, 1.0]);
  end;

  if Assigned(FOnChange) then FOnchange(Self);
end;

procedure TFrameEditUIBodyShape.BEditGradientClick(Sender: TObject);
begin
  FormEditGradient := TFormEditGradient.Create(NIL);
  try
    FormEditGradient.OnChange := @ProcessGradientChange;
    FormEditGradient.Edit(@FTargetSurface.BackGradient, FTargetSurface.Width, FTargetSurface.Height);
    FormEditGradient.ShowModal;
  finally
    FormEditGradient.Free;
  end;
end;

procedure TFrameEditUIBodyShape.ProcessGradientChange(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TFrameEditUIBodyShape.GetBorderBlendMode: byte;
begin
  Result := CBBorderBlend.ItemIndex;
end;

function TFrameEditUIBodyShape.GetBorderColor: TBGRAPixel;
begin
  Result := ColorToBGRA(CBBorder.ButtonColor, SEBorderAlpha.Value);
end;

function TFrameEditUIBodyShape.GetBorderWidth: single;
begin
  Result := FSEBorderWidth.Value;
end;

function TFrameEditUIBodyShape.GetBorderIsVisible: boolean;
begin
  Result := CBBorderVisible.Checked;
end;

procedure TFrameEditUIBodyShape.SetBorderBlendMode(AValue: byte);
begin
  if AValue > 2 then AValue := 2;
  CBBorderBlend.ItemIndex := AValue;
end;

procedure TFrameEditUIBodyShape.SetBorderColor(AValue: TBGRAPixel);
begin
  CBBorder.ButtonColor := AValue.ToColor;
  SEBorderAlpha.Value := AValue.alpha;
end;

procedure TFrameEditUIBodyShape.SetBorderWidth(AValue: single);
begin
  FSEBorderWidth.Value := AValue;
end;

procedure TFrameEditUIBodyShape.SetBorderIsVisible(AValue: boolean);
begin
  CBBorderVisible.Checked := AValue;
end;

procedure TFrameEditUIBodyShape.UpdateFromSurface(aSurface: TUIClickableWithBodyShape);
begin
  FInitializing := True;

  TargetSurface := aSurface;

  CBBorderVisible.Checked := aSurface.BodyShape.Border.Visible;
  CBBorder.ButtonColor := aSurface.BodyShape.Border.Color.ToColor;
  SEBorderAlpha.Value := aSurface.BodyShape.Border.Color.alpha;
  FSEBorderWidth.Value := aSurface.BodyShape.Border.Width;
  CBBorderBlend.ItemIndex := aSurface.BodyShape.Border.BlendMode;
  ComboBox1.ItemIndex := Ord(aSurface.BodyShape.Border.LinePosition);

  CBFillVisible.Checked := aSurface.BodyShape.Fill.Visible or
                           aSurface.BackGradient.Visible;
  RBFillSingleColor.Checked := aSurface.BodyShape.Fill.Visible and
                               not aSurface.BackGradient.Visible;
  RBFillGradient.Checked := aSurface.BackGradient.Visible;
  CBFill.ButtonColor := aSurface.BodyShape.Fill.Color.ToColor;
  SEFillAlpha.Value := aSurface.BodyShape.Fill.Color.alpha;
  CBFillBlend.ItemIndex := aSurface.BodyShape.Fill.BlendMode;

  FInitializing := False;
end;

end.

