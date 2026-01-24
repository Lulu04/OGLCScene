unit form_edituibodyshape;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin,
  OGLCScene, BGRABitmap, BGRABitmapTypes,
  u_presetmanager, frame_edit_uibodyshape;

type

  { TFormEditBodyShape }

  TFormEditBodyShape = class(TForm)
    BPreset: TSpeedButton;
    CBBottomLeft: TComboBox;
    CBBottomRight: TComboBox;
    CBTopLeft: TComboBox;
    CBTopRight: TComboBox;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RBEllipse: TRadioButton;
    RBRectangle: TRadioButton;
    RBRoundRect: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSE1Change(Sender: TObject);
  private
    FPresets: TPresetManager;
    procedure PresetToWidget(const data: string);
    function WidgetToPreset: string;
  private
    FrameEditUIBodyShape: TFrameEditUIBodyShape;
    procedure ProcessFrameSomethingChange(Sender: TObject);
  private
    FTargetSurface: TUIClickableWithBodyShape;
    FModified: boolean;
    FInitializing: Boolean;
    function WidgetToShapeType: OGLCScene.TShapetype;
    procedure ShapeTypeToWidget(aShapeType: OGLCScene.TShapetype);
    function CBToRoundRectOptions: TRoundRectangleOptions;
    procedure RoundRectOptionsToCB(aOptions: TRoundRectangleOptions);
  public
    procedure Edit(aSurface: TUIClickableWithBodyShape);
    property Modified: boolean read FModified;
  end;

var
  FormEditBodyShape: TFormEditBodyShape;

implementation

uses form_main, u_project;

{$R *.lfm}

{ TFormEditBodyShape }

procedure TFormEditBodyShape.FormCreate(Sender: TObject);
begin
  FrameEditUIBodyShape := TFrameEditUIBodyShape.Create(Self);
  FrameEditUIBodyShape.Parent := Panel2;
  FrameEditUIBodyShape.Align := alClient;
  FrameEditUIBodyShape.OnChange := @ProcessFrameSomethingChange;

  FPresets := TPresetManager.Create(Self);
  FPresets.Init1('UI BodyShape presets', BPreset, Project.FolderUserPreset+'UIBodyShape.preset');
  FPresets.Init2(@PresetToWidget, @WidgetToPreset);
  FPresets.Load;
end;

procedure TFormEditBodyShape.FormShow(Sender: TObject);
begin
  Left := FormMain.Width - Width - PPIScale(10);
end;

procedure TFormEditBodyShape.FSE1Change(Sender: TObject);
begin
  Label2.Enabled := RBRoundRect.Checked;
  FSE1.Enabled := RBRoundRect.Checked;
  Label3.Enabled := RBRoundRect.Checked;
  FSE2.Enabled := RBRoundRect.Checked;
  CBTopLeft.Enabled := RBRoundRect.Checked;
  CBTopRight.Enabled := RBRoundRect.Checked;
  CBBottomLeft.Enabled := RBRoundRect.Checked;
  CBBottomRight.Enabled := RBRoundRect.Checked;

  if FInitializing then exit;

  if RBRectangle.Checked then
    FTargetSurface.BodyShape.SetShapeRectangle(FTargetSurface.Width, FTargetSurface.Height, FrameEditUIBodyShape.BorderWidth);

  if RBRoundRect.Checked then
      FTargetSurface.BodyShape.SetShapeRoundRect(FTargetSurface.Width, FTargetSurface.Height, FSE1.Value, FSE2.Value, FrameEditUIBodyShape.BorderWidth, CBToRoundRectOptions);

  if RBEllipse.Checked then
    FTargetSurface.BodyShape.SetShapeEllipse(FTargetSurface.Width, FTargetSurface.Height, FrameEditUIBodyShape.BorderWidth);
end;

procedure TFormEditBodyShape.PresetToWidget(const data: string);
var A: TStringArray;
  w, h: Integer;
begin
  A := data.Split(['#']);
  if Length(A) <> 2 then exit;

  // sets the preset on the target surface
  w := FTargetSurface.Width;
  h := FTargetSurface.Height;
  FTargetSurface.BodyShape.LoadFromString(A[0]);
  FTargetSurface.BodyShape.ResizeCurrentShape(w, h, True);
  FTargetSurface.BackGradient.LoadGradientDataFromString(A[1]);

  // sets the preset on widgets
  FInitializing := True;
  ShapeTypeToWidget(FTargetSurface.BodyShape.Shapetype);
  FSE1.Value := FTargetSurface.BodyShape.Rx;
  FSE2.Value := FTargetSurface.BodyShape.Ry;
  RoundRectOptionsToCB(FTargetSurface.BodyShape.RoundRectOptions);
  FrameEditUIBodyShape.UpdateFromSurface(FTargetSurface);

  FInitializing := False;
end;

function TFormEditBodyShape.WidgetToPreset: string;
begin
  Result := FTargetSurface.BodyShape.SaveToString+'#'+FTargetSurface.BackGradient.SaveGradientDataToString;
end;

procedure TFormEditBodyShape.ProcessFrameSomethingChange(Sender: TObject);
begin
  if FInitializing then exit;
  FModified := True;
end;

function TFormEditBodyShape.WidgetToShapeType: OGLCScene.TShapetype;
begin
  if RBRectangle.Checked then Result := stRectangle
  else
  if RBRoundRect.Checked then Result := stRoundRect
  else
  if RBEllipse.Checked then Result := stEllipse
  else Result := stRectangle;
end;

procedure TFormEditBodyShape.ShapeTypeToWidget(aShapeType: OGLCScene.TShapetype);
begin
  case aShapeType of
    stRoundRect: RBRoundRect.Checked := True;
    stEllipse: RBEllipse.Checked := True;
    else RBRectangle.Checked := True;
  end;
end;

function TFormEditBodyShape.CBToRoundRectOptions: TRoundRectangleOptions;
begin
  Result := [];
  case CBTopLeft.ItemIndex of
    0: Include(Result, rrTopLeftSquare);
    1:;
    2: Include(Result, rrTopLeftBevel);
  end;

  case CBTopRight.ItemIndex of
    0: Include(Result, rrTopRightSquare);
    1:;
    2: Include(Result, rrTopRightBevel);
  end;

  case CBBottomRight.ItemIndex of
    0: Include(Result, rrBottomRightSquare);
    1:;
    2: Include(Result, rrBottomRightBevel);
  end;

  case CBBottomLeft.ItemIndex of
    0: Include(Result, rrBottomLeftSquare);
    1:;
    2: Include(Result, rrBottomLeftBevel);
  end;
end;

procedure TFormEditBodyShape.RoundRectOptionsToCB(aOptions: TRoundRectangleOptions);
begin
  if rrTopLeftSquare in aOptions then CBTopLeft.ItemIndex := 0
  else if rrTopLeftBevel in aOptions then CBTopLeft.ItemIndex := 2;

  if rrTopRightSquare in aOptions then CBTopRight.ItemIndex := 0
  else if rrTopRightBevel in aOptions then CBTopRight.ItemIndex := 2;

  if rrBottomLeftSquare in aOptions then CBBottomLeft.ItemIndex := 0
  else if rrBottomLeftBevel in aOptions then CBBottomLeft.ItemIndex := 2;

  if rrBottomRightSquare in aOptions then CBBottomRight.ItemIndex := 0
  else if rrBottomRightBevel in aOptions then CBBottomRight.ItemIndex := 2;
end;

procedure TFormEditBodyShape.Edit(aSurface: TUIClickableWithBodyShape);
begin
  FTargetSurface := aSurface;
  FModified := False;

  FInitializing := True;
  ShapeTypeToWidget(FTargetSurface.BodyShape.Shapetype);
  FSE1.Value := FTargetSurface.BodyShape.Rx;
  FSE2.Value := FTargetSurface.BodyShape.Ry;
  RoundRectOptionsToCB(FTargetSurface.BodyShape.RoundRectOptions);
  FrameEditUIBodyShape.UpdateFromSurface(FTargetSurface);
  FInitializing := False;
end;

end.

