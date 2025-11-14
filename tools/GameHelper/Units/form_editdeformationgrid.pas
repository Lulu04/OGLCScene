unit form_editdeformationgrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ComCtrls, Buttons,
  OGLCScene, u_surface_list, u_presetmanager;

type

  TLabelGrid = class(TLabel)
  public
    Row: integer;
    Col: integer;
  end;

  { TFormEditDeformationGrid }

  TFormEditDeformationGrid = class(TForm)
    BFlipH: TSpeedButton;
    BFlipV: TSpeedButton;
    BPreset: TSpeedButton;
    BRotate90CCW: TSpeedButton;
    BRotate90CW: TSpeedButton;
    CBType: TComboBox;
    CheckBox1: TCheckBox;
    FSE5: TFloatSpinEdit;
    FSE6: TFloatSpinEdit;
    FSE1: TFloatSpinEdit;
    FSE2: TFloatSpinEdit;
    FSE3: TFloatSpinEdit;
    FSE4: TFloatSpinEdit;
    GroupBox1: TGroupBox;
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
    Panel2: TPanel;
    Panel1: TPanel;
    SE1: TSpinEdit;
    SE2: TSpinEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure BFlipHClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SE1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FSurfaceDescriptor: PSurfaceDescriptor;
    FDeformationGrid: TDeformationGrid;
    FInitializing: boolean;
    procedure FillCBType;
  private
    FModified: boolean;
    FRect: TRect;
    FRowLabel: array of TLabelGrid;
    FColumnLabel:array of TLabelGrid;
    FNodeLabel:  array of array of TLabelGrid;
    procedure DoCreateNode;
    procedure ComputeWorkingRect;
    procedure KillObjects;
    procedure CreateObjects;
    procedure SetNodeLabelHint(aRow, aCol: integer);
    procedure PlaceObjects;
    procedure ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProcessLabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SelectLabel(aLabel: TLabelGrid);
    procedure UnselectLabel(aLabel: TLabelGrid);
    procedure DoSetGridSize;
  private
    FPresets: TPresetManager;
    procedure PresetToWidget(const A: TStringArray);
    function WidgetToPreset: string;
  public
    procedure Edit(aSurface: PSurfaceDescriptor);
    property Modified: boolean read FModified;
  end;

implementation
uses BGRABitmap, BGRABitmapTypes, form_main, u_app_pref, Math;

{$R *.lfm}

{ TFormEditDeformationGrid }

procedure TFormEditDeformationGrid.FormCreate(Sender: TObject);
begin
  FInitializing := True;
  FillCBType;
  CBType.ItemIndex := 0;
  FInitializing := False;

  FPresets := TPresetManager.Create(Self);
  FPresets.Init1('Deformation grid presets', BPreset, GetPresetFolder+'DeformationGrid.preset');
  FPresets.Init2(@PresetToWidget, @WidgetToPreset);
  FPresets.Load;
end;

procedure TFormEditDeformationGrid.FormResize(Sender: TObject);
begin
  ComputeWorkingRect;
  PlaceObjects;
end;

procedure TFormEditDeformationGrid.FormShow(Sender: TObject);
begin
  Left := FormMain.Width-Width;
end;

procedure TFormEditDeformationGrid.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FSurfaceDescriptor^.DeformationGridData := FDeformationGrid.SaveDeformationDataToString;
end;

procedure TFormEditDeformationGrid.BFlipHClick(Sender: TObject);
   procedure SwapRowColumnCount;
   var v: integer;
   begin
     FInitializing := True;
     v := SE1.Value;
     SE1.Value :=  SE2.Value;
     SE2.Value := v;
     FInitializing := False;
   end;

begin
  if Sender = BFlipH then begin
    FDeformationGrid.GridFlipH;
    FDeformationGrid.ResetNodePositions;
    DoCreateNode;
    FModified := True;
  end;

  if Sender = BFlipV then begin
    FDeformationGrid.GridFlipV;
    FDeformationGrid.ResetNodePositions;
    DoCreateNode;
    FModified := True;
  end;

  if Sender = BRotate90CCW then begin
    FDeformationGrid.GridRotate90CCW;
    FDeformationGrid.ResetNodePositions;
    DoCreateNode;
    FModified := True;
  end;

  if Sender = BRotate90CW then begin
    FDeformationGrid.GridRotate90CW;
    SwapRowColumnCount;
    DoCreateNode;
    FModified := True;
  end;
end;

procedure TFormEditDeformationGrid.SE1Change(Sender: TObject);
begin
  if FInitializing then exit;

  if (Sender = SE1) or (Sender = SE2) then begin
    DoSetGridSize;
    CreateObjects;
    FModified := True;
  end;

  if Sender = CBType then begin
    FDeformationGrid.ApplyDeformation(TDeformationType(CBType.ItemIndex));
    FModified := True;
  end;

  if Sender = CheckBox1 then
    FDeformationGrid.ShowGrid := CheckBox1.Checked;

  if (Sender = FSE1) or (Sender = FSE2) then begin
    FDeformationGrid.Amplitude.Value := PointF(FSE1.Value, FSE2.Value);
    FModified := True;
  end;

  if (Sender = FSE3) or (Sender = FSE4) then begin
    FDeformationGrid.DeformationSpeed.Value := PointF(FSE3.Value, FSE4.Value);
    FModified := True;
  end;
end;

procedure TFormEditDeformationGrid.SpeedButton1Click(Sender: TObject);
begin
  FDeformationGrid.ResetNodePositions;
end;

procedure TFormEditDeformationGrid.SpeedButton2Click(Sender: TObject);
var ro, co: integer;
begin
  FDeformationGrid.ResetNodeCoeffs;
  for ro:=0 to High(FNodeLabel) do
    for co:=0 to High(FNodeLabel[ro]) do
      SetNodeLabelHint(ro, co);
  FModified := True;
end;

procedure TFormEditDeformationGrid.FillCBType;
var value: string;
  i: TDeformationType;
begin
  CBType.Clear;
  for i in TDeformationType do begin
    WriteStr(value, i);
    CBType.Items.Add(value);
  end;
end;

procedure TFormEditDeformationGrid.DoCreateNode;
begin
  FInitializing := True;

  SE1.Value := FDeformationGrid.RowCount;
  SE2.Value := FDeformationGrid.ColumnCount;
  CBType.ItemIndex := Ord(FDeformationGrid.DeformationType);
  CheckBox1.Checked := FDeformationGrid.ShowGrid;
  FSE1.Value := FDeformationGrid.Amplitude.x.Value;
  FSE2.Value := FDeformationGrid.Amplitude.y.Value;
  FSE3.Value := FDeformationGrid.DeformationSpeed.x.Value;
  FSE4.Value := FDeformationGrid.DeformationSpeed.y.Value;

  FInitializing := False;

  CreateObjects;
end;

procedure TFormEditDeformationGrid.ComputeWorkingRect;
var ratio: single;
  w, h, x, y: integer;
begin
  if FDeformationGrid.Height = 0 then exit;

  ratio := FDeformationGrid.Width / FDeformationGrid.Height;
  if Panel2.ClientWidth < Panel2.ClientHeight then begin
    w := Round(Panel2.ClientWidth / ratio);
    h := Round(w * ratio);
  end else begin
    h := Round(Panel2.ClientHeight * ratio);
    w := Round(h / ratio);
  end;
  x := (Panel2.ClientWidth - w) div 2;
  y :=(Panel2.ClientHeight - h) div 2;
  FRect := Rect(x, y, x+w, y+h);
end;

procedure TFormEditDeformationGrid.KillObjects;
var i, ro, co: integer;
begin
  for i:=0 to High(FRowLabel) do
    FRowLabel[i].Free;
  FRowLabel := NIL;

  for i:=0 to High(FColumnLabel) do
    FColumnLabel[i].Free;
  FColumnLabel := NIL;

  for ro:=0 to High(FNodeLabel) do
    for co:=0 to High(FNodeLabel[ro]) do
      FNodeLabel[ro, co].Free;
  FNodeLabel := NIL;
end;

procedure TFormEditDeformationGrid.CreateObjects;
var i, ro, co: integer;
begin
  KillObjects;

  SetLength(FRowLabel, FDeformationGrid.RowCount+1);
  for i:=0 to High(FRowLabel) do begin
    FRowLabel[i] := TLabelGrid.Create(Self);
    FRowLabel[i].Name := 'RowLabel'+i.ToString;
    FRowLabel[i].Parent := Panel2;
    FRowLabel[i].Caption := '▶';
    FRowLabel[i].Cursor := crHandPoint;
    FRowLabel[i].Transparent := False;
    FRowLabel[i].ShowHint := True;
    FRowLabel[i].Hint := 'Set node values on whole row';
    FRowLabel[i].OnMouseDown := @ProcessLabelMouseDown;
    FRowLabel[i].OnMouseUp := @ProcessLabelMouseUp;
    FRowLabel[i].Row := i;
    FRowLabel[i].Col := -1;
    UnselectLabel(FRowLabel[i]);
  end;

  SetLength(FColumnLabel, FDeformationGrid.ColumnCount+1);
  for i:=0 to High(FColumnLabel) do begin
    FColumnLabel[i] := TLabelGrid.Create(Self);
    FColumnLabel[i].Name := 'ColumnLabel'+i.ToString;
    FColumnLabel[i].Parent := Panel2;
    FColumnLabel[i].Caption := '▼';
    FColumnLabel[i].OnMouseDown := @ProcessLabelMouseDown;
    FColumnLabel[i].OnMouseUp := @ProcessLabelMouseUp;
    FColumnLabel[i].Cursor := crHandPoint;
    FColumnLabel[i].Transparent := False;
    FColumnLabel[i].ShowHint := True;
    FColumnLabel[i].Hint := 'Set node values on whole column';
    FColumnLabel[i].Row := -1;
    FColumnLabel[i].Col := i;
    UnselectLabel(FColumnLabel[i]);
  end;

  SetLength(FNodeLabel, Length(FRowLabel), Length(FColumnLabel));
  ro := 0;
  co := 0;
  for ro:=0 to High(FRowLabel) do
    for co:=0 to High(FColumnLabel) do begin
      FNodeLabel[ro, co] := TLabelGrid.Create(Self);
      FNodeLabel[ro, co].Name := 'NodeLabel'+ro.ToString+'_'+co.ToString;
      FNodeLabel[ro, co].Parent := Panel2;
      FNodeLabel[ro, co].Caption := '⚫';
      FNodeLabel[ro, co].OnMouseDown := @ProcessLabelMouseDown;
      FNodeLabel[ro, co].OnMouseUp := @ProcessLabelMouseUp;
      FNodeLabel[ro, co].Cursor := crHandPoint;
      FNodeLabel[ro, co].Row := ro;
      FNodeLabel[ro, co].Col := co;
      FNodeLabel[ro, co].Transparent := False;
      FNodeLabel[ro, co].ShowHint := True;
      SetNodeLabelHint(ro, co);
      UnselectLabel(FNodeLabel[ro, co]);
    end;

  PlaceObjects;
end;

procedure TFormEditDeformationGrid.SetNodeLabelHint(aRow, aCol: integer);
begin
  FNodeLabel[aRow, aCol].Hint := 'Deformation amount: '+
              FormatFloatWithDot('0.00', FDeformationGrid.Grid[aRow, aCol].DeformationAmount)+LineEnding+
              'Time multiplicator: '+
              FormatFloatWithDot('0.00', FDeformationGrid.Grid[aRow, aCol].TimeMultiplicator);
end;

procedure TFormEditDeformationGrid.PlaceObjects;
var i, ro, co: integer;
   x, y, mx, my: single;
begin
  mx := (Panel2.ClientWidth-FColumnLabel[0].Width)/(FDeformationGrid.ColumnCount+1.5);
  my := (Panel2.ClientHeight-FRowLabel[0].Height)/(FDeformationGrid.RowCount+1);

  x := 0;
  y := my*0.5;
  for i:=0 to High(FRowLabel) do begin
    FRowLabel[i].Left := Trunc(x);
    FRowLabel[i].Top := Trunc(y);
    y := y + my;
  end;

  x := mx*0.5;
  y := 0;
  for i:=0 to High(FColumnLabel) do begin
    FColumnLabel[i].Left := Trunc(x);
    FColumnLabel[i].Top := Trunc(y);
    x := x + mx;
  end;

  y := my*0.5;
  for ro:=0 to High(FRowLabel) do begin
    x := mx*0.5;
    for co:=0 to High(FColumnLabel) do begin
      FNodeLabel[ro, co].Left := Trunc(x);
      FNodeLabel[ro, co].Top := Trunc(y);
      x := x + mx;
    end;
    y := y + my;
  end;
end;

procedure TFormEditDeformationGrid.ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SelectLabel(Sender as TLabelGrid);
end;

procedure TFormEditDeformationGrid.ProcessLabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lab: TLabelGrid;
   ro, co: integer;
begin
  lab := Sender as TLabelGrid;
  UnselectLabel(lab);

  if lab.Col = -1 then begin
    // row label
    ro := lab.Row;
    for co:=0 to High(FDeformationGrid.Grid[ro]) do begin
      FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
      FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
      SetNodeLabelHint(ro, co);
    end;
    FModified := True;
  end else if lab.Row = -1 then begin
    // column label
    co := lab.Col;
    for ro:=0 to High(FRowLabel) do begin
      FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
      FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
      SetNodeLabelHint(ro, co);
    end;
    FModified := True;
  end else begin
    // node label
    ro := lab.Row;
    co := lab.Col;
    FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
    FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
    SetNodeLabelHint(ro, co);
    FModified := True;
  end;
end;

procedure TFormEditDeformationGrid.SelectLabel(aLabel: TLabelGrid);
begin
  if aLabel = NIL then exit;
  aLabel.Color := clHighlight;
  if (aLabel.Row = -1) or (aLabel.Col = -1) then aLabel.Font.Color := $005B00B7
    else aLabel.Font.Color := clWhite;
end;

procedure TFormEditDeformationGrid.UnselectLabel(aLabel: TLabelGrid);
begin
  if aLabel = NIL then exit;
  aLabel.Color := Panel2.Color;
  if (aLabel.Row = -1) or (aLabel.Col = -1) then aLabel.Font.Color := $005B00B7
    else aLabel.Font.Color := clWhite;
end;

procedure TFormEditDeformationGrid.DoSetGridSize;
var grid: ArrayOfArrayOfDeformedPoint;
   ix, iy, len1, len2: integer;
begin
  // save grid values
  grid := Copy(FDeformationGrid.Grid);

  FDeformationGrid.SetGrid(SE1.Value, SE2.Value);

  // rewrite saved values because they are reseted to default
  len1 := Min(Length(grid), Length(FDeformationGrid.Grid)) - 1;
  len2 := Min(Length(grid[0]), Length(FDeformationGrid.Grid[0])) - 1;

  for iy:=0 to len1 do
    for ix:=0 to len2 do begin
      FDeformationGrid.Grid[iy][ix].DeformationAmount := grid[iy][ix].DeformationAmount;
      FDeformationGrid.Grid[iy][ix].TimeMultiplicator := grid[iy][ix].TimeMultiplicator;
      FDeformationGrid.Grid[iy][ix].x := grid[iy][ix].x;
      FDeformationGrid.Grid[iy][ix].y := grid[iy][ix].y;
    end;
end;

procedure TFormEditDeformationGrid.PresetToWidget(const A: TStringArray);
begin
  FDeformationGrid.ResetNodePositions;
  FDeformationGrid.LoadDeformationDataFromString(A[0]);
  DoCreateNode;
  FModified := True;
end;

function TFormEditDeformationGrid.WidgetToPreset: string;
begin
  Result := FDeformationGrid.SaveDeformationDataToString;
end;

procedure TFormEditDeformationGrid.Edit(aSurface: PSurfaceDescriptor);
begin
  FSurfaceDescriptor := aSurface;
  FDeformationGrid := aSurface^.surface as TDeformationGrid;
  DoCreateNode;
  FModified := False;
end;

end.

