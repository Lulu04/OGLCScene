unit form_editdeformationgrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ComCtrls, Buttons,
  OGLCScene, u_surface_list;

type

  { TFormEditDeformationGrid }

  TFormEditDeformationGrid = class(TForm)
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
    FRowLabel: array of TLabel;
    FColumnLabel:array of TLabel;
    FNodeLabel:  array of array of TLabel;
    procedure ComputeWorkingRect;
    procedure KillObjects;
    procedure CreateObjects;
    procedure SetNodeLabelHint(aRow, aCol: integer);
    procedure PlaceObjects;
    procedure ProcessLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProcessLabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SelectLabel(aLabel: TLabel);
    procedure UnselectLabel(aLabel: TLabel);
  public
    procedure Edit(aSurface: PSurfaceDescriptor); //TDeformationGrid);
    property Modified: boolean read FModified;
  end;

implementation
uses BGRABitmap, BGRABitmapTypes, form_main;

{$R *.lfm}

{ TFormEditDeformationGrid }

procedure TFormEditDeformationGrid.FormCreate(Sender: TObject);
begin
  FInitializing := True;
  FillCBType;
  CBType.ItemIndex := 0;
  FInitializing := False;
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

procedure TFormEditDeformationGrid.SE1Change(Sender: TObject);
begin
  if FInitializing then exit;

  if (Sender = SE1) or (Sender = SE2) then begin
    FDeformationGrid.SetGrid(SE1.Value, SE2.Value);
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
    FRowLabel[i] := TLabel.Create(Self);
    FRowLabel[i].Name := 'RowLabel'+i.ToString;
    FRowLabel[i].Parent := Panel2;
    FRowLabel[i].Caption := '▶';
    FRowLabel[i].Cursor := crHandPoint;
    FRowLabel[i].Transparent := False;
    FRowLabel[i].ShowHint := True;
    FRowLabel[i].Hint := 'Set node values on whole row';
    FRowLabel[i].OnMouseDown := @ProcessLabelMouseDown;
    FRowLabel[i].OnMouseUp := @ProcessLabelMouseUp;
    FRowLabel[i].Tag := 1000+i;
    UnselectLabel(FRowLabel[i]);
  end;

  SetLength(FColumnLabel, FDeformationGrid.ColumnCount+1);
  for i:=0 to High(FColumnLabel) do begin
    FColumnLabel[i] := TLabel.Create(Self);
    FColumnLabel[i].Name := 'ColumnLabel'+i.ToString;
    FColumnLabel[i].Parent := Panel2;
    FColumnLabel[i].Caption := '▼';
    FColumnLabel[i].OnMouseDown := @ProcessLabelMouseDown;
    FColumnLabel[i].OnMouseUp := @ProcessLabelMouseUp;
    FColumnLabel[i].Cursor := crHandPoint;
    FColumnLabel[i].Transparent := False;
    FColumnLabel[i].ShowHint := True;
    FColumnLabel[i].Hint := 'Set node values on whole column';
    FColumnLabel[i].Tag := 2000+i;
    UnselectLabel(FColumnLabel[i]);
  end;

  SetLength(FNodeLabel, Length(FRowLabel), Length(FColumnLabel));
  ro := 0;
  co := 0;
  for ro:=0 to High(FRowLabel) do
    for co:=0 to High(FColumnLabel) do begin
      FNodeLabel[ro, co] := TLabel.Create(Self);
      FNodeLabel[ro, co].Name := 'NodeLabel'+ro.ToString+'_'+co.ToString;
      FNodeLabel[ro, co].Parent := Panel2;
      FNodeLabel[ro, co].Caption := '■';
      FNodeLabel[ro, co].OnMouseDown := @ProcessLabelMouseDown;
      FNodeLabel[ro, co].OnMouseUp := @ProcessLabelMouseUp;
      FNodeLabel[ro, co].Cursor := crHandPoint;
      FNodeLabel[ro, co].Tag := (ro shl 16) or co;
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
  SelectLabel(Sender as TLabel);
end;

procedure TFormEditDeformationGrid.ProcessLabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var lab: TLabel;
   ro, co: integer;
begin
  lab := Sender as TLabel;
  UnselectLabel(lab);
  if lab.Tag < 1000 then begin
    // node label
    ro := lab.Tag shr 16;
    co := lab.Tag and $FFFF;
    FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
    FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
    SetNodeLabelHint(ro, co);
    FModified := True;
  end else if lab.Tag < 2000 then begin
    // row label
    ro := lab.Tag - 1000;
    for co:=0 to High(FDeformationGrid.Grid[ro]) do begin
      FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
      FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
      SetNodeLabelHint(ro, co);
    end;
    FModified := True;
  end else begin
    // column label
    co := lab.Tag - 2000;
    for ro:=0 to High(FRowLabel) do begin
      FDeformationGrid.Grid[ro, co].DeformationAmount := FSE5.Value;
      FDeformationGrid.Grid[ro, co].TimeMultiplicator := FSE6.Value;
      SetNodeLabelHint(ro, co);
    end;
    FModified := True;
  end;
end;

procedure TFormEditDeformationGrid.SelectLabel(aLabel: TLabel);
begin
  if aLabel = NIL then exit;
  aLabel.Color := clHighlight;
  aLabel.Font.Color := clWhite;
end;

procedure TFormEditDeformationGrid.UnselectLabel(aLabel: TLabel);
begin
  if aLabel = NIL then exit;
  aLabel.Color := Panel2.Color;
  aLabel.Font.Color := clWhite;
end;

procedure TFormEditDeformationGrid.Edit(aSurface: PSurfaceDescriptor);
begin
  FSurfaceDescriptor := aSurface;
  FDeformationGrid := aSurface^.surface as TDeformationGrid;

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
  FModified := False;
end;

end.

