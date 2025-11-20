unit form_editgradient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, OGLCScene, BGRABitmap, BGRABitmapTypes,
  frame_gradientrow, u_presetmanager;

type

  { TFormEditGradient }

  TFormEditGradient = class(TForm)
    BHelp: TSpeedButton;
    BFlipH: TSpeedButton;
    BFlipV: TSpeedButton;
    BRotate90CCW: TSpeedButton;
    BRotate90CW: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ColorButton1: TColorButton;
    Label1: TLabel;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    BDeleteFrame: TSpeedButton;
    BColorToRow: TSpeedButton;
    BAddRow: TSpeedButton;
    BPreset: TSpeedButton;
    procedure BAddRowClick(Sender: TObject);
    procedure BColorToRowClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure BFlipHClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BDeleteFrameClick(Sender: TObject);
  private
    FModified: boolean;
    FGradient: PGradientDescriptor;
    FGWidth, FGHeight: integer;
    FInitializing: boolean;
    FNewGradientData: string;
    procedure DoFlipH;
    procedure DoFlipV;
    procedure DoRotate90CCW;
    procedure DoRotate90CW;
  private
    Frames: array of TFrameGradientRow;
    FIDName: integer;
    procedure Clear;
    function AddFrame: integer;
    procedure InsertFrame;
    function GetSelectedFrameIndex: integer;
    procedure DeleteFrame(aIndex: integer);
    procedure UpdateFramesPosition;
    procedure ProcessFrameCurveChangeEvent(Sender: TObject);
    procedure ProcessFrameNodeModifiedEvent(Sender: TObject; aNodeIndex: integer);
    procedure ProcessFrameDeleteNodeEvent(Sender: TObject; aNodeIndex: integer);
    procedure ProcessFrameAddNodeEvent(Sender: TObject; aNodeIndex: integer; const aColor: TBGRAPixel);
  private
    FPresets: TPresetManager;
    procedure PresetToWidget(const A: TStringArray);
    function WidgetToPreset: string;
    procedure DoGradientToFrame;
  public
    procedure SelectNone;

    procedure Edit(aGradient: PGradientDescriptor; aWidth, aHeight: integer);
    property Modified: boolean read FModified;
    property NewGradientData: string read FNewGradientData;
  end;

var
  FormEditGradient: TFormEditGradient;

implementation

uses form_main, u_app_pref, form_showhelp;

{$R *.lfm}

{ TFormEditGradient }

procedure TFormEditGradient.FormShow(Sender: TObject);
begin
  Left := FormMain.Width-Width;
end;

procedure TFormEditGradient.BDeleteFrameClick(Sender: TObject);
var i: integer;
begin
  i := GetSelectedFrameIndex;
  if (i = -1) or (i = 0) or (i = High(Frames)) then exit;
  DeleteFrame(i);
end;

procedure TFormEditGradient.DoFlipH;
begin
  if Length(FGradient^.Rows) = 0 then exit;
  FGradient^.FlipH;
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.DoFlipV;
begin
  if Length(FGradient^.Rows) < 2 then exit;
  FGradient^.FlipV;
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.DoRotate90CCW;
begin
  if Length(FGradient^.Rows) = 0 then exit;
  FGradient^.Rotate90CCW;
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.DoRotate90CW;
begin
  if Length(FGradient^.Rows) = 0 then exit;
  FGradient^.Rotate90CW;
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.Clear;
var i: integer;
begin
  for i:=0 to High(Frames) do
    Frames[i].Free;
  Frames := NIL;
end;

function TFormEditGradient.AddFrame: integer;
begin
  inc(FIDName);
  Result := Length(Frames);
  SetLength(Frames, Result+1);
  Frames[Result] := TFrameGradientRow.Create(Self);
  Frames[Result].Name := 'FrameGradientRow'+FIDName.ToString;
  Frames[Result].Parent := ScrollBox1;
  Frames[Result].Index := Result;
  Frames[Result].OnCurveChange := @ProcessFrameCurveChangeEvent;
  Frames[Result].OnNodeModified:= @ProcessFrameNodeModifiedEvent;
  Frames[Result].OnDeleteNode := @ProcessFrameDeleteNodeEvent;
  Frames[Result].OnAddNode := @ProcessFrameAddNodeEvent;
end;

procedure TFormEditGradient.InsertFrame;
var i, k: integer;
  fr: TFrameGradientRow;
  clone: TGradientRow;
begin
  k := GetSelectedFrameIndex;
  if k = -1 then exit;
  if k = 0 then k := 1;

  // insert a new frame in Frames[]
  inc(FIDName);
  fr := TFrameGradientRow.Create(Self);
  fr.Name := 'FrameGradientRow'+FIDName.ToString;
  fr.Parent := ScrollBox1;
  fr.Index := k;
  fr.OnCurveChange := @ProcessFrameCurveChangeEvent;
  fr.OnNodeModified:= @ProcessFrameNodeModifiedEvent;
  fr.OnDeleteNode := @ProcessFrameDeleteNodeEvent;
  fr.OnAddNode := @ProcessFrameAddNodeEvent;
  system.Insert(fr, Frames, k);
  // shift other indexes
  for i:=k+1 to High(Frames) do
    Frames[i].Index := i;
  UpdateFramesPosition;

  // insert the previous row in gradient
  clone.YPosition := (FGradient^.Rows[k].YPosition-FGradient^.Rows[k-1].YPosition)*0.5+
                     FGradient^.Rows[k-1].YPosition;
  clone.Items := Copy(FGradient^.Rows[k-1].Items, 0, Length(FGradient^.Rows[k-1].Items));
  system.Insert(clone, FGradient^.Rows, k);
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);

  // because the insertion have relocated the array in memory, we have to re-init the pointers
  for i:=0 to High(Frames) do
    if Frames[i] <> fr then Frames[i].Nodes := @FGradient^.Rows[i].Items;

  fr.Edit(clone.YPosition, @(FGradient^.Rows[k].Items));

  FrameToolsSpriteBuilder.Modified := True;
end;

function TFormEditGradient.GetSelectedFrameIndex: integer;
var i: integer;
begin
  for i:=0 to High(Frames) do if
    Frames[i].Selected then exit(i);
  Result := -1;
end;

procedure TFormEditGradient.DeleteFrame(aIndex: integer);
var i: integer;
begin
  if (aIndex = 0) or (aIndex = High(Frames)) then exit;

  // delete the frame
  Frames[aIndex].Free;
  system.Delete(Frames, aIndex, 1);
  // shift other index
  for i:= aIndex to High(Frames) do
    Frames[i].Index := Frames[i].Index - 1;
  UpdateFramesPosition;

  // delete in gradient
  system.Delete(FGradient^.Rows, aIndex, 1);
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);

  FrameToolsSpriteBuilder.Modified := True;
end;

procedure TFormEditGradient.UpdateFramesPosition;
var i, y: integer;
begin
  y := 0;
  for i:=0 to High(Frames) do begin
    Frames[i].SetBounds(PPIScale(2), y, Round(ScrollBox1.ClientWidth*0.95), Frames[i].Height);
    y := y + Frames[i].Height + PPIScale(2);
  end;
end;

procedure TFormEditGradient.ProcessFrameCurveChangeEvent(Sender: TObject);
var fr: TFrameGradientRow;
begin
  fr := TFrameGradientRow(Sender);
  FGradient^.Rows[fr.Index].YPosition := fr.YPosition;

  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.ProcessFrameNodeModifiedEvent(Sender: TObject; aNodeIndex: integer);
var fr: TFrameGradientRow;
  i: integer;
  colorRef: TBGRAPixel;
  posRef: single;
begin
  fr := TFrameGradientRow(Sender);
  FGradient^.Rows[fr.Index].YPosition := fr.YPosition;

  colorRef := FGradient^.Rows[fr.Index].Items[aNodeIndex].Color;
  posRef := FGradient^.Rows[fr.Index].Items[aNodeIndex].XPosition;
  for i:=0 to High(Frames) do
    if i <> fr.Index then begin
      // same color on nodes with same index
      if CheckBox1.Checked then
        FGradient^.Rows[i].Items[aNodeIndex].Color := colorRef;
      if CheckBox2.Checked then
        FGradient^.Rows[i].Items[aNodeIndex].XPosition := posRef;
      Frames[i].PB.Invalidate;
    end;

  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.ProcessFrameDeleteNodeEvent(Sender: TObject; aNodeIndex: integer);
var fr: TFrameGradientRow;
  i: integer;
begin
  fr := TFrameGradientRow(Sender);

  for i:=0 to High(Frames) do
    if (Frames[i] <> fr) and (aNodeIndex < Length(FGradient^.Rows[i].Items)) then begin
      system.Delete(FGradient^.Rows[i].Items, aNodeIndex, 1);
      Frames[i].PB.Invalidate;
    end;

  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.ProcessFrameAddNodeEvent(Sender: TObject;
  aNodeIndex: integer; const aColor: TBGRAPixel);
var fr: TFrameGradientRow;
  xPos: single;
  o: TGradientItem;
  i: integer;
begin
  fr := TFrameGradientRow(Sender);

  for i:=0 to High(Frames) do
    if (Frames[i] <> fr) and (aNodeIndex <= High(FGradient^.Rows[i].Items)) and (aNodeIndex > 0) then begin
      xPos := FGradient^.Rows[i].Items[aNodeIndex-1].XPosition;
      xPos := xPos + (FGradient^.Rows[i].Items[aNodeIndex].XPosition - xPos) * 0.5;
      o.XPosition := xPos;
      o.Color := aColor;
      system.Insert(o, FGradient^.Rows[i].Items, aNodeIndex);

      Frames[i].PB.Invalidate;
    end;

  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.PresetToWidget(const A: TStringArray);
begin
  FGradient^.LoadGradientDataFromString(A[0]);
  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  DoGradientToFrame;
  FModified := True;
end;

function TFormEditGradient.WidgetToPreset: string;
begin
  Result := FGradient^.SaveGradientDataToString;
end;

procedure TFormEditGradient.DoGradientToFrame;
var i, j: integer;
  sameXPos, sameColor: boolean;
begin
  Clear;

  FInitializing := True;
  // check options
  sameXPos := True;
  sameColor := True;
  for i:=0 to High(FGradient^.Rows[0].Items) do
    for j:=0 to High(FGradient^.Rows)-1 do begin
      sameXPos := sameXPos and (FGradient^.Rows[j].Items[i].XPosition = FGradient^.Rows[j+1].Items[i].XPosition);
      sameColor := sameColor and (FGradient^.Rows[j].Items[i].Color = FGradient^.Rows[j+1].Items[i].Color);
    end;
  CheckBox1.Checked := sameColor;
  CheckBox2.Checked := sameXPos;


  for i:=0 to High(FGradient^.Rows) do begin
    AddFrame;
    Frames[i].Edit(FGradient^.Rows[i].YPosition, @(FGradient^.Rows[i].Items));
    Frames[i].FSE1.ReadOnly := (i = 0) or (i = High(FGradient^.Rows));
  end;
  UpdateFramesPosition;

  FInitializing := False;
end;

procedure TFormEditGradient.SelectNone;
var i: integer;
begin
  for i:=0 to High(Frames) do
    Frames[i].Selected := False;
end;

procedure TFormEditGradient.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CloseAction;

  FNewGradientData := FGradient^.SaveGradientDataToString;
end;

procedure TFormEditGradient.FormCreate(Sender: TObject);
begin
  FPresets := TPresetManager.Create(Self);
  FPresets.Init1('Gradient presets', BPreset, GetPresetFolder+'Gradient.preset');
  FPresets.Init2(@PresetToWidget, @WidgetToPreset);
  FPresets.Load;
end;

procedure TFormEditGradient.BAddRowClick(Sender: TObject);
begin
  InsertFrame;
  UpdateFramesPosition;
end;

procedure TFormEditGradient.BColorToRowClick(Sender: TObject);
var k: integer;
begin
  k := GetSelectedFrameIndex;
  if k = -1 then exit;
  Frames[k].SetSameColorOnAllNodes(ColorButton1.ButtonColor);

  FGradient^.ComputeVerticesAndIndices(FGWidth, FGHeight);
  FModified := True;
end;

procedure TFormEditGradient.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('GRADIENT EDITOR'#10#10+
     'This tool allow to edit gradients (only linear gradient) with multiple colors.'#10+
     'A gradient is made of multiple rows, and a row can have multiple color nodes.'#10+
     'A row have a Y position in range 0.0 to 1.0 (top to bottom).'#10+
     'A node is represented by a colored rectangle.'#10+
     'All row have the same node count.'#10#10+
     'ADD A NODE:'#10+
     ' - click on empty area in the node graph, a new node will appear with white color.'#10#10+
     'DELETE A NODE:'#10+
     ' - right click on the node.'#10#10+
     'MOVING A NODE:'#10+
     ' - simply drag the node with the mouse to the left or right.'#10#10+
     'NODE COLOR:'#10+
     ' - double click on a node open the color window -> pick a color'#10#10+
     'NODE OPACITY:'#10+
     ' - hold Ctrl key while dragging the node up and down.'#10#10+
     'SELECT A ROW:'#10+
     ' - left click on the left part of the row.'#10#10+
     'INSERT A NEW ROW:'#10+
     ' - select a row to define where the new one will be inserted and click "Insert row".'#10+
     '   NOTE: you can insert a row only between the first and the last row.'#10#10+
     'DELETE A ROW:'#10+
     ' - select the row to delete and click "Delete row".'#10#10);
end;

procedure TFormEditGradient.BFlipHClick(Sender: TObject);
begin
  if Sender = BFlipH then DoFlipH;
  if Sender = BFlipV then DoFlipV;
  if Sender = BRotate90CCW then DoRotate90CCW;
  if Sender = BRotate90CW then DoRotate90CW;
end;

procedure TFormEditGradient.CheckBox1Change(Sender: TObject);
var i, j: integer;
begin
  if FInitializing then exit;
  i := GetSelectedFrameIndex;
  if i = -1 then exit;

  if CheckBox1.Checked or CheckBox2.Checked then
    for j:=0 to High(FGradient^.Rows[i].Items) do
      ProcessFrameNodeModifiedEvent(Frames[i], j);
end;

procedure TFormEditGradient.Edit(aGradient: PGradientDescriptor; aWidth, aHeight: integer);
begin
  FGradient := aGradient;
  FGWidth := aWidth;
  FGHeight := aHeight;
  DoGradientToFrame;
end;

end.

