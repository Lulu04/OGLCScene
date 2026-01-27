unit form_editscenario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  OGLCScene, u_ui_objectlist, u_presetmanager;

type

  { TFormEditScenario }

  TFormEditScenario = class(TForm)
    BPreset: TSpeedButton;
    BSave: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LB: TListBox;
    LBVelocity: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    procedure BSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBDblClick(Sender: TObject);
    procedure LBSelectionChange(Sender: TObject; User: boolean);
    procedure LBVelocityDblClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FPresets: TPresetManager;
    procedure PresetToWidget(const data: string);
    function WidgetToPreset: string;
  private
    FModified: boolean;
    procedure DoClearAll;
    procedure DoCheckScenario;
    procedure FormatScenario;
  public
    function GetScenarioName: string;
    function GetScenario: string;

    procedure EditNew;
    procedure EditScenarioFromBank(aItem: TScenarioDescriptorItem);
    property Modified: boolean read FModified write FModified;
  end;

var
  FormEditScenario: TFormEditScenario;

implementation

uses u_utils, u_resourcestring, u_project, u_common;

{$R *.lfm}

type
TActionDescriptor = record
  A,          // action
  D,          // description
  S,          // syntax
  E: string;  // example
end;

const
Actions: array[0..47] of TActionDescriptor =(
 (A:'Loop'; D:'Loop the scenario to its beginning.'#10'No parameters.'; S:'Loop'; E:'...'#10'Loop'),
 (A:'Label'; D:'Declare a label. Later you can use GOTO to jump to the label line.'; S:'Label <name>'; E:'...'#10'Label here'#10'...'#10'Goto here'),
 (A:'Goto'; D:'Jump to the specified label.'; S:'Goto <label_name>'; E:'...'#10'Label here'#10'...'#10'Goto here'),
 (A:'Wait'; D:'Pause the execution for a duration in seconds.'; S:'Wait <float_value>'; E:'FlipH TRUE'#10'Wait 0.5'#10'FlipH FALSE'#10'Wait 0.5'#10'Loop'),
 (A:'Kill'; D:'Kill the surface. The surface will be freed next frame.'; S:'Kill'; E:'OpacityChange 0 3.0 idcLinear'#10'Wait 3.0'#10'Kill'),
 (A:'Freeze'; D:'Freeze/Unfreeze the surface. The surface continue to be rendered but not updated.'; S:'Freeze <boolean_value>'; E:'Freeze True'),
 (A:'Visible'; D:'The surface becomes visible or not. the surface continue to be updated.'; S:'Visible <boolean_value>'; E:'Visible TRUE'#10'Wait 1.0'#10'Visible FALSE'#10'Wait 1.0'#10'Loop'),
 (A:'FlipH'; D:'Flip the surface horizontaly or not.'; S:'FlipH <boolean_value>'; E:'FlipH TRUE'#10'Wait 0.5'#10'FlipH FALSE'#10'Wait 0.5'#10'Loop'),
 (A:'FlipV'; D:'Flip the surface verticaly or not.'; S:'FlipV <boolean_value>'; E:'FlipV TRUE'#10'Wait 0.5'#10'FlipV FALSE'#10'Wait 0.5'#10'Loop'),
 (A:'ToggleFlipH'; D:'Toggle the horizontal flip.'; S:'ToggleFlipH'; E:'ToggleFlipH'#10'Wait 0.25'#10'ToggleFlipH'#10'Wait 0.25'#10'Loop'),
 (A:'ToggleFlipV'; D:'Toggle the vertical flip.'; S:'ToggleFlipV'; E:'ToggleFlipV'#10'Wait 0.25'#10'ToggleFlipV'#10'Wait 0.25'#10'Loop'),
 (A:'Opacity'; D:'Set the opacity value.'#10'Value range is 0..255.'; S:'Opacity <integer_value>'; E:'Opacity 0'#10'OpacityChange 255 2.5 idcLinear'),
 (A:'OpacityChange'; D:'Change the opacity value over time.'#10'parameters:'#10'1: the new opacity value in range 0..255'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'OpacityChange <integer_value> <float_value> <curve_ID>'; E:'Opacity 0'#10'OpacityChange 255 2.5 idcLinear'),
 (A:'Animate'; D:'Change the frame index over time.'#10'Frame index is 1 based'#10'parameters:'#10'1: The start frame index'#10'2: The last frame index'#10'3: The amount of frame to add per seconds.'; S:'Animate <integer_value> <integer_value> <float_value>'; E:'Animate 1 3 4.0'),
 (A:'IncFrame'; D:'Increment the frame index.'; S:'IncFrame'; E:'IncFrame'#10'Wait 0.25'#10'DecFrame'#10'Loop'),
 (A:'DecFrame'; D:'Decrement the frame index.'; S:'DecFrame'; E:'IncFrame'#10'Wait 0.25'#10'DecFrame'#10'Loop'),
 (A:'SetFrame'; D:'Set the frame index.'#10'parameters:'#10'1: The new frame index (1 based).'; S:'SetFrame <integer_value>'; E:'SetFrame 1'#10'Wait 2.0'#10'Animate 2 5 4.0'#10'Wait 2.0'#10'Loop'),
 (A:'Tint'; D:'Set the tint value.'#10'parameters:'#10'1: red value in range 0..255'#10'2: green value in range 0..255'#10'3: blue value in range 0..255'#10'4:alpha value in range 0..255'; S:'Tint <integer_value> <integer_value> <integer_value> <integer_value>'; E:'Tint 0 0 0 0'#10'Wait 1.0'#10'Tint 255 0 0 128'#10'Wait 1.0'#10'Loop'),
 (A:'TintChange'; D:'Change the tint value over time.'#10'parameters:'#10'1: new red value in range 0..255'#10'2: new green value in range 0..255'#10'3: new blue value in range 0..255'#10'4: new alpha value in range 0..255'#10'5: the duration in seconds'#10'6: the velocity curve ID'; S:'TintChange <integer_value> <integer_value> <integer_value> <integer_value> <float_value> <curve_ID>'; E:'Tint 0 0 0 0'#10'TintChange 64 128 255 255 2.5 idcLinear'),
 (A:'TintRedChange'; D:'Change only the tint red value over the time.'#10'parameters:'#10'1: the new red value in range 0..255'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'TintRedChange <integer_value> <float_value> <curve_ID>'; E:'Tint 0 0 0 255'#10'Label here'#10'TintRedChange 255 0.5 idcLinear'#10'Wait 0.5'#10'TintRedChange 0 0.5 idcLinear'#10'Wait 0.5'#10'Goto here'),
 (A:'TintGreenChange'; D:'Change only the tint green value over the time.'#10'parameters:'#10'1: the new green value in range 0..255'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'TintGreenChange <integer_value> <float_value> <curve_ID>'; E:'Tint 0 0 0 255'#10'Label here'#10'TintGreenChange 255 0.5 idcLinear'#10'Wait 0.5'#10'TintGreenChange 0 0.5 idcLinear'#10'Wait 0.5'#10'Goto here'),
 (A:'TintBlueChange'; D:'Change only the tint blue value over the time.'#10'parameters:'#10'1: the new blue value in range 0..255'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'TintBlueChange <integer_value> <float_value> <curve_ID>'; E:'Tint 0 0 0 255'#10'Label here'#10'TintBlueChange 255 0.5 idcLinear'#10'Wait 0.5'#10'TintBlueChange 0 0.5 idcLinear'#10'Wait 0.5'#10'Goto here'),
 (A:'TintAlphaChange'; D:'Change only the tint alpha value over the time.'#10'parameters:'#10'1: the new alpha value in range 0..255'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'TintAlphaChange <integer_value> <float_value> <curve_ID>'; E:'Tint 255 0 0 0'#10'Label here'#10'TintAlphaChange 128 0.5 idcLinear'#10'Wait 0.5'#10'TintAlphaChange 0 0.5 idcLinear'#10'Wait 0.5'#10'Goto here'),
 (A:'Angle'; D:'Sets the angle value.'#10'parameters'#10'1: the new angle in degree, 0° is oriented to the right, CW'; S:'Angle <float_value>'; E:'Angle 90'#10'Wait 1.0'#10'Angle -90'#10'Wait 1.0'#10'Loop'),
 (A:'Rotate'; D:'Rotate continuously the surface.'#10'parameters'#10'1: the delta angle to add per seconds'; S:'Rotate <float_value>'; E:'Rotate 360'#10'Wait 1.0'#10'Rotate -360'#10'Wait 1.0'#10'Loop'),
 (A:'RotateTo'; D:'Change the angle value over time.'#10'parameters'#10'1: the new angle value in degree, 0° is on the right, CW'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'RotateTo <float_value> <float_value> <curve_ID>'; E:'RotateTo 90.0 1.0 idSinusoid'#10'Wait 1.0'#10'RotateTo -90 1.0 idSinusoid'#10'Wait 1.0'#10'Loop'),
 (A:'RotateAroundPoint'; D:'rotate the surface around a point.'#10'parameters:'#10'1: coordinate X of the point'#10'2: coordinate Y of the point'#10'3: delta angle to add per seconds in degree, 0° is on the right, CW'#10'4: Self rotation: set it to TRUE if you want the surface to rotate on itself'; S:'RotateAroundPoint <float_value> <float_value> <float_value> <boolean_value>'; E:'RotateAroundPoint 500.0 500.0 360.0 TRUE'),
 (A:'Scale'; D:'Sets the scale value.'#10'parameters:'#10'1: The scale value to apply on both axis horizontal and vertical.'; S:'Scale <float_value>'; E:'Scale 0.5'#10'Wait 1.0'#10'Scale 1.0'#10'Wait 1.0'#10'Loop'),
 (A:'ScaleChange'; D:'Change the scale value over time.'#10'parameters:'#10'1: the new scale value to apply on both axis horizontal and vertical.'#10'2: The duration in secods'#10'3: The velocity curve ID'; S:'ScaleChange <float_value> <float_value> <curve_ID>'; E:'ScaleChange 0.5 1.0 idcSinusoid'#10'Wait 1.0'#10'ScaleChange 1.0 1.0 idcSinusoid'#10'Wait 1.0'#10'Loop'),
 (A:'ScaleH'; D:'Sets the horizontal scale value.'#10'parameters:'#10'1: the new scale value to apply horizontally'; S:'ScaleH <float_value>'; E:'ScaleH 0.5'#10'Wait 1.0'#10'ScaleH 1.0'#10'Wait 1.0'#10'Loop'),
 (A:'ScaleHChange'; D:'Change the horizontal scale value over time.'#10'parameters:'#10'1: the new scale value'#10'2: the duration in seconds'#10' the velocity curve ID'; S: 'ScaleHChange <float_value> <float_value> <curve_ID>'; E:'ScaleHChange 0.5 1.0 idcSinusoid'#10'Wait 1.0'#10'ScaleHChange 1.0 1.0 idcSinusoid'#10'Wait 1.0'#10'Loop'),
 (A:'ScaleV'; D:'Sets the vertical scale value.'#10'parameters:'#10'1: the new scale value to apply vertically'; S:'ScaleV <float_value>'; E:'ScaleV 0.5'#10'Wait 1.0'#10'ScaleV 1.0'#10'Wait 1.0'#10'Loop'),
 (A:'ScaleVChange'; D:'Change the vertical scale value over time.'#10'parameters:'#10'1: the new scale value'#10'2: the duration in seconds'#10' the velocity curve ID'; S: 'ScaleVChange <float_value> <float_value> <curve_ID>'; E:'ScaleVChange 0.5 1.0 idcSinusoid'#10'Wait 1.0'#10'ScaleVChange 1.0 1.0 idcSinusoid'#10'Wait 1.0'#10'Loop'),
 (A:'Blink'; D:'Blink the surface.'#10'parameters:'#10'1: the number of blink, -1 for infinite'#10'2: the visible time in seconds'#10'3: the not visible time in seconds'; S:'Blink <integer_value> <float_value> <float_value>'; E:'Blink -1 0.5 0.5'),
 (A:'StopBlink'; D:'Stop the surface to blink.'; S:'StopBlink'; E:'Blink -1 0.5 0.5'#10'Wait 5.0'#10'StopBlink'#10'Visible TRUE'),
 (A:'MoveTo'; D:'Move the surface to new coordinates over time.'#10'parameters:'#10'1: the new X coordinate'#10'2: the new Y coordinate'#10'3: the duration in seconds'#10'the velocity curve ID'; S:'MoveTo <float_value> <float_value> <float_value> <curve_ID>'; E:'MoveTo 0 0 3.0 idcLinear'#10'Wait 3.0'#10'MoveTo 500 500 3.0 idcLinear'#10'Wait 3.0'#10'Loop'),
 (A:'MoveXTo'; D:'Change the X coordinate over time.'#10'parameters:'#10'1: the new X coordinate'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'MoveXTo <float_value> <float_value> <curve_ID>'; E:'MoveXTo 0 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveXTo 500 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveYTo'; D:'Change the Y coordinate over time.'#10'parameters:'#10'1: the new Y coordinate'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'MoveYTo <float_value> <float_value> <curve_ID>'; E:'MoveYTo 0 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveYTo 500 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveCenterTo'; D:'Change the center coordinates of the center of the surface over time.'#10'parameters:'#10'1: the new X center'#10'2: the new Y center'#10'3: the duration in seconds'#10'the velocity curve ID'; S:'MoveCenterTo <float_value> <float_value> <float_value> <curve_ID>'; E:'MoveCenterTo 0 0 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveCenterTo 500 500 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveXCenterTo'; D:'Change the X center coordinate of the surface over time.'#10'parameters:'#10'1: the new X center coordinate'#10'2: the duration in seconds'#10'3: the vemocity curve ID'; S:'MoveXCenterTo <float_value> <float_value> <curve_ID>'; E:'MoveXCenterTo 0 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveXCenterTo 500 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveYCenterTo'; D:'Change the Y center coordinate of the surface over time.'#10'parameters:'#10'1: the new Y center coordinate'#10'2: the duration in seconds'#10'3: the vemocity curve ID'; S:'MoveYCenterTo <float_value> <float_value> <curve_ID>'; E:'MoveYCenterTo 0 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveYCenterTo 500 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveRelative'; D:'Add an offset to the current coordinate of the surface over time.'#10'parameters:'#10'1: delta X to add'#10'2: delta Y to add'#10'3: the duration in seconds'#10'4: the velocity curve ID'; S:'MoveRelative <float_value> <float_value> <float_value> <curve_ID>'; E:'MoveRelative -50 50 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveRelative 50 -50 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveXRelative'; D:'Add an offset to the current X coordinate of the surface over time.'#10'parameters:'#10'1: delta X to add'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'MoveXRelative <float_value> <float_value> <curve_ID>'; E:'MoveXRelative -50 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveXRelative 50 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'MoveYRelative'; D:'Add an offset to the current Y coordinate of the surface over time.'#10'parameters:'#10'1: delta Y to add'#10'2: the duration in seconds'#10'3: the velocity curve ID'; S:'MoveYRelative <float_value> <float_value> <curve_ID>'; E:'MoveYRelative -50 3.0 idcSinusoid'#10'Wait 3.0'#10'MoveYRelative 50 3.0 idcSinusoid'#10'Wait 3.0'#10'Loop'),
 (A:'SetCoor'; D:'Sets the surface coordinates.'#10'parameters:'#10'1: the new X coordinate'#10'2: the nex Y coordinate'; S:'SetCoor <float_value> <float_value>'; E:'SetCoor 0 0'#10'Wait 3.0'#10'SetCoor 500 500'#10'Wait 3.0'#10'Loop'),
 (A:'SetCenterCoor'; D:'Sets the coordinates of the center of the surface.'#10'parameters:'#10'1: the new center X'#10'2: the new center Y'; S:'SetCenterCoor <float_value> <float_value>'; E:'SetCenterCoor 0 0'#10'Wait 3.0'#10'SetCenterCoor 500 500'#10'Wait 3.0'#10'Loop'),
 (A:'CenterOnScene'; D:'Center the surface on the scene.'; S:'CenterOnScene'; E:'CenterOnScene'),
 (A:'PostMessage'; D:'Post a message to the surface. The message will be received by the ProcessMessage method of the surface.'#10'parameters:'#10'1: the message value'#10'2: the delay in seconds after which the message will be received in ProcessMessage().'; S:'PostMessage <integer_value> <float_value>'; E:'OpacityChange 0 3.0 idcLinear'#10'Wait 3.0'#10'PostMessage 200 0.0')
 );

{ TFormEditScenario }

procedure TFormEditScenario.LBDblClick(Sender: TObject);
var i: Integer;
begin
  i := LB.ItemIndex;
  if i = -1 then exit;
  if Memo1.Lines.Count > 1 then
    if Memo1.Lines[Memo1.Lines.Count-1] = '' then
      Memo1.Lines.Delete(Memo1.Lines.Count-1);
  Memo1.Lines.Add(LB.GetSelectedText);
  FModified := True;
end;

procedure TFormEditScenario.BSaveClick(Sender: TObject);
var nam: string;
  item: TScenarioDescriptorItem;
begin
  Sender := Sender;

  nam := Trim(Edit1.Text);
  if nam = '' then exit;
  if Memo1.Lines.Count = 0 then exit;
  if not IsValidPascalVariableName(nam, True) then exit;
  if not ScenarioBank.AskUserToReplaceExistingItem(nam) then exit;

  FormatScenario;
  DoCheckScenario;
  FormatScenario; // because doCheck add an empty line at the end...

  if Label6.Caption <> 'OK' then exit;

  // retrieve the existing item or create one
  item := ScenarioBank.GetByName(nam);
  if item = NIL then item := ScenarioBank.AddEmpty;

  item._Name := nam;
  item.ScenarioData := Memo1.Lines.Text;
  ScenarioBank.Save;
  Modified := False;
  DoClearAll;

  // remove old variable initialization
  Project.Config.TargetLazarusProject.UCommonRemoveVarInitialization(item.PascalCodeToInitializeVariable);
  // create the variable in u_common and generate the code to initialize it
  Project.Config.TargetLazarusProject.UCommonAddVar(item.VariableName, 'string');
  Project.Config.TargetLazarusProject.UCommonAddVarInitialization(item.PascalCodeToInitializeVariable);

  ModalResult := mrOk;
end;

procedure TFormEditScenario.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Sender := Sender;
  if FModified then
    CanClose := QuestionDlg('', sIfYouLeaveChangeWillBeLost, mtWarning,
                   [mrOk, sLeaveWithoutSaving, mrCancel, sCancel], 0) = mrOk;
end;

procedure TFormEditScenario.FormCreate(Sender: TObject);
begin
  FPresets := TPresetManager.Create(Self);
  FPresets.Init1('Scenario presets', BPreset, Project.FolderUserPreset+'Scenario.preset');
  FPresets.Init2(@PresetToWidget, @WidgetToPreset);
  FPresets.Load;
end;

procedure TFormEditScenario.FormShow(Sender: TObject);
var i: integer;
begin
  LB.Clear;
  for i:=0 to High(Actions) do
    LB.Items.Add(Actions[i].A);
end;

procedure TFormEditScenario.LBSelectionChange(Sender: TObject; User: boolean);
var i: Integer;
  actionName: String;
begin
  Sender := Sender;
  User := User;

  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;

  i := LB.ItemIndex;
  if i = -1 then exit;

  actionName := LB.GetSelectedText;
  // retrieve the action
  for i:=0 to High(Actions) do
    if Actions[i].A = actionName then begin
      Memo2.Lines.Text := Actions[i].D;
      Memo3.Lines.Text := Actions[i].S;
      Memo4.Lines.Text := Actions[i].E;
    end;
end;

procedure TFormEditScenario.LBVelocityDblClick(Sender: TObject);
var i: Integer;
  p: TPoint;
  s: string;
begin
  i := LBVelocity.ItemIndex;
  if i = -1 then exit;
  p := Memo1.CaretPos;
  s := Memo1.Lines.Strings[p.y];
  s := s + ' ' + LBVelocity.GetSelectedText;
  Memo1.Lines.Strings[p.y] := s;
  FModified := True;
end;

procedure TFormEditScenario.Memo1Change(Sender: TObject);
begin
  Sender := Sender;
  FModified := True;
end;

procedure TFormEditScenario.SpeedButton1Click(Sender: TObject);
begin
  FormatScenario;
  DoCheckScenario;
end;

procedure TFormEditScenario.PresetToWidget(const data: string);
var prop: TProperties;
  s: string;
begin
  s := '';
  prop.Split(data, ' ');
  prop.StringValueOf('Data', s, '');
  Memo1.Lines.Text := s;
end;

function TFormEditScenario.WidgetToPreset: string;
var prop: TProperties;
begin
  prop.Init(' ');
  prop.Add('Data', Memo1.Lines.Text, True);
  Result := prop.PackedProperty;
end;

procedure TFormEditScenario.DoClearAll;
begin
  Edit1.Text := '';
  Memo1.Clear;
  LB.ItemIndex := -1;
  FModified := False;
end;

procedure TFormEditScenario.DoCheckScenario;
var o: TSprite;
  scenarioToCheck: TScenario;
begin
  o := TSprite.Create(NIL, False);
  o.ParentScene := FScene;
  scenarioToCheck := o.AddScenario(Memo1.Lines.Text);
  try
    if not scenarioToCheck.Error then Label6.Caption := 'OK'
    else begin
      Label6.Caption := scenarioToCheck.ErrorMessage;
      Memo1.CaretPos := Point(0, scenarioToCheck.LineError);
    end;
  finally
    o.Free;
  end;
end;

procedure TFormEditScenario.FormatScenario;
var i, j: integer;
  s: string;
  A: TStringArray;
begin
  // on each line, remove trailing space and set first letter uppercase
  for i:=0 to Memo1.Lines.Count-1 do begin
    s := Trim(Memo1.Lines.Strings[i]).TrimRight;
    if Length(s) > 0 then
      if s[1] in ['a'..'z'] then s[1] := Chr(Ord(S[1])-32);
    Memo1.Lines.Strings[i] := s;
  end;

  // remove empty lines
  for i:=Memo1.Lines.Count-1 downto 0 do
    if Trim(Memo1.Lines.Strings[i]) = '' then
      Memo1.Lines.Delete(i);

  // ensure there is only one space between parameters
  for i:=0 to Memo1.Lines.Count-1 do begin
    s := Memo1.Lines.Strings[i];
    A := s.Split([' ']);
    for j:= High(A) downto 0 do
      if A[j] = '' then system.Delete(A, j, 1);
    s := '';
    for j:=0 to High(A)-1 do
      s := s + A[j] + ' ';
    s := s + A[High(A)];
    Memo1.Lines.Strings[i] := s;
  end;
end;

function TFormEditScenario.GetScenarioName: string;
begin
  Result := Edit1.Text;
end;

function TFormEditScenario.GetScenario: string;
begin
  Result := Memo1.Lines.Text;
end;

procedure TFormEditScenario.EditNew;
begin
  DoClearAll;
end;

procedure TFormEditScenario.EditScenarioFromBank(aItem: TScenarioDescriptorItem);
begin
  Edit1.Text := aItem._Name;
  Memo1.Lines.Text := aItem.ScenarioData;
  FModified := False;
end;

end.

