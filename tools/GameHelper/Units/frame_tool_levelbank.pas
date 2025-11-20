unit frame_tool_levelbank;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls,
  Dialogs, ComCtrls, u_levelbank, frame_viewlevelbank;

type

  { TFrameToolLevelBank }

  TFrameToolLevelBank = class(TFrame)
    BEdit: TSpeedButton;
    BHelp: TSpeedButton;
    Label24: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Splitter1: TSplitter;
    procedure BEditClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
  private
    procedure DoExportToPascalScreenUnit;
  private
    FUndoRedoManager: TLevelBankUndoRedoManager;
    FInitializingWidget: boolean;
    procedure UpdateWidgetState;
    procedure ProcessFrameViewLevelBankSelectionChangeEvent(Sender: TObject);
  public
    FrameViewLevelBank: TFrameViewLevelBank;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnShow;

    // called from project configuration window
    procedure ClearAll;
  end;

implementation

uses u_screen_levelbank, form_main,  form_showhelp,
  u_layerlist, OGLCScene;

{$R *.lfm}

{ TFrameToolLevelBank }

procedure TFrameToolLevelBank.BEditClick(Sender: TObject);
begin
  if not FrameViewLevelBank.SelectedIsLevel then exit;

  FormMain.EditLevelInLevelBank(FrameViewLevelBank.GetSelectedText);
end;

procedure TFrameToolLevelBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Level Bank contains the levels that you have created for your game.'#10+
  'A level is made with textured objects placed on the screen (world) to construct a decors.'#10#10+
  'GROUPS AND LEVELS:'#10+
  'The levels are organized into groups.'#10+
  'For example a game have 10 levels in the group "Summer", 10 levels in the group "Winter", etc... As you see, its easy to group the game levels by categories.'#10+
  'TO ADD A NEW GROUP:'#10+
  ''#10#10+
  'RENAME A GROUP:'#10+
  ''#10#10+
  'DELETE A GROUP:'#10+
  ''#10#10+
  'ADD A NEW LEVEL:'#10+
  ' - click ''LEVEL EDITOR'' button.'#10#10+
  'RENAME A LEVEL:'#10+
  ''#10#10+
  'TO EDIT AN EXISTING LEVEL:'#10+
  ' - select the level in the list.'#10+
  ' - click ''Edit in Level Editor''.'#10#10+
  'EXPORT LEVELS TO PASCAL UNIT:'#10+
  '  exporting to a Pascal unit allow to easily include the levels definition to your program.'#10+
  ' - enter a class name, for example TGameLevels. Code will be generated to encapsulate the levels in a class with this name.'#10+
  ' - enter a unit name, for example ''u_gamelevels.pas''. The code is saved in a Pascal unit with this name.'#10+
  ' - click ''Export all levels to Pascal unit''.'#10#10+
  'HOW TO USE THIS UNIT IN YOUR PROGRAM:'#10+
  ' - under Lazarus, open the unit in the source editor.'#10+
  ' - add it the to project: Project->Project Inspector->Add->Files from editor, select the unit and click ''Add files''.'#10#10+
  'As you can see, the unit contains one class with:'#10+
  ' - a nice procedure LoadTexture(aAtlas: TAtlas); to load the level textures in your atlas.'#10+
  ' - a nice procedure BuildLevel(aIndex: integer); to build the level (index is 0 based).'#10+
  ' - a property WorldArea: TRectF to retrieve the world size. This property is not directly visible because it is declared in the ancestor of the class.');
end;

procedure TFrameToolLevelBank.DoExportToPascalScreenUnit;
{var t: TStringList;
  nameUnit, screenClassName, screenVarName, decorClassName, decorVarName,
    texFilename, s, sw, sh: string;
  layersUsed, temp: TArrayOfInteger;
  i: integer;
  p: SizeInt; }
begin
  {
  nameUnit := Label43.Caption;
  SD1.FileName := nameUnit;
  if not SD1.Execute then exit;
  screenClassName := Label41.Caption;
  screenVarName := Label45.Caption;
  decorClassName := Label47.Caption;
  decorVarName := Label49.Caption;

  t := TStringList.Create;
  t.Add('{');
  AddFileGeneratedByGameHelper(t);
  t.AddText('  Usage:'#10+
            '   - add this file in your project with the Project Inspector'#10+
            '   - add the creation of the screen in ''form_main.LoadCommonData()'''#10+
            '   - add the destruction of the screen in ''form_main.FreeCommonData()'''#10+
            '   - insert ''FScene.RunScreen('+screenVarName+')'' somewhere in your code');
  t.Add('}');
  t.Add('');
  AddInterface(t, nameUnit);

  // declaration of decor class
  AddDeclarationOfDecorClass(t, decorClassName);

  t.AddText('{ '+screenClassName+'}'#10#10+
            screenClassName+' = class(TScreenTemplate)');

  // retrieve the index of layers used
  layersUsed := NIL;
  if UserWantCamera then begin
    for i:=0 to WorkingLevelGroup.Size-1 do
      if UserWantAllLevels or (not UserWantAllLevels and LB.Selected[i]) then begin
        temp := WorkingLevelGroup.Mutable[i]^.GetUserLayerIndexesUsed;
        if Length(temp) > Length(layersUsed) then
          layersUsed := Copy(temp);
      end;
  end;

  t.Add('private');
  // declare the camera variables
  if Length(layersUsed) > 0 then
    for i:=0 to High(layersUsed) do
      t.Add('  FCamera'+(i+1).ToString+': TOGLCCamera;');
  // declare the decor variable
  t.Add('  '+decorVarName+': '+decorClassName+';');
  // declare the atlas variable (if needed)
  if CheckBox1.Checked then
    t.Add('  FAtlas: TAtlas;');
  t.AddText('public'#10+
            '  procedure CreateObjects; override;'#10+
            '  procedure FreeObjects; override;'#10+
            'end;');
  t.Add('');
  AddImplementation(t);

  // CONST decor data in string format
  t.Add('const');
  for i:=0 to WorkingLevelGroup.Size-1 do
    if (RadioGroup1.ItemIndex = 1) or
       ((RadioGroup1.ItemIndex = 0) and LB.Selected[i]) then
      WorkingLevelGroup.Mutable[i]^.ExportToPascalConst(t);

  // implementation of decor.LoadTexture()
  t.AddText('class procedure '+decorClassName+'.LoadTexture(aAtlas: TOGLCTextureAtlas);'#10+
            'var dataFolder: string;'#10+
            'begin'#10+
            '  inherited LoadTexture(aAtlas);'#10+
            '  dataFolder := u_common.DataFolder;');
  with WorkingLevelGroup.Textures do
    for i:=0 to Size-1 do begin
      // texture filename must be relative to application Data folder
      texFilename := Mutable[i]^.filename;
      p := texFilename.LastIndexOf(DirectorySeparator+'Data'+DirectorySeparator);
      texFilename := texFilename.Remove(0, p+6);
      texFilename := 'dataFolder+'''+texFilename+'''';

      s := '  '; //+Mutable[i]^.name + ' := ';
      if ExtractFileExt(Mutable[i]^.filename) = '.svg' then begin
        if Mutable[i]^.width = -1 then sw := '-1'
          else sw := 'u_common.ScaleW('+Mutable[i]^.width.ToString+')';
        if Mutable[i]^.height = -1 then sh := '-1'
          else sh := 'u_common.ScaleH('+Mutable[i]^.height.ToString+')';

        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImageFromSVG('+texFilename+
             ', '+sw+', '+sh+
             ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
             ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+
             ', 0);'
        else
          s := s + 'aAtlas.AddFromSVG('+texFilename+', '+sw+', '+sh+');';
      end else begin
        if Mutable[i]^.isMultiFrame then
          s := s + 'aAtlas.AddMultiFrameImage('+texFilename+
          ', '+(Mutable[i]^.width div Mutable[i]^.frameWidth).ToString+
          ', '+(Mutable[i]^.height div Mutable[i]^.frameHeight).ToString+');'
        else
          s := s + 'aAtlas.Add('+texFilename+');';
      end;
      t.Add(s);
    end;
  t.AddText('end;'#10+#10);

  // implementation of decor.BuildLevel
  AddImplementationOfDecorBuildLevel(t, decorClassName);

  // implementation of screen class .CreateObjects
  t.Add('{ '+screenClassName+'}');
  t.Add('');
  t.AddText('procedure '+screenClassName+'.CreateObjects;'#10+
            'begin');

  if CheckBox1.Checked then begin
    // we need a local atlas
    t.AddText('  // atlas creation'#10+
              '  FAtlas := FScene.CreateAtlas;'#10+
              '  FAtlas.Spacing := 2;'#10+
              '  AdditionnalScale := 1.0;'#10+
              '  '+decorClassName+'.LoadTexture(FAtlas);'#10+
              '  FAtlas.TryToPack;'#10+
              '  FAtlas.Build;'#10);
  end;
  // camera creation
  if Length(layersUsed) = 1 then begin
    t.Add('  FCamera := FScene.CreateCamera;');
    s := '  FCamera.AssignToLayers([';
    for i:=0 to High(layersUsed) do begin
      s := s + Layers.Names[layersUsed[i]];
      if i < High(layersUsed) then s := s + ', ';
    end;
    t.Add(s+']);');
    if RadioButton3.Checked then
      t.Add('  FCamera.AutoFollow.ComputeBoundsFromWorldArea('+decorVarName+'.WorldArea);')
    else t.Add('  FCamera.AutoFollow.ApplyBounds := False;');
  end else begin
    for i:=0 to High(layersUsed) do begin
      t.Add('  FCamera'+(i+1).ToString+' := FScene.CreateCamera;');
      t.Add('  FCamera'+(i+1).ToString+'.AssignToLayer('+Layers.Names[layersUsed[i]]+');');
      if RadioButton3.Checked then
        t.Add('  FCamera'+(i+1).ToString+'.AutoFollow.ComputeBoundsFromWorldArea('+decorVarName+'.WorldArea);')
      else t.Add('  FCamera'+(i+1).ToString+'.AutoFollow.ApplyBounds := False;');
    end;
  end;
  t.Add('end;');
  t.Add('');

  // implementation of screen class .FreeObjects
  t.AddText('procedure '+screenClassName+'.FreeObjects;'#10+
            'begin'#10+
            '  FreeAndNil('+decorVarName+');');
  if Length(layersUsed) > 0 then
    for i:=0 to High(layersUsed) do
      t.Add('  FScene.KillCamera(FCamera'+(i+1).ToString+');');
  if CheckBox1.Checked then
    t.Add('  FreeAndNil(FAtlas);');
  t.AddText('  FScene.ClearAllLayer;'#10+
            'end;'#10);

  t.Add('');
  t.Add('end.');

  try
    t.SaveToFile(ExtractFilePath(SD1.FileName)+nameUnit+'.pas');
  finally
    t.Free;
  end;
}
end;

procedure TFrameToolLevelBank.UpdateWidgetState;
begin
  BEdit.Enabled := FrameViewLevelBank.SelectedIsLevel;
end;

procedure TFrameToolLevelBank.ProcessFrameViewLevelBankSelectionChangeEvent(Sender: TObject);
begin
  Sender := Sender;
  if FrameViewLevelBank.ASelectionIsAvailable and FrameViewLevelBank.SelectedIsLevel
    then ScreenLevelBank.ShowLevel(FrameViewLevelBank.GetSelectedLevel)
    else ScreenLevelBank.ClearView;

  UpdateWidgetState;
end;

constructor TFrameToolLevelBank.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FUndoRedoManager := TLevelBankUndoRedoManager.Create;

  FrameViewLevelBank := TFrameViewLevelBank.Create(Self);
  FrameViewLevelBank.Parent := Panel6;
  FrameViewLevelBank.Align := alClient;
  FrameViewLevelBank.IsEditable := True;
  FrameViewLevelBank.OnSelectionChange := @ProcessFrameViewLevelBankSelectionChangeEvent;
end;

destructor TFrameToolLevelBank.Destroy;
begin
  FUndoRedoManager.Free;
  FUndoRedoManager := NIL;
  inherited Destroy;
end;

procedure TFrameToolLevelBank.OnShow;
begin
  FInitializingWidget := True;
  FrameViewLevelBank.FillWithLevelBank;
  UpdateWidgetState;

  FInitializingWidget := False;
end;

procedure TFrameToolLevelBank.ClearAll;
begin
  FrameViewLevelBank.Clear;
end;

end.

