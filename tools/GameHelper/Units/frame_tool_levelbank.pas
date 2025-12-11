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
    BHelp: TSpeedButton;
    Label24: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Splitter1: TSplitter;
    procedure BHelpClick(Sender: TObject);
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

uses u_screen_levelbank, form_showhelp,
  u_layerlist, OGLCScene;

{$R *.lfm}

{ TFrameToolLevelBank }

procedure TFrameToolLevelBank.BHelpClick(Sender: TObject);
begin
  form_showhelp.ShowHelp('The Level Bank contains the levels that you have created for your game.'#10+
  'A level is made with textures placed on the screen (world) to construct a decors.'#10#10+
  'GROUPS AND LEVELS:'#10+
  'The levels are organized into groups.'#10+
  'For example a game have 10 levels in the group "Summer", 10 levels in the group "Winter", etc... As you see, its easy to group the game levels by categories.'#10+
  'NOTE 1: textures are belong one single group and are not shared with the other.'#10+
  'NOTE 2: you can add a description for groups or levels to display to the player.'#10+
  '        They are included in the exported Pascal unit.'#10+
  'ADD A NEW GROUP:'#10+
  ' - in the tree, select the root item named ''Groups'' then click on the +.'+#10+
  ' - or right click on an empty space of the tree.'#10#10+
  'RENAME OR DELETE A GROUP:'#10+
  ' - select a group and click on the appropriate button on the right.'#10#10+
  'ADD A GROUP DESCRIPTION:'#10+
  ' - select a group and click the page icon.'#10+
  '   Descriptions are saved in the game.'#10#10+
  'ADD A LEVEL IN A GROUP:'#10+
  ' - select a group and click on the +.'#10#10+
  'EDIT, RENAME, DUPLICATE OR DELETE A LEVEL:'#10+
  ' - select a level and click on the appropriate button on the right.'#10#10+
  'ADD A LEVEL DESCRIPTION:'#10+
  ' - select a level and click the page icon.'#10#10+

  'EXPORTED PASCAL UNIT:'#10+
  '  Pascal code is generated automatically in the unit Units\Levels\u_gamelevels.pas'#10+
  '  This unit contains the data of your levels and a class that encapsulate functionalities to use in your screen.'#10+
  '  See the comments in this unit.');
end;

procedure TFrameToolLevelBank.UpdateWidgetState;
begin
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

