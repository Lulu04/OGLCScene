unit frame_tool_leveleditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls;

type

  { TFrameToolLevelEditor }

  TFrameToolLevelEditor = class(TFrame)
    PC1: TPageControl;
  private

  public
    procedure OnShow;

  end;

implementation

{$R *.lfm}

{ TFrameToolLevelEditor }

procedure TFrameToolLevelEditor.OnShow;
begin

end;

end.

