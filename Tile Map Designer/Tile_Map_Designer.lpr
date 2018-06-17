program Tile_Map_Designer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, common, WindowsScene, Main,
  screen_map, tileset_manager, uAskTileSize, uabout, uAskGroundType,
  uinsertlinecolumn, uAskMapSize, uexportgroundtype,
  VelocityCurve,
  uaskeventvalue, usavemap, uexporteventtype;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Principale, Form_Principale);
  Application.CreateForm(TWindow_Scene, Window_Scene);
  Application.CreateForm(TForm_AskTileSize, Form_AskTileSize);
  Application.CreateForm(TForm_About, Form_About);
  Application.CreateForm(TForm_AskGroundType, Form_AskGroundType);
  Application.CreateForm(TForm_InsertLineColumn, Form_InsertLineColumn);
  Application.CreateForm(TForm_AskMapSize, Form_AskMapSize);
  Application.CreateForm(TForm_ExportGroundType, Form_ExportGroundType);
  Application.CreateForm(TForm_AskEvent, Form_AskEvent);
  Application.CreateForm(TForm_ExportEvent, Form_ExportEvent);
  Application.Run;

end.

