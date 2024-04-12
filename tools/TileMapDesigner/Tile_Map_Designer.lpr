program Tile_Map_Designer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, common, u_main, u_tool_window, screen_map,
  tileset_manager, uAskTileSize, uabout, uAskGroundType, uinsertlinecolumn,
  uAskMapSize, uexportgroundtype, VelocityCurve, uaskeventvalue, usavemap,
  uexporteventtype, umaps, uasknewlayermapinfo, uaskrenamemap, u_tileset_edit,
  u_tool_minimap, u_tool_layer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TForm_Tools, Form_Tools);
  Application.CreateForm(TForm_AskTileSize, Form_AskTileSize);
  Application.CreateForm(TForm_About, Form_About);
  Application.CreateForm(TForm_AskGroundType, Form_AskGroundType);
  Application.CreateForm(TForm_InsertLineColumn, Form_InsertLineColumn);
  Application.CreateForm(TForm_AskMapSize, Form_AskMapSize);
  Application.CreateForm(TForm_ExportGroundType, Form_ExportGroundType);
  Application.CreateForm(TForm_AskEvent, Form_AskEvent);
  Application.CreateForm(TForm_ExportEvent, Form_ExportEvent);
  Application.CreateForm(TForm_AskNewLayerMapInfo, Form_AskNewLayerMapInfo);
  Application.CreateForm(TForm_RenameMap, Form_RenameMap);
  Application.CreateForm(TForm_Minimap, Form_Minimap);
  Application.CreateForm(TForm_ToolLayer, Form_ToolLayer);
  Application.Run;

end.

