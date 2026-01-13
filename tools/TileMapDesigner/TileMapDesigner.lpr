program TileMapDesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, common, 
form_main, form_tools, screen_map,
  tileset_manager, 
form_asktilesize, form_about, form_askgroundtype, form_insertlinecolumn,
  form_askmapsize, form_exportgroundtype, form_askeventvalue, usavemap,
  form_exporteventtype, umaps, form_asknewlayermapinfo, 
form_askrenamemap, u_tileset_edit,
  u_tool_minimap, u_tool_layer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTools, FormTools);
  Application.CreateForm(TFormAskTileSize, FormAskTileSize);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormAskGroundType, FormAskGroundType);
  Application.CreateForm(TFormInsertLineColumn, FormInsertLineColumn);
  Application.CreateForm(TFormAskMapSize, FormAskMapSize);
  Application.CreateForm(TFormExportGroundType, FormExportGroundType);
  Application.CreateForm(TForm_AskEvent, Form_AskEvent);
  Application.CreateForm(TForm_ExportEvent, Form_ExportEvent);
  Application.CreateForm(TFormAskNewLayerMapInfo, FormAskNewLayerMapInfo);
  Application.CreateForm(TForm_RenameMap, Form_RenameMap);
  Application.CreateForm(TForm_Minimap, Form_Minimap);
  Application.CreateForm(TForm_ToolLayer, Form_ToolLayer);
  Application.Run;

end.

