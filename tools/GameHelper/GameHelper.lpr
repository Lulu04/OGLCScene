program GameHelper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, u_common, form_main,
  u_screen_spritebuilder, u_project, u_texture_list, u_app_pref, Dialogs,
  u_surface_list, u_utils, u_screen_template, u_spritebank, u_ui_handle,
  u_screen_spritebank, frame_tool_spritebuilder, frame_tool_spritebank,
  u_collisionbody_list, u_datamodule, u_undo_redo, u_presetmanager,
  u_undoredo_spritebuilder, u_posture_list, frame_tool_leveleditor,
  u_screen_levelbank, u_levelbank, frame_texturelist, u_screen_leveleditor,
  frame_tool_levelbank, u_layerlist, frame_viewlayerlist, form_projectconfig,
  form_showhelp, form_editdeformationgrid, form_editgradient, frame_gradientrow,
  u_screen_list, form_newproject, u_target_lazarusproject, frame_viewlevelbank,
  frame_tool_screenbank, u_resourcestring, u_connection_to_ide,
  form_enter_description, frame_edit_uibodyshape, frame_tool_uipaneleditor,
  frame_viewfontbank, u_screen_fontbank, u_screen_uipaneleditor,
  u_ui_objectlist, frame_tool_panelbank;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;

  AppPref := TAppPref.CreateFolder('LuluGame');
  if not AppPref.FolderCreated then ShowMessage('Application preferences can not be saved...')
    else AppPref.Load;
    Application.CreateForm(TFormMain, FormMain);
    Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;

  AppPref.Free;
end.

