program GameHelper;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, u_common, form_main,
  u_screen_spritebuilder, u_project, u_texture_list,
  u_app_pref, Dialogs, u_surface_list, u_utils, u_screen_template, u_spritebank,
  u_ui_handle, u_screen_spritebank, frame_tool_spritebuilder,
frame_tool_spritebank, u_collisionbody_list, u_ui_atlas, u_datamodule,
u_undo_redo, u_undoredo_spritebuilder;

{$R *.res}

begin
  Application.Initialize;

  AppPref := TAppPref.CreateFolder('LuluGame');
  if not AppPref.FolderCreated then ShowMessage('Application preferences can not be saved...')
    else AppPref.Load;

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;

  AppPref.Free;

end.

