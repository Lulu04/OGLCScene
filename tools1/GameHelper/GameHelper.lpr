program GameHelper;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene,
  u_common,
  form_main, u_screen_title, u_project, u_atlas_manager, form_tools,
u_app_pref, Dialogs;

{$R *.res}

begin
  Application.Initialize;

  AppPref := TAppPref.CreateFolder('LuluGame');
  if not AppPref.FolderCreated then ShowMessage('Application preferences can not be saved...')
    else AppPref.Load;

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTools, FormTools);
  Application.Run;

  AppPref.Free;

end.

