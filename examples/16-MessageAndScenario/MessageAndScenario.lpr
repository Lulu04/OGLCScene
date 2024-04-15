program MessageAndScenario;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene,
  u_common,
  form_main, screen_demo, u_sprite_def;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.

