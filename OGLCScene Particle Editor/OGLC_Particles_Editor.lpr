program OGLC_Particles_Editor;

{$mode objfpc}{$H+}
{$DEFINE ProgrammePrincipal}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene,
  common,
  Main, screen_Home, Frame_CurveEdit, frame_ShowColor;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Principale, Form_Principale);
  Application.Run;

end.

