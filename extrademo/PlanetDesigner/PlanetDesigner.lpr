program PlanetDesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, OGLCScene, u_common, form_main, screen_demo,
  u_ProceduralPlanet, u_export_to_string, form_importfromforum;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCopyToClipboard, FormCopyToClipboard);
  Application.CreateForm(TFormImportPreset, FormImportPreset);
  Application.Run;

end.

