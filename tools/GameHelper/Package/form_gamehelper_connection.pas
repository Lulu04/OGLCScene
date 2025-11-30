unit form_gamehelper_connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls,
  Process;

type

  TGameHelperProcess = class
  private
    FProcess: TProcess;
    FBufferIndex: integer;
    FBuffer: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(const aExecutable, aLazarusProjectFilename: string);
  end;

  { TFormGameHelperConnexion }

  TFormGameHelperConnexion = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    BUserWantToQuit: TSpeedButton;
    Timer1: TTimer;
    procedure BUserWantToQuitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FUserWantToQuit: boolean;
    FGameHelperProcess: TGameHelperProcess;
    FExecutable, FLazarusProjectFilename: string;
    procedure ProcessOnIdleEvent(Sender: TObject; var Done: Boolean);
  public
    procedure ShowMessage(aMess: string);
    procedure SetParameters(const aExecutable, aLazarusProjectFilename: string);
    property UserWantToQuit: boolean read FUserWantToQuit write FUserWantToQuit;
  end;

var
  FormGameHelperConnexion: TFormGameHelperConnexion;


implementation
uses u_package_register, u_respons_commontype;

{$R *.lfm}

{ TGameHelperProcess }

constructor TGameHelperProcess.Create;
begin
  FBuffer := '';
  SetLength(FBuffer, 2048);
  FBufferIndex := 1;
  FProcess := NIL;
end;

destructor TGameHelperProcess.Destroy;
begin
  if FProcess <> NIL then FProcess.Free;
  FProcess := NIL;
  inherited Destroy;
end;

procedure TGameHelperProcess.Start(const aExecutable, aLazarusProjectFilename: string);
var messageLine: string;
  ByteRead: byte;
  flagQuit: boolean;
begin
  // open Game Helper with command line parameters
  // <executable>  path_for_respons  path_to_current_project
  FProcess := TProcess.Create(NIL);
  FProcess.Executable := aExecutable;
  FProcess.Parameters.Add(CI_PARAMETER_HEADER);
  FProcess.Parameters.Add(aLazarusProjectFilename);
  FProcess.Options := FProcess.Options + [poUsePipes];
  FProcess.PipeBufferSize := 2048;
  FProcess.Execute;
  Sleep(100);

  FBuffer := '';
  SetLength(FBuffer, 2048);
  FBufferIndex := 1;
  flagQuit := False;
  repeat
    while FProcess.Output.NumBytesAvailable > 0 do begin
      ByteRead := FProcess.Output.ReadByte;
      if ByteRead <> 13 then  // skip #13
        if ByteRead = 10 then begin  // end of line
          messageLine := Copy(FBuffer, 1, FBufferIndex-1);
          FormGameHelperConnexion.ShowMessage(messageline);
          flagQuit := ConnectionUtils.DecodeMessageFromGameHelper(messageLine);
          FBufferIndex := 1;
        end else begin
          FBuffer[FBufferIndex] := Chr(ByteRead);
          inc(FBufferIndex);
        end;
    end;

    Sleep(10);
    Application.ProcessMessages;
  until flagQuit or not FProcess.Running or FormGameHelperConnexion.UserWantToQuit;

  if FormGameHelperConnexion.UserWantToQuit then begin
    ConnectionUtils.ShowMessageInIDE('Try to terminate Game Helper');
    FProcess.Terminate(0);
    FProcess.WaitOnExit(1000);
    //Sleep(100);
    FormGameHelperConnexion.ModalResult := mrCancel;
  end else FormGameHelperConnexion.ModalResult := mrOk;

  FProcess.Free;
  FProcess := NIL;
end;

{ TFormGameHelperConnexion }

procedure TFormGameHelperConnexion.BUserWantToQuitClick(Sender: TObject);
begin
  BUserWantToQuit.Enabled := False;
  UserWantToQuit := True;
end;

procedure TFormGameHelperConnexion.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.RemoveOnIdleHandler(@ProcessOnIdleEvent);
end;

procedure TFormGameHelperConnexion.FormShow(Sender: TObject);
begin
  Memo1.Clear;
  UserWantToQuit := False;
  FGameHelperProcess := NIL;

  Application.AddOnIdleHandler(@ProcessOnIdleEvent);
end;

procedure TFormGameHelperConnexion.Timer1Timer(Sender: TObject);
begin
  Label2.Visible := not Label2.Visible;
end;

procedure TFormGameHelperConnexion.ProcessOnIdleEvent(Sender: TObject; var Done: Boolean);
begin
  if not Assigned(FGameHelperProcess) then begin
    FGameHelperProcess := TGameHelperProcess.Create;
    FGameHelperProcess.Start(FExecutable, FLazarusProjectFilename);
  end;
  Done := True;
end;

procedure TFormGameHelperConnexion.ShowMessage(aMess: string);
begin
  Memo1.Append(aMess);
end;

procedure TFormGameHelperConnexion.SetParameters(const aExecutable, aLazarusProjectFilename: string);
begin
  FExecutable := aExecutable;
  FLazarusProjectFilename := aLazarusProjectFilename;
end;

end.

