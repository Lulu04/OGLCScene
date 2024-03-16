{$ifdef oglcINTERFACE}
type

// You can organize your game/application by screen
// i.e. one screen for the title, another for the game itself, ...
// Use TOGLCScene.RunScreen(...) to start a screen.




{ TScreenTemplate }

TScreenTemplate = class
private
  FMessageList: TMessageList;
public
  constructor Create;
  destructor Destroy; override;
public
  // Set to true to free automatically this screen when the application leave it.
  FreeWhenLeave: boolean;

  // Post a message to the screen message queue. It will be processed in the next execution of Update method.
  // The delay allows to post the message only after a lapse of time (in seconds): it's usefull to construct chained animation.
  procedure PostMessage(UserValue: TUserMessageValue; aDelay: single=0);
  // Override to process the received messages.
  procedure ProcessMessage({%H-}UserValue: TUserMessageValue); virtual;
  // Delete all message from the list
  procedure ClearMessageList;

  // override to create all objects related to this screen.
  procedure CreateObjects; virtual; abstract;
  // override to free all objects related to this screen.
  procedure FreeObjects; virtual; abstract;
  // override to update objects according time. don't forget to call inherited.
  procedure Update(const AElapsedTime: single); virtual;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TScreenTemplate }

procedure TScreenTemplate.ClearMessageList;
begin
  FMessageList.Clear;
end;

constructor TScreenTemplate.Create;
begin
  FMessageList := TMessageList.Create(@ProcessMessage);
end;

destructor TScreenTemplate.Destroy;
begin
  FreeAndNil(FMessageList);
  inherited Destroy;
end;

procedure TScreenTemplate.PostMessage(UserValue: TUserMessageValue; aDelay: single);
begin
  FMessageList.Add(UserValue, aDelay);
end;

procedure TScreenTemplate.ProcessMessage(UserValue: TUserMessageValue);
begin
  // override to process the received message
end;

procedure TScreenTemplate.Update(const AElapsedTime: single);
begin
  FMessageList.ProcessMessages(AElapsedTime);
end;

{$endif oglcIMPLEMENTATION}

