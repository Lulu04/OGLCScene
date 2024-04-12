{$ifdef oglcINTERFACE}
type

TUserMessageValue = $00000000..$0FFFFFF;

PMessageItem = ^TMessageItem;
TMessageItem = record
  Delay: single;
  UserValue: TUserMessageValue;
  Next,Prev: PMessageItem;
end;

TMessageCallBack = procedure(UserValue: TUserMessageValue) of object;

{ TMessageList }

TMessageList = class(specialize TOGLCGList<TMessageItem>)
private
  FCallBack: TMessageCallBack;
  procedure ProcessMessages(const aElapsedTime: single);
public
  constructor Create(aCallBack: TMessageCallBack);
  procedure Add(aUserValue: TUserMessageValue; aDelay: single=0);
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TMessageList.ProcessMessages(const aElapsedTime: single);
var p :PMessageItem;
begin
  if Count = 0 then exit;
  p := LastItem;
  repeat
   p^.Delay := p^.Delay - aElapsedTime;
   if p^.Delay <= 0 then begin
     FCallBack(p^.UserValue);
     p := p^.Prev;
     DoDelete(p^.Next);
   end else p := p^.Prev;
  until p = FInstance0;
end;

constructor TMessageList.Create(aCallBack: TMessageCallBack);
begin
  inherited Create;
  FCallBack := aCallBack;
end;

procedure TMessageList.Add(aUserValue: TUserMessageValue; aDelay: single);
var m: PMessageItem;
begin
  m := AddInstanceToList;
  m^.Delay := aDelay;
  m^.UserValue := aUserValue;
end;

{$endif oglcIMPLEMENTATION}

