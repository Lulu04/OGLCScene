{
  Message to communicate between surfaces.
  example:
            Sprite1,
            Sprite2: TSprite;
            ....

        - Send a message with user value 3 to sprite1
            Sprite1.AddMessage( 3 );

        - Wait 1.5 sec then send a message with user value 5 to sprite1
            Sprite1.AddMessage( 5, 1.5 );
}

{$ifdef oglcINTERFACE}
type
PMessageObject = ^TMessageObject;
TMessageObject = record
                  Delay    : single;
                  UserValue: word;
                  CallBack : TMessageCallBack;
                  Next     : PMessageObject;
                  Prev     : PMessageObject;
end;

{ TMessageManager }
TOGLCGMessageListSpec = specialize TOGLCGList<TMessageObject>;

TMessageList = class( TOGLCGMessageListSpec )
private
 procedure ProcessMessages( const aElapsedTime: single );
public
 procedure Add( aUserValue: word; aCallBack: TMessageCallBack; aDelay: single=0 );
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

procedure TMessageList.ProcessMessages(const aElapsedTime: single);
var p, p1 :PMessageObject;
begin
 p := FInstance0;
 while p^.Next<>NIL do p:=p^.Next;

 while p<>NIL do begin
   p^.Delay := p^.Delay - aElapsedTime;
   if p^.Delay <= 0 then begin
     if p^.CallBack <> NIL then p^.CallBack( p^.UserValue );
     p1 := p;
     p:=p^.Prev;
     DoDelete( p1 );
   end else p:=p^.Prev;
 end;
end;

procedure TMessageList.Add(aUserValue: word; aCallBack: TMessageCallBack; aDelay: single);
var m: PMessageObject;
begin
 m := AddInstanceToList;
 m^.Delay := aDelay;
 m^.CallBack := aCallBack;
 m^.UserValue := aUserValue;
end;

{$endif oglcIMPLEMENTATION}

