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
var p :PMessageObject;
begin

 p := FInstance0;
 while p^.Next <> NIL do begin
   p^.Next^.Delay := p^.Next^.Delay - aElapsedTime;
   if p^.Next^.Delay <= 0
     then begin
           if p^.Next^.CallBack <> NIL then p^.Next^.CallBack ( p^.Next^.UserValue );
           DoDelete(p^.Next);
     end;
   p := p^.Next;
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

