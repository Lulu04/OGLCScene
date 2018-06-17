{$ifdef oglcINTERFACE}
type

// GENERIC LIST
{ TOGLCGList }

generic TOGLCGList<T> = class
 Constructor Create;
 Destructor Destroy; override;
private
 type PT=^T;
private
 FInstance0: PT;
 FCount: integer;
 function AddInstanceToList: PT;
 procedure DoDelete( anInstance: PT );
public
 procedure Clear;
end;


{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TOGLCGList }

constructor TOGLCGList.Create;
begin
 New( FInstance0 );
 FInstance0^.prev := NIL ;
 FInstance0^.Next := NIL ;
 FCount := 0 ;
end;

destructor TOGLCGList.Destroy;
begin
 Clear;
 Dispose( FInstance0 );
 inherited Destroy;
end;

function TOGLCGList.AddInstanceToList: PT;
var p: PT;
begin
 p := FInstance0;
 while p^.Next <> NIL do p := p^.Next;
 New( Result );
 p^.Next := Result;
 Result^.Next := NIL;
 Result^.Prev := p;
 inc(FCount);
end;

procedure TOGLCGList.DoDelete(anInstance: PT);
var p, n:PT;
begin
 if anInstance^.Next <> NIL
   then begin
         p := anInstance^.Prev;
         n := anInstance^.Next;
         p^.Next := n;
         n^.Prev := p;
        end
   else begin
         p := anInstance^.Prev;
         p^.Next := NIL;
        end;
 Dispose( anInstance );
 dec(FCount);
end;

procedure TOGLCGList.Clear;
begin
 while FInstance0^.Next <> NIL do DoDelete ( FInstance0^.Next );
end;



{$endif oglcIMPLEMENTATION}

