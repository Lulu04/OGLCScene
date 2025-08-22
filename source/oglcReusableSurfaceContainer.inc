{$ifdef oglcINTERFACE}
type

{ TGenericReusableSurfaceContainer }

generic TGenericReusableSurfaceContainer<T> = class
private type
  TReusableSurfaceItem = record
    surface: T;
    active: boolean;
    index: SizeUInt;
  end;
  TSurfaces = class(specialize TVector<TReusableSurfaceItem>);
private
  FList: TSurfaces;
  FLastInactiveIndex: SizeUInt;
  procedure AddNewItem;
  function GetCount: integer;
  function GetSurface(Index: SizeUInt): T;
public
  constructor Create;
  destructor Destroy; override;

  // override with the code to create a new surface instance
  // this instance is pushed in the container
  function CreateNewSurface: T; virtual; abstract;

  // return an index of an available surface
  function GetInactiveIndex: SizeUInt;
  //
  procedure SetInactive(aIndex: SizeUInt);

  property Surfaces[Index: SizeUInt]: T read GetSurface; Default;
  property Count: integer read GetCount;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TGenericReusableSurfaceContainer }

procedure TGenericReusableSurfaceContainer.AddNewItem;
var o: TReusableSurfaceItem;
begin
  o.active := True;
  o.surface := CreateNewSurface;
  o.index := FList.Size;
  FList.PushBack(o);
end;

function TGenericReusableSurfaceContainer.GetCount: integer;
begin
  Result := integer(FList.Size);
end;

function TGenericReusableSurfaceContainer.GetSurface(Index: SizeUInt): T;
begin
  Result := FList.Mutable[Index]^.surface;
end;

constructor TGenericReusableSurfaceContainer.Create;
begin
  FList := TSurfaces.Create;
  FLastInactiveIndex := SizeUInt.MaxValue;
end;

destructor TGenericReusableSurfaceContainer.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TGenericReusableSurfaceContainer.GetInactiveIndex: SizeUInt;
var i: SizeUInt;
begin
  if FLastInactiveIndex <> SizeUInt.MaxValue then begin
    Result := FLastInactiveIndex;
    FList.Mutable[Result]^.active := True;
    FLastInactiveIndex := SizeUInt.MaxValue;
    exit;
  end;

  if FList.Size > 0 then
    for i:=0 to FList.Size-1 do
      if not FList.Mutable[i]^.active then begin
        FList.Mutable[i]^.active := True;
        Result := i;
        exit;
      end;

  AddNewItem;
  Result := FList.Size-1;
end;

procedure TGenericReusableSurfaceContainer.SetInactive(aIndex: SizeUInt);
begin
  FList.Mutable[aIndex]^.active := False;

  if (FLastInactiveIndex = SizeUInt.MaxValue) or (FLastInactiveIndex < aIndex) then
    FLastInactiveIndex := aIndex;
end;

{$endif oglcIMPLEMENTATION}

