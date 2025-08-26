{$ifdef oglcINTERFACE}
type

{ TGenericReusableSurfaceContainer }

generic TGenericReusableSurfaceContainer<T> = class
private type
  TReusableSurfaceItem = record
    surface: TSimpleSurfaceWithEffect;
    activated: boolean;
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
  function GetInactiveSurface: T;
  //
  procedure SetInactive(aSurface: T);

  property Surfaces[Index: SizeUInt]: T read GetSurface; Default;
  property Count: integer read GetCount;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}

{ TGenericReusableSurfaceContainer }

procedure TGenericReusableSurfaceContainer.AddNewItem;
var o: TReusableSurfaceItem;
begin
  o.activated := True;
  o.surface := CreateNewSurface;
  o.surface.Tag1 := FList.Size;
  FList.PushBack(o);
end;

function TGenericReusableSurfaceContainer.GetCount: integer;
begin
  Result := integer(FList.Size);
end;

function TGenericReusableSurfaceContainer.GetSurface(Index: SizeUInt): T;
begin
  Result := T(FList.Mutable[Index]^.surface);
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

function TGenericReusableSurfaceContainer.GetInactiveSurface: T;
var i: SizeUInt;
begin
  if FLastInactiveIndex <> SizeUInt.MaxValue then begin
    with FList.Mutable[FLastInactiveIndex]^ do begin
      activated := True;
      Result := T(surface);
      with surface do begin
        Visible := True;
        Freeze := False;
      end;
    end;
    FLastInactiveIndex := SizeUInt.MaxValue;
    exit;
  end;

  if FList.Size > 0 then
    for i:=0 to FList.Size-1 do
      with FList.Mutable[i]^ do
        if not activated then begin
          activated := True;
          Result := T(surface);
          with surface do begin
            Visible := True;
            Freeze := False;
          end;
          exit;
        end;

  AddNewItem;
  Result := T(FList.Mutable[FList.Size-1]^.surface);
end;

procedure TGenericReusableSurfaceContainer.SetInactive(aSurface: T);
begin
  with TSimpleSurfaceWithEffect(aSurface) do begin
    Visible := False;
    Freeze := True;
    FList.Mutable[Tag1]^.activated := False;
    if (FLastInactiveIndex = SizeUInt.MaxValue) or (FLastInactiveIndex < Tag1) then
      FLastInactiveIndex := Tag1;
  end;
end;

{$endif oglcIMPLEMENTATION}

