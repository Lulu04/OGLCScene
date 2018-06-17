
{$ifdef oglcINTERFACE}
type

TSimpleSurface = class;
TSimpleSurfaceWithEffect = class;
TSprite = class;
TTileEngine = class;

//TCallBackNotification = procedure ( Sender : TSimpleSurfaceWithEffect ) of object;

TBlink = record
          BlinkToGo                    : integer ;
          VisibleTime                  : single  ;
          InvisibleTime                : single  ;
          CountVisible                 : single  ;
          CountInvisible               : single  ;
         end;

TRotationAroundPoint = record
                       InProgress       : boolean ;
                       XPt              : single ;
                       YPt              : single ;
                       AnglePerSecond   : single ;
                       ActualAngleRad   : single ;
                       Radius           : single ;
                      end;

TBindToSprite = record
                 InProgress       : boolean       ;
                 ParentSurface    : TSimpleSurface ;
                 OffsetX          : single        ;
                 OffsetY          : single        ;
                end;

TLayer = class;

{ TSurfaceList }

TSurfaceList = class
   Constructor Create ;
   Destructor Destroy ; override ;
   protected
     FList : TList ;
     function GetSurfaceCount: integer; inline;
     function GetSurface(aIndex: integer): TSimpleSurfaceWithEffect ; inline;
     procedure Delete ( Index:integer );
   public
     procedure Clear; virtual;
     function Add ( aSurface : TSimpleSurfaceWithEffect ) : integer ; virtual ;
     procedure Insert ( aIndex : integer ; aSurface : TSimpleSurfaceWithEffect ) ; virtual ;
     property SurfaceCount : integer read GetSurfaceCount ;
     property Surface[aIndex:integer]:TSimpleSurfaceWithEffect read GetSurface ; default;
end;

{ TLayer }

TLayer = class (TSurfaceList)
   private
    FParentScene : TOGLCScene ;
    FVisible : boolean ;
    FFreeze  : boolean ;
    // Blink
    FBlink : TBlink ;
    // Tile engine
    FTileEngine : TTileEngine ;
    FTileEngineAutoFree : boolean ;
   public
    Constructor Create;
    Destructor Destroy; override;
    procedure Draw;
    procedure Update( const aElapsedTime : single ) ;
    procedure Clear; override;
   public
    // Tile engine
    procedure AssignTileEngine( aTileEngine: TTileEngine; AutoFree: boolean=TRUE );
   public
    Angle: TFParam;
    Opacity: TBoundedFParam;

    procedure KillAll;

    // Blink
    procedure Blink ( aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink

// Methods below affect the surfaces of the layer
//    if  aGroup = -1  methods affect all surfaces of the layer.
//    if  aGroup > -1  methods affect only the surfaces with specified group number
// Note: every created surface have group property set to zero by default.
    procedure GroupKill ( aGroup: integer) ;
    procedure GroupVisible ( aGroup: integer; aValue:boolean ) ;
    procedure GroupFreeze (aGroup: integer ; aValue:boolean ) ;
    procedure GroupOpacity ( aGroup: integer; aNewOpacity:byte );
    procedure GroupOpacityVariation ( aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word=idcLinear ) ;
    procedure GroupTint ( aGroup : integer; aTint : TBGRAPixel );
    procedure GroupTintOpacityVariation( aGroup : integer; aNewOpacity : byte ; aDuration: single; aCurve: Word=idcLinear);
    procedure GroupMoveTo ( aGroup : integer; aX , aY : single ; aDuration : single ; aCurve:Word = idcLinear ) ;
    procedure GroupMoveRelative ( aGroup : integer; aDeltaX , aDeltaY : single ; aDuration : single ; aCurve:Word = idcLinear ) ;
    procedure GroupBlink ( aGroup : integer; aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink
    procedure GroupScale( aGroup : integer; aHScale, aVScale : single ; aDuration : single ; aHScaleCurve:Word=idcLinear; aVScaleCurve : Word=idcLinear ) ; // aHScale, aVScale = -1 for no change
    procedure GroupRotate ( aGroup : integer; aAnglePerSecond: single) ;
    procedure GroupFlipH ( aGroup : integer; aValue:boolean );
    procedure GroupFlipV ( aGroup : integer; aValue:boolean );

    // Collision test: return surface collided, NIL if no collision
    function CollisionTest( aSurface: TSimpleSurfaceWithEffect ) : TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: single ): TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aX, aY, aWidth, aHeight: single) : TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aX, aY, aWidth, aHeight: single; DontTestSurface: TSimpleSurfaceWithEffect) : TSimpleSurfaceWithEffect; overload;

    // layer's property
    property Freeze : boolean read FFreeze write FFreeze ;    // Freeze all moves and variations of the layer and every surfaces of the layer
    property Visible : boolean read FVisible write FVisible;
end;


TSurfaceChangeLayer= record
                 Surface: TSimpleSurfaceWithEffect;
                 NewLayerIndex: integer;
end;
TArrayOfSurfaceChangeLayer = array of TSurfaceChangeLayer;

{ TLayerList }

TLayerList = class
   Constructor Create;
   Destructor Destroy; override;
   private
     FList : TList;
     function GetLayerCount: integer;
     function GetLayer(Index: integer): TLayer;
     function GetSurfaceCount: integer;
     procedure SetLayerCount(AValue: integer ); virtual ;
     procedure AddSurfaceToLayer ( aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0 );

     procedure InsertSurfaceToLayer ( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0 );
   private
     FListSurfaceChangeLayer: TArrayOfSurfaceChangeLayer;
   public
    procedure ClearAllLayer; // delete all surfaces in all layer
    procedure ClearLayer( aLayerIndex:integer ); // delete all surfaces in specified layer

    // move a surface on another layer
    procedure SurfaceChangeLayer( aSurface: TSimpleSurfaceWithEffect; aNewLayerIndex: integer );

    procedure AllLayerVisible;
    procedure OnlyThisLayerVisible ( tab:array of integer );

    property LayerCount:integer read GetLayerCount write SetLayerCount;
    property Layer[Index:integer]:TLayer read GetLayer;
    property SurfaceCount:integer read GetSurfaceCount;
end;

TProcedureWrapper = procedure of object;
TProcedureDrawWrapper = procedure ( const aLayerPercentOpacity:single ) of object ;
TProcedureUpdateWrapper = procedure ( const DT:single ) of object ;
TProcedureDestroyWrapper = procedure of object;

{$endif oglcINTERFACE}



{$ifdef oglcIMPLEMENTATION}
{ TSurfaceList }

constructor TSurfaceList.Create;
begin
 FList := TList.Create ;
end;

destructor TSurfaceList.Destroy;
begin
 Clear ;
 FList.Free ;
 inherited Destroy;
end;

function TSurfaceList.GetSurfaceCount: integer;
begin
 Result := FList.Count ;
end;

function TSurfaceList.GetSurface(aIndex: integer): TSimpleSurfaceWithEffect;
begin
 Result := TSimpleSurfaceWithEffect ( FList.Items[aIndex] ) ;
end;


function TSurfaceList.Add(aSurface: TSimpleSurfaceWithEffect): integer;
begin
 Result := FList.Add ( aSurface ) ;
 aSurface.SceneIndex := Result ;
end;


procedure TSurfaceList.Insert(aIndex: integer;aSurface: TSimpleSurfaceWithEffect);
begin
 if aIndex < 0 then aIndex := 0;
 if aIndex < FList.Count
   then begin
         aSurface.SceneIndex := aIndex ;
         FList.Insert ( aIndex , aSurface ) ;
   end else begin
    FList.Add( aSurface );
    aSurface.SceneIndex := FList.Count-1;
   end;
end;

procedure TSurfaceList.Delete( Index:integer ) ;
var i :integer;
begin
 for i:=Index+1 to FList.Count-1 do
  TSimpleSurface( FList.Items[i]).SceneIndex := TSimpleSurface( FList.Items[i]).SceneIndex - 1 ;
 FList.Delete ( Index );
// FList.Pack;
end;

procedure TSurfaceList.Clear;
var  o : TSimpleSurface ;
begin
  while FList.Count > 0 do
   begin
    o := TSimpleSurface( FList.Items[0] );
    FList.Delete ( 0 );
    o.Free;
   end;
end;


{ TLayer }

constructor TLayer.Create;
begin
 inherited Create;
 FVisible := TRUE ;
 FFreeze := FALSE ;
 // Blink
 FBlink.BlinkToGo := 0 ;
 // Opacity variation
 Opacity:= CreateBoundedFParam(0, 255);
 Opacity.Value := 255;
 // Self rotation
 Angle:= TFParam.Create;
 // TileEngine
 FTileEngine := NIL;
end;

destructor TLayer.Destroy;
begin
 FreeAndNil( Angle );
 FreeAndNil( Opacity );

 if ( FTileEngine <> NIL ) and FTileEngineAutoFree
   then begin
         FTileEngine.Free ;
         FTileEngine := NIL ;
   end;
 inherited Destroy;
end;

procedure TLayer.Draw;
var i : integer;
    FOpacityPercent: single;
begin
 if not FVisible then exit;
 if Angle.Value <> 0.0
   then begin
         glPushMatrix;
         glTranslatef(FParentScene.Width * 0.5, FParentScene.Height * 0.5, 0 );
         glRotatef(Angle.Value, 0, 0, 1);
         glTranslatef(-FParentScene.Width * 0.5, -FParentScene.Height * 0.5, 0 );
        end;
 FOpacityPercent:= Opacity.Value/255;

 if FTileEngine <> NIL then FTileEngine.Draw( FOpacityPercent ) ;

 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i] ) do if Visible then Draw( FOpacityPercent );

 if Angle.Value <> 0.0 then glPopMatrix;
end;

procedure TLayer.Update(const aElapsedTime: single);
var i: integer;
begin
 if FFreeze then exit ;
 // Do Blink
 with FBlink do
 if BlinkToGo <> 0
 then begin
       if CountVisible > 0
         then begin // Layer is visible
               FVisible := TRUE ;
               CountVisible -= aElapsedTime ;
               CountInvisible := InvisibleTime ;
              end
         else begin // Layer is invisible
               FVisible := FALSE ;
               CountInvisible -= aElapsedTime ;
               if CountInvisible <= 0
                 then begin
                       CountVisible := VisibleTime ;
                       if BlinkToGo > 0 then dec ( BlinkToGo ) ;
                       if BlinkToGo = 0 then FVisible := TRUE ;
                      end;
              end;
      end;

 Angle.OnElapse( aElapsedTime );

 Opacity.OnElapse( aElapsedTime );

 if FTileEngine <> NIL then FTileEngine.Update( aElapsedTime ) ;

 for i:=0 to FList.Count-1 do
   TSimpleSurfaceWithEffect( FList.Items[i] ).Update( aElapsedTime );
end;

procedure TLayer.Clear;
begin
 inherited Clear;

 {
  if ( FTileEngine <> NIL ) and FTileEngineAutoFree
    then FTileEngine.Free;
  FTileEngineAutoFree := FALSE;
  FTileEngine := NIL;
 }
end;

procedure TLayer.AssignTileEngine(aTileEngine: TTileEngine; AutoFree: boolean);
begin
 if ( FTileEngine <> NIL ) and FTileEngineAutoFree
   then FreeAndNil( FTileEngine );

 FTileEngine := aTileEngine;
 if FTileEngine <> NIl
   then begin
         FTileEngine.FParentScene := FParentScene;
         FTileEngine.FParentLayer := Self;
   end;

 FTileEngineAutoFree := AutoFree;
end;

procedure TLayer.KillAll;
var i: integer;
begin
 for i:=0 to SurfaceCount-1 do Surface[i].Kill;
end;

procedure TLayer.Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single);
begin
 with FBlink do
  begin
   BlinkToGo := aNumberOfBlink ;
   VisibleTime := aVisibleTime ;
   InvisibleTime := aInvisibleTime ;
   CountVisible := aVisibleTime ;
   CountInvisible := aInvisibleTime ;
  end;
end;


procedure TLayer.GroupKill ( aGroup : integer ) ; // kill all surface with group=aGroup
var i:integer ;
begin
 if aGroup = -1 then
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Kill;
end;

procedure TLayer.GroupVisible ( aGroup : integer; aValue:boolean ) ;
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Visible := aValue;
end;

procedure TLayer.GroupFreeze(aGroup: integer; aValue:boolean );
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Freeze:=aValue ;
end;

procedure TLayer.GroupTint(aGroup: integer; aTint: TBGRAPixel);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Tint.Value := aTint ;
end;

procedure TLayer.GroupMoveTo( aGroup: integer; aX, aY: single; aDuration: single; aCurve:Word);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then MoveTo(aX,aY,aDuration,aCurve);
end;

procedure TLayer.GroupMoveRelative(aGroup: integer; aDeltaX, aDeltaY: single;
  aDuration: single; aCurve: Word);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
   if ( aGroup = -1 ) or ( Group = aGroup) then MoveRelative(aDeltaX,aDeltaY,aDuration,aCurve);
end;

procedure TLayer.GroupBlink(aGroup: integer; aNumberOfBlink: integer;
  aVisibleTime, aInvisibleTime: single);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Blink(aNumberOfBlink,aVisibleTime,aInvisibleTime);
end;

procedure TLayer.GroupScale(aGroup: integer; aHScale, aVScale: single;
  aDuration: single; aHScaleCurve:Word; aVScaleCurve: Word);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup)
      then begin
            Scale.x.ChangeTo( aHScale, aDuration, aHScaleCurve);
            Scale.y.ChangeTo( aVScale, aDuration, aVScaleCurve);
      end;
end;

procedure TLayer.GroupRotate(aGroup: integer; aAnglePerSecond: single);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Angle.AddConstant( aAnglePerSecond );
end;

procedure TLayer.GroupOpacityVariation(aGroup: integer;
  aNewOpacity: byte; aDuration: single; aCurve: Word);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Opacity.ChangeTo(aNewOpacity,aDuration,aCurve);
end;

procedure TLayer.GroupTintOpacityVariation(aGroup: integer;
  aNewOpacity: byte; aDuration: single; aCurve: Word);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Tint.ChangeTo(aNewOpacity,aDuration,aCurve);
end;

procedure TLayer.GroupOpacity(aGroup: integer; aNewOpacity: byte);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then Opacity.Value := aNewOpacity;
end;

procedure TLayer.GroupFlipH(aGroup: integer; aValue: boolean);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then FlipH:=aValue;
end;

procedure TLayer.GroupFlipV(aGroup: integer; aValue: boolean);
var i:integer ;
begin
 for i:=0 to FList.Count-1 do
  with TSimpleSurfaceWithEffect( FList.Items[i]) do
    if ( aGroup = -1 ) or ( Group = aGroup) then FlipV:=aValue;
end;

function TLayer.CollisionTest(aSurface : TSimpleSurfaceWithEffect): TSimpleSurfaceWithEffect;
begin
 Result := CollisionTest( aSurface.X.Value, aSurface.Y.Value, aSurface.Width, aSurface.Height ) ;
end;

function TLayer.CollisionTest(aSurface: TSimpleSurfaceWithEffect; HTolerance,
  VTolerance: single): TSimpleSurfaceWithEffect;
begin
 with aSurface do
   Result := CollisionTest( X.Value+HTolerance, Y.Value+VTolerance,
                            Width-HTolerance, Height-VTolerance, aSurface );
end;

function TLayer.CollisionTest(aX, aY, aWidth, aHeight: single): TSimpleSurfaceWithEffect;
var i: integer;
begin
 Result := NIL;
 for i:=0 to FList.Count-1 do
  begin
   with Surface[i] do
    if not ( ( aX > GetRightX ) or ( aX+aWidth < x.Value ) or
             ( aY > GetBottomY) or ( aY+aHeight < y.Value ) )
       then begin
        Result := Surface[i];
        exit;
       end;
  end;
end;

function TLayer.CollisionTest(aX, aY, aWidth, aHeight: single;
  DontTestSurface: TSimpleSurfaceWithEffect): TSimpleSurfaceWithEffect;
var i: integer;
    o: TSimpleSurfaceWithEffect;
begin
 Result := NIL;
 for i:=0 to FList.Count-1 do
  begin
   o := Surface[i];
    if not ( ( aX > o.GetRightX ) or ( aX+aWidth < o.X.Value ) or
             ( aY > o.GetBottomY) or ( aY+aHeight < o.Y.Value ) ) and
      ( o <> DontTestSurface )
       then begin
        Result := o;
        exit;
       end;
  end;
end;


{ TLayerList }

constructor TLayerList.Create;
begin
 FList := TList.Create;
 SetLayerCount(1);
end;

destructor TLayerList.Destroy;
begin
 FList.Free;
 inherited Destroy;
end;

procedure TLayerList.ClearAllLayer;
var i:integer;
begin
 for i:=0 to GetLayerCount-1 do Layer[i].Clear;
end;

procedure TLayerList.ClearLayer(aLayerIndex: integer);
begin
 TLayer( FList.Items[aLayerIndex] ).Clear;
end;

function TLayerList.GetLayerCount: integer;
begin
 Result := FList.Count;
end;

function TLayerList.GetLayer(Index: integer): TLayer;
begin
 Result := TLayer(FList.Items[Index]);
end;

function TLayerList.GetSurfaceCount: integer;
var i:integer;
begin
 Result:=0;
 for i:=0 to FList.Count-1 do Result += TLayer(FList.Items[i]).GetSurfaceCount;
end;

procedure TLayerList.SetLayerCount(AValue: integer );
var i:integer;
begin
 if AValue = FList.Count then exit;
 if AValue > FList.Count
   then begin
         for i:=1 to AValue-FList.Count do
          FList.Add( TLayer.Create );
   end
 else begin
       while AValue < FList.Count do
        begin
         Layer[FList.Count-1].Free;
         FList.Delete(FList.Count-1);
        end;
 end;
end;

procedure TLayerList.AddSurfaceToLayer( aSurface : TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
 if aLayerIndex >= FList.Count
   then aLayerIndex:=FList.Count-1
   else if aLayerIndex < 0 then aLayerIndex := 0;
 Layer[aLayerIndex].Add(aSurface);
end;

procedure TLayerList.InsertSurfaceToLayer(aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer);
begin
 if aLayerIndex >= FList.Count
   then aLayerIndex:=FList.Count-1
   else if aLayerIndex < 0 then aLayerIndex := 0;
 Layer[aLayerIndex].Insert(aSurfaceIndex, aSurface );
end;

procedure TLayerList.SurfaceChangeLayer(aSurface: TSimpleSurfaceWithEffect;
  aNewLayerIndex: integer);
var i: integer;
begin
 if ( aNewLayerIndex < 0 ) or ( aNewLayerIndex >= LayerCount ) then exit;
 if aSurface = NIL then exit;
 if aSurface.FParentLayer = NIL then exit;
 if aSurface.FParentLayer = Layer[aNewLayerIndex] then exit;

 // push the 'ChangeLayer' request in list. it will be processed at the right place in the scene loop
 i := Length( FListSurfaceChangeLayer );
 SetLength( FListSurfaceChangeLayer, i+1 );
 FListSurfaceChangeLayer[i].Surface := aSurface;
 FListSurfaceChangeLayer[i].NewLayerIndex := aNewLayerIndex;

end;

procedure TLayerList.AllLayerVisible;
var i:integer;
begin
 for i:=0 to FList.Count-1 do TLayer(FList.Items[i]).Visible:=TRUE;
end;

procedure TLayerList.OnlyThisLayerVisible(tab: array of integer);
var i:integer;
begin
 for i:=0 to FList.Count-1 do TLayer(FList.Items[i]).Visible:=FALSE;
 for i:=low(tab) to high(tab) do
   if ( tab[i] < FList.Count ) and ( tab[i] >= 0 )
     then TLayer(FList.Items[tab[i]]).Visible := TRUE;
end;
{$endif oglcIMPLEMENTATION}


