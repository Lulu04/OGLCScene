{$ifdef oglcINTERFACE}
type

{
  Textures can be individual or contained in an atlas.
  Using Atlas is faster.
  A texture is associated with a name to retrieve it in the atlas. This is necessary when the atlas is loaded from file.
  That's why a name is required in method that add a TBGRABitmap in the atlas.
  The texture name must be UNIQUE.
  For image loaded from file, the texture name is ExtractFilename(aFilename)
}

{ TOGLCImagePackerItem }

POGLCImagePackerItem = ^TOGLCImagePackerItem;
TOGLCImagePackerItem = record
  Prev, Next: POGLCImagePackerItem;
  Texture: PTexture;
  Bitmap: TBGRABitmap;
  Placed: boolean;
  ComputedRect: TRect;
  FrameSize: TSize;
  function PixelsConsumed: integer;
  function IsOverlappedByRec(const aRec: TRect; aSpacing: integer): boolean;

  procedure SaveTo(aStream: TFileStream);
  procedure LoadFrom(aStream: TFileStream);
end;


{ TOGLCImagePacker }

TOGLCImagePacker = class(specialize TOGLCGList<TOGLCImagePackerItem>)
private
  FCurrentAtlasSize,
  FBiggerItemSize,
  FTotalPixelConsumedByItems,
  FSpacing: integer;
  FAllItemPlaced: boolean;
  FTexturedFontToFree: array of TTexturedFont;
  FLoadedFromFile: boolean;
private
  FInflateDelta: integer;
  function RawPackedImage: TBGRABitmap;
  procedure InternalInsertSorted(aItem: POGLCImagePackerItem);
  procedure SetAllItemsNotPlaced;
  function InitFromBitmap(aIma: TBGRABitmap; AFrameWidth, AFrameHeight: integer): PTexture;
  procedure FreeTexturedFonts;
  procedure FreeItemTextures;
  procedure SetInflateDelta(AValue: integer);
public
  constructor Create;
  destructor Destroy; override;

  function Add(const aFilename: string): PTexture; overload;
  function Add(const aFilename: string; AFrameWidth, AFrameHeight: integer): PTexture; overload;
  // Add a texture from an image. You must give a UNIQUE name to the texture
  // because a texture is associated with a name in order to retrieve it in the atlas (function RetrieveTextureByFileName).
  // Image Packer become the owner of aImage, don't free it in your code !
  function Add(aImage: TBGRABitmap; const aName: string): PTexture; overload;
  // Add a texture from an image with multiple frames. You must give a UNIQUE name to the texture
  // because a texture is associated with a name in order to retrieve it in the atlas (function RetrieveTextureByFileName).
  // Image Packer become the owner of aImage, don't free it in your code !
  function Add(aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer; const aName: string): PTexture; overload;
  // Add a texture from an svg file.
  // aWidth and aHeight = -1 => return an image with original size as described in svg.
  // aWidth and aHeight <> -1 => return a maximized image that fit in the given rectangle (keep aspect ratio).
  // Only aWidth = '-1' => the image width is computed from aHeight to keep aspect ratio.
  // Only aHeight = '-1' => the image height is computed from aWidth to keep aspect ratio.
  function AddFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): PTexture;

  // Construct a bitmap with several frames and add it to the Image Packer.
  // aFileNames is an array that contains the file images to use.
  // Each images must have the same size.
  function AddMultiFrameImage(const aFileNames: TStringArray; aXCount, aYCount, aSpacing: integer): PTexture;

  // Create a texture from a svg file that contain an image with multiple frames.
  function AddMultiFrameImageFromSVG(const aSVGFilename: string; aWidth, aHeight, aXCount, aYCount: integer): PTexture; overload;

  // Construct a bitmap with several frames and add it to the Image Packer.
  // aSVGFilenames is an array that contains the SVG files that represent one frame.
  // aFrameWidth, aFrameHeight: the size of one frame
  // aXCount, aYCount: the frame count horizontally and vertically
  // aSpacing: pixel to add between each frame
  function AddMultiFrameImageFromSVG(const aSVGFilenames: TStringArray;
                                     aFrameWidth, aFrameHeight,
                                     aXCount, aYCount, aSpacing: integer): PTexture; overload;

  // Add a textured font.
  // Image Packer is the owner of the returned TTexturedFont, don't free it in your code !
  function AddTexturedFont(aScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap=NIL): TTexturedFont;

  // Construct a tileset image from several svg files (tiles with same size) and
  // add it to the atlas. Tiles must be square.
  function AddTileSetFromSVG(const aName: string; aTileSize, aRowCount, aColumnCount: integer; const aSVGFileNames: TStringArray): PTexture;

  function RetrieveTextureByFileName(const aFilename: string): PTexture; virtual;

  // try to pack all added images in single one with size constraints.
  // aSpacing is the number of pixel to insert between each image both vertically
  // and horizontally.
  function TryToPack(aMaxTextureSize: integer): boolean; overload;
  // try to pack all added images in single one without size constraints.
  function TryToPack: boolean; overload;

  // if you no longer need to use PackedImage or GetPackedImagePowerOfTwo, you
  // should call this method to free all items bitmap from memory.
  procedure FreeItemImages;

  // gives the whole atlas image.
  // Remember to free the bitmap after you use it.
  // Use this property after a call to TryToPack or Pack
  function GetPackedImage(aWantSizePowerOfTwo: boolean=False; aWantSquare: boolean=False): TBGRABitmap;

  // The images coordinates (UV) in the atlas are computed with floating point numbers.
  // Due to their lack of precision, it can happen that a subtexture is displayed
  // with the first line of the adjacent one. To avoid this, we can insert 1 pixel
  // (or more) between each image both vertically and horizontally.
  // So, if you see some display error, try to set Spacing value to 1 or more. Default value is 0.
  property Spacing: integer read FSpacing write FSpacing;

  // When an image cannot be placed due to lack of space, the width and height of the working area is
  // increased by the value of InflateDelta.
  // Default value is 16. If you don't want any waste space set it to 1.
  property InflateDelta: integer read FInflateDelta write SetInflateDelta;

  // Use this property after a call to TryToPack or Pack to retrieve the size of the whole image.
  property ComputedAtlasSize: integer read FCurrentAtlasSize;

  // True if the atlas is loaded from a file
  property LoadedFromFile: boolean read FLoadedFromFile;
end;


{ TOGLCTextureAtlas }

TOGLCTextureAtlas = class(TOGLCImagePacker)
private
  FParentScene: TOGLCScene;
  FAtlasTexture: PTexture;
  function GetAtlasTexture: PTexture;
  function SavePackedImage(const aFilename: string): boolean;
public
  constructor Create(aParentScene: TOGLCScene);
  destructor Destroy; override;
  // Atlas is the owner of the returned TTexturedFont, don't free it!
  function AddTexturedFont(aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap=NIL): TTexturedFont; reintroduce;
  // Draw the string on an image then add it to the atlas.
  function AddString(const aString: string; aFont: TFontDescriptor; aFillTexture: TBGRABitmap): PTexture;
  // Load the image, if needed resize it according the scene PPI and add it to the atlas.
  function AddScaledPPI(const aFilename: string): PTexture;

  // After a successfull TryToPack, you can call this method to build the whole
  // texture atlas and its sub-textures.
  procedure Build(aForceSizePowerOfTwo: boolean=True; aForceSquare: boolean=True);

  // Save the atlas to a file.
  //  aGameVersion is the version of the game when the atlas is saved.
  procedure SaveToFile(const aFilename, aGameVersion: string);
  // Load the atlas from file.
  procedure LoadFromFile(const aFilename: string);
  // The function return the version of the game when the atlas was saved. Compare it
  // with the current version, and if they don't match you can reconstruct the atlas
  // because graphics may change from a version to another.
  class function GetAtlasFileVersion(const aFilename: string): string;

  function RetrieveTextureByFileName(const aFilename: string): PTexture; override;

  property Texture: PTexture read GetAtlasTexture;
end;

// define an alias to TOGLCTextureAtlas with shortest name
TAtlas = TOGLCTextureAtlas;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}
const MAX_ATLAS_SIZE = 16384;

{ TOGLCImagePackerItem }

function TOGLCImagePackerItem.PixelsConsumed: integer;
begin
  Result := Bitmap.Width*Bitmap.Height;
end;

function TOGLCImagePackerItem.IsOverlappedByRec(const aRec: TRect; aSpacing: integer): boolean;
var r: TRect;
begin
  r.Create(ComputedRect.TopLeft, ComputedRect.Width+aSpacing, ComputedRect.Height+aSpacing);
  Result := Placed and r.IntersectsWith(aRec);
end;

procedure TOGLCImagePackerItem.SaveTo(aStream: TFileStream);
begin
  aStream.WriteAnsiString(Texture^.Filename);
  aStream.WriteWord(ComputedRect.Left);
  aStream.WriteWord(ComputedRect.Top);
  aStream.WriteWord(ComputedRect.Right);
  aStream.WriteWord(ComputedRect.Bottom);
  aStream.WriteWord(FrameSize.cx);
  aStream.WriteWord(FrameSize.cy);
end;

procedure TOGLCImagePackerItem.LoadFrom(aStream: TFileStream);
begin
  Texture^.Filename := aStream.ReadAnsiString;
  ComputedRect.Left := aStream.ReadWord;
  ComputedRect.Top := aStream.ReadWord;
  ComputedRect.Right := aStream.ReadWord;
  ComputedRect.Bottom := aStream.ReadWord;
  FrameSize.cx := aStream.ReadWord;
  FrameSize.cy := aStream.ReadWord;
end;

{ TOGLCImagePacker }

function TOGLCImagePacker.RawPackedImage: TBGRABitmap;
var item: POGLCImagePackerItem;
begin
  if FAllItemPlaced then
  begin
    Result := TBGRABitmap.Create(FCurrentAtlasSize, FCurrentAtlasSize, BGRAPixelTransparent);
    item := FirstItem;
    repeat
      Result.PutImage(item^.ComputedRect.Left, item^.ComputedRect.Top, item^.Bitmap, dmSet);
      item := item^.Next;
    until item = NIL;
  end else Result := NIL;
end;

function TOGLCImagePacker.GetPackedImage(aWantSizePowerOfTwo: boolean; aWantSquare: boolean): TBGRABitmap;
var ima: TBGRABitmap;
   w, h: integer;
begin
  if FAllItemPlaced then
  begin
    Result := RawPackedImage;
    if aWantSizePowerOfTwo then
    begin
      if aWantSquare then
      begin
        w := NextPowerOfTwo(Max(Result.Width, Result.Height));
        h := w;
      end else begin
        w := NextPowerOfTwo(Result.Width);
        h := NextPowerOfTwo(Result.Height);
      end;
      ima := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
      ima.PutImage(0, 0, Result, dmSet);
      Result.Free;
      Result := ima;
    end else begin
      w := Result.Width;
      h := Result.Height;
      if aWantSquare and (w <> h) then
      begin
        if w < h then w := h;
        if h < w then h := w;
        ima := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
        ima.PutImage(0, 0, Result, dmSet);
        Result.Free;
        Result := ima;
      end;
    end;
  end else Result := NIL;
end;

procedure TOGLCImagePacker.InternalInsertSorted(aItem: POGLCImagePackerItem);
var itemInList: POGLCImagePackerItem;
begin
  itemInList := FirstItem;
  if itemInList = NIL then Append(aItem)
  else begin
    repeat
      if Max(aItem^.Bitmap.Width, aItem^.Bitmap.Height) > Max(itemInList^.Bitmap.Width, itemInList^.Bitmap.Height) then begin
        InsertBefore(aItem, itemInList);
        exit;
      end;
      itemInList := itemInList^.Next;
    until itemInList = NIL;
    Append(aItem);
  end;
end;

procedure TOGLCImagePacker.SetAllItemsNotPlaced;
var item: POGLCImagePackerItem;
begin
  item := FirstItem;
  while item <> NIL do
  begin
    item^.Placed := False;
    item := item^.Next;
  end;
end;

function TOGLCImagePacker.InitFromBitmap(aIma: TBGRABitmap; AFrameWidth, AFrameHeight: integer): PTexture;
var item: POGLCImagePackerItem;
begin
  item := CreateInstance;
  item^.Placed := False;
  item^.Bitmap := aIma;
  item^.ComputedRect.Width := aIma.Width;
  item^.ComputedRect.Height := aIma.Height;
  new(item^.Texture);
  item^.FrameSize.cx := AFrameWidth;
  item^.FrameSize.cy := AFrameHeight;
  InternalInsertSorted(item);

  FBiggerItemSize := Max(FBiggerItemSize, aIma.Width);
  FBiggerItemSize := Max(FBiggerItemSize, aIma.Height);
  FTotalPixelConsumedByItems := FTotalPixelConsumedByItems + item^.PixelsConsumed;

  Result := item^.Texture;
end;

procedure TOGLCImagePacker.FreeTexturedFonts;
var tf: TTexturedFont;
begin
  for tf in FTexturedFontToFree do tf.Free;
end;

procedure TOGLCImagePacker.FreeItemTextures;
var item: POGLCImagePackerItem;
begin
  item := FirstItem;
  if item = NIL then exit;
  repeat
    Dispose(item^.Texture);
    item := item^.Next;
  until item = NIL;
end;

procedure TOGLCImagePacker.SetInflateDelta(AValue: integer);
begin
  if FInflateDelta = AValue then Exit;
  FInflateDelta := Max(1, AValue);
end;

constructor TOGLCImagePacker.Create;
begin
  inherited Create;
  FInflateDelta := 16;
end;

destructor TOGLCImagePacker.Destroy;
begin
  FreeItemImages;
  FreeTexturedFonts;
  FreeItemTextures;
  inherited Destroy;
end;

function TOGLCImagePacker.Add(const aFilename: string): PTexture;
var ima: TBGRABitmap;
begin
  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aFilename))
  else begin
    ima := TBGRABitmap.Create(aFilename);
    Result := Add(ima, ExtractFilename(aFilename));
  end;
end;

function TOGLCImagePacker.Add(const aFilename: string; AFrameWidth, AFrameHeight: integer): PTexture;
var ima: TBGRABitmap;
begin
  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aFilename))
  else begin
    ima := TBGRABitmap.Create(aFilename);
    Result := Add(ima, AFrameWidth, AFrameHeight, ExtractFilename(aFilename));
  end;
end;

function TOGLCImagePacker.Add(aImage: TBGRABitmap; const aName: string): PTexture;
begin
  Result := InitFromBitmap(aImage, aImage.Width, aImage.Height);
  Result^.Filename := aName;
end;

function TOGLCImagePacker.Add(aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer; const aName: string): PTexture;
begin
  Result := InitFromBitmap(aImage, AFrameWidth, AFrameHeight);
  Result^.Filename := aName;
end;

function TOGLCImagePacker.AddFromSVG(const aSVGFilename: string; aWidth, aHeight: integer): PTexture;
var ima: TBGRABitmap;
begin
  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aSVGFilename))
  else begin
    ima := LoadBitmapFromSVG(aSVGFilename, aWidth, aHeight);
    Result := Add(ima, ExtractFilename(aSVGFilename));
  end;
end;

function TOGLCImagePacker.AddMultiFrameImage(const aFileNames: TStringArray; aXCount, aYCount, aSpacing: integer): PTexture;
var ima, imaWhole: TBGRABitmap;
  i, ix, xx, yy, frameWidth, frameHeight: integer;
begin
  if Length(aFileNames) = 0 then raise exception.create('you must provide a valid array of filenames');
  if aXCount*aYCount < Length(aFileNames) then raise exception.Create('bad value for aXCount and/or aYCount');

  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aFileNames[0]))
  else begin
    imaWhole := TBGRABitmap.Create(1,1);
    xx := 0;
    yy := 0;
    ix := 0;
    for i:=0 to High(aFileNames) do begin
      ima := TBGRABitmap.Create(aFileNames[i]);
      if i = 0 then begin
        frameWidth := ima.Width;
        frameHeight := ima.Height;
        imaWhole.SetSize(ima.Width*aXCount, ima.Height*aYCount);
        imaWhole.Fill(BGRAPixelTransparent);
      end;
      imaWhole.PutImage(xx, yy, ima, dmDrawWithTransparency);
      ima.Free;
      inc(ix);
      if ix = aXCount then begin
        ix := 0;
        xx := 0;
        yy := yy + frameHeight + aSpacing;
      end else xx := xx + frameWidth + aSpacing;
    end;
    Result := Add(imaWhole, frameWidth, frameHeight, ExtractFilename(aFileNames[0]));
  end;
end;

function TOGLCImagePacker.AddMultiFrameImageFromSVG(const aSVGFilename: string;
  aWidth, aHeight, aXCount, aYCount: integer): PTexture;
var ima: TBGRABitmap;
begin
  if (aXCount = 0) and (aYCount = 0) then raise exception.Create('aXCount and aYCount can not be both null');
  //if aXCount

  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aSVGFilename))
  else begin
    ima := LoadBitmapFromSVG(aSVGFilename, aWidth, aHeight);
    Result := Add(ima, ima.Width div aXcount, ima.Height div aYCount, ExtractFilename(aSVGFilename));
  end;
end;

function TOGLCImagePacker.AddMultiFrameImageFromSVG(const aSVGFilenames: TStringArray; aFrameWidth, aFrameHeight, aXCount, aYCount, aSpacing: integer): PTexture;
var ima, imaWhole: TBGRABitmap;
  i, ix, xx, yy, frameWidth, frameHeight: integer;
begin
  if Length(aSVGFilenames) = 0 then raise exception.create('you must provide a valid array of filenames');
  if aXCount*aYCount < Length(aSVGFilenames) then raise exception.Create('bad value for aXCount and/or aYCount');

  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aSVGFilenames[0]))
  else begin
    imaWhole := TBGRABitmap.Create(1,1);
    xx := 0;
    yy := 0;
    ix := 0;
    for i:=0 to High(aSVGFilenames) do begin
      ima := LoadBitmapFromSVG(aSVGFilenames[i], aFrameWidth, aFrameHeight);
      if i = 0 then begin
        frameWidth := ima.Width + aSpacing;
        frameHeight := ima.Height + aSpacing;
        imaWhole.SetSize(frameWidth*aXCount, frameHeight*aYCount);
        imaWhole.Fill(BGRAPixelTransparent);
      end;
      imaWhole.PutImage(xx, yy, ima, dmDrawWithTransparency);
      ima.Free;
      inc(ix);
      if ix = aXCount then begin
        ix := 0;
        xx := 0;
        yy := yy + frameHeight;
      end else xx := xx + frameWidth;
    end;
    Result := Add(imaWhole, frameWidth, frameHeight, ExtractFilename(aSVGFilenames[0]));
  end;
end;

function TOGLCImagePacker.AddTexturedFont(aScene: TOGLCScene; aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap): TTexturedFont;
begin
  aFont.Check;
  Result := TTexturedFont.CreateFromImagePacker(aScene, Self, FLoadedFromFile, aFont, aCharSet, aFillTexture);

  SetLength(FTexturedFontToFree, Length(FTexturedFontToFree)+1);
  FTexturedFontToFree[High(FTexturedFontToFree)] := Result;
end;

function TOGLCImagePacker.AddTileSetFromSVG(const aName: string; aTileSize,
  aRowCount, aColumnCount: integer; const aSVGFileNames: TStringArray): PTexture;
var wholeIma, ima: TBGRABitmap;
  i, ro, co: integer;
begin
  if Length(aSVGFileNames) = 0 then raise exception.Create('file array is empty...');

  if LoadedFromFile then
    Result := RetrieveTextureByFileName(aName)
  else begin
    wholeIma := TBGRABitmap.Create(aColumnCount*aTileSize, aRowCount*aTileSize, BGRAPixelTransparent);
    i := 0;
    for ro:=0 to aRowCount-1 do
      for co:=0 to aColumnCount-1 do begin
        if i < Length(aSVGFileNames) then begin
          ima := LoadBitmapFromSVG(aSVGFileNames[i], aTileSize, aTileSize);
          wholeIma.PutImage(aTileSize*co, aTileSize*ro, ima, dmDrawWithTransparency);
          ima.Free;
          inc(i);
        end;
      end;

    Result := Add(wholeIma, aTileSize, aTileSize, aName);
  end;
end;

function TOGLCImagePacker.RetrieveTextureByFileName(const aFilename: string): PTexture;
var item: POGLCImagePackerItem;
begin
  item := FirstItem;
  repeat
    if item^.Texture <> NIL then
      if item^.Texture^.Filename = aFilename then begin
        Result := item^.Texture;
        exit;
      end;
    item := item^.Next;
  until item = NIL;
  Result := NIL;
end;

function TOGLCImagePacker.TryToPack(aMaxTextureSize: integer): boolean;
var item: POGLCImagePackerItem;
  allDone: boolean;

  function CanIncreaseAtlasSize: boolean; inline;
  begin
    Result := FCurrentAtlasSize < aMaxTextureSize;
  end;

  procedure IncreaseAtlasSize; inline;
  begin
    FCurrentAtlasSize := Min(FCurrentAtlasSize+FInflateDelta, aMaxTextureSize);
  end;

  procedure PlaceItemAt(aItem: POGLCImagePackerItem; aPt: TPoint); inline;
  begin
    with aItem^ do begin
      ComputedRect.Create(aPt, Bitmap.Width, Bitmap.Height);
      Placed := True;
    end;
  end;

  function FitInAtlas(aItem: POGLCImagePackerItem; aPt: TPoint): boolean; inline;
  begin
//    Result := (aPt.x+aItem^.Bitmap.Width-1 < FCurrentAtlasSize) and
//              (aPt.y+aItem^.Bitmap.Height-1 < FCurrentAtlasSize);
    Result := (aPt.x+aItem^.Bitmap.Width-1+FSpacing < FCurrentAtlasSize) and
              (aPt.y+aItem^.Bitmap.Height-1+FSpacing < FCurrentAtlasSize);
  end;

  function ItemOverlapsAnother(aItem: POGLCImagePackerItem; aPt: TPoint;
                               out aItemOverlapped: POGLCImagePackerItem): boolean;
  var itm: POGLCImagePackerItem;
    r: TRect;
  begin
    Result := False;
    itm := FirstItem;
    r := TRect.Create(aPt, aItem^.Bitmap.Width, aItem^.Bitmap.Height);
    repeat
      if not itm^.Placed then exit;
      if itm^.IsOverlappedByRec(r, FSpacing) then begin
        aItemOverlapped := itm;
        Result := True;
        exit;
      end;
      itm := itm^.Next;
    until itm = NIL;
  end;

  function SearchEmptyPlaceOnRow(aItem: POGLCImagePackerItem; var aY: integer): boolean;
  var overlapped: POGLCImagePackerItem;
    x: integer;
  begin
    Result := False;
    x := 0;
    while ItemOverlapsAnother(aItem, Point(x, aY), overlapped) do begin
      x := overlapped^.ComputedRect.Right+FSpacing;

      //if not FitInAtlas(aItem, Point(x, aY)) then exit;
      if not FitInAtlas(aItem, Point(x, aY)) then begin
        aY := overlapped^.ComputedRect.Bottom + FSpacing;
        exit;
      end;
    end;
   // if aY > 0 then aY := aY+FSpacing;
    if FitInAtlas(aItem, Point(x, aY)) then begin
      // we found a place!
      PlaceItemAt(aItem, Point(x, aY));
      Result := True;
    end;
  end;

  function TryToPlace(aItem: POGLCImagePackerItem): boolean;
  var y: Integer;
  begin
    y := 0;
    repeat
      Result := SearchEmptyPlaceOnRow(aItem, y);
      if Result then exit;
      inc(y);
    until y >= FCurrentAtlasSize;
  end;

begin
  Result := False;
  FAllItemPlaced := False;
  if Count = 0 then begin
    Raise Exception.Create('there is no item to pack');
    exit(False);
  end;

  SetAllItemsNotPlaced;

  if aMaxTextureSize*aMaxTextureSize < FTotalPixelConsumedByItems then exit(False);
  if FBiggerItemSize > aMaxTextureSize then exit(False);

  FCurrentAtlasSize := Max(Round(Sqrt(FTotalPixelConsumedByItems)), FBiggerItemSize);
  FCurrentAtlasSize := Min(FCurrentAtlasSize, aMaxTextureSize);

  repeat
    item := FirstItem;
    repeat
      if not item^.Placed then
        if not TryToPlace(item) then break;
      item := item^.Next;
    until item = NIL;

    allDone := item = NIL;
    if not allDone then
      if not CanIncreaseAtlasSize then exit
      else IncreaseAtlasSize;
  until allDone;

  FAllItemPlaced := True;
  Result := True;
end;

function TOGLCImagePacker.TryToPack: boolean;
begin
  if FAllItemPlaced then exit(True);

  if not FAllItemPlaced then TryToPack(MAX_ATLAS_SIZE);
  Result := FAllItemPlaced;
end;

procedure TOGLCImagePacker.FreeItemImages;
var item: POGLCImagePackerItem;
begin
  item := FirstItem;
  if item = NIL then exit;
  repeat
    if item^.Bitmap <> NIL then begin
      item^.Bitmap.Free;
      item^.Bitmap := NIL;
    end;
    item := item^.Next;
  until item = NIL;
end;

{ TOGLCTextureAtlas }

function TOGLCTextureAtlas.SavePackedImage(const aFilename: string): boolean;
var ima: TBGRABitmap;
begin
  Result := True;
  ima := RawPackedImage;
  try
    ima.SaveToFile(ChangeFileExt(aFilename, '.png'));
  except
    Result := False;
  end;
  ima.Free;
end;

function TOGLCTextureAtlas.GetAtlasTexture: PTexture;
begin
  Result := FAtlasTexture;
end;

constructor TOGLCTextureAtlas.Create(aParentScene: TOGLCScene);
begin
  Inherited Create;
  FParentScene := aParentScene;

  TTexturedFont.ResetCharPrefix;
end;

destructor TOGLCTextureAtlas.Destroy;
begin
  FParentScene.TexMan.Delete(FAtlasTexture);
  inherited Destroy;
end;

function TOGLCTextureAtlas.AddTexturedFont(aFont: TFontDescriptor; const aCharSet: string; aFillTexture: TBGRABitmap): TTexturedFont;
begin
  Result := inherited AddTexturedFont(FParentScene, aFont, aCharSet, aFillTexture);
  Result.FAtlas := Self;
end;

function TOGLCTextureAtlas.AddString(const aString: string; aFont: TFontDescriptor;
        aFillTexture: TBGRABitmap): PTexture;
begin
  if LoadedFromFile then
    Result := RetrieveTextureByFileName(aString)
  else begin
    Result := Add(aFont.StringToBitmap(aString, aFillTexture), aString);
  end;
end;

function TOGLCTextureAtlas.AddScaledPPI(const aFilename: string): PTexture;
var ima: TBGRABitmap;
begin
  if LoadedFromFile then
    Result := RetrieveTextureByFileName(ExtractFilename(aFilename))
  else begin
    ima := LoadBitmapScaledPPI(FParentScene, aFileName);
    Result := InitFromBitmap(ima, ima.Width, ima.Height);
    Result^.Filename := ExtractFilename(aFilename);
  end;
end;

procedure TOGLCTextureAtlas.Build(aForceSizePowerOfTwo: boolean; aForceSquare: boolean);
var wholeImage: TBGRABitmap;
   item: POGLCImagePackerItem;
begin
  if not FAllItemPlaced then begin
    raise exception.create('pack the images before build the atlas');
    exit;
  end;

  // construct the atlas texture
  wholeImage := GetPackedImage(aForceSizePowerOfTwo, aForceSquare);
  FAtlasTexture := FParentScene.TexMan.Add(wholeImage);
  wholeImage.Free;

  // initialize all item sub-textures
  item := FirstItem;
  repeat
    with item^ do
      Texture^.TakePartOf(FAtlasTexture, ComputedRect, FrameSize.cx, FrameSize.cy);
    item := item^.Next;
  until item = NIL;
end;

procedure TOGLCTextureAtlas.SaveToFile(const aFilename, aGameVersion: string);
var stream: TFileStream;
   streamImage: TMemoryStream;
  item: POGLCImagePackerItem;
  wholeImage: TBGRABitmap;
begin
  stream := TFileStream.Create(aFilename, fmCreate);
  try
    // save game version
    stream.WriteAnsiString(aGameVersion);

    // save the whole image as png
    // we use a temporary TMemoryStream because TBGRABitmap.SaveToStreamAsPng() sets
    // the stream position to the begining, and overwrite previous data pushed into the stream.
    wholeImage := GetPackedImage;
    streamImage := TMemoryStream.Create;
    wholeImage.SaveToStreamAsPng(streamImage);
    wholeImage.Free;
    streamImage.SaveToStream(stream);
    streamImage.Free;

 {   wholeImage := GetPackedImage;
    wholeImage.SaveToStreamAsPng(stream);
    wholeImage.Free;   }

    // save sub-textures info
    stream.WriteDWord(Count);
    item := FirstItem;
    repeat
      item^.SaveTo(stream);
      item := item^.Next;
    until item = NIL;
  finally
    stream.Free;
  end;
end;

procedure TOGLCTextureAtlas.LoadFromFile(const aFilename: string);
var stream: TFileStream;
  c: integer;
  item: POGLCImagePackerItem;
  wholeImage: TBGRABitmap;
begin
  FLoadedFromFile := True;

  // load atlas data
  stream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
  try
    // read atlas version
    stream.ReadAnsiString;

    // read the whole image as png and create the whole texture
    wholeImage := TBGRABitmap.Create;
    wholeImage.LoadFromStream(stream);
    FAtlasTexture := FParentScene.TexMan.Add(wholeImage);
    wholeImage.Free;


    // read the sub-textures info
    c := stream.ReadDWord;
    while c > 0 do begin
      item := AddInstanceToList;
      item^.Placed := True;
      item^.Bitmap := NIL;
      new(item^.Texture);
      item^.LoadFrom(stream);
      dec(c);
    end;
  finally
    stream.Free;
  end;

  // initialize all sub-textures
  item := FirstItem;
  repeat
    with item^ do
      Texture^.TakePartOf(FAtlasTexture, ComputedRect, FrameSize.cx, FrameSize.cy);
    item := item^.Next;
  until item = NIL;
end;

class function TOGLCTextureAtlas.GetAtlasFileVersion(const aFilename: string
  ): string;
var stream: TFileStream;
begin
  stream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
  try
    try
      Result := stream.ReadAnsiString;
    except
      Result := '';
    end;
  finally
    stream.Free;
  end;
end;

function TOGLCTextureAtlas.RetrieveTextureByFileName(const aFilename: string): PTexture;
begin
  Result := inherited RetrieveTextureByFileName(aFilename);
 { if Result = NIL then
    FParentScene.LogError('texture "'+aFilename+'" not found in atlas'); }
end;

{$endif oglcIMPLEMENTATION}

