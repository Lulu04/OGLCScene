unit u_datamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ILIconCursor: TImageList;
    ILIconUI: TImageList;
    ILIconAlign: TImageList;
    ILIconLayerList: TImageList;
    ILLevelTreeView: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
    procedure RedrawImageForIconImageList1;
    procedure RedrawImageForCursor;
    procedure RedrawImageForIconAlign;
    procedure RedrawImageForIconLayerList;
    procedure RedrawImageForIconLevelTreeView;
  public
  end;

var
  DataModule1: TDataModule1;

implementation

uses u_app_pref, form_main, BGRABitmap, BGRABitmapTypes, OGLCScene;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  RedrawImageForCursor;
  RedrawImageForIconAlign;
  RedrawImageForIconLayerList;
  RedrawImageForIconLevelTreeView;
  RedrawImageForIconImageList1;
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := LoadBitmapFromSVG(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForIconImageList1;
begin
  ILIconUI.BeginUpdate;
  ILIconUI.Clear;
  ILIconUI.Width := FormMain.ScaleDesignToForm(20);
  ILIconUI.Height := ILIconUI.Width;

  FPath := GetIconFolder;
  AddImageToImageList('ProjectNew.svg', ILIconUI);
  AddImageToImageList('ProjectOpen.svg', ILIconUI);
  AddImageToImageList('ProjectSave.svg', ILIconUI);
  AddImageToImageList('ProjectOptions.svg', ILIconUI);
  AddImageToImageList('Add.svg', ILIconUI);
  AddImageToImageList('TrashCan.svg', ILIconUI);       // 5
  AddImageToImageList('TextureUpdate.svg', ILIconUI);
  AddImageToImageList('TextureAdd.svg', ILIconUI);
  AddImageToImageList('HelpButton.svg', ILIconUI);
  AddImageToImageList('Undo.svg', ILIconUI);
  AddImageToImageList('Redo.svg', ILIconUI);          // 10
  AddImageToImageList('Duplicate.svg', ILIconUI);
  AddImageToImageList('Rename.svg', ILIconUI);
  AddImageToImageList('Cancel.svg', ILIconUI);
  AddImageToImageList('Checked.svg', ILIconUI);
  AddImageToImageList('ImageOpen.svg', ILIconUI);    // 15

  ILIconUI.EndUpdate;
end;

procedure TDataModule1.RedrawImageForCursor;
begin
  ILIconCursor.BeginUpdate;
  ILIconCursor.Clear;
  ILIconCursor.Width := FormMain.ScaleDesignToForm(24);
  ILIconCursor.Height := ILIconCursor.Width;

  FPath := GetIconFolder;
  AddImageToImageList('Select.svg', ILIconCursor);
  AddImageToImageList('Line.svg', ILIconCursor);
  AddImageToImageList('Circle.svg', ILIconCursor);
  AddImageToImageList('Rectangle.svg', ILIconCursor);
  AddImageToImageList('Polygon.svg', ILIconCursor);
  AddImageToImageList('AddNode.svg', ILIconCursor);
  AddImageToImageList('Point.svg', ILIconCursor);

  ILIconCursor.EndUpdate;
end;

procedure TDataModule1.RedrawImageForIconAlign;
begin
  ILIconAlign.BeginUpdate;
  ILIconAlign.Clear;
  ILIconAlign.Width := FormMain.ScaleDesignToForm(24);
  ILIconAlign.Height := ILIconAlign.Width;

  FPath := GetIconFolder;
  // 0
  AddImageToImageList('AlignHRightToLeft.svg', ILIconAlign);
  AddImageToImageList('AlignHRightToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHLeft.svg', ILIconAlign);
  AddImageToImageList('AlignHCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHRight.svg', ILIconAlign);
  AddImageToImageList('AlignHLeftToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignHLeftToRight.svg', ILIconAlign);

  // 7
  AddImageToImageList('AlignVBottomToTop.svg', ILIconAlign);
  AddImageToImageList('AlignVBottomToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVTop.svg', ILIconAlign);
  AddImageToImageList('AlignVCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVBottom.svg', ILIconAlign);
  AddImageToImageList('AlignVTopToCenter.svg', ILIconAlign);
  AddImageToImageList('AlignVTopToBottom.svg', ILIconAlign);

  // 14
  AddImageToImageList('Rotate90CCW.svg', ILIconAlign);
  AddImageToImageList('Rotate90CW.svg', ILIconAlign);
  AddImageToImageList('MirrorH.svg', ILIconAlign);
  AddImageToImageList('MirrorV.svg', ILIconAlign);
  AddImageToImageList('ShiftTop.svg', ILIconAlign);
  AddImageToImageList('ShiftTopOneStep.svg', ILIconAlign);
  AddImageToImageList('ShiftBackOneStep.svg', ILIconAlign);
  AddImageToImageList('ShiftBack.svg', ILIconAlign);

  // 22
  AddImageToImageList('ZoomWorld.svg', ILIconAlign);
  AddImageToImageList('ZoomScene.svg', ILIconAlign);
  AddImageToImageList('ZoomOnSelection.svg', ILIconAlign);

  // 25
  AddImageToImageList('DistributeRegularlyH.svg', ILIconAlign);
  AddImageToImageList('DistributeRegularlyV.svg', ILIconAlign);

  ILIconAlign.EndUpdate;
end;

procedure TDataModule1.RedrawImageForIconLayerList;
begin
  ILIconLayerList.BeginUpdate;
  ILIconLayerList.Clear;
  ILIconLayerList.Width := FormMain.ScaleDesignToForm(18);
  ILIconLayerList.Height := ILIconLayerList.Width;

  FPath := GetIconFolder;
  AddImageToImageList('LayerVisible.svg', ILIconLayerList);
  AddImageToImageList('LayerNotVisible.svg', ILIconLayerList);

  ILIconLayerList.EndUpdate;
end;

procedure TDataModule1.RedrawImageForIconLevelTreeView;
begin
  ILLevelTreeView.BeginUpdate;
  ILLevelTreeView.Clear;
  ILLevelTreeView.Width := FormMain.ScaleDesignToForm(16);
  ILLevelTreeView.Height := ILLevelTreeView.Width;

  FPath := GetIconFolder;
  AddImageToImageList('Folder.svg', ILLevelTreeView);
  AddImageToImageList('Landscape.svg', ILLevelTreeView);
  AddImageToImageList('Add.svg', ILLevelTreeView);
  AddImageToImageList('TrashCan.svg', ILLevelTreeView);
  AddImageToImageList('Rename.svg', ILLevelTreeView);
  AddImageToImageList('Duplicate.svg', ILLevelTreeView);  // 5
  AddImageToImageList('Description.svg', ILLevelTreeView);
  AddImageToImageList('SpriteBuilder.svg', ILLevelTreeView);

  ILLevelTreeView.EndUpdate;
end;

end.

