unit u_datamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ILCursor: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForCursor;
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
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := LoadBitmapFromSVG(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForCursor;
begin
  ILCursor.BeginUpdate;
  ILCursor.Clear;
  ILCursor.Width := FormMain.ScaleDesignToForm(32);
  ILCursor.Height := ILCursor.Width;

  FPath := GetCursorFolder;
  AddImageToImageList('Select.svg', ILCursor);
  AddImageToImageList('Line.svg', ILCursor);
  AddImageToImageList('Circle.svg', ILCursor);
  AddImageToImageList('Rectangle.svg', ILCursor);
  AddImageToImageList('Polygon.svg', ILCursor);

  ILCursor.EndUpdate;
end;

end.

