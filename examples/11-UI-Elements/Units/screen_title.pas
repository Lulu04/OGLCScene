unit screen_title;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmap, BGRABitmapTypes,
  OGLCScene,
  u_common;

type

{ TScreenDemo }

TScreenDemo = class(TScreenTemplate)
private
  FAtlas: TOGLCTextureAtlas; // we need an atlas to contains all images in a single texture

  FtexFont: TTexturedFont;
  FtexWarning, FtexChecked, FtexUnchecked: PTexture;

  FLabel1, FLabel2, FLabel3, FLabel4, FLabel5, FLabel6, FLabel7, FLabel8, FLabel9, FLabel10: TUILabel;
  FButton1: TUIButton;
  FPanel1, FPanel2, FPanel3, FPanel4, FPanel5: TUIPanel;
  FCheckBox1, FCheckBox2: TUICheck;
  FRadio1, FRadio2, FRadio3: TUIRadio;
  FProgressBar1, FProgressBar2: TUIProgressBar;
  FScrollBar1: TUIScrollBar;
  FListBox1: TUIListBox;
  FTextArea1, FTextArea2: TUITextArea;
  FScrollBox1: TUIScrollBox;

  procedure InitCallbackOn(aSurface: TUIClickableObject);
  procedure ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseEnter(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseLeave(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessUIMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure ProcessUIMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  procedure ProcessUICheckboxChange(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessUIRadioChange(Sender: TSimpleSurfaceWithEffect);
  procedure ProcessListBoxSelectionChange(Sender: TSimpleSurfaceWithEffect);
public
  procedure CreateObjects; override;
  procedure FreeObjects; override;
  procedure Update(const aElapsedTime: single); override;

  procedure SetColorTheme;
end;

var ScreenDemo: TScreenDemo = NIL;

implementation
uses Forms, Graphics, form_main;

{ TScreenDemo }

procedure TScreenDemo.InitCallbackOn(aSurface: TUIClickableObject);
begin
  aSurface.OnClick := @ProcessUIClick;
  aSurface.OnMouseEnter := @ProcessUIMouseEnter;
  aSurface.OnMouseLeave := @ProcessUIMouseLeave;
  aSurface.OnMouseDown := @ProcessUIMouseDown;
  aSurface.OnMouseUp := @ProcessUIMouseUp;
  aSurface.OnMouseWheel := @processUIMouseWheel;
end;

procedure TScreenDemo.ProcessUIClick(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - clicked');
  if Sender = FButton1 then begin
    FormMain.Memo1.Lines.Add('Button1 - clicked');
    FTextArea1.Text.Caption := 'Hello from here!';
  end;
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - clicked');
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - clicked');
  if Sender = FScrollBar1 then FLabel3.Caption := Format('TUIScrollBar     %d/%d  page size %d', [FScrollBar1.Position, FScrollBar1.Max, FScrollBar1.PageSize]);
end;

procedure TScreenDemo.ProcessUIMouseEnter(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - mouse enter');
  if Sender = FButton1 then FormMain.Memo1.Lines.Add('Button1 - mouse enter');
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - mouse enter');
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - mouse enter');
end;

procedure TScreenDemo.ProcessUIMouseLeave(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - mouse leave');
  if Sender = FButton1 then FormMain.Memo1.Lines.Add('Button1 - mouse leave');
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - mouse leave');
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - mouse leave');
end;

procedure TScreenDemo.ProcessUIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
begin
  s := ' @('+X.ToString+','+Y.ToString+')';
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - mouse down'+s);
  if Sender = FButton1 then FormMain.Memo1.Lines.Add('Button1 - mouse down'+s);
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - mouse down'+s);
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - mouse down'+s);
  if Sender = FScrollBox1 then begin
    s := ' @('+(X+FScrollBox1.ViewOffset.x).ToString+','+(Y+FScrollBox1.ViewOffset.y).ToString+')';
    FormMain.Memo1.Lines.Add('ScrollBox1 - mouse down'+s);
  end;
end;

procedure TScreenDemo.ProcessUIMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
begin
  s := ' @('+X.ToString+','+Y.ToString+')';
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - mouse up'+s);
  if Sender = FButton1 then FormMain.Memo1.Lines.Add('Button1 - mouse up'+s);
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - mouse up'+s);
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - mouse up'+s);
  if Sender = FScrollBox1 then begin
    s := ' @('+(X+FScrollBox1.ViewOffset.x).ToString+','+(Y+FScrollBox1.ViewOffset.y).ToString+')';
    FormMain.Memo1.Lines.Add('ScrollBox1 - mouse up'+s);
  end;
end;

procedure TScreenDemo.ProcessUIMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Sender = FLabel1 then FormMain.Memo1.Lines.Add('Label1 - mouse wheel');
  if Sender = FButton1 then FormMain.Memo1.Lines.Add('Button1 - mouse wheel');
  if Sender = FPanel1 then FormMain.Memo1.Lines.Add('Panel1 - mouse wheel');
  if Sender = FCheckBox1 then FormMain.Memo1.Lines.Add('CheckBox1 - mouse wheel');
end;

procedure TScreenDemo.ProcessUICheckboxChange(Sender: TSimpleSurfaceWithEffect);
begin
  if Sender = FCheckBox1 then FPanel1.ChildClippingEnabled := FCheckBox1.Checked;
  if Sender = FCheckBox2 then begin
    FPanel2.ChildClippingEnabled := FCheckBox2.Checked;
    FPanel3.ChildClippingEnabled := FCheckBox2.Checked;
    FPanel4.ChildClippingEnabled := FCheckBox2.Checked;
  end;
end;

procedure TScreenDemo.ProcessUIRadioChange(Sender: TSimpleSurfaceWithEffect);
begin
  if FRadio1.Checked then FormMain.Memo1.Lines.Add('Radio1 checked');
  if FRadio2.Checked then FormMain.Memo1.Lines.Add('Radio2 checked');
  if FRadio3.Checked then FormMain.Memo1.Lines.Add('Radio3 checked');
end;

procedure TScreenDemo.ProcessListBoxSelectionChange(Sender: TSimpleSurfaceWithEffect);
var i: Integer;
begin
  if FListBox1.SelectedCount = 0 then FormMain.Memo1.Lines.Add('ListBox1: no selection')
  else begin
    FormMain.Memo1.Lines.Add('ListBox1 selection:');
    for i:=0 to FListBox1.Count-1 do
      if FListBox1.Selected[i] then FormMain.Memo1.Lines.Add('   '+FListBox1.Items[i]);
  end;
end;

procedure TScreenDemo.SetColorTheme;
const COLOR_POSITION: array[0..2] of single=(0, 0.5, 1);
var cLeft, cMiddle, cRight: TBGRAPixel;
  gradHeavy, gradSoft, gradButton: TGradientDescriptor;
  o: TSimpleSurfaceWithEffect;
  i: Integer;

begin
  gradHeavy.InitDefault;
  gradSoft.InitDefault;
  gradButton.InitDefault;
  cLeft := BGRA(30,30,30);
  cRight := cLeft;
  cMiddle := BGRA(220,220,220);

  if FormMain.RadioButton1.Checked then begin
    cLeft := BGRA(100,50,255,100);
    cMiddle := BGRA(255,0,255,200);
    cRight := BGRA(50,0,255,100);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(0,0,255,10), BGRA(255,0,255,30), BGRA(0,0,255,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end else
  if FormMain.RadioButton2.Checked then begin
    cLeft := BGRA(100,255,50,30);
    cMiddle := BGRA(100,255,0,180);
    cRight := BGRA(50,255,0,30);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(100,255,50,10), BGRA(100,255,0,30), BGRA(50,255,0,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end else
  if FormMain.RadioButton3.Checked then begin
    cLeft := BGRA(255,64,0,60);
    cMiddle := BGRA(255,128,32,220);
    cRight := BGRA(255,64,0,60);
    gradHeavy.CreateHorizontal([cLeft, cMiddle, cRight], COLOR_POSITION);
    gradSoft.CreateHorizontal([BGRA(255,64,32,10), BGRA(255,64,32,30), BGRA(255,128,32,10)], COLOR_POSITION);
    gradButton.CreateVertical([cLeft, cMiddle, cRight], COLOR_POSITION);
  end;

  for i:=0 to FScene.Layer[0].SurfaceCount-1 do begin
    o := FScene.Layer[0].Surface[i];
    if (o is TUIPanel) or (o is TUITextArea) or (o is TUIScrollBar) or (o is TUIScrollBox) then
      TUIClickableWithBodyShape(o).BackGradient.CopyFrom(gradHeavy)
    else
    if (o is TUIListBox) then begin
      TUIListBox(o).ItemColor.GradientItem.CopyFrom(gradSoft);
      TUIListBox(o).ItemColor.GradientItemSelected.CopyFrom(gradHeavy);
    end else
    if (o is TUIProgressBar) then
      TUIProgressBar(o).BodyShape.Fill.Color := cLeft
    else
    if (o is TUICheck) then
      TUICheck(o).ColorChecked := cMiddle
    else
    if (o is TUIRadio) then
      TUIRadio(o).ColorChecked := cMiddle
    else
    if (o is TUIButton) then
      TUIButton(o).BackGradient.CopyFrom(gradButton);
  end;

{  FPanel1.BackGradient.CopyFrom(gradHeavy);
  FPanel2.BackGradient.CopyFrom(gradHeavy);
  FTextArea1.BackGradient.CopyFrom(gradHeavy);

  FScrollBar1.BackGradient.CopyFrom(gradSoft);
  FScrollBar1.SliderShape.Fill.Color := cLeft;
  //FProgressBar1.BackGradient.CopyFrom(gradSoft);
  //FProgressBar2.BackGradient.CopyFrom(gradSoft);
  FProgressBar1.BodyShape.Fill.Color := cLeft;
  FProgressBar2.BodyShape.Fill.Color := cLeft;

  FListBox1.ItemColor.GradientItem.CopyFrom(gradSoft);
  FListBox1.ItemColor.GradientItemSelected.CopyFrom(gradHeavy);  }

end;

procedure TScreenDemo.CreateObjects;
var path: string;
  ima: TBGRABitmap;
  fd: TFontDescriptor;
begin
  path := Application.Location+'..'+DirectorySeparator+'Data'+DirectorySeparator;

  // we create an atlas at run time to ensure all images are in the same OpenGL texture -> speed optimization
  FAtlas := FScene.CreateAtlas;
  // to avoid texture artifacts, we want all image in the packed texture separated by 1 pixel
  FAtlas.Spacing := 1;

  // we define a font for the text
  fd.Create('Arial', FScene.ScaleDesignToScene(18), [], BGRA(0,0,0));
  FtexFont := FAtlas.AddTexturedFont(fd, SIMPLELATIN_CHARSET + ASCII_SYMBOL_CHARSET); // use 2 predefined charsets

  FtexWarning := FAtlas.AddFromSVG(path+'DlgWarning.svg', FScene.ScaleDesignToScene(32), -1);
  FtexUnchecked := FAtlas.AddFromSVG(path+'CheckBoxUnchecked.svg', FScene.ScaleDesignToScene(32), -1);
  FtexChecked := FAtlas.AddFromSVG(path+'CheckBoxChecked.svg', FScene.ScaleDesignToScene(32), -1);

  FAtlas.TryToPack;
  FAtlas.Build;    // here the atlas is built and all individuals textures are initialized as part of the
                   // whole atlas texture.

  // for the example, we save the packed image just to see how the atlas work.
  ima := FAtlas.GetPackedImage(True, True);
  ima.SaveToFile(path+'atlas.png');
  ima.Free;

  // label on the scene
  FLabel1 := FScene.Add_UILabel('Hello! My name is Label1 and I''m a TUILabel ! I''m only clipped by the bounds of the scene.', FtexFont);
  FLabel1.CenterX := FScene.Width*0.5;
  FLabel1.Y.Value := FScene.Height*0.1;
  FLabel1.SetCoordinate(10, 64);
  FLabel1.Tint.Value := BGRA(220,220,220);
  InitCallbackOn(FLabel1);

  // button on the scene
  FButton1 := FScene.Add_UIButton('Button1', FtexFont, FtexWarning);
  FButton1.BodyShape.SetShapeRoundRect(10,5,8,8,2);
  FButton1._Label.Tint.Value := BGRA(220,220,220);
  FButton1.CenterX := FLabel1.CenterX;
  FButton1.Y.Value := FLabel1.BottomY;
  InitCallbackOn(FButton1);

  // Panel1
  FPanel1 := FScene.Add_UIPanel;
  FPanel1.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(135), 10, 10, 2, []);
  FPanel1.AnchorPosToSurface(FButton1, haCenter, haCenter, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  InitCallbackOn(FPanel1);
    // label2, child of FPanel1
    FLabel2 := TUILabel.Create(FScene, 'A TUILabel child of TUIPanel -> I''m clipped by my parent!', FtexFont);
    FPanel1.AddChild(FLabel2);
    FLabel2.CenterOnParent;
    FLabel2.Tint.Value := BGRA(255,128,220);
    FLabel2.Angle.AddConstant(10);
    // checkbox1, child of FPanel1
    FCheckBox1 := TUICheck.Create(FScene, 'Enabled Child clipping', FtexFont);
    FPanel1.AddChild(FCheckBox1);
    FCheckBox1.AnchorPosToParent(haCenter, haCenter, 0, vaTop, vaTop, FScene.ScaleDesignToScene(5));
    FCheckBox1._Label.Tint.Value := BGRA(230,230,200);
    FCheckBox1.CustomizeCheckBox(ctRectangle, cfCross);
    FCheckBox1.ColorChecked := BGRA(255,255,0);
    FCheckBox1.Checked := True;
    FCheckBox1.OnChange := @ProcessUICheckboxChange;
    InitCallbackOn(FCheckBox1);
  // label above panel1
  FLabel6 := FScene.Add_UILabel('TUIPanel', FtexFont);
  FLabel6.AnchorPosToSurface(FPanel1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel6.Tint.Value := BGRA(255,255,0);


  // Panel2
  FPanel2 := FScene.Add_UIPanel;
  FPanel2.AnchorPosToSurface(FPanel1, haCenter, haCenter, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FPanel2.SetCoordinate(170,350);
  FPanel2.BodyShape.SetShapeRoundRect(400, 275, 10, 10, 2, []);
  FPanel2.BodyShape.Border.Color := BGRA(255,255,255);
  FPanel2.BodyShape.Border.LinePosition := lpInside; // bpInside   bpMiddle
  FPanel2.BodyShape.Fill.Color := BGRA(81,79,66);
    // checkbox2, child of FPanel2
    FCheckBox2 := TUICheck.Create(FScene, 'Enabled Child clipping', FtexFont);
    FPanel2.AddChild(FCheckBox2);
    FCheckBox2.AnchorPosToParent(haRight, haRight, 0, vaTop, vaTop, FScene.ScaleDesignToScene(5));
    FCheckBox2._Label.Tint.Value := BGRA(230,230,200);
    FCheckBox2.CustomizeCheckBox(ctCircle, cfColor);
    FCheckBox2.ColorChecked := BGRA(255,255,0);
    FCheckBox2.Checked := True;
    FCheckBox2.OnChange := @ProcessUICheckboxChange;
    // panel3 child of panel2
    FPanel3 := TUIPanel.Create(FScene);
    FPanel3.BodyShape.SetShapeRoundRect(200, 150, 5, 5, 5);
    FPanel3.BodyShape.Fill.Color := BGRA(255,0,0);
    FPanel3.BodyShape.Border.Color := BGRA(0,255,0);
    FPanel3.BodyShape.Border.LinePosition := lpInside;
    FPanel2.AddChild(FPanel3);
    FPanel3.SetCenterCoordinate(20, FPanel2.Height*0.5);
    //Panel2.MoveXRelative(150, 5.0, idcSinusoid);
    //Panel2.Angle.AddConstant(20);
      // panel4, child of panel3
      FPanel4 := TUIPanel.Create(FScene);
      FPanel4.BodyShape.SetShapeEllipse(240, 35, 3);
      FPanel4.BodyShape.Fill.Color := BGRA(0,0,255, 80);
      FPanel4.BodyShape.Border.Color := BGRA(255,128,64);
      FPanel3.AddChild(FPanel4);
      FPanel4.SetCenterCoordinate(FPanel3.Width*0.5, FPanel3.Height*0.5);
      FPanel4.Angle.AddConstant(-30);
        // panel5, child of panel4
        FPanel5 := TUIPanel.Create(FScene);
        FPanel5.BodyShape.SetShapeRectangle(280, 10, 3);
        FPanel5.BodyShape.Fill.Color := BGRA(255,0,255);
        FPanel5.BodyShape.Border.Color := BGRA(0,255,255);
        FPanel4.AddChild(FPanel5);
        FPanel5.SetCenterCoordinate(FPanel4.Width*0.5, FPanel4.Height*0.5);
        FPanel5.Angle.AddConstant(30);

  // 3x radio button on scene
  FRadio1 := FScene.Add_UIRadio('TUIRadio 1', FtexFont);
  FRadio1.AnchorPosToSurface(FPanel2, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FRadio1._Label.Tint.Value := BGRA(220,220,220);
  FRadio1.Checked := True;
  FRadio1.OnChange := @ProcessUIRadioChange;

  FRadio2 := FScene.Add_UIRadio('TUIRadio 2', FtexFont);
  FRadio2.AnchorPosToSurface(FRadio1, haLeft, haRight, FScene.ScaleDesignToScene(20), vaCenter, vaCenter, 0);
  FRadio2.CustomizeCheckBox(ctRectangle, cfCross);
  FRadio2._Label.Tint.Value := BGRA(220,220,220);
  FRadio2.OnChange := @ProcessUIRadioChange;
  // radio3 is customized with textures
  FRadio3 := FScene.Add_UIRadio('TUIRadio 3', FtexFont);
  FRadio3.AnchorPosToSurface(FRadio2, haLeft, haRight, FScene.ScaleDesignToScene(20), vaCenter, vaCenter, 0);
  FRadio3.CustomizeCheckBox(FtexUnchecked, FtexChecked, False);
  FRadio3._Label.Tint.Value := BGRA(220,220,220);
  FRadio3.OnChange := @ProcessUIRadioChange;

  // 2x progress bar
  FProgressBar1 := FScene.Add_UIProgressBar(uioHorizontal);
  FProgressBar1.AnchorPosToSurface(FRadio1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FProgressBar1.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(50),10,10,2);
  FProgressBar1.BodyShape.Border.Color := BGRA(200,200,200);
  FProgressBar1.BodyShape.Fill.Color := BGRA(20,20,20);
  FProgressBar1.Gradient.CreateHorizontal([BGRA(0,255,0), BGRA(255,255,0), BGRA(255,0,0)], [0,0.7,1]);
  // label above progressbar1
  FLabel6 := FScene.Add_UILabel('TUIProgressBar', FtexFont);
  FLabel6.AnchorPosToSurface(FProgressBar1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel6.Tint.Value := BGRA(255,255,0);

  FProgressBar2 := FScene.Add_UIProgressBar(uioHorizontal);
  FProgressBar2.AnchorPosToSurface(FProgressBar1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(5));
  FProgressBar2.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(50),10,10,2);
  FProgressBar2.BodyShape.Border.Color := BGRA(200,200,200);
  FProgressBar2.BodyShape.Fill.Color := BGRA(20,20,20);
  FProgressBar2.Gradient.Clear;
  FProgressBar2.Gradient.BeginUpdate;
  FProgressBar2.Gradient.AddRow(0.0, [BGRA(128,0,0), BGRA(128,0,0), BGRA(128,0,0)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.4, [BGRA(255,0,0), BGRA(255,0,0), BGRA(255,0,0)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.5, [BGRA(255,255,255), BGRA(255,255,255), BGRA(255,255,255)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(0.6, [BGRA(0,255,255), BGRA(0,255,255), BGRA(0,255,255)], [0,0.5,1]);
  FProgressBar2.Gradient.AddRow(1.0, [BGRA(0,128,128), BGRA(0,128,128), BGRA(0,128,128)], [0,0.5,1]);
  FProgressBar2.Gradient.EndUpdate;

  // scroll bar
  FScrollBar1 := FScene.Add_UIScrollBar(uioHorizontal);
  FScrollBar1.AnchorPosToSurface(FProgressBar2, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FScrollBar1.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(15), 30, 30, 1.6);
  FScrollBar1.SetParams(250, 0, 1500, 250);
  FScrollBar1.OnChange := @ProcessUIClick;
  // label above the scroll bar
  FLabel3 := FScene.Add_UILabel('TUIScrollBar', FtexFont);
  FLabel3.AnchorPosToSurface(FScrollBar1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel3.Tint.Value := BGRA(255,255,0);

  // list box
  FListBox1 := FScene.Add_UIListBox(FtexFont);
  FListBox1.AnchorPosToSurface(FPanel1, haLeft, haRight, FScene.ScaleDesignToScene(20), vaTop, vaTop, 0);
  FListBox1.BodyShape.SetShapeRoundRect(200, 250, 10, 10, 2);
  FListBox1.ItemColor.ColorText := BGRA(220,220,220);
  FListBox1.ItemColor.ColorSelectedText := BGRA(255,255,255);
  FListBox1.ItemHeight := 20;

  FListBox1.ItemColor.GradientItem.CreateHorizontal([BGRA(0,0,255,10), BGRA(255,0,255,20), BGRA(0,0,255,10)],[0,0.5,1]);
  FListBox1.ItemColor.GradientItemSelected.CreateHorizontal([BGRA(100,50,255,100), BGRA(255,0,255,200), BGRA(50,0,255,100)], [0,0.5,1]);

  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.Append(['00 Apple','01 Pear','02 Plum','03 Banana','04 Mango','05 Lychee','06 Cherry']);
  FListBox1.FirstSelectedIndex := 3;
  FListBox1.MultiSelect := True;
  FListBox1.OnSelectionChange := @ProcessListBoxSelectionChange;
  // label above listbox1
  FLabel4 := FScene.Add_UILabel('TUIListBox: shift/ctrl for multiselect', FtexFont);
  FLabel4.AnchorPosToSurface(FListBox1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel4.Tint.Value := BGRA(255,255,0);

  // text area
  FTextArea1 := FScene.Add_UITextArea;
  FTextArea1.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(150), 8, 8, 2);
  FTextArea1.AnchorPosToSurface(FListBox1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  FTextArea1.Text.Caption := 'We believe that we can change the things around us in accordance with our desires-we believe it because otherwise we can see no favourable outcome. We do not think of the outcome which generally comes to pass and is also favourable: we do not succeed in changing things in accordance with our desires, but gradually our desires change. The situation that we hoped to change because it was intolerable becomes unimportant to us. We have failed to surmount the obstacle, as we were absolutely determined to do, but life has taken us round it, led us beyond it, and then if we turn round to gaze into the distance of the past, we can barely see it, so imperceptible has it become.'#10'Marcel Proust, In Search of Lost Time';
  FTextArea1.Text.TexturedFont := FtexFont;
  FTextArea1.Text.Align := taTopCenter;
FTextArea1.MakeLastLineVisible;
  // label above text area
  FLabel5 := FScene.Add_UILabel('TUITextArea', FtexFont);
  FLabel5.AnchorPosToSurface(FTextArea1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel5.Tint.Value := BGRA(255,255,0);

  FScrollBox1 := FScene.Add_UIScrollBox(True, True);
  FScrollBox1.BodyShape.SetShapeRoundRect(FScene.ScaleDesignToScene(400), FScene.ScaleDesignToScene(150), 8, 8, 2);
  FScrollBox1.AnchorPosToSurface(FTextArea1, haLeft, haLeft, 0, vaTop, vaBottom, FScene.ScaleDesignToScene(20));
  InitCallbackOn(FScrollBox1);
  // label above scrollbox1
  FLabel9 := FScene.Add_UILabel('TUIScrollBox', FtexFont);
  FLabel9.AnchorPosToSurface(FScrollBox1, haLeft, haLeft, 0, vaBottom, vaTop, 0);
  FLabel9.Tint.Value := BGRA(255,255,0);
    FLabel7 := TUILabel.Create(FScene, 'Use mouse wheel on the ScrollBars!', FtexFont);
    FLabel7.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel7);
    FLabel10 := TUILabel.Create(FScene, 'Click on ScrollBox to see the coordinates', FtexFont);
    FLabel10.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel10);
    FLabel10.SetCoordinate(0, FLabel7.BottomY);

    FLabel8 := TUILabel.Create(FScene, 'You found me!', FtexFont);
    FLabel8.Tint.Value := BGRA(220,220,220);
    FScrollBox1.AddChild(FLabel8);
    FLabel8.SetCoordinate(FScrollBox1.Width*2, FScrollBox1.Height*2);

end;

procedure TScreenDemo.FreeObjects;
begin
  FScene.ClearAllLayer;    // kill all surfaces on all layer
  FreeAndNil(FAtlas);
end;

procedure TScreenDemo.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);

  // update periodically the progress bars
  FProgressBar1.Percent := FScene.Mouse.Position.X / FScene.Width;
  FProgressBar2.Percent := FScene.Mouse.Position.Y / FScene.Height;
end;


end.

