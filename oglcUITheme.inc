{$ifdef oglcINTERFACE}
type

TUIFillTheme = record
  Visible: boolean;
  BlendMode: byte;
  Color,
  CenterColor,
  Tint: TBGRAPixel;
end;

TUIBorderTheme = record
  Visible: boolean;
  BlendMode: byte;
  LinePosition: TOGLCLinePosition;
  Color, Tint: TBGRAPixel;
  Width: single
end;

TUIPanelTheme = class

end;

TUIThemeTemplate = class
  Panel: TUIPanelTheme;
end;

{$endif oglcINTERFACE}
{$ifdef oglcIMPLEMENTATION}



{$endif oglcIMPLEMENTATION}

