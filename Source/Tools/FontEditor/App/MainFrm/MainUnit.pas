unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeFont, sgeFontGlyph, sgeSprite,

  GDIPAPI, GDIPOBJ,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, Spin, Windows;

type
  TMainForm = class(TForm)
    edGlyphCaption: TEdit;
    edGlyphHeight: TSpinEdit;
    edGlyphBaseLine: TSpinEdit;
    lblGlyphCaption: TLabel;
    lblGlyphWidth: TLabel;
    lblGlyphHeight: TLabel;
    lblGlyphBaseLine: TLabel;
    lbGlyphList: TListBox;
    MainMenu: TMainMenu;
    miNew: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    miGenerate: TMenuItem;
    miSprite: TMenuItem;
    miFile: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miSeparator1: TMenuItem;
    miClose: TMenuItem;
    pnlPaint: TPanel;
    pnlGlyphEditor: TPanel;
    pnlWorkArea: TPanel;
    pnlGlyph: TPanel;
    PanelSplitter: TSplitter;
    edGlyphWidth: TSpinEdit;
    WorkAreaScrollBox: TScrollBox;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    procedure edGlyphBaseLineChange(Sender: TObject);
    procedure edGlyphCaptionChange(Sender: TObject);
    procedure edGlyphHeightChange(Sender: TObject);
    procedure edGlyphWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbGlyphListClick(Sender: TObject);
    procedure miGenerateClick(Sender: TObject);
    procedure pnlPaintPaint(Sender: TObject);
  private
    FFont: TsgeFont;
    FGlyphCanvas: Graphics.TBitmap;

    procedure SetEnableEditorHandlers(AEnable: Boolean);
    procedure ReloadGlyphListBoxItemByIndex(Index: Integer);
    procedure ReloadGlyphListBox;
    function GetGlyphEditIndex: Integer;
    procedure PaintGlyphRect(Glyph: TsgeFontGlyph; AColor: TColor; ACanvas: TCanvas);
    procedure PaintGlyphRects(AColor: TColor; ACanvas: TCanvas);
    procedure CorrectPaintPanelByBitmap(Bmp: Graphics.TBitmap);
    procedure SpriteToPaintPanel(Sprite: TsgeSprite);

    procedure RepaintPaintBox;

    procedure SetGlyphEditorByIndex(Index: Integer = -1);

    procedure GenerateFont(FontName: String; FontSize: Word; FontAttrib: TsgeFontAttributes);
    procedure FillFontGlyphInfo(AFont: TGPFont);
    procedure CopyGpBitmapToBitmap(GpBmp: TGPBitmap; Bmp: Graphics.TBitmap);
    function FontAttribToGPFontAttrib(Attrib: TsgeFontAttributes): Integer;
    function MultiByteToWideChar(Str: String): UnicodeString;
    function GetFontSprite(AFont: TGPFont; SymbolSize: Integer): TGPBitmap;
  public


  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;


procedure TMainForm.lbGlyphListClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetGlyphEditIndex;
  SetGlyphEditorByIndex(Index);

  if (Index >= 0) and (Index <= $FF) then
  begin
    PaintGlyphRect(FFont.GlyphList[Index], clRed, pnlPaint.Canvas);
    pnlPaint.Invalidate;
  end;
end;


procedure TMainForm.miGenerateClick(Sender: TObject);
begin
  GenerateFont('Lucida Console', 22, []);

  SetEnableEditorHandlers(False);
  ReloadGlyphListBox;
  SetGlyphEditorByIndex;
  SetEnableEditorHandlers(True);
end;


procedure TMainForm.pnlPaintPaint(Sender: TObject);
begin
  RepaintPaintBox;
end;


procedure TMainForm.SetEnableEditorHandlers(AEnable: Boolean);
begin
  if AEnable = false then
  begin
    edGlyphCaption.OnChange := nil;
    edGlyphWidth.OnChange := nil;
    edGlyphHeight.OnChange := nil;
    edGlyphBaseLine.OnChange := nil;
  end
  else
  begin
    edGlyphCaption.OnChange := @edGlyphCaptionChange;
    edGlyphWidth.OnChange := @edGlyphWidthChange;
    edGlyphHeight.OnChange := @edGlyphHeightChange;
    edGlyphBaseLine.OnChange := @edGlyphBaseLineChange;
  end;
end;


procedure TMainForm.ReloadGlyphListBoxItemByIndex(Index: Integer);
begin
  if (Index < 0) or (Index > $FF) then
    Exit;

  lbGlyphList.Items.Strings[Index] := Format('%3d. %s', [Index, FFont.GlyphList[Index].Caption]);
end;


procedure TMainForm.ReloadGlyphListBox;
var
  i: Integer;
begin
  lbGlyphList.Items.BeginUpdate;

  lbGlyphList.Items.Clear;

  for i := 0 to $FF do
  begin
    lbGlyphList.Items.Add('');
    ReloadGlyphListBoxItemByIndex(i);
  end;

  lbGlyphList.Items.EndUpdate;
end;


function TMainForm.GetGlyphEditIndex: Integer;
begin
  Result := lbGlyphList.ItemIndex;
end;


procedure TMainForm.PaintGlyphRect(Glyph: TsgeFontGlyph; AColor: TColor; ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Color := AColor;

    Rectangle(
      Glyph.SpriteRect.X1,
      Glyph.SpriteRect.Y1,
      Glyph.SpriteRect.X1 + Glyph.SpriteRect.X2,
      Glyph.SpriteRect.Y1 + Glyph.SpriteRect.Y2);
  end;
end;


procedure TMainForm.PaintGlyphRects(AColor: TColor; ACanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to $FF do
    PaintGlyphRect(FFont.GlyphList[i], AColor, ACanvas);
end;


procedure TMainForm.CorrectPaintPanelByBitmap(Bmp: Graphics.TBitmap);
begin
  //Поправить размеры панели
  pnlPaint.Width := Bmp.Width;
  pnlPaint.Height := Bmp.Height;
end;


procedure TMainForm.SpriteToPaintPanel(Sprite: TsgeSprite);
var
  SpriteLine, BitmapLine: Pointer;
  i, BytesPerLine: Integer;
begin
  //Поправить размеры панели
  pnlPaint.Width := Sprite.Width;
  pnlPaint.Height := Sprite.Height;

  //Поправить размеры холста
  FGlyphCanvas.Width := Sprite.Width;
  FGlyphCanvas.Height := Sprite.Height;

  //Скопировать данные на холст
  BytesPerLine := Sprite.Width * 4;
  for i := 0 to Sprite.Height - 1 do
  begin
    SpriteLine := Sprite.Data + i * BytesPerLine;
    BitmapLine := FGlyphCanvas.ScanLine[Sprite.Height - 1 - i];
    Move(SpriteLine^, BitmapLine^, BytesPerLine);
  end;
end;


procedure TMainForm.RepaintPaintBox;
var
  Index: Integer;
begin
  pnlPaint.Width := FGlyphCanvas.Width;
  pnlPaint.Height := FGlyphCanvas.Height;

  //Вывод спрайта с символами
  pnlPaint.Canvas.Draw(0, 0, FGlyphCanvas);

  //Вывод активного глифа
  Index := GetGlyphEditIndex;
  if (Index >= 0) and (Index <= $FF) then
    PaintGlyphRect(FFont.GlyphList[Index], clRed, pnlPaint.Canvas);
end;


procedure TMainForm.SetGlyphEditorByIndex(Index: Integer);
begin
  pnlGlyphEditor.Enabled := (Index >= 0) and (Index <= $FF);

  if (Index < 0) or (Index > $FF) then
  begin
    edGlyphCaption.Text := '';
    edGlyphWidth.Value := 0;
    edGlyphHeight.Value := 0;
    edGlyphBaseLine.Value := 0;
  end
  else
  begin
    edGlyphCaption.Text := FFont.GlyphList[Index].Caption;
    edGlyphWidth.Value := FFont.GlyphList[Index].Width;
    edGlyphHeight.Value := FFont.GlyphList[Index].Height;
    edGlyphBaseLine.Value := FFont.GlyphList[Index].BaseLine;
  end;
end;


function TMainForm.MultiByteToWideChar(Str: String): UnicodeString;
var
  Len: Integer;
begin
  Result := '';

  //Определить длину возвращаемого буфера
  Len := Windows.MultiByteToWideChar(CP_ACP, 0, PChar(Str), Length(Str), nil, 0);

  if Len <= 0 then
    Exit;

  //Выделить память под ответ
  SetLength(Result, Len);

  //Получить преобразованную строку
  Windows.MultiByteToWideChar(CP_ACP, 0, PChar(Str), Length(Str), PWideChar(Result), Len);
end;


procedure TMainForm.GenerateFont(FontName: String; FontSize: Word; FontAttrib: TsgeFontAttributes);
var
  FontFamily: TGPFontFamily;
  AFont: TGPFont;
  SymbolSize: Integer;
  Bmp: TGPBitmap;
begin
  //Создать шрифт с нужными параметрами
  FontFamily := TGPFontFamily.Create(WideString(FontName));
  AFont := TGPFont.Create(FontFamily, FontSize, FontAttribToGPFontAttrib(FontAttrib));

  //Размер одной клетки
  SymbolSize := Round(AFont.GetHeight(96));

  //Сгенерировать спрайт с глифами
  Bmp := GetFontSprite(AFont, SymbolSize);

  //Скопировать спрайт на рабочий битмап
  CopyGpBitmapToBitmap(Bmp, FGlyphCanvas);

  //Поправить панель для рисования
  CorrectPaintPanelByBitmap(FGlyphCanvas);

  //Заполнить шрифт информацией о глифах
  FillFontGlyphInfo(AFont);

  //Почистить память
  Bmp.Free;
  AFont.Free;
  FontFamily.Free;
end;


procedure TMainForm.FillFontGlyphInfo(AFont: TGPFont);
var
  X, Y, i, SymbolSize: Integer;
  Glyph: TsgeFontGlyph;
begin
  //Размер глифа
  SymbolSize := Round(AFont.GetHeight(96));

  //Заполнить параметры
  FFont.Height := SymbolSize;
  FFont.LineSpace := 0;
  FFont.GlyphSpace := 0;


  X := -SymbolSize;
  Y := -SymbolSize;

  //Заполнить глифы
  for i := 0 to $FF do
  begin
    Glyph := FFont.GlyphList[i];

    Glyph.Caption := MultiByteToWideChar(Chr(i));
    Glyph.Width := SymbolSize;
    Glyph.Height := SymbolSize;
    Glyph.BaseLine := 0;

    //Заполнить текстурные координаты
    X := X + SymbolSize;
    if i mod 16 = 0 then
    begin
      X := 0;
      Y := Y + SymbolSize;
    end;

    //Прямоугольник вывода глифа
    Glyph.SpriteRect := sgeGetIntRect(X, Y, SymbolSize, SymbolSize);
  end;
end;


procedure TMainForm.CopyGpBitmapToBitmap(GpBmp: TGPBitmap; Bmp: Graphics.TBitmap);
var
  Graphic: TGPGraphics;
begin
  //Поправить размеры битмапа
  Bmp.Width := GpBmp.GetWidth;
  Bmp.Height := GpBmp.GetHeight;

  //Вспомогательный класс для рисования
  Graphic := TGPGraphics.Create(Bmp.Canvas.Handle);

  //Вывод
  Graphic.DrawImage(GpBmp, 0, 0);

  //Почистить память
  Graphic.Free;
end;


function TMainForm.FontAttribToGPFontAttrib(Attrib: TsgeFontAttributes): Integer;
begin
  Result := FontStyleRegular;

  if faBold in Attrib then
    Result := Result + FontStyleBold;

  if faItalic in Attrib then
    Result := Result + FontStyleItalic;

  if faUnderline in Attrib then
    Result := Result or FontStyleUnderline;

  if faStrikeOut in Attrib then
    Result := Result or FontStyleStrikeout;
end;


function TMainForm.GetFontSprite(AFont: TGPFont; SymbolSize: Integer): TGPBitmap;
var
  FontBrush, ContourBrush, BGBrush: TGPSolidBrush;
  Graphic: TGPGraphics;
  SpriteSize, X, Y, i: Integer;
  C: UnicodeString;

  procedure DrawSymbol(Symbol: UnicodeString; X, Y: Integer; Brush: TGPBrush);
  var
    GlyphRect: TGPRectF;
  begin
    GlyphRect.X := X;
    GlyphRect.Y := Y;
    GlyphRect.Width := SpriteSize;
    GlyphRect.Height := SpriteSize;

    Graphic.DrawString(C, Length(C), AFont, GlyphRect, nil, Brush);
  end;

begin
  //Размер спрайта
  SpriteSize := SymbolSize * 16;

  //Создать битмап
  Result := TGPBitmap.Create(SpriteSize, SpriteSize);

  //Подготовить кисти для заливки
  FontBrush := TGPSolidBrush.Create(MakeColor(255, 255, 255));
  ContourBrush := TGPSolidBrush.Create(MakeColor(0, 0, 0));
  BGBrush := TGPSolidBrush.Create(MakeColor(127, 127, 127));

  //Вспомогательный класс рисования
  Graphic := TGPGraphics.Create(Result);
  Graphic.SetTextRenderingHint(TextRenderingHintSingleBitPerPixelGridFit);

  //Стереть фон
  Graphic.FillRectangle(BGBrush, MakeRect(0, 0, SpriteSize, SpriteSize));

  //Вывести глифы
  X := -SymbolSize;
  Y := -SymbolSize;

  for i := 0 to $FF do
  begin
    //Поправить координаты
    X := X + SymbolSize;
    if i mod 16 = 0 then
    begin
      X := 0;
      Y := Y + SymbolSize;
    end;

    //Преобразование кодировки
    C := MultiByteToWideChar(Chr(i));

    //Контур
    DrawSymbol(C, X - 1, Y - 1, ContourBrush);
    DrawSymbol(C, X + 1, Y - 1, ContourBrush);
    DrawSymbol(C, X - 1, Y + 1, ContourBrush);
    DrawSymbol(C, X + 1, Y + 1, ContourBrush);
    DrawSymbol(C, X - 1, Y, ContourBrush);
    DrawSymbol(C, X + 1, Y, ContourBrush);
    DrawSymbol(C, X, Y + 1, ContourBrush);
    DrawSymbol(C, X, Y - 1, ContourBrush);

    //Символ
    DrawSymbol(C, X, Y, FontBrush);
  end;

  //Почистить память
  Graphic.Free;
  FontBrush.Free;
  ContourBrush.Free;
  BGBrush.Free;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FFont := TsgeFont.Create('');
  FGlyphCanvas := Graphics.TBitmap.Create;
  FGlyphCanvas.PixelFormat := pf32bit;

  ReloadGlyphListBox;
  SetGlyphEditorByIndex;


  FFont.Sprite.Width := 200;
  FFont.Sprite.Height := 200;
  FFont.Sprite.FillChessBoard(25);

  SpriteToPaintPanel(FFont.Sprite);
end;


procedure TMainForm.edGlyphCaptionChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Caption := edGlyphCaption.Text;
  ReloadGlyphListBoxItemByIndex(Idx);
end;


procedure TMainForm.edGlyphBaseLineChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].BaseLine := edGlyphBaseLine.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
end;


procedure TMainForm.edGlyphHeightChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Height := edGlyphHeight.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
end;


procedure TMainForm.edGlyphWidthChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetGlyphEditIndex;

  FFont.GlyphList[Idx].Width := edGlyphWidth.Value;
  ReloadGlyphListBoxItemByIndex(Idx);
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGlyphCanvas);
  FFont.Free;
end;



end.

