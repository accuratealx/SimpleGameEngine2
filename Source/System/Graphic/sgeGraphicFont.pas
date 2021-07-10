{
Пакет             Simple Game Engine 2
Файл              sgeGraphicFont.pas
Версия            1.0
Создан            05.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс битового шрифта для вывода посредством OpenGL.
                  Данный механизм имеет один недостаток - это вывод
                  только в кодировке ANSI и не более 255 символов.
                  Для перевода в Utf8 нужен другой механизм.

                  ВНИМАНИЕ! Только векторные шрифты будут точной высоты,
                  при использовании растровых шрифтов, выбираеться первый
                  подходящий по размеру, но не больше чем указанная высота.
                  Если в конструктор передать шрифт System, 14, то реальная
                  высота быдет выбрана 16, как наиболее близкая.

                  ПОСЛЕСЛОВИЕ! Какая же всё таки срань с этими шрифтами.
                  Человек, придумавший такой способ, должен застрелиться!
                  В общем такой подход мне совсем не нравится, теперь понятно
                  почему в OpenGL не сделали вывод текста, а оставили нам.
                  Походу самый правильный вариант - это самому сделать
                  векторный шрифт с помощью полигонов OpenGL. Это правда
                  долго, возможно, когда нибудь наступит час и я этим займусь.
                  Ну а пока оставлю так.
}
{$Include Defines.inc}

unit sgeGraphicFont;

{$mode objfpc}{$H+}

interface

uses
  dglOpenGL,
  Windows;


type
  //Атрибуты шрифта (Жирный, Наклонный, Подчеркнутый, Перечёркнутый)
  TsgeGraphicFontAttrib = set of (gfaBold, gfaItalic, gfaUnderline, gfaStrikeOut);


  TsgeGraphicFont = class
  private
    FGLHandle: GLuint;
    FHeight: Word;
    FName: String;
    FAttrib: TsgeGraphicFontAttrib;

    FDC: HDC;
    FFont: HFONT;

    procedure BuildFont;
    procedure SetName(AName: String);
    procedure SetHeight(AHeight: Word);
    procedure SetAttrib(AAttrib: TsgeGraphicFontAttrib);
  public
    constructor Create(Name: String; Height: Word; Attrib: TsgeGraphicFontAttrib = []);
    destructor  Destroy; override;

    function GetStringWidth(Str: String): Integer;
    function GetStringHeight(Str: String): Integer;

    property GLHandle: GLuint read FGLHandle;
    property Height: Word read FHeight write SetHeight;
    property Name: String read FName write SetName;
    property Attrib: TsgeGraphicFontAttrib read FAttrib write SetAttrib;
  end;



function sgeFontAttribToString(Attrib: TsgeGraphicFontAttrib): String;
function sgeFontStringToAttrib(AttribStr: String): TsgeGraphicFontAttrib;



implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'GraphicFont';

  Err_CantCreateWindowsFont = 'CantCreateWindowsFont';
  Err_CantCreateGLFont      = 'CantCreateGLFont';
  Err_CantAllocGLMemory     = 'CantAllocGLMemory';


function sgeFontAttribToString(Attrib: TsgeGraphicFontAttrib): String;
begin
  Result := '';

  if (gfaBold in Attrib) then Result := Result + 'B';
  if (gfaItalic in Attrib) then Result := Result + 'I';
  if (gfaUnderline in Attrib) then Result := Result + 'U';
  if (gfaStrikeOut in Attrib) then Result := Result + 'S';
end;


function sgeFontStringToAttrib(AttribStr: String): TsgeGraphicFontAttrib;
begin
  Result := [];

  //Определить параметры
  AttribStr := LowerCase(AttribStr);
  if sgePos('b', AttribStr) > 0 then Include(Result, gfaBold);
  if sgePos('i', AttribStr) > 0 then Include(Result, gfaItalic);
  if sgePos('u', AttribStr) > 0 then Include(Result, gfaUnderline);
  if sgePos('s', AttribStr) > 0 then Include(Result, gfaStrikeOut);

  //Особый случай
  if sgePos('[]', AttribStr) > 0 then Result := [];
end;




procedure TsgeGraphicFont.BuildFont;
var
  Fnt: HFONT;
  LogFont: TLOGFONT;
  ErrStr: String;
begin
  //Параметры шрифта
  ErrStr := FName + ' ' + sgeIntToStr(FHeight) + ' ' + sgeFontAttribToString(FAttrib);

  //Структура логического шрифта
  LogFont.lfHeight := -FHeight;                   //Высота
  LogFont.lfWidth := 0;                           //Автоподбор ширины
  LogFont.lfEscapement := 0;                      //Угол в десятых долях между вектором спуска и осью x устройства
  LogFont.lfOrientation := 0;                     //Угол в градусах
  if (gfaBold in FAttrib) then LogFont.lfWeight := FW_BOLD else LogFont.lfWeight := FW_NORMAL; //Толщина шрифта
  if (gfaItalic in FAttrib) then LogFont.lfItalic := 1 else LogFont.lfItalic := 0;             //Наклон
  if (gfaUnderline in FAttrib) then LogFont.lfUnderline := 1 else LogFont.lfUnderline := 0;    //Подчёркивание
  if (gfaStrikeOut in FAttrib) then LogFont.lfStrikeOut := 1 else LogFont.lfStrikeOut := 0;    //Перечёркивание
  LogFont.lfCharSet := DEFAULT_CHARSET;           //Набор символов
  LogFont.lfOutPrecision := OUT_TT_PRECIS;        //Точность вывода
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS; //Точность отсечения
  LogFont.lfQuality := DRAFT_QUALITY;             //Качество вывода (ANTIALIASED_QUALITY)
  LogFont.lfPitchAndFamily := DEFAULT_PITCH;      //Настройки вида шрифта, если не найдено точное совпадение
  LogFont.lfFaceName := PAnsiChar(FName);         //Имя шрифта

  //Создать шрифт и проверить
  Fnt := CreateFontIndirect(LogFont);
  if Fnt = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateWindowsFont, ErrStr);

  //выбрать шрифт
  SelectObject(FDC, Fnt);

  //Создать дисплейные списки и проверить
  glPushAttrib(GL_LIST_BASE);
  glListBase(FGLHandle);
  if wglUseFontBitmaps(FDc, 0, 256, FGLHandle) = False then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateGLFont, ErrStr);
  glPopAttrib;

  //Почистить память
  DeleteObject(FFont);

  //Запомнить новый шрифт
  FFont := Fnt;
end;


procedure TsgeGraphicFont.SetName(AName: String);
begin
  if FName = AName then Exit;
  FName := AName;
  BuildFont;
end;


procedure TsgeGraphicFont.SetHeight(AHeight: Word);
begin
  if AHeight < 1 then AHeight := 1;
  if FHeight = AHeight then Exit;
  FHeight := AHeight;
  BuildFont;
end;


procedure TsgeGraphicFont.SetAttrib(AAttrib: TsgeGraphicFontAttrib);
begin
  if FAttrib = AAttrib then Exit;
  FAttrib := AAttrib;
  BuildFont;
end;


constructor TsgeGraphicFont.Create(Name: String; Height: Word; Attrib: TsgeGraphicFontAttrib);
begin
  if Height < 1 then Height := 1; //Поправить размер
  FHeight := Height;              //Запомнить высоту шрифта
  FName := Name;                  //Запомнить имя шрифта
  FAttrib := Attrib;              //Атрибуты шрифта

  //Выделить 256 дисплейных списков и проверить
  FGLHandle := glGenLists(256);
  if FGLHandle = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantAllocGLMemory, Name);

  //Создать совместимый контекст в памяти
  FDC := CreateCompatibleDC(0);

  //Построить шрифт
  BuildFont;
end;


destructor TsgeGraphicFont.Destroy;
begin
  if FGLHandle = 0 then Exit;
  DeleteObject(FFont);            //Удалить логический шрифт
  DeleteDC(FDC);                  //Удалить контекс
  glDeleteLists(FGLHandle, 256);  //Удалить дисплейные списки
end;


function TsgeGraphicFont.GetStringWidth(Str: String): Integer;
var
  Sz: tagSIZE;
begin
  GetTextExtentPoint32(FDC, PChar(Str), Length(Str), Sz);
  Result := Sz.Width;
end;


function TsgeGraphicFont.GetStringHeight(Str: String): Integer;
var
  Sz: tagSIZE;
begin
  GetTextExtentPoint32(FDC, PChar(Str), Length(Str), Sz);
  Result := Sz.Height;
end;



end.

