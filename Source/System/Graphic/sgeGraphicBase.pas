{
Пакет             Simple Game Engine 2
Файл              sgeGraphicBase.pas
Версия            1.1
Создан            02.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс графики. Создание контекста.
}
{$Include Defines.inc}

unit sgeGraphicBase;

{$mode objfpc}{$H+}

interface

uses
  Windows;

type
  TsgeGraphicBase = class
  protected
    FDC: HDC;                                                       //Хэндл окна
    FGLContext: HGLRC;                                              //Контекст OpenGL
    FWidth: Integer;                                                //Ширина окна
    FHeight: Integer;                                               //Высота окна

    procedure SetView(AWidth, AHeight: Integer);
  public
    constructor Create(DC: HDC; Width, Height: Integer); virtual;
    destructor  Destroy; override;

    procedure Activate;
    procedure Deactivate;
    procedure ChangeViewArea(AWidth, AHeight: Integer);

    property Context: HGLRC read FGLContext;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;


implementation

uses
  sgeErrors,
  dglOpenGL;

const
  _UNITNAME = 'GraphicBase';

  Err_CantLoadOpenGLLib         = 'CantLoadOpenGLLib';
  Err_CantSelectPixelFormal     = 'CantSelectPixelFormal';
  Err_CantSetPixelFormat        = 'CantSetPixelFormat';
  Err_CantCreateContext         = 'CantCreateContext';
  Err_CantActivateContext       = 'CantActivateContext';



procedure TsgeGraphicBase.SetView(AWidth, AHeight: Integer);
begin
  glViewport(0, 0, AWidth, AHeight);      //Задать область вывода
  glMatrixMode(GL_PROJECTION);            //Выбрать матрицу проекций
  glLoadIdentity;                         //Изменить проекцию на эталонную
  glOrtho(0, AWidth, AHeight, 0, -1, 1);  //Изменить проекцию на ортографическую
  glMatrixMode(GL_MODELVIEW);             //Выбрать матрицу модели
  glLoadIdentity;                         //Изменить проекцию на эталонную
end;


constructor TsgeGraphicBase.Create(DC: HDC; Width, Height: Integer);
var
  PFD: TPIXELFORMATDESCRIPTOR;
  PixelFormat: Integer;
begin
  //Запомнить размеры
  FWidth := Width;
  FHeight := Height;

  //Загрузить библиотеку
  if GL_LibHandle = nil then InitOpenGL;

  //Проверить загрузилась ли Opengl32.dll
  if not Assigned(GL_LibHandle) then
    raise EsgeException.Create(_UNITNAME, Err_CantLoadOpenGLLib);

  //Прочитать адреса функций
  ReadOpenGLCore;

  //Запомнить DC WIndows
  FDC := DC;

  //Заполнить Pixel format
  ZeroMemory(@PFD, SizeOf(PFD));
  with PFD do
    begin
    nSize := SizeOf(PFD);
    dwFlags := PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_DRAW_TO_WINDOW or PFD_TYPE_RGBA; //Поддержка OpenGL, двойной буфер, рисуем на окне, альфаканал
    iPixelType := PFD_TYPE_RGBA;  //Формат цвета 32 бита на точку
    iLayerType := PFD_MAIN_PLANE; //Основная плоскость
    cColorBits := 24;             //Количество бит для одного цвета без альфаканала
    end;

  //Попросить Windows подобрать запрошенный формат пикселя
  PixelFormat := ChoosePixelFormat(FDC, @PFD);

  //Проверить подобрался ли формат пикселя
  if PixelFormat = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantSelectPixelFormal);

  //Попробовать установить нужный формат пикселя и проверить
  if SetPixelFormat(FDC, PixelFormat, @PFD) = LongBool(0) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetPixelFormat);

  //Создать контекст OpenGL
  FGLContext := wglCreateContext(FDC);

  //Проверить создался ли контекст
  if FGLContext = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateContext);
end;


destructor TsgeGraphicBase.Destroy;
begin
  Deactivate;                             //Отменить выбор контекста
  wglDeleteContext(FGLContext);           //Удалить контекст
end;


procedure TsgeGraphicBase.Activate;
begin
  if not wglMakeCurrent(FDC, FGLContext) then
    raise EsgeException.Create(_UNITNAME, Err_CantActivateContext);
end;


procedure TsgeGraphicBase.Deactivate;
begin
  wglMakeCurrent(0, 0);
end;


procedure TsgeGraphicBase.ChangeViewArea(AWidth, AHeight: Integer);
begin
  //Запомнить размеры окна
  if AWidth < 1 then AWidth := 1;
  FWidth := AWidth;

  if AHeight < 1 then AHeight := 1;
  FHeight := AHeight;

  //Установить область вывода
  SetView(FWidth, FHeight);
end;


end.

