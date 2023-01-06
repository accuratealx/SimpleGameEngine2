{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGL.pas
Версия            1.0
Создан            06.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Реализация интерфейса IsgeGraphic под OpenGL
}
{$Include Defines.inc}

unit sgeGraphicOpenGL;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL, Windows,
  sgeTypes, sgeGraphicColor, sgeGraphic;


type
  //Информация о драйвере
  TsgeGraphicInfo = (
    giVendor,             //Производитель
    giRenderer,           //Название видеокарты
    giVersion,            //Версия OpenGL
    giExtensions,         //Расширения
    giShading             //Версия шейдеров
  );


  //Рендерер OpenGL
  TsgeGraphicOpenGL = class(TInterfacedObject, IsgeGraphic)
  private
    FDC: HDC;                                                       //Хэндл окна
    FGLContext: HGLRC;                                              //Контекст OpenGL
    FWidth: Integer;                                                //Ширина области вывода
    FHeight: Integer;                                               //Высота области вывода

    function  GetInfo(Index: TsgeGraphicInfo): String;

    procedure SetVerticalSync(AEnable: Boolean);
    function  GetVerticalSync: Boolean;
  public
    constructor Create(DC: HDC; MajorVersion, MinorVersion: Byte);
    destructor Destroy; override;

    //Системные функции
    procedure ChangeViewPort(AWidth, AHeight: Integer);             //Изменить область вывода
    procedure Activate;
    procedure Deactivate;
    procedure SwapBuffers;
    procedure Finish;
    procedure Flush;
    procedure SetBGColor(Color: TsgeColor);
    procedure EraseBG;


    //IsgeGraphic
    function  GetWidth: Integer;
    function  GetHeight: Integer;
    procedure SetScale(X, Y: Single);
    procedure SetScale(Pos: TsgeFloatPoint);
    procedure SetRotate(Angle: Single);
    procedure SetPos(X, Y: Single);
    procedure SetPos(Pos: TsgeFloatPoint);
    procedure SaveState;
    procedure LoadState;
    procedure SetColor(Color: TsgeColor);


    //Свойства
    property DC: HDC read FDC;
    property GLContext: HGLRC read FGLContext;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property VerticalSync: Boolean read GetVerticalSync write SetVerticalSync;
    property Info[Index: TsgeGraphicInfo]: String read GetInfo;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicOpenGL';

  Err_CantLoadOpenGLLib         = 'CantLoadOpenGLLib';
  Err_CantSelectPixelFormal     = 'CantSelectPixelFormal';
  Err_CantSetPixelFormat        = 'CantSetPixelFormat';
  Err_CantCreateContext         = 'CantCreateContext';
  Err_ContextVersionNotSupport  = 'ContextVersionNotSupport';
  Err_CantActivateContext       = 'CantActivateContext';
  Err_VerticalSyncNotSupported  = 'VerticalSyncNotSupported';



function TsgeGraphicOpenGL.GetInfo(Index: TsgeGraphicInfo): String;
begin
  case Index of
    giVendor:
      Result := glGetString(GL_VENDOR);

    giRenderer:
      Result := glGetString(GL_RENDERER);

    giVersion:
      Result := glGetString(GL_VERSION);

    giExtensions:
      Result := glGetString(GL_EXTENSIONS);

    giShading:
      Result := glGetString(GL_SHADING_LANGUAGE_VERSION);
  end;
end;


procedure TsgeGraphicOpenGL.SetVerticalSync(AEnable: Boolean);
begin
  if wglSwapIntervalEXT = nil then
    raise EsgeException.Create(_UNITNAME, Err_VerticalSyncNotSupported);

  if AEnable then
    wglSwapIntervalEXT(1)
  else
    wglSwapIntervalEXT(0);
end;


function TsgeGraphicOpenGL.GetVerticalSync: Boolean;
begin
  if wglGetSwapIntervalEXT = nil then
    raise EsgeException.Create(_UNITNAME, Err_VerticalSyncNotSupported);

  Result := (wglGetSwapIntervalEXT() = 1);
end;


constructor TsgeGraphicOpenGL.Create(DC: HDC; MajorVersion, MinorVersion: Byte);
var
  PFDescriptor: TPixelFormatDescriptor;
  PixelFormat: Integer;
  LegacyRC: HGLRC;
  Attribs: array of Integer;
begin
  //Запомнить параметры
  FDC := DC;

  //Загрузить библиотеку
  if GL_LibHandle = nil then
    InitOpenGL;

  //Проверить загрузилась ли Opengl32.dll
  if not Assigned(GL_LibHandle) then
    raise EsgeException.Create(_UNITNAME, Err_CantLoadOpenGLLib);

  //Прочитать адреса функций
  ReadOpenGLCore;

  //Заполнить Pixel format
  ZeroMemory(@PFDescriptor, SizeOf(PFDescriptor));
  with PFDescriptor do
  begin
    nSize := SizeOf(PFDescriptor);
    nVersion := 1;
    dwFlags := PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_DRAW_TO_WINDOW or PFD_TYPE_RGBA; //Поддержка OpenGL, двойной буфер, рисуем на окне, альфаканал
    iPixelType := PFD_TYPE_RGBA;  //Формат цвета 32 бита на точку
    iLayerType := PFD_MAIN_PLANE; //Основная плоскость
    cColorBits := 24;             //Количество бит для одного цвета без альфаканала
  end;

  //Попросить Windows подобрать запрошенный формат пикселя
  PixelFormat := ChoosePixelFormat(FDC, @PFDescriptor);

  //Проверить подобрался ли формат пикселя
  if PixelFormat = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantSelectPixelFormal);

  //Попробовать установить нужный формат пикселя и проверить
  if SetPixelFormat(FDC, PixelFormat, @PFDescriptor) = LongBool(0) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetPixelFormat);

  //Создать контекст OpenGL
  LegacyRC := wglCreateContext(FDC);

  //Проверить создался ли контекст
  if LegacyRC = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateContext);

  //Активировать контекст
  wglMakeCurrent(FDC, LegacyRC);

  //Ищем функция создания версионного контекста
  Pointer(wglCreateContextAttribsARB) := wglGetProcAddress('wglCreateContextAttribsARB');

  //Проверить поддержку расширения
  if not Assigned(wglCreateContextAttribsARB) then
  begin
    wglDeleteContext(LegacyRC);
    raise EsgeException.Create(_UNITNAME, Err_ContextVersionNotSupport);
  end;

  //Заполняем версионные атрибуты
  Attribs := nil;
  SetLength(Attribs, 5);
  Attribs[0] := WGL_CONTEXT_MAJOR_VERSION_ARB;
  Attribs[1] := MajorVersion;
  Attribs[2] := WGL_CONTEXT_MINOR_VERSION_ARB;
  Attribs[3] := MinorVersion;
  Attribs[4] := 0;                                                  //Конец структуры - 0

  //Создать версионный контекст
  FGLContext := wglCreateContextAttribsARB(FDC, 0, @Attribs[0]);

  //Проверить создание контекста
  if FGLContext = 0 then
  begin
    wglDeleteContext(LegacyRC);
    raise EsgeException.Create(_UNITNAME, Err_CantCreateContext, sgeIntToStr(MajorVersion) + '.' + sgeIntToStr(MinorVersion));
  end;

  //Почистить временный контекст
  wglDeleteContext(LegacyRC);
end;


destructor TsgeGraphicOpenGL.Destroy;
begin
  wglDeleteContext(FGLContext);
end;


procedure TsgeGraphicOpenGL.ChangeViewPort(AWidth, AHeight: Integer);
begin
  //Проверить размеры вывода
  if AWidth < 1 then
    AWidth := 1;
  if AHeight < 1 then
    AHeight := 1;

  //Сохранить параметры
  FWidth := AWidth;
  FHeight := AHeight;

  //Колдунство
  glViewport(0, 0, FWidth, FHeight);                                //Задать область вывода
  glMatrixMode(GL_PROJECTION);                                      //Выбрать матрицу проекций
  glLoadIdentity;                                                   //Изменить проекцию на эталонную
  glOrtho(0, AWidth, AHeight, 0, -1, 1);                            //Изменить проекцию на ортографическую
  glMatrixMode(GL_MODELVIEW);                                       //Выбрать матрицу модели
  glLoadIdentity;                                                   //Изменить проекцию на эталонную
end;


procedure TsgeGraphicOpenGL.Activate;
begin
  if not wglMakeCurrent(FDC, FGLContext) then
    raise EsgeException.Create(_UNITNAME, Err_CantActivateContext);
end;


procedure TsgeGraphicOpenGL.Deactivate;
begin
  wglMakeCurrent(0, 0);
end;


procedure TsgeGraphicOpenGL.SwapBuffers;
begin
  Windows.SwapBuffers(FDC);
end;


procedure TsgeGraphicOpenGL.Finish;
begin
  glFinish;
end;


procedure TsgeGraphicOpenGL.Flush;
begin
  glFlush;
end;


procedure TsgeGraphicOpenGL.SetBGColor(Color: TsgeColor);
begin
  glClearColor(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;


procedure TsgeGraphicOpenGL.EraseBG;
begin
  glClear(GL_COLOR_BUFFER_BIT);
end;


function TsgeGraphicOpenGL.GetWidth: Integer;
begin
  Result := FWidth;
end;


function TsgeGraphicOpenGL.GetHeight: Integer;
begin
  Result := FHeight;
end;


procedure TsgeGraphicOpenGL.SetScale(X, Y: Single);
begin
  glScalef(X, Y, 0);
end;


procedure TsgeGraphicOpenGL.SetScale(Pos: TsgeFloatPoint);
begin
  SetScale(Pos.X, Pos.Y);
end;


procedure TsgeGraphicOpenGL.SetRotate(Angle: Single);
begin
  glRotatef(Angle, 0, 0, 1);
end;


procedure TsgeGraphicOpenGL.SetPos(X, Y: Single);
begin
  glTranslatef(X, Y, 0);
end;


procedure TsgeGraphicOpenGL.SetPos(Pos: TsgeFloatPoint);
begin
  SetPos(Pos.X, Pos.Y);
end;


procedure TsgeGraphicOpenGL.SaveState;
begin
  //Сохранить матрицу модели
  glPushMatrix;

  //Сохранить настройки для векторов
  glPushClientAttrib( GL_CLIENT_VERTEX_ARRAY_BIT);

  //Сохранить атрибуты
  glPushAttrib(GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT or GL_ENABLE_BIT or GL_HINT_BIT or GL_LINE_BIT or
               GL_LIST_BIT or GL_POINT_BIT or GL_POLYGON_BIT or GL_TEXTURE_BIT);
end;


procedure TsgeGraphicOpenGL.LoadState;
begin
  glPopAttrib;
  glPopClientAttrib;
  glPopMatrix;
end;


procedure TsgeGraphicOpenGL.SetColor(Color: TsgeColor);
begin
  glColor4fv(@Color);
end;



end.

