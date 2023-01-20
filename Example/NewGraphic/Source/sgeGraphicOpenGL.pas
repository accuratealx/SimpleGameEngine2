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
  sgeTypes, sgeSprite, sgeGraphicColor, sgeGraphic, sgeGraphicOpenGLSprite;


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
    procedure DrawSprite(X, Y: Single; Sprite: TsgeSprite);


    procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single; Color: TsgeColor);


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

  //Прочитать адреса функций
  ReadOpenGLCore;

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
  //Attribs[4] := WGL_CONTEXT_FLAGS_ARB;
  //Attribs[5] := WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
  Attribs[4] := 0;                                                  //Конец структуры - 0

  //Создать версионный контекст
  FGLContext := wglCreateContextAttribsARB(FDC, 0, @Attribs[0]);

  //Проверить создание контекста
  if FGLContext = 0 then
  begin
    wglDeleteContext(LegacyRC);
    raise EsgeException.Create(_UNITNAME, Err_CantCreateContext, sgeIntToStr(MajorVersion) + '.' + sgeIntToStr(MinorVersion));
  end;

  //Активировать новый контекст
  wglMakeCurrent(FDC, FGLContext);

  //Почистить временный контекст
  wglDeleteContext(LegacyRC);
end;


destructor TsgeGraphicOpenGL.Destroy;
begin
  wglMakeCurrent(0, 0);
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


procedure TsgeGraphicOpenGL.DrawSprite(X, Y: Single; Sprite: TsgeSprite);
var
  Spr: TsgeGraphicOpenGLSprite;

  VBOHandle: GLuint;
  VBO: array of TsgeFloatPoint;
begin
  Spr := TsgeGraphicOpenGLSprite.Create(Sprite);

  glBindTexture(GL_TEXTURE_2D, Spr.GLHandle);

  glGenBuffers(1, @VBOHandle);






  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);





  glBindTexture(GL_TEXTURE_2D, 0);

  glDisable(GL_TEXTURE_2D);

  glDeleteBuffers(1, @VBOHandle);

  Spr.Free;
end;


procedure TsgeGraphicOpenGL.DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single; Color: TsgeColor);
const
  VertexShaderSource: String =
  '#version 400 core' + LineEnding +
  'layout (location = 0) in vec2 aPos;' + LineEnding +

  'void main()' + LineEnding +
  '{'  + LineEnding +
  '  gl_Position = vec4(aPos.x / 1000, -aPos.y / 1000, 0, 1.0);'  + LineEnding +
  '}'#0;

  FragmentShaderSource: String =
  '#version 400 core' + LineEnding +
  'out vec4 FragColor;' + LineEnding +

  'void main()' + LineEnding +
  '{' + LineEnding +
  '  FragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);' + LineEnding +
  '}'#0;

var
  Vertex: array of GLfloat;

  VBO: GLuint;

  VAO: GLuint;

  ShaderProgram, ShaderProgramStatus: GLuint;
  ShaderProgramError: array[0..1024] of Char;

  VertexShader, VertexShaderStatus: GLuint;
  VertexShaderError: array[0..1024] of Char;

  FragmentShader, FragmentShaderStatus: GLuint;
  FragmentShaderError: array[0..1024] of Char;
begin
  //Выделить память под шейдер
  VertexShader := glCreateShader(GL_VERTEX_SHADER);
  //Установить исходник шейдера
  glShaderSource(VertexShader, 1, @VertexShaderSource, Nil);
  //Собрать шейдер
  glCompileShader(vertexShader);
  //Проверить на ошибки
  glGetShaderiv(VertexShader, GL_COMPILE_STATUS, @VertexShaderStatus);
  if VertexShaderStatus = 0 then
  begin
    glGetShaderInfoLog(VertexShader, Length(VertexShaderError), nil, @VertexShaderError);
    raise EsgeException.Create(_UNITNAME, 'Vertex shader error', VertexShaderError);
  end;


  //Выделить память под шейдер
  FragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  //Установить исходник шейдера
  glShaderSource(FragmentShader, 1, @FragmentShaderSource, Nil);
  //Собрать шейдер
  glCompileShader(FragmentShader);
  //Проверить на ошибки
  glGetShaderiv(FragmentShader, GL_COMPILE_STATUS, @FragmentShaderStatus);
  if FragmentShaderStatus = 0 then
  begin
    glGetShaderInfoLog(FragmentShader, Length(FragmentShaderError), nil, @FragmentShaderError);
    raise EsgeException.Create(_UNITNAME, 'Fragment shader error', FragmentShaderError);
  end;


  //Выделить память под программу
  ShaderProgram := glCreateProgram();
  //Привязать вершинный шейдер
  glAttachShader(ShaderProgram, VertexShader);
  //Привязать фрагментный шейдер
  glAttachShader(ShaderProgram, FragmentShader);

  //Собрать все до кучи
  glLinkProgram(ShaderProgram);

  //Проверить на ошибки
  glGetProgramiv(ShaderProgram, GL_LINK_STATUS, @ShaderProgramStatus);
  if ShaderProgramStatus = 0 then
  begin
    glGetProgramInfoLog(ShaderProgram, Length(ShaderProgramError), nil, @ShaderProgramError);
    raise EsgeException.Create(_UNITNAME, 'Shader program error', ShaderProgramError);
  end;

  //Удалить ненужные шейдеры
  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);





  SetLength(Vertex, 6);
  Vertex[0] := X1;
  Vertex[1] := Y1;
  Vertex[2] := X2;
  Vertex[3] := Y2;
  Vertex[4] := X3;
  Vertex[5] := Y3;

  //Выделить память под VAO
  glGenVertexArrays(1, @VAO);
  //Выбрать VAO для работы
  glBindVertexArray(VAO);


  //Выделить память под массив вершин
  glGenBuffers(1, @VBO);
  //Выбрать буфер для работы
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  //Залить массив вершин
  glBufferData(GL_ARRAY_BUFFER, 6 * SizeOf(GLfloat), @Vertex[0], GL_STATIC_DRAW);


  //Указать параметры данных вершин
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), nil);
  //Разрешить использовать 0 набор данных в шейдере
  glEnableVertexAttribArray(0);


  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);



  //Использовать текущую программу
  glUseProgram(ShaderProgram);
  //Выбрать настроенный объект
  glBindVertexArray(VAO);
  //Отрисовать вершины
  glDrawArrays(GL_TRIANGLES, 0, 3);
  //Отключить объект
  glBindVertexArray(0);



  glDeleteVertexArrays(1, @VAO);
  glDeleteProgram(ShaderProgram);
  glDeleteBuffers(1, @VBO);
end;



end.

