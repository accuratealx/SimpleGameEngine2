{
Пакет             Simple Game Engine 2
Файл              sgeGraphic.pas
Версия            1.9
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс графики
}
{$Include Defines.inc}

unit sgeGraphic;

{$mode objfpc}{$H+}{$Inline On}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeMemoryStream,
  sgeGraphicColor, sgeGraphicSprite,  sgeGraphicFont, sgeGraphicAnimation,
  Windows;


type
  //Информация о драйвере (Производитель, Название видеокарты, Версия OpenGL, Расширения, Версия шейдеров)
  TsgeGraphicInfo = (giVendor, giRenderer, giVersion, giExtensions, giShading);


  //Штриховка линий
  TsgeGraphicLineStipple = (glsSolid, glsDash, glsNarrowDash, glsWideDash, glsDot, glsDashDot, glsDashDotDot);


  //Режим вывода полигонов (Заливка, Линия, Точки)
  TsgeGraphicPolygonMode = (gpmFill, gpmLine, gpmDot);


  //Режим смешивания для цветов
  TsgeGraphicBlendFunction = (gbfTransparent);


  //Режим затенения (Последний цвет, Градиент)
  TsgeGraphicShadeModel = (gsmFlat, gsmSmooth);


  //Активный буфер рисования (Передний, Задний)
  TsgeGraphicRenderBuffer = (grbFront, grbBack);


  //Место для отрисовки
  TsgeGraphicRenderPlace = (grpScreen, grpSprite);


  //Режим наложения текстуры
  TsgeGraphicTextureEnvironment = (gteAdd, gteModulate, gteDecal, gteBlend, gteReplace);


  //Тип координат
  TsgeGraphicCoordinateType = (gctNormal, gctClassic, gctCentered);


  //Режим отражения спрайта
  TsgeGraphicReflectType = (grtHorizontal, grtVertical);
  TsgeGraphicReflectTypes = set of TsgeGraphicReflectType;


  //Опции вывода
  TsgeGraphicDrawOptions = record
    Rect: TsgeFloatRect;                          //Координаты вывода примитива
  	CoordinateType: TsgeGraphicCoordinateType;    //Тип координат
  	Angle: Single;                                //Угол поворота
  	AnglePoint: TsgeFloatPoint;                   //Точка поворота
    Scale: TsgeFloatPoint;                        //Масштаб вывода
    TransparentColor: TsgeColor;                  //Цвет прозрачности
  	Reflect: TsgeGraphicReflectTypes;             //Режим отражения
  	SpriteRect: TsgeFloatRect;                    //Координаты спрайта
    Sprite: TsgeGraphicSprite;                    //Указатель на спрайт
  end;


  //Класс графики
  TsgeGraphic = class
  private
    FDC: HDC;                                                       //Хэндл окна
    FGLContext: HGLRC;                                              //Контекст OpenGL
    FWidth: Integer;                                                //Ширина окна
    FHeight: Integer;                                               //Высота окна

    FRenderBuffer: TsgeGraphicRenderBuffer;                         //Активный буфер
    FRenderPlace: TsgeGraphicRenderPlace;                           //Место отрисовки
    FRenderSprite: TsgeGraphicSprite;                               //Спрайт вывода
    FFrameBuffer: Cardinal;                                         //Кадровый буфер
    FBlendFunction: TsgeGraphicBlendFunction;                       //Режим смешивания цветов
    FDrawOptions: TsgeGraphicDrawOptions;                           //Настройки вывода спрайтов

    function  GetInfo(Index: TsgeGraphicInfo): String;
    procedure SetColor(AColor: TsgeColor);
    function  GetColor: TsgeColor;
    procedure SetBGColor(Acolor: TsgeColor);
    function  GetBGColor: TsgeColor;
    procedure SetPointSize(ASize: Single);
    function  GetPointSize: Single;
    procedure SetLineWidth(AWidth: Single);
    function  GetLineWidth: Single;
    procedure SetPoligonMode(AMode: TsgeGraphicPolygonMode);
    function  GetPoligonMode: TsgeGraphicPolygonMode;
    procedure SetShadeModel(AMode: TsgeGraphicShadeModel);
    function  GetShadeModel: TsgeGraphicShadeModel;
    procedure SetRenderBuffer(ABuffer: TsgeGraphicRenderBuffer);
    procedure SetVerticalSync(AEnable: Boolean);
    function  GetVerticalSync: Boolean;
    procedure SetTextureEnvironment(AInvironment: TsgeGraphicTextureEnvironment);
    function  GetTextureEnvironment: TsgeGraphicTextureEnvironment;
    procedure SetColorBlend(ABlend: Boolean);
    function  GetColorBlend: Boolean;
    procedure SetTexturing(ATexturing: Boolean);
    function  GetTexturing: Boolean;
    procedure SetLineStipple(AStipple: Boolean);
    function  GetLineStipple: Boolean;

    procedure SetBlendFunction(AFunction: TsgeGraphicBlendFunction);
    procedure SetRenderSprite(ASprite: TsgeGraphicSprite);
    procedure SetRenderPlace(APlace: TsgeGraphicRenderPlace);

    procedure SetView(AWidth, AHeight: Integer);

    //Вспомогательные функции вывода
    procedure FillSprite(const Rect, SpriteRect: TsgeFloatRect);                                                    //Вывести примитив со спрайтом
    procedure FillSpriteSegment(const Rect, SpriteRect, SpriteMetric: TsgeFloatRect; const Metric: TsgeIntRect);    //Вывести примитив со спрайтом по частям
    function  GetPrimitiveRect(const Options: TsgeGraphicDrawOptions): TsgeFloatRect;                               //Вернуть координаты примитива с учётом типа и поворота
    function  GetSpriteRect(const Options: TsgeGraphicDrawOptions): TsgeFloatRect;                                  //Вернуть координаты спрайта
    function  GetSpriteMetricRect(const Options: TsgeGraphicDrawOptions; const Metric: TsgeIntRect): TsgeFloatRect; //Возврат смещения для спрайта в координатах OpenGL
    function  GetZeroPoint(const Options: TsgeGraphicDrawOptions): TsgeFloatPoint;                                  //Вернуть точку 0
    function  GetRectByCoordinateType(const Rect: TsgeFloatRect; const CoordType: TsgeGraphicCoordinateType): TsgeFloatRect;  //Вернуть координаты прямоугольника с учётом типа

  public
    constructor Create(DC: HDC; Width, Height: Integer);
    destructor  Destroy; override;

    procedure Init;                                                 //Инициализация
    procedure Done;                                                 //Финализация
    procedure ShareList(Context: HGLRC);                            //Объеденить ресурсы с контекстом

    procedure Activate;
    procedure Deactivate;
    procedure ChangeViewArea(AWidth, AHeight: Integer);

    procedure Reset;
    procedure SwapBuffers;
    procedure Finish;
    procedure Flush;
    procedure EraseBG;
    procedure PushMatrix;
    procedure PopMatrix;
    procedure PushAttrib;
    procedure PopAttrib;

    procedure SetScale(X, Y: Single);
    procedure SetScale(Pos: TsgeFloatPoint);
    procedure SetRotate(Angle: Single);
    procedure SetPos(X, Y: Single);
    procedure SetPos(Pos: TsgeFloatPoint);

    procedure SetLineStipple(Scale: Integer; Pattern: Word);
    procedure SetLineStipple(Scale: Integer; Mode: TsgeGraphicLineStipple);

    procedure ResetDrawOptions;

    //Функции вывода примитивов
    procedure DrawPoint(Point: TsgeFloatPoint);
    procedure DrawPoint(Point: TsgeFloatPoint; Color: TsgeColor);
    procedure DrawPoint(X, Y: Single);
    procedure DrawPoint(X, Y: Single; Color: TsgeColor);

    procedure DrawLine(X1, Y1, X2, Y2: Single);
    procedure DrawLine(Point1, Point2: TsgeFloatPoint);
    procedure DrawLine(X1, Y1, X2, Y2: Single; Color: TsgeColor);
    procedure DrawLine(Point1, Point2: TsgeFloatPoint; Color: TsgeColor);

    procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single);
    procedure DrawTriangle(Point1, Point2, Point3: TsgeFloatPoint);
    procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single; Color: TsgeColor);
    procedure DrawTriangle(Point1, Point2, Point3: TsgeFloatPoint; Color: TsgeColor);

    procedure DrawRect(X1, Y1, X2, Y2: Single);
    procedure DrawRect(Rect: TsgeFloatRect);
    procedure DrawRect(X1, Y1, X2, Y2: Single; Color: TsgeColor);
    procedure DrawRect(Rect: TsgeFloatRect; Color: TsgeColor);

    procedure DrawRectGradient(X1, Y1, X2, Y2: Single; Colors: TsgeQuadColor);
    procedure DrawRectGradient(Rect: TsgeFloatRect; Colors: TsgeQuadColor);

    procedure DrawCircle(XCenter, YCenter: Single; Radius: Single; Quality: Word = 16);
    procedure DrawCircle(XCenter, YCenter: Single; Radius: Single; Color: TsgeColor; Quality: Word = 16);

    //Функции универсального вывода спрайтов
    procedure DrawSprite(const Options: TsgeGraphicDrawOptions);
    procedure DrawSpriteSegment(const Options: TsgeGraphicDrawOptions; const Metric: TsgeIntRect);

    //Функции высокого уровня
    procedure DrawSprite(X, Y, W, H: Single; Sprite: TsgeGraphicSprite);
    procedure DrawSprite(X, Y: Single; Sprite: TsgeGraphicSprite);
    procedure DrawSprite(Rect: TsgeFloatRect; Sprite: TsgeGraphicSprite);

    procedure DrawSpritePart(Rect, SpriteRect: TsgeFloatRect; Sprite: TsgeGraphicSprite);
    procedure DrawSpritePart(rX1, rY1, rX2, rY2: Single; SpriteRect: TsgeFloatRect; Sprite: TsgeGraphicSprite);
    procedure DrawSpritePart(Rect: TsgeFloatRect; sX1, sY1, sX2, sY2: Single; Sprite: TsgeGraphicSprite);
    procedure DrawSpritePart(rX1, rY1, rX2, rY2: Single; sX1, sY1, sX2, sY2: Single; Sprite: TsgeGraphicSprite);

    procedure DrawSpriteTiled(Rect: TsgeFloatRect; Col, Row: Word; Sprite: TsgeGraphicSprite);
    procedure DrawSpriteTiled(X1, Y1, X2, Y2: Single; Col, Row: Word; Sprite: TsgeGraphicSprite);
    procedure DrawSpriteTiled(X, Y: Single; Col, Row: Word; Sprite: TsgeGraphicSprite);

    procedure DrawAnimation(Rect: TsgeFloatRect; Animation: TsgeGraphicAnimation);
    procedure DrawAnimation(X1, Y1, X2, Y2: Single; Animation: TsgeGraphicAnimation);
    procedure DrawAnimation(X1, Y1: Single; Animation: TsgeGraphicAnimation);

    procedure DrawText(X, Y: Single; Font: TsgeGraphicFont; Text: String = '');

    procedure ScreenShot(Stream: TsgeMemoryStream);

    //Свойства
    property Context: HGLRC read FGLContext;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Info[Index: TsgeGraphicInfo]: String read GetInfo;
    property Color: TsgeColor read GetColor write SetColor;
    property BGColor: TsgeColor read GetBGColor write SetBGColor;
    property PointSize: Single read GetPointSize write SetPointSize;
    property LineWidth: Single read GetLineWidth write SetLineWidth;
    property PoligonMode: TsgeGraphicPolygonMode read GetPoligonMode write SetPoligonMode;
    property ShadeModel: TsgeGraphicShadeModel read GetShadeModel write SetShadeModel;
    property TextureEnvironment: TsgeGraphicTextureEnvironment read GetTextureEnvironment write SetTextureEnvironment;
    property RenderBuffer: TsgeGraphicRenderBuffer read FRenderBuffer write SetRenderBuffer;
    property BlendFunction: TsgeGraphicBlendFunction read FBlendFunction write SetBlendFunction;
    property VerticalSync: Boolean read GetVerticalSync write SetVerticalSync;
    property RenderPlace: TsgeGraphicRenderPlace read FRenderPlace write SetRenderPlace;
    property RenderSprite: TsgeGraphicSprite read FRenderSprite write SetRenderSprite;
    property ColorBlend: Boolean read GetColorBlend write SetColorBlend;
    property Texturing: Boolean read GetTexturing write SetTexturing;
    property LineStipple: Boolean read GetLineStipple write SetLineStipple;

    //Настройки отрисовки
    property doCoordinateType: TsgeGraphicCoordinateType read FDrawOptions.CoordinateType write FDrawOptions.CoordinateType;
  	property doAngle: Single read FDrawOptions.Angle write FDrawOptions.Angle;
  	property doAnglePoint: TsgeFloatPoint read FDrawOptions.AnglePoint write FDrawOptions.AnglePoint;
    property doScale: TsgeFloatPoint read FDrawOptions.Scale write FDrawOptions.Scale;
    property doTransparentColor: TsgeColor read FDrawOptions.TransparentColor write FDrawOptions.TransparentColor;
  	property doReflect: TsgeGraphicReflectTypes read FDrawOptions.Reflect write FDrawOptions.Reflect;

  end;


const
  //Опции вывода по умолчанию
  DefaultDrawOptions: TsgeGraphicDrawOptions = (
    Rect: (X1: 0; Y1: 0; X2: 0; Y2: 0);
  	CoordinateType: gctNormal;
  	Angle: 0;
  	AnglePoint: (X: 0; Y: 0);
    Scale: (X: 1; Y: 1);
    TransparentColor: (Red: 1; Green: 1; Blue: 1; Alpha: 1);
    Reflect: [];
    SpriteRect: (X1: 0; Y1: 1; X2: 1; Y2: 0);
    Sprite: nil
    );


implementation

uses
  sgeErrors, sgeGraphicAnimationFrames, sgeGraphicUtils,
  dglOpenGL, Math;


const
  _UNITNAME = 'Graphic';

  Err_CantLoadOpenGLLib         = 'CantLoadOpenGLLib';
  Err_CantSelectPixelFormal     = 'CantSelectPixelFormal';
  Err_CantSetPixelFormat        = 'CantSetPixelFormat';
  Err_CantCreateContext         = 'CantCreateContext';
  Err_CantActivateContext       = 'CantActivateContext';
  Err_VerticalSyncNotSupported  = 'VerticalSyncNotSupported';
  Err_CantShareContext          = 'CantShareContext';
  Err_SpriteIsEmpty             = 'SpriteIsEmpty';



function TsgeGraphic.GetInfo(Index: TsgeGraphicInfo): String;
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


procedure TsgeGraphic.SetColor(AColor: TsgeColor);
begin
  glColor4fv(@Acolor);
end;


function TsgeGraphic.GetColor: TsgeColor;
begin
  glGetFloatv(GL_CURRENT_COLOR, @Result);
end;


procedure TsgeGraphic.SetBGColor(Acolor: TsgeColor);
begin
  glClearColor(Acolor.Red, Acolor.Green, Acolor.Blue, Acolor.Alpha);
end;


function TsgeGraphic.GetBGColor: TsgeColor;
begin
  glGetFloatv(GL_COLOR_CLEAR_VALUE, @Result);
end;


procedure TsgeGraphic.SetPointSize(ASize: Single);
begin
  glPointSize(ASize);
end;


function TsgeGraphic.GetPointSize: Single;
begin
  glGetFloatv(GL_POINT_SIZE, @Result);
end;


procedure TsgeGraphic.SetLineWidth(AWidth: Single);
begin
  glLineWidth(AWidth);
end;


function TsgeGraphic.GetLineWidth: Single;
begin
  glGetFloatv(GL_LINE_WIDTH, @Result);
end;


procedure TsgeGraphic.SetPoligonMode(AMode: TsgeGraphicPolygonMode);
begin
  case AMode of
    gpmDot:
      glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);

    gpmLine:
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    gpmFill:
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
end;


function TsgeGraphic.GetPoligonMode: TsgeGraphicPolygonMode;
var
  rs: array[0..1] of Integer;
begin
  //rs[0] - Front
  //rs[1] - Back
  glGetIntegerv(GL_POLYGON_MODE, @rs[0]);
  if rs[0] = GL_POINT then
    Result := gpmDot;

  if rs[0] = GL_LINE then
    Result := gpmLine;

  if rs[0] = GL_FILL then
    Result := gpmFill;
end;


procedure TsgeGraphic.SetShadeModel(AMode: TsgeGraphicShadeModel);
begin
  case AMode of
    gsmFlat:
      glShadeModel(GL_FLAT);

    gsmSmooth:
      glShadeModel(GL_SMOOTH);
  end;
end;


function TsgeGraphic.GetShadeModel: TsgeGraphicShadeModel;
var
  rs: Integer;
begin
  glGetIntegerv(GL_SHADE_MODEL, @rs);
  if rs = GL_FLAT then
    Result := gsmFlat;

  if rs = GL_SMOOTH then
    Result := gsmSmooth;
end;


procedure TsgeGraphic.SetRenderBuffer(ABuffer: TsgeGraphicRenderBuffer);
begin
  FRenderBuffer := ABuffer;

  case FRenderBuffer of
    grbFront:
      glDrawBuffer(GL_FRONT);

    grbBack:
      glDrawBuffer(GL_BACK);
  end;
end;


procedure TsgeGraphic.SetVerticalSync(AEnable: Boolean);
begin
  if wglSwapIntervalEXT = nil then
    raise EsgeException.Create(_UNITNAME, Err_VerticalSyncNotSupported);

  if AEnable then
    wglSwapIntervalEXT(1)
  else
    wglSwapIntervalEXT(0);
end;


function TsgeGraphic.GetVerticalSync: Boolean;
begin
  if wglGetSwapIntervalEXT = nil then
    raise EsgeException.Create(_UNITNAME, Err_VerticalSyncNotSupported);

  Result := (wglGetSwapIntervalEXT() = 1);
end;


procedure TsgeGraphic.SetTextureEnvironment(AInvironment: TsgeGraphicTextureEnvironment);
begin
  case AInvironment of
    gteAdd:
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_ADD);

    gteModulate:
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    gteDecal:
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

    gteBlend:
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);

    gteReplace:
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  end;
end;


function TsgeGraphic.GetTextureEnvironment: TsgeGraphicTextureEnvironment;
var
  i: GLint;
begin
  glGetTexEnviv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, @i);

  case i of
    GL_ADD:
      Result := gteAdd;

    GL_MODULATE:
      Result := gteModulate;

    GL_DECAL:
      Result := gteDecal;

    GL_BLEND:
      Result := gteBlend;

    GL_REPLACE:
      Result := gteReplace;

    else
      Result := gteModulate;
  end;
end;


procedure TsgeGraphic.SetColorBlend(ABlend: Boolean);
begin
  if ABlend then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
end;


function TsgeGraphic.GetColorBlend: Boolean;
begin
  Result := glIsEnabled(GL_BLEND);
end;


procedure TsgeGraphic.SetTexturing(ATexturing: Boolean);
begin
  if ATexturing then
    glEnable(GL_TEXTURE_2D)
  else
    glDisable(GL_TEXTURE_2D);
end;


function TsgeGraphic.GetTexturing: Boolean;
begin
  Result := glIsEnabled(GL_TEXTURE_2D)
end;


procedure TsgeGraphic.SetLineStipple(AStipple: Boolean);
begin
  if AStipple then
    glEnable(GL_LINE_STIPPLE)
  else
    glDisable(GL_LINE_STIPPLE);
end;


function TsgeGraphic.GetLineStipple: Boolean;
begin
  Result := glIsEnabled(GL_LINE_STIPPLE);
end;


procedure TsgeGraphic.SetBlendFunction(AFunction: TsgeGraphicBlendFunction);
begin
  if FBlendFunction = AFunction then
    Exit;

  FBlendFunction := AFunction;
  case AFunction of
    gbfTransparent:
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
end;


procedure TsgeGraphic.SetRenderSprite(ASprite: TsgeGraphicSprite);
begin
  //Установить спрайт для вывода
  FRenderSprite := ASprite;

  //Если режим вывода в спрайт, то изменить привязку
  if (FRenderPlace = grpSprite) and (ASprite <> nil) then
    SetRenderPlace(grpSprite);
end;


procedure TsgeGraphic.SetRenderPlace(APlace: TsgeGraphicRenderPlace);
begin
  case APlace of
    grpScreen:
    begin
      glBindFramebuffer(GL_FRAMEBUFFER, 0);                         //Отвязать буфер от вывода
      SetView(FWidth, FHeight);                                     //Вернуть размеры окна
    end;

    grpSprite:
    begin
      if FRenderSprite = nil then
        raise EsgeException.Create(_UNITNAME, Err_SpriteIsEmpty);

      glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuffer);                                                        //Установить временный буфер для вывода
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FRenderSprite.GLHandle, 0); //Связать буфер кадра текстурой
      SetView(FRenderSprite.Width, FRenderSprite.Height);                                                     //Изменить размеры области вывода на размеры спрайта
    end;
  end;

  FRenderPlace := APlace;
end;


procedure TsgeGraphic.SetView(AWidth, AHeight: Integer);
begin
  glViewport(0, 0, AWidth, AHeight);                                //Задать область вывода
  glMatrixMode(GL_PROJECTION);                                      //Выбрать матрицу проекций
  glLoadIdentity;                                                   //Изменить проекцию на эталонную
  glOrtho(0, AWidth, AHeight, 0, -1, 1);                            //Изменить проекцию на ортографическую
  glMatrixMode(GL_MODELVIEW);                                       //Выбрать матрицу модели
  glLoadIdentity;                                                   //Изменить проекцию на эталонную
end;


procedure TsgeGraphic.FillSprite(const Rect, SpriteRect: TsgeFloatRect);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(SpriteRect.X1, SpriteRect.Y1);                     //Top-Left
    glVertex2f(Rect.X1, Rect.Y1);
    glTexCoord2f(SpriteRect.X1, SpriteRect.Y2);                     //Bottom-Left
    glVertex2f(Rect.X1, Rect.Y2);
    glTexCoord2f(SpriteRect.X2, SpriteRect.Y2);                     //Bottom-Right
    glVertex2f(Rect.X2, Rect.Y2);
    glTexCoord2f(SpriteRect.X2, SpriteRect.Y1);                     //Top-Right
    glVertex2f(Rect.X2, Rect.Y1);
  glEnd;
end;


procedure TsgeGraphic.FillSpriteSegment(const Rect, SpriteRect, SpriteMetric: TsgeFloatRect; const Metric: TsgeIntRect);
var
  Rct, sRct: TsgeFloatRect;
begin
  //Top-Left
  Rct.X1 := Rect.X1;
  Rct.Y1 := Rect.Y1;
  Rct.X2 := Rect.X1 + Metric.X1;
  Rct.Y2 := Rect.Y1 + Metric.Y1;
  sRct.X1 := SpriteRect.X1;
  sRct.Y1 := SpriteRect.Y1;
  sRct.X2 := sRct.X1 + SpriteMetric.X1;
  sRct.Y2 := sRct.Y1 - SpriteMetric.Y1;
  FillSprite(Rct, sRct);

  //Top
  Rct.X1 := Rect.X1 + Metric.X1;
  Rct.Y1 := Rect.Y1;
  Rct.X2 := Rect.X2 - Metric.X2;
  Rct.Y2 := Rect.Y1 + Metric.Y1;
  sRct.X1 := SpriteRect.X1 + SpriteMetric.X1;
  sRct.Y1 := SpriteRect.Y1;
  sRct.X2 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y2 := sRct.Y1 - SpriteMetric.Y1;
  FillSprite(Rct, sRct);

  //Top-Right
  Rct.X1 := Rect.X2 - Metric.X2;
  Rct.Y1 := Rect.Y1;
  Rct.X2 := Rect.X2;
  Rct.Y2 := Rect.Y1 + Metric.Y1;
  sRct.X1 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y1 := SpriteRect.Y1;
  sRct.X2 := SpriteRect.X2;
  sRct.Y2 := sRct.Y1 - SpriteMetric.Y1;
  FillSprite(Rct, sRct);

  //Right
  Rct.X1 := Rect.X2 - Metric.X2;
  Rct.Y1 := Rect.Y1 + Metric.Y1;
  Rct.X2 := Rect.X2;
  Rct.Y2 := Rect.Y2 - Metric.Y2;
  sRct.X1 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y1 := SpriteRect.Y1 - SpriteMetric.Y1;
  sRct.X2 := SpriteRect.X2;
  sRct.Y2 := SpriteRect.Y2 + SpriteMetric.Y2;
  FillSprite(Rct, sRct);

  //Bottom-Right
  Rct.X1 := Rect.X2 - Metric.X2;
  Rct.Y1 := Rect.Y2 - Metric.Y2;
  Rct.X2 := Rect.X2;
  Rct.Y2 := Rect.Y2;
  sRct.X1 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y1 := SpriteRect.Y2 + SpriteMetric.Y2;
  sRct.X2 := SpriteRect.X2;
  sRct.Y2 := SpriteRect.Y2;
  FillSprite(Rct, sRct);

  //Bottom
  Rct.X1 := Rect.X1 + Metric.X1;
  Rct.Y1 := Rect.Y2 - Metric.Y2;
  Rct.X2 := Rect.X2 - Metric.X2;
  Rct.Y2 := Rect.Y2;
  sRct.X1 := SpriteRect.X1 + SpriteMetric.X1;
  sRct.Y1 := SpriteRect.Y2 + SpriteMetric.Y2;
  sRct.X2 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y2 := SpriteRect.Y2;
  FillSprite(Rct, sRct);

  //Left-Bottom
  Rct.X1 := Rect.X1;
  Rct.Y1 := Rect.Y2 - Metric.Y2;
  Rct.X2 := Rect.X1 + Metric.X1;
  Rct.Y2 := Rect.Y2;
  sRct.X1 := SpriteRect.X1;
  sRct.Y1 := SpriteRect.Y2 + SpriteMetric.Y2;
  sRct.X2 := SpriteRect.X1 + SpriteMetric.X1;
  sRct.Y2 := SpriteRect.Y2;
  FillSprite(Rct, sRct);

  //Left
  Rct.X1 := Rect.X1;
  Rct.Y1 := Rect.Y1 + Metric.Y1;
  Rct.X2 := Rect.X1 + Metric.X1;
  Rct.Y2 := Rect.Y2 - Metric.Y2;
  sRct.X1 := SpriteRect.X1;
  sRct.Y1 := SpriteRect.Y1 - SpriteMetric.Y1;
  sRct.X2 := SpriteRect.X1 + SpriteMetric.X1;
  sRct.Y2 := SpriteRect.Y2 + SpriteMetric.Y2;
  FillSprite(Rct, sRct);

  //Center
  Rct.X1 := Rect.X1 + Metric.X1;
  Rct.Y1 := Rect.Y1 + Metric.Y1;
  Rct.X2 := Rect.X2 - Metric.X2;
  Rct.Y2 := Rect.Y2 - Metric.Y2;
  sRct.X1 := SpriteRect.X1 + SpriteMetric.X1;
  sRct.Y1 := SpriteRect.Y1 - SpriteMetric.Y1;
  sRct.X2 := SpriteRect.X2 - SpriteMetric.X2;
  sRct.Y2 := SpriteRect.Y2 + SpriteMetric.Y2;
  FillSprite(Rct, sRct);
end;


function TsgeGraphic.GetPrimitiveRect(const Options: TsgeGraphicDrawOptions): TsgeFloatRect;
var
  W, H, HalfW, HalfH, xt, yt: Single;
begin
  case Options.CoordinateType of
    gctClassic:
    begin
      //Размеры примитива
      W := Options.Rect.X2 - Options.Rect.X1;
      H := Options.Rect.Y2 - Options.Rect.Y1;

      //Пол размера
      HalfW := W / 2;
      HalfH := H / 2;

      //Смещение для 0 точки
      xt := Options.Rect.X1 + HalfW + Options.AnglePoint.X;
      yt := Options.Rect.Y1 + HalfH + Options.AnglePoint.Y;

      //Координаты примитива
      Result.X1 := Options.Rect.X1 - xt;
      Result.X2 := Result.X1 + W;
      Result.Y1 := Options.Rect.Y1 - yt;
      Result.Y2 := Result.Y1 + H;
    end;

    gctNormal:
    begin
      //Размеры примитива
      W := Options.Rect.X2;
      H := Options.Rect.Y2;

      //Пол размера
      HalfW := W / 2;
      HalfH := H / 2;

      //Смещение для 0 точки
      xt := Options.Rect.X1 + HalfW + Options.AnglePoint.X;
      yt := Options.Rect.Y1 + HalfH + Options.AnglePoint.Y;

      //Координаты примитива
      Result.X1 := Options.Rect.X1 - xt;
      Result.X2 := Result.X1 + W;
      Result.Y1 := Options.Rect.Y1 - yt;
      Result.Y2 := Result.Y1 + H;
    end;

    gctCentered:
    begin
      //Размеры примитива
      W := Options.Rect.X2;
      H := Options.Rect.Y2;

      //Пол размера
      HalfW := W / 2;
      HalfH := H / 2;

      //Смещение для 0 точки
      xt := Options.Rect.X1 + Options.AnglePoint.X;
      yt := Options.Rect.Y1 + Options.AnglePoint.Y;

      //Координаты примитива
      Result.X1 := Options.Rect.X1 - xt - HalfW;
      Result.X2 := Result.X1 + W;
      Result.Y1 := Options.Rect.Y1 - yt - HalfH;
      Result.Y2 := Result.Y1 + H;
    end;
  end;
end;


function TsgeGraphic.GetSpriteRect(const Options: TsgeGraphicDrawOptions): TsgeFloatRect;
var
  D: Single;
begin
  Result := Options.SpriteRect;

  //Отражение спрайта по горизонтали
  if grtHorizontal in Options.Reflect then
  begin
    D := Result.X1;
    Result.X1 := Result.X2;
    Result.X2 := D;
  end;

  //Отражение спрайта по вертикали
  if grtVertical in Options.Reflect then
  begin
    D := Result.Y1;
    Result.Y1 := Result.Y2;
    Result.Y2 := D;
  end;
end;


function TsgeGraphic.GetSpriteMetricRect(const Options: TsgeGraphicDrawOptions; const Metric: TsgeIntRect): TsgeFloatRect;
begin
  //Смещение
  Result.X1 := Metric.X1 * Options.Sprite.GLPixelWidth;
  Result.Y1 := Metric.Y1 * Options.Sprite.GLPixelHeight;
  Result.X2 := Metric.X2 * Options.Sprite.GLPixelWidth;
  Result.Y2 := Metric.Y2 * Options.Sprite.GLPixelHeight;

  //Проверить горизонтальное отражение
  if grtHorizontal in Options.Reflect then
  begin
    Result.X1 := -Result.X1;
    Result.X2 := -Result.X2;
  end;

  //Проверить вертикальное отражение
  if grtVertical in Options.Reflect then
  begin
    Result.Y1 := -Result.Y1;
    Result.Y2 := -Result.Y2;
  end;
end;


function TsgeGraphic.GetZeroPoint(const Options: TsgeGraphicDrawOptions): TsgeFloatPoint;
begin
  case Options.CoordinateType of
    gctClassic:
    begin
      Result.X := Options.Rect.X1 + (Options.Rect.X2 - Options.Rect.X1) / 2 + Options.AnglePoint.X;
      Result.Y := Options.Rect.Y1 + (Options.Rect.Y2 - Options.Rect.Y1) / 2 + Options.AnglePoint.Y;
    end;

    gctNormal:
    begin
      Result.X := Options.Rect.X1 + Options.Rect.X2 / 2 + Options.AnglePoint.X;
      Result.Y := Options.Rect.Y1 + Options.Rect.Y2 / 2 + Options.AnglePoint.Y;
    end;

    gctCentered:
    begin
      Result.X := Options.Rect.X1 + Options.AnglePoint.X;
      Result.Y := Options.Rect.Y1 + Options.AnglePoint.Y;
    end;
  end;
end;


function TsgeGraphic.GetRectByCoordinateType(const Rect: TsgeFloatRect; const CoordType: TsgeGraphicCoordinateType): TsgeFloatRect;
var
  HalfWidth, HalfHeight: Single;
begin
  //Значение по умолчанию
  Result := Rect;

  case CoordType of
    gctNormal:
    begin
      Result.X2 := Rect.X1 + Rect.X2;
      Result.Y2 := Rect.Y1 + Rect.Y2;
    end;

    gctCentered:
    begin
      HalfWidth := Rect.X2 / 2;
      HalfHeight := Rect.Y2 / 2;
      Result.X1 := Rect.X1 - HalfWidth;
      Result.Y1 := Rect.Y1 - HalfHeight;
      Result.X2 := Result.X1 + HalfWidth;
      Result.Y2 := Result.Y1 + HalfHeight;
    end;
  end;
end;


constructor TsgeGraphic.Create(DC: HDC; Width, Height: Integer);
var
  PFD: TPIXELFORMATDESCRIPTOR;
  PixelFormat: Integer;
begin
  //Запомнить размеры
  FWidth := Width;
  FHeight := Height;
  FBlendFunction := gbfTransparent;

  //Загрузить библиотеку
  if GL_LibHandle = nil then
    InitOpenGL;

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

  //Настройки вывода по умолчанию
  FDrawOptions := DefaultDrawOptions;
end;



destructor TsgeGraphic.Destroy;
begin
  wglDeleteContext(FGLContext);           //Удалить контекст
end;


procedure TsgeGraphic.Init;
begin
  //Установка начальных значений
  Activate;                                                   //Выбрать контекст OpenGL
  ReadExtensions;                                             //Найти адреса всех расширений
  glGenFramebuffers(1, @FFrameBuffer);                        //Выделить память для буфера кадра
  ChangeViewArea(FWidth, FHeight);                            //Изменить вывод OpenGL
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);          //Задать режим смешивания
  SetTextureEnvironment(gteReplace);                          //Задать режим текстурирования
  FRenderBuffer := grbBack;                                   //По умолчанию задний буфер
  SetVerticalSync(True);                                      //Включить вертикальную синхронизацию
  SetTexturing(True);                                         //Включить текстурирование
  SetColorBlend(True);                                        //Включить смешивание цветов
  Deactivate;                                                 //Отменить выбор контекста
end;


procedure TsgeGraphic.Done;
begin
  //Удалить буфер кадра
  glDeleteFramebuffers(1, @FFrameBuffer);

  //Освободить контекст
  wglMakeCurrent(0, 0);
end;


procedure TsgeGraphic.ShareList(Context: HGLRC);
begin
  //Расшарить ресурсы между контекстами
  if not wglShareLists(FGLContext, Context) then
    raise EsgeException.Create(_UNITNAME, Err_CantShareContext);
end;


procedure TsgeGraphic.Activate;
begin
  if not wglMakeCurrent(FDC, FGLContext) then
    raise EsgeException.Create(_UNITNAME, Err_CantActivateContext);
end;


procedure TsgeGraphic.Deactivate;
begin
  wglMakeCurrent(0, 0);
end;


procedure TsgeGraphic.ChangeViewArea(AWidth, AHeight: Integer);
begin
  //Запомнить размеры окна
  if AWidth < 1 then
    AWidth := 1;
  FWidth := AWidth;

  if AHeight < 1 then
    AHeight := 1;
  FHeight := AHeight;

  //Установить область вывода
  SetView(FWidth, FHeight);
end;


procedure TsgeGraphic.Reset;
begin
  glLoadIdentity;
end;


procedure TsgeGraphic.SwapBuffers;
begin
  Windows.SwapBuffers(FDC);
end;


procedure TsgeGraphic.Finish;
begin
  glFinish;
end;


procedure TsgeGraphic.Flush;
begin
  glFlush;
end;


procedure TsgeGraphic.EraseBG;
begin
  glClear(GL_COLOR_BUFFER_BIT);
end;


procedure TsgeGraphic.PushMatrix;
begin
  glPushMatrix;
end;


procedure TsgeGraphic.PopMatrix;
begin
  glPopMatrix;
end;


procedure TsgeGraphic.PushAttrib;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
end;


procedure TsgeGraphic.PopAttrib;
begin
  glPopAttrib;
end;


procedure TsgeGraphic.SetScale(X, Y: Single);
begin
  glScalef(X, Y, 0);
end;


procedure TsgeGraphic.SetScale(Pos: TsgeFloatPoint);
begin
  SetScale(Pos.X, Pos.Y);
end;


procedure TsgeGraphic.SetRotate(Angle: Single);
begin
  glRotatef(Angle, 0, 0, 1);
end;


procedure TsgeGraphic.SetPos(X, Y: Single);
begin
  glTranslatef(X, Y, 0);
end;


procedure TsgeGraphic.SetPos(Pos: TsgeFloatPoint);
begin
  SetPos(Pos.X, Pos.Y);
end;


procedure TsgeGraphic.SetLineStipple(Scale: Integer; Pattern: Word);
begin
  glLineStipple(Scale, Pattern);
end;


procedure TsgeGraphic.SetLineStipple(Scale: Integer; Mode: TsgeGraphicLineStipple);
var
  w: Word;
begin
  case Mode of
    glsSolid      : w := $FFFF;                                     //****************
    glsDash       : w := $0F0F;                                     //----****----****
    glsNarrowDash : w := $7777;                                     //-***-***-***-***
    glsWideDash   : w := $3F3F;                                     //--******--******
    glsDot        : w := $5555;                                     //-*-*-*-*-*-*-*-*
    glsDashDot    : w := $2727;                                     //--*--***--*--***
    glsDashDotDot : w := $5757;                                     //-*-*-***-*-*-***
  end;
  glLineStipple(Scale, W);
end;


procedure TsgeGraphic.ResetDrawOptions;
begin
  FDrawOptions := DefaultDrawOptions;
end;


procedure TsgeGraphic.DrawPoint(X, Y: Single);
begin
  glBegin(GL_POINTS);
    glVertex2f(X, Y);
  glEnd;
end;


procedure TsgeGraphic.DrawPoint(X, Y: Single; Color: TsgeColor);
begin
  glPushAttrib(GL_CURRENT_BIT);                                     //Сохранить текущий цвет
  glColor4fv(@Color);                                               //Установить цвет
  DrawPoint(X, Y);                                                  //Вывод точки
  glPopAttrib;                                                      //Восстановить цвет
end;


procedure TsgeGraphic.DrawPoint(Point: TsgeFloatPoint);
begin
  DrawPoint(Point.X, Point.Y);
end;


procedure TsgeGraphic.DrawPoint(Point: TsgeFloatPoint; Color: TsgeColor);
begin
  DrawPoint(Point.X, Point.Y, Color);
end;


procedure TsgeGraphic.DrawLine(X1, Y1, X2, Y2: Single);
begin
  glBegin(GL_LINES);
    glVertex2f(X1, Y1);
    glVertex2f(X2, Y2);
  glEnd;
end;


procedure TsgeGraphic.DrawLine(Point1, Point2: TsgeFloatPoint);
begin
  DrawLine(Point1.X, Point1.Y, Point2.X, Point2.Y);
end;


procedure TsgeGraphic.DrawLine(X1, Y1, X2, Y2: Single; Color: TsgeColor);
begin
  glPushAttrib(GL_CURRENT_BIT);
  glColor4fv(@Color);
  DrawLine(X1, Y1, X2, Y2);
  glPopAttrib;
end;


procedure TsgeGraphic.DrawLine(Point1, Point2: TsgeFloatPoint; Color: TsgeColor);
begin
  DrawLine(Point1.X, Point1.Y, Point2.X, Point2.Y, Color);
end;


procedure TsgeGraphic.DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single);
begin
  glBegin(GL_TRIANGLES);
    glVertex2f(X1, Y1);
    glVertex2f(X2, Y2);
    glVertex2f(X3, Y3);
  glEnd;
end;


procedure TsgeGraphic.DrawTriangle(Point1, Point2, Point3: TsgeFloatPoint);
begin
  DrawTriangle(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y);
end;


procedure TsgeGraphic.DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Single; Color: TsgeColor);
begin
  glPushAttrib(GL_CURRENT_BIT);
  glColor4fv(@Color);
  DrawTriangle(X1, Y1, X2, Y2, X3, Y3);
  glPopAttrib;
end;


procedure TsgeGraphic.DrawTriangle(Point1, Point2, Point3: TsgeFloatPoint; Color: TsgeColor);
begin
  DrawTriangle(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X, Point3.Y, Color);
end;


procedure TsgeGraphic.DrawRect(X1, Y1, X2, Y2: Single);
begin
  DrawRect(sgeGetFloatRect(X1, Y1, X2, Y2));
end;


procedure TsgeGraphic.DrawRect(Rect: TsgeFloatRect);
begin
  Rect := GetRectByCoordinateType(Rect, FDrawOptions.CoordinateType);

  glBegin(GL_QUADS);
    glVertex2f(Rect.X1, Rect.Y1);
    glVertex2f(Rect.X1, Rect.Y2);
    glVertex2f(Rect.X2, Rect.Y2);
    glVertex2f(Rect.X2, Rect.Y1);
  glEnd;
end;


procedure TsgeGraphic.DrawRect(X1, Y1, X2, Y2: Single; Color: TsgeColor);
begin
  glPushAttrib(GL_CURRENT_BIT);
  glColor4fv(@Color);
  DrawRect(X1, Y1, X2, Y2);
  glPopAttrib;
end;


procedure TsgeGraphic.DrawRect(Rect: TsgeFloatRect; Color: TsgeColor);
begin
  DrawRect(Rect.X1, Rect.Y1, Rect.X2, Rect.Y2, Color);
end;


procedure TsgeGraphic.DrawRectGradient(X1, Y1, X2, Y2: Single; Colors: TsgeQuadColor);
begin
  DrawRectGradient(sgeGetFloatRect(X1, Y1, X2, Y2), Colors);
end;


procedure TsgeGraphic.DrawRectGradient(Rect: TsgeFloatRect; Colors: TsgeQuadColor);
begin
  Rect := GetRectByCoordinateType(Rect, FDrawOptions.CoordinateType);

  glPushAttrib(GL_CURRENT_BIT);
  glBegin(GL_QUADS);
    glColor4fv(@Colors.Color1);
    glVertex2f(Rect.X1, Rect.Y1);
    glColor4fv(@Colors.Color2);
    glVertex2f(Rect.X1, Rect.Y2);
    glColor4fv(@Colors.Color3);
    glVertex2f(Rect.X2, Rect.Y2);
    glColor4fv(@Colors.Color4);
    glVertex2f(Rect.X2, Rect.Y1);
  glEnd;
  glPopAttrib;
end;


procedure TsgeGraphic.DrawCircle(XCenter, YCenter: Single; Radius: Single; Quality: Word);
var
  i, c: Integer;
  aCos, aSin, da: Single;
begin
  da := (Pi * 2) / Quality;
  c := Quality - 1;

  glBegin(GL_POLYGON);
    for i := 0 to c do
    begin
      SinCos(da * i, aSin, aCos);
      glVertex2f(XCenter + Radius * aCos, YCenter + Radius * aSin);
    end;
  glEnd;
end;


procedure TsgeGraphic.DrawCircle(XCenter, YCenter: Single; Radius: Single; Color: TsgeColor; Quality: Word);
begin
  glPushAttrib(GL_CURRENT_BIT);
  glColor4fv(@Color);
  DrawCircle(XCenter, YCenter, Radius, Quality);
  PopAttrib;
end;


procedure TsgeGraphic.DrawSprite(const Options: TsgeGraphicDrawOptions);
var
  Rect, SprRect: TsgeFloatRect;
  Pt: TsgeFloatPoint;
begin
  Rect := GetPrimitiveRect(Options);                                //Координаты примитива
  SprRect := GetSpriteRect(Options);                                //Подготовить координаты спрайта

  glPushMatrix;                                                     //Сохранить матрицу
  Pt := GetZeroPoint(Options);                                      //Точка переноса
  glTranslatef(Pt.X, Pt.Y, 0);                                      //Перенос координат
  glRotatef(Options.Angle, 0, 0, 1);                                //Поворот
  glScalef(Options.Scale.X, Options.Scale.Y, 0);                    //Масштаб по осям
  glPushAttrib(GL_CURRENT_BIT or GL_TEXTURE_BIT);                   //Запомнить состояние цвета и режима наложения текстуры
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);      //Изменить режим наложения текстуры на смешивание
  glColor4fv(@Options.TransparentColor);                            //Изменить цвет примитива для смешивания с текстурой

  glBindTexture(GL_TEXTURE_2D, Options.Sprite.GLHandle);            //Привязать текстуру
  FillSprite(Rect, SprRect);                                        //Вывод спрайта
  glBindTexture(GL_TEXTURE_2D, 0);                                  //Отвязать текстуру

  glPopAttrib;                                                      //Восстановить настройки цвета и режима наложения текстуры
  glPopMatrix;                                                      //Восстановить матрицу
end;


procedure TsgeGraphic.DrawSpriteSegment(const Options: TsgeGraphicDrawOptions; const Metric: TsgeIntRect);
var
  Rect, SprRect, MetricRect: TsgeFloatRect;
  Pt: TsgeFloatPoint;
begin
  Rect := GetPrimitiveRect(Options);                                //Координаты примитива
  SprRect := GetSpriteRect(Options);                                //Подготовить координаты спрайта
  MetricRect := GetSpriteMetricRect(Options, Metric);               //Подготовить смещение для спрайта

  glPushMatrix;                                                     //Сохранить матрицу
  Pt := GetZeroPoint(Options);                                      //Точка переноса
  glTranslatef(Pt.X, Pt.Y, 0);                                      //Перенос координат
  glRotatef(Options.Angle, 0, 0, 1);                                //Поворот
  glScalef(Options.Scale.X, Options.Scale.Y, 0);                    //Масштаб по осям
  glPushAttrib(GL_CURRENT_BIT or GL_TEXTURE_BIT);                   //Запомнить состояние цвета и режима наложения текстуры
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);      //Изменить режим наложения текстуры на смешивание
  glColor4fv(@Options.TransparentColor);                            //Изменить цвет примитива для смешивания с текстурой

  glBindTexture(GL_TEXTURE_2D, Options.Sprite.GLHandle);            //Привязать текстуру
  FillSpriteSegment(Rect, SprRect, MetricRect, Metric);             //Вывод спрайта по частям
  glBindTexture(GL_TEXTURE_2D, 0);                                  //Отвязать текстуру

  glPopAttrib;                                                      //Восстановить настройки цвета и режима наложения текстуры
  glPopMatrix;                                                      //Восстановить матрицу
end;


procedure TsgeGraphic.DrawSprite(X, Y, W, H: Single; Sprite: TsgeGraphicSprite);
begin
  DrawSprite(sgeGetFloatRect(X, Y, W, H), Sprite);
end;


procedure TsgeGraphic.DrawSprite(X, Y: Single; Sprite: TsgeGraphicSprite);
var
  Opt: TsgeGraphicDrawOptions;
begin
  if Sprite = nil then
    Exit;

  Opt := FDrawOptions;
  Opt.Sprite := Sprite;
  Opt.Rect.X1 := X;
  Opt.Rect.Y1 := Y;
  Opt.Rect.X2 := Sprite.Width;
  Opt.Rect.Y2 := Sprite.Height;
  if Opt.CoordinateType = gctClassic then
    Opt.CoordinateType := gctNormal;

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawSprite(Rect: TsgeFloatRect; Sprite: TsgeGraphicSprite);
var
  Opt: TsgeGraphicDrawOptions;
begin
  if Sprite = nil then
    Exit;

  Opt := FDrawOptions;
  Opt.Sprite := Sprite;
  Opt.Rect := Rect;

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawSpritePart(Rect, SpriteRect: TsgeFloatRect; Sprite: TsgeGraphicSprite);
var
  Opt: TsgeGraphicDrawOptions;
begin
  if Sprite = nil then
    Exit;

  Opt := FDrawOptions;
  Opt.Sprite := Sprite;
  Opt.Rect := Rect;
  Opt.SpriteRect := sgeGetTextureRect(Sprite, SpriteRect);

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawSpritePart(rX1, rY1, rX2, rY2: Single; SpriteRect: TsgeFloatRect; Sprite: TsgeGraphicSprite);
begin
  DrawSpritePart(sgeGetFloatRect(rX1, rY1, rX2, rY2), SpriteRect, Sprite);
end;


procedure TsgeGraphic.DrawSpritePart(Rect: TsgeFloatRect; sX1, sY1, sX2, sY2: Single; Sprite: TsgeGraphicSprite);
begin
  DrawSpritePart(Rect, sgeGetFloatRect(sX1, sY1, sX2, sY2), Sprite);
end;


procedure TsgeGraphic.DrawSpritePart(rX1, rY1, rX2, rY2: Single; sX1, sY1, sX2, sY2: Single; Sprite: TsgeGraphicSprite);
begin
  DrawSpritePart(sgeGetFloatRect(rX1, rY1, rX2, rY2), sgeGetFloatRect(sX1, sY1, sX2, sY2), Sprite);
end;


procedure TsgeGraphic.DrawSpriteTiled(Rect: TsgeFloatRect; Col, Row: Word; Sprite: TsgeGraphicSprite);
var
  Opt: TsgeGraphicDrawOptions;
begin
  if Sprite = nil then
    Exit;

  Opt := FDrawOptions;
  Opt.Sprite := Sprite;
  Opt.Rect := Rect;
  Opt.SpriteRect := sgeGetTextureTileRect(Sprite, Col, Row);

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawSpriteTiled(X1, Y1, X2, Y2: Single; Col, Row: Word; Sprite: TsgeGraphicSprite);
begin
  DrawSpriteTiled(sgeGetFloatRect(X1, Y1, X2, Y2), Col, Row, Sprite);
end;


procedure TsgeGraphic.DrawSpriteTiled(X, Y: Single; Col, Row: Word; Sprite: TsgeGraphicSprite);
var
  Opt: TsgeGraphicDrawOptions;
begin
  if Sprite = nil then
    Exit;

  Opt := FDrawOptions;
  Opt.Sprite := Sprite;
  Opt.Rect.X1 := X;
  Opt.Rect.Y1 := Y;
  Opt.Rect.X2 := Sprite.TileWidth;
  Opt.Rect.Y2 := Sprite.TileHeight;
  Opt.SpriteRect := sgeGetTextureTileRect(Sprite, Col, Row);

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawAnimation(Rect: TsgeFloatRect; Animation: TsgeGraphicAnimation);
var
  Opt: TsgeGraphicDrawOptions;
  Frame: TsgeGraphicAnimationFrame;
begin
  if Animation = nil then
    Exit;

  Frame := Animation.CurrentFrame;

  Opt := FDrawOptions;
  Opt.Sprite := Frame.Sprite;
  Opt.Rect := Rect;
  Opt.SpriteRect := sgeGetTextureTileRect(Frame.Sprite, Frame.Col, Frame.Row);

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawAnimation(X1, Y1, X2, Y2: Single; Animation: TsgeGraphicAnimation);
begin
  DrawAnimation(sgeGetFloatRect(X1, Y1, X2, Y2), Animation);
end;


procedure TsgeGraphic.DrawAnimation(X1, Y1: Single; Animation: TsgeGraphicAnimation);
var
  Opt: TsgeGraphicDrawOptions;
  Frame: TsgeGraphicAnimationFrame;
begin
  if Animation = nil then
    Exit;

  Frame := Animation.CurrentFrame;

  Opt := FDrawOptions;
  Opt.Sprite := Frame.Sprite;
  Opt.Rect.X1 := X1;
  Opt.Rect.Y1 := Y1;
  Opt.Rect.X2 := Animation.Width;
  Opt.Rect.Y2 := Animation.Height;
  Opt.SpriteRect := sgeGetTextureTileRect(Frame.Sprite, Frame.Col, Frame.Row);
  if Opt.CoordinateType = gctClassic then
    Opt.CoordinateType := gctNormal;

  DrawSprite(Opt);
end;


procedure TsgeGraphic.DrawText(X, Y: Single; Font: TsgeGraphicFont; Text: String);
begin
  if Font = nil then
    Exit;
  if Text = '' then
    Exit;

  glPushAttrib(GL_LIST_BIT);                                        //Сохранить настройки дисплейных списков
  glRasterPos2f(X, Y + Font.Height - Font.CharDescent);             //Указать координаты вывода растров
  glListBase(Font.GLHandle);                                        //Выбрать первый дисплейный список
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, PAnsiChar(Text));     //Вывести списки с номерами равным коду символа
  glPopAttrib;                                                      //Вернуть настройки
end;


procedure TsgeGraphic.ScreenShot(Stream: TsgeMemoryStream);
var
  BFH: TBitmapFileHeader;
  BIH: TBITMAPINFOHEADER;
  BytesPerLine, Trash, Size, szFileHeader, szInfoHeader: Integer;
  DATA: array of Byte;
begin
  //Байтов в одной строке
  BytesPerLine := FWidth * 3;

  //Определить количество байт для выравнивания
  Trash := BytesPerLine mod 4;

  //Определить размер данных с мусором
  Size := (BytesPerLine + Trash) * FHeight;

  //Чтение данных из OpenGL
  SetLength(DATA, Size);                                            //Буфер для OpenGL
  glPushClientAttrib(GL_CLIENT_PIXEL_STORE_BIT);                    //Запомнить настройки выравнивания битов
  glPixelStorei(GL_PACK_ALIGNMENT, 4);                              //Выравнивание по dword
  glReadBuffer(GL_FRONT);                                           //Указать передний буфер кадра
  glReadPixels(0, 0, FWidth, FHeight, GL_BGR, GL_UNSIGNED_BYTE, @DATA[0]);  //Прочесть в буфер цвета точек без прозрачности
  glPopClientAttrib;                                                //Вернуть настройки выравнивания

  //Определить размеры структур
  szFileHeader := SizeOf(TBitmapFileHeader);
  szInfoHeader := SizeOf(TBITMAPINFOHEADER);

  //Описатель BMP файла
  ZeroMemory(@BFH, szFileHeader);
  BFH.bfType := $4D42;                                              //Волшебное слово от микрософта - BM
  BFH.bfOffBits := szFileHeader + szInfoHeader;                     //Смещение от начала файла до самих данных
  BFH.bfSize := BFH.bfOffBits + Size;                               //Размер файла целиком со структурами и мусором

  //Описатель BMP
  ZeroMemory(@BIH, szInfoHeader);
  BIH.biSize := szInfoHeader;                                       //Размер этой структуры. Интересно зачем
  BIH.biWidth := FWidth;                                            //Ширина битмапа
  BIH.biHeight := FHeight;                                          //Высота битмапа
  BIH.biPlanes := 1;                                                //Сколько слоёв
  BIH.biBitCount := 24;                                             //Бит на пиксель
  BIH.biCompression := BI_RGB;                                      //Без сжатия
  BIH.biXPelsPerMeter := 3780;                                      //Разрешение по X
  BIH.biYPelsPerMeter := 3780;                                      //Разрешение по Y

  //Записать в поток
  Stream.Size := 0;                                                 //Обнулить память
  Stream.Write(BFH, 0, SizeOf(BFH));                                //Заголовок файла
  Stream.Write(BIH, szFileHeader, SizeOf(BIH));                     //Описание битмапа
  Stream.Write(DATA[0], szFileHeader + szInfoHeader, Size);         //Записать данные

  //Очистить буфер
  SetLength(DATA, 0);
end;



end.
