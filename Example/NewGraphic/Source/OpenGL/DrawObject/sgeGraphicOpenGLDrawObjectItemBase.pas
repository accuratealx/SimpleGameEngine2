{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemBase.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Базовый
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemBase;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicOpenGL, sgeGraphicOpenGLShaderProgram,
  sgeDisplayElementItemBase,
  sgeGraphicOpenGLVertexArrayObject, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectItemBase = class
  protected
    FVAO: TsgeGraphicOpenGLVertexArrayObject;                       //Объект хранения буферов и настроек вывода
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;                 //Ссылка на шейдерную программу
    FVertexBuffer: TsgeGraphicOpenGLBuffer;                         //Массив вершин

    FElement: TsgeDisplayElementItemBase;                           //Ссылка на элемент вывода (копия)

    function  GetShaderProgramName: String; virtual; abstract;      //Имя шейдерной программы для класса
    procedure UserInit; virtual;                                    //Пользовательский конструктор
    procedure UserDone; virtual;                                    //Пользовательский деструктор
    procedure UserDrawBegin; virtual;                               //Пользовательский рендер начало
    procedure UserDrawEnd; virtual;                                 //Пользовательский рендер конец

    procedure UpdateVertexBuffer(AElement: TsgeDisplayElementItemBase); //Обновить координаты вершин
  public
    constructor Create(Element: TsgeDisplayElementItemBase = nil); virtual;
    destructor  Destroy; override;

    procedure Update(Element: TsgeDisplayElementItemBase); virtual; //Обновление параметров

    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatTriple); virtual;
  end;


implementation

uses
  dglOpenGL,
  sgeErrors,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLCoordBuffer;

const
  _UNITNAME = 'sgeGraphicOpenGLDrawObjectItemBase';

  Err_EmptyElement = 'EmptyElement';


procedure TsgeGraphicOpenGLDrawObjectItemBase.UserInit;
begin
  //Заглушка пользовательского конструктора
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.UserDone;
begin
  //Заглушка пользовательского деструктора
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.UserDrawBegin;
begin
  //Заглушка пользовательской рисовалки
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.UserDrawEnd;
begin
  //Заглушка пользовательской рисовалки
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.UpdateVertexBuffer(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  w, h: GLfloat;
begin
  //Создать буфер c координатами
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;
  if AElement.Centered then
  begin
    w := AElement.Width / 2;
    h := AElement.Height / 2;
    Buff.AddQuad(-w, -h, w, h);
  end
  else
    Buff.AddQuad(0, 0, AElement.Width, AElement.Height);

  //Залить данные в видеокарту
  FVertexBuffer.SetData(Buff);

  //Удалить промежуточный буфер
  Buff.Free;
end;


constructor TsgeGraphicOpenGLDrawObjectItemBase.Create(Element: TsgeDisplayElementItemBase);
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(GetShaderProgramName);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create(vtTriangle);

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Пользовательский конструктор
  UserInit;

  //Обновить из элемента
  if Element <> nil then
    Update(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectItemBase.Destroy;
begin
  //Пользовательский деструктор
  UserDone;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.Update(Element: TsgeDisplayElementItemBase);
begin
  //Проверить объект
  if Element = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyElement);

  //Запонить элемент
  FElement := Element;

  //Обновить буфер вершин
  UpdateVertexBuffer(FElement);
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatTriple);
begin
  //Отключить смешивание цветов
  if not FElement.Transparent then
    Graphic.Disable(gcColorBlend);

  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetScreenSize(ScreenSize);
  FShaderProgram.SetLayer(LayerInfo);
  FShaderProgram.SetPos(sgeGetFloatPoint(FElement.X, FElement.Y));
  FShaderProgram.SetScaleAngleAlpha(sgeGetFloatTriple(FElement.Scale, FElement.Angle, FElement.Alpha));

  //Пользовательский рендер
  UserDrawBegin;

  //Нарисовать
  FVAO.DrawArray;

  //Пользовательский рендер
  UserDrawEnd;

  //Включить смешивание цветов
  if not FElement.Transparent then
    Graphic.Enable(gcColorBlend);
end;



end.

