{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemRect.pas
Версия            1.1
Создан            28.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase, sgeDisplayElementItemRect,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectItemRect = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FData: TsgeDisplayElementItemRectData;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;

  public
    constructor Create(Element: TsgeDisplayElementItemBase); override;
    destructor  Destroy; override;

    procedure Update(AElement: TsgeDisplayElementItemBase); override;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect); override;
  end;


implementation

uses
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemRect.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'Rect';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectItemRect.Destroy;
begin
  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemRect.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemRect absolute AElement;
begin
  //Положение
  if deircsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if deircsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Buff.AddQuad(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deircsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deircsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deircsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deircsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;
end;


procedure TsgeGraphicOpenGLDrawObjectItemRect.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
begin
  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetScreenSize(ScreenSize);
  FShaderProgram.SetLayer(LayerInfo);

  FShaderProgram.SetPos(FData.Position);
  FShaderProgram.SetColor(FData.Color);
  FShaderProgram.SetScale(FData.Scale);
  FShaderProgram.SetOrigin(FData.Origin);
  FShaderProgram.SetAngle(FData.Angle);

  //Нарисовать
  Graphic.DrawArray(FVAO.VertexType, 0, FVAO.VertexCount);
end;



end.

