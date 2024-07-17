{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectRect.pas
Версия            1.3
Создан            28.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementRect,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectRect = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementRectData;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;

  public
    constructor Create(Element: TsgeDisplayElement); override;
    destructor  Destroy; override;

    procedure Update(AElement: TsgeDisplayElement); override;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); override;
  end;


implementation

uses
  sgeGraphicOpenGLShaderProgramTable;


constructor TsgeGraphicOpenGLDrawObjectRect.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'Rect';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create(1);

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectRect.Destroy;
begin
  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectRect.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementRect absolute AElement;
begin
  //Положение
  if dercsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if dercsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    //Заполнить буфер
    FVertexBuffer.Quad[0] := sgeGetFloatRect(0, 0, FData.Size.X, FData.Size.Y);

    //Залить данные в OpenGL
    FVertexBuffer.UpdateOpenGLData;
  end;

  //Масштаб
  if dercsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if dercsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if dercsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if dercsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;
end;


procedure TsgeGraphicOpenGLDrawObjectRect.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
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

