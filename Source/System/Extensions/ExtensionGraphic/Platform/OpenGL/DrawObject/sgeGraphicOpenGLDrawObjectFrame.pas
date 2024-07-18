{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemFrame.pas
Версия            1.4
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветная рамка
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectFrame;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementFrame,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectFrame = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementFrame.TData;
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


constructor TsgeGraphicOpenGLDrawObjectFrame.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'Frame';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create(1);

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create(vtLineLoop);

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectFrame.Destroy;
begin
  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectFrame.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementFrame absolute AElement;
begin
  //Положение
  if csPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if csSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    //Заполнить буфер
    FVertexBuffer.Quad[0] := sgeGetFloatRect(0, 0, FData.Size.X, FData.Size.Y);

    //Залить данные в OpenGL
    FVertexBuffer.UpdateOpenGLData;
  end;

  //Масштаб
  if csScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if csOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if csAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if csColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Толщина
  if csThickness in Element.ChangeSet then
    FData.Thickness := Element.Data.Thickness;

  //Штриховка
  if csStipple in Element.ChangeSet then
    FData.Stipple := Element.Data.Stipple;

  //масштаб штриховки
  if csStippleScale in Element.ChangeSet then
    FData.StippleScale := Element.Data.StippleScale;
end;


procedure TsgeGraphicOpenGLDrawObjectFrame.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
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

  //Настроить толщину линии
  Graphic.SetLineWidth(FData.Thickness);

  //Настроить штриховку

  //Настроить масштаб штриховки


  //Нарисовать
  Graphic.DrawArray(FVAO.VertexType, 0, FVAO.VertexCount);
end;



end.

