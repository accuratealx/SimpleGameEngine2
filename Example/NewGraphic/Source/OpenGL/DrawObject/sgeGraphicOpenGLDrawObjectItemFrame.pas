{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemFrame.pas
Версия            1.1
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветная рамка
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemFrame;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase, sgeDisplayElementItemFrame,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectItemFrame = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FData: TsgeDisplayElementItemFrameData;
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
  sgeGraphicOpenGLTypes, sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemFrame.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'Frame';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create(vtLineLoop);

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectItemFrame.Destroy;
begin
  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemFrame.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemFrame absolute AElement;
begin
  //Положение
  if deifcsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if deifcsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Buff.AddLineRect(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deifcsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deifcsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deifcsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deifcsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Толщина
  if deifcsThickness in Element.ChangeSet then
    FData.Thickness := Element.Data.Thickness;

  //Штриховка
  if deifcsStipple in Element.ChangeSet then
    FData.Stipple := Element.Data.Stipple;

  //масштаб штриховки
  if deifcsStippleScale in Element.ChangeSet then
    FData.StippleScale := Element.Data.StippleScale;
end;


procedure TsgeGraphicOpenGLDrawObjectItemFrame.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
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

