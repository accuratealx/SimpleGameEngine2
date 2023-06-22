{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSprite.pas
Версия            1.1
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase, sgeDisplayElementItemSprite,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectItemSprite = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FData: TsgeDisplayElementItemSptiteData;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;
    FGLSprite: TsgeGraphicOpenGLSprite;

  public
    constructor Create(Element: TsgeDisplayElementItemBase); override;
    destructor  Destroy; override;

    procedure Update(AElement: TsgeDisplayElementItemBase); override;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect); override;
  end;


implementation

uses
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemSprite.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'Sprite';
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;
  FVAO.Attach;

  //Привязать буфер вершин к VAO
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Привязать буфер координат
  FTextureBuffer.Attach;
  FVAO.BindTextureCoord(FTextureBuffer);

  //Залить текстурные координаты
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;
  Buff.AddQuad(0, 1, 1, 0);
  FTextureBuffer.SetData(Buff);
  Buff.Free;

  //Обнулим указатель на спрайт
  FData.Sprite := nil;

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectItemSprite.Destroy;
begin
  //Удалить спрайт из таблицы
  if Assigned(FData.Sprite) then
    OpenGLSpriteTable.Delete(FData.Sprite);

  //Удалить буфер вершинных координат
  FTextureBuffer.Free;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemSprite absolute AElement;
begin
  //Положение
  if deiscsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if deiscsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Buff.AddQuad(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deiscsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deiscsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deiscsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deiscsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Спрайт
  if deiscsSprite in Element.ChangeSet then
  begin
    if FData.Sprite <> Element.Data.Sprite then
    begin
      //Удалить старый спрайт
      if FData.Sprite <> nil then
        OpenGLSpriteTable.Delete(Element.Data.Sprite);

      //Найти новый спрайт
      FData.Sprite := Element.Data.Sprite;
      FGLSprite := OpenGLSpriteTable.Add(FData.Sprite);
    end;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
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

  //Привязать спрайт
  FGLSprite.Attach;

  //Нарисовать
  Graphic.DrawArray(FVAO.VertexType, 0, FVAO.VertexCount);

  //Отвязать спрайт
  FGLSprite.Detach;
end;



end.

