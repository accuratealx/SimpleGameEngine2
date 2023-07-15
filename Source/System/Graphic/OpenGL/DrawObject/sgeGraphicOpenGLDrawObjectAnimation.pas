{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectAnimation.pas
Версия            1.2
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Анимация
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectAnimation;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementAnimation,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectAnimation = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementAnimationData;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;
    FGLSprite: TsgeGraphicOpenGLSprite;

  public
    constructor Create(Element: TsgeDisplayElement); override;
    destructor  Destroy; override;

    procedure Update(AElement: TsgeDisplayElement); override;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); override;
  end;


implementation

uses
  sgeGraphicOpenGLUtils,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectAnimation.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'Animation';
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

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectAnimation.Destroy;
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


procedure TsgeGraphicOpenGLDrawObjectAnimation.Update(AElement: TsgeDisplayElement);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementAnimation absolute AElement;
  Rect: TsgeFloatRect;
  i: Integer;
begin
  //Положение
  if deacsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Спрайт
  if deacsSprite in Element.ChangeSet then
  begin
    if FData.Sprite <> Element.Data.Sprite then
    begin
      //Удалить старый спрайт
      if FData.Sprite <> nil then
        OpenGLSpriteTable.Delete(FData.Sprite);

      //Найти новый спрайт
      FData.Sprite := Element.Data.Sprite;
      FGLSprite := OpenGLSpriteTable.Add(FData.Sprite);
    end;
  end;

  //Кадры анимации
  if deacsFrames in Element.ChangeSet then
  begin
    FData.Frames := Element.Data.Frames;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    for i := 0 to FData.Frames.Count - 1 do
    begin
      Rect := sgeGetTextureTileRect(
        FGLSprite.GLTileWidth,
        FGLSprite.GLTileHeight,
        FData.Frames.Item[i].Column,
        FData.Frames.Item[i].Row
      );
      Buff.AddQuad(Rect);
    end;
    FTextureBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Размеры
  if deacsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;
    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    for i := 0 to FData.Frames.Count - 1 do
      Buff.AddQuad(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deacsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deacsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deacsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deacsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Номер кадра
  if deacsFrameIndex in Element.ChangeSet then
    FData.FrameIndex := Element.Data.FrameIndex;
end;


procedure TsgeGraphicOpenGLDrawObjectAnimation.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
const
  ANIMATION_FRAME_VERTEX_COUNT = 6;
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

  //Нарисовать 6 вершин нужного кадра
  Graphic.DrawArray(
    FVAO.VertexType,                                  //Тип вершин
    FData.FrameIndex * ANIMATION_FRAME_VERTEX_COUNT,  //Начальный номер вершин
    ANIMATION_FRAME_VERTEX_COUNT                      //Количество вершин
  );

  //Отвязать спрайт
  FGLSprite.Detach;
end;



end.

