{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectAnimationUnamnaged.pas
Версия            1.1
Создан            15.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Неуправляемая анимация
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectAnimationUnamnaged;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeAnimation,
  sgeDisplayElement, sgeDisplayElementAnimationUnmanaged,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectAnimationUnamnaged = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementAnimationUnmanagedData;
    FVAO: TsgeGraphicOpenGLVertexArrayObject;
    FShaderProgram: TsgeGraphicOpenGLShaderProgram;
    FVertexBuffer: TsgeGraphicOpenGLBuffer;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;
    FGLSprite: TsgeGraphicOpenGLSprite;
    FAnimation: TsgeAnimation;

  public
    constructor Create(Element: TsgeDisplayElement); override;
    destructor  Destroy; override;

    procedure Update(AElement: TsgeDisplayElement); override;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); override;
  end;


implementation

uses
  sgeGraphicOpenGLUtils,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable;


constructor TsgeGraphicOpenGLDrawObjectAnimationUnamnaged.Create(Element: TsgeDisplayElement);
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

  //Создать анимацию
  FAnimation := TsgeAnimation.Create(TsgeDisplayElementAnimationUnmanaged(Element).Data.Frames);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectAnimationUnamnaged.Destroy;
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

  //Удалить анимацию
  FAnimation.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectAnimationUnamnaged.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementAnimationUnmanaged absolute AElement;
  Rect: TsgeFloatRect;
  i: Integer;
begin
  //Положение
  if deaucsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Отражение
  if deaucsReflect in Element.ChangeSet then
    FData.Reflect := Element.Data.Reflect;

  //Спрайт
  if deaucsSprite in Element.ChangeSet then
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
  if (deaucsFrames in Element.ChangeSet) or (deaucsReflect in Element.ChangeSet) then
  begin
    FData.Frames := Element.Data.Frames;


    FTextureBuffer.QuadCount := FData.Frames.Count;
    for i := 0 to FData.Frames.Count - 1 do
    begin
      Rect := sgeGetTextureTileRect(
        FGLSprite.GLTileWidth,
        FGLSprite.GLTileHeight,
        FData.Frames.Item[i].Column,
        FData.Frames.Item[i].Row
      );

      //Проверить на отражение
      if FData.Reflect <> [] then
        Rect := sgeGetReflectRect(Rect, FData.Reflect);

      //Подготовить буфер
      FTextureBuffer.Quad[i] := Rect;
    end;

    //Залить данные в OpenGL
    FTextureBuffer.UpdateOpenGLData;

    //Изменить кадры анимации
    FAnimation.FrameList := FData.Frames;
  end;

  //Размеры
  if deaucsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    //Подготовить буфер
    FVertexBuffer.QuadCount := FData.Frames.Count;
    for i := 0 to FData.Frames.Count - 1 do
      FVertexBuffer.Quad[i] := sgeGetFloatRect(0, 0, FData.Size.X, FData.Size.Y);

    //Залить данные в OpenGL
    FVertexBuffer.UpdateOpenGLData;
  end;

  //Масштаб
  if deaucsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deaucsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deaucsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deaucsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;
end;


procedure TsgeGraphicOpenGLDrawObjectAnimationUnamnaged.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
const
  ANIMATION_FRAME_VERTEX_COUNT = 6;
begin
  //Поправить кадр анимации
  FAnimation.IsFrameChanged;

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
    FVAO.VertexType,                                              //Тип вершин
    FAnimation.CurrentFrameIndex * ANIMATION_FRAME_VERTEX_COUNT,  //Начальный номер вершин
    ANIMATION_FRAME_VERTEX_COUNT                                  //Количество вершин
  );

  //Отвязать спрайт
  FGLSprite.Detach;
end;



end.

