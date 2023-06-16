{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemAnimation.pas
Версия            1.0
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemAnimation;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectItemAnimation = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
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
  sgeGraphicOpenGLUtils,
  sgeDisplayElementItemAnimation,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemAnimation.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'Sprite';
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


destructor TsgeGraphicOpenGLDrawObjectItemAnimation.Destroy;
begin
  //Удалить спрайт из таблицы
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemAnimation(FElement).Sprite);

  //Удалить буфер вершинных координат
  FTextureBuffer.Free;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemAnimation.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemAnimation absolute AElement;
  Rect: TsgeFloatRect;
  i: Integer;
begin
  //Удалить старый спрайт, если происходит обновление элемента
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemAnimation(FElement).Sprite);

  inherited Update(Element);

  //Найти спрайт в таблице
  FGLSprite := OpenGLSpriteTable.Add(Element.Sprite);

  //Залить данные в видеокарту
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;

  //Вершины каждого кадра
  for i := 0 to Element.Animation.FrameList.Count - 1 do
    Buff.AddQuad(0, 0, Element.Rect.Width, Element.Rect.Height);
  FVertexBuffer.SetData(Buff);

  //Текстурные координаты
  Buff.Clear;
  for i := 0 to Element.Animation.FrameList.Count - 1 do
  begin
    Rect := sgeGetTextureTileRect(
      FGLSprite.GLTileWidth,
      FGLSprite.GLTileHeight,
      Element.Animation.FrameList.Item[i].Column,
      Element.Animation.FrameList.Item[i].Row
    );
    Buff.AddQuad(Rect);
  end;
  FTextureBuffer.SetData(Buff);

  //Удалить временный буфер
  Buff.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemAnimation.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
const
  ANIMATION_FRAME_VERTEX_COUNT = 6;
var
  Element: TsgeDisplayElementItemAnimation;
begin
  Element := TsgeDisplayElementItemAnimation(FElement);

  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetScreenSize(ScreenSize);
  FShaderProgram.SetLayer(LayerInfo);

  FShaderProgram.SetPos(sgeGetFloatPoint(Element.Rect.X1, Element.Rect.Y1));
  FShaderProgram.SetColor(Element.Color.Color);
  FShaderProgram.SetScale(Element.Scale.Scale);
  FShaderProgram.SetOrigin(Element.Origin.Point);
  FShaderProgram.SetAngle(Element.Rotate.Angle);

  //Привязать спрайт
  FGLSprite.Attach;

  //Нарисовать 6 вершин нужного кадра
  Graphic.DrawArray(
    FVAO.VertexType,
    Element.Animation.CurrentFrameIndex * ANIMATION_FRAME_VERTEX_COUNT,
    ANIMATION_FRAME_VERTEX_COUNT
  );

  //Отвязать спрайт
  FGLSprite.Detach;
end;



end.

