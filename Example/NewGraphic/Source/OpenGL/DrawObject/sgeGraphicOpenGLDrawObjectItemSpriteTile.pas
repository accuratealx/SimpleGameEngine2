{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSpriteTile.pas
Версия            1.0
Создан            15.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Плитка спрайта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSpriteTile;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectItemSpriteTile = class(TsgeGraphicOpenGLDrawObjectItemBase)
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
  sgeDisplayElementItemSpriteTile,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemSpriteTile.Create(Element: TsgeDisplayElementItemBase);
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


destructor TsgeGraphicOpenGLDrawObjectItemSpriteTile.Destroy;
begin
  //Удалить спрайт из таблицы
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemSpriteTile(FElement).Sprite);

  //Удалить буфер вершинных координат
  FTextureBuffer.Free;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSpriteTile.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemSpriteTile absolute AElement;
  X1, Y1, X2, Y2: Single;
begin
  //Удалить старый спрайт, если происходит обновление элемента
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemSpriteTile(FElement).Sprite);

  inherited Update(Element);

  //Найти спрайт в таблице
  FGLSprite := OpenGLSpriteTable.Add(Element.Sprite);

  //Залить данные в видеокарту
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;

  //Вершины
  Buff.AddQuad(0, 0, Element.Rect.Width, Element.Rect.Height);
  FVertexBuffer.SetData(Buff);

  //Текстурные координаты
  Buff.Clear;
  X1 := Element.Tile.X * FGLSprite.GLTileWidth;
  Y1 := 1 - Element.Tile.Y * FGLSprite.GLTileHeight;
  X2 := X1 + FGLSprite.GLTileWidth;
  Y2 := Y1 - FGLSprite.GLTileHeight;
  Buff.AddQuad(X1, Y1, X2, Y2);
  FTextureBuffer.SetData(Buff);

  //Удалить временный буфер
  Buff.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSpriteTile.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
var
  Element: TsgeDisplayElementItemSpriteTile;
begin
  Element := TsgeDisplayElementItemSpriteTile(FElement);

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

  //Нарисовать
  Graphic.DrawArray(FVAO.VertexType, 0, FVAO.VertexCount);

  //Отвязать спрайт
  FGLSprite.Detach;
end;



end.

