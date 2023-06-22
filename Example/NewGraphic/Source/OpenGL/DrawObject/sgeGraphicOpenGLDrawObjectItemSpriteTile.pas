{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSpriteTile.pas
Версия            1.1
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
  sgeDisplayElementItemBase, sgeDisplayElementItemSpriteTile,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectItemSpriteTile = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FData: TsgeDisplayElementItemSptiteTileData;
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
  sgeGraphicOpenGLUtils, sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable,
  sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemSpriteTile.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'SpriteTile';
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
  if Assigned(FData.Sprite) then
    OpenGLSpriteTable.Delete(FData.Sprite);

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
  Rect: TsgeFloatRect;
begin
  //Положение
  if deistcsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if deistcsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Buff.AddQuad(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deistcsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deistcsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deistcsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deistcsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Спрайт
  if deistcsSprite in Element.ChangeSet then
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

  //Координаты спрайта
  if deistcsTile in Element.ChangeSet then
  begin
    FData.Column := Element.Data.Column;
    FData.Row := Element.Data.Row;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Rect := sgeGetTextureTileRect(FGLSprite.GLTileWidth, FGLSprite.GLTileHeight, FData.Column, FData.Row);
    Buff.AddQuad(Rect);
    FTextureBuffer.SetData(Buff);
    Buff.Free;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSpriteTile.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
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

