{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSpritePart.pas
Версия            1.1
Создан            15.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Часть спрайта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSpritePart;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase, sgeDisplayElementItemSpritePart,
  sgeGraphicOpenGL, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectItemSpritePart = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FData: TsgeDisplayElementItemSptitePartData;
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


constructor TsgeGraphicOpenGLDrawObjectItemSpritePart.Create(Element: TsgeDisplayElementItemBase);
const
  SHADER_NAME = 'SpritePart';
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


destructor TsgeGraphicOpenGLDrawObjectItemSpritePart.Destroy;
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


procedure TsgeGraphicOpenGLDrawObjectItemSpritePart.Update(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemSpritePart absolute AElement;
  Rect: TsgeFloatRect;
begin
  //Положение
  if deispcsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Размеры
  if deispcsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Buff.AddQuad(0, 0, FData.Size.X, FData.Size.Y);
    FVertexBuffer.SetData(Buff);
    Buff.Free;
  end;

  //Масштаб
  if deispcsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if deispcsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if deispcsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if deispcsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;

  //Спрайт
  if deispcsSprite in Element.ChangeSet then
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
  if deispcsSpriteRect in Element.ChangeSet then
  begin
    FData.SpriteRect := Element.Data.SpriteRect;

    Buff := TsgeGraphicOpenGLCoordBuffer.Create;
    Rect := sgeGetTextureRect(FGLSprite.GLPixelWidth, FGLSprite.GLPixelHeight, FData.SpriteRect);
    Buff.AddQuad(Rect);
    FTextureBuffer.SetData(Buff);
    Buff.Free;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSpritePart.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
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

