{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectSpritePart.pas
Версия            1.4
Создан            15.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Часть спрайта
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectSpritePart;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementSpritePart,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectSpritePart = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementSpritePart.TData;
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
  sgeGraphicOpenGLUtils, sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable;


constructor TsgeGraphicOpenGLDrawObjectSpritePart.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'SpritePart';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create(1);

  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create(1);

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


destructor TsgeGraphicOpenGLDrawObjectSpritePart.Destroy;
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


procedure TsgeGraphicOpenGLDrawObjectSpritePart.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementSpritePart absolute AElement;
  Rect: TsgeFloatRect;
begin
  //Положение
  if csPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Отражение
  if csReflect in Element.ChangeSet then
    FData.Reflect := Element.Data.Reflect;

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

  //Спрайт
  if csSprite in Element.ChangeSet then
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

  //Координаты спрайта
  if (csSpriteRect in Element.ChangeSet) or (csReflect in Element.ChangeSet) then
  begin
    FData.SpriteRect := Element.Data.SpriteRect;

    Rect := sgeGetTextureRect(FGLSprite.GLPixelWidth, FGLSprite.GLPixelHeight, FData.SpriteRect);

    //Проверить на отражение
    if FData.Reflect <> [] then
      Rect := sgeGetReflectRect(Rect, FData.Reflect);

    //Заполнить буфер
    FTextureBuffer.Quad[0] := Rect;

    //Залить данные в OpenGL
    FTextureBuffer.UpdateOpenGLData;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectSpritePart.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
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

