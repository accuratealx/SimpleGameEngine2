{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectSpriteNine.pas
Версия            1.3
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Спрайт 9
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectSpriteNine;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementSpriteNine,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;


type
  TsgeGraphicOpenGLDrawObjectSpriteNine = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementSptiteNineData;
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
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable;


constructor TsgeGraphicOpenGLDrawObjectSpriteNine.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'SpriteNine';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create(9);

  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create(9);

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


destructor TsgeGraphicOpenGLDrawObjectSpriteNine.Destroy;
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


procedure TsgeGraphicOpenGLDrawObjectSpriteNine.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementSpriteNine absolute AElement;
  X1, Y1, X2, Y2: Single;
begin
  //Положение
  if desncsPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

  //Спрайт
  if desncsSprite in Element.ChangeSet then
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
  if (desncsOffset in Element.ChangeSet) then
  begin
    FData.Offset := Element.Data.Offset;

    //Проверить на отражение
    {if FData.Reflect <> [] then
      Rect := sgeGetReflectRect(Rect, FData.Reflect);}


    //Перевести смещение в координаты OpenGL
    X1 := FData.Offset.X1 * FGLSprite.GLPixelWidth;
    Y1 := FData.Offset.Y1 * FGLSprite.GLPixelHeight;
    X2 := FData.Offset.X2 * FGLSprite.GLPixelWidth;
    Y2 := FData.Offset.Y2 * FGLSprite.GLPixelHeight;

    //Подготовить буфер
    FTextureBuffer.Quad[0] := sgeGetFloatRect(0, 1, X1, 1 - Y1);
    FTextureBuffer.Quad[1] := sgeGetFloatRect(X1, 1, 1 - X2, 1 - Y1);
    FTextureBuffer.Quad[2] := sgeGetFloatRect(1 - X2, 1, 1, 1 - Y1);
    FTextureBuffer.Quad[3] := sgeGetFloatRect(0, 1 - Y1, X1, Y2);
    FTextureBuffer.Quad[4] := sgeGetFloatRect(X1, 1 - Y1, 1 - X2, Y2);
    FTextureBuffer.Quad[5] := sgeGetFloatRect(1 - X2, 1 - Y1, 1, Y2);
    FTextureBuffer.Quad[6] := sgeGetFloatRect(0, Y2, X1, 0);
    FTextureBuffer.Quad[7] := sgeGetFloatRect(X1, Y2, 1 - X2, 0);
    FTextureBuffer.Quad[8] := sgeGetFloatRect(1 - X2, Y2, 1, 0);

    //Залить данные в OpenGL
    FTextureBuffer.UpdateOpenGLData;
  end;

  //Размеры
  if desncsSize in Element.ChangeSet then
  begin
    FData.Size := Element.Data.Size;

    //Перевести смещение в координаты OpenGL
    X1 := FData.Offset.X1;
    Y1 := FData.Offset.Y1;
    X2 := FData.Offset.X2;
    Y2 := FData.Offset.Y2;

    //Подготовить буфер
    FVertexBuffer.Quad[0] := sgeGetFloatRect(0, 0, X1, Y1);
    FVertexBuffer.Quad[1] := sgeGetFloatRect(X1, 0, FData.Size.X - X2,Y1);
    FVertexBuffer.Quad[2] := sgeGetFloatRect(FData.Size.X - X2, 0, FData.Size.X, Y1);
    FVertexBuffer.Quad[3] := sgeGetFloatRect(0, Y1, X1, FData.Size.Y - Y2);
    FVertexBuffer.Quad[4] := sgeGetFloatRect(X1, Y1, FData.Size.X - X2, FData.Size.Y - Y2);
    FVertexBuffer.Quad[5] := sgeGetFloatRect(FData.Size.X - X2, Y1, FData.Size.X, FData.Size.Y - Y2);
    FVertexBuffer.Quad[6] := sgeGetFloatRect(0, FData.Size.Y - Y2, X1, FData.Size.Y);
    FVertexBuffer.Quad[7] := sgeGetFloatRect(X1, FData.Size.Y - Y2, FData.Size.X - X2, FData.Size.Y);
    FVertexBuffer.Quad[8] := sgeGetFloatRect(FData.Size.X - X2, FData.Size.Y - Y2, FData.Size.X, FData.Size.Y);

    //Залить данные в OpenGL
    FVertexBuffer.UpdateOpenGLData;
  end;

  //Масштаб
  if desncsScale in Element.ChangeSet then
    FData.Scale := Element.Data.Scale;

  //Точка поворота
  if desncsOrigin in Element.ChangeSet then
    FData.Origin := Element.Data.Origin;

  //Угол
  if desncsAngle in Element.ChangeSet then
    FData.Angle := Element.Data.Angle;

  //Цвет
  if desncsColor in Element.ChangeSet then
    FData.Color := Element.Data.Color;
end;


procedure TsgeGraphicOpenGLDrawObjectSpriteNine.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
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

