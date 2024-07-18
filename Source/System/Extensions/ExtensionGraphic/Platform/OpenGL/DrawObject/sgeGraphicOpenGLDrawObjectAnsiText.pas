{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectAnsiText.pas
Версия            1.4
Создан            28.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Текст
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectAnsiText;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElement, sgeDisplayElementAnsiText,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLDrawObject, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLSprite;

type
  TsgeGraphicOpenGLDrawObjectAnsiText = class(TsgeGraphicOpenGLDrawObject)
  private
    FData: TsgeDisplayElementAnsiText.TData;
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
  sgeAnsiFontGlyph,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable;


constructor TsgeGraphicOpenGLDrawObjectAnsiText.Create(Element: TsgeDisplayElement);
const
  SHADER_NAME = 'Text';
begin
  //Найти шейдерную программу в таблице
  FShaderProgram := OpenGLShaderProgramTable.Get(SHADER_NAME);

  //Создать вершинный буфер
  FVertexBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create;

  //Создать VAO
  FVAO := TsgeGraphicOpenGLVertexArrayObject.Create;

  //Привязать буфер вершин к VAO
  FVAO.Attach;
  FVertexBuffer.Attach;
  FVAO.BindVertexCoord(FVertexBuffer);

  //Привязать буфер координат
  FTextureBuffer.Attach;
  FVAO.BindTextureCoord(FTextureBuffer);

  //Родительский конструктор
  inherited Create(Element);
end;


destructor TsgeGraphicOpenGLDrawObjectAnsiText.Destroy;
begin
  //Удалить спрайт из таблицы
  if Assigned(FData.Font.Sprite) then
    OpenGLSpriteTable.Delete(FData.Font.Sprite);

  //Удалить буфер вершинных координат
  FTextureBuffer.Free;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectAnsiText.Update(AElement: TsgeDisplayElement);
var
  Element: TsgeDisplayElementAnsiText absolute AElement;
  c, i: Integer;
  Glyph: TsgeAnsiFontGlyph;
  X, X1, Y1, X2, Y2: Single;
begin
  //Положение
  if csPosition in Element.ChangeSet then
    FData.Position := Element.Data.Position;

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

  //Текст
  if csText in Element.ChangeSet then
    FData.TextBytes := Element.Data.TextBytes;

  //Шрифт
  if csFont in Element.ChangeSet then
  begin
    if FData.Font <> Element.Data.Font then
    begin
      //Удалить старый спрайт
      if (FData.Font <> nil) and (FData.Font.Sprite <> nil) then
        OpenGLSpriteTable.Delete(FData.Font.Sprite);

      //Найти новый спрайт
      FData.Font := Element.Data.Font;
      FGLSprite := OpenGLSpriteTable.Add(FData.Font.Sprite);
    end;
  end;

  //Шрифт или Текст
  if (csFont in Element.ChangeSet) or ((csText in Element.ChangeSet)) then
  begin
    //Подготовить данные
    X := 0;
    c := Length(FData.TextBytes) - 1;

    FVertexBuffer.QuadCount := c + 1;
    FTextureBuffer.QuadCount := c + 1;

    for i := 0 to c do
    begin
      //Ссылка на глиф
      Glyph := Element.Font.GlyphList[FData.TextBytes[i]];

      //Вершины
      X1 := X;
      X2 := X1 + Glyph.Width;
      Y1 := Element.Font.Height - Element.Font.BaseLine - Glyph.Height - Glyph.BaseLine;
      Y2 := Y1 + Glyph.Height;
      FVertexBuffer.Quad[i] := sgeGetFloatRect(X1, Y1, X2, Y2);

      //Текстуры
      X1 := Glyph.X1 * FGLSprite.GLPixelWidth;
      Y1 := 1 - Glyph.Y1 * FGLSprite.GLPixelHeight;
      X2 := Glyph.X2 * FGLSprite.GLPixelWidth;
      Y2 := 1 - Glyph.Y2 * FGLSprite.GLPixelHeight;
      FTextureBuffer.Quad[i] := sgeGetFloatRect(X1, Y1, X2, Y2);

      //Сместить X следующего глифа
      X := X + Glyph.Width + Element.Font.GlyphSpace;
    end;

    //Залить данные в OpenGL
    FVertexBuffer.UpdateOpenGLData;
    FTextureBuffer.UpdateOpenGLData;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectAnsiText.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
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

