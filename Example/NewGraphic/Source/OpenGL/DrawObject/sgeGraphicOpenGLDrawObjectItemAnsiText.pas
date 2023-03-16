{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemAnsiText.pas
Версия            1.0
Создан            28.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Текст
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemAnsiText;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemBase,
  sgeGraphicOpenGL, sgeGraphicOpenGLSprite, sgeGraphicOpenGLDrawObjectItemBase, sgeGraphicOpenGLVertexArrayObject,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLBuffer;

type
  TsgeGraphicOpenGLDrawObjectItemAnsiText = class(TsgeGraphicOpenGLDrawObjectItemBase)
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
  sgeAnsiFontGlyph,
  sgeDisplayElementItemAnsiText,
  sgeGraphicOpenGLShaderProgramTable, sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;


constructor TsgeGraphicOpenGLDrawObjectItemAnsiText.Create(Element: TsgeDisplayElementItemBase);
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


destructor TsgeGraphicOpenGLDrawObjectItemAnsiText.Destroy;
begin
  //Удалить спрайт из таблицы
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemAnsiText(FElement).Font.Sprite);

  //Удалить буфер вершинных координат
  FTextureBuffer.Free;

  //Удалить буфер вершин
  FVertexBuffer.Free;

  //Удалить объект настройки вывода
  FVAO.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemAnsiText.Update(AElement: TsgeDisplayElementItemBase);
var
  VertexBuff, TexBuff: TsgeGraphicOpenGLCoordBuffer;
  Element: TsgeDisplayElementItemAnsiText absolute AElement;
  c, i: Integer;
  Glyph: TsgeAnsiFontGlyph;
  X, X1, Y1, X2, Y2: Single;
begin
  //Удалить старый спрайт, если происходит обновление элемента
  if Assigned(FElement) then
    OpenGLSpriteTable.Delete(TsgeDisplayElementItemAnsiText(FElement).Font.Sprite);

  inherited Update(Element);

  //Найти спрайт в таблице
  FGLSprite := OpenGLSpriteTable.Add(Element.Font.Sprite);

  //Залить данные в видеокарту
  VertexBuff := TsgeGraphicOpenGLCoordBuffer.Create;
  TexBuff := TsgeGraphicOpenGLCoordBuffer.Create;

  //Подготовить данные
  X := 0;
  c := Length(Element.TextBytes) - 1;
  for i := 0 to c do
  begin
    //Ссылка на глиф
    Glyph := Element.Font.GlyphList[Element.TextBytes[i]];

    //Вершины
    X1 := X;
    X2 := X1 + Glyph.Width;
    Y1 := Element.Font.Height - Element.Font.BaseLine - Glyph.Height - Glyph.BaseLine;
    Y2 := Y1 + Glyph.Height;
    VertexBuff.AddQuad(X1, Y1, X2, Y2);

    //Текстуры
    X1 := Glyph.X1 * FGLSprite.GLPixelWidth;
    Y1 := 1 - Glyph.Y1 * FGLSprite.GLPixelHeight;
    X2 := Glyph.X2 * FGLSprite.GLPixelWidth;
    Y2 := 1 - Glyph.Y2 * FGLSprite.GLPixelHeight;
    TexBuff.AddQuad(X1, Y1, X2, Y2);

    //Сместить X следующего глифа
    X := X + Glyph.Width + Element.Font.GlyphSpace;
  end;

  //Координаты вершин
  FVertexBuffer.SetData(VertexBuff);

  //Текстурные координаты
  FTextureBuffer.SetData(TexBuff);

  //Почистить память
  VertexBuff.Free;
  TexBuff.Free;
end;


procedure TsgeGraphicOpenGLDrawObjectItemAnsiText.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect);
var
  Element: TsgeDisplayElementItemAnsiText;
begin
  Element := TsgeDisplayElementItemAnsiText(FElement);

  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetScreenSize(ScreenSize);
  FShaderProgram.SetLayer(LayerInfo);

  FShaderProgram.SetPos(sgeGetFloatPoint(Element.Point.X, Element.Point.Y));
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

