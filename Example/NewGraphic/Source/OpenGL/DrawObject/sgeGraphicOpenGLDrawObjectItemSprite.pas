{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSprite.pas
Версия            1.0
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Спрайт
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemSprite,
  sgeGraphicOpenGL, sgeGraphicOpenGLBuffer, sgeGraphicOpenGLTypes, sgeGraphicOpenGLSprite,
  sgeGraphicOpenGLShaderProgram, sgeGraphicOpenGLDrawObjectItemBase;


type
  TsgeGraphicOpenGLDrawObjectItemSprite = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FSprite: TsgeDisplayElementItemSprite;
    FGLSprite: TsgeGraphicOpenGLSprite;
    FTextureBuffer: TsgeGraphicOpenGLBuffer;

  public
    constructor Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram; Sprite: TsgeDisplayElementItemSprite);
    destructor  Destroy; override;

    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); override;
  end;


implementation

uses
  dglOpenGL,
  sgeErrors,
  sgeGraphicOpenGLSpriteTable, sgeGraphicOpenGLCoordBuffer;

const
  _UNITNAME = 'GraphicOpenGLDrawObjectItemSprite';

  Err_EmptySprite = 'EmptySprite';


constructor TsgeGraphicOpenGLDrawObjectItemSprite.Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram; Sprite: TsgeDisplayElementItemSprite);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  w, h: GLfloat;
begin
  if Sprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  //Запомнить объект
  FSprite := Sprite;

  inherited Create(ShaderProgram);

  //Сохранить положение
  FPosition.X := FSprite.X;
  FPosition.Y := FSprite.Y;

  //Создать промежуточный буфер
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;

  //Проверить вывод по центру
  if FSprite.Centered then
  begin
    w := FSprite.Width / 2;
    h := FSprite.Height / 2;
    Buff.AddQuad(-w, -h, w, h);
  end
  else
    Buff.AddQuad(0, 0, FSprite.Width, FSprite.Height);

  //Залить данные координат в видеокарту
  FVertexBuffer.SetData(Buff);

  //Подготовить буфер с текстурными координатами
  FTextureBuffer := TsgeGraphicOpenGLBuffer.Create;
  FTextureBuffer.Attach;
  FVAO.BindTextureCoord(FTextureBuffer);

  //Задать текстурные координаты
  Buff.Clear;
  Buff.AddQuad(0, 1, 1, 0);

  //Залить данные текстурных координат в видеокарту
  FTextureBuffer.SetData(Buff);

  //Удалить промежуточный буфер
  Buff.Free;

  //Найти спрайт в таблице
  FGLSprite := OpenGLSpriteTable.Add(FSprite.Sprite);
end;


destructor TsgeGraphicOpenGLDrawObjectItemSprite.Destroy;
begin
  OpenGLSpriteTable.Delete(FSprite.Sprite);

  FTextureBuffer.Free;

  inherited Destroy;
end;


procedure TsgeGraphicOpenGLDrawObjectItemSprite.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
begin

end;



end.

