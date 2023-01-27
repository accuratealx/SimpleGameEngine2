{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemRect.pas
Версия            1.0
Создан            28.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeDisplayElementItemRect,
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes, sgeGraphicOpenGLShaderProgram,
  sgeGraphicOpenGLDrawObjectItemBase;

type
  TsgeGraphicOpenGLDrawObjectItemRect = class(TsgeGraphicOpenGLDrawObjectItemBase)
  private
    FRect: TsgeDisplayElementItemRect;

  public
    constructor Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram; Rect: TsgeDisplayElementItemRect);
    destructor  Destroy; override;

    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); override;
  end;


implementation

uses
  sgeErrors,
  sgeGraphicBuffer;

const
  _UNITNAME = 'GraphicOpenGLDrawObjectItemRect';

  Err_EmptyRect = 'EmptyRect';


constructor TsgeGraphicOpenGLDrawObjectItemRect.Create(ShaderProgram: TsgeGraphicOpenGLShaderProgram; Rect: TsgeDisplayElementItemRect);
var
  Buff: TsgeGraphicBuffer;
begin
  if Rect = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyRect);

  //Запомнить объект
  FRect := Rect;

  inherited Create(ShaderProgram);

  //Сохранить положение
  FPosition.X := Rect.X;
  FPosition.Y := Rect.Y;

  //Создать буфер c координатами
  Buff := TsgeGraphicBuffer.Create;
  Buff.AddQuad(0, 0, Rect.Width, Rect.Height);

  //Залить данные в видеокарту
  FVertexBuffer.SetData(Buff);

  //Удалить промежуточный буфер
  Buff.Free;
end;


destructor TsgeGraphicOpenGLDrawObjectItemRect.Destroy;
begin
  inherited Destroy;
end;


procedure TsgeGraphicOpenGLDrawObjectItemRect.Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo);
begin
  //Отключить смешивание цветов
  if not FRect.Transparent then
    Graphic.Disable(gcColorBlend);

  //Выбрать объект
  FVAO.Attach;

  //Активировать программу
  FShaderProgram.Attach;

  //Передать параметры в программу
  FShaderProgram.SetScreenSize(ScreenSize);
  FShaderProgram.SetLayer(LayerInfo);
  FShaderProgram.SetPos(FPosition);
  FShaderProgram.SetColor(FRect.Color);

  //Нарисовать
  FVAO.DrawArray;

  //Включить смешивание цветов
  if not FRect.Transparent then
    Graphic.Enable(gcColorBlend);
end;



end.

