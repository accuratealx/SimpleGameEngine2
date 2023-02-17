{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemFrame.pas
Версия            1.0
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Цветная рамка
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemFrame;

{$mode ObjFPC}{$H+}

interface

uses
  sgeDisplayElementItemBase,
  sgeGraphicOpenGL, sgeGraphicOpenGLVertexArrayObject, sgeGraphicOpenGLDrawObjectItemSimple;

type
  TsgeGraphicOpenGLDrawObjectItemFrame = class(TsgeGraphicOpenGLDrawObjectItemSimple)
  protected
    function  GetShaderProgramName: String; override;
    function  GetVertexArrayObjectType: TsgeGraphicOpenGLVertexType; override;
    procedure UserDrawBegin(Graphic: TsgeGraphicOpenGL); override;
    procedure UpdateVertexBuffer(AElement: TsgeDisplayElementItemBase); override;
  end;

implementation

uses
  dglOpenGL,
  sgeDisplayElementItemFrame,
  sgeGraphicOpenGLCoordBuffer;


function TsgeGraphicOpenGLDrawObjectItemFrame.GetShaderProgramName: String;
begin
  Result := 'Frame';
end;


function TsgeGraphicOpenGLDrawObjectItemFrame.GetVertexArrayObjectType: TsgeGraphicOpenGLVertexType;
begin
  Result := vtLineLoop;
end;


procedure TsgeGraphicOpenGLDrawObjectItemFrame.UserDrawBegin(Graphic: TsgeGraphicOpenGL);
var
  ElementFrame: TsgeDisplayElementItemFrame;
begin
  //Ссылка на елемент
  ElementFrame := FElement as TsgeDisplayElementItemFrame;

  //Настроить толщину линии
  Graphic.SetLineWidth(ElementFrame.LineWidth);

  //Задать цвет для шейдера
  FShaderProgram.SetColor(ElementFrame.Color);
end;


procedure TsgeGraphicOpenGLDrawObjectItemFrame.UpdateVertexBuffer(AElement: TsgeDisplayElementItemBase);
var
  Buff: TsgeGraphicOpenGLCoordBuffer;
  w, h: GLfloat;
begin
  //Создать буфер c координатами
  Buff := TsgeGraphicOpenGLCoordBuffer.Create;
  if AElement.Centered then
  begin
    w := AElement.Width / 2;
    h := AElement.Height / 2;
    Buff.AddLineRect(-w, -h, w, h);
  end
  else
    Buff.AddLineRect(0, 0, AElement.Width, AElement.Height);

  //Залить данные в видеокарту
  FVertexBuffer.SetData(Buff);

  //Удалить промежуточный буфер
  Buff.Free;
end;



end.

