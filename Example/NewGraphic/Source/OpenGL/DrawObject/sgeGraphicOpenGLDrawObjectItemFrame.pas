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
  sgeGraphicOpenGL, sgeGraphicOpenGLVertexArrayObject, sgeGraphicOpenGLDrawObjectItemSimple;

type
  TsgeGraphicOpenGLDrawObjectItemFrame = class(TsgeGraphicOpenGLDrawObjectItemSimple)
  protected
    function  GetShaderProgramName: String; override;
    function  GetVertexArrayObjectType: TsgeGraphicOpenGLVertexType; override;
    procedure UserDrawBegin(Graphic: TsgeGraphicOpenGL); override;
    procedure UserDrawEnd(Graphic: TsgeGraphicOpenGL); override;
  public

  end;

implementation

uses
  sgeDisplayElementItemFrame;


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

  //Настроить штриховку
  Graphic.Enable(gcLineStipple);
  Graphic.SetLineStipple(ElementFrame.StippleScale, ElementFrame.LineStipple);

  //Задать цвет для шейдера
  FShaderProgram.SetColor(ElementFrame.Color);
end;


procedure TsgeGraphicOpenGLDrawObjectItemFrame.UserDrawEnd(Graphic: TsgeGraphicOpenGL);
begin
  //Отключить штриховку
  Graphic.Disable(gcLineStipple);
end;



end.

