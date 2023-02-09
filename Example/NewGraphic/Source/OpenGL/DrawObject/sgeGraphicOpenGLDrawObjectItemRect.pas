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
  sgeGraphicOpenGLDrawObjectItemBase;

type
  TsgeGraphicOpenGLDrawObjectItemRect = class(TsgeGraphicOpenGLDrawObjectItemBase)
  protected
    function  GetShaderProgramName: String; override;
    procedure UserDrawBegin; override;

  end;


implementation

uses
  sgeDisplayElementItemRect;


function TsgeGraphicOpenGLDrawObjectItemRect.GetShaderProgramName: String;
begin
  Result := 'Rect';
end;


procedure TsgeGraphicOpenGLDrawObjectItemRect.UserDrawBegin;
begin
  //Задать цвет для шейдера
  FShaderProgram.SetColor(TsgeDisplayElementItemRect(FElement).Color);
end;



end.

