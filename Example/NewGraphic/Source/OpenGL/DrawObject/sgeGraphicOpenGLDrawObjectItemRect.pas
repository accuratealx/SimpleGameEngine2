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
  sgeGraphicOpenGLDrawObjectItemSimple;

type
  TsgeGraphicOpenGLDrawObjectItemRect = class(TsgeGraphicOpenGLDrawObjectItemSimple)
  protected
    function  GetShaderProgramName: String; override;
  end;


implementation


function TsgeGraphicOpenGLDrawObjectItemRect.GetShaderProgramName: String;
begin
  Result := 'Rect';
end;



end.

