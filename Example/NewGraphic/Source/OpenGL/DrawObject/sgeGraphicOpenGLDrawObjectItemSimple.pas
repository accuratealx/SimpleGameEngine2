{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemSimple.pas
Версия            1.0
Создан            10.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Простой элемент
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemSimple;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicOpenGL,
  sgeGraphicOpenGLDrawObjectItemBase;

type
  TsgeGraphicOpenGLDrawObjectItemSimple = class(TsgeGraphicOpenGLDrawObjectItemBase)
  protected
    procedure UserDrawBegin(Graphic: TsgeGraphicOpenGL); override;
  end;


implementation

uses
  sgeDisplayElementItemSimple;


procedure TsgeGraphicOpenGLDrawObjectItemSimple.UserDrawBegin(Graphic: TsgeGraphicOpenGL);
var
  ElementSimple: TsgeDisplayElementSimple;
begin
  //Ссылка на элемент
  ElementSimple := FElement as TsgeDisplayElementSimple;

  //Задать цвет для шейдера
  FShaderProgram.SetColor(ElementSimple.Color);
end;



end.

