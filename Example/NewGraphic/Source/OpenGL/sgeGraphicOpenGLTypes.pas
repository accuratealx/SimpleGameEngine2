{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLTypes.pas
Версия            1.0
Создан            23.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Типы
}
{$Include Defines.inc}

unit sgeGraphicOpenGLTypes;

{$mode ObjFPC}{$H+}

interface

uses
  dglOpenGL;

type
  TsgeLayerInfo = record
    X: GLfloat;
    Y: GLfloat;
    Scale: GLfloat;
  end;


function sgeGetLayerInfo(X, Y: GLfloat; Scale: GLfloat = 1): TsgeLayerInfo;


implementation


function sgeGetLayerInfo(X, Y: GLfloat; Scale: GLfloat): TsgeLayerInfo;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Scale := Scale;
end;



end.

