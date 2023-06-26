{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObject.pas
Версия            1.2
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Базовый
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObject;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicOpenGL,
  sgeDisplayElement;

type
  TsgeGraphicOpenGLDrawObject = class
  public
    constructor Create(Element: TsgeDisplayElement); virtual;

    procedure Update(Element: TsgeDisplayElement); virtual; abstract;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect); virtual; abstract;
  end;


implementation


constructor TsgeGraphicOpenGLDrawObject.Create(Element: TsgeDisplayElement);
begin
  Update(Element);
end;



end.

