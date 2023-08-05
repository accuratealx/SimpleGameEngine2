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
  sgeGraphicOpenGL, sgeGraphicOpenGLTypes,
  sgeDisplayElement;

type
  TsgeGraphicOpenGLDrawObject = class
  private
    FVisible: Boolean;        //Видимость
    FClipped: Boolean;        //Режим обрезания :)
    FClipRect: TsgeClipRect;  //Прямоугольник ограничивающий вывод
  public
    constructor Create(Element: TsgeDisplayElement); virtual;

    procedure Update(Element: TsgeDisplayElement); virtual; abstract;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeLayerInfo); virtual; abstract;

    property Visible: Boolean read FVisible write FVisible;
    property Clipped: Boolean read FClipped write FClipped;
    property ClipRect: TsgeClipRect read FClipRect write FClipRect;
  end;


implementation


constructor TsgeGraphicOpenGLDrawObject.Create(Element: TsgeDisplayElement);
begin
  FVisible := True;
  FClipped := False;

  Update(Element);
end;



end.

