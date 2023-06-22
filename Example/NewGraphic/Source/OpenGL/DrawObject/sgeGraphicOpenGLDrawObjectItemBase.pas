{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectItemBase.pas
Версия            1.0
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Базовый
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectItemBase;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicOpenGL,
  sgeDisplayElementItemBase;

type
  TsgeGraphicOpenGLDrawObjectItemBase = class
  protected
    FElement: TsgeDisplayElementItemBase; //Ссылка на элемент вывода (копия)

  public
    constructor Create(Element: TsgeDisplayElementItemBase); virtual;

    procedure Update(Element: TsgeDisplayElementItemBase); virtual;
    procedure Draw(Graphic: TsgeGraphicOpenGL; ScreenSize: TsgeFloatPoint; LayerInfo: TsgeFloatRect); virtual; abstract;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'GraphicOpenGLDrawObjectItemBase';

  Err_EmptyElement = 'EmptyElement';


constructor TsgeGraphicOpenGLDrawObjectItemBase.Create(Element: TsgeDisplayElementItemBase);
begin
  Update(Element);
end;


procedure TsgeGraphicOpenGLDrawObjectItemBase.Update(Element: TsgeDisplayElementItemBase);
begin
  //Проверить объект
  if Element = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyElement);

  { #todo : Раскомментировать }
  //Удалить старый элемент
  //if FElement <> nil then
  //  sgeFreeAndNil(FElement);

  //Запонить элемент
  FElement := Element;
end;



end.

