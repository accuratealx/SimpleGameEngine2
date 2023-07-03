{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectFadeItemList.pas
Версия            1.0
Создан            13.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Затемнение экрана: Список элементов затемнения
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectFadeItemList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTemplateList,
  sgeGraphicOpenGLDrawObjectFadeItem;

type
  TsgeGraphicOpenGLDrawObjectFadeItemList = class(specialize TsgeTemplateList<TsgeGraphicOpenGLDrawObjectFadeItem>)
  end;

implementation



end.

