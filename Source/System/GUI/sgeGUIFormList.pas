{
Пакет             Simple Game Engine 2
Файл              sgeGUIFormList.pas
Версия            1.0
Создан            04.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Список форм
}
{$Include Defines.inc}

unit sgeGUIFormList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateObjectCollection, sgeGUIForm;


type
  TsgeGUIFormListTemplate = specialize TsgeTemplateObjectCollection<TsgeGUIForm>;


  TsgeGUIFormList = class(TsgeGUIFormListTemplate)
  private
  public
  end;


implementation

end.

