{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemList.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список элементов рисования
}
{$Include Defines.inc}

unit sgeDisplayElementItemList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTemplateList,
  sgeDisplayElementItemBase;

type
  TsgeDisplayElementItemList = class(specialize TsgeTemplateList<TsgeDisplayElementItemBase>)
  end;


implementation

end.

