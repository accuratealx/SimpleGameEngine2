{
Пакет             Simple Game Engine 2
Файл              sgeEventList.pas
Версия            1.2
Создан            20.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список объектов событий
}
{$Include Defines.inc}

unit sgeEventList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeList, sgeEventBase;


type
  TsgeEventList = class(specialize TsgeTemplateThreadSafeList<TsgeEventBase>);


implementation



end.

