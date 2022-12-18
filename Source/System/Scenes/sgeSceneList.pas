{
Пакет             Simple Game Engine 2
Файл              sgeSceneList.pas
Версия            1.0
Создан            17.12.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список сцен
}
{$Include Defines.inc}

unit sgeSceneList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTemplateThreadSafeList,
  sgeSceneBase;

type
  TsgeSceneList = class(specialize TsgeTemplateThreadSafeList<TsgeSceneBase>);


implementation


end.

