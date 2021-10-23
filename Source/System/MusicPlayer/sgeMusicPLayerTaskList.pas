{
Пакет             Simple Game Engine 2
Файл              sgeMusicPLayerTaskList.pas
Версия            1.0
Создан            21.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список задачь музыкального проигрывателя
}
{$Include Defines.inc}

unit sgeMusicPLayerTaskList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeList,
  sgeMusicPLayerTaskBase;

type
  TsgeMusicPLayerTaskList = class(specialize TsgeTemplateThreadSafeList<TsgeMusicPlayerTaskBase>)
  end;


implementation



end.

