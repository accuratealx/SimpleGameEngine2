{
Пакет             Simple Game Engine 2
Файл              sgeCorePointerUtils.pas
Версия            1.0
Создан            28.06.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции CorePointerList с зависимостями SGE
}
{$Include Defines.inc}

unit sgeCorePointerUtils;

{$mode ObjFPC}{$H+}

interface

uses
  sgeErrorManager, sgeEventManager,
  SimpleGameEngine;


function sgeCorePointer_GetSGE: TSimpleGameEngine;
function sgeCorePointer_GetErrorManager: TsgeErrorManager;
function sgeCorePointer_GetEventManager: TsgeEventManager;


implementation

uses
  sgeCorePointerList;

var
  FSGE: TSimpleGameEngine;
  FErrorManager: TsgeErrorManager;
  FEventManager: TsgeEventManager;


function sgeCorePointer_GetSGE: TSimpleGameEngine;
begin
  if FSGE = nil then
    FSGE := TSimpleGameEngine(CorePointerList.GetObject(Object_SGE, ItemType_SGE));
  Result := FSGE;
end;


function sgeCorePointer_GetErrorManager: TsgeErrorManager;
begin
  if FErrorManager = nil then
    FErrorManager := TsgeErrorManager(CorePointerList.GetObject(Object_ErrorManager, ItemType_SGEErrorManager));
  Result := FErrorManager;
end;


function sgeCorePointer_GetEventManager: TsgeEventManager;
begin
  if FEventManager = nil then
    FEventManager := TsgeEventManager(CorePointerList.GetObject(Object_EventManager, ItemType_SGEEventManager));
  Result := FEventManager;
end;



end.

