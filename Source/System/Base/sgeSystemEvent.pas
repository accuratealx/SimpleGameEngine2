{
Пакет             Simple Game Engine 2
Файл              sgeSystemEvent.pas
Версия            1.1
Создан            27.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системного события
}
{$Include Defines.inc}

unit sgeSystemEvent;

{$mode objfpc}{$H+}

interface


type
  TsgeSystemEvent = class
  private
    FHandle: THandle;
  public
    constructor Create(AutoReset: Boolean = True; InitialState: Boolean = False);
    destructor  Destroy; override;

    procedure Up;         //Поднять флаг
    procedure Down;       //Опустить флаг

    procedure Wait;       //Ожидание изменения состояния
  end;


implementation

uses
  sgeErrors, sgeOSPlatform;


const
  _UNITNAME = 'SystemEvent';

  Err_CantCreateSystemEvent = 'CantCreateSystemEvent';


constructor TsgeSystemEvent.Create(AutoReset: Boolean; InitialState: Boolean);
begin
  FHandle := sgeCreateEvent(not AutoReset, InitialState);

  if FHandle = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateSystemEvent);
end;


destructor TsgeSystemEvent.Destroy;
begin
  sgeCloseEvent(FHandle);
end;


procedure TsgeSystemEvent.Up;
begin
  sgeSetEvent(FHandle);
end;


procedure TsgeSystemEvent.Down;
begin
  sgeResetEvent(FHandle);
end;


procedure TsgeSystemEvent.Wait;
begin
  sgeWaitForSingleObject(FHandle, INFINITE);
end;


end.

