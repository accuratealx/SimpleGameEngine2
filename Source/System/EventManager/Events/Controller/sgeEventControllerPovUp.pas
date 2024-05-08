{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerPovUp.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Поднятие крестовины
}
{$Include Defines.inc}

unit sgeEventControllerPovUp;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventController;


const
  Event_ControllerPovUp = 'Controller.PovUp';


type
  TsgeEventControllerPovUp = class(TsgeEventController)
  protected
    FDirection: TsgeControllerPovDirection;
    function GetName: ShortString; override;
  public
    constructor Create(ID: Byte; Direction: TsgeControllerPovDirection);

    function Copy: TsgeEventBase; override;

    property Direction: TsgeControllerPovDirection read FDirection;
  end;


implementation


function TsgeEventControllerPovUp.GetName: ShortString;
begin
  Result := Event_ControllerPovUp;
end;


constructor TsgeEventControllerPovUp.Create(ID: Byte; Direction: TsgeControllerPovDirection);
begin
  inherited Create(ID);
  FDirection := Direction;
end;


function TsgeEventControllerPovUp.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerPovUp.Create(FID, FDirection);
end;



end.

