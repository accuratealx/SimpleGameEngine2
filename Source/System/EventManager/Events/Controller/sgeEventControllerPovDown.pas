{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerPovDown.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Нажатие крестовины
}
{$Include Defines.inc}

unit sgeEventControllerPovDown;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventControllerPovUp;


const
  Event_ControllerPovDown = 'Controller.PovDown';


type
  TsgeEventControllerPovDown = class(TsgeEventControllerPovUp)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventControllerPovDown.GetName: ShortString;
begin
  Result := Event_ControllerPovDown;
end;


function TsgeEventControllerPovDown.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerPovDown.Create(FID, FDirection);
end;



end.

