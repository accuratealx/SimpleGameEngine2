{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerDetach.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Отключение
}
{$Include Defines.inc}

unit sgeEventControllerDetach;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventController;


const
  Event_ControllerDetach = 'Controller.Detach';


type
  TsgeEventControllerDetach = class(TsgeEventController)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventControllerDetach.GetName: ShortString;
begin
  Result := Event_ControllerDetach;
end;


function TsgeEventControllerDetach.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerDetach.Create(FID);
end;



end.

