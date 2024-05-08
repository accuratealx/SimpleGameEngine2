{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerAttach.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Подключение
}
{$Include Defines.inc}

unit sgeEventControllerAttach;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventController;


const
  Event_ControllerAttach = 'Controller.Attach';


type
  TsgeEventControllerAttach = class(TsgeEventController)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventControllerAttach.GetName: ShortString;
begin
  Result := Event_ControllerAttach;
end;


function TsgeEventControllerAttach.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAttach.Create(FID);
end;



end.

