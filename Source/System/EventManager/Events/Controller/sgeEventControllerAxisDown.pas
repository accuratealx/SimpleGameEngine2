{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerAxisDown.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Опускание оси
}
{$Include Defines.inc}

unit sgeEventControllerAxisDown;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventControllerAxisUp;


const
  Event_ControllerAxisDown = 'Controller.AxisDown';


type
  TsgeEventControllerAxisDown = class(TsgeEventControllerAxisUp)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventControllerAxisDown.GetName: ShortString;
begin
  Result := Event_ControllerAxisDown;
end;


function TsgeEventControllerAxisDown.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerAxisDown.Create(FID, FAxis, FTilt);
end;



end.

