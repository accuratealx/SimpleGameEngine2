{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerButtonDown.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Нажатие кнопки
}
{$Include Defines.inc}

unit sgeEventControllerButtonDown;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventControllerButtonUp;


const
  Event_ControllerButtonDown = 'Controller.ButtonDown';


type
  TsgeEventControllerButtonDown = class(TsgeEventControllerButtonUp)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventControllerButtonDown.GetName: ShortString;
begin
  Result := Event_ControllerButtonDown;
end;


function TsgeEventControllerButtonDown.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerButtonDown.Create(FID, FButtonID);
end;



end.

