{
Пакет             Simple Game Engine 2
Файл              sgeEventControllerButtonUp.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Контроллер: Отпускание кнопки
}
{$Include Defines.inc}

unit sgeEventControllerButtonUp;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase, sgeEventController;


const
  Event_ControllerButtonUp = 'Controller.ButtonUp';


type
  TsgeEventControllerButtonUp = class(TsgeEventController)
  protected
    FButtonID: Byte;
    function GetName: ShortString; override;
  public
    constructor Create(ID: Byte; ButtonID: Byte);

    function Copy: TsgeEventBase; override;

    property ButtonID: Byte read FButtonID;
  end;


implementation


function TsgeEventControllerButtonUp.GetName: ShortString;
begin
  Result := Event_ControllerButtonUp;
end;


constructor TsgeEventControllerButtonUp.Create(ID: Byte; ButtonID: Byte);
begin
  inherited Create(ID);
  FButtonID := ButtonID;
end;


function TsgeEventControllerButtonUp.Copy: TsgeEventBase;
begin
  Result := TsgeEventControllerButtonUp.Create(FID, FButtonID);
end;



end.

