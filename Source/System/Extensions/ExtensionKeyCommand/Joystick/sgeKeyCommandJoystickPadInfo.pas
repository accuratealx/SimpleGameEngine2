{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystickPadInfo.pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о крестовине джойстика
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickPadInfo;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeKeyCommandTypes;


type
  TsgeKeyCommandJoystickPadInfo = class
  private
    FList: array[TsgeControllerPovDirection] of TsgeKeyCommandAction;

    function GetItem(Index: TsgeControllerPovDirection): TsgeKeyCommandAction;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Index: TsgeControllerPovDirection);

    property Item[Index: TsgeControllerPovDirection]: TsgeKeyCommandAction read GetItem;
  end;


implementation



function TsgeKeyCommandJoystickPadInfo.GetItem(Index: TsgeControllerPovDirection): TsgeKeyCommandAction;
begin
  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystickPadInfo.Create;
var
  i: TsgeControllerPovDirection;
begin
  for i := cpdUp to cpdLeft do
    FList[i] := TsgeKeyCommandAction.Create;
end;


destructor TsgeKeyCommandJoystickPadInfo.Destroy;
var
  i: TsgeControllerPovDirection;
begin
  for i := cpdUp to cpdLeft do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystickPadInfo.Clear;
var
  i: TsgeControllerPovDirection;
begin
  for i := cpdUp to cpdLeft do
    Delete(i);
end;


procedure TsgeKeyCommandJoystickPadInfo.Delete(Index: TsgeControllerPovDirection);
begin
  FList[Index].Up := '';
  FList[Index].Down := '';
end;



end.

