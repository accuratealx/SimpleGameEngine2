{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystickAxisInfo.pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о крестовине джойстика
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickAxisInfo;

{$mode objfpc}{$H+}

interface

uses
  sgeEventControllers,
  sgeKeyCommandJoystickAxisTiltInfo;


type
  TsgeKeyCommandJoystickAxisInfo = class
  private
    FList: array[TsgeControllerAxisType] of TsgeKeyCommandJoystickAxisTiltInfo;

    function GetItem(Index: TsgeControllerAxisType): TsgeKeyCommandJoystickAxisTiltInfo;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Index: TsgeControllerAxisType);

    property Item[Index: TsgeControllerAxisType]: TsgeKeyCommandJoystickAxisTiltInfo read GetItem;
  end;


implementation



function TsgeKeyCommandJoystickAxisInfo.GetItem(Index: TsgeControllerAxisType): TsgeKeyCommandJoystickAxisTiltInfo;
begin
  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystickAxisInfo.Create;
var
  i: TsgeControllerAxisType;
begin
  for i := catX to catZ do
    FList[i] := TsgeKeyCommandJoystickAxisTiltInfo.Create;
end;


destructor TsgeKeyCommandJoystickAxisInfo.Destroy;
var
  i: TsgeControllerAxisType;
begin
  for i := catX to catZ do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystickAxisInfo.Clear;
var
  i: TsgeControllerAxisType;
begin
  for i := catX to catZ do
    Delete(i);
end;


procedure TsgeKeyCommandJoystickAxisInfo.Delete(Index: TsgeControllerAxisType);
begin
  FList[Index].Clear;
end;



end.

