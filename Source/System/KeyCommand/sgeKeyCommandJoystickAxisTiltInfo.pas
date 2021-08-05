{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о наклоне оси джойстика
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickAxisTiltInfo;

{$mode objfpc}{$H+}

interface

uses
  sgeEventControllers, sgeKeyCommandTypes;


type
  TsgeKeyCommandJoystickAxisTiltInfo = class
  private
    FList: array[TsgeControllerAxisTilt] of TsgeKeyCommandAction;

    function GetTilt(Index: TsgeControllerAxisTilt): TsgeKeyCommandAction;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Index: TsgeControllerAxisTilt);

    property Tilt[Index: TsgeControllerAxisTilt]: TsgeKeyCommandAction read GetTilt;
  end;


implementation



function TsgeKeyCommandJoystickAxisTiltInfo.GetTilt(Index: TsgeControllerAxisTilt): TsgeKeyCommandAction;
begin
  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystickAxisTiltInfo.Create;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    FList[i] := TsgeKeyCommandAction.Create;
end;


destructor TsgeKeyCommandJoystickAxisTiltInfo.Destroy;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystickAxisTiltInfo.Clear;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    Delete(i);
end;


procedure TsgeKeyCommandJoystickAxisTiltInfo.Delete(Index: TsgeControllerAxisTilt);
begin
  FList[Index].Up := '';
  FList[Index].Down := '';
end;



end.


