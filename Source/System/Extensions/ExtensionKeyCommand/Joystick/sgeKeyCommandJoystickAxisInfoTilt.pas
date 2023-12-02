{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystickAxisInfoTilt.pas
Версия            1.1
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о наклоне оси джойстика
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickAxisInfoTilt;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandTypes, sgeEventControllers;


type
  TsgeKeyCommandJoystickAxisInfoTilt = class
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



function TsgeKeyCommandJoystickAxisInfoTilt.GetTilt(Index: TsgeControllerAxisTilt): TsgeKeyCommandAction;
begin
  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystickAxisInfoTilt.Create;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    FList[i] := TsgeKeyCommandAction.Create;
end;


destructor TsgeKeyCommandJoystickAxisInfoTilt.Destroy;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystickAxisInfoTilt.Clear;
var
  i: TsgeControllerAxisTilt;
begin
  for i := catMin to catMax do
    Delete(i);
end;


procedure TsgeKeyCommandJoystickAxisInfoTilt.Delete(Index: TsgeControllerAxisTilt);
begin
  FList[Index].Up := '';
  FList[Index].Down := '';
end;



end.


