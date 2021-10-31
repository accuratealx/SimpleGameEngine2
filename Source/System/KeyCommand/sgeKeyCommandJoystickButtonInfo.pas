{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystickButtonInfo.pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о кнопках джойстика
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickButtonInfo;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandTypes;


type
  //Информация о кнопках джойстика
  TsgeKeyCommandJoystickButtonsInfo = class
  const
    MAX_BUTTONS = 31;
  private
    FList: array[0..MAX_BUTTONS] of TsgeKeyCommandAction;

    function GetCount: Byte;
    function GetItem(Index: Byte): TsgeKeyCommandAction;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Index: Byte);

    property Count: Byte read GetCount;
    property Item[Index: Byte]: TsgeKeyCommandAction read GetItem;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'KeyCommandJoystickButtonInfo';



function TsgeKeyCommandJoystickButtonsInfo.GetCount: Byte;
begin
  Result := MAX_BUTTONS;
end;


function TsgeKeyCommandJoystickButtonsInfo.GetItem(Index: Byte): TsgeKeyCommandAction;
begin
  if (Index > MAX_BUTTONS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystickButtonsInfo.Create;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i] := TsgeKeyCommandAction.Create;
end;


destructor TsgeKeyCommandJoystickButtonsInfo.Destroy;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystickButtonsInfo.Clear;
var
  i: Integer;
begin
  for i := 0 to MAX_BUTTONS do
    Delete(i);
end;


procedure TsgeKeyCommandJoystickButtonsInfo.Delete(Index: Byte);
begin
  if (Index > MAX_BUTTONS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FList[Index].Up := '';
  FList[Index].Down := '';
end;



end.

