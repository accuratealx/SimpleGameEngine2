{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystick.pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Джойстики
}
{$Include Defines.inc}

unit sgeKeyCommandJoystick;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandJoystickInfo;


type
  TsgeKeyCommandJoystick = class
  const
    MAX_JOYSTICKS = 7;

  private
    FList: array[0..MAX_JOYSTICKS] of TsgeKeyCommandJoystickInfo;

    function GetCount: Byte;
    function GetItem(Index: Byte): TsgeKeyCommandJoystickInfo;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Delete(Index: Byte);

    property Count: Byte read GetCount;
    property Item[Index: Byte]: TsgeKeyCommandJoystickInfo read GetItem;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils,
  sgeKeyCommandTypes;

const
  _UNITNAME = 'KeyCommandJoystick';


function TsgeKeyCommandJoystick.GetCount: Byte;
begin
  Result := MAX_JOYSTICKS;
end;


function TsgeKeyCommandJoystick.GetItem(Index: Byte): TsgeKeyCommandJoystickInfo;
begin
  if (Index > MAX_JOYSTICKS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


constructor TsgeKeyCommandJoystick.Create;
var
  i: Integer;
begin
  for i := 0 to MAX_JOYSTICKS do
    FList[i] := TsgeKeyCommandJoystickInfo.Create;
end;


destructor TsgeKeyCommandJoystick.Destroy;
var
  i: Integer;
begin
  for i := 0 to MAX_JOYSTICKS do
    FList[i].Free;
end;


procedure TsgeKeyCommandJoystick.Clear;
var
  i: Integer;
begin
  for i := 0 to MAX_JOYSTICKS do
    Delete(i);
end;


procedure TsgeKeyCommandJoystick.Delete(Index: Byte);
begin
  if (Index > MAX_JOYSTICKS) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FList[Index].Clear;
end;



end.

