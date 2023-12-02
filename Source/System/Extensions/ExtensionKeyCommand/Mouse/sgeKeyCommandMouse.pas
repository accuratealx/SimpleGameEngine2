{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandMouse.pas
Версия            1.1
Создан            04.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Мышь
}
{$Include Defines.inc}

unit sgeKeyCommandMouse;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeKeyCommandMouseActionList, sgeKeyCommandKeyboardActionList;


type
  TsgeKeyCommandMouse = class
  private
    FButtonList: array[TsgeMouseButton] of TsgeKeyCommandMouseActionList;
    FWheel: TsgeKeyCommandKeyboardActionList;

    function GetButton(Index: TsgeMouseButton): TsgeKeyCommandMouseActionList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure DeleteButton(Index: TsgeMouseButton);
    procedure DeleteWheel;

    property Button[Index: TsgeMouseButton]: TsgeKeyCommandMouseActionList read GetButton;
    property Wheel: TsgeKeyCommandKeyboardActionList read FWheel;
  end;



implementation


function TsgeKeyCommandMouse.GetButton(Index: TsgeMouseButton): TsgeKeyCommandMouseActionList;
begin
  Result := FButtonList[Index];
end;


constructor TsgeKeyCommandMouse.Create;
var
  i: TsgeMouseButton;
begin
  //Создать кнопки
  for i := mbLeft to mbExtra2 do
    FButtonList[i] := TsgeKeyCommandMouseActionList.Create;

  //Создать колесо
  FWheel := TsgeKeyCommandKeyboardActionList.Create;
end;


destructor TsgeKeyCommandMouse.Destroy;
var
  i: TsgeMouseButton;
begin
  //Удалить кнопки
  for i := mbLeft to mbExtra2 do
    FButtonList[i].Free;

  //Удалить колесо
  FWheel.Free;
end;


procedure TsgeKeyCommandMouse.Clear;
var
  i: TsgeMouseButton;
begin
  //Очистить кнопки
  for i := mbLeft to mbExtra2 do
    DeleteButton(i);

  //Очистить прокрутку
  DeleteWheel;
end;


procedure TsgeKeyCommandMouse.DeleteButton(Index: TsgeMouseButton);
begin
  FButtonList[Index].Clear;
end;


procedure TsgeKeyCommandMouse.DeleteWheel;
begin
  FWheel.Clear;
end;



end.

