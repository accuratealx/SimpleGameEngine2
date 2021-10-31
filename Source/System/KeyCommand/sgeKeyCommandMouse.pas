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
  sgeKeyCommandTypes;


type
  TsgeKeyCommandMouse = class
  private
    FButtonList: array[TsgeMouseButton] of TsgeKeyCommandMouseAction;
    FWheel: TsgeKeyCommandAction;

    function GetButton(Index: TsgeMouseButton): TsgeKeyCommandMouseAction;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure DeleteButton(Index: TsgeMouseButton);
    procedure DeleteWheel;

    property Button[Index: TsgeMouseButton]: TsgeKeyCommandMouseAction read GetButton;
    property Wheel: TsgeKeyCommandAction read FWheel;
  end;



implementation


function TsgeKeyCommandMouse.GetButton(Index: TsgeMouseButton): TsgeKeyCommandMouseAction;
begin
  Result := FButtonList[Index];
end;


constructor TsgeKeyCommandMouse.Create;
var
  i: TsgeMouseButton;
begin
  //Создать кнопки
  for i := mbLeft to mbExtra2 do
    FButtonList[i] := TsgeKeyCommandMouseAction.Create;

  //Создать колесо
  FWheel := TsgeKeyCommandAction.Create;
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
  FButtonList[Index].Up := '';
  FButtonList[Index].Down := '';
  FButtonList[Index].DblClick := '';
end;


procedure TsgeKeyCommandMouse.DeleteWheel;
begin
  FWheel.Up := '';
  FWheel.Down := '';
end;





end.

