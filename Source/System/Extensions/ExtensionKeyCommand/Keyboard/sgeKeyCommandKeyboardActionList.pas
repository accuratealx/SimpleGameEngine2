{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandKeyboardActionList.pas
Версия            1.0
Создан            26.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список команд кнопок клавиатуры с модификаторами
}
{$Include Defines.inc}

unit sgeKeyCommandKeyboardActionList;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeTemplateCollection,
  sgeKeyCommandTypes, sgeKeyCommandKeyboardAction;


type
  TsgeKeyCommandKeyboardActionList = class(specialize TsgeTemplateCollection<TsgeKeyCommandKeyboardAction>)
  private
    function GetAction(KeyboardShifts: TsgeKeyboardShifts): TsgeKeyCommandAction;

    procedure CheckActionForDelete(Index: Integer);
  public
    constructor Create;

    function IndexOf(KeyboardShifts: TsgeKeyboardShifts): Integer;

    procedure DeleteAction(KeyboardShifts: TsgeKeyboardShifts = []);

    procedure SetAction(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = ''; Up: String = '');
    procedure SetActionUp(KeyboardShifts: TsgeKeyboardShifts = []; Up: String = '');
    procedure SetActionDown(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = '');

    function  GetActionUp(KeyboardShifts: TsgeKeyboardShifts = []): String;
    function  GetActionDown(KeyboardShifts: TsgeKeyboardShifts = []): String;

    property Action[KeyboardShifts: TsgeKeyboardShifts]: TsgeKeyCommandAction read GetAction;
  end;


implementation


function TsgeKeyCommandKeyboardActionList.GetAction(KeyboardShifts: TsgeKeyboardShifts): TsgeKeyCommandAction;
var
  Idx: Integer;
begin
  Result := nil;

  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    Result := FList[Idx].Action;
end;


procedure TsgeKeyCommandKeyboardActionList.CheckActionForDelete(Index: Integer);
begin
  if (FList[Index].Action.Up = '') and (FList[Index].Action.Down = '') then
    Delete(Index);
end;


constructor TsgeKeyCommandKeyboardActionList.Create;
begin
  inherited Create(True);
end;


function TsgeKeyCommandKeyboardActionList.IndexOf(KeyboardShifts: TsgeKeyboardShifts): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if KeyboardShifts = FList[i].KeyboardShifts then
      Exit(i);
end;


procedure TsgeKeyCommandKeyboardActionList.DeleteAction(KeyboardShifts: TsgeKeyboardShifts);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    Delete(Idx);
end;


procedure TsgeKeyCommandKeyboardActionList.SetAction(KeyboardShifts: TsgeKeyboardShifts; Down: String; Up: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
  begin
    FList[Idx].Action.Up := Up;
    FList[Idx].Action.Down := Down;
    CheckActionForDelete(Idx);
  end
  else
    Add(TsgeKeyCommandKeyboardAction.Create(KeyboardShifts, Down, Up));
end;


procedure TsgeKeyCommandKeyboardActionList.SetActionUp(KeyboardShifts: TsgeKeyboardShifts; Up: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
  begin
    FList[Idx].Action.Up := Up;
    CheckActionForDelete(Idx);
  end
  else
    Add(TsgeKeyCommandKeyboardAction.Create(KeyboardShifts, '', Up));
end;


procedure TsgeKeyCommandKeyboardActionList.SetActionDown(KeyboardShifts: TsgeKeyboardShifts; Down: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
  begin
    FList[Idx].Action.Down := Down;
    CheckActionForDelete(Idx);
  end
  else
    Add(TsgeKeyCommandKeyboardAction.Create(KeyboardShifts, Down, ''));
end;


function TsgeKeyCommandKeyboardActionList.GetActionUp(KeyboardShifts: TsgeKeyboardShifts): String;
var
  Idx: Integer;
begin
  Result := '';
  Idx := IndexOf(KeyboardShifts);
  if Idx = -1 then
    Exit;

  Result := FList[Idx].Action.Up;
end;


function TsgeKeyCommandKeyboardActionList.GetActionDown(KeyboardShifts: TsgeKeyboardShifts): String;
var
  Idx: Integer;
begin
  Result := '';
  Idx := IndexOf(KeyboardShifts);
  if Idx = -1 then
    Exit;

  Result := FList[Idx].Action.Down;
end;



end.

