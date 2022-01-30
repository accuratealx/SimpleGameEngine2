{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandMouseActionList.pas
Версия            1.0
Создан            30.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список команд кнопок мыши с модификаторами
}
{$Include Defines.inc}

unit sgeKeyCommandMouseActionList;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeTemplateCollection,
  sgeKeyCommandTypes, sgeKeyCommandMouseAction;


type
  TsgeKeyCommandMouseActionList = class(specialize TsgeTemplateCollection<TsgeKeyCommandMouseAction>)
  private
    function GetAction(KeyboardShifts: TsgeKeyboardShifts): TsgeKeyCommandActionMouse;

    procedure CheckActionForDelete(Index: Integer);
  public
    constructor Create;

    function IndexOf(KeyboardShifts: TsgeKeyboardShifts): Integer;

    procedure DeleteAction(KeyboardShifts: TsgeKeyboardShifts = []);

    procedure SetAction(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = ''; Up: String = ''; DblClick: String = '');
    procedure SetActionUp(KeyboardShifts: TsgeKeyboardShifts = []; Up: String = '');
    procedure SetActionDown(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = '');
    procedure SetActionDblClick(KeyboardShifts: TsgeKeyboardShifts = []; DblClick: String = '');

    function  GetActionUp(KeyboardShifts: TsgeKeyboardShifts = []): String;
    function  GetActionDown(KeyboardShifts: TsgeKeyboardShifts = []): String;
    function  GetActionDblClick(KeyboardShifts: TsgeKeyboardShifts = []): String;

    property Action[KeyboardShifts: TsgeKeyboardShifts]: TsgeKeyCommandActionMouse read GetAction;
  end;


implementation


function TsgeKeyCommandMouseActionList.GetAction(KeyboardShifts: TsgeKeyboardShifts): TsgeKeyCommandActionMouse;
var
  Idx: Integer;
begin
  Result := nil;

  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    Result := FList[Idx].Action;
end;


procedure TsgeKeyCommandMouseActionList.CheckActionForDelete(Index: Integer);
begin
  if (FList[Index].Action.Up = '') and (FList[Index].Action.Down = '') and (FList[Index].Action.DblClick = '') then
    Delete(Index);
end;


constructor TsgeKeyCommandMouseActionList.Create;
begin
  inherited Create(True);
end;


function TsgeKeyCommandMouseActionList.IndexOf(KeyboardShifts: TsgeKeyboardShifts): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if KeyboardShifts = FList[i].KeyboardShifts then
      Exit(i);
end;


procedure TsgeKeyCommandMouseActionList.DeleteAction(KeyboardShifts: TsgeKeyboardShifts);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    Delete(Idx);
end;


procedure TsgeKeyCommandMouseActionList.SetAction(KeyboardShifts: TsgeKeyboardShifts; Down: String; Up: String; DblClick: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    begin
    FList[Idx].Action.Up := Up;
    FList[Idx].Action.Down := Down;
    FList[Idx].Action.DblClick := DblClick;
    CheckActionForDelete(Idx);
    end
    else Add(TsgeKeyCommandMouseAction.Create(KeyboardShifts, Down, Up, DblClick));
end;


procedure TsgeKeyCommandMouseActionList.SetActionUp(KeyboardShifts: TsgeKeyboardShifts; Up: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    begin
    FList[Idx].Action.Up := Up;
    CheckActionForDelete(Idx);
    end
    else Add(TsgeKeyCommandMouseAction.Create(KeyboardShifts, '', Up, ''));
end;


procedure TsgeKeyCommandMouseActionList.SetActionDown(KeyboardShifts: TsgeKeyboardShifts; Down: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    begin
    FList[Idx].Action.Down := Down;
    CheckActionForDelete(Idx);
    end
    else Add(TsgeKeyCommandMouseAction.Create(KeyboardShifts, Down, '', ''));
end;


procedure TsgeKeyCommandMouseActionList.SetActionDblClick(KeyboardShifts: TsgeKeyboardShifts; DblClick: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(KeyboardShifts);
  if Idx <> -1 then
    begin
    FList[Idx].Action.DblClick := DblClick;
    CheckActionForDelete(Idx);
    end
    else Add(TsgeKeyCommandMouseAction.Create(KeyboardShifts, '', '', DblClick));
end;


function TsgeKeyCommandMouseActionList.GetActionUp(KeyboardShifts: TsgeKeyboardShifts): String;
var
  Idx: Integer;
begin
  Result := '';
  Idx := IndexOf(KeyboardShifts);
  if Idx = -1 then Exit;

  Result := FList[Idx].Action.Up;
end;


function TsgeKeyCommandMouseActionList.GetActionDown(KeyboardShifts: TsgeKeyboardShifts): String;
var
  Idx: Integer;
begin
  Result := '';
  Idx := IndexOf(KeyboardShifts);
  if Idx = -1 then Exit;

  Result := FList[Idx].Action.Down;
end;


function TsgeKeyCommandMouseActionList.GetActionDblClick(KeyboardShifts: TsgeKeyboardShifts): String;
var
  Idx: Integer;
begin
  Result := '';
  Idx := IndexOf(KeyboardShifts);
  if Idx = -1 then Exit;

  Result := FList[Idx].Action.DblClick;
end;



end.

