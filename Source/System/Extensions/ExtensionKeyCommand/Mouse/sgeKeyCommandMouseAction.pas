{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandMouseAction.pas
Версия            1.0
Создан            30.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения команд кнопки мыши с модификатором
}
{$Include Defines.inc}

unit sgeKeyCommandMouseAction;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeKeyCommandTypes;

type
  TsgeKeyCommandMouseAction = class
  private
    FKeyboardShifts: TsgeKeyboardShifts;
    FAction: TsgeKeyCommandActionMouse;

  public
    constructor Create(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = ''; Up: String = ''; DblClick: String = '');
    destructor  Destroy; override;

    property KeyboardShifts: TsgeKeyboardShifts read FKeyboardShifts;
    property Action: TsgeKeyCommandActionMouse read FAction;
  end;


implementation


constructor TsgeKeyCommandMouseAction.Create(KeyboardShifts: TsgeKeyboardShifts; Down: String; Up: String; DblClick: String);
begin
  FAction := TsgeKeyCommandActionMouse.Create;
  FAction.Up := Up;
  FAction.Down := Down;
  FAction.DblClick := DblClick;
  FKeyboardShifts := KeyboardShifts;
end;


destructor TsgeKeyCommandMouseAction.Destroy;
begin
  FAction.Free;
end;


end.

