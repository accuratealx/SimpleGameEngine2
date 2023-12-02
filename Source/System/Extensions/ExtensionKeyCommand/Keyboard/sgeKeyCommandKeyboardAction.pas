{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandKeyboardAction.pas
Версия            1.0
Создан            26.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс хранения команд кнопки клавиатуры с модификатором
}
{$Include Defines.inc}

unit sgeKeyCommandKeyboardAction;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeKeyCommandTypes;

type
  TsgeKeyCommandKeyboardAction = class
  private
    FKeyboardShifts: TsgeKeyboardShifts;
    FAction: TsgeKeyCommandAction;

  public
    constructor Create(KeyboardShifts: TsgeKeyboardShifts = []; Down: String = ''; Up: String = '');
    destructor  Destroy; override;

    property KeyboardShifts: TsgeKeyboardShifts read FKeyboardShifts;
    property Action: TsgeKeyCommandAction read FAction;
  end;


implementation


constructor TsgeKeyCommandKeyboardAction.Create(KeyboardShifts: TsgeKeyboardShifts; Down: String; Up: String);
begin
  FAction := TsgeKeyCommandAction.Create;
  FAction.Up := Up;
  FAction.Down := Down;
  FKeyboardShifts := KeyboardShifts;
end;


destructor TsgeKeyCommandKeyboardAction.Destroy;
begin
  FAction.Free;
end;


end.

