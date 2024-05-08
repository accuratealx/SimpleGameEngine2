{
Пакет             Simple Game Engine 2
Файл              sgeEventKeyboardKeyUp.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Клавиатура: Отпускание кнопки
}
{$Include Defines.inc}

unit sgeEventKeyboardKeyUp;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_KeyboardUp = 'Keyboard.KeyUp';


type
  TsgeEventKeyboardKeyUp = class(TsgeEventBase)
  protected
    FKey: Byte;
    FKeyboardButtons: TsgeKeyboardButtons;

    function GetName: ShortString; override;
  public
    constructor Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);

    function Copy: TsgeEventBase; override;

    property Key: Byte read FKey;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;


implementation


function TsgeEventKeyboardKeyUp.GetName: ShortString;
begin
  Result := Event_KeyboardUp;
end;


constructor TsgeEventKeyboardKeyUp.Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);
begin
  FKey := Key;
  FKeyboardButtons := KeyboardButtons;
end;


function TsgeEventKeyboardKeyUp.Copy: TsgeEventBase;
begin
  Result := TsgeEventKeyboardKeyUp.Create(FKey, FKeyboardButtons);
end;



end.

