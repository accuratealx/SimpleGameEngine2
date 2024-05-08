{
Пакет             Simple Game Engine 2
Файл              sgeEventKeyboardKeyDown.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Клавиатура: Нажатие кнопки
}
{$Include Defines.inc}

unit sgeEventKeyboardKeyDown;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventKeyboardKeyUp;


const
  Event_KeyboardDown = 'Keyboard.KeyDown';


type
  TsgeEventKeyboardKeyDown = class(TsgeEventKeyboardKeyUp)
  private
    FFirstDown: Boolean;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);

    function Copy: TsgeEventBase; override;

    property FirstDown: Boolean read FFirstDown;
  end;


implementation


function TsgeEventKeyboardKeyDown.GetName: ShortString;
begin
  Result := Event_KeyboardDown;
end;


constructor TsgeEventKeyboardKeyDown.Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);
begin
  inherited Create(Key, KeyboardButtons);
  FFirstDown := FirstDown;
end;


function TsgeEventKeyboardKeyDown.Copy: TsgeEventBase;
begin
  Result := TsgeEventKeyboardKeyDown.Create(FKey, FKeyboardButtons, FFirstDown);
end;



end.

