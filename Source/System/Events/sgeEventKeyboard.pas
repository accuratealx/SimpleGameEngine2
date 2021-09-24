{
Пакет             Simple Game Engine 2
Файл              sgeEventKeyboard.pas
Версия            1.0
Создан            14.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Клавиатура
}
{$Include Defines.inc}

unit sgeEventKeyboard;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeEventBase;


const
  Event_KeyboardChar  = 'Keyboard.Char';
  Event_KeyboardDown  = 'Keyboard.KeyDown';
  Event_KeyboardUp    = 'Keyboard.KeyUp';

type
  //Ввод символа
  TsgeEventKeyboardChar = class(TsgeEventBase)
  private
    FChar: Char;
    FKeyboardButtons: TsgeKeyboardButtons;
  public
    constructor Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);

    property Char: Char read FChar;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;



  //Кнопки
  TsgeEventKeyboard = class(TsgeEventBase)
  private
    FKey: Byte;
    FKeyboardButtons: TsgeKeyboardButtons;
    FFirstDown: Boolean;
  public
    constructor Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);

    property Key: Byte read FKey;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
    property FirstDown: Boolean read FFirstDown;
  end;


implementation


constructor TsgeEventKeyboardChar.Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);
begin
  FChar := Char;
  FKeyboardButtons := KeyboardButtons;
end;


constructor TsgeEventKeyboard.Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);
begin
  FKey := Key;
  FKeyboardButtons := KeyboardButtons;
  FFirstDown := FirstDown;
end;




end.
