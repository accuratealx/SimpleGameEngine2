{
Пакет             Simple Game Engine 2
Файл              sgeEventKeyboard.pas
Версия            1.2
Создан            14.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Клавиатура
}
{$Include Defines.inc}

unit sgeEventKeyboard;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

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
    constructor Create(Name: ShortString; Char: Char; KeyboardButtons: TsgeKeyboardButtons);

    function Copy: TsgeEventBase; override;

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
    constructor Create(Name: ShortString; Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);

    function Copy: TsgeEventBase; override;

    property Key: Byte read FKey;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
    property FirstDown: Boolean read FFirstDown;
  end;


implementation


constructor TsgeEventKeyboardChar.Create(Name: ShortString; Char: Char; KeyboardButtons: TsgeKeyboardButtons);
begin
  inherited Create(Name);

  FChar := Char;
  FKeyboardButtons := KeyboardButtons;
end;


function TsgeEventKeyboardChar.Copy: TsgeEventBase;
begin
  Result := TsgeEventKeyboardChar.Create(FName, FChar, FKeyboardButtons);
end;


constructor TsgeEventKeyboard.Create(Name: ShortString; Key: Byte; KeyboardButtons: TsgeKeyboardButtons; FirstDown: Boolean);
begin
  inherited Create(Name);

  FKey := Key;
  FKeyboardButtons := KeyboardButtons;
  FFirstDown := FirstDown;
end;


function TsgeEventKeyboard.Copy: TsgeEventBase;
begin
  Result := TsgeEventKeyboard.Create(FName, FKey, FKeyboardButtons, FFirstDown);
end;



end.

