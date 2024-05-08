{
Пакет             Simple Game Engine 2
Файл              sgeEventKeyboardChar.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Клавиатура: Ввод локализованного символа
}
{$Include Defines.inc}

unit sgeEventKeyboardChar;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase;


const
  Event_KeyboardChar  = 'Keyboard.Char';


type
  TsgeEventKeyboardChar = class(TsgeEventBase)
  private
    FChar: Char;
    FKeyboardButtons: TsgeKeyboardButtons;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);

    function Copy: TsgeEventBase; override;

    property Char: Char read FChar;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;


implementation


function TsgeEventKeyboardChar.GetName: ShortString;
begin
  Result := Event_KeyboardChar;
end;


constructor TsgeEventKeyboardChar.Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);
begin
  FChar := Char;
  FKeyboardButtons := KeyboardButtons;
end;


function TsgeEventKeyboardChar.Copy: TsgeEventBase;
begin
  Result := TsgeEventKeyboardChar.Create(FChar, FKeyboardButtons);
end;



end.

