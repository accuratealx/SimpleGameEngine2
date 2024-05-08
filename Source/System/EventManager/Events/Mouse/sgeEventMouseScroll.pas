{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseScroll.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Прокрутка колесом
}
{$Include Defines.inc}

unit sgeEventMouseScroll;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventMouse;


const
  Event_MouseScroll = 'Mouse.Scroll';


type
  TsgeEventMouseScroll = class(TsgeEventMouse)
  protected
    FDelta: Integer;

    function GetName: ShortString; override;
  public
    constructor Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer); reintroduce;

    function Copy: TsgeEventBase; override;

    property Delta: Integer read FDelta;
  end;


implementation


function TsgeEventMouseScroll.GetName: ShortString;
begin
  Result := Event_MouseScroll;
end;


constructor TsgeEventMouseScroll.Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer);
begin
  inherited Create(X, Y, MouseButtons, KeyboardButtons);
  FDelta := Delta;
end;


function TsgeEventMouseScroll.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseScroll.Create(FX, FY, FMouseButtons, FKeyboardButtons, FDelta);
end;

end.

