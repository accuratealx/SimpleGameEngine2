{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseDown.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Нажатие кнопки
}
{$Include Defines.inc}

unit sgeEventMouseDown;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventMouse;


const
  Event_MouseDown = 'Mouse.Down';


type
  TsgeEventMouseDown = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseDown.GetName: ShortString;
begin
  Result := Event_MouseDown;
end;


function TsgeEventMouseDown.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseDown.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

