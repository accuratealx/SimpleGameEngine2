{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseEnter.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Заход на форму
}
{$Include Defines.inc}

unit sgeEventMouseEnter;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventMouse;


const
  Event_MouseEnter = 'Mouse.Enter';


type
  TsgeEventMouseEnter = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseEnter.GetName: ShortString;
begin
  Result := Event_MouseEnter;
end;


function TsgeEventMouseEnter.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseEnter.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

