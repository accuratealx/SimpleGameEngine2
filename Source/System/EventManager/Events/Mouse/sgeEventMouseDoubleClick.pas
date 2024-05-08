{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseDoubleClick.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Двойной клик
}
{$Include Defines.inc}

unit sgeEventMouseDoubleClick;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventMouse;


const
  Event_MouseDoubleClick = 'Mouse.DoubleClick';


type
  TsgeEventMouseDoubleClick = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseDoubleClick.GetName: ShortString;
begin
  Result := Event_MouseDoubleClick;
end;


function TsgeEventMouseDoubleClick.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseDoubleClick.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

