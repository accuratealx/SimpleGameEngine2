{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseMove.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Движение над формой
}
{$Include Defines.inc}

unit sgeEventMouseMove;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventMouse;


const
  Event_MouseMove = 'Mouse.Move';


type
  TsgeEventMouseMove = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseMove.GetName: ShortString;
begin
  Result := Event_MouseMove;
end;


function TsgeEventMouseMove.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseMove.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

