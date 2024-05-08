{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseLeave.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Уход мыши с формы
}
{$Include Defines.inc}

unit sgeEventMouseLeave;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventMouse;


const
  Event_MouseLeave = 'Mouse.Leave';


type
  TsgeEventMouseLeave = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseLeave.GetName: ShortString;
begin
  Result := Event_MouseLeave;
end;


function TsgeEventMouseLeave.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseLeave.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

