{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowLostFocus.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Потеря фокуса
}
{$Include Defines.inc}

unit sgeEventWindowLostFocus;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowLostFocus = 'Window.LostFocus';


type
  TsgeEventWindowLostFocus = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowLostFocus.GetName: ShortString;
begin
  Result := Event_WindowLostFocus;
end;


function TsgeEventWindowLostFocus.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowLostFocus.Create;
end;



end.

