{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowSetFocus.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Установить фокус
}
{$Include Defines.inc}

unit sgeEventWindowSetFocus;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowSetFocus = 'Window.SetFocus';


type
  TsgeEventWindowSetFocus = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowSetFocus.GetName: ShortString;
begin
  Result := Event_WindowSetFocus;
end;


function TsgeEventWindowSetFocus.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowSetFocus.Create;
end;



end.

