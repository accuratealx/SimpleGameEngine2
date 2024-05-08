{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowMaximize.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Распахнуть
}
{$Include Defines.inc}

unit sgeEventWindowMaximize;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowMaximize = 'Window.Maximize';


type
  TsgeEventWindowMaximize = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowMaximize.GetName: ShortString;
begin
  Result := Event_WindowMaximize;
end;


function TsgeEventWindowMaximize.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowMaximize.Create;
end;



end.

