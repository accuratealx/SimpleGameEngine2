{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowDeactivate.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Деактивация
}
{$Include Defines.inc}

unit sgeEventWindowDeactivate;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowDeActivate = 'Window.Deactivate';


type
  TsgeEventWindowDeactivate = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowDeactivate.GetName: ShortString;
begin
  Result := Event_WindowDeActivate;
end;


function TsgeEventWindowDeactivate.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowDeactivate.Create;
end;



end.

