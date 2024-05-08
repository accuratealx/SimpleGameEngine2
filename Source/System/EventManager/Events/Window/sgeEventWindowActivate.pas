{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowActivate.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Активация
}
{$Include Defines.inc}

unit sgeEventWindowActivate;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowActivate = 'Window.Activate';


type
  TsgeEventWindowActivate = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowActivate.GetName: ShortString;
begin
  Result := Event_WindowActivate;
end;


function TsgeEventWindowActivate.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowActivate.Create;
end;



end.

