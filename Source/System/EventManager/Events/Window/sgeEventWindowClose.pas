{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowClose.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Закрытие
}
{$Include Defines.inc}

unit sgeEventWindowClose;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowClose = 'Window.Close';


type
  TsgeEventWindowClose = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowClose.GetName: ShortString;
begin
  Result := Event_WindowClose;
end;


function TsgeEventWindowClose.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowClose.Create;
end;



end.

