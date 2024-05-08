{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowRestore.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Восстановить
}
{$Include Defines.inc}

unit sgeEventWindowRestore;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowRestore = 'Window.Restore';


type
  TsgeEventWindowRestore = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowRestore.GetName: ShortString;
begin
  Result := Event_WindowRestore;
end;


function TsgeEventWindowRestore.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowRestore.Create;
end;



end.

