{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowMinimize.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Свернуть
}
{$Include Defines.inc}

unit sgeEventWindowMinimize;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowMinimize = 'Window.Minimize';


type
  TsgeEventWindowMinimize = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation
    

function TsgeEventWindowMinimize.GetName: ShortString;
begin
  Result := Event_WindowMinimize;
end;


function TsgeEventWindowMinimize.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowMinimize.Create;
end;



end.

