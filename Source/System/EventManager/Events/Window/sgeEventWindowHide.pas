{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowHide.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Спрятать
}
{$Include Defines.inc}

unit sgeEventWindowHide;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowHide = 'Window.Hide';


type
  TsgeEventWindowHide = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowHide.GetName: ShortString;
begin
  Result := Event_WindowHide;
end;


function TsgeEventWindowHide.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowHide.Create;
end;



end.

