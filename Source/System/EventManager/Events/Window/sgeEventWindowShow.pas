{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowShow.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Показать
}
{$Include Defines.inc}

unit sgeEventWindowShow;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_WindowShow = 'Window.Show';


type
  TsgeEventWindowShow = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventWindowShow.GetName: ShortString;
begin
  Result := Event_WindowShow;
end;


function TsgeEventWindowShow.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowShow.Create;
end;



end.

