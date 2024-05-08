{
Пакет             Simple Game Engine 2
Файл              sgeEventMouseUp.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь: Отпускание кнопки
}
{$Include Defines.inc}

unit sgeEventMouseUp;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventMouse;


const
  Event_MouseUp = 'Mouse.Up';


type
  TsgeEventMouseUp = class(TsgeEventMouse)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMouseUp.GetName: ShortString;
begin
  Result := Event_MouseUp;
end;


function TsgeEventMouseUp.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouseUp.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;

end.

