{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElementDelete.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы: Удалить
}
{$Include Defines.inc}

unit sgeEventGraphicElementDelete;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventGraphicElement;


const
  Event_Graphic_ItemDelete = 'Graphic.Item.Delete';


type
  TsgeEventGraphicElementDelete = class(TsgeEventGraphicElement)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventGraphicElementDelete.GetName: ShortString;
begin
  Result := Event_Graphic_ItemDelete;
end;


function TsgeEventGraphicElementDelete.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementDelete.Create(FUniqueID);
end;



end.

