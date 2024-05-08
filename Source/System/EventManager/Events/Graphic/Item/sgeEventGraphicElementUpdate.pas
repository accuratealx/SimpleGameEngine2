{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElementUpdate.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы: Обновление элемента
}
{$Include Defines.inc}

unit sgeEventGraphicElementUpdate;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase, sgeEventGraphicElement,
  sgeDisplayElement;


const
  Event_Graphic_ItemUpdate = 'Graphic.Item.Update';


type
  TsgeEventGraphicElementUpdate = class(TsgeEventGraphicElement)
  protected
    FItem: TsgeDisplayElement;
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer; Item: TsgeDisplayElement);

    function Copy: TsgeEventBase; override;

    property Item: TsgeDisplayElement read FItem;
  end;


implementation


function TsgeEventGraphicElementUpdate.GetName: ShortString;
begin
  Result := Event_Graphic_ItemUpdate;
end;


constructor TsgeEventGraphicElementUpdate.Create(UniqueID: Integer; Item: TsgeDisplayElement);
begin
  inherited Create(UniqueID);
  FItem := Item;
end;


function TsgeEventGraphicElementUpdate.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementUpdate.Create(FUniqueID, FItem);
end;



end.

