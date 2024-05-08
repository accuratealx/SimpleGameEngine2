{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElementAdd.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы: Добавление элемента
}
{$Include Defines.inc}

unit sgeEventGraphicElementAdd;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase, sgeEventGraphicElementUpdate,
  sgeDisplayElement;


const
  Event_Graphic_ItemAdd = 'Graphic.Item.Add';


type
  TsgeEventGraphicElementAdd = class(TsgeEventGraphicElementUpdate)
  private
    FLayerName: String;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer; Item: TsgeDisplayElement; LayerName: String);

    function Copy: TsgeEventBase; override;

    property LayerName: String read FLayerName;
  end;


implementation


function TsgeEventGraphicElementAdd.GetName: ShortString;
begin
  Result := Event_Graphic_ItemAdd;
end;


constructor TsgeEventGraphicElementAdd.Create(UniqueID: Integer; Item: TsgeDisplayElement; LayerName: String);
begin
  inherited Create(UniqueID, Item);
  FLayerName := LayerName;
end;


function TsgeEventGraphicElementAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementAdd.Create(FUniqueID, FItem, FLayerName);
end;



end.

