{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicLayerAdd.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Слой: Добавить
}
{$Include Defines.inc}

unit sgeEventGraphicLayerAdd;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase, sgeEventGraphicLayer,
  sgeDisplayLayer;


const
  Event_Graphic_LayerAdd = 'Graphic.Layer.Add';


type
  TsgeEventGraphicLayerAdd = class(TsgeEventGraphicLayer)
  protected
    FLayer: TsgeDisplayLayer;
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer; Layer: TsgeDisplayLayer);

    function Copy: TsgeEventBase; override;

    property Layer: TsgeDisplayLayer read FLayer;
  end;


implementation


function TsgeEventGraphicLayerAdd.GetName: ShortString;
begin
  Result := Event_Graphic_LayerAdd;
end;


constructor TsgeEventGraphicLayerAdd.Create(UniqueID: Integer; Layer: TsgeDisplayLayer);
begin
  inherited Create(UniqueID);
  FLayer := Layer;
end;


function TsgeEventGraphicLayerAdd.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayerAdd.Create(FUniqueID, FLayer);
end;



end.

