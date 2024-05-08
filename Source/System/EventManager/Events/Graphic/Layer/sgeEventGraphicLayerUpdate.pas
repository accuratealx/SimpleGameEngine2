{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicLayerUpdate.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Слой: Обновить
}
{$Include Defines.inc}

unit sgeEventGraphicLayerUpdate;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventGraphicLayerAdd;


const
  Event_Graphic_LayerUpdate = 'Graphic.Layer.Update';


type
  TsgeEventGraphicLayerUpdate = class(TsgeEventGraphicLayerAdd)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventGraphicLayerUpdate.GetName: ShortString;
begin
  Result := Event_Graphic_LayerUpdate;
end;


function TsgeEventGraphicLayerUpdate.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayerUpdate.Create(FUniqueID, FLayer);
end;



end.

