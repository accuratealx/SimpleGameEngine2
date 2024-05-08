{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicLayerDelete.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Слой: Удалить
}
{$Include Defines.inc}

unit sgeEventGraphicLayerDelete;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventGraphicLayer;


const
  Event_Graphic_LayerDelete = 'Graphic.Layer.Delete';


type
  TsgeEventGraphicLayerDelete = class(TsgeEventGraphicLayer)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventGraphicLayerDelete.GetName: ShortString;
begin
  Result := Event_Graphic_LayerDelete;
end;


function TsgeEventGraphicLayerDelete.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayerDelete.Create(FUniqueID);
end;



end.

