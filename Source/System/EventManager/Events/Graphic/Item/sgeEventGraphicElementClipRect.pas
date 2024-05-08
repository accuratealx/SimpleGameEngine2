{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElementClipRect.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы: Ограничение вывода
}
{$Include Defines.inc}

unit sgeEventGraphicElementClipRect;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeEventBase, sgeEventGraphicElement;


const
  Event_Graphic_ItemClipRect = 'Graphic.Item.ClipRect';


type
  TsgeEventGraphicElementClipRect = class(TsgeEventGraphicElement)
  private
    FClipped: Boolean;
    FClipRect: TsgeClipRect;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer; Clipped: Boolean; ClipRect: TsgeClipRect);

    function Copy: TsgeEventBase; override;

    property Clipped: Boolean read FClipped;
    property ClipRect: TsgeClipRect read FClipRect;
  end;


implementation


function TsgeEventGraphicElementClipRect.GetName: ShortString;
begin
  Result := Event_Graphic_ItemClipRect;
end;


constructor TsgeEventGraphicElementClipRect.Create(UniqueID: Integer; Clipped: Boolean; ClipRect: TsgeClipRect);
begin
  inherited Create(UniqueID);
  FClipped := Clipped;
  FClipRect := ClipRect;
end;


function TsgeEventGraphicElementClipRect.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementClipRect.Create(FUniqueID, FClipped, FClipRect);
end;



end.

