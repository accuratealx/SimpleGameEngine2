{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElementVisible.pas
Версия            1.0
Создан            07.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы: Изменение видимости
}
{$Include Defines.inc}

unit sgeEventGraphicElementVisible;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase, sgeEventGraphicElement;


const
  Event_Graphic_ItemVisible = 'Graphic.Item.Visible';


type
  TsgeEventGraphicElementVisible = class(TsgeEventGraphicElement)
  private
    FVisible: Boolean;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer; Visible: Boolean);

    function Copy: TsgeEventBase; override;

    property Visible: Boolean read FVisible;
  end;


implementation


function TsgeEventGraphicElementVisible.GetName: ShortString;
begin
  Result := Event_Graphic_ItemVisible;
end;


constructor TsgeEventGraphicElementVisible.Create(UniqueID: Integer; Visible: Boolean);
begin
  inherited Create(UniqueID);
  FVisible := Visible;
end;


function TsgeEventGraphicElementVisible.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElementVisible.Create(FUniqueID, FVisible);
end;



end.

