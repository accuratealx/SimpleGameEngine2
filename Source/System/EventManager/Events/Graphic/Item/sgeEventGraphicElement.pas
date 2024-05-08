{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicElement.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Элементы
}
{$Include Defines.inc}

unit sgeEventGraphicElement;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


type
  TsgeEventGraphicElement = class(TsgeEventBase)
  protected
    FUniqueID: Integer;
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer);

    function Copy: TsgeEventBase; override;

    property UniqueID: Integer read FUniqueID;
  end;


implementation


function TsgeEventGraphicElement.GetName: ShortString;
begin
  Result := 'Graphic.Item';
end;


constructor TsgeEventGraphicElement.Create(UniqueID: Integer);
begin
  FUniqueID := UniqueID;
end;


function TsgeEventGraphicElement.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicElement.Create(FUniqueID);
end;



end.

