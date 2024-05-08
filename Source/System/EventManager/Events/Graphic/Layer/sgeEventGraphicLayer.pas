{
Пакет             Simple Game Engine 2
Файл              sgeEventGraphicLayer.pas
Версия            1.0
Создан            05.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Графика: Слой
}
{$Include Defines.inc}

unit sgeEventGraphicLayer;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


type
  TsgeEventGraphicLayer = class(TsgeEventBase)
  protected
    FUniqueID: Integer;
    function GetName: ShortString; override;
  public
    constructor Create(UniqueID: Integer);

    function Copy: TsgeEventBase; override;

    property UniqueID: Integer read FUniqueID;
  end;


implementation


function TsgeEventGraphicLayer.GetName: ShortString;
begin
  Result := 'Graphic.Layer';
end;


constructor TsgeEventGraphicLayer.Create(UniqueID: Integer);
begin
  FUniqueID := UniqueID;
end;


function TsgeEventGraphicLayer.Copy: TsgeEventBase;
begin
  Result := TsgeEventGraphicLayer.Create(FUniqueID);
end;



end.

