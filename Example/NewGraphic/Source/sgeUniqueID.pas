{
Пакет             Simple Game Engine 2
Файл              sgeUniqueID.pas
Версия            1.0
Создан            05.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс получения уникального иденификатора
}
{$Include Defines.inc}

unit sgeUniqueID;

{$mode ObjFPC}{$H+}

interface

type
  TsgeUniqueID = class
  private
    FCurrentID: LongInt;

  public
    function GetID: LongInt;
  end;


var
  UniqueID: TsgeUniqueID;


implementation


function TsgeUniqueID.GetID: LongInt;
begin
  Inc(FCurrentID);

  //Проверить на выход за диапазон
  if FCurrentID > MaxLongint - 1 then
    FCurrentID := 1;

  //Результат
  Result := FCurrentID;
end;


initialization
begin
  UniqueID := TsgeUniqueID.Create;
end;


finalization
begin
  UniqueID.Free;
end;



end.

