{
Пакет             Simple Game Engine 2
Файл              sgeStringUtils.pas
Версия            1.0
Создан            14.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные математические функции
}
{$Include Defines.inc}

unit sgeMathUtils;

{$mode objfpc}{$H+}
{$Inline On}

interface


function sgeMin(a, b: Integer): Integer;
function sgeMax(a, b: Integer): Integer;

function sgeFloor(x: Single): integer;
function sgeFloor(x: Double): integer;

function sgeCeil(x: Single): Integer;
function sgeCeil(x: Double): Integer;



implementation


function sgeMin(a, b: Integer): Integer; inline;
begin
  if a < b then Result := a else Result := b;
end;


function sgeMax(a, b: Integer): Integer; inline;
begin
  if a > b then Result := a else Result := b;
end;


function sgeFloor(x: Single): Integer; inline;
begin
  Result := Trunc(x) - ord(Frac(x) < 0);
end;


function sgeFloor(x: Double): Integer; inline;
begin
  Result := Trunc(x) - ord(Frac(x) < 0);
end;


function sgeCeil(x: Single): Integer; inline;
begin
  Result := Trunc(x) + ord(Frac(x) > 0);
end;


function sgeCeil(x: Double): Integer;
begin
  Result := Trunc(x) + ord(Frac(x) > 0);
end;



end.

