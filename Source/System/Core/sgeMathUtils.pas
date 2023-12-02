{
Пакет             Simple Game Engine 2
Файл              sgeStringUtils.pas
Версия            1.2
Создан            14.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные математические функции
}
{$Include Defines.inc}

unit sgeMathUtils;

{$mode objfpc}{$H+}
{$Inline On}

interface

const
  _PI = 3.1415926538;
  _PI_DIV_180 = PI / 180.0;
  _180_DIV_PI = 180 / PI;


function sgeMin(a, b: Integer): Integer; inline;
function sgeMax(a, b: Integer): Integer; inline;

function sgeFloor(x: Single): integer; inline;
function sgeFloor(x: Double): integer; inline;

function sgeCeil(x: Single): Integer; inline;
function sgeCeil(x: Double): Integer; inline;

function sgeRadToDeg(Rad: Single): Single; inline;
function sgeRadToDeg(Rad: Double): Double; inline;

function sgeDegToRad(Deg: Single): Single; inline;
function sgeDegToRad(Deg: Double): Double; inline;

procedure sgeFitToRange(var Value: Single; Min: Single = 0; Max: Single = 1); inline;

implementation


function sgeMin(a, b: Integer): Integer; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


function sgeMax(a, b: Integer): Integer; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
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


function sgeRadToDeg(Rad: Single): Single;
begin
  Result := Rad * _180_DIV_PI;
end;


function sgeRadToDeg(Rad: Double): Double;
begin
  Result := Rad * _180_DIV_PI;
end;


function sgeDegToRad(Deg: Single): Single;
begin
  Result := Deg * _PI_DIV_180;
end;


function sgeDegToRad(Deg: Double): Double;
begin
  Result := Deg * _PI_DIV_180;
end;


procedure sgeFitToRange(var Value: Single; Min: Single; Max: Single);
begin
  if Value < Min then
    Value := Min;
  if Value > Max then
    Value := Max;
end;



end.

