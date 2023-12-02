{
Пакет             Simple Game Engine 2
Файл              sgeColorUtils.pas
Версия            1.0
Создан            22.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные методы цвета
}
{$Include Defines.inc}

unit sgeColorUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeColor;



function sgeRGBAToString(R, G, B, A: Byte; Separator: Char = '.'): String;
function sgeRGBAToString(Color: TsgeRGBA; Separator: Char = '.'): String;
function sgeStringToRGBA(Str: String; Separator: Char = '.'): TsgeRGBA;


implementation

uses
  sgeStringList, sgeSystemUtils;


function sgeRGBAToString(R, G, B, A: Byte; Separator: Char): String;
begin
  Result := sgeIntToStr(R) + Separator + sgeIntToStr(G) + Separator + sgeIntToStr(B) + Separator + sgeIntToStr(A);
end;


function sgeRGBAToString(Color: TsgeRGBA; Separator: Char): String;
begin
  Result := sgeRGBAToString(Color.Red, Color.Green, Color.Blue, Color.Alpha);
end;


function sgeStringToRGBA(Str: String; Separator: Char): TsgeRGBA;
var
  List: TsgeStringList;
  i: Integer;
begin
  //Чёрный цвет
  Result := sgeGetRGBA(0, 0, 0, 255);

  //Подготовить список частей
  List := TsgeStringList.Create;
  List.Separator := Separator;
  List.FromString(Str);

  //Выделить компоненты
  if List.Count > 0 then
    if sgeTryStrToInt(List.Part[0], i) then
      Result.Red := i;

  if List.Count > 1 then
    if sgeTryStrToInt(List.Part[1], i) then
      Result.Green := i;

  if List.Count > 2 then
    if sgeTryStrToInt(List.Part[2], i) then
      Result.Blue := i;

  if List.Count > 3 then
    if sgeTryStrToInt(List.Part[3], i) then
      Result.Alpha := i;

  //Удалить список
  List.Free;
end;




end.

