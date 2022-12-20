{
Пакет             Simple Game Engine 2
Файл              sgeDateUtils.pas
Версия            1.1
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Функции работы с датой
}
{$Include Defines.inc}

unit sgeDateUtils;

{$mode objfpc}{$H+}

interface

type
  TDateTime = type Double;


function sgeNow: TDateTime;
function sgeFormatDateTime(const FormatStr: string; DateTime: TDateTime): string;
function sgeGetTimeStamp: String;



implementation

uses
  SysUtils;


function sgeNow: TDateTime;
begin
  Result := SysUtils.Now;
end;


function sgeFormatDateTime(const FormatStr: string; DateTime: TDateTime): string;
begin
  Result := SysUtils.FormatDateTime(FormatStr, DateTime);
end;


function sgeGetTimeStamp: String;
begin
  Result := sgeFormatDateTime('yyyy.mm.dd-hh.nn.ss', Now);
end;



end.

