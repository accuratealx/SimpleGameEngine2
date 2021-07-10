{
Пакет             Simple Game Engine 2
Файл              sgeStringUtils.pas
Версия            1.0
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции для работы со строками
}
{$Include Defines.inc}

unit sgeStringUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters;


type
  //Модификаторы замены текста
  TsgeReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);



function  sgeStringReplace(const S, OldPattern, NewPattern: string; Flags: TsgeReplaceFlags): string;
function  sgeSubstituteParamsToString(Str: String; Parameters: TsgeSimpleParameters; OpenQuote: String = ''; CloseQuote: String = ''): String;

implementation

uses
  SysUtils;


function sgeStringReplace(const S, OldPattern, NewPattern: string; Flags: TsgeReplaceFlags): string;
begin
  Result := SysUtils.StringReplace(S, OldPattern, NewPattern, TReplaceFlags(Flags));
end;


function sgeSubstituteParamsToString(Str: String; Parameters: TsgeSimpleParameters; OpenQuote: String; CloseQuote: String): String;
var
  i, c: Integer;
begin
  Result := Str;

  c := Parameters.Count - 1;
  for i := 0 to c do
    Result := StringReplace(Result, OpenQuote + Parameters.Parameter[i].Name + CloseQuote, Parameters.Parameter[i].Value, [SysUtils.rfIgnoreCase, SysUtils.rfReplaceAll]);
end;


end.

