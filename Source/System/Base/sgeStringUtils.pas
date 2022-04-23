{
Пакет             Simple Game Engine 2
Файл              sgeStringUtils.pas
Версия            1.1
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции для работы со строками
}
{$Include Defines.inc}

unit sgeStringUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeStringList, sgeSimpleParameters;


type
  //Модификаторы замены текста
  TsgeReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);


function  sgeStringReplace(const S, OldPattern, NewPattern: string; Flags: TsgeReplaceFlags): string;
function  sgeSubstituteParamsToString(Str: String; Parameters: TsgeSimpleParameters; OpenQuote: String = ''; CloseQuote: String = ''): String;
function  sgeGetListIndexByValue(List: TsgeStringList; Value: String): Integer;
function  sgeMatchString(const Str: String; const Mask: String = '*'): Boolean;

implementation

uses
  SysUtils, sgeSystemUtils, sgeMatch;


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


function sgeGetListIndexByValue(List: TsgeStringList; Value: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  Value := LowerCase(sgeTrim(Value));
  for i := 0 to List.Count - 1 do
    if LowerCase(List.Part[i]) = Value then
      Exit(i);
end;


function sgeMatchString(const Str: String; const Mask: String): Boolean;
begin
  with TsgeMatch.Create(Mask) do
  try
    Result := Match(Str);
  finally
    Free;
  end;
end;


end.

