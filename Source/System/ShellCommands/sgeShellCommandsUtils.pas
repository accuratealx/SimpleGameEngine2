{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandsUtils.pas
Версия            1.1
Создан            27.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции команд
}
{$Include Defines.inc}

unit sgeShellCommandsUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeShellScript;


procedure sgeExtractScriptAndLabel(Str: String; var Script: ShortString; var LabelName: ShortString; Separator: Char = ':');
function  sgeGetLabelPosInScript(Script: TsgeShellScript; LabelName: ShortString): Integer;
function  sgeGetProcedurePosInScript(Script: TsgeShellScript; ProcedureName: ShortString): Integer;
function  sgeGetProcedureEndInScript(Script: TsgeShellScript; ProcedureName: ShortString; StartPos: Integer): Integer;
function  sgeGetMaskSafe(Str: String; Strict: Boolean = True): String;

implementation

uses
  sgeSystemUtils, sgeSimpleCommand;


procedure sgeExtractScriptAndLabel(Str: String; var Script: ShortString; var LabelName: ShortString; Separator: Char);
var
  Pos: Integer;
begin
  Script := '';
  LabelName := Str;

  //Определить положение разделителя
  Pos := sgePos(Separator, Str);

  if Pos > 0 then
  begin
    //Есть имя скрипта
    Script := Copy(Str, 1, Pos - 1);
    LabelName := Copy(Str, Pos + 1, Length(Str) - Pos);
  end;
end;


function sgeGetLabelPosInScript(Script: TsgeShellScript; LabelName: ShortString): Integer;
var
  i: Integer;
  Line: TsgeSimpleCommand;
begin
  Result := -1;
  LabelName := LowerCase(LabelName);

  Line := TsgeSimpleCommand.Create;
  try
    for i := 0 to Script.Count - 1 do
    begin
      //Разобрать строку
      Line.Command := Script.Item[i];

      //Пропуск не подходящих строк
      if Line.Count < 2 then
        Continue;

      //Проверить на совпадение имени метки
      if (LowerCase(Line.Part[0]) = 'label') and (LowerCase(Line.Part[1]) = LabelName) then Exit(i);
    end;
  finally
    Line.Free;
  end;
end;


function sgeGetProcedurePosInScript(Script: TsgeShellScript; ProcedureName: ShortString): Integer;
var
  i: Integer;
  Line: TsgeSimpleCommand;
begin
  Result := -1;
  ProcedureName := LowerCase(ProcedureName);

  Line := TsgeSimpleCommand.Create;
  try
    for i := 0 to Script.Count - 1 do
    begin
      //Разобрать строку
      Line.Command := Script.Item[i];

      //Пропуск не подходящих строк
      if Line.Count < 2 then Continue;

      //Проверить на совпадение имени метки
      if (LowerCase(Line.Part[0]) = 'procedure') and (LowerCase(Line.Part[1]) = ProcedureName) then
        Exit(i);
    end;
  finally
    Line.Free;
  end;
end;


function sgeGetProcedureEndInScript(Script: TsgeShellScript; ProcedureName: ShortString; StartPos: Integer): Integer;
var
  i: Integer;
  Line: TsgeSimpleCommand;
begin
  Result := -1;
  ProcedureName := LowerCase(ProcedureName);

  Line := TsgeSimpleCommand.Create;
  try
    for i := StartPos to Script.Count - 1 do
    begin
      //Разобрать строку на части
      Line.Command := Script.Item[i];

      //Пропуск если нет двух частей
      if Line.Count < 2 then
        Continue;

      //Проверить совпадение
      if (LowerCase(Line.Part[0]) = 'return') and (LowerCase(Line.Part[1]) = ProcedureName) then
        Exit(i);
    end;
  finally
    Line.Free;
  end;
end;


function sgeGetMaskSafe(Str: String; Strict: Boolean): String;
const
  sAsterisk = '*';
var
  s: String;
begin
  s := sgeTrim(str);
  if s = '' then
    s := sAsterisk;

  if not Strict then
  begin
    if s[1] <> sAsterisk then
      s := sAsterisk + s;

    if s[Length(s)] <> sAsterisk then
      s[Length(s)] := sAsterisk;
  end;

  Result := s;
end;



end.

