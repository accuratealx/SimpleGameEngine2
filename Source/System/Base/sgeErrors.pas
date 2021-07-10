{
Пакет             Simple Game Engine 2
Файл              sgeErrors.pas
Версия            1.5
Создан            24.02.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс ошибок
}
{$Include Defines.inc}

unit sgeErrors;

{$mode objfpc}{$H+}

interface


type
  EsgeException = class(TObject)
  private
    FMessage: String;
  public
    constructor Create(Msg: String);
    constructor Create(ModuleName, Error: String; Info: String = ''; NewLine: String = '');

    property Message: string read FMessage write FMessage;
  end;


function sgeCreateErrorString(ModuleName, Error: String; Info: String = ''; NewLine: String = ''): String;

implementation


constructor EsgeException.Create(Msg: String);
begin
  FMessage := Msg;
end;


constructor EsgeException.Create(ModuleName, Error: String; Info: String; NewLine: String);
begin
  Create(sgeCreateErrorString(ModuleName, Error, Info, NewLine));
end;


function sgeCreateErrorString(ModuleName, Error: String; Info: String = ''; NewLine: String = ''): String;
const
  ParamSeparator = ';';
  LineSeparator = #13#10;
begin
  Result := ModuleName + ParamSeparator + Error;
  if Info <> '' then Result := Result + ParamSeparator + Info;
  if NewLine <> '' then Result := Result + LineSeparator + NewLine;
end;


end.

