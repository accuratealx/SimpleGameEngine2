{
Пакет             Simple Game Engine 2
Файл              sgeShellScript.pas
Версия            1.2
Создан            24.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Сценарий оболочки
}
{$Include Defines.inc}

unit sgeShellScript;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection;


type
  TsgeShellScript = class(specialize TsgeTemplateCollection<String>)
  private
    FName: ShortString;

  public
    constructor Create(Name: ShortString);
    constructor Create(Name: ShortString; Lines: String);

    procedure FromString(Str: String);

    property Name: ShortString read FName;
  end;




implementation

uses
  sgeStringList, sgeFileUtils;


constructor TsgeShellScript.Create(Name: ShortString);
begin
  inherited Create;

  //Запомнить параметры
  FName := Name;
end;


constructor TsgeShellScript.Create(Name: ShortString; Lines: String);
begin
  Create(Name);

  //Загрузить из строки
  FromString(Lines);
end;


procedure TsgeShellScript.FromString(Str: String);
var
  List, Line: TsgeStringList;
  i, j: Integer;
begin
  List := TsgeStringList.Create;
  List.Separator := sgeLineEnd;
  List.FromString(Str);

  Line := TsgeStringList.Create;
  Line.Separator := ';';

  try
    //Очистить строки
    Clear;

    //Добавить строки
    for i := 0 to List.Count - 1 do
    begin
      if List.Part[i] = '' then
        Add(List.Part[i])
      else
      begin
        Line.FromString(List.Part[i]);
        for j := 0 to Line.Count - 1 do
          Add(Line.Part[j]);
      end;
    end;

  finally
    Line.Free;
    List.Free;
  end;
end;




end.

