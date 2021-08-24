{
Пакет             Simple Game Engine 2
Файл              sgeShellScript.pas
Версия            1.0
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
  TsgeShellScriptTemplate = specialize TsgeTemplateCollection<String>;


  TsgeShellScript = class(TsgeShellScriptTemplate)
  private
    FName: ShortString;                                             //Имя скрипта
  public
    constructor Create(Name: ShortString);
    constructor Create(Name: ShortString; Lines: String);

    procedure FromString(Str: String);

    property Name: ShortString read FName;
  end;




implementation

uses
  sgeStringList;


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
  List: TsgeStringList;
  i: Integer;
begin
  List := TsgeStringList.Create;
  List.Separator := #13#10;
  List.FromString(Str);

  for i := 0 to List.Count - 1 do
    Add(List.Part[i]);

  List.Free;
end;




end.

