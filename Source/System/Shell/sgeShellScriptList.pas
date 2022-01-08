{
Пакет             Simple Game Engine 2
Файл              sgeShellScriptList.pas
Версия            1.3
Создан            24.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список сценариев оболочки
}
{$Include Defines.inc}

unit sgeShellScriptList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeShellScript;


type
  TsgeShellScriptList = class(specialize TsgeTemplateCollection<TsgeShellScript>)
  private
  public
    function IndexOf(Name: ShortString): Integer;
    function GetByName(Name: ShortString): TsgeShellScript;

    procedure Add(Name: ShortString; Lines: String);
    procedure Delete(Name: ShortString);
    procedure SafeDelete(Name: ShortString);
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ShellScriptList';

  Err_ScriptNotFound      = 'ScriptNotFound';


function TsgeShellScriptList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if Name = LowerCase(FList[i].Name) then
      begin
      Result := i;
      Break;
      end;
end;


function TsgeShellScriptList.GetByName(Name: ShortString): TsgeShellScript;
var
  i: Integer;
begin
  Result := nil;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if Name = LowerCase(FList[i].Name) then
      begin
      Result := FList[i];
      Break;
      end;
end;


procedure TsgeShellScriptList.Add(Name: ShortString; Lines: String);
var
  Idx: Integer;
begin
  //Проверить на дубликат
  Idx := IndexOf(Name);
  if Idx <> -1 then FList[Idx].FromString(Lines)
    else inherited Add(TsgeShellScript.Create(Name, Lines));
end;


procedure TsgeShellScriptList.Delete(Name: ShortString);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ScriptNotFound, Name);

  inherited Delete(Idx);
end;


procedure TsgeShellScriptList.SafeDelete(Name: ShortString);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx <> -1 then
    inherited Delete(Idx);
end;


end.

