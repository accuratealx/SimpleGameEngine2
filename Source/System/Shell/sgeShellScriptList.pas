{
Пакет             Simple Game Engine 2
Файл              sgeShellScriptList.pas
Версия            1.0
Создан            24.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список сценариев оболочки
}
{$Include Defines.inc}

unit sgeShellScriptList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateObjectCollection, sgeShellScript;


type
  TsgeShellScriptListTemplate = specialize TsgeTemplateObjectCollection<TsgeShellScript>;


  TsgeShellScriptList = class(TsgeShellScriptListTemplate)
  private
  public
    function IndexOf(Name: ShortString): Integer;
    function GetByName(Name: ShortString): TsgeShellScript;

    procedure Delete(Name: ShortString);
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'ShellScriptList';

  Err_ScriptNotFound = 'ScriptNotFound';


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


procedure TsgeShellScriptList.Delete(Name: ShortString);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ScriptNotFound, Name);

  inherited Delete(Idx);
end;


end.

