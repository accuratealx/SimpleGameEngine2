{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandList.pas
Версия            1.0
Создан            30.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка команд оболочки
}
{$Include Defines.inc}

unit sgeShellCommandList;

{$mode objfpc}{$H+}

interface

uses
  sgeShellCommand, sgeTemplateObjectCollection;

type
  TsgeShellCommandListTemplate = specialize TsgeTemplateObjectCollection<TsgeShellCommand>;


  TsgeShellCommandList = class(TsgeShellCommandListTemplate)
  public
    function  IndexOf(Name: ShortString): Integer;

    procedure Add(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word; Group: ShortString);
  end;


implementation


function TsgeShellCommandList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if LowerCase(FList[i].Name) = Name then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeShellCommandList.Add(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word; Group: ShortString);
begin
  inherited Add(TsgeShellCommand.Create(Name, Proc, MinParamCount, Group));
end;


end.

