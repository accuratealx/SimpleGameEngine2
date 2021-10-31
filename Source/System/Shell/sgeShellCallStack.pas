{
Пакет             Simple Game Engine 2
Файл              sgeShellCallStack.pas
Версия            1.1
Создан            28.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Стек вызовов скрипта оболочки
}
{$Include Defines.inc}

unit sgeShellCallStack;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeShellCallStackItem;


type
  TsgeShellCallStack = class(specialize TsgeTemplateCollection<TsgeShellStackItem>)
  private
  public
    function Add(ScriptName: ShortString; Pos: Integer = 0): TsgeShellStackItem;

    function  GetLast: TsgeShellStackItem;
    procedure DeleteLast;
  end;




implementation


function TsgeShellCallStack.Add(ScriptName: ShortString; Pos: Integer): TsgeShellStackItem;
begin
  Result := TsgeShellStackItem.Create(ScriptName, Pos);

  inherited Add(Result);
end;


function TsgeShellCallStack.GetLast: TsgeShellStackItem;
begin
  Result := nil;

  if FCount > 0 then Result := FList[FCount - 1];
end;


procedure TsgeShellCallStack.DeleteLast;
begin
  if FCount > 0 then Delete(FCount - 1);
end;



end.

