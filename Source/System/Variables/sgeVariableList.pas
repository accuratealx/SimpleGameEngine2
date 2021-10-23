{
Пакет             Simple Game Engine 2
Файл              sgeVariableList.pas
Версия            1.2
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка переменных
}
{$Include Defines.inc}

unit sgeVariableList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeVariableBase;


type
  TsgeVariableList = class(specialize TsgeTemplateCollection<TsgeVariableBase>)
  public
    function  IndexOf(Name: ShortString): Integer;

    procedure DeleteNonSystem;
    procedure Delete(Name: ShortString);
  end;


implementation



function TsgeVariableList.IndexOf(Name: ShortString): Integer;
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


procedure TsgeVariableList.DeleteNonSystem;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
    begin
    if FList[i].Imbedded = False then
      inherited Delete(i);
    end;
end;


procedure TsgeVariableList.Delete(Name: ShortString);
var
  Idx: Integer;
begin
  //Индекс переменной
  Idx := IndexOf(Name);

  //Если нет переменной, то выход
  if Idx = -1 then Exit;

  //Освободить память объекта
  FList[Idx].Free;

  //Удалить переменную
  inherited Delete(Idx);
end;





end.

