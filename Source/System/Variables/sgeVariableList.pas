{
Пакет             Simple Game Engine 2
Файл              sgeVariableList.pas
Версия            1.0
Создан            20.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка переменных
}
{$Include Defines.inc}

unit sgeVariableList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeVariableBase;


type
  TsgeVariableListTemplate = specialize TsgeTemplateCollection<TsgeVariableBase>;


  TsgeVariableList = class(TsgeVariableListTemplate)
  private
    procedure ClearItem; override;
  public
    function IndexOf(Name: ShortString): Integer;

    procedure Clear;

    procedure Delete(Name: ShortString);
  end;


implementation


procedure TsgeVariableList.ClearItem;
  var
  i: Integer;
begin
  //Удалить объекты
  for i := 0 to FCount - 1 do
    FList[i].Free;

  //Обнулить массив
  inherited ClearItem;
end;


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


procedure TsgeVariableList.Clear;
begin
  ClearItem;
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
  DeleteItem(Idx);
end;





end.

