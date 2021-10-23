{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementList.pas
Версия            1.2
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список элементов отисовки
}
{$Include Defines.inc}

unit sgeGraphicElementList;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicElementBase, sgeTemplateList;


type
  TsgeGraphicElementList = class(specialize TsgeTemplateList<TsgeGraphicElementBase>)
  private
    FIterator: PListItem;
  public
    function  GetFirst: TsgeGraphicElementBase;           //Взять первый элемент
    function  GetNext: TsgeGraphicElementBase;            //Следующий элемент
    procedure DeleteCurrentElement;                       //Удалить текущий элемент
  end;


implementation


function TsgeGraphicElementList.GetFirst: TsgeGraphicElementBase;
begin
  //Результат по умолчанию
  Result := nil;

  //Запомнить первый элемент
  FIterator := FFirst;

  //Проверить на пустое значение
  if FIterator = nil then Exit;

  Result := FIterator^.Item;
end;


function TsgeGraphicElementList.GetNext: TsgeGraphicElementBase;
begin
  Result := nil;

  //Проверить на конец списка
  if FIterator = nil then Exit;

  //Вернуть текущий элемент
  Result := FIterator^.Item;

  //Сместить на следующий элемент
  FIterator := FIterator^.Next;
end;


procedure TsgeGraphicElementList.DeleteCurrentElement;
var
  P: PListItem;
begin
  //Запомнить адрес текущего элемента
  P := FIterator;

  //Проверка на пустое значение
  if FIterator = nil then Exit;

  //Сместить указатель на следующий элемент
  FIterator := FIterator^.Next;

  //Удалить текущий элемент
  DeleteItemByPointer(P);
end;


end.

