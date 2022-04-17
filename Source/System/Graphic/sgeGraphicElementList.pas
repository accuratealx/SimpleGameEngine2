{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementList.pas
Версия            1.3
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
    function  IndexOf(aItem: TsgeGraphicElementBase): PListItem;    //Найти элемент по Data
    procedure MoveToEnd(Element: TsgeGraphicElementBase);           //Переместить элемент в конец списка

    function  GetFirst: TsgeGraphicElementBase;                     //Взять первый элемент
    function  GetNext: TsgeGraphicElementBase;                      //Следующий элемент
    procedure DeleteCurrentElement;                                 //Удалить текущий элемент
  end;


implementation


function TsgeGraphicElementList.IndexOf(aItem: TsgeGraphicElementBase): PListItem;
var
  P: PListItem;
begin
  Result := nil;

  //Поиск по полезной нагрузке
  P := FFirst;
  while P <> nil do
  begin
    if P^.Item = aItem then
      Exit(P);
    P := P^.Next;
  end;
end;


procedure TsgeGraphicElementList.MoveToEnd(Element: TsgeGraphicElementBase);
var
  PItem: PListItem;
  Data: TsgeGraphicElementBase;
begin
  //Найти указатель на запись
  PItem := IndexOf(Element);
  if pItem <> nil then
  begin
    //Ссылка на данные
    Data := PItem^.Item;

    //Поправить ссылки списка
    if PItem^.Next <> nil then
      PItem^.Next^.Prev := PItem^.Prev
    else
      FLast := PItem^.Prev;

    if PItem^.Prev <> nil then
      PItem^.Prev^.Next := PItem^.Next
    else
      FFirst := PItem^.Next;

    //Удалить память текущей записи
    Dispose(PItem);

    //Уменьшить счётчик
    Dec(FCount);

    //Добавить элемент в хвост
    Add(Data);
  end;
end;


function TsgeGraphicElementList.GetFirst: TsgeGraphicElementBase;
begin
  //Результат по умолчанию
  Result := nil;

  //Запомнить первый элемент
  FIterator := FFirst;

  //Проверить на пустое значение
  if FIterator <> nil then
    Result := FIterator^.Item;
end;


function TsgeGraphicElementList.GetNext: TsgeGraphicElementBase;
begin
  //Результат по умолчанию
  Result := nil;

  //Сместить на следующий элемент
  FIterator := FIterator^.Next;

  //Проверить на пустое значение
  if FIterator <> nil then
    Result := FIterator^.Item;
end;


procedure TsgeGraphicElementList.DeleteCurrentElement;
var
  P: PListItem;
begin
  //Запомнить адрес текущего элемента
  P := FIterator;

  //Проверка на пустое значение
  if FIterator = nil then
    Exit;

  //Сместить указатель на следующий элемент
  FIterator := FIterator^.Next;

  //Удалить текущий элемент
  DeleteItemByPointer(P);
end;


end.

