{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementList.pas
Версия            1.1
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
  TsgeGraphicElementListTemplate = specialize TsgeTemplateList<TsgeGraphicElementBase>;


  TsgeGraphicElementList = class(TsgeGraphicElementListTemplate)
  private
    FIterator: PListItem;

  public
    procedure ClearItem; override;

    procedure Add(Element: TsgeGraphicElementBase);

    function  GetFirst: TsgeGraphicElementBase;           //Взять первый элемент
    function  GetNext: TsgeGraphicElementBase;            //Следующий элемент
    procedure DeleteCurrentElement;                       //Удалить текущий элемент
  end;


implementation


procedure TsgeGraphicElementList.ClearItem;
var
  P, D: PListItem;
begin
  if FCount = 0 then Exit;

  //Пробежать по элементам
  P := FFirst;
  while P <> nil do
    begin
    //Удалить объект
    P^.Item.Free;

    //Освободить память
    D := P;
    P := P^.Next;
    Dispose(D);
    end;

  //Поправить параметры
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;


procedure TsgeGraphicElementList.Add(Element: TsgeGraphicElementBase);
begin
  AddItem(Element);
end;


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

  //Почистить память объекта
  P^.Item.Free;

  //Удалить текущий элемент
  DeleteItemByPointer(P);
end;

end.

