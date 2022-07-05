{
Пакет             Simple Game Engine 2
Файл              sgeTemplateList.pas
Версия            1.3
Создан            13.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс-шаблон: Связный список
}
{$Include Defines.inc}

unit sgeTemplateList;

{$mode objfpc}{$H+}

interface

type
  generic TsgeTemplateList<T> = class
  protected
    type
      PListItem = ^TListItem;
      TListItem = record                                            //Одна запись под хранение элемента
        Item: T;
        Prev: PListItem;
        Next: PListItem;
      end;

  protected
    FFreeObjects: Boolean;                                          //Удалять объекты
    FCount: Integer;                                                //Количество элементов
    FFirst: PListItem;                                              //Первый элемент
    FLast: PListItem;                                               //Последний элемент

    //Вспомогательные методы
    function  GetItemByIndex(Index: Integer): PListItem;            //Найти указатель на элемент по индексу
    procedure DeleteItemByPointer(Item: PListItem; FreeObject: Boolean);

    //Свойства
    function GetItem(Index: Integer): T;
  public
    constructor Create(FreeObjects: Boolean = False);
    destructor  Destroy; override;

    procedure Clear;
    procedure Add(Item: T);
    procedure Delete(Index: Integer);
    procedure Delete(Item: T);
    procedure Remove(Index: Integer);
    procedure Remove(Item: T);
    procedure Insert(Index: Integer; Item: T);

    property Count: Integer read FCount;
    property Item[Index: Integer]: T read GetItem;
  end;



const
  _UNITNAME = 'TemplateList';

  Err_IndexOutOfBounds  = 'IndexOutOfBounds';
  Err_ItemNotFund       = 'ItemNotFund';


implementation

uses
  sgeErrors, sgeTypes, sgeSystemUtils;


function TsgeTemplateList.GetItemByIndex(Index: Integer): PListItem;
var
  Idx: Integer;
  P: PListItem;
  Direction: TsgeDirection;
begin
  //Значение по умолчаню
  Result := nil;

  //Определить направление поиска
  if Index > FCount div 2 then
    Direction := dBackward
  else
    Direction := dForward;

  //Поиск указателя на элемент
  case Direction of

    dForward:
    begin
      Idx := 0;
      P := FFirst;
      while P <> nil do
        begin
        if Idx = Index then
        begin
          Result := P;
          Break;
        end;

        Inc(Idx);
        P := P^.Next;
        end;
    end;

    dBackward:
    begin
      Idx := FCount - 1;
      P := FLast;
      while P <> nil do
      begin
        if Idx = Index then
        begin
          Result := P;
          Break;
        end;

        Dec(Idx);
        P := P^.Prev;
      end;
    end;

  end;
end;


procedure TsgeTemplateList.DeleteItemByPointer(Item: PListItem; FreeObject: Boolean);
begin
  //Поправить ссылки
  if Item^.Next <> nil then
    Item^.Next^.Prev := Item^.Prev
  else
    FLast := Item^.Prev;

  if Item^.Prev <> nil then
    Item^.Prev^.Next := Item^.Next
  else
    FFirst := Item^.Next;

  //Удалить память объекта
  if FreeObject then
    TObject(Item^.Item).Free;

  //Удалить память текущей записи
  Dispose(Item);

  //Уменьшить счётчик
  Dec(FCount);
end;


function TsgeTemplateList.GetItem(Index: Integer): T;
var
  P: PListItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель по индексу
  P := GetItemByIndex(Index);
  Result := P^.Item;
end;


constructor TsgeTemplateList.Create(FreeObjects: Boolean);
begin
  FFreeObjects := FreeObjects;
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;


destructor TsgeTemplateList.Destroy;
begin
  Clear;
end;


procedure TsgeTemplateList.Clear;
var
  P, D: PListItem;
begin
  if FCount = 0 then
    Exit;

  //Пробежать по элементам
  P := FFirst;
  while P <> nil do
  begin
    //Освободить память объекта
    if FFreeObjects then
      TObject(P^.Item).Free;

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


procedure TsgeTemplateList.Add(Item: T);
var
  I: PListItem;
begin
  //Подготовить данные
  New(I);
  I^.Item := Item;
  I^.Prev := nil;
  I^.Next := nil;

  //Добавить элемент
  if FCount = 0 then
  begin
    //Первый элемент
    FFirst := I;
    FLast := I;
  end
  else
  begin
    FLast^.Next := I;
    I^.Prev := FLast;
    FLast := I;
  end;

  //Увеличить количество
  Inc(FCount);
end;


procedure TsgeTemplateList.Delete(Index: Integer);
var
  P: PListItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель на элемент
  P := GetItemByIndex(Index);

  //Удалить элемент
  DeleteItemByPointer(P, FFreeObjects);
end;


procedure TsgeTemplateList.Delete(Item: T);
var
  Idx: Integer;
  P: PListItem;
begin
  Idx := 0;
  P := FFirst;
  while P <> nil do
  begin
    if P^.Item = Item then
    begin
      //Удалить элемент
      DeleteItemByPointer(P, FFreeObjects);

      //Выход
      Exit;
    end;

    Inc(Idx);
    P := P^.Next;
  end;

  //Ошибка если не найдено совпадение
  raise EsgeException.Create(_UNITNAME, Err_ItemNotFund);
end;


procedure TsgeTemplateList.Remove(Index: Integer);
var
  P: PListItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель на элемент
  P := GetItemByIndex(Index);

  //Удалить элемент
  DeleteItemByPointer(P, False);
end;


procedure TsgeTemplateList.Remove(Item: T);
var
  Idx: Integer;
  P: PListItem;
begin
  Idx := 0;
  P := FFirst;
  while P <> nil do
  begin
    if P^.Item = Item then
    begin
      //Удалить элемент
      DeleteItemByPointer(P, False);

      //Выход
      Exit;
    end;

    Inc(Idx);
    P := P^.Next;
  end;

  //Ошибка если не найдено совпадение
  raise EsgeException.Create(_UNITNAME, Err_ItemNotFund);
end;


procedure TsgeTemplateList.Insert(Index: Integer; Item: T);
var
  PRight, PLeft, PNew: PListItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Подготовить новый элемент
  New(PNew);
  PNew^.Item := Item;
  PNew^.Next := nil;
  PNew^.Prev := nil;

  //Найти указатель на элемент
  PRight := GetItemByIndex(Index);

  //Ссылка на элемент слева
  PLeft := PRight^.Prev;

  //Изменение связей
  PRight^.Prev := PNew;
  PNew^.Next := PRight;

  if PLeft <> nil then
  begin
    PNew^.Prev := PLeft;
    PLeft^.Next := PNew;
  end
  else
  begin
    //0 элемент
    PNew^.Prev := nil;
    FFirst := PNew;
  end;

  //Увеличить счётчик
  Inc(FCount);
end;



end.


