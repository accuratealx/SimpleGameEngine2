{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberList.pas
Версия            1.0
Создан            25.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberList;

{$mode objfpc}{$H+}

interface

uses
  sgeEventHandlerList;


type
  //Элемент подписки
  TsgeEventSubscriberItem = record
    Name: String;                                                         //Имя события
    List: TsgeEventHandlerList;                                           //Список подписчиков
  end;


  //Список подписчиков
  TsgeEventSubscriberList = class
  private
    FCount: Integer;
    FList: array of TsgeEventSubscriberItem;

    function  GetItem(Index: Integer): TsgeEventSubscriberItem;
    function  GetHandlerList(EventName: String): TsgeEventHandlerList;    //Вернуть указатель на список обработчиков

    procedure AddItem(EventName: String);
    procedure DeleteItem(Index: Integer);
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(EventName: String): Integer;                        //Найти индекс по имени
    procedure Clear;                                                      //Очистить список

    procedure Add(EventName: String; EventHandler: TsgeEventHandler);     //Добавить подписчика
    procedure Delete(EventName: String; EventHandler: TsgeEventHandler);  //Удалить подписчика по методу
    procedure Delete(EventName: String; Obj: TObject);                    //Удалить подписчика по объекту
    procedure Delete(Obj: TObject);                                       //Удалить все подписки по объекту

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeEventSubscriberItem read GetItem;
    property HandlerList[EventName: String]: TsgeEventHandlerList read GetHandlerList;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'EventSubscriberList';


  Err_IndexOutOfBounds = 'IndexOutOfBounds';


function TsgeEventSubscriberList.GetItem(Index: Integer): TsgeEventSubscriberItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


function TsgeEventSubscriberList.GetHandlerList(EventName: String): TsgeEventHandlerList;
var
  Idx: Integer;
begin
  Result := nil;

  //Поиск индекса
  Idx := IndexOf(EventName);

  //Если есть событие, то вернуть список подписчиков
  if Idx <> -1 then Result := FList[Idx].List;
end;


procedure TsgeEventSubscriberList.DeleteItem(Index: Integer);
var
  i, c: Integer;
begin
  //Проверить индекс
  c := FCount - 1;
  if (Index < 0) or (Index > c) then Exit;

  //Удалить список
  FList[Index].List.Free;

  //Сместить элементы на место удалённого
  for i := Index to c - 1 do
    FList[i] := FList[i + 1];

  //Удалить элемент массива
  SetLength(FList, c);

  //Уменьшить счётчик
  Dec(FCount);
end;


procedure TsgeEventSubscriberList.AddItem(EventName: String);
begin
  //Добавить элемент массива
  SetLength(FList, FCount + 1);

  //Задать поля
  FList[FCount].Name := EventName;
  FList[FCount].List := TsgeEventHandlerList.Create;

  //Увеличить счётчик
  Inc(FCount);
end;


constructor TsgeEventSubscriberList.Create;
begin
  FCount := 0;
end;


destructor TsgeEventSubscriberList.Destroy;
begin
  Clear;
end;


function TsgeEventSubscriberList.IndexOf(EventName: String): Integer;
  var
  i: Integer;
begin
  Result := -1;

  //В нижний регистр имя события
  EventName := LowerCase(EventName);

  //Просмотреть массив на совпадение
  for i := 0 to Fcount - 1 do
    if EventName = LowerCase(FList[i].Name) then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeEventSubscriberList.Clear;
var
  i: Integer;
begin
  //Удалить списки
  for i := 0 to FCount - 1 do
    FList[i].List.Free;

  //Очистить массив
  FList := nil;
end;


procedure TsgeEventSubscriberList.Add(EventName: String; EventHandler: TsgeEventHandler);
var
  Idx: Integer;
begin
  //Поиск индекса
  Idx := IndexOf(EventName);

  //Если нет такого события, то добавить
  if Idx = -1 then AddItem(EventName);

  //Добавить подписчика в список
  FList[FCount - 1].List.Add(EventHandler);
end;


procedure TsgeEventSubscriberList.Delete(EventName: String; EventHandler: TsgeEventHandler);
var
  Idx: Integer;
begin
  //Поиск индекса
  Idx := IndexOf(EventName);

  //Нет такого события, выход
  if Idx = -1 then Exit;

  //Удалить подписчика по методу
  FList[Idx].List.Delete(EventHandler);

  //Проверить на 0 подписок
  if FList[Idx].List.Count = 0 then DeleteItem(Idx);
end;


procedure TsgeEventSubscriberList.Delete(EventName: String; Obj: TObject);
var
  Idx: Integer;
begin
  //Поиск индекса
  Idx := IndexOf(EventName);

  //Нет такого события, выход
  if Idx = -1 then Exit;

  //Удалить подписчика по объекту
  FList[Idx].List.Delete(Obj);

  //Проверить на 0 подписок
  if FList[Idx].List.Count = 0 then DeleteItem(Idx);
end;


procedure TsgeEventSubscriberList.Delete(Obj: TObject);
var
  i: Integer;
begin
  i := 0;
  while i <= FCount - 1 do
    begin
    //Удалить подписки по объекту
    FList[i].List.Delete(Obj);

    //Проверить на пустой список
    if FList[i].List.Count = 0 then
      begin
      DeleteItem(i);
      Dec(i);
      end;

    Inc(i);
    end;
end;


end.

