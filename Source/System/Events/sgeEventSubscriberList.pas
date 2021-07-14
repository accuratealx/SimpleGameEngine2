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
  sgeTemplateCollection, sgeEventBase, sgeEventSubscriber;


type
  //Шаблон
  TsgeEventSubscriberTemplate = specialize TsgeTemplateCollection<TsgeEventSubscriber>;


  //Список подписчиков
  TsgeEventSubscriberList = class(TsgeEventSubscriberTemplate)
  private
    procedure Sort;
    procedure ClearItem; override;
  public
    function IndexOf(Handler: TsgeEventHandler): Integer;

    procedure Clear;

    procedure Add(Subscriber: TsgeEventSubscriber);
    procedure Add(Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True);
    procedure Delete(Index: Integer);
    procedure Delete(Handler: TsgeEventHandler);
    procedure Delete(Obj: TObject);
  end;


implementation


procedure TsgeEventSubscriberList.Sort;
var
  i, j, ci, cj: Integer;
  El: TsgeEventSubscriber;
begin
  ci := Fcount - 1;
  cj := ci - 1;
  for i := 0 to ci do
    for j := 0 to cj - i do
      if FList[j].Priority < FList[j + 1].Priority then
        begin
        El := FList[j];
        FList[j] := FList[j + 1];
        FList[j + 1] := El;
        end;
end;


procedure TsgeEventSubscriberList.ClearItem;
var
  i: Integer;
begin
  //Удалить объекты
  for i := 0 to FCount - 1 do
    FList[i].Free;

  //Обнулить массив
  inherited ClearItem;
end;


function TsgeEventSubscriberList.IndexOf(Handler: TsgeEventHandler): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    if FList[i].Handler = Handler then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeEventSubscriberList.Clear;
begin
  ClearItem;
end;


procedure TsgeEventSubscriberList.Add(Subscriber: TsgeEventSubscriber);
begin
  //Проверить есть ли уже подписчик
  if IndexOf(Subscriber.Handler) <> -1 then Exit;

  //Добавить элемент
  AddItem(Subscriber);

  //Упорядочить
  Sort;
end;


procedure TsgeEventSubscriberList.Add(Handler: TsgeEventHandler; Priority: Word; Enable: Boolean);
begin
  Add(TsgeEventSubscriber.Create(Handler, Priority, Enable));
end;


procedure TsgeEventSubscriberList.Delete(Index: Integer);
begin
  //Проверить индекс
  if (Index < 0) or (Index > FCount - 1) then Exit;

  //Удалить память объекта
  FList[Index].Free;

  //Удалить элемент
  DeleteItem(Index);
end;


procedure TsgeEventSubscriberList.Delete(Handler: TsgeEventHandler);
begin
  //Удалить элемент
  Delete(IndexOf(Handler));
end;


procedure TsgeEventSubscriberList.Delete(Obj: TObject);
var
  i: Integer;
begin
  i := -1;
  while i < FCount - 1 do
    begin
    Inc(i);

    if TMethod(FList[i].Handler).Data = @Obj then
      begin
      Delete(i);
      Dec(i)
      end;
    end;
end;




end.
