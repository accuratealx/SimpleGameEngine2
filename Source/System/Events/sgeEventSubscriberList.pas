{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberList.pas
Версия            1.3
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
  TsgeEventSubscriberList = class(specialize TsgeTemplateCollection<TsgeEventSubscriber>)
  private
    procedure Sort;

  public
    function IndexOfHandler(Handler: TsgeEventHandler): Integer;

    procedure Add(Subscriber: TsgeEventSubscriber);
    function  Add(Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True): TsgeEventSubscriber;
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


function TsgeEventSubscriberList.IndexOfHandler(Handler: TsgeEventHandler): Integer;
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


procedure TsgeEventSubscriberList.Add(Subscriber: TsgeEventSubscriber);
begin
  //Проверить есть ли уже подписчик
  if IndexOfHandler(Subscriber.Handler) <> -1 then Exit;

  //Добавить элемент
  inherited Add(Subscriber);

  //Упорядочить
  Sort;
end;


function TsgeEventSubscriberList.Add(Handler: TsgeEventHandler; Priority: Word; Enable: Boolean): TsgeEventSubscriber;
begin
  //Проверить есть ли уже подписчик
  if IndexOfHandler(Handler) <> -1 then Exit;

  //Создать подписчика
  Result := TsgeEventSubscriber.Create(Handler, Priority, Enable);

  //Добавить элемент
  Add(Result);

  //Упорядочить
  Sort;
end;


procedure TsgeEventSubscriberList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;


procedure TsgeEventSubscriberList.Delete(Handler: TsgeEventHandler);
begin
  Delete(IndexOfHandler(Handler));
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
