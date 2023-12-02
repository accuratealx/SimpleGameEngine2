{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberList.pas
Версия            1.4
Создан            25.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeCollection, sgeEventBase, sgeEventSubscriber;


type
  TsgeEventSubscriberList = class(specialize TsgeTemplateThreadSafeCollection<TsgeEventSubscriber>)
  private
    procedure Sort;

  public
    procedure Lock;
    procedure Unlock;

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
  FCS.Enter;
  try
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
  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeEventSubscriberList.Unlock;
begin
  FCS.Leave;
end;


function TsgeEventSubscriberList.IndexOfHandler(Handler: TsgeEventHandler): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := -1;

    for i := 0 to FCount - 1 do
      if FList[i].Handler = Handler then
        Exit(i);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberList.Add(Subscriber: TsgeEventSubscriber);
begin
  FCS.Enter;
  try
    //Проверить есть ли уже подписчик
    if IndexOfHandler(Subscriber.Handler) <> -1 then
      Exit;

    //Добавить элемент
    inherited Add(Subscriber);

    //Упорядочить
    Sort;

  finally
    FCS.Leave;
  end;
end;


function TsgeEventSubscriberList.Add(Handler: TsgeEventHandler; Priority: Word; Enable: Boolean): TsgeEventSubscriber;
begin
  FCS.Enter;
  try
    //Проверить есть ли уже подписчик
    if IndexOfHandler(Handler) <> -1 then
      Exit;

    //Создать подписчика
    Result := TsgeEventSubscriber.Create(Handler, Priority, Enable);

    //Добавить элемент
    Add(Result);

    //Упорядочить
    Sort;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberList.Delete(Index: Integer);
begin
  FCS.Enter;
  try
    inherited Delete(Index);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberList.Delete(Handler: TsgeEventHandler);
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    Idx := IndexOfHandler(Handler);
    if Idx <> -1 then
      Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberList.Delete(Obj: TObject);
var
  i: Integer;
begin
  FCS.Enter;
  try
    i := -1;
    while i < FCount - 1 do
    begin
      Inc(i);

      if TObject(TMethod(FList[i].Handler).Data) = Obj then
      begin
        Delete(i);
        Dec(i)
      end;
    end;

  finally
    FCS.Leave;
  end;
end;




end.
