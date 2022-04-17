{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberGroupList.pas
Версия            1.5
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка групп подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberGroupList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeCollection,
  sgeEventBase, sgeEventSubscriber, sgeEventSubscriberGroup;


type
  TsgeEventSubscriberGroupList = class(specialize TsgeTemplateThreadSafeCollection<TsgeEventSubscriberGroup>)
  public
    procedure Lock;
    procedure Unlock;

    function  IndexOf(Name: ShortString): Integer;

    function  Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True): TsgeEventSubscriber;

    procedure UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler); //Отписаться от события по имени и обработчику
    procedure UnSubscribe(EventName: ShortString; Obj: TObject);    //Отписаться от события по имени и объекту
    procedure UnSubscribe(Handler: TsgeEventHandler);               //Отписаться от событий по обработчику
    procedure UnSubscribe(Obj: TObject);                            //Отписаться от событий по объекту
  end;


implementation




procedure TsgeEventSubscriberGroupList.Lock;
begin
  FCS.Enter;
end;


procedure TsgeEventSubscriberGroupList.Unlock;
begin
  FCS.Leave;
end;


function TsgeEventSubscriberGroupList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try

    Result := -1;

    //Поиск совпадения по имени
    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if Name = LowerCase(FList[i].Name) then
        Exit(i);

  finally
    FCS.Leave;
  end;
end;


function TsgeEventSubscriberGroupList.Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word; Enable: Boolean): TsgeEventSubscriber;
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Найти индекс группы
    Idx := IndexOf(EventName);

    //Если нет группы, то создать
    if Idx = -1 then
    begin
      Add(TsgeEventSubscriberGroup.Create(EventName));
      Idx := FCount - 1;
    end;

    //Добавить подписчика
    Result := FList[Idx].Subscribers.Add(Handler, Priority, Enable);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Поиск группы
    Idx := IndexOf(EventName);

    //Группа не найдена
    if Idx = -1 then
      Exit;

    //Удалить подписчика
    FList[Idx].Subscribers.Delete(Handler);

    //Проверить на пустую группу
    if FList[Idx].Subscribers.Count = 0 then
      Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Obj: TObject);
var
  Idx: Integer;
begin
  FCS.Enter;
  try

    //Поиск группы
    Idx := IndexOf(EventName);

    //Группа не найдена
    if Idx = -1 then
      Exit;

    //Удалить подписчика
    FList[Idx].Subscribers.Delete(Obj);

    //Проверить на пустую группу
    if FList[Idx].Subscribers.Count = 0 then
      Delete(Idx);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Handler: TsgeEventHandler);
var
  i: Integer;
begin
  FCS.Enter;
  try

    i := -1;
    while i < FCount - 1 do
    begin
      Inc(i);

      //Удалить подписчика в группе
      FList[i].Subscribers.Delete(Handler);

      //Проверить на пустую группу
      if FList[i].Subscribers.Count = 0 then
      begin
        Delete(i);
        Dec(i)
      end;
    end;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Obj: TObject);
var
  i: Integer;
begin
  FCS.Enter;
  try

    i := -1;
    while i < FCount - 1 do
    begin
      Inc(i);

      //Удалить подписчика в группе по объекту
      FList[i].Subscribers.Delete(Obj);

      //Проверить на пустую группу
      if FList[i].Subscribers.Count = 0 then
      begin
        Delete(i);
        Dec(i);
      end;
    end;

  finally
    FCS.Leave;
  end;
end;


end.

