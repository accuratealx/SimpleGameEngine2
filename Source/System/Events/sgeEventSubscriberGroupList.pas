{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberGroupList.pas
Версия            1.1
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка групп подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberGroupList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection, sgeEventBase, sgeEventSubscriberGroup;

type
  //Шаблон списка
  TsgeEventSubscriberGroupListTemplate = specialize TsgeTemplateCollection<TsgeEventSubscriberGroup>;


  //Список групп подписчиков
  TsgeEventSubscriberGroupList = class(TsgeEventSubscriberGroupListTemplate)
  private
    procedure ClearItem; override;

  public
    function IndexOf(Name: ShortString): Integer;

    procedure Clear;
    procedure Add(Name: String);
    procedure Delete(Index: Integer);

    procedure Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word = 0; Enable: Boolean = True);

    procedure UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
    procedure UnSubscribe(EventName: ShortString; Obj: TObject);
    procedure UnSubscribe(Handler: TsgeEventHandler);
    procedure UnSubscribe(Obj: TObject);
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'EventSubscriberGroupList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';


procedure TsgeEventSubscriberGroupList.ClearItem;
var
  i: Integer;
begin
  //Удалить объекты
  for i := 0 to FCount - 1 do
    FList[i].Free;

  //Обнулить массив
  inherited ClearItem;
end;


function TsgeEventSubscriberGroupList.IndexOf(Name: ShortString): Integer;
var
  i: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if Name = LowerCase(FList[i].Name) then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeEventSubscriberGroupList.Clear;
begin
  ClearItem;
end;


procedure TsgeEventSubscriberGroupList.Add(Name: String);
begin
  AddItem(TsgeEventSubscriberGroup.Create(Name));
end;


procedure TsgeEventSubscriberGroupList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Удалить память объекта
  FList[Index].Free;

  //Удалить элемент
  DeleteItem(Index);
end;


procedure TsgeEventSubscriberGroupList.Subscribe(EventName: ShortString; Handler: TsgeEventHandler; Priority: Word; Enable: Boolean);
var
  Idx: Integer;
begin
  //Найти индекс группы
  Idx := IndexOf(EventName);

  //Если нет группы, то создать
  if Idx = -1 then
    begin
    Add(EventName);
    Idx := FCount - 1;
    end;

  //Добавить подписчика
  FList[Idx].Subscribers.Add(Handler, Priority, Enable);
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Handler: TsgeEventHandler);
var
  Idx: Integer;
begin
  //Поиск группы
  Idx := IndexOf(EventName);

  //Группа не найдена
  if Idx = -1 then Exit;

  //Удалить подписчика
  FList[Idx].Subscribers.Delete(Handler);

  //Проверить на пустую группу
  if FList[Idx].Subscribers.Count = 0 then Delete(Idx);
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(EventName: ShortString; Obj: TObject);
var
  Idx: Integer;
begin
  //Поиск группы
  Idx := IndexOf(EventName);

  //Группа не найдена
  if Idx = -1 then Exit;

  //Удалить подписчика
  FList[Idx].Subscribers.Delete(Obj);

  //Проверить на пустую группу
  if FList[Idx].Subscribers.Count = 0 then Delete(Idx);
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Handler: TsgeEventHandler);
var
  i: Integer;
begin
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
end;


procedure TsgeEventSubscriberGroupList.UnSubscribe(Obj: TObject);
var
  i: Integer;
begin
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
      Dec(i)
      end;
    end;
end;


end.

