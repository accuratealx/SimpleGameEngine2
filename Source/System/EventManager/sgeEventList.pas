{
Пакет             Simple Game Engine 2
Файл              sgeEventList.pas
Версия            1.1
Создан            20.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список объектов событий
}
{$Include Defines.inc}

unit sgeEventList;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  sgeErrors, sgeTemplateList, sgeEventBase;


type
  //Запись для хранения одного элемента события
  TsgeEventListItem = record
      Name: String;                                           //Имя события
      Obj: TsgeEventBase;                                     //Объект события
      class operator = (A, B: TsgeEventListItem): Boolean;
    end;
  PsgeEventListItem = ^TsgeEventListItem;


  TsgeEventListTemplate = specialize TsgeTemplateList<TsgeEventListItem>;


  //Список событий
  TsgeEventList = class(TsgeEventListTemplate)
  private
    procedure ClearItem; override;

  public
    procedure Clear;
    procedure Add(Name: String; Obj: TsgeEventBase);
    procedure Delete(Index: Integer);
  end;


implementation

uses
  sgeSystemUtils;

const
  _UNITNAME = 'EventList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';



class operator TsgeEventListItem. = (A, B: TsgeEventListItem): Boolean;
begin
  Result := (LowerCase(A.Name) = LowerCase(B.Name));
end;



procedure TsgeEventList.ClearItem;
var
  P, D: PListItem;
begin
  if FCount = 0 then Exit;

  //Пробежать по элементам
  P := FFirst;
  while P <> nil do
    begin
    //Удалить объект
    P^.Item.Obj.Free;

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


procedure TsgeEventList.Clear;
begin
  ClearItem;
end;


procedure TsgeEventList.Add(Name: String; Obj: TsgeEventBase);
var
  I: TsgeEventListItem;
begin
  //Подготовить данные
  I.Name := Name;
  I.Obj := Obj;

  //Добавить элемент
  AddItem(I);
end;


procedure TsgeEventList.Delete(Index: Integer);
var
  P: PListItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель на элемент
  P := GetItemByIndex(Index);

  //Освободить память объекта
  P^.Item.Obj.Free;

  //Удалить элемент по указателю
  DeleteItemByPointer(P);
end;


end.

