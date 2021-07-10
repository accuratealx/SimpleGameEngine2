{
Пакет             Simple Game Engine 2
Файл              sgeEventList.pas
Версия            1.0
Создан            20.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список объектов событий
}
{$Include Defines.inc}

unit sgeEventList;

{$mode objfpc}{$H+}

interface

uses
  sgeErrors, sgeEventBase;


type
  //Запись для хранения одного элемента события
  TsgeEventListItem = record
      Name: String;                                   //Имя события
      Obj: TsgeEventBase;                             //Объект события
      Prev: ^TsgeEventListItem;                       //Предыдущий элемент
      Next: ^TsgeEventListItem;                       //Следующий элемент
    end;
  PsgeEventListItem = ^TsgeEventListItem;


  //Список событий
  TsgeEventList = class
  private
    FCount: Integer;                                  //Количество объектов
    FFirst: PsgeEventListItem;                        //Указатель на первый элемент
    FLast: PsgeEventListItem;                         //Указатель на последний элемент

    function  GetItem(Index: Integer): TsgeEventListItem;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    procedure Add(Name: String; Obj: TsgeEventBase);
    procedure Delete(Index: Integer);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeEventListItem read GetItem;
  end;


implementation

uses
  sgeSystemUtils;


const
  _UNITNAME = 'EventList';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';


function TsgeEventList.GetItem(Index: Integer): TsgeEventListItem;
var
  P: PsgeEventListItem;
  Idx: Integer;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель по индексу
  Idx := 0;
  P := FFirst;
  while P <> nil do
    begin
    if Idx = Index then
      begin
      Result := P^;
      Break;
      end;

    Inc(Idx);
    P := P^.Next;
    end;
end;


constructor TsgeEventList.Create;
begin
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;


destructor TsgeEventList.Destroy;
begin
  Clear;
end;


procedure TsgeEventList.Clear;
var
  P, D: PsgeEventListItem;
begin
  if FCount = 0 then Exit;

  //Пробежать по элементам
  P := FFirst;
  while p <> nil do
    begin
    //Прибить объект
    if P^.Obj <> nil then P^.Obj.Free;

    //Освободить память
    D := P;
    P := P^.Next;
    Dispose(D);
    end;

  //Обнулить счётчик
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;


procedure TsgeEventList.Add(Name: String; Obj: TsgeEventBase);
var
  P: PsgeEventListItem;
begin
  //Подготовить данные
  New(P);
  P^.Name := Name;
  P^.Obj := Obj;
  P^.Prev := nil;
  P^.Next := nil;

  //Добавить элемент
  if FCount = 0 then
    begin
    FFirst := P;
    FLast := P;
    end
    else begin
    FLast^.Next := P;
    P^.Prev := FLast;
    FLast := P;
    end;

  //Увеличить количество
  Inc(FCount);
end;


procedure TsgeEventList.Delete(Index: Integer);
var
  P: PsgeEventListItem;
  Idx: Integer;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Найти указатель на элемент
  Idx := 0;
  P := FFirst;
  while P <> nil do
    begin
    if Idx = Index then Break;
    Inc(Idx);
    P := P^.Next;
    end;

  //Удалить объект
  if P^.Obj <> nil then P^.Obj.Free;

  //Следующий элемент
  if P^.Next <> nil then P^.Next^.Prev := P^.Prev else FLast := P^.Prev;

  //Предыдущий элемент
  if P^.Prev <> nil then P^.Prev^.Next := P^.Next else FFirst := P^.Next;

  //Удалить память
  Dispose(P);

  //Уменьшить счётчик
  Dec(FCount);
end;


end.

