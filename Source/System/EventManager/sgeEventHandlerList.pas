{
Пакет             Simple Game Engine 2
Файл              sgeEventHandlerList.pas
Версия            1.0
Создан            25.03.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс списка обработчиков событий
}
{$Include Defines.inc}

unit sgeEventHandlerList;

{$mode objfpc}{$H+}

interface

uses
  sgeEventBase;


type
  //Обработчик события
  TsgeEventHandler = procedure(EventObj: TsgeEventBase) of object;


  //Список обработчиков
  TsgeEventHandlerList = class
  private
    FCount: Integer;
    FList: array of TsgeEventHandler;

    function GetItem(Index: Integer): TsgeEventHandler;
  public
    constructor Create;
    destructor  Destroy; override;

    function IndexOf(Proc: TsgeEventHandler): Integer;

    procedure Clear;
    procedure Add(Proc: TsgeEventHandler);
    procedure Delete(Index: Integer);
    procedure Delete(Proc: TsgeEventHandler);
    procedure Delete(Obj: TObject);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeEventHandler read GetItem;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'EventHandlerList';


  Err_IndexOutOfBounds = 'IndexOutOfBounds';


function TsgeEventHandlerList.GetItem(Index: Integer): TsgeEventHandler;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


constructor TsgeEventHandlerList.Create;
begin
  FCount := 0;
end;


destructor TsgeEventHandlerList.Destroy;
begin
  Clear;
end;


function TsgeEventHandlerList.IndexOf(Proc: TsgeEventHandler): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Fcount - 1 do
    if Proc = FList[i] then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeEventHandlerList.Clear;
begin
  FList := nil;
end;


procedure TsgeEventHandlerList.Add(Proc: TsgeEventHandler);
begin
  //Проверить на существование обработчика
  if IndexOf(Proc) <> -1 then Exit;

  //Увеличить массив
  SetLength(FList, FCount + 1);

  //Заполнить поля
  FList[FCount] := Proc;

  //Увеличить счётчик
  Inc(FCount);
end;


procedure TsgeEventHandlerList.Delete(Index: Integer);
var
  i, c: Integer;
begin
  //Проверить индекс
  c := FCount - 1;
  if (Index < 0) or (Index > c) then Exit;

  //Сместить элементы на место удалённого
  for i := Index to c - 1 do
    FList[i] := FList[i + 1];

  //Удалить элемент массива
  SetLength(FList, c);

  //Уменьшить счётчик
  Dec(FCount);
end;


procedure TsgeEventHandlerList.Delete(Proc: TsgeEventHandler);
var
  Idx: Integer;
begin
  Idx := IndexOf(Proc);
  Delete(Idx);
end;


procedure TsgeEventHandlerList.Delete(Obj: TObject);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if TMethod(FList[i]).Data = Pointer(Obj) then
      begin
      Delete(i);
      Break;
      end;
end;


end.

