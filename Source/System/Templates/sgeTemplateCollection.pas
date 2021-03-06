{
Пакет             Simple Game Engine 2
Файл              sgeTemplateObjectCollection.pas
Версия            1.1
Создан            29.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс-шаблон: Коллекция
}
{$Include Defines.inc}

unit sgeTemplateCollection;

{$mode objfpc}{$H+}

interface


type
  generic TsgeTemplateCollection<T> = class
  protected
    FFreeObjects: Boolean;
    FCount: Integer;
    FList: array of T;

    function GetItem(Index: Integer): T;
  public
    constructor Create(FreeObjects: Boolean = False);
    destructor  Destroy; override;

    procedure Clear;
    procedure Add(Item: T);
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; Item: T);

    property Count: Integer read FCount;
    property Item[Index: Integer]: T read GetItem;
  end;



const
  _UNITNAME = 'TemplateCollection';

  Err_IndexOutOfBounds = 'IndexOutOfBounds';


implementation

uses
  sgeErrors, sgeSystemUtils;


function TsgeTemplateCollection.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FList[Index];
end;


constructor TsgeTemplateCollection.Create(FreeObjects: Boolean);
begin
  FFreeObjects := FreeObjects;
  FCount := 0;
end;


destructor TsgeTemplateCollection.Destroy;
begin
  Clear;
end;


procedure TsgeTemplateCollection.Clear;
var
  i: Integer;
begin
  //Удалить память объектов
  if FFreeObjects then
    for i := FCount - 1 downto 0 do
      TObject(FList[i]).Free;

  //Поправить параметры
  FCount := 0;

  //Очистить массив
  SetLength(FList, FCount);
end;


procedure TsgeTemplateCollection.Add(Item: T);
begin
  Inc(FCount);
  SetLength(FList, FCount);
  FList[FCount - 1] := Item;
end;


procedure TsgeTemplateCollection.Delete(Index: Integer);
var
  c, i: Integer;
begin
  c := Fcount - 1;
  if (Index < 0) or (Index > FCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Удалить память объекта
  if FFreeObjects then
    TObject(FList[Index]).Free;

  //Сдвинуть хвост
  for i := Index to c - 1 do
    FList[i] := FList[i + 1];

  //Удалить последний элемент
  SetLength(FList, c);

  //Уменьшить счётчик
  Dec(FCount);
end;


procedure TsgeTemplateCollection.Insert(Index: Integer; Item: T);
var
  i: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Увеличить массив на один элемент
  SetLength(FList, FCount + 1);
  for i := FCount downto Index + 1 do
    FList[i] := FList[i - 1];

  //Вставить элемент
  FList[Index] := Item;

  //Увеличить счётчик
  Inc(FCount);
end;


end.


