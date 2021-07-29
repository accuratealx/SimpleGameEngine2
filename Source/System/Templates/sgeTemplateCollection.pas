{
Пакет             Simple Game Engine 2
Файл              sgeTemplateCollection.pas
Версия            1.1
Создан            14.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс-шаблон: Коллекция
}

unit sgeTemplateCollection;

{$mode objfpc}{$H+}

interface


type
  generic TsgeTemplateCollection<T> = class
  protected
    FCount: Integer;
    FList: array of T;

    function GetItem(Index: Integer): T;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    //function  IndexOf(AItem: T): Integer;

    procedure ClearItem; virtual;

    procedure AddItem(Item: T);
    procedure DeleteItem(Index: Integer);
    procedure InsertItem(Index: Integer; Item: T);

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


constructor TsgeTemplateCollection.Create;
begin
  FCount := 0;
end;


destructor TsgeTemplateCollection.Destroy;
begin
  ClearItem;
end;


{function TsgeTemplateCollection.IndexOf(AItem: T): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FCount - 1 do
    begin
    if FList[i] = AItem then
      begin
      Result := i;
      Break;
      end;
    end;
end;}


procedure TsgeTemplateCollection.ClearItem;
begin
  //Поправить параметры
  FCount := 0;

  //Очистить массив
  SetLength(FList, FCount);
end;


procedure TsgeTemplateCollection.AddItem(Item: T);
begin
  Inc(FCount);
  SetLength(FList, FCount);
  FList[FCount - 1] := Item;
end;


procedure TsgeTemplateCollection.DeleteItem(Index: Integer);
var
  c, i: Integer;
begin
  c := Fcount - 1;
  if (Index < 0) or (Index > FCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Сдвинуть хвост
  for i := Index to c - 1 do
    FList[i] := FList[i + 1];

  //Удалить последний элемент
  SetLength(FList, c);

  //Уменьшить счётчик
  Dec(FCount);
end;


procedure TsgeTemplateCollection.InsertItem(Index: Integer; Item: T);
var
  i: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Раздвинуть
  SetLength(FList, FCount + 1);
  for i := FCount downto Index + 1 do
    FList[i] := FList[i - 1];

  //Вставить
  FList[Index] := Item;

  //Увеличить счётчик
  Inc(FCount);
end;


end.


