{
Пакет             Simple Game Engine 2
Файл              sgeTemplateThreadSafeCollection.pas
Версия            1.0
Создан            21.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс-шаблон: Потокобезопасная коллекция
}
{$Include Defines.inc}

unit sgeTemplateThreadSafeCollection;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection;


type
  generic TsgeTemplateThreadSafeCollection<T> = class
  protected
    FCS: TsgeCriticalSection;
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


function TsgeTemplateThreadSafeCollection.GetItem(Index: Integer): T;
begin
  FCS.Enter;
  try

    if (Index < 0) or (Index > FCount - 1) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    Result := FList[Index];

  finally
    FCS.Leave;
  end;
end;


constructor TsgeTemplateThreadSafeCollection.Create(FreeObjects: Boolean);
begin
  FCS := TsgeCriticalSection.Create;

  //Запомнить параметры
  FFreeObjects := FreeObjects;
  FCount := 0;
end;


destructor TsgeTemplateThreadSafeCollection.Destroy;
begin
  Clear;

  FCS.Free;
end;


procedure TsgeTemplateThreadSafeCollection.Clear;
var
  i: Integer;
begin
  FCS.Enter;
  try

    //Удалить память объектов
    if FFreeObjects then
      for i := 0 to FCount - 1 do
        TObject(FList[i]).Free;

    //Поправить параметры
    FCount := 0;

    //Очистить массив
    SetLength(FList, FCount);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTemplateThreadSafeCollection.Add(Item: T);
begin
  FCS.Enter;
  try

    Inc(FCount);
    SetLength(FList, FCount);
    FList[FCount - 1] := Item;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTemplateThreadSafeCollection.Delete(Index: Integer);
var
  c, i: Integer;
begin
  FCS.Enter;
  try

    c := Fcount - 1;
    if (Index < 0) or (Index > FCount) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    //Удалить память объекта
    if FFreeObjects then  TObject(FList[Index]).Free;

    //Сдвинуть хвост
    for i := Index to c - 1 do
      FList[i] := FList[i + 1];

    //Удалить последний элемент
    SetLength(FList, c);

    //Уменьшить счётчик
    Dec(FCount);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeTemplateThreadSafeCollection.Insert(Index: Integer; Item: T);
var
  i: Integer;
begin
  FCS.Enter;
  try

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

  finally
    FCS.Leave;
  end;
end;


end.


