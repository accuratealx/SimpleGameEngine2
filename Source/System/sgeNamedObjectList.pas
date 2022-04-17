{
Пакет             Simple Game Engine 2
Файл              sgeNamedObjectList.pas
Версия            1.1
Создан            21.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Именованный список объектов
}
{$Include Defines.inc}

unit sgeNamedObjectList;

{$mode objfpc}{$H+}


interface

uses
  sgeCriticalSection;


const
  Object_ObjecttList = 'ObjectList';


type
  //Запись одного объекта
  TsgeNamedObjectListItem = record
    Name: ShortString;
    Obj: TObject;
  end;


  //Список объектов
  TsgeNamedObjectList = class
  private
    FCS: TsgeCriticalSection;
    FList: array of TsgeNamedObjectListItem;

    function GetCount: Integer;
    function GetItem(Index: Integer): TsgeNamedObjectListItem;
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(Name: String): Integer;
    procedure Clear;
    procedure Add(Name: String; Obj: TObject);
    procedure Delete(Name: String);
    function  Exist(Name: String): Boolean;

    function  Get(Name: String): TObject;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgeNamedObjectListItem read GetItem;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'NamedObjectList';

  Err_IndexOutOfBounds    = 'IndexOutOfBounds';
  Err_ObjectAlreadyExists = 'ObjectAlreadyExists';
  Err_ObjectNotFound      = 'ObjectNotFound';



function TsgeNamedObjectList.GetCount: Integer;
begin
  FCS.Enter;
  try
    Result := Length(FList);
  finally
    FCS.Leave;
  end;
end;


function TsgeNamedObjectList.GetItem(Index: Integer): TsgeNamedObjectListItem;
begin
  FCS.Enter;
  try
    if (Index < 0) or (Index > GetCount - 1) then
      raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    Result := FList[Index];
  finally
    FCS.Leave;
  end;
end;


constructor TsgeNamedObjectList.Create;
begin
  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeNamedObjectList.Destroy;
begin
  Clear;
  FCS.Free;
end;


function TsgeNamedObjectList.IndexOf(Name: String): Integer;
var
  i, c: Integer;
begin
  FCS.Enter;
  try
    Result := -1;

    Name := LowerCase(Name);
    c := Length(FList) - 1;
    for i := 0 to c do
      if LowerCase(FList[i].Name) = Name then
        Exit(i);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeNamedObjectList.Clear;
begin
  FCS.Enter;
  try
    SetLength(FList, 0);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeNamedObjectList.Add(Name: String; Obj: TObject);
var
  c: Integer;
begin
  FCS.Enter;
  try
    if IndexOf(Name) <> -1 then
      raise EsgeException.Create(_UNITNAME, Err_ObjectAlreadyExists, Name);

    c := GetCount;
    SetLength(FList, c + 1);
    FList[c].Name := Name;
    FList[c].Obj := Obj;
  finally
    FCS.Leave;
  end;
end;


procedure TsgeNamedObjectList.Delete(Name: String);
var
  Idx, c, i: Integer;
begin
  FCS.Enter;
  try
    //Найти индекс
    Idx := IndexOf(Name);
    if Idx = -1 then
      raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);

    //Сдвинуть хвост
    c := GetCount - 2;
    for i := Idx to c do
      FList[i] := FList[i + 1];

    SetLength(FList, c + 1);
  finally
    FCS.Leave;
  end;
end;


function TsgeNamedObjectList.Exist(Name: String): Boolean;
begin
  FCS.Enter;
  try
    Result := (IndexOf(Name) <> -1);
  finally
    FCS.Leave;
  end;
end;


function TsgeNamedObjectList.Get(Name: String): TObject;
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    Result := nil;
    Idx := IndexOf(Name);
    if Idx <> -1 then
      Result := FList[Idx].Obj;
  finally
    FCS.Leave;
  end;
end;


end.

