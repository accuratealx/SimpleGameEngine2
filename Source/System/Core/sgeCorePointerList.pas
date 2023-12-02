{
Пакет             Simple Game Engine 2
Файл              sgeNamedObjectList.pas
Версия            1.1
Создан            21.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Именованный список указателей
}
{$Include Defines.inc}

unit sgeCorePointerList;

{$mode objfpc}{$H+}


interface

uses
  sgeCriticalSection;


const
  Object_ObjecttList = 'ObjectList';

  ItemType_Object = 'Object';
  ItemType_ClassProcedure = 'ClassProcedure';
  ItemType_ClassFunction = 'ClassFunction';
  ItemType_Record = 'Record';
  ItemType_Procedure = 'Procedure';
  ItemType_Function = 'Function';

  ItemType_SGE = 'SGE';
  ItemType_SGEErrorManager = 'SGE.ErrorManager';
  ItemType_SGEEventManager = 'SGE.EventManager';
  ItemType_SGEExtensionList = 'SGE.ExtensionList';
  ItemType_SGEExtension = 'SGE.Extension';


type
  //Запись одного объекта
  TsgeCorePointerListItem = record
    ItemType: ShortString;
    Name: ShortString;
    Obj: Pointer;
  end;


  //Список объектов
  TsgeCorePointerList = class
  private
    FCS: TsgeCriticalSection;

    FCount: Integer;
    FList: array of TsgeCorePointerListItem;

    procedure Delete(Index: Integer);
    function  GetItem(Index: Integer): TsgeCorePointerListItem;
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(Name: String): Integer;
    function  IndexOf(Name: String; ItemType: ShortString): Integer;

    procedure Clear;
    procedure Add(Name: String; Obj: Pointer; ItemType: ShortString = '');
    procedure Delete(Name: String);
    procedure Delete(Name: String; ItemType: ShortString);
    function  Exist(Name: String): Boolean;
    function  Exist(Name: String; ItemType: ShortString): Boolean;
    function  Get(Name: String): Pointer;
    function  Get(Name: String; ItemType: ShortString): Pointer;

    //Обертки
    procedure AddObject(Name: String; Obj: TObject; ItemType: ShortString = '');
    function  GetObject(Name: String): TObject;
    function  GetObject(Name: String; ItemType: ShortString): TObject;

    property Count: Integer read FCount;
    property Item[Index: Integer]: TsgeCorePointerListItem read GetItem;
  end;


var
  CorePointerList: TsgeCorePointerList;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'NamedObjectList';

  Err_IndexOutOfBounds    = 'IndexOutOfBounds';
  Err_ObjectAlreadyExists = 'ObjectAlreadyExists';
  Err_ObjectNotFound      = 'ObjectNotFound';


procedure TsgeCorePointerList.Delete(Index: Integer);
var
  i: Integer;
begin
  FCS.Enter;
  try
    if (Index < 0) or (Index > FCount - 1) then
      EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

    //Сдвинуть хвост
    for i := Index to FCount - 2 do
      FList[i] := FList[i + 1];

    //Поправить длину
    SetLength(FList, FCount - 1);
    Dec(FCount);
  finally
    FCS.Leave;
  end;
end;


function TsgeCorePointerList.GetItem(Index: Integer): TsgeCorePointerListItem;
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


constructor TsgeCorePointerList.Create;
begin
  FCS := TsgeCriticalSection.Create;
  FCount := 0;
end;


destructor TsgeCorePointerList.Destroy;
begin
  Clear;
  FCS.Free;
end;


function TsgeCorePointerList.IndexOf(Name: String): Integer;
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

function TsgeCorePointerList.IndexOf(Name: String; ItemType: ShortString): Integer;
var
  i, c: Integer;
begin
  FCS.Enter;
  try
    Result := -1;

    Name := LowerCase(Name);
    ItemType := LowerCase(ItemType);
    c := Length(FList) - 1;
    for i := 0 to c do
      if (LowerCase(FList[i].ItemType) = ItemType) and (LowerCase(FList[i].Name) = Name) then
        Exit(i);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeCorePointerList.Clear;
begin
  FCS.Enter;
  try
    SetLength(FList, 0);
    FCount := 0;
  finally
    FCS.Leave;
  end;
end;


procedure TsgeCorePointerList.Add(Name: String; Obj: Pointer; ItemType: ShortString);
begin
  FCS.Enter;
  try
    if IndexOf(Name, ItemType) <> -1 then
      raise EsgeException.Create(_UNITNAME, Err_ObjectAlreadyExists, Name + ' ' + ItemType);

    SetLength(FList, FCount + 1);
    FList[FCount].Name := Name;
    FList[FCount].Obj := Obj;
    FList[FCount].ItemType := ItemType;

    Inc(FCount);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeCorePointerList.Delete(Name: String);
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    //Найти индекс
    Idx := IndexOf(Name);
    if Idx = -1 then
      raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);

    //Удалить
    Delete(Idx);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeCorePointerList.Delete(Name: String; ItemType: ShortString);
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    //Найти индекс
    Idx := IndexOf(Name, ItemType);
    if Idx = -1 then
      raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);

    //Удалить
    Delete(Idx);
  finally
    FCS.Leave;
  end;
end;


function TsgeCorePointerList.Exist(Name: String): Boolean;
begin
  FCS.Enter;
  try
    Result := (IndexOf(Name) <> -1);
  finally
    FCS.Leave;
  end;
end;


function TsgeCorePointerList.Exist(Name: String; ItemType: ShortString): Boolean;
begin
  FCS.Enter;
  try
    Result := (IndexOf(Name, ItemType) <> -1);
  finally
    FCS.Leave;
  end;
end;


function TsgeCorePointerList.Get(Name: String): Pointer;
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


function TsgeCorePointerList.Get(Name: String; ItemType: ShortString): Pointer;
var
  Idx: Integer;
begin
  FCS.Enter;
  try
    Result := nil;
    Idx := IndexOf(Name, ItemType);
    if Idx <> -1 then
      Result := FList[Idx].Obj;
  finally
    FCS.Leave;
  end;
end;


procedure TsgeCorePointerList.AddObject(Name: String; Obj: TObject; ItemType: ShortString);
begin
  Add(Name, (Obj), ItemType);
end;


function TsgeCorePointerList.GetObject(Name: String): TObject;
begin
  Result := TObject(Get(Name));
end;


function TsgeCorePointerList.GetObject(Name: String; ItemType: ShortString): TObject;
begin
  Result := TObject(Get(Name, ItemType));
end;



initialization
begin
  CorePointerList := TsgeCorePointerList.Create;
end;


finalization
begin
  CorePointerList.Free;
end;



end.

