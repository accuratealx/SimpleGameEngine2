{
Пакет             Simple Game Engine 2
Файл              sgeExtensionList.pas
Версия            1.2
Создан            24.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Список расширений
}
{$Include Defines.inc}

unit sgeExtensionList;

{$mode objfpc}{$H+}


interface

uses
  sgeCriticalSection,
  sgeExtensionBase;


const
  //Имя объекта
  Object_ExtensionList = 'ExtensionList';


type
  //Список расширений
  TsgeExtensionList = class
  private
    FCS: TsgeCriticalSection;
    FList: array of TsgeExtensionBase;

    function GetCount: Integer;
    function GetItem(Index: Integer): TsgeExtensionBase;
  public
    constructor Create;
    destructor  Destroy; override;

    function  IndexOf(Name: String): Integer;

    procedure Clear;
    procedure Add(Extension: TsgeExtensionBase);
    procedure Delete(Name: String);
    function  Exist(Name: String): Boolean;

    function  Get(Name: String): TsgeExtensionBase;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgeExtensionBase read GetItem;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils;

const
  _UNITNAME = 'ExtensionList';

  Err_IndexOutOfBounds  = 'IndexOutOfBounds';
  Err_ObjectNotFound    = 'ObjectNotFound';



function TsgeExtensionList.GetCount: Integer;
begin
  FCS.Enter;
  try

    Result := Length(FList);

  finally
    FCS.Leave;
  end;
end;


function TsgeExtensionList.GetItem(Index: Integer): TsgeExtensionBase;
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


constructor TsgeExtensionList.Create;
begin
  FCS := TsgeCriticalSection.Create;
end;


destructor TsgeExtensionList.Destroy;
begin
  Clear;
  FCS.Free;
end;


function TsgeExtensionList.IndexOf(Name: String): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  Name := LowerCase(Name);
  c := Length(FList) - 1;
  for i := 0 to c do
    if LowerCase(FList[i].Name) = Name then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgeExtensionList.Clear;
var
  i, c: Integer;
begin
  FCS.Enter;
  try

    //Уничтожить расширения
    c := GetCount - 1;
    for i := c downto 0 do
      FList[i].Free;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeExtensionList.Add(Extension: TsgeExtensionBase);
var
  c: Integer;
begin
  FCS.Enter;
  try

    c := GetCount;
    SetLength(FList, c + 1);
    FList[c] := Extension;

  finally
    FCS.Leave;
  end;
end;


procedure TsgeExtensionList.Delete(Name: String);
var
  Idx, c, i: Integer;
begin
  //Найти индекс
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ObjectNotFound, Name);

  //Сдвинуть хвост
  c := GetCount - 2;
  for i := Idx to c do
    FList[i] := FList[i + 1];

  SetLength(FList, c + 1);
end;


function TsgeExtensionList.Exist(Name: String): Boolean;
begin
  Result := (IndexOf(Name) <> -1);
end;


function TsgeExtensionList.Get(Name: String): TsgeExtensionBase;
var
  Idx: Integer;
begin
  Result := nil;

  Idx := IndexOf(Name);
  if Idx <> -1 then Result := FList[Idx];
end;


end.

