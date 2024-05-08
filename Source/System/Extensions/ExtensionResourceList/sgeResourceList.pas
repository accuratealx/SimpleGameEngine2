{
Пакет             Simple Game Engine 2
Файл              sgeResourceList.pas
Версия            1.5
Создан            15.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Хранилище загруженных ресурсов
}
{$Include Defines.inc}

unit sgeResourceList;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeResourceItem;


type
  TsgeResourceList = class(specialize TsgeTemplateCollection<TsgeResourceItem>)
  private
    function  GetTypedItem(Name: String; ResType: TsgeResourceType): TsgeResourceItem;
    function  GetObject(Name: String): TObject;
    function  GetTypedObject(Name: String; ResType: TsgeResourceType): TObject;

  public
    constructor Create;

    procedure AddItem(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaStr: String = ''; Group: String = '');
    procedure Delete(Name: String);
    procedure Delete(Name: String; ResType: TsgeResourceType);
    procedure DeleteByGroup(Group: String);

    function  IndexOf(Name: String): Integer;
    function  IndexOf(Name: String; ResType: TsgeResourceType): Integer;
    function  IndexOf(Obj: TObject): Integer;

    property TypedItem[Name: String; ResType: TsgeResourceType]: TsgeResourceItem read GetTypedItem;

    property Obj[Name: String]: TObject read GetObject;
    property TypedObj[Name: String; ResType: TsgeResourceType]: TObject read GetTypedObject;
  end;


function sgeStrToResType(Str: String): TsgeResourceType;


implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'ResourceList';

  Err_ResourceNotFound = 'ResourceNotFound';
  //Err_IndexOutOfBounds = 'IndexOutOfBounds';
  //Err_ObjectIsEmpty    = 'ObjectIsEmpty';


function sgeStrToResType(Str: String): TsgeResourceType;
var
  i: TsgeResourceType;
begin
  Result := rtUnknown;
  Str := LowerCase(Str);

  for i := Low(TsgeResourceType) to High(TsgeResourceType) do
    if Str = LowerCase(sgeResourceNames[i]) then
    begin
      Result := i;
      Break;
    end;
end;


function TsgeResourceList.GetTypedItem(Name: String; ResType: TsgeResourceType): TsgeResourceItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name + ', ' + sgeResourceNames[ResType]);

  Result := FList[Idx];
end;


function TsgeResourceList.GetObject(Name: String): TObject;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Name);
  if Idx = -1 then
    Exit;

  Result := FList[Idx].Obj;
end;


function TsgeResourceList.GetTypedObject(Name: String; ResType: TsgeResourceType): TObject;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    Exit;

  Result := FList[Idx].Obj;
end;


constructor TsgeResourceList.Create;
begin
  inherited Create(True);
end;


procedure TsgeResourceList.AddItem(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaStr: String; Group: String);
var
  AItem: TsgeResourceItem;
begin
  //Создать элемент
  AItem := TsgeResourceItem.Create(Name, ResType, Obj, MetaStr, Group);

  //Добавить в массив
  inherited Add(AItem);
end;


procedure TsgeResourceList.Delete(Name: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name);

  inherited Delete(Idx);
end;


procedure TsgeResourceList.Delete(Name: String; ResType: TsgeResourceType);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name);

  inherited Delete(Idx);
end;


procedure TsgeResourceList.DeleteByGroup(Group: String);
var
  i: Integer;
begin
  Group := LowerCase(Group);

  i := -1;
  while i < FCount - 1 do
  begin
    Inc(i);

    if LowerCase(FList[i].Group) = Group then
    begin
      inherited Delete(i);
      Dec(i)
    end;
  end;
end;


function TsgeResourceList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if Name = LowerCase(FList[i].Name) then
      Exit(i);
end;


function TsgeResourceList.IndexOf(Name: String; ResType: TsgeResourceType): Integer;
var
  i: Integer;
begin
  Result := -1;
  Name := LowerCase(Name);
  for i := 0 to FCount - 1 do
    if (Name = LowerCase(FList[i].Name)) and (ResType = FList[i].ResType) then
      Exit(i);
end;


function TsgeResourceList.IndexOf(Obj: TObject): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList[i].Obj = Obj then
      Exit(i);
end;



end.

