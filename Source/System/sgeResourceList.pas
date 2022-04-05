{
Пакет             Simple Game Engine 2
Файл              sgeResourceList.pas
Версия            1.4
Создан            15.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Хранилище загруженных ресурсов
}
{$Include Defines.inc}

unit sgeResourceList;

{$mode objfpc}{$H+}

interface

uses
  sgeMetaInfoList;



type
  //Типы ресурсов
  TsgeResourceType = (
    rtUnknown,
    rtSystemFont,
    rtFont,
    rtSprite,
    rtAnimationFrames,
    rtAnimation,
    rtSoundBuffer,
    rtStringList,
    rtParameters,
    rtContainer,
    rtCursor
    );

const
  //Имена ресурсов
  sgeResourceNames: array[TsgeResourceType] of String = (
    'Unknown',
    'SystemFont',
    'Font',
    'Sprite',
    'AnimFrames',
    'Animation',
    'SoundBuffer',
    'StringList',
    'Parameters',
    'Container',
    'Cursor'
    );


type
  //Запись для одного ресурса
  TsgeResource = record
    Name: String;                   //Имя ресурса в таблице
    ResType: TsgeResourceType;      //Тип ресурса
    Obj: TObject;                   //Ссылка на объект
    Group: String;                  //Группа
    Meta: TsgeMetaInfoList;         //Список метаинформации
  end;


  TsgeResourceList = class
  private
    //Классы
    FResources: array of TsgeResource;

    function  GetCount: Integer;
    function  GetItem(Index: Integer): TsgeResource;
    function  GetTypedItem(Name: String; ResType: TsgeResourceType): TsgeResource;

    function  GetObject(Name: String): TObject;
    function  GetTypedObject(Name: String; ResType: TsgeResourceType): TObject;
  public
    destructor  Destroy; override;

    procedure Clear;
    procedure AddItem(AItem: TsgeResource);
    procedure AddItem(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaInfo: TsgeMetaInfoList; Group: String = '');
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);
    procedure Delete(Name: String; ResType: TsgeResourceType);
    procedure DeleteByGroup(Group: String);
    function  IndexOf(Name: String): Integer;
    function  IndexOf(Name: String; ResType: TsgeResourceType): Integer;
    function  IndexOf(Obj: TObject): Integer;

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgeResource read GetItem;
    property TypedItem[Name: String; ResType: TsgeResourceType]: TsgeResource read GetTypedItem;

    property Obj[Name: String]: TObject read GetObject;
    property TypedObj[Name: String; ResType: TsgeResourceType]: TObject read GetTypedObject;
  end;


function sgeStrToResType(Str: String): TsgeResourceType;


implementation

uses
  sgeErrors, sgeSystemUtils;


const
  _UNITNAME = 'ResourceList';

  Err_IndexOutOfBounds          = 'IndexOutOfBounds';
  Err_ResourceNotFound          = 'ResourceNotFound';
  Err_ObjectIsEmpty             = 'ObjectIsEmpty';


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


function TsgeResourceList.GetCount: Integer;
begin
  Result := Length(FResources);
end;


function TsgeResourceList.GetItem(Index: Integer): TsgeResource;
var
  c: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FResources[Index];
end;


function TsgeResourceList.GetTypedItem(Name: String; ResType: TsgeResourceType): TsgeResource;
var
  Idx: Integer;
begin
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name + ', ' + sgeResourceNames[ResType]);

  Result := FResources[Idx];
end;


function TsgeResourceList.GetObject(Name: String): TObject;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Name);
  if Idx = -1 then
    Exit;
  Result := FResources[Idx].Obj;
end;


function TsgeResourceList.GetTypedObject(Name: String; ResType: TsgeResourceType): TObject;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    Exit;
  Result := FResources[Idx].Obj;
end;


destructor TsgeResourceList.Destroy;
begin
  Clear;
end;


procedure TsgeResourceList.Clear;
var
  i, c: Integer;
begin
  c := GetCount - 1;
  for i := 0 to c do
  begin
    FResources[i].Meta.Free;
    FResources[i].Obj.Free;
  end;

  SetLength(FResources, 0);
end;


procedure TsgeResourceList.AddItem(AItem: TsgeResource);
var
  c: Integer;
begin
  //Проверить класс метаинформации
  if AItem.Meta = nil then
    raise EsgeException.Create(_UNITNAME, Err_ObjectIsEmpty, 'MetaInfo');

  c := GetCount;
  SetLength(FResources, c + 1);
  FResources[c] := AItem;
end;


procedure TsgeResourceList.AddItem(Name: String; ResType: TsgeResourceType; Obj: TObject; MetaInfo: TsgeMetaInfoList; Group: String);
var
  I: TsgeResource;
begin
  I.Name := Name;
  I.ResType := ResType;
  I.Obj := Obj;
  I.Group := Group;
  I.Meta := MetaInfo;

  //Добавить в массив
  AddItem(I);
end;


procedure TsgeResourceList.Delete(Index: Integer);
var
  i, c: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Удалить метаинформацию
  FResources[Index].Meta.Free;

  //Удалить объект
  FResources[Index].Obj.Free;

  for i := Index to c - 1 do
    FResources[i] := FResources[i + 1];

  SetLength(FResources, c);
end;


procedure TsgeResourceList.Delete(Name: String);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name);

  Delete(Idx);
end;


procedure TsgeResourceList.Delete(Name: String; ResType: TsgeResourceType);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name, ResType);
  if Idx = -1 then
    raise EsgeException.Create(_UNITNAME, Err_ResourceNotFound, Name);

  Delete(Idx);
end;


procedure TsgeResourceList.DeleteByGroup(Group: String);
var
  i: Integer;
begin
  Group := LowerCase(Group);

  i := -1;
  while i < GetCount - 1 do
  begin
    Inc(i);

    if LowerCase(FResources[i].Group) = Group then
    begin
      Delete(i);
      Dec(i)
    end;
  end;
end;


function TsgeResourceList.IndexOf(Name: String): Integer;
var
  i, c: Integer;
begin
  Result := -1;
  c := GetCount - 1;
  Name := LowerCase(Name);
  for i := 0 to c do
    if Name = LowerCase(FResources[i].Name) then
    begin
      Result := i;
      Break;
    end;
end;


function TsgeResourceList.IndexOf(Name: String; ResType: TsgeResourceType): Integer;
var
  i, c: Integer;
begin
  Result := -1;
  c := GetCount - 1;
  Name := LowerCase(Name);
  for i := 0 to c do
    if (Name = LowerCase(FResources[i].Name)) and (ResType = FResources[i].ResType) then
    begin
      Result := i;
      Break;
    end;
end;


function TsgeResourceList.IndexOf(Obj: TObject): Integer;
var
  i, c: Integer;
begin
  Result := -1;
  c := GetCount - 1;
  for i := 0 to c do
    if FResources[i].Obj = Obj then
    begin
      Result := i;
      Break;
    end;
end;



end.

