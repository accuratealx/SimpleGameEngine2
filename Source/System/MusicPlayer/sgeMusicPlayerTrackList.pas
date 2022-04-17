{
Пакет             Simple Game Engine 2
Файл              sgeMusicPlayerTrackList.pas
Версия            1.3
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          MusicPlayer: Список дорожек
}
{$Include Defines.inc}

unit sgeMusicPlayerTrackList;

{$mode objfpc}{$H+}

interface

uses
  sgeStringList, sgeMemoryStream, sgeTemplateThreadSafeCollection,
  sgeMusicPlayerTrack;


type
  //Режим загрузки списка
  TsgeMusicPlayerTrackListLoadMode = (mptlmAdd, mptlmReplace);


  TsgeMusicPlayerTrackList = class(specialize TsgeTemplateThreadSafeCollection<TsgeMusicPlayerTrack>)
  private
    FCurrentTrack: TsgeMusicPLayerTrack;

    procedure GetTrackListByGroup(List: TsgeMusicPlayerTrackList; Group: String = '');
  public
    function IndexOfName(Name: String): Integer;
    function IndexOfGroup(Group: String): Integer;

    procedure Add(Track: TsgeMusicPLayerTrack);
    procedure Add(const Name, FileName: String; const Group: String = '');
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);
    procedure DeleteByGroup(Group: String);

    function  GetRandomTrack(Group: String = ''): TsgeMusicPLayerTrack;
    function  GetNextTrack(Group: String = ''): TsgeMusicPLayerTrack;
    function  GetPrevTrack(Group: String = ''): TsgeMusicPLayerTrack;

    procedure FromString(Str: String; BasePath: String = ''; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);
    procedure FromMemoryStream(Stream: TsgeMemoryStream; BasePath: String = ''; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);
    procedure LoadFromFile(FileName: String; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);

    property CurrentTrack: TsgeMusicPLayerTrack read FCurrentTrack;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform, sgeFileUtils;

const
  _UNITNAME = 'MusicPlayerTrackList';

  Err_TrackNotFound = 'TrackNotFound';
  Err_FileNotFound  = 'FileNotFound';
  Err_CantReadFile  = 'CantReadFile';



procedure TsgeMusicPlayerTrackList.GetTrackListByGroup(List: TsgeMusicPlayerTrackList; Group: String);
var
  i: Integer;
  isAdd: Boolean;
begin
  //Подготовить группу
  Group := LowerCase(Group);

  //Найти дорожки совпавшие по группе
  for i := 0 to FCount - 1 do
  begin
    isAdd := False;

    //Проверить совпадение
    if Group = '' then
      isAdd := True
    else
      if LowerCase(FList[i].Group) = Group then
        isAdd := True;

    //Добавить в список
    if isAdd then
      List.Add(FList[i]);
  end;
end;


function TsgeMusicPlayerTrackList.IndexOfName(Name: String): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := -1;
    Name := LowerCase(Name);
    for i := 0 to FCount - 1 do
      if Name = LowerCase(FList[i].Name) then
        Exit(i);
  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.IndexOfGroup(Group: String): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := -1;
    Group := LowerCase(Group);
    for i := 0 to FCount - 1 do
      if Group = LowerCase(FList[i].Group) then
        Exit(i);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.Add(Track: TsgeMusicPLayerTrack);
begin
  FCS.Enter;
  try
    inherited Add(Track);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.Add(const Name, FileName: String; const Group: String);
begin
  Add(TsgeMusicPLayerTrack.Create(Name, FileName, Group));
end;


procedure TsgeMusicPlayerTrackList.Delete(Index: Integer);
begin
  FCS.Enter;
  try
    //Поправить текушую дорожку
    if FList[Index] = FCurrentTrack then
      FCurrentTrack := nil;

    //Удалить
    inherited Delete(Index);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.Delete(Name: String);
var
  idx: Integer;
begin
  FCS.Enter;
  try
    //Найти индекс дорожки
    idx := IndexOfName(Name);
    if idx = -1 then
      raise EsgeException.Create(_UNITNAME, Err_TrackNotFound, Name);

    //Удалить
    Delete(idx);
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.DeleteByGroup(Group: String);
var
  idx: Integer;
begin
  FCS.Enter;
  try
    idx := IndexOfGroup(Group);
    while idx <> - 1 do
    begin
      Delete(idx);
      idx := IndexOfGroup(Group);
    end;
  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.GetRandomTrack(Group: String): TsgeMusicPLayerTrack;
var
  List: TsgeMusicPlayerTrackList;
begin
  Result := nil;

  FCS.Enter;
  try
    //Получить список дорожек по группе
    List := TsgeMusicPlayerTrackList.Create(False);
    GetTrackListByGroup(List, Group);

    //Найти случайную дорожку
    if List.Count > 0 then
      Result := FList[Random(List.Count)];

    //Почистить память
    List.Free;

    //Запомнить текущую дорожку
    FCurrentTrack := Result;
  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.GetNextTrack(Group: String): TsgeMusicPLayerTrack;
var
  List: TsgeMusicPlayerTrackList;
  Idx: Integer;
begin
  Result := nil;

  FCS.Enter;
  try
    //Получить список дорожек по группе
    List := TsgeMusicPlayerTrackList.Create(False);
    GetTrackListByGroup(List, Group);

    if List.Count > 0 then
    begin
      //Определить индекс текущей дорожки
      if FCurrentTrack <> nil then
        Idx := List.IndexOfName(FCurrentTrack.Name)
      else
        Idx := -1;

      //Перейти на следующую дорожку
      Inc(Idx);

      //Если это последний, то с начала
      if Idx > List.Count - 1 then
        Idx := 0;

      //Вернуть следующую дорожку
      Result := List.Item[Idx];
    end;

    //Почистить память
    List.Free;

    //Запомнить текущую дорожку
    FCurrentTrack := Result;
  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.GetPrevTrack(Group: String): TsgeMusicPLayerTrack;
var
  List: TsgeMusicPlayerTrackList;
  Idx: Integer;
begin
  Result := nil;

  FCS.Enter;
  try
    //Получить список дорожек по группе
    List := TsgeMusicPlayerTrackList.Create(False);
    GetTrackListByGroup(List, Group);

    if List.Count > 0 then
    begin
      //Определить индекс текущей дорожки
      if FCurrentTrack <> nil then
        Idx := List.IndexOfName(FCurrentTrack.Name)
      else
        Idx := -1;

      //Перейти на следующую дорожку
      Dec(Idx);

      //Если это первый, то с конца
      if Idx < 0 then
        Idx := List.Count - 1;

      //Вернуть следующую дорожку
      Result := List.Item[Idx];
    end;

    //Почистить память
    List.Free;

    //Запомнить текущую дорожку
    FCurrentTrack := Result;
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.FromString(Str: String; BasePath: String; Mode: TsgeMusicPlayerTrackListLoadMode);
const
  Separator = ';';
  Comment = '#';
var
  List, Line: TsgeStringList;
  i: Integer;
  s, Name, FileName, Group: String;
begin
  FCS.Enter;
  try
    //Удалить текушую дорожку
    FCurrentTrack := nil;

    //Проверить на затирание дорожек
    if Mode = mptlmReplace then
      Clear;

    //Разобрать строку на список
    List := TsgeStringList.Create;
    List.FromString(Str);

    //Подготовить класс разбора строки на части
    Line := TsgeStringList.Create;
    Line.Separator := Separator;

    //Просмотреть список
    for i := 0 to List.Count - 1 do
    begin
      //Обрезать лишние символы
      s := sgeTrim(List.Part[i]);

      //Пустая строка
      if s = '' then
        Continue;

      //Проверка на заметку
      if s[1] = Comment then
        Continue;

      //Разобрать строку на части
      Line.FromString(s);

      //Проверить наличие 2 частей
      if Line.Count < 2 then
        Continue;

      //Подготовить данные
      Name := sgeTrim(Line.Part[0]);
      FileName := BasePath + sgeTrim(Line.Part[1]);
      Group := '';
      if Line.Count > 2 then
        Group := sgeTrim(Line.Part[2]);

      //Добавить дорожку
      Add(TsgeMusicPLayerTrack.Create(Name, FileName, Group));
    end;

    //Почистить память
    Line.Free;
    List.Free;
  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.FromMemoryStream(Stream: TsgeMemoryStream; BasePath: String; Mode: TsgeMusicPlayerTrackListLoadMode);
begin
  FromString(Stream.ToString, BasePath, Mode);
end;


procedure TsgeMusicPlayerTrackList.LoadFromFile(FileName: String; Mode: TsgeMusicPlayerTrackListLoadMode);
var
  Stream: TsgeMemoryStream;
begin
  //Проверить на существование файла
  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  Stream := TsgeMemoryStream.Create;
  try
    //Прочитать файл
    try
      Stream.LoadFromFile(FileName);
    except
      raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);
    end;

    //Загрузить
    FromMemoryStream(Stream, sgeExtractFilePath(FileName), Mode);
  finally
    Stream.Free;
  end;
end;


end.

