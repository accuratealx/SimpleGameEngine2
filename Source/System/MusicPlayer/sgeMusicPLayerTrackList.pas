{
Пакет             Simple Game Engine 2
Файл              sgeMusicPLayerTrackList.pas
Версия            1.0
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          MusicPlayer: Список дорожек
}
{$Include Defines.inc}

unit sgeMusicPLayerTrackList;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection, sgeStringList, sgeMemoryStream, sgeTemplateObjectCollection,
  sgeMusicPLayerTrack;


type
  //Режим загрузки списка
  TsgeMusicPlayerTrackListLoadMode = (mptlmAdd, mptlmReplace);


  TsgeMusicPlayerTrackListTemplate = specialize TsgeTemplateObjectCollection<TsgeMusicPLayerTrack>;


  TsgeMusicPlayerTrackList = class(TsgeMusicPlayerTrackListTemplate)
  private
    FCS: TsgeCriticalSection;

    FCurrentTrack: TsgeMusicPLayerTrack;                            //Последняя запрашиваемая дорожка

    procedure GetTrackListByGroup(List: TsgeMusicPlayerTrackList; Group: String = ''); unimplemented;

  protected
    function GetItem(Index: Integer): TsgeMusicPLayerTrack; override;

  public
    constructor Create(FreeObjects: Boolean = True); override;
    destructor  Destroy; override;

    function IndexOfName(Name: String): Integer;
    function IndexOfGroup(Group: String): Integer;

    procedure Clear;
    procedure Add(Track: TsgeMusicPLayerTrack);
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);
    procedure DeleteByGroup(Group: String);

    function  GetRandomTrack(Group: String = ''): TsgeMusicPLayerTrack; unimplemented;
    function  GetNextTrack(Group: String = ''): TsgeMusicPLayerTrack; unimplemented;
    function  GetPrevTrack(Group: String = ''): TsgeMusicPLayerTrack; unimplemented;

    procedure FromString(Str: String; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);
    procedure FromMemoryStream(Stream: TsgeMemoryStream; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);
    procedure LoadFromFile(FileName: String; Mode: TsgeMusicPlayerTrackListLoadMode = mptlmAdd);

    property CurrentTrack: TsgeMusicPLayerTrack read FCurrentTrack;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform;

const
  _UNITNAME = 'MusicPlayerTrackList';

  Err_TrackNotFound = 'TrackNotFound';
  Err_FileNotFound = 'FileNotFound';
  Err_CantReadFile = 'CantReadFile';


procedure TsgeMusicPlayerTrackList.GetTrackListByGroup(List: TsgeMusicPlayerTrackList; Group: String);
begin
  //Отобрать треки по группе
end;


function TsgeMusicPlayerTrackList.GetItem(Index: Integer): TsgeMusicPLayerTrack;
begin
  FCS.Enter;
  try

    Result := inherited GetItem(Index);

  finally
    FCS.Leave;
  end;
end;


constructor TsgeMusicPlayerTrackList.Create(FreeObjects: Boolean);
begin
  inherited Create(FreeObjects);

  FCS := sgeCriticalSection.TsgeCriticalSection.Create;
end;


destructor TsgeMusicPlayerTrackList.Destroy;
begin
  FCS.Free;

  inherited Destroy;
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
      if Name = LowerCase(FList[i].Name) then Exit(i);

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
      if Group = LowerCase(FList[i].Group) then Exit(i);

  finally
    FCS.Leave;
  end;
end;


procedure TsgeMusicPlayerTrackList.Clear;
begin
  FCS.Enter;
  try

    inherited Clear;

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


procedure TsgeMusicPlayerTrackList.Delete(Index: Integer);
begin
  FCS.Enter;
  try

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

    idx := IndexOfName(Name);
    if idx = -1 then
      raise EsgeException.Create(_UNITNAME, Err_TrackNotFound, Name);

    inherited Delete(idx);

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
begin
  FCS.Enter;
  try

    //Получить список треков по группе
    //Вернуть случайный

  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.GetNextTrack(Group: String): TsgeMusicPLayerTrack;
begin
  FCS.Enter;
  try

    //Получить список треков по группе
    //Вернуть следующий

  finally
    FCS.Leave;
  end;
end;


function TsgeMusicPlayerTrackList.GetPrevTrack(Group: String): TsgeMusicPLayerTrack;
begin
  FCS.Enter;
  try

    //Получить список треков по группе
    //Вернуть предыдущий

  finally
    FCS.Leave;
  end;
end;


//# - Comment
//Name; FileName; <Group>
procedure TsgeMusicPlayerTrackList.FromString(Str: String; Mode: TsgeMusicPlayerTrackListLoadMode);
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
    //Проверить на затирание дорожек
    if Mode = mptlmReplace then Clear;

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
      if s = '' then Continue;

      //Проверка на заметку
      if s[1] = Comment then Continue;

      //Разобрать строку на части
      Line.FromString(s);

      //Проверить наличие 2 частей
      if Line.Count < 2 then Continue;

      //Подготовить данные
      Name := sgeTrim(Line.Part[0]);
      FileName := sgeTrim(Line.Part[1]);
      Group := '';
      if Line.Count > 2 then Group := sgeTrim(Line.Part[2]);

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


procedure TsgeMusicPlayerTrackList.FromMemoryStream(Stream: TsgeMemoryStream; Mode: TsgeMusicPlayerTrackListLoadMode);
begin
  FromString(Stream.ToString, Mode);
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
    FromMemoryStream(Stream, Mode);

  finally
    Stream.Free;
  end;
end;


end.

