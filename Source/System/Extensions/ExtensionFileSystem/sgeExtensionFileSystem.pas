{
Пакет             Simple Game Engine 2
Файл              sgeExtensionFileSystem.pas
Версия            1.4
Создан            12.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Файловая система
}
{$Include Defines.inc}

unit sgeExtensionFileSystem;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream, sgeStringList,
  sgeExtensionBase,
  sgeExtensionPackList;

const
  Extension_FileSystem = 'FileSystem';


type
  TsgeExtensionFileSystem = class(TsgeExtensionBase)
  private
    //Ссылки
    FExtPackList: TsgeExtensionPackList;

    //Переменные
    FMainDir: String;
    FScreenshotDir: String;


    //Поиск файла в списке архивов
    procedure GetFilePackIndex(FileName: String; var PackIndex, FileIndex: Integer);

    function GetNormalFileName(FileName: String): String;
  protected
    function GetName: String; override;

  public
    constructor Create; override;

    //Каталоги
    procedure ForceDirectories(Directory: String);
    function  DirectoryExists(Directory: String): Boolean;

    //Операции с файлами
    function  FileExists(FileName: String): Boolean;
    procedure ReadFile(FileName: String; Stream: TsgeMemoryStream);
    function  ReadFile(FileName: String): String;
    procedure WriteFile(FileName: String; Stream: TsgeMemoryStream);
    procedure WriteFile(FileName: String; Data: String);
    procedure DeleteFile(FileName: String);
    procedure RenameFile(OldName, NewName: String);
    function  GetFileSize(FileName: String): Int64;
    procedure GetFileList(Directory: String; List: TsgeStringList);
    procedure GetDirectoryList(Directory: String; List: TsgeStringList);
    Procedure FindFiles(Directory: String; List: TsgeStringList; Ext: String = '');

    property MainDir: String read FMainDir;
    property ScreenshotDir: String read FScreenshotDir;
  end;



implementation

uses
  sgeErrors, sgeTypes, sgeFile, sgeSystemUtils, sgeFileUtils, sgeOSPlatform;


const
  _UNITNAME = 'ExtensionFileSystem';

  Err_CantCreateDirectory = 'CantCreateDirectory';
  Err_FileNotFound        = 'FileNotFound';
  Err_CantReadFile        = 'CantReadFile';
  Err_CantWriteFile       = 'CantWriteFile';
  Err_CantDeleteFile      = 'CantDeleteFile';
  Err_CantRenameFile      = 'CantRenameFile';



procedure TsgeExtensionFileSystem.GetFilePackIndex(FileName: String; var PackIndex, FileIndex: Integer);
var
  i, j, c, k: Integer;
begin
  //Результат по умолчанию
  PackIndex := -1;
  FileIndex := -1;

  //Проверить файл в архивах
  FileName := LowerCase(FileName);

  //Цикл в обратном порядке по архивам
  c := FExtPackList.PackList.Count - 1;
  for i := c downto 0 do
  begin
    //Цикл в обратном порядке по архиву
    k := FExtPackList.PackList.Item[i].Count - 1;
    for j := k downto 0 do
      if FileName = LowerCase(FExtPackList.PackList.Item[i].Item[j].FileName) then
      begin
        PackIndex := i;
        FileIndex := j;
        Exit;
      end;
  end;
end;


function TsgeExtensionFileSystem.GetNormalFileName(FileName: String): String;
begin
  if sgeIsFullPath(FileName) then
    Result := FileName
  else
    Result := FMainDir + FileName;
end;


function TsgeExtensionFileSystem.GetName: String;
begin
  Result := Extension_FileSystem;
end;


constructor TsgeExtensionFileSystem.Create;
begin
  try
    inherited Create;

    //Параметры по умолчанию
    FMainDir := sgeGetApplicationDirectory;
    FScreenshotDir := FMainDir + 'Screenshot\';

    //Получить ссылки на объекты
    FExtPackList := TsgeExtensionPackList(GetExtension(Extension_PackList));
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


procedure TsgeExtensionFileSystem.ForceDirectories(Directory: String);
begin
  if Directory = '' then
    Exit;

  if not sgeForceDirectories(FMainDir + Directory) then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateDirectory, Directory);
end;


function TsgeExtensionFileSystem.DirectoryExists(Directory: String): Boolean;
begin
  Result := sgeDirectoryExists(FMainDir + Directory);
end;


function TsgeExtensionFileSystem.FileExists(FileName: String): Boolean;
var
  PackIdx, FileIdx: Integer;
begin
  //Проверить в файловой системе
  if sgeFileExists(GetNormalFileName(FileName)) then
    Exit(True);

  //Проверить файл в архивах
  GetFilePackIndex(FileName, PackIdx, FileIdx);
  Result := FileIdx <> -1;
end;


procedure TsgeExtensionFileSystem.ReadFile(FileName: String; Stream: TsgeMemoryStream);
const
  ModeFile = 0;
  ModePack = 1;
var
  Fn: String;
  F: TsgeFile;
  PackIdx, FileIdx: Integer;
  Mode: Byte;
begin
  //Определить имя файла
  Fn := GetNormalFileName(FileName);

  //Определить режим загрузки
  if sgeFileExists(Fn) then
    Mode := ModeFile
  else
    Mode := ModePack;

  //Загрузить файл
  case Mode of
    ModeFile:
      try
        F := TsgeFile.Create(Fn, fmRead, False);
        Stream.Size := F.Size;
        F.Read(Stream.Data^, F.Size);
        F.Free;
      except
        on E: EsgeException do
        begin
          F.Free;
          raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
        end;
      end;


    //Чтение из архива
    ModePack:
    begin
      //Поиск индекса в архивах
      GetFilePackIndex(FileName, PackIdx, FileIdx);

      if FileIdx = -1 then
        raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

      //Загрузка из архива
      try
        FExtPackList.PackList.Item[PackIdx].GetItemData(FileIdx, Stream);
      except
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName);
      end;
    end;
  end;
end;


function TsgeExtensionFileSystem.ReadFile(FileName: String): String;
var
  MS: TsgeMemoryStream;
begin
  Result := '';

  //Прочитать файл
  MS := TsgeMemoryStream.Create;
  try
    ReadFile(FileName, MS);
    Result := MS.ToString;
  finally
    MS.Free;
  end;
end;


procedure TsgeExtensionFileSystem.WriteFile(FileName: String; Stream: TsgeMemoryStream);
var
  Fn, Dir: String;
begin
  //Определить имя файла
  Fn := GetNormalFileName(FileName);
  Dir := sgeExtractFilePath(Fn);

  //Cоздать недостающие каталоги
  if not sgeForceDirectories(Dir) then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateDirectory, Dir);

  //Сохранить файл
  try
    Stream.SaveToFile(Fn);
  except
    raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, Fn);
  end;
end;


procedure TsgeExtensionFileSystem.WriteFile(FileName: String; Data: String);
var
  MS: TsgeMemoryStream;
begin
  //Записать файл
  MS := TsgeMemoryStream.Create;
  try
    MS.FromString(Data);
    WriteFile(FileName, MS);
  finally
    MS.Free;
  end;
end;


procedure TsgeExtensionFileSystem.DeleteFile(FileName: String);
var
  Fn: String;
begin
  //Определить имя файла
  Fn := GetNormalFileName(FileName);

  //Удалить
  if not sgeDeleteFile(Fn) then
    raise EsgeException.Create(_UNITNAME, Err_CantDeleteFile, Fn);
end;


procedure TsgeExtensionFileSystem.RenameFile(OldName, NewName: String);
var
  OldFn, NewFn: String;
begin
  //Определить имена файлов
  OldFn := GetNormalFileName(OldName);
  NewFn := GetNormalFileName(NewName);

  if not sgeRenameFile(OldFn, NewFn) then
    raise EsgeException.Create(_UNITNAME, Err_CantRenameFile);
end;


function TsgeExtensionFileSystem.GetFileSize(FileName: String): Int64;
var
  PackIdx, FileIdx: Integer;
  Fn: String;
begin
  Result := -1;

  //Проверить файл на диске
  Fn := GetNormalFileName(FileName);
  if sgeFileExists(Fn) then
    Exit(sgeGetFileSize(Fn));

  //Поиск в архивах
  GetFilePackIndex(FileName, PackIdx, FileIdx);
  if FileIdx <> -1 then
    Result := FExtPackList.PackList.Item[PackIdx].Item[FileIdx].DataSize;
end;


procedure TsgeExtensionFileSystem.GetFileList(Directory: String; List: TsgeStringList);
var
  i, j, c, k: Integer;
  fnPath: String;
  so: TsgeSearchOptions;
begin
  //Подготовить список
  List.Clear;
  so := List.SearchOptions;
  List.SearchOptions := [soUnique];

  //Получить список из файловой системы
  sgeGetFileListInFolder(FMainDir + Directory, List);


  //Получить список из архивов
  if Directory <> '' then
    Directory := LowerCase(sgeCheckPathDelimiter(Directory));

  c := FExtPackList.PackList.Count - 1;
  for i := c downto 0 do
  begin
    k := FExtPackList.PackList.Item[i].Count - 1;
    for j := k downto 0 do
    begin
      fnPath := LowerCase(sgeExtractFilePath(FExtPackList.PackList.Item[i].Item[j].FileName));
      if fnPath = Directory then
        List.Add(sgeExtractFileName(FExtPackList.PackList.Item[i].Item[j].FileName));
    end;
  end;

  //Вернуть режим списку
  List.SearchOptions := so;
end;


procedure TsgeExtensionFileSystem.GetDirectoryList(Directory: String; List: TsgeStringList);
var
  i, j, c, k, DirSize: Integer;
  so: TsgeSearchOptions;
  Lst: TsgeStringList;
  fn: String;
  b: Boolean;
begin
  //Подготовить список
  List.Clear;
  so := List.SearchOptions;
  List.SearchOptions := [soUnique];

  //Получить список из файловой системы
  sgeGetDirectoryListInFolder(FMainDir + Directory, List);


  //Получить список каталогов из архивов
  if Directory <> '' then
    Directory := LowerCase(sgeCheckPathDelimiter(Directory));
  DirSize := Length(Directory);

  //Просмотреть архивы
  Lst := TsgeStringList.Create;
  Lst.Separator := '\';

  c := FExtPackList.PackList.Count - 1;
  for i := c downto 0 do
  begin
    k := FExtPackList.PackList.Item[i].Count - 1;
    for j := k downto 0 do
    begin
      fn := sgeExtractFilePath(FExtPackList.PackList.Item[i].Item[j].FileName); //Выделить путь из имени файла
      B := False;
      if Directory = '' then                                        //Частный случай, Корень системы
        B := True;
      if sgePos(Directory, LowerCase(fn)) = 1 then                  //Совпадение с начальным каталогом
         B := True;
      if B then
      begin
        Delete(fn, 1, DirSize);                                     //Отрезать базовый путь
        Lst.FromString(fn);                                         //Разобрать путь на части
        if Lst.Count > 0 then List.Add(Lst.Part[0]);                //Если есть больше одной части, то добавить
      end;
    end;
  end;
  Lst.Free;

  //Вернуть режим списку
  List.SearchOptions := so;
end;


procedure TsgeExtensionFileSystem.FindFiles(Directory: String; List: TsgeStringList; Ext: String);
var
  i, j, c, k, DirSize: Integer;
  so: TsgeSearchOptions;
  fn, fnExt: String;
  b: Boolean;
begin
  //Подготовить список
  List.Clear;
  so := List.SearchOptions;
  List.SearchOptions := [soUnique];

  //Подготовить базовый каталог
  if Directory <> '' then
    Directory := LowerCase(sgeCheckPathDelimiter(Directory));
  DirSize := Length(Directory);

  //Получить список из файловой системы
  sgeFindFilesInFolderByExt(Directory, List, Ext);

  //Поиск файлов в архивах
  c := FExtPackList.PackList.Count - 1;
  Ext := LowerCase(Ext);
  for i := c downto 0 do
  begin
    k := FExtPackList.PackList.Item[i].Count - 1;
    for j := k downto 0 do
    begin
      //Имя файла
      fn := FExtPackList.PackList.Item[i].Item[j].FileName;

      //Проверить каталог
      if (Directory = '') or (sgePos(Directory, LowerCase(fn)) = 1) then
      begin
        //Проверить по расширению
        Delete(fn, 1, DirSize);
        B := False;
        if Ext = '' then
          B := True
        else
        begin
          fnExt := LowerCase(sgeExtractFileExt(fn));
          if (fnExt <> '') and (fnExt[1] = '.') then Delete(fnExt, 1, 1);
          if fnExt = Ext then B := True;
        end;

        //Добавить в результат
        if B then
          List.Add(fn);
      end;
    end;
  end;

  //Вернуть режим списку
  List.SearchOptions := so;
end;


end.

