{
Пакет             Simple Game Engine 2
Файл              sgeFileUtils.pas
Версия            1.5
Создан            08.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Вспомогательные функции файловой системы
}
{$Include Defines.inc}

unit sgeFileUtils;

{$mode objfpc}{$H+}

interface

uses
  sgeStringList;


const
  sgeLineEnd = #13#10;
  sgeDriveSeparator = ':';
  sgePathSeparator = '\';
  sgeExtSeparator = '.';


function  sgeGetUniqueFileName: String;

function  sgeExtractFileName(const FileName: String): String;
function  sgeExtractFileExt(const FileName: String): String;
function  sgeExtractFilePath(const FileName: String): String;
function  sgeIsFullPath(const FileName: String): Boolean;                                   //Определить является ли путь к файлу полным
function  sgeCheckPathDelimiter(Path: String): String;
function  sgeChangeFileExt(const FileName: String; Ext: String = ''): String;               //Изменить расширение файла

procedure sgeFindFilesInFolder(Path: String; List: TsgeStringList);                         //Поиск всех файлов каталоге
procedure sgeFindFilesInFolderByExt(Path: String; List: TsgeStringList; Ext: String = '');  //Поиск всех файлов каталоге по маске
procedure sgeGetFileListInFolder(Path: String; List: TsgeStringList);                       //Поиск файлов в каталоге
procedure sgeGetDirectoryListInFolder(Path: String; List: TsgeStringList);                  //Поиск файлов в каталоге


implementation

uses
  sgeDateUtils, sgeOSPlatform;



function sgeGetUniqueFileName: String;
begin
  Result := sgeFormatDateTime('yyyy.mm.dd-hh.nn.ss', sgeNow);
end;


function sgeExtractFileName(const FileName: String): String;
var
  i: longint;
  EndSep: Set of Char;
begin
  Result := '';

  i := Length(FileName);
  EndSep := AllowDirectorySeparators + AllowDriveSeparators;

  while (i > 0) and not (FileName[i] in EndSep) do
    Dec(i);

  Result := Copy(FileName, i + 1, MaxInt);
end;


function sgeExtractFileExt(const FileName: String): String;
var
  I: longint;
  EndSep: Set of Char;
  SOF: Boolean;
begin
  Result := '';
  I := Length(FileName);
  EndSep := AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];

  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);

  if (I > 0) and (FileName[I] = ExtensionSeparator) then
    begin
	  SOF := (I = 1) or (FileName[i - 1] in AllowDirectorySeparators);
	  if (Not SOF) or FirstDotAtFileNameStartIsExtension then Result := Copy(FileName, I, MaxInt);
	  end else Result := '';
end;


function sgeExtractFilePath(const FileName: String): String;
var
  i: longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep := AllowDirectorySeparators + AllowDriveSeparators;

  while (i > 0) and not (FileName[i] in EndSep) do
    Dec(i);

  If I > 0 then Result := Copy(FileName, 1, i) else Result := '';
end;


function sgeIsFullPath(const FileName: String): Boolean;
const
  DriveLeter = ['A'..'Z'];
begin
  Result := False;

  if Length(FileName) > 3 then
    Result := (FileName[1] in DriveLeter) and (FileName[2] = sgeDriveSeparator) and (FileName[3] = sgePathSeparator);
end;


function sgeCheckPathDelimiter(Path: String): String;
var
  c: Integer;
begin
  Result := Path;

  c := Length(Result);
  if c = 0 then Exit;

  if Result[c] <> System.DirectorySeparator then Result := Path + System.DirectorySeparator;
end;


function sgeChangeFileExt(const FileName: String; Ext: String): String;
var
  i: Integer;
begin
  //Результат по умолчанию
  Result := FileName;

  //Если разделитель с точкой, то удалить
  if Ext <> '' then if Ext[1] = sgeExtSeparator then Delete(Ext, 1, 1);

  for i := Length(FileName) downto 1 do
    begin
    //Дальше смотреть нет смысла
    if (FileName[i] = sgePathSeparator) then Break;

    //Проверить на разделитель расширения
    if FileName[i] = sgeExtSeparator then
      begin
      Result := Copy(FileName, 1, i - 1);
      if Ext <> '' then Result := Result + sgeExtSeparator + Ext;
      Break;
      end;
    end;
end;


//Рекурсивный поиск файлов в каталоге
procedure sgeFindFilesInFolder(Path: String; List: TsgeStringList);
var
  o: TsgeSearchRec;
  Idx: Integer;
begin
  Path := sgeCheckPathDelimiter(Path);

  Idx := sgeFindFirst(Path + '*', FileAttribAnyFile, o);
  while Idx = 0 do
    begin

    if (o.Name <> '.') and (o.Name <> '..') then
      begin
      if (o.Attr and FileAttribDirectory) = FileAttribDirectory then
        sgeFindFilesInFolder(Path + o.Name, List) else List.Add(Path + o.Name);
      end;

    Idx := sgeFindNext(o);
    end;

  sgeFindClose(o);
end;


//Рекурсивный поиск файлов в каталоге по расширению
procedure sgeFindFilesInFolderByExt(Path: String; List: TsgeStringList; Ext: String);
var
  i, c, len: Integer;
  Lst: TsgeStringList;
  FnExt: String;
begin
  //Подготовить расширение
  Ext := LowerCase(Ext);
  if (Ext <> '') and (Ext[1] = '.') then Delete(Ext, 1, 1);

  //Подготовить путь
  if Path <> '' then Path := sgeCheckPathDelimiter(Path);
  Len := Length(Path);

  try
    //Создать временный список
    Lst := TsgeStringList.Create;

    //Взять файлы в каталоге
    sgeFindFilesInFolder(Path, Lst);

    //Отпилить базовый путь
    c := Lst.Count - 1;
    for i := 0 to c do
      begin
      FnExt := Lst.Part[i];
      Delete(FnExt, 1, Len);
      Lst.Part[i] := FnExt;
      end;


    //Очистить выходной массив
    List.Clear;

    //Проверить расширение на пустое значение
    if Ext = '' then
      begin
      List.CopyFrom(Lst);
      Exit;
      end;

    //Проврить расширение
    c := Lst.Count - 1;
    for i := 0 to c do
      begin
      fnExt := LowerCase(sgeExtractFileExt(Lst.Part[i]));
      if (fnExt <> '') and (fnExt[1] = '.') then Delete(fnExt, 1, 1);
      if FnExt = Ext then List.Add(Lst.Part[i]);
      end;

  finally
    Lst.Free;
  end;
end;


procedure sgeGetFileListInFolder(Path: String; List: TsgeStringList);
var
  o: TsgeSearchRec;
  Idx: Integer;
begin
  Path := sgeCheckPathDelimiter(Path);

  Idx := sgeFindFirst(Path + '*', FileAttribAnyFile, o);
  while Idx = 0 do
    begin

    if (o.Name <> '.') and (o.Name <> '..') then
      if (o.Attr and FileAttribDirectory) <> FileAttribDirectory then List.Add(o.Name);

    Idx := sgeFindNext(o);
    end;

  sgeFindClose(o);
end;


procedure sgeGetDirectoryListInFolder(Path: String; List: TsgeStringList);
var
  o: TsgeSearchRec;
  Idx: Integer;
begin
  Path := sgeCheckPathDelimiter(Path);

  Idx := sgeFindFirst(Path + '*', FileAttribAnyFile, o);
  while Idx = 0 do
    begin

    if (o.Name <> '.') and (o.Name <> '..') then
      if (o.Attr and FileAttribDirectory) = FileAttribDirectory then List.Add(o.Name);

    Idx := sgeFindNext(o);
    end;

  sgeFindClose(o);
end;


end.

