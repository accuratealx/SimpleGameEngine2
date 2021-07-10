{
Пакет             Simple Game Engine 2
Файл              sgePackFileList.pas
Версия            1.1
Создан            08.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Список архивов
}
{$Include Defines.inc}

unit sgePackFileList;

{$mode objfpc}{$H+}

interface

uses
  sgePackFileReader;


type
  TsgePackFileList = class
  private
    FPackList: array of TsgePackFileReader;

    function  GetCount: Integer;
    function  GetItem(Index: Integer): TsgePackFileReader;
  public
    destructor  Destroy; override;

    procedure Clear;                              //Удалить элементы
    procedure Add(PackFile: TsgePackFileReader);
    procedure Add(FileName: String);              //Добавить из файла (Полный путь)
    procedure Delete(Index: Integer);
    procedure Delete(Name: String);               //Удалить архив по имени без полного пути
    function  IndexOf(Name: String): Integer;     //Найти индекс по имени без полного пути

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgePackFileReader read GetItem;
  end;




implementation

uses
  sgeErrors, sgeSystemUtils, sgeFileUtils, sgeOSPlatform;

const
  _UNITNAME = 'PackFileList';

  Err_IndexOutOfBounds  = 'IndexOutOfBounds';
  Err_FileNotFound      = 'FileNotFound';
  Err_CantReadFile      = 'CantReadFile';


function TsgePackFileList.GetCount: Integer;
begin
  Result := Length(FPackList);
end;


function TsgePackFileList.GetItem(Index: Integer): TsgePackFileReader;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FPackList[Index];
end;


destructor TsgePackFileList.Destroy;
begin
  Clear;
end;


procedure TsgePackFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to GetCount - 1 do
    FPackList[i].Free;

  SetLength(FPackList, 0);
end;


procedure TsgePackFileList.Add(PackFile: TsgePackFileReader);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FPackList, c + 1);
  FPackList[c] := PackFile;
end;


procedure TsgePackFileList.Add(FileName: String);
var
  F: TsgePackFileReader;
begin
  //Проверить существование файла
  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  //Загрузить файл
  try
    F := TsgePackFileReader.Create(FileName);
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
  end;

  //Добавить в массив архивов
  Add(F);
end;


procedure TsgePackFileList.Delete(Index: Integer);
var
  i, c: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FPackList[Index].Free;

  for i := Index to c - 1 do
    FPackList[i] := FPackList[i + 1];
  SetLength(FPackList, c);
end;


procedure TsgePackFileList.Delete(Name: String);
begin
  Delete(IndexOf(Name));
end;


function TsgePackFileList.IndexOf(Name: String): Integer;
var
  PackName: String;
  i, c: Integer;
begin
  Result := -1;

  Name := LowerCase(sgeExtractFileName(Name));
  c := GetCount - 1;
  for i := 0 to c do
    begin
    PackName := LowerCase(sgeExtractFileName(FPackList[i].FileName));
    if Name = PackName then
      begin
      Result := i;
      Break;
      end;
    end;
end;


end.

