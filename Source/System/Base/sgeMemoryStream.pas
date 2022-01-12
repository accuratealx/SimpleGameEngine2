{
Пакет             Simple Game Engine 2
Файл              sgeFileStream.pas
Версия            1.4
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс доступа к набору байт в памяти
}
{$Include Defines.inc}

unit sgeMemoryStream;

{$mode objfpc}{$H+}

interface

type
  TsgeMemoryStream = class
  private
    FData: Pointer;
    FSize: Int64;

    procedure SetSize(ASize: Int64);
  public
    destructor Destroy; override;

    procedure Clear;
    procedure Write(const Buffer; Offset: Int64; Size: Int64);
    procedure Read(out Buffer; Offset: Int64; Size: Int64);

    procedure FromString(Str: String);
    function  ToString: String; override;
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);

    property Data: Pointer read FData;
    property Size: Int64 read FSize write SetSize;
  end;





implementation

uses
  sgeErrors, sgeSystemUtils, sgeOSPlatform, sgeFile;


const
  _UNITNAME = 'MemoryStream';

  Err_IndexOutsideTheData = 'IndexOutsideTheData';
  Err_CantReallocMemory   = 'CantReallocMemory';
  Err_FileNotFound        = 'FileNotFound';
  Err_CantReadFile        = 'CantReadFile';
  Err_CantWriteFile       = 'CantWriteFile';



procedure TsgeMemoryStream.SetSize(ASize: Int64);
begin
  FSize := ASize;
  FData := ReAllocMem(FData, FSize);
end;


destructor TsgeMemoryStream.Destroy;
begin
  Clear;
end;


procedure TsgeMemoryStream.Clear;
begin
  Freemem(FData, FSize);
  FSize := 0;
  FData := nil;
end;


procedure TsgeMemoryStream.Write(const Buffer; Offset: Int64; Size: Int64);
var
  Sz: Int64;
begin
  if Size = 0 then Exit;

  //Проверить длину памяти что бы влез буфер
  try
    Sz := Offset + Size;
    if Sz > FSize then
      begin
      FData := ReAllocMem(FData, Sz);
      FSize := Sz;
      end;
  except
    raise EsgeException.Create(_UNITNAME, Err_CantReallocMemory, sgeIntToStr(Sz));
  end;

  //Записать буфер
  Move(Buffer, (FData + Offset)^, Size);
end;


procedure TsgeMemoryStream.Read(out Buffer; Offset: Int64; Size: Int64);
var
  sz: Int64;
begin
  if Size = 0 then Exit;

  //Проверить возможность скопировать
  sz := Offset + Size;
  if sz > FSize then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutsideTheData, sgeIntToStr(Sz));

  //Скопировать данные
  Move((FData + Offset)^, Buffer, Size);
end;


procedure TsgeMemoryStream.FromString(Str: String);
var
  Sz: Int64;
begin
  Sz := Length(Str);                //Узнать длину
  FData := ReAllocMem(FData, Sz);   //Подготовить память
  Move(Str[1], FData^, Sz);         //Скопировать данные
  FSize := Sz;                      //Запомнить размер
end;


function TsgeMemoryStream.ToString: String;
begin
  Result := '';
  if FSize = 0 then Exit;

  SetLength(Result, FSize);
  Read(Result[1], 0, FSize);
end;


procedure TsgeMemoryStream.SaveToFile(FileName: String);
var
  F: TsgeFile;
begin
  try
    F := TsgeFile.Create(FileName, fmWrite, True);
    F.Size := 0;
    F.Write(Data^, FSize);
    F.Flush;
    F.Free;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, FileName, E.Message);
  end;
end;


procedure TsgeMemoryStream.LoadFromFile(FileName: String);
var
  F: TsgeFile;
begin
  //Проверить на существование файла
  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);

  //Загрузка из файла
  try
    try
      F := TsgeFile.Create(FileName, fmRead, False);
      F.Seek(0, foBegin);
      FSize := F.Size;                          //Запомнить размер
      FData := ReAllocMem(FData, F.Size);       //Выделить память на куче
      F.Read(FData^, FSize);                    //Прочитать из файла
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
    end;


  finally
    F.Free;
  end;
end;



end.

