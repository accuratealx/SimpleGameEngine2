{
Пакет             Simple Game Engine 2
Файл              sgeFile.pas
Версия            1.1
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс доступа к файлу
}
{$Include Defines.inc}

unit sgeFile;

{$mode objfpc}{$H+}

interface


type
  //Режимы открытия файла
  TsgeFileMode = (fmRead, fmWrite);

  //Начало отступа в файле
  TsgeFileOrigin = (foBegin, foCurrent, foEnd);


  TsgeFile = class
  private
    FHandle: THandle;                         //Хэндл файла
    FSize: Int64;                             //Размер
    FMode: TsgeFileMode;                      //Режим работы с файлом
    FExclusive: Boolean;                      //Монопольный доступ
    FPosition: Int64;                         //Текушее положение курсора
    FFileName: String;                        //Имя файла

    procedure OpenFile(FileName: String);
    procedure CloseFile;

    procedure SetFileName(AFileName: String);
    procedure SetSize(ASize: Int64);
    procedure SetMode(AMode: TsgeFileMode);
    procedure SetExclusive(AExclusive: Boolean);
  public
    constructor Create(FileName: String; Mode: TsgeFileMode = fmRead; Exclusive: Boolean = False);
    destructor  Destroy; override;

    procedure Flush;
    procedure SeekEnd;
    procedure Seek(Offset: Int64; Origin: TsgeFileOrigin = foBegin);
    procedure Write(const Buffer; Size: Int64);
    procedure Read(out Buffer; Size: Int64);

    function  ToString: String; reintroduce;

    property Handle: THandle read FHandle;
    property FileName: String read FFileName write SetFileName;
    property Size: Int64 read FSize write SetSize;
    property Mode: TsgeFileMode read FMode write SetMode;
    property Position: Int64 read FPosition;
  end;



implementation


uses
  sgeErrors, sgeOSPlatform;


const
  _UNITNAME = 'File';

  Err_CantOpenFile      = 'CantOpenFile';
  Err_CantCreateFile    = 'CantCreateFile';
  Err_CantReadFileSize  = 'CantReadFileSize';
  Err_CantSetFileSize   = 'CantSetFileSize';
  Err_CantFlushFile     = 'CantFlushFile';
  Err_CantWriteBuffer   = 'CantWriteBuffer';
  Err_CantReadBuffer    = 'CantReadBuffer';



procedure TsgeFile.OpenFile(FileName: String);
const
  MCreate = 0;
  MOpen = 1;
var
  Method: Byte;
  H: THandle;
  s: String;
  Sz: Int64;
begin
  //Определить как открывать файл
  if sgeFileExists(FileName) then Method := MOpen else Method := MCreate;

  //Подключиться к файлу
  case Method of
    MOpen   : H := sgeOpenFile(FileName, Ord(FMode));
    MCreate : H := sgeCreateFile(FileName, Ord(FMode));
  end;

  //Проверить открытие файла
  if H = FileInvalidHandle then
    begin
    case Method of
      MCreate : s := Err_CantCreateFile;
      MOpen   : s := Err_CantOpenFile;
    end;

    raise EsgeException.Create(_UNITNAME, s, GetLastOSErrorMessage);
    end;

  //Прочитать размер
  Sz := sgeGetFileSize(FileName);
  if Sz = -1 then
    begin
    sgeCloseFile(H);
    raise EsgeException.Create(_UNITNAME, Err_CantReadFileSize);
    end;

  //Применить параметры
  CloseFile;
  FPosition := 0;
  FFileName := FileName;
  FHandle := H;
  FSize := Sz;
end;


procedure TsgeFile.CloseFile;
begin
  sgeCloseFile(FHandle);
  FHandle := FileInvalidHandle;
  FPosition := 0;
  FSize := 0;
end;


procedure TsgeFile.SetFileName(AFileName: String);
begin
  if FFileName = AFileName then Exit;

  OpenFile(AFileName);
end;


procedure TsgeFile.SetSize(ASize: Int64);
begin
  if FHandle = FileInvalidHandle then Exit;
  if FSize = ASize then Exit;

  if not sgeTruncateFile(FHandle, ASize) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetFileSize, GetLastOSErrorMessage);

  //Применить параметры
  FSize := ASize;
  FPosition := ASize;
end;


procedure TsgeFile.SetMode(AMode: TsgeFileMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  CloseFile;
  OpenFile(FFileName);
end;


procedure TsgeFile.SetExclusive(AExclusive: Boolean);
begin
  if FExclusive = AExclusive then Exit;

  FExclusive := AExclusive;
  CloseFile;
  OpenFile(FFileName);
end;


constructor TsgeFile.Create(FileName: String; Mode: TsgeFileMode; Exclusive: Boolean);
begin
  FHandle := FileInvalidHandle;
  FMode := Mode;
  FExclusive := Exclusive;

  OpenFile(FileName);
end;


destructor TsgeFile.Destroy;
begin
  CloseFile;
end;


procedure TsgeFile.Flush;
begin
  if FHandle = FileInvalidHandle then Exit;

  if not sgeFlushFile(FHandle) then
    raise EsgeException.Create(_UNITNAME, Err_CantFlushFile, GetLastOSErrorMessage);
end;


procedure TsgeFile.SeekEnd;
begin
  Seek(0, foEnd);
end;


procedure TsgeFile.Seek(Offset: Int64; Origin: TsgeFileOrigin);
begin
  if FHandle = FileInvalidHandle then Exit;

  FPosition := sgeSeekFile(FHandle, Offset, Ord(Origin));
end;


procedure TsgeFile.Write(const Buffer; Size: Int64);
begin
  if FHandle = FileInvalidHandle then Exit;

  if not sgeWriteFile(FHandle, Buffer, Size) then
    raise EsgeException.Create(_UNITNAME, Err_CantWriteBuffer, GetLastOSErrorMessage);
end;


procedure TsgeFile.Read(out Buffer; Size: Int64);
begin
  if FHandle = FileInvalidHandle then Exit;

  if not sgeReadFile(FHandle, Buffer, Size) then
    raise EsgeException.Create(_UNITNAME, Err_CantReadBuffer, GetLastOSErrorMessage);
end;


function TsgeFile.ToString: String;
begin
  Result := '';
  if FSize = 0 then Exit;

  SetLength(Result, FSize);
  Seek(0, foBegin);
  Read(Result[1], FSize);
end;



end.

