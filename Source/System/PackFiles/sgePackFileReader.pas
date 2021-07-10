{
Пакет             Simple Game Engine 2
Файл              sgePackFileReader.pas
Версия            1.0
Создан            07.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс чтения содежимого архива
}
{$Include Defines.inc}

unit sgePackFileReader;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream, sgeFile;


type
  //Описание одного блока при чтении
  TsgePackFileReaderBlock = record
    StartPos: Int64;                  //Адрес начала данных
    DataSize: Int64;                  //Длина данных
    FileName: String;                 //Имя файла
  end;


  //Класс чтения содержимого архива
  TsgePackFileReader = class
  private
    FPackFile: TsgeFile;                          //Файл
    FFileList: array of TsgePackFileReaderBlock;  //Список файлов

    procedure Read;
    procedure Add(ABlock: TsgePackFileReaderBlock);
    procedure Clear;
    function  GetCount: Integer;
    function  GetItem(Index: Integer): TsgePackFileReaderBlock;

    procedure SetFileName(AFileName: String);
    function  GetFileName: String;
  public
    constructor Create(FileName: String);
    destructor  Destroy; override;

    function  IndexOf(FileName: String): Integer;
    procedure GetItemData(Index: Integer; Stream: TsgeMemoryStream);

    property FileName: String read GetFileName write SetFileName;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgePackFileReaderBlock read GetItem;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils, sgePackFile;

const
  _UNITNAME = 'PackFileReader';

  Err_WrongFileHeader   = 'WrongFileHeader';
  Err_IndexOutOfBounds  = 'IndexOutOfBounds';


procedure TsgePackFileReader.Read;
var
  Hdr, DefHdr: TsgePackFileHeader;
  BlockHdr: TsgePackFileBlock;
  BlockRec: TsgePackFileReaderBlock;
  Offset: Int64;
begin
  //Прочитать заголовок файла
  DefHdr := sgePackFile_GetFileHead;
  FPackFile.Seek(0);
  FPackFile.Read(Hdr, SizeOf(TsgePackFileHeader));

  //Сравнить сигнатуру заголовка
  if not sgeCompareMem(@Hdr, @DefHdr, SizeOf(TsgePackFileHeader)) then
    raise EsgeException.Create(_UNITNAME, Err_WrongFileHeader, FPackFile.FileName);

  //Читать в цикле блоки
  Offset := SizeOf(TsgePackFileHeader);
  while Offset < FPackFile.Size do
    begin
    FPackFile.Seek(Offset, foBegin);                                  //Подвинуть маркер
    FPackFile.Read(BlockHdr, SizeOf(TsgePackFileBlock));              //Прочитать заголовок блока

    //Подготовить запись для списка
    BlockRec.StartPos := Offset + SizeOf(TsgePackFileBlock);          //Начало данных
    BlockRec.DataSize := BlockHdr.TotalSize - BlockHdr.NameSize - SizeOf(TsgePackFileBlock);  //Размер данных

    //Прочитать имя файла
    FPackFile.Seek(BlockRec.StartPos + BlockRec.DataSize, foBegin);   //Подвинуть маркер
    SetLength(BlockRec.FileName, BlockHdr.NameSize);                  //Выделить память для имени
    FPackFile.Read(BlockRec.FileName[1], BlockHdr.NameSize);          //Прочитать имя файла

    //Сместить указатель на следующий блок
    Offset := Offset + BlockHdr.TotalSize;

    //Добавить файл в список
    Add(BlockRec);
    end;
end;


procedure TsgePackFileReader.Add(ABlock: TsgePackFileReaderBlock);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FFileList, c + 1);
  FFileList[c] := ABlock;
end;


procedure TsgePackFileReader.Clear;
begin
  SetLength(FFileList, 0);
end;


function TsgePackFileReader.GetCount: Integer;
begin
  Result := Length(FFileList);
end;


function TsgePackFileReader.GetItem(Index: Integer): TsgePackFileReaderBlock;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FFileList[Index];
end;


procedure TsgePackFileReader.SetFileName(AFileName: String);
begin
  if FPackFile.FileName = AFileName then Exit;

  FPackFile.FileName := AFileName;  //Изменить файл
  Read;                             //Перечитать список файлов
end;


function TsgePackFileReader.GetFileName: String;
begin
  Result := FPackFile.FileName;
end;


constructor TsgePackFileReader.Create(FileName: String);
begin
  FPackFile := TsgeFile.Create(FileName, fmRead, False);
  Read;
end;


destructor TsgePackFileReader.Destroy;
begin
  FPackFile.Free;
  Clear;
end;


function TsgePackFileReader.IndexOf(FileName: String): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  FileName := LowerCase(FileName);
  c := GetCount - 1;
  for i := c downto 0 do
    if FileName = LowerCase(FFileList[i].FileName) then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgePackFileReader.GetItemData(Index: Integer; Stream: TsgeMemoryStream);
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  //Выделить память для файла
  Stream.Size := FFileList[Index].DataSize;

  //Прочитать из файла
  FPackFile.Seek(FFileList[Index].StartPos);
  FPackFile.Read(Stream.Data^, FFileList[Index].DataSize);
end;



end.

