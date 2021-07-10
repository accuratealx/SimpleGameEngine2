{
Пакет             Simple Game Engine 2
Файл              sgePackFileWriter.pas
Версия            1.0
Создан            08.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс записи содежимого архива
}
{$Include Defines.inc}

unit sgePackFileWriter;

{$mode objfpc}{$H+}

interface


type
  //Тип файла
  TPackFileType = (pftFile, pftPack);


  //Описание одного блока при записи
  TsgePackFileWriterBlock = record
    PackName: String;           //Имя файла внутри архива
    FileName: String;           //Полный путь к файлу на диске
    FileSize: Cardinal;         //Размер данных в байтах
    FileType: TPackFileType;    //Тип файла
    Index: Word;                //Номер блока в архиве
  end;


  TsgePackFileWriter = class
  private
    FFileList: array of TsgePackFileWriterBlock;

    function  GetCount: Integer;
    procedure SetItem(Index: Integer; AItem: TsgePackFileWriterBlock);
    function  GetItem(Index: Integer): TsgePackFileWriterBlock;
  public
    destructor  Destroy; override;

    procedure Clear;
    function  IndexOf(PackName: String): Integer;
    procedure Add(&File: TsgePackFileWriterBlock);
    procedure Add(PackName, FileName: String; FileType: TPackFileType = pftFile; Index: Word = 0);
    procedure Delete(Index: Integer);
    procedure Delete(PackName: String);

    procedure SaveToFile(FileName: String);

    property Count: Integer read GetCount;
    property Item[Index: Integer]: TsgePackFileWriterBlock read GetItem write SetItem;
  end;



implementation

uses
  sgeErrors, sgeSystemUtils, sgeMemoryStream, sgeFile, sgePackFile, sgePackFileReader;


const
  _UNITNAME = 'PackFileWriter';

  Err_IndexOutOfBounds  = 'IndexOutOfBounds';
  Err_CantWriteFile     = 'CantWriteFile';



function TsgePackFileWriter.GetCount: Integer;
begin
  Result := Length(FFileList);
end;


procedure TsgePackFileWriter.SetItem(Index: Integer; AItem: TsgePackFileWriterBlock);
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FFileList[Index] := AItem;
end;


function TsgePackFileWriter.GetItem(Index: Integer): TsgePackFileWriterBlock;
begin
  if (Index < 0) or (Index > GetCount - 1) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FFileList[Index];
end;


destructor TsgePackFileWriter.Destroy;
begin
  Clear;
end;


procedure TsgePackFileWriter.Clear;
begin
  SetLength(FFileList, 0);
end;


function TsgePackFileWriter.IndexOf(PackName: String): Integer;
var
  i, c: Integer;
  s: String;
begin
  Result := -1;
  c := GetCount - 1;
  s := LowerCase(PackName);

  for i := 0 to c do
    if LowerCase(FFileList[i].PackName) = s then
      begin
      Result := i;
      Break;
      end;
end;


procedure TsgePackFileWriter.Add(&File: TsgePackFileWriterBlock);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FFileList, c + 1);
  FFileList[c] := &File;
end;


procedure TsgePackFileWriter.Add(PackName, FileName: String; FileType: TPackFileType; Index: Word);
var
  F: TsgePackFileWriterBlock;
begin
  F.PackName := PackName;
  F.FileName := FileName;
  F.FileType := FileType;
  F.Index := Index;

  Add(F);
end;


procedure TsgePackFileWriter.Delete(Index: Integer);
var
  i, c: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  for i := Index to c - 1 do
    FFileList[i] := FFileList[i + 1];

  SetLength(FFileList, c);
end;


procedure TsgePackFileWriter.Delete(PackName: String);
begin
  Delete(IndexOf(PackName));
end;


procedure TsgePackFileWriter.SaveToFile(FileName: String);
var
  F: TsgeFile;
  FPack: TsgePackFileReader;
  Ms: TsgeMemoryStream;
  i, c: Integer;
  FileHead: TsgePackFileHeader;
  BlockHead: TsgePackFileBlock;
begin
  try
    try
      F := TsgeFile.Create(FileName, fmWrite, True);
      F.Size := 0;

      //Записать заголовок
      FileHead := sgePackFile_GetFileHead;
      F.Write(FileHead, SizeOf(TsgePackFileHeader));

      //Записать файлы в архив
      Ms := TsgeMemoryStream.Create;
      c := GetCount - 1;
      for i := 0 to c do
        begin
        Ms.Size := 0;

        //Прочитать данные в память
        case FFileList[i].FileType of
          pftFile:
            Ms.LoadFromFile(FFileList[i].FileName);

          pftPack:
            begin
            FPack := TsgePackFileReader.Create(FFileList[i].FileName);
            FPack.GetItemData(FFileList[i].Index, Ms);
            sgeFreeAndNil(FPack);
            end;
        end;


        //Записать блок 1 уровня
        BlockHead.NameSize := Length(FFileList[i].PackName);
        BlockHead.TotalSize := BlockHead.NameSize + SizeOf(TsgePackFileBlock) + FFileList[i].FileSize;
        F.Write(BlockHead, SizeOf(TsgePackFileBlock));

        //Записать файл
        F.Write(Ms.Data^, Ms.Size);

        //Записать имя файла
        F.Write(FFileList[i].PackName[1], BlockHead.NameSize);
        end;  //For

      except
        on E: EsgeException do
          raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, FileName, E.Message);
      end;


    finally
      F.Free;
      FPack.Free;
      Ms.Free;
    end;
end;



end.

