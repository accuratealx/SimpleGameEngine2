{
Пакет             Simple Game Engine 2
Файл              sgeSprite.pas
Версия            1.0
Создан            05.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Спрайт в памяти
}
{$Include Defines.inc}

unit sgeSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream;


type
  TsgeSprite = class
  private
    FData: Pointer;
    FSize: Int64;
    FWidth: Integer;                                                //Ширина спрайта в пикселях
    FHeight: Integer;                                               //Высота спрайта в пикселях

    FFileName: String;                                              //Имя файла

    FTileCols: Word;                                                //Плиток по X
    FTileRows: Word;                                                //Плиток по Y
    FTileWidth: Word;                                               //Ширина одной плитки в пикселях
    FTileHeight: Word;                                              //Высота одной плитки в пикселях

    procedure Data_ChangeSize(Width, Height: Integer);              //Выделить память
    procedure Data_Free;                                            //Освободить память

    procedure CalcTiles;                                            //Пересчитать ширину и высоту плиток

    procedure SetTileCols(ACols: Word);
    procedure SetTileRows(ARows: Word);
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);
  public
    constructor Create(FileName: String; TileCols: Word = 1; TileRows: Word = 1);
    constructor Create(Stream: TsgeMemoryStream; TileCols: Word = 1; TileRows: Word = 1);
    destructor  Destroy; override;

    procedure LoadFromFile(FileName: String);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);



    property Data: Pointer read FData;
    property Size: Int64 read FSize;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;

    property FileName: String read FFileName write FFileName;

    property TileCols: Word read FTileCols write SetTileCols;
    property TileRows: Word read FTileRows write SetTileRows;
    property TileWidth: Word read FTileWidth;
    property TileHeight: Word read FTileHeight;
  end;

implementation

uses
  sgeErrors, sgeSpriteLoaderGDIP;

const
  _UNITNAME = 'Sprite';

  Err_CantReadFile        = 'CantReadFile';
  //Err_CantWriteFile       = 'CantWriteFile';
  Err_CantLoadFromStream  = 'CantLoadFromStream';


procedure TsgeSprite.Data_ChangeSize(Width, Height: Integer);
begin
  //Освободить память
  Data_Free;

  //Новый размер блока
  FSize := Width * Height * 4;

  //Выделить память
  FData := AllocMem(FSize);
end;


procedure TsgeSprite.Data_Free;
begin
  //Освободить память
  Freemem(FData, FSize);

  //Обнулить переменные
  FData := nil;
  FSize := 0;
end;


procedure TsgeSprite.CalcTiles;
begin
  //Размеры одной плитки
  FTileWidth := FWidth div FTileCols;
  FTileHeight := FHeight div FTileRows;
end;


procedure TsgeSprite.SetTileCols(ACols: Word);
begin
  if ACols < 1 then
    ACols := 1;

  FTileCols := ACols;

  CalcTiles;
end;


procedure TsgeSprite.SetTileRows(ARows: Word);
begin
  if ARows < 1 then
    ARows := 1;

  FTileRows := ARows;

  CalcTiles;
end;


procedure TsgeSprite.SetWidth(AWidth: Integer);
begin
  if AWidth < 0 then
    AWidth := 0;

  //Запомнить новую ширину
  FWidth := AWidth;

  //Изменить размер памяти
  Data_ChangeSize(FWidth, FHeight);

  //Пересчитать размеры плиток
  CalcTiles;
end;


procedure TsgeSprite.SetHeight(AHeight: Integer);
begin
  if AHeight < 0 then
    AHeight := 0;

  //Запомнить новую высоту
  FHeight := AHeight;

  //Изменить размер памяти
  Data_ChangeSize(FWidth, FHeight);

  //Пересчитать размеры плиток
  CalcTiles;
end;


constructor TsgeSprite.Create(FileName: String; TileCols: Word; TileRows: Word);
begin
  //Обработать информацию о плитках
  if TileCols < 1 then
    TileCols := 1;
  if TileRows < 1 then
    TileRows := 1;

  //Сохранить размерность плиток
  FTileCols := TileCols;
  FTileRows := TileRows;

  //Попробовать загрузить данные из файла
  LoadFromFile(FileName);

  //Запомнить имя файла
  FFileName := FileName;
end;


constructor TsgeSprite.Create(Stream: TsgeMemoryStream; TileCols: Word; TileRows: Word);
begin
  //Обработать информацию о плитках
  if TileCols < 1 then
    TileCols := 1;
  if TileRows < 1 then
    TileRows := 1;

  //Сохранить размерность плиток
  FTileCols := TileCols;
  FTileRows := TileRows;

  //Загрузить из памяти
  FromMemoryStream(Stream);
end;


destructor TsgeSprite.Destroy;
begin
  Data_Free;
end;


procedure TsgeSprite.LoadFromFile(FileName: String);
var
  Ms: TsgeMemoryStream;
begin
  try
    Ms := TsgeMemoryStream.Create;

    try
      //Загрузить из файла
      Ms.LoadFromFile(FileName);

      //Загрузить из потока
      FromMemoryStream(Ms);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
    end;

  finally
    Ms.Free;
  end;
end;


procedure TsgeSprite.FromMemoryStream(Stream: TsgeMemoryStream);
var
  Loader: TsgeSpriteLoaderGDIP;
begin
  Loader := nil;
  try
    try
      //Грузим данные
      Loader := TsgeSpriteLoaderGDIP.Create(Stream);

      //Запомнить размеры
      FWidth := Loader.Width;
      FHeight := Loader.Height;

      //Выделить память под данные
      Data_ChangeSize(FWidth, FHeight);

      //Скопировать данные
      Move(Loader.Data, FData, FSize);

      //Пересчитать размеры
      CalcTiles;

    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
    end;
  finally
    Loader.Free;
  end;
end;



end.

