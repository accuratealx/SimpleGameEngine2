{
Пакет             Simple Game Engine 2
Файл              sgeSprite.pas
Версия            1.1
Создан            05.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Спрайт в памяти
}
{$Include Defines.inc}

unit sgeSprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream,
  sgeGraphicColor;


type
  //Фильтрация текстуры при увеличении
  TsgeSpriteMagFilter = (
    smagfNearest,               //По соседним (По умолчанию)
    smagfLinear                 //Линейная
  );


  //Фильтрация текстуры при уменьшении
  TsgeSpriteMinFilter = (
    sminfNearest,               //По соседним
    sminfLinear,                //Линейная
    sminfNearestMipmapNearest,
    sminfLinearMipmapNearest,
    sminfNearestMipmapLinear,   //(По умолчанию)
    sminfLinearMipmapLinear
  );


  //Заворачивание текстуры
  TsgeSpriteWrapMode = (
     swmRepeat,                 //Повтор
     swmMirroredRepeat,         //Повтор через отражение
     swmClampToEdge,            //Координаты от 0..1, за пределами цвет границы текстуры (По умолчанию)
     swmClampToBorder,          //Координаты от 0..1, за пределами заранее определенный цвет
     swmMirrorClampToEdge       //Отражается один раз, при выходе за пределы цвет границы текстуры
  );


  //Класс спрайта
  TsgeSprite = class
  private
    FData: Pointer;
    FSize: Int64;
    FWidth: Integer;                                                //Ширина спрайта в пикселях
    FHeight: Integer;                                               //Высота спрайта в пикселях
    FMagFilter: TsgeSpriteMagFilter;                                //Фильтрация увеличения
    FMinFilter: TsgeSpriteMinFilter;                                //Фильтрация уменьшения
    FWrapModeHorizontal: TsgeSpriteWrapMode;                        //Горизонтальное заворачивание
    FWrapModeVertical: TsgeSpriteWrapMode;                          //Вертикальное заворачивание
    FWrapModeColor: TsgeColor;                                      //Цвет границы заворачивания текстуры
    FTileCols: Word;                                                //Плиток по X
    FTileRows: Word;                                                //Плиток по Y
    FTileWidth: Word;                                               //Ширина одной плитки в пикселях
    FTileHeight: Word;                                              //Высота одной плитки в пикселях

    procedure Data_ChangeSize(Width, Height: Integer);              //Выделить память
    procedure Data_Free;                                            //Освободить память

    procedure CalcTiles;                                            //Пересчитать ширину и высоту плиток
    procedure SetTiles(ATileCols, ATileRows: Word);

    procedure SetTileCols(ACols: Word);
    procedure SetTileRows(ARows: Word);
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);

    procedure SetDefaultParameters;
  public
    constructor Create(Stream: TsgeMemoryStream; TileCols: Word = 1; TileRows: Word = 1);
    constructor Create(Width: Integer = 0; Height: Integer = 0; TileCols: Word = 1; TileRows: Word = 1);
    destructor  Destroy; override;

    procedure FillColor(Color: TsgeColor);
    procedure FillChessBoard(CellSize: Integer);

    procedure FromMemoryStream(Stream: TsgeMemoryStream);

    property Data: Pointer read FData;
    property Size: Int64 read FSize;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property MagFilter: TsgeSpriteMagFilter read FMagFilter write FMagFilter;
    property MinFilter: TsgeSpriteMinFilter read FMinFilter write FMinFilter;
    property WrapModeHorizontal: TsgeSpriteWrapMode read FWrapModeHorizontal write FWrapModeHorizontal;
    property WrapModeVertical: TsgeSpriteWrapMode read FWrapModeVertical write FWrapModeVertical;
    property WrapModeColor: TsgeColor read FWrapModeColor write FWrapModeColor;
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


procedure TsgeSprite.SetTiles(ATileCols, ATileRows: Word);
begin
  //Обработать информацию о плитках
  if ATileCols < 1 then
    ATileCols := 1;
  if ATileRows < 1 then
    ATileRows := 1;

  //Сохранить размерность плиток
  FTileCols := ATileCols;
  FTileRows := ATileRows;
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


procedure TsgeSprite.SetDefaultParameters;
begin
  //Фильтрация
  FMagFilter := smagfNearest;
  FMinFilter := sminfNearestMipmapLinear;

  //Заворачивание текстуры
  FWrapModeHorizontal := swmClampToEdge;
  FWrapModeVertical := swmClampToEdge;
  FWrapModeColor := cBlack;
end;


constructor TsgeSprite.Create(Stream: TsgeMemoryStream; TileCols: Word; TileRows: Word);
begin
  //Установить параметры по умолчанию
  SetDefaultParameters;

  //Обработать информацию о плитках
  SetTiles(TileCols, TileRows);

  //Загрузить из памяти
  FromMemoryStream(Stream);
end;


constructor TsgeSprite.Create(Width: Integer; Height: Integer; TileCols: Word; TileRows: Word);
begin
  //Установить параметры по умолчанию
  SetDefaultParameters;

  //Обработать информацию о плитках
  SetTiles(TileCols, TileRows);

  //Проверить размеры
  if Width < 0 then
    Width := 0;
  if Height < 0 then
    Height := 0;

  //Запомнить размеры
  FWidth := Width;
  FHeight := Height;

  //Выделить память;
  Data_ChangeSize(FWidth, FHeight);

  //Пересчитать размеры плиток
  CalcTiles;
end;


destructor TsgeSprite.Destroy;
begin
  Data_Free;
end;


procedure TsgeSprite.FillColor(Color: TsgeColor);
var
  i, c: Integer;
  cl: TsgeRGBA;
  Quad: ^TsgeRGBA;
begin
  //Преобразовать цвет
  cl := sgeColorToRGBA(Color);

  //Подготовить массив точек с прозрачностью
  c := (FSize div 4) - 1;
  for i := 0 to c do
  begin
    //Адрес RGBQuad
    Quad := Pointer(FData + i * 4);

    //Заполнить цвет
    Quad^.Red := cl.Red;
    Quad^.Green := cl.Green;
    Quad^.Blue := cl.Blue;
    Quad^.Alpha := cl.Alpha;
  end;
end;


procedure TsgeSprite.FillChessBoard(CellSize: Integer);
var
  X, Y, i, c: Integer;
  a: Byte;
  Quad: ^TsgeRGBA;
begin
  //Поправить размер клетки
  if CellSize < 1 then
    CellSize := 1;

  //Подготовить массив точек с прозрачностью
  c := (FSize div 4) - 1;
  for i := 0 to c do
  begin
    X := i div FWidth div CellSize;                                 //Номер столбца с учётом ширины клетки
    Y := (i mod FWidth) div CellSize;                               //Номер строки с учётом ширины клетки

    //Адрес RGBQuad
    Quad := Pointer(FData + i * 4);

    //Определить цвет для долей
    if Odd(X + Y) then
      a := 255
    else
      a := 0;

    //Заполнить цвет
    Quad^.Red := a;
    Quad^.Green := a;
    Quad^.Blue := a;
    Quad^.Alpha := 255;
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
      Move(Loader.Data^, FData^, FSize);

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

