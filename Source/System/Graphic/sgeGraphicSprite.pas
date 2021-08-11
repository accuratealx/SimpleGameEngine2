{
Пакет             Simple Game Engine 2
Файл              sgeGraphicSprite.pas
Версия            1.2
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс спрайт OpenGL
}
{$Include Defines.inc}

unit sgeGraphicSprite;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicColor, sgeMemoryStream,
  dglOpenGL;


type
  //Фильтрация текстуры (Без сглаживания, Интерполяция)
  TsgeGraphicSpriteFilter = (gsfNearest, gsfLinear);


  //Режим намотки текстуры
  TsgeGraphicSpriteWrapMode = (gswmClampToEdge, gswmRepeat, gswmRepeatMirror);


  TsgeGraphicSprite = class
  private
    FGLHandle: GLuint;                  //Номер OpenGL
    FGLTileWidth: Single;               //Ширина одной плитки в координатах OpenGL
    FGLTileHeight: Single;              //Высота одной плитки в координатах OpenGL
    FGLPixelWidth: Single;              //Ширина одного пикселя в координатах OpenGL
    FGLPixelHeight: Single;             //Высота одного пикселя в координатах OpenGL
    FFileName: String;                  //Имя файла
    FWidth: Integer;                    //Ширина спрайта в пикселях
    FHeight: Integer;                   //Высота спрайта в пикселях
    FTileCols: Word;                    //Плиток по X
    FTileRows: Word;                    //Плиток по Y
    FTileWidth: Word;                   //Ширина одной плитки в пикселях
    FTileHeight: Word;                  //Высота одной плитки в пикселях

    procedure SetMagFilter(AFilter: TsgeGraphicSpriteFilter);
    function  GetMagFilter: TsgeGraphicSpriteFilter;
    procedure SetMinFilter(AFilter: TsgeGraphicSpriteFilter);
    function  GetMinFilter: TsgeGraphicSpriteFilter;
    procedure SetWrapModeX(AMode: TsgeGraphicSpriteWrapMode);
    function  GetWrapModeX: TsgeGraphicSpriteWrapMode;
    procedure SetWrapModeY(AMode: TsgeGraphicSpriteWrapMode);
    function  GetWrapModeY: TsgeGraphicSpriteWrapMode;
    procedure SetTileCols(ACols: Word);
    procedure SetTileRows(ARows: Word);
    procedure CalcTiles;
    procedure SetWidth(AWidth: Integer);
    procedure SetHeight(AHeight: Integer);

    procedure ChangeTexture(AWidth, AHeight: Integer; Data: Pointer);

    procedure PreCreate;
  public
    constructor Create(FileName: String; TileCols: Word = 1; TileRows: Word = 1; MagFilter: TsgeGraphicSpriteFilter = gsfNearest; MinFilter: TsgeGraphicSpriteFilter = gsfNearest);
    constructor Create(Width, Height: Integer; BGColor: TsgeColor);
    constructor Create(Stream: TsgeMemoryStream; TileCols: Word = 1; TileRows: Word = 1; MagFilter: TsgeGraphicSpriteFilter = gsfNearest; MinFilter: TsgeGraphicSpriteFilter = gsfNearest);
    constructor Create(Width: Integer = 32; Height: Integer = 32);
    destructor  Destroy; override;

    procedure SetSize(AWidth, AHeight: Integer);
    procedure FillColor(Color: TsgeColor);
    procedure FillChessBoard(CellSize: Integer);
    procedure LoadFromFile(FileName: String);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);

    procedure SaveToStream(Stream: TsgeMemoryStream);
    procedure SaveToFile(FileName: String);

    procedure Reload;

    property GLHandle: GLuint read FGLHandle;
    property GLTileWidth: Single read FGLTileWidth;
    property GLTileHeight: Single read FGLTileHeight;
    property GLPixelWidth: Single read FGLPixelWidth;
    property GLPixelHeight: Single read FGLPixelHeight;
    property FileName: String read FFileName write FFileName;
    property MagFilter: TsgeGraphicSpriteFilter read GetMagFilter write SetMagFilter;
    property MinFilter: TsgeGraphicSpriteFilter read GetMagFilter write SetMagFilter;
    property WrapModeX: TsgeGraphicSpriteWrapMode read GetWrapModeX write SetWrapModeX;
    property WrapModeY: TsgeGraphicSpriteWrapMode read GetWrapModeY write SetWrapModeY;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property TileCols: Word read FTileCols write SetTileCols;
    property TileRows: Word read FTileRows write SetTileRows;
    property TileWidth: Word read FTileWidth;
    property TileHeight: Word read FTileHeight;
  end;



implementation

uses
  sgeErrors,
  sgeGraphicSpriteGDIPLoader,
  Windows;


const
  _UNITNAME = 'GraphicSprite';

  Err_CantReadFile        = 'CantReadFile';
  Err_CantWriteFile       = 'CantWriteFile';
  Err_CantLoadFromStream  = 'CantLoadFromStream';



procedure TsgeGraphicSprite.SetMagFilter(AFilter: TsgeGraphicSpriteFilter);
var
  Filter: GLint;
begin
  case AFilter of
    gsfNearest: Filter := GL_NEAREST;
    gsfLinear : Filter := GL_LINEAR;
  end;

  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, Filter);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


function TsgeGraphicSprite.GetMagFilter: TsgeGraphicSpriteFilter;
var
  Filter: GLint;
begin
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, @Filter);
  glBindTexture(GL_TEXTURE_2D, 0);

  case Filter of
    GL_NEAREST: Result := gsfNearest;
    GL_LINEAR : Result := gsfLinear;
  end;
end;


procedure TsgeGraphicSprite.SetMinFilter(AFilter: TsgeGraphicSpriteFilter);
var
  Filter: GLint;
begin
  case AFilter of
    gsfNearest: Filter := GL_NEAREST;
    gsfLinear : Filter := GL_LINEAR;
  end;

  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, Filter);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


function TsgeGraphicSprite.GetMinFilter: TsgeGraphicSpriteFilter;
var
  Filter: GLint;
begin
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, @Filter);
  glBindTexture(GL_TEXTURE_2D, 0);

  case Filter of
    GL_NEAREST: Result := gsfNearest;
    GL_LINEAR : Result := gsfLinear;
  end;
end;


procedure TsgeGraphicSprite.SetWrapModeX(AMode: TsgeGraphicSpriteWrapMode);
var
  Mode: GLint;
begin
  case AMode of
    gswmClampToEdge : Mode := GL_CLAMP_TO_EDGE;
    gswmRepeat      : Mode := GL_REPEAT;
    gswmRepeatMirror: Mode := GL_MIRRORED_REPEAT;
  end;

  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Mode);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


function TsgeGraphicSprite.GetWrapModeX: TsgeGraphicSpriteWrapMode;
var
  Mode: GLint;
begin
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, @Mode);
  glBindTexture(GL_TEXTURE_2D, 0);

  case Mode of
    GL_CLAMP_TO_EDGE   : Result := gswmClampToEdge;
    GL_REPEAT          : Result := gswmRepeat;
    GL_MIRRORED_REPEAT : Result := gswmRepeatMirror;
  end;
end;


procedure TsgeGraphicSprite.SetWrapModeY(AMode: TsgeGraphicSpriteWrapMode);
var
  Mode: GLint;
begin
  case AMode of
    gswmClampToEdge : Mode := GL_CLAMP_TO_EDGE;
    gswmRepeat      : Mode := GL_REPEAT;
    gswmRepeatMirror: Mode := GL_MIRRORED_REPEAT;
  end;

  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexParameterI(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Mode);
  glBindTexture(GL_TEXTURE_2D, 0);
end;


function TsgeGraphicSprite.GetWrapModeY: TsgeGraphicSpriteWrapMode;
var
  Mode: GLint;
begin
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, @Mode);
  glBindTexture(GL_TEXTURE_2D, 0);

  case Mode of
    GL_CLAMP_TO_EDGE   : Result := gswmClampToEdge;
    GL_REPEAT          : Result := gswmRepeat;
    GL_MIRRORED_REPEAT : Result := gswmRepeatMirror;
  end;
end;


procedure TsgeGraphicSprite.SetTileCols(ACols: Word);
begin
  if ACols < 1 then ACols := 1;
  FTileCols := ACols;
  CalcTiles;
end;


procedure TsgeGraphicSprite.SetTileRows(ARows: Word);
begin
  if ARows < 1 then ARows := 1;
  FTileRows := ARows;
  CalcTiles;
end;


procedure TsgeGraphicSprite.CalcTiles;
begin
  //Размеры плитки в координатах OpenGL
  FGLTileWidth := 1 / FTileCols;
  FGLTileHeight := 1 / FTileRows;

  //Размеры одной плитки
  FTileWidth := FWidth div FTileCols;
  FTileHeight := FHeight div FTileRows;

  //Размеры одного пикселя в координатах OpenGL
  FGLPixelWidth := 1 / FWidth;
  FGLPixelHeight := 1 / FHeight;
end;


procedure TsgeGraphicSprite.SetWidth(AWidth: Integer);
begin
  if AWidth < 0 then AWidth := 0;
  FWidth := AWidth;
  CalcTiles;
  ChangeTexture(FWidth, FHeight, nil);
end;


procedure TsgeGraphicSprite.SetHeight(AHeight: Integer);
begin
  if AHeight < 0 then AHeight := 0;
  FHeight := AHeight;
  CalcTiles;
  ChangeTexture(FWidth, FHeight, nil);
end;


procedure TsgeGraphicSprite.ChangeTexture(AWidth, AHeight: Integer; Data: Pointer);
begin
  //Залить данные в OpenGL
  glBindTexture(GL_TEXTURE_2D, FGLHandle);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, AWidth, AHeight, 0, GL_BGRA, GL_UNSIGNED_BYTE, Data);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TsgeGraphicSprite.PreCreate;
begin
  glGenTextures(1, @FGLHandle);                                           //Выделить память для текстуры
  glBindTexture(GL_TEXTURE_2D, FGLHandle);                                //Сделать текстуру активной
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);    //Привязывать края к границе полигона X
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);    //Привязывать края к границе полигона Y
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);      //Интерполяция при увеличении по соседям
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);      //Интерполяция при уменьшении по соседям
  glBindTexture(GL_TEXTURE_2D, 0);                                        //Отменить выбор текстуры
end;


constructor TsgeGraphicSprite.Create(FileName: String; TileCols: Word; TileRows: Word; MagFilter: TsgeGraphicSpriteFilter; MinFilter: TsgeGraphicSpriteFilter);
begin
  PreCreate;

  //Обработать информацию о плитках
  if TileCols < 1 then TileCols := 1;                                     //Поправить количество плиток
  if TileRows < 1 then TileRows := 1;
  FTileCols := TileCols;                                                  //Запомнить количество плиток
  FTileRows := TileRows;

  //Попробовать загрузить данные из файла
  try
    LoadFromFile(FileName);
  except
    on E: EsgeException do
      begin
      glDeleteTextures(1, @FGLHandle);
      raise EsgeException.Create(E.Message);
      end;
  end;

  //Запомнить имя файла
  FFileName := FileName;

  //Костыль
  glFinish;
end;


constructor TsgeGraphicSprite.Create(Width, Height: Integer; BGColor: TsgeColor);
begin
  PreCreate;

  //Размеры
  if Width < 0 then Width := 0;
  if Height < 0 then Height := 0;
  FWidth := Width;
  FHeight := Height;

  //Обработать информацию о плитках
  FTileCols := 1;
  FTileRows := 1;

  //Пересчитать переменные
  CalcTiles;

  //Залить цветом
  FillColor(BGColor);

  //Костыль
  glFinish;
end;


constructor TsgeGraphicSprite.Create(Stream: TsgeMemoryStream; TileCols: Word; TileRows: Word; MagFilter: TsgeGraphicSpriteFilter; MinFilter: TsgeGraphicSpriteFilter);
begin
  PreCreate;

  //Обработать информацию о плитках
  if TileCols < 1 then TileCols := 1;
  if TileRows < 1 then TileRows := 1;
  FTileCols := TileCols;
  FTileRows := TileRows;

  //Загрузить из памяти
  FromMemoryStream(Stream);

  //Костыль
  glFinish;
end;


constructor TsgeGraphicSprite.Create(Width: Integer; Height: Integer);
begin
  PreCreate;

  //Размеры
  if Width < 0 then Width := 0;
  if Height < 0 then Height := 0;
  FWidth := Width;
  FHeight := Height;

  //Обработать информацию о плитках
  FTileCols := 1;
  FTileRows := 1;

  //Пересчитать переменные
  CalcTiles;

  //Залить цветом
  FillChessBoard(8);

  //Костыль
  glFinish;
end;


destructor TsgeGraphicSprite.Destroy;
begin
  if FGLHandle = 0 then Exit;
  glDeleteTextures(1, @FGLHandle);
end;


procedure TsgeGraphicSprite.SetSize(AWidth, AHeight: Integer);
begin
  if AWidth < 1 then AWidth := 1;
  FWidth := AWidth;

  if AHeight < 1 then AHeight := 1;
  FHeight := AHeight;

  ChangeTexture(FWidth, FHeight, nil);
end;


procedure TsgeGraphicSprite.FillColor(Color: TsgeColor);
var
  Data: array of Byte;
  Size, i, c, Idx: Integer;
  cl: TsgeRGBA;
begin
  //Преобразовать цвет
  cl := sgeColorToRGBA(Color);

  //Определить размер
  Size := FWidth * FHeight * 4;

  //Выделить память под буфер
  SetLength(Data, Size);

  //Подготовить массив точек с прозрачностью
  c := (Size div 4) - 1;                      //Номер последнего пиксела
  for i := 0 to c do
    begin
    Idx := i * 4;                             //Индекс начала RGBQuad
    Data[Idx + 0] := cl.Blue;                 //Blue
    Data[Idx + 1] := cl.Green;                //Green
    Data[Idx + 2] := cl.Red;                  //Red
    Data[Idx + 3] := cl.Alpha;                //Alpha
    end;

  //Залить в OpenGL
  ChangeTexture(FWidth, FHeight, @Data[0]);

  //Почистить память
  SetLength(Data, 0);
end;


procedure TsgeGraphicSprite.FillChessBoard(CellSize: Integer);
var
  Data: array of Byte;
  Size, X, Y, i, c, Idx: Integer;
  a: Byte;
begin
  //Поправить размер клетки
  if CellSize < 1 then CellSize := 1;

  //Определить размер
  Size := FWidth * FHeight * 4;

  //Выделить память под буфер
  SetLength(Data, Size);

  //Подготовить массив точек с прозрачностью
  c := (Size div 4) - 1;                      //Номер последнего пикселя
  for i := 0 to c do
    begin
    X := i div FWidth div CellSize;           //Номер столбца с учётом ширины клетки
    Y := (i mod FWidth) div CellSize;         //Номер строки с учётом ширины клетки

    if odd(X + Y) then a := 255 else a := 0;  //Определить цвет для долей
    Idx := i * 4;                             //Индекс начала RGBQuad

    Data[Idx + 0] := a;                       //Blue
    Data[Idx + 1] := a;                       //Green
    Data[Idx + 2] := a;                       //Red
    Data[Idx + 3] := 255;                     //Alpha
    end;

  //Залить в OpenGL
  ChangeTexture(FWidth, FHeight, @Data[0]);

  //Почистить память
  SetLength(Data, 0);
end;


procedure TsgeGraphicSprite.LoadFromFile(FileName: String);
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


procedure TsgeGraphicSprite.FromMemoryStream(Stream: TsgeMemoryStream);
var
  Loader: TsgeGraphicSpriteGDIPLoader;
begin
  try
    try
      //Грузим данные
      Loader := TsgeGraphicSpriteGDIPLoader.Create(Stream);

      //Залить данные в OpenGL
      ChangeTexture(Loader.Width, Loader.Height, Loader.Data);

      //Запомнить размеры
      FWidth := Loader.Width;
      FHeight := Loader.Height;

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


procedure TsgeGraphicSprite.SaveToStream(Stream: TsgeMemoryStream);
var
  BFH: TBitmapFileHeader;
  BIH: TBITMAPINFOHEADER;
  BytesPerLine, Trash, Size, szFileHeader, szInfoHeader: Integer;
  DATA: array of Byte;
begin
  //Байтов в одной строке
  BytesPerLine := FWidth * 3;

  //Определить количество байт для выравнивания
  Trash := 4 mod (BytesPerLine mod 4);

  //Определить размер данных с мусором
  Size := (BytesPerLine + Trash) * FHeight;

  //Чтение данных из OpenGL
  SetLength(DATA, Size);                                                //Буфер для OpenGL
  glBindTexture(GL_TEXTURE_2D, FGLHandle);                              //Привязать текстуру
  glGetTexImage(GL_TEXTURE_2D, 0, GL_BGR, GL_UNSIGNED_BYTE, @DATA[0]);  //Скопировать цвета в буфер
  glBindTexture(GL_TEXTURE_2D, 0);                                      //Отвязать текстуру

  //Определить размеры структур
  szFileHeader := SizeOf(TBitmapFileHeader);
  szInfoHeader := SizeOf(TBITMAPINFOHEADER);

  //Описатель BMP файла
  ZeroMemory(@BFH, szFileHeader);
  BFH.bfType := $4D42;                                      //Волшебное слово от микрософта - BM
  BFH.bfReserved1 := 0;
  BFH.bfReserved2 := 0;
  BFH.bfOffBits := szFileHeader + szInfoHeader;             //Смещение от начала файла до самих данных
  BFH.bfSize := BFH.bfOffBits + Size;                       //Размер файла целиком со структурами и мусором

  //Описатель BMP
  ZeroMemory(@BIH, szInfoHeader);
  BIH.biSize := szInfoHeader;                               //Размер этой структуры. Интересно зачем
  BIH.biWidth := FWidth;                                    //Ширина битмапа
  BIH.biHeight := FHeight;                                  //Высота битмапа
  BIH.biPlanes := 1;                                        //Сколько слоёв
  BIH.biBitCount := 24;                                     //Бит на пиксель
  BIH.biCompression := BI_RGB;                              //Без сжатия
  BIH.biSizeImage := 0;                                     //Не используется без сжатия
  BIH.biXPelsPerMeter := 0;                                 //Разрешение по X
  BIH.biYPelsPerMeter := 0;                                 //Разрешение по Y
  BIH.biClrUsed := 0;                                       //Сколько цветов в таблице индексов
  BIH.biClrImportant := 0;                                  //0 - все индексы цветов доступны

  //Записать в поток
  Stream.Size := 0;                                         //Обнулить память
  Stream.Write(BFH, 0, SizeOf(BFH));                        //Заголовок файла
  Stream.Write(BIH, szFileHeader, SizeOf(BIH));             //Описание битмапа
  Stream.Write(DATA[0], szFileHeader + szInfoHeader, Size); //Записать данные

  //Очистить буфер
  SetLength(DATA, 0);
end;

procedure TsgeGraphicSprite.SaveToFile(FileName: String);
var
  ms: TsgeMemoryStream;
begin
  try
    ms := TsgeMemoryStream.Create;

    //Взять данные
    SaveToStream(ms);

    //Записать в файл
    try
      ms.SaveToFile(FileName);
    except
      on E:EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, '', E.Message);
    end;

  finally
    ms.Free;
  end;
end;


procedure TsgeGraphicSprite.Reload;
begin
  LoadFromFile(FFileName);
end;



end.


