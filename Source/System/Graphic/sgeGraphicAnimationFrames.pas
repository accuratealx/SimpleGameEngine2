{
Пакет             Simple Game Engine 2
Файл              sgeGraphicAnimationFrames.pas
Версия            1.3
Создан            14.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс с набором плиток анимации
}
{$Include Defines.inc}

unit sgeGraphicAnimationFrames;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicSprite, sgeMemoryStream, sgeResourceList;


type
  //Инормация об одном кадре
  TsgeGraphicAnimationFrame = record
    Sprite: TsgeGraphicSprite;                                      //Указатель на спрайт
    SpriteName: String;                                             //Имя спрайта в таблице ресурсов
    Col: Word;                                                      //Номер столбца плитки
    Row: Word;                                                      //Номер строки плитки
    Time: Integer;                                                  //Время показа на экране в милисекундах
  end;


  //Массив кадров
  TsgeGraphicAnimationFrameArray = array of TsgeGraphicAnimationFrame;  //Массив кадров


  TsgeGraphicAnimationFrames = class
  private
    FResources: TsgeResourceList;                                   //Указатель на список ресурсов

    FFrames: TsgeGraphicAnimationFrameArray;                        //Список кадров
    FFileName: String;                                              //Имя файла

    function  GetCount: Integer;
    procedure SetFrame(Index: Integer; AFrame: TsgeGraphicAnimationFrame);
    function  GetFrame(Index: Integer): TsgeGraphicAnimationFrame;
    function  GetFrameAsString(AFrame: TsgeGraphicAnimationFrame): String;
  public
    constructor Create;
    constructor Create(AFrames: TsgeGraphicAnimationFrameArray);
    constructor Create(FileName: String; Resources: TsgeResourceList);
    constructor Create(Stream: TsgeMemoryStream; Resources: TsgeResourceList);
    destructor  Destroy; override;

    procedure Reload;

    procedure Clear;
    procedure Add(AFrame: TsgeGraphicAnimationFrame);
    procedure Add(Sprite: TsgeGraphicSprite; Col: Word = 0; Row: Word = 0; Time: Integer = 1000);
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; AFrame: TsgeGraphicAnimationFrame);

    procedure FromString(Str: String);
    function  ToString: String; override;
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure ToMemoryStream(Stream: TsgeMemoryStream);

    property FileName: String read FFileName write FFileName;
    property Count: Integer read GetCount;
    property Frame[Index: Integer]: TsgeGraphicAnimationFrame read GetFrame write SetFrame;
  end;


const
  DefaultAnimationFrame: TsgeGraphicAnimationFrame = (
    Sprite: nil;
    SpriteName: '';
    Col: 0;
    Row: 0;
    Time: 0
    );


implementation

uses
  sgeErrors, sgeStringList, sgeSimpleCommand, sgeFile, sgeSystemUtils;

const
  _UNITNAME = 'GraphicAnimationFrames';

  LineSeparator = #13#10;

  Err_IndexOutOfBounds        = 'IndexOutOfBounds';
  Err_ObjectIsEmpty           = 'ObjectIsEmpty';
  Err_NoPartsToLoad           = 'NoPartsToLoad';
  Err_NotEnoughParameters     = 'NotEnoughParameters';
  Err_SpriteNotFound          = 'SpriteNotFound';
  Err_UnableToDetermineColumn = 'UnableToDetermineColumn';
  Err_UnableToDetermineRow    = 'UnableToDetermineRow';
  Err_UnableToDetermineTime   = 'UnableToDetermineTime';
  Err_CantReadData            = 'CantReadData';
  Err_CantReadFile            = 'CantReadFile';
  Err_CantWriteFile           = 'CantWriteFile';
  Err_CantLoadFromStream      = 'CantLoadFromStream';


function TsgeGraphicAnimationFrames.GetCount: Integer;
begin
  Result := Length(FFrames);
end;


procedure TsgeGraphicAnimationFrames.SetFrame(Index: Integer; AFrame: TsgeGraphicAnimationFrame);
var
  c: Integer;
begin
  c := Length(FFrames);
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  FFrames[Index] := AFrame;
end;


function TsgeGraphicAnimationFrames.GetFrame(Index: Integer): TsgeGraphicAnimationFrame;
var
  c: Integer;
begin
  c := Length(FFrames);
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  Result := FFrames[Index];
end;


function TsgeGraphicAnimationFrames.GetFrameAsString(AFrame: TsgeGraphicAnimationFrame): String;
begin
  Result := AFrame.SpriteName + ' ' +
            sgeIntToStr(AFrame.Col) + ' ' +
            sgeIntToStr(AFrame.Row) + ' ' +
            sgeIntToStr(AFrame.Time);
end;


constructor TsgeGraphicAnimationFrames.Create;
begin

end;


constructor TsgeGraphicAnimationFrames.Create(AFrames: TsgeGraphicAnimationFrameArray);
var
  i, c: Integer;
begin
  Clear;
  c := Length(AFrames) - 1;
  SetLength(FFrames, c + 1);

  for i := 0 to c do
    FFrames[i] := AFrames[i];
end;


constructor TsgeGraphicAnimationFrames.Create(FileName: String; Resources: TsgeResourceList);
begin
  FResources := Resources;
  FFileName := FileName;

  LoadFromFile(FFileName);
end;


constructor TsgeGraphicAnimationFrames.Create(Stream: TsgeMemoryStream; Resources: TsgeResourceList);
begin
  FResources := Resources;
  FFileName := '';

  FromMemoryStream(Stream);
end;


destructor TsgeGraphicAnimationFrames.Destroy;
begin
  Clear;
end;


procedure TsgeGraphicAnimationFrames.Reload;
begin
  LoadFromFile(FFileName);
end;


procedure TsgeGraphicAnimationFrames.Clear;
begin
  SetLength(FFrames, 0);
end;


procedure TsgeGraphicAnimationFrames.Add(AFrame: TsgeGraphicAnimationFrame);
var
  c: Integer;
begin
  c := GetCount;
  SetLength(FFrames, c + 1);
  FFrames[c] := AFrame;
end;


procedure TsgeGraphicAnimationFrames.Add(Sprite: TsgeGraphicSprite; Col: Word; Row: Word; Time: Integer);
var
  F: TsgeGraphicAnimationFrame;
begin
  F.SpriteName := '';
  F.Sprite := Sprite;
  F.Col := Col;
  F.Row := Row;
  F.Time := Time;

  Add(F);
end;


procedure TsgeGraphicAnimationFrames.Delete(Index: Integer);
var
  i, c: Integer;
begin
  c := GetCount - 1;
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  for i := 0 to c - 1 do
    FFrames[i] := FFrames[i + 1];

  SetLength(FFrames, c);
end;


procedure TsgeGraphicAnimationFrames.Insert(Index: Integer; AFrame: TsgeGraphicAnimationFrame);
var
  i, c: Integer;
begin
  c := Length(FFrames);
  if (Index < 0) or (Index > c) then
    raise EsgeException.Create(_UNITNAME, Err_IndexOutOfBounds, sgeIntToStr(Index));

  SetLength(FFrames, c + 1);                                        //Добавить 1 кадр
  for i := c downto Index + 1 do                                    //Сместить кадры
    FFrames[i] := FFrames[i - 1];

  FFrames[Index] := AFrame;                                         //Вставить новый кадр
end;


procedure TsgeGraphicAnimationFrames.FromString(Str: String);
var
  i, c, Cnt: Integer;
  Fr: TsgeGraphicAnimationFrame;
  aCol, aRow: Integer;
  TempFrames: TsgeGraphicAnimationFrameArray;
  Lines: TsgeStringList;
  Frm: TsgeSimpleCommand;
begin
  //Проверить на существование объекта
  if FResources = nil then
    raise EsgeException.Create(_UNITNAME, Err_ObjectIsEmpty, 'Resources');

  try
    //Подготовить классы
    Frm := TsgeSimpleCommand.Create;
    Lines := TsgeStringList.Create;
    Lines.Separator := LineSeparator;
    Lines.FromString(sgeTrim(Str));
    Cnt := Lines.Count - 1;

    //Проверить на наличие одного кадра
    if Cnt < 0 then
      raise EsgeException.Create(_UNITNAME, Err_NoPartsToLoad);


    try
      SetLength(TempFrames, 0);

      for i := 0 to Cnt do
      begin
        //Почистить запись
        Fr.SpriteName := '';
        Fr.Sprite := nil;
        Fr.Col := 0;
        Fr.Row := 0;
        Fr.Time := 0;

        //Разобрать кадр на части
        Frm.Command := Lines.Part[i];

        //Проверить на наличие 4 частей
        if Frm.Count < 4 then
          raise EsgeException.Create(_UNITNAME, Err_NotEnoughParameters, Lines.Part[i]);

        //Определить имя кадра
        Fr.SpriteName := Frm.Part[0];
        Fr.Sprite := TsgeGraphicSprite(FResources.TypedObj[Frm.Part[0], rtSprite]);
        if Fr.Sprite = nil then
          raise EsgeException.Create(_UNITNAME, Err_SpriteNotFound, Frm.Part[0]);

        //Определить номер столбца
        if not sgeTryStrToInt(Frm.Part[1], aCol) then
          raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineColumn, Frm.Part[1]);
        Fr.Col := aCol;

        //Определить номер строки
        if not sgeTryStrToInt(Frm.Part[2], aRow) then
          raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineRow, Frm.Part[2]);
        Fr.Row := aRow;

        //Определить время видимости в строке милисекунды
        if not sgeTryStrToInt(Frm.Part[3], Fr.Time) then
          raise EsgeException.Create(_UNITNAME, Err_UnableToDetermineTime, Frm.Part[3]);
        if Fr.Time < 1 then Fr.Time := 0;

        //Добавить во временный массив
        c := Length(TempFrames);
        SetLength(TempFrames, c + 1);
        TempFrames[c] := Fr;
      end;


    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadData, Lines.Part[i], E.Message);
    end;


    //Скопировать данные
    c := Length(TempFrames) - 1;
    SetLength(FFrames, c + 1);
    for i := 0 to c do
      FFrames[i] := TempFrames[i];


  finally
    SetLength(TempFrames, 0);
    Lines.Free;
    Frm.Free;
  end;
end;


function TsgeGraphicAnimationFrames.ToString: String;
var
  i, c: Integer;
begin
  c := Length(FFrames) - 1;
  Result := '';
  for i := 0 to c do
  begin
    Result := Result + GetFrameAsString(FFrames[i]);
    if i <> c then
      Result := Result + LineSeparator;
  end;
end;


procedure TsgeGraphicAnimationFrames.LoadFromFile(FileName: String);
var
  F: TsgeFile;
begin
  try
    try
      F := TsgeFile.Create(FileName, fmRead, False);
      FromString(F.ToString);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
    end;
  finally
    F.Free;
  end;
end;


procedure TsgeGraphicAnimationFrames.SaveToFile(FileName: String);
var
  s: String;
  F: TsgeFile;
begin
  try
    try
      F := TsgeFile.Create(FileName, fmWrite, True);
      s := ToString;
      F.Size := 0;
      F.Write(s[1], Length(s));
    except
      raise EsgeException.Create(_UNITNAME, Err_CantWriteFile, FileName);
    end;
  finally
    F.Free;
  end;
end;


procedure TsgeGraphicAnimationFrames.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  try
    FromString(Stream.ToString);
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
  end;
end;


procedure TsgeGraphicAnimationFrames.ToMemoryStream(Stream: TsgeMemoryStream);
begin
  Stream.FromString(ToString);
end;





end.


