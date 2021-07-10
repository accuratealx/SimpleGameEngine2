{
Пакет             Simple Game Engine 2
Файл              sgeSoundBuffer.pas
Версия            1.0
Создан            18.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Набор аудиоданных
}
{$Include Defines.inc}

unit sgeSoundBuffer;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream,
  OpenAL;


type
  //Битность данных (0, 8, 16)
  TsgeSoundBufferDept = (sbd0Bit, sbd8Bit, sbd16Bit);

  //Количество каналов (0, 1, 2)
  TsgeSoundBufferChannes = (sbcZero, sbcMono, sbcStereo);


  TsgeSoundBuffer = class
  private
    FHandle: TALuint;
    FFileName: String;

    function GetDept: TsgeSoundBufferDept;
    function GetChannels: TsgeSoundBufferChannes;
    function GetFrequency: Cardinal;
    function GetSize: Cardinal;

    function GetSamples: Single;
    function GetTime: Single;

    procedure PreCreate;
  public
    constructor Create(FileName: String);
    constructor Create(Stream: TsgeMemoryStream);
    constructor CreateBlank;
    destructor  Destroy; override;

    procedure FromMemoryStream(Stream: TsgeMemoryStream);
    procedure LoadFromFile(FileName: String);
    procedure Reload;

    property FileName: String read FFileName write FFileName;
    property Handle: Cardinal read FHandle;
    property Dept: TsgeSoundBufferDept read GetDept;
    property Channels: TsgeSoundBufferChannes read GetChannels;
    property Frequency: Cardinal read GetFrequency;
    property Size: Cardinal read GetSize;
    property Samples: Single read GetSamples;
    property Time: Single read GetTime;
  end;



implementation

uses
  sgeErrors, sgeSoundBufferWavLoader;


const
  _UNITNAME = 'SoundBuffer';

  Err_SoundNotInitialized = 'SoundNotInitialized';
  Err_ReachedBufferLimit  = 'ReachedBufferLimit';
  Err_OutOfMemory         = 'OutOfMemory';
  Err_WrongDataFormat     = 'WrongDataFormat';
  Err_UnsupportedFormat   = 'UnsupportedFormat';
  Err_CantLoadFromStream  = 'CantLoadFromStream';
  Err_CantReadFile        = 'CantReadFile';



function TsgeSoundBuffer.GetDept: TsgeSoundBufferDept;
var
  i: TALint;
begin
  alGetBufferi(FHandle, AL_BITS, @i); //Запросить глубину

  case i of
    8 : Result := sbd8Bit;
    16: Result := sbd16Bit;
    else Result := sbd0Bit;
  end;
end;


function TsgeSoundBuffer.GetChannels: TsgeSoundBufferChannes;
var
  i: TALint;
begin
  alGetBufferi(FHandle, AL_CHANNELS, @i); //Запросить каналы

  case i of
    1: Result := sbcMono;
    2: Result := sbcStereo;
    else Result := sbcZero;
  end;
end;


function TsgeSoundBuffer.GetFrequency: Cardinal;
var
  i: TALint;
begin
  alGetBufferi(FHandle, AL_FREQUENCY, @i);
  Result := i;
end;


function TsgeSoundBuffer.GetSize: Cardinal;
var
  i: TALint;
begin
  alGetBufferi(FHandle, AL_SIZE, @i);
  Result := i;
end;


function TsgeSoundBuffer.GetSamples: Single;
var
  Chnl, Bits: Byte;
begin
  //Определить длину в семплах
  case GetChannels of
    sbcMono   : Chnl := 1;
    sbcStereo : Chnl := 2;
    else Chnl := 0;
  end;

  //Определить глубину
  case GetDept of
    sbd8Bit : Bits := 8;
    sbd16Bit: Bits := 16;
    else Bits := 0;
  end;

  //Результат
  Result := GetSize * 8 / (Chnl * Bits);
end;


function TsgeSoundBuffer.GetTime: Single;
begin
  Result := GetSamples / GetFrequency;
end;


procedure TsgeSoundBuffer.PreCreate;
begin
  //Проверить указатель на функцию
  if not Assigned(alGenBuffers) then
    raise EsgeException.Create(_UNITNAME, Err_SoundNotInitialized);

  //Обнулить ошибки
  alGetError;

  //Запросить буфер
  alGenBuffers(1, @FHandle);
  case alGetError() of
    AL_INVALID_VALUE: raise EsgeException.Create(_UNITNAME, Err_ReachedBufferLimit);
    AL_OUT_OF_MEMORY: raise EsgeException.Create(_UNITNAME, Err_OutOfMemory);
  end;
end;


constructor TsgeSoundBuffer.Create(FileName: String);
begin
  //Подготовительные работы
  PreCreate;

  //Запомнить путь
  FFileName := FileName;

  //Загрузить из файла
  LoadFromFile(FileName);
end;


constructor TsgeSoundBuffer.Create(Stream: TsgeMemoryStream);
begin
  //Подготовительные работы
  PreCreate;

  //Обнулить путь
  FFileName := '';

  //Загрузить из стрима
  FromMemoryStream(Stream);
end;


constructor TsgeSoundBuffer.CreateBlank;
begin
  //Подготовительные работы
  PreCreate;
end;


destructor TsgeSoundBuffer.Destroy;
begin
  if not Assigned(alDeleteBuffers) then Exit; //Проверить указатель на функцию

  alDeleteBuffers(1, @FHandle);               //Удалить буфер
end;


procedure TsgeSoundBuffer.FromMemoryStream(Stream: TsgeMemoryStream);
var
  Loader: TsgeSoundBufferWavLoader;
begin
  try
    try
      //Грузим данные
      Loader := TsgeSoundBufferWavLoader.Create(Stream);

      //Залить данные в OpenAL
      alBufferData(FHandle, Loader.Format, Loader.Data, Loader.Size, Loader.Frequency);

      //Проверить на ошибки
      case alGetError() of
        AL_INVALID_VALUE: raise EsgeException.Create(_UNITNAME, Err_WrongDataFormat);
        AL_OUT_OF_MEMORY: raise EsgeException.Create(_UNITNAME, Err_OutOfMemory);
        AL_INVALID_ENUM : raise EsgeException.Create(_UNITNAME, Err_UnsupportedFormat);
      end;

    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
    end;

  finally
    Loader.Free;
  end;
end;


procedure TsgeSoundBuffer.LoadFromFile(FileName: String);
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


procedure TsgeSoundBuffer.Reload;
begin
  LoadFromFile(FFileName);
end;





end.

