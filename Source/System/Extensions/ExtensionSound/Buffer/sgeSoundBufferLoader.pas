{
Пакет             Simple Game Engine 2
Файл              sgeSoundBufferLoader.pas
Версия            1.0
Создан            18.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс чтения аудио
}
{$Include Defines.inc}

unit sgeSoundBufferLoader;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream;


type
  //Базовый класс чтения звуковых данных
  TsgeSoundBufferLoader = class
  protected
    FFormat: Integer;                                               //Формат данных
    FFrequency: Cardinal;                                           //Частота
    FSize: Cardinal;                                                //Длина данных в байтах
    FData: Pointer;                                                 //Указатель на данные
  public
    constructor Create(FileName: String);
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure FromMemoryStream(Stream: TsgeMemoryStream); virtual abstract;
    procedure LoadFromFile(FileName: String);

    property Format: Integer read FFormat;
    property Frequency: Cardinal read FFrequency;
    property Size: Cardinal read FSize;
    property Data: Pointer read FData;
  end;



implementation

uses
  sgeErrors;


const
  _UNITNAME = 'SoundBufferLoader';

  Err_CantReadFile = 'CantReadFile';


constructor TsgeSoundBufferLoader.Create(FileName: String);
begin
  LoadFromFile(FileName);
end;


constructor TsgeSoundBufferLoader.Create(Stream: TsgeMemoryStream);
begin
  FromMemoryStream(Stream);
end;


destructor TsgeSoundBufferLoader.Destroy;
begin
  Freemem(FData, FSize);
end;


procedure TsgeSoundBufferLoader.LoadFromFile(FileName: String);
var
  Ms: TsgeMemoryStream;
begin
  try
    Ms := TsgeMemoryStream.Create;

    try
      //Чтение из файла
      Ms.LoadFromFile(FileName);

      //Загрузка из потока
      FromMemoryStream(Ms);
    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
    end;
  finally
    Ms.Free;
  end;
end;



end.

