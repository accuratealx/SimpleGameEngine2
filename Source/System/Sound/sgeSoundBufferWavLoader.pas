{
Пакет             Simple Game Engine 2
Файл              sgeSoundBufferWavLoader.pas
Версия            1.0
Создан            18.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс чтения WAV аудио
}
{$Include Defines.inc}

unit sgeSoundBufferWavLoader;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream, sgeSoundBufferLoader;



type
  TsgeSoundBufferWavLoader = class(TsgeSoundBufferLoader)
  public
    procedure FromMemoryStream(Stream: TsgeMemoryStream); override;
  end;



implementation


uses
  sgeErrors, sgeSystemUtils,
  OpenAl, MMSystem;


const
  _UNITNAME = 'SoundBufferWavLoader';

  Err_WrongFileHeader         = 'WrongFileHeader';
  Err_WrongWAVEHeader         = 'WrongWAVEHeader';
  Err_UnsupportedFormat       = 'UnsupportedFormat';
  Err_BitsPerSampleNotSupport = 'BitsPerSampleNotSupport';
  Err_ChannelNumberNotSupport = 'ChannelNumberNotSupport';
  Err_CantLoadFromStream      = 'CantLoadFromStream';


type
  //Заголовок RIFF
  TChunkHeader = packed record
    Header: array[0..3] of Char;
    Size: LongInt;
  end;

  //Заголовок секции описания формата
  TFmtHeader = packed record
    Header: array[0..3] of Char;
    HeaderSize: LongWord;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: LongWord;
    BytesPerSecond: LongWord;
    BytesPerSample: Word;
    BitsPerSample: Word;
  end;



procedure TsgeSoundBufferWavLoader.FromMemoryStream(Stream: TsgeMemoryStream);
var
  Chunk: TChunkHeader;
  FmtHeader: TFmtHeader;
  Pos, DataSize: Cardinal;
  ChunkSize, FmtSize, DataIdx: Integer;
  Header: array[0..3] of Char;
  Buffer: Pointer;
begin
  //Подготовительные действия
  ChunkSize := SizeOf(TChunkHeader);
  FmtSize := SizeOf(TFmtHeader);
  FillChar(FmtHeader, FmtSize, 0);    //Обнулить заголовок формата
  FmtHeader.BitsPerSample := 8;       //Частный случай, выравнивание данных по 2 байта будем считать изначально 8 бит


  try
    //Чтение заголовка файла
    Stream.Read(Header, 0, SizeOf(Header));
    if Header <> 'RIFF' then
      raise EsgeException.Create(_UNITNAME, Err_WrongFileHeader, Header);


    //Чтение заголовка WAVE
    Stream.Read(Header, 8, SizeOf(Header));
    if Header <> 'WAVE' then
      raise EsgeException.Create(_UNITNAME, Err_WrongWAVEHeader, Header);


    //Обнулить память
    Freemem(FData, FSize);


    //Чтение чанков
    Pos := 12;
    while Pos < Stream.Size do
      begin
      //Чтение имени заголовка
      FillChar(Header, 4, 0);
      Stream.Read(Header, Pos, 4);


      //Чтение описания формата
      if Header = 'fmt ' then
        begin
        //Прочесть заголовок с данными
        Stream.Read(FmtHeader, Pos, FmtSize);

        //Проверить тип сжатие. Поддержка только PCM
        if FmtHeader.FormatCode <> WAVE_FORMAT_PCM then
          raise EsgeException.Create(_UNITNAME, Err_UnsupportedFormat, sgeIntToStr(FmtHeader.FormatCode));

        //Определить формат данных
        case FmtHeader.ChannelNumber of
          //Mono
          1:case FmtHeader.BitsPerSample of
              8 : FFormat := AL_FORMAT_MONO8;
              16: FFormat := AL_FORMAT_MONO16;
              else
                raise EsgeException.Create(_UNITNAME, Err_BitsPerSampleNotSupport, sgeIntToStr(FmtHeader.BitsPerSample));
            end;

          //Stereo
          2:case FmtHeader.BitsPerSample of
              8 : FFormat := AL_FORMAT_STEREO8;
              16: FFormat := AL_FORMAT_STEREO16;
              else
                raise EsgeException.Create(_UNITNAME, Err_BitsPerSampleNotSupport, sgeIntToStr(FmtHeader.BitsPerSample));
            end;

          else
            raise EsgeException.Create(_UNITNAME, Err_ChannelNumberNotSupport, sgeIntToStr(FmtHeader.ChannelNumber));
        end;

        //Определить частоту
        FFrequency := FmtHeader.SampleRate;

        //Сместить положение кусора
        Pos := Pos + FmtHeader.HeaderSize + 8;
        Continue;
        end;


      //Чтение данных звука
      if Header = 'data' then
        begin
        //Прочесть заголовок с данными
        Stream.Read(Chunk, Pos, ChunkSize);

        //Определить размер загружаемых данных
        DataSize := Chunk.Size;
        if (FmtHeader.BitsPerSample = 8) and not odd(DataSize) then Inc(DataSize, 1); //Поправка для 8-битных файлов

        //Чтение данных
        Buffer := AllocMem(DataSize);
        Stream.Read(Buffer^, Pos + ChunkSize, DataSize);

        //Скопировать данные
        DataIdx := FSize;
        FSize := FSize + DataSize;
        FData := ReAllocMem(FData, FSize);
        Move(Buffer^, (FData + DataIdx)^, DataSize);

        //Удалить буфер
        Freemem(Buffer, DataSize);

        //Сместить положение курсора
        Pos := Pos + DataSize + ChunkSize;
        Continue;
        end;



      //Другие чанки пропускаем
      Stream.Read(Chunk, Pos, ChunkSize);
      Pos := Pos + Chunk.Size + ChunkSize;
      end;



  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantLoadFromStream, '', E.Message);
  end;
end;


end.

