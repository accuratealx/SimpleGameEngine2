{
Пакет             Simple Game Engine 2
Файл              sgeGraphicSpriteLoader.pas
Версия            1.0
Создан            27.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс загрузчика спрайтов: Базовый
}
{$Include Defines.inc}

unit sgeGraphicSpriteLoader;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream;


type
  TsgeGraphicSpriteLoader = class
  protected
    FData: Pointer;
    FWidth: DWord;
    FHeight: DWord;

  public
    constructor Create(FileName: String);
    constructor Create(Stream: TsgeMemoryStream);

    procedure FromMemoryStream(Stream: TsgeMemoryStream); virtual abstract;
    procedure LoadFromFile(FileName: String);

    property Data: Pointer read FData;
    property Width: DWord read FWidth;
    property Height: DWord read FHeight;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'GraphicSpriteLoader';

  Err_CantReadFile = 'CantReadFile';



constructor TsgeGraphicSpriteLoader.Create(FileName: String);
begin
  LoadFromFile(FileName);
end;


constructor TsgeGraphicSpriteLoader.Create(Stream: TsgeMemoryStream);
begin
  FromMemoryStream(Stream);
end;


procedure TsgeGraphicSpriteLoader.LoadFromFile(FileName: String);
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
      on E:EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantReadFile, FileName, E.Message);
    end;

  finally
    Ms.Free;
  end;
end;


end.

