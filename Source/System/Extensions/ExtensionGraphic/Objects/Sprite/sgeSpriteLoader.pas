{
Пакет             Simple Game Engine 2
Файл              sgeSpriteLoader.pas
Версия            1.0
Создан            05.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс загрузчика спрайтов: Базовый
}
{$Include Defines.inc}

unit sgeSpriteLoader;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream;


type
  TsgeSpriteLoader = class
  protected
    FData: Pointer;
    FSize: Int64;
    FWidth: DWord;
    FHeight: DWord;

  public
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    procedure FromMemoryStream(Stream: TsgeMemoryStream); virtual abstract;

    property Data: Pointer read FData;
    property Size: Int64 read FSize;
    property Width: DWord read FWidth;
    property Height: DWord read FHeight;
  end;


implementation


constructor TsgeSpriteLoader.Create(Stream: TsgeMemoryStream);
begin
  FromMemoryStream(Stream);
end;


destructor TsgeSpriteLoader.Destroy;
begin
  //Почистить память
  Freemem(FData, FSize);
end;



end.

