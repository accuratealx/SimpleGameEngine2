{
Пакет             Simple Game Engine 2
Файл              sgeFontLoader.pas
Версия            1.0
Создан            23.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Загрузчик шрифта из файла
}
{$Include Defines.inc}

unit sgeFontLoader;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream,
  sgeFont;

type
  TsgeFontLoader = class
  private
    procedure FromMemoryStream(Stream: TsgeMemoryStream; Font: TsgeFont);

  public
    constructor Create(FileName: String; Font: TsgeFont);
    constructor Create(Stream: TsgeMemoryStream; Font: TsgeFont);
  end;


implementation

uses
  sgeErrors, sgeOSPlatform;

const
  _UNITNAME = 'sgeFontLoader';

  Err_FileNotFound = 'FileNotFound';
  Err_FileReadError = 'FileReadError';


procedure TsgeFontLoader.FromMemoryStream(Stream: TsgeMemoryStream; Font: TsgeFont);
begin

end;


constructor TsgeFontLoader.Create(FileName: String; Font: TsgeFont);
var
  Ms: TsgeMemoryStream;
begin
  if not sgeFileExists(FileName) then
    raise EsgeException.Create(_UNITNAME, Err_FileNotFound, FileName);


  Ms := TsgeMemoryStream.Create;
  try
    try
      //Прочитать из файла
      Ms.LoadFromFile(FileName);

      //Загрузить из потока
      FromMemoryStream(Ms, Font);

    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_FileReadError, FileName, E.Message);
    end;

  finally
    Ms.Free;
  end;
end;


constructor TsgeFontLoader.Create(Stream: TsgeMemoryStream; Font: TsgeFont);
begin
  FromMemoryStream(Stream, Font);
end;


end.

