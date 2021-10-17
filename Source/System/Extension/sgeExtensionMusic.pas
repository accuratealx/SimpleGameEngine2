{
Пакет             Simple Game Engine 2
Файл              sgeExtensionMusic.pas
Версия            1.0
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Музыкальный проигрыватель
}
{$Include Defines.inc}

unit sgeExtensionMusic;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase;


const
  Extension_Music = 'Music';


type
  TsgeExtensionMusic = class(TsgeExtensionBase)
  private

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;


  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionMusic';


class function TsgeExtensionMusic.GetName: String;
begin
  Result := Extension_Music;
end;


constructor TsgeExtensionMusic.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionMusic.Destroy;
begin
  inherited Destroy;
end;



end.

