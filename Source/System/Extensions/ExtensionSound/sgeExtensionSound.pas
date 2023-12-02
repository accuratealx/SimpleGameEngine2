{
Пакет             Simple Game Engine 2
Файл              sgeExtensionSound.pas
Версия            1.0
Создан            18.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Звуковая система
}
{$Include Defines.inc}

unit sgeExtensionSound;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeSound;


const
  Extension_Sound = 'Sound';


type
  TsgeExtensionSound = class(TsgeExtensionBase)
  private
    FSound: TsgeSound;

  protected
    function GetName: String; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    property Sound: TsgeSound read FSound;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionSound';


function TsgeExtensionSound.GetName: String;
begin
  Result := Extension_Sound;
end;


constructor TsgeExtensionSound.Create;
begin
  try
    inherited Create;

    FSound := TsgeSound.Create;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionSound.Destroy;
begin
  FSound.Free;

  inherited Destroy;
end;


end.

