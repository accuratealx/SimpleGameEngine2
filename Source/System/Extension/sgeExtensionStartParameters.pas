{
Пакет             Simple Game Engine 2
Файл              sgeExtensionStartParameters.pas
Версия            1.1
Создан            17.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Стартовые параметры
}
{$Include Defines.inc}

unit sgeExtensionStartParameters;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeStartParameters;


const
  Extension_StartParameters = 'StartParameters';


type
  TsgeExtensionStartParameters = class(TsgeExtensionBase)
  private
    FParameters: TsgeStartParameters;

  protected
    function GetName: String; override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    property Parameters: TsgeStartParameters read FParameters;
  end;



implementation

uses
  sgeErrors;

const
  _UNITNAME = 'ExtensionStartParameters';



function TsgeExtensionStartParameters.GetName: String;
begin
  Result := Extension_StartParameters;
end;


constructor TsgeExtensionStartParameters.Create;
begin
  try
    inherited Create;

    FParameters := TsgeStartParameters.Create;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionStartParameters.Destroy;
begin
  FParameters.Free;

  inherited Destroy;
end;


end.

