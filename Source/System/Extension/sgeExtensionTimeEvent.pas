{
Пакет             Simple Game Engine 2
Файл              sgeExtensionTimeEvent.pas
Версия            1.0
Создан            29.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Таймерные события
}
{$Include Defines.inc}

unit sgeExtensionTimeEvent;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase;


const
  Extension_TimeEvent = 'TimeEvent';


type
  TsgeExtensionTimeEvent = class(TsgeExtensionBase)
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
  _UNITNAME = 'ExtensionTimeEvent';


class function TsgeExtensionTimeEvent.GetName: String;
begin
  Result := Extension_TimeEvent;
end;


constructor TsgeExtensionTimeEvent.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Параметры по умолчанию
    //FMainDir := sgeGetApplicationDirectory;
    //FScreenshotDir := FMainDir + 'Screenshot\';

    //Получить ссылки на объекты
    //FExtPackList := TsgeExtensionPackList(GetExtension(Extension_PackList));

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionTimeEvent.Destroy;
begin
  inherited Destroy;
end;




end.

