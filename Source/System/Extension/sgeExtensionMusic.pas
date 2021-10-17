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
  sgeExtensionBase,
  sgeMusicPLayerTrackList;


const
  Extension_Music = 'Music';


type
  TsgeExtensionMusic = class(TsgeExtensionBase)
  private
    FTrackList: TsgeMusicPlayerTrackList;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    property TrackList: TsgeMusicPlayerTrackList read FTrackList;
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

    //Список дорожек
    FTrackList := TsgeMusicPlayerTrackList.Create(True);

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionMusic.Destroy;
begin
  FTrackList.Free;

  inherited Destroy;
end;



end.

