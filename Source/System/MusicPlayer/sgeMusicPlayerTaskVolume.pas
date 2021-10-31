{
Пакет             Simple Game Engine 2
Файл              sgeMusicPlayerTaskVolume.pas
Версия            1.0
Создан            23.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Music player: Задача: Volume
}
{$Include Defines.inc}

unit sgeMusicPlayerTaskVolume;

{$mode objfpc}{$H+}

interface

uses
  sgeMusicPLayerTaskBase;


type
  TsgeMusicPlayerTaskVolume = class(TsgeMusicPlayerTaskBase)
  const
    VOLUME_TIMES = 2000;
  private
    FVolume: Single;
  protected
    function  GetDeltaTime: Single; override;
    procedure Stop; override;
  public
    constructor Create(Volume: Single);
  end;


implementation

uses
  sgeVars,
  sgeExtensionMusicPlayer;

type
  TsgeExtMusicPlayerExt = class(TsgeExtensionMusicPlayer);


function TsgeMusicPlayerTaskVolume.GetDeltaTime: Single;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  //Изменить количество срабатываний, 2 секунды на смену громкости (по умолчанию)
  FTimes := VOLUME_TIMES div ExtMusic.FThreadDelay;

  //Посчитать шаг приращения громкости
  Result := CalculateDeltaTime(ExtMusic.FSource.Gain, FVolume, FTimes);
end;


procedure TsgeMusicPlayerTaskVolume.Stop;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  //Поправить громкость точно по свойству
  ExtMusic.Fsource.Gain := FVolume;
end;


constructor TsgeMusicPlayerTaskVolume.Create(Volume: Single);
begin
  FVolume := Volume;

  inherited Create;
end;



end.

