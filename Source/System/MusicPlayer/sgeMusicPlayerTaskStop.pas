{
Пакет             Simple Game Engine 2
Файл              sgeMusicPlayerTaskStop.pas
Версия            1.0
Создан            25.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Music player: Задача: Stop
}
{$Include Defines.inc}

unit sgeMusicPlayerTaskStop;

{$mode objfpc}{$H+}

interface

uses
  sgeMusicPLayerTaskBase;


type
  TsgeMusicPlayerTaskStop = class(TsgeMusicPlayerTaskBase)
  protected
    function GetDeltaTime: Single; override;
    procedure Stop; override;
  end;


implementation

uses
  sgeVars,
  sgeExtensionMusicPlayer;

type
  TsgeExtMusicPlayerExt = class(TsgeExtensionMusicPlayer);


function TsgeMusicPlayerTaskStop.GetDeltaTime: Single;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  Result := CalculateDeltaTime(ExtMusic.FSource.Gain, 0, FTimes);
end;


procedure TsgeMusicPlayerTaskStop.Stop;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(SGE.ExtMusicPlayer);

  //Остановить проигрывание
  ExtMusic.FSource.Stop;

  //Отвязать буфер
  ExtMusic.FSource.Buffer := nil;

  //Поправить громкость точно по свойству
  ExtMusic.Fsource.Gain := 0;
end;


end.

