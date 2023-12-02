{
Пакет             Simple Game Engine 2
Файл              sgeMusicPlayerTaskPlay.pas
Версия            1.0
Создан            23.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Music player: Задача: Play
}
{$Include Defines.inc}

unit sgeMusicPlayerTaskPlay;

{$mode objfpc}{$H+}

interface

uses
  sgeMusicPLayerTaskBase, sgeMusicPlayerTrack;


type
  TsgeMusicPlayerTaskPlay = class(TsgeMusicPlayerTaskBase)
  private
    FTrack: TsgeMusicPlayerTrack;

  protected
    function  GetDeltaTime: Single; override;
    procedure Start; override;
    procedure Stop; override;

  public
    constructor Create(Track: TsgeMusicPlayerTrack);
  end;


implementation

uses
  sgeCorePointerUtils,
  sgeErrors, sgeMemoryStream,
  sgeEventMusicPlayer,
  sgeExtensionMusicPlayer;

type
  TsgeExtMusicPlayerExt = class(TsgeExtensionMusicPlayer);

const
  _UNITNAME = 'MusicPlayerTaskPlay';

  Err_CantPlay = 'CantPlay';


function TsgeMusicPlayerTaskPlay.GetDeltaTime: Single;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(sgeCorePointer_GetSGE.ExtMusicPlayer);

  Result := CalculateDeltaTime(ExtMusic.FSource.Gain, ExtMusic.FVolume, FTimes);
end;


procedure TsgeMusicPlayerTaskPlay.Start;
var
  Ms: TsgeMemoryStream;
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(sgeCorePointer_GetSGE.ExtMusicPlayer);

  try
    Ms := TsgeMemoryStream.Create;

    //Загрузить дорожку из файла
    try
      sgeCorePointer_GetSGE.ExtFileSystem.ReadFile(FTrack.FileName, Ms);
    except
      on E: EsgeException do
      begin
        sgeCorePointer_GetSGE.ErrorManager.ProcessError(sgeCreateErrorString(_UNITNAME, Err_CantPlay, '', E.Message));
        FDone := True;
        Exit;
      end;
    end;

    //Залить в буфер
    ExtMusic.FBuffer.FromMemoryStream(Ms);

    //Установить буфер
    ExtMusic.FSource.Buffer := ExtMusic.FBuffer;

    //Запустить проигрывание
    ExtMusic.FSource.Play;

    //Добавить событие старта
    sgeCorePointer_GetSGE.EventManager.Publish(TsgeEventMusicPlayerStart.Create(Event_MusicPLayerStart, FTrack));
  finally
    Ms.Free;
  end;
end;


procedure TsgeMusicPlayerTaskPlay.Stop;
var
  ExtMusic: TsgeExtMusicPlayerExt;
begin
  //Ссылка на расширение
  ExtMusic := TsgeExtMusicPlayerExt(sgeCorePointer_GetSGE.ExtMusicPlayer);

  //Поправить громкость точно по свойству
  ExtMusic.Fsource.Gain := ExtMusic.FVolume;
end;


constructor TsgeMusicPlayerTaskPlay.Create(Track: TsgeMusicPlayerTrack);
begin
  inherited Create;

  FTrack := Track;
end;


end.

