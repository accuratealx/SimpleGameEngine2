{
Пакет             Simple Game Engine 2
Файл              sgeEventMusicPlayer.pas
Версия            1.2
Создан            03.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Музыкальный проигрыватель
}
{$Include Defines.inc}

unit sgeEventMusicPlayer;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase,
  sgeMusicPlayerTrack;


const
  Event_MusicPLayerStart = 'Music.Start';
  Event_MusicPLayerStop  = 'Music.Stop';


type
  TsgeEventMusicPlayerStart = class(TsgeEventBase)
  private
    FTrack: TsgeMusicPlayerTrack;
  public
    constructor Create(Name: ShortString; Track: TsgeMusicPlayerTrack);

    function Copy: TsgeEventBase; override;

    property Track: TsgeMusicPlayerTrack read FTrack;
  end;



implementation


constructor TsgeEventMusicPlayerStart.Create(Name: ShortString; Track: TsgeMusicPlayerTrack);
begin
  inherited Create(Name);

  FTrack := Track;
end;


function TsgeEventMusicPlayerStart.Copy: TsgeEventBase;
begin
  Result := TsgeEventMusicPlayerStart.Create(FName, FTrack);
end;



end.

