{
Пакет             Simple Game Engine 2
Файл              sgeEventMusicPlayer.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Музыкальный проигрыватель
}
{$Include Defines.inc}

unit sgeEventMusicPlayer;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeMusicPlayerTrack;


type
  TsgeEventMusicPlayer = class(TsgeEventBase)
  protected
    FTrack: TsgeMusicPlayerTrack;
    function GetName: ShortString; override;
  public
    constructor Create(Track: TsgeMusicPlayerTrack);

    function Copy: TsgeEventBase; override;

    property Track: TsgeMusicPlayerTrack read FTrack;
  end;


implementation


function TsgeEventMusicPlayer.GetName: ShortString;
begin
  Result := 'MusicPlayer';
end;


constructor TsgeEventMusicPlayer.Create(Track: TsgeMusicPlayerTrack);
begin
  FTrack := Track;
end;


function TsgeEventMusicPlayer.Copy: TsgeEventBase;
begin
  Result := TsgeEventMusicPlayer.Create(FTrack);
end;



end.

