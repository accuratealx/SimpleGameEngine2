{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Музыкальный проигрыватель: Запуск
}
{$Include Defines.inc}

unit sgeEventMusicPlayerStart;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase, sgeEventMusicPlayer;


const
  Event_MusicPlayerStart = 'MusicPlayer.Start';


type
  TsgeEventMusicPlayerStart = class(TsgeEventMusicPlayer)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMusicPlayerStart.GetName: ShortString;
begin
  Result := Event_MusicPlayerStart;
end;


function TsgeEventMusicPlayerStart.Copy: TsgeEventBase;
begin
  Result := TsgeEventMusicPlayerStart.Create(FTrack);
end;



end.

