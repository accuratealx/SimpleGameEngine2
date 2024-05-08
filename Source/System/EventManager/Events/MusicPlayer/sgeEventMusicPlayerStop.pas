{
Пакет             Simple Game Engine 2
Файл              sgeEventMusicPlayerStop.pas
Версия            1.0
Создан            08.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Музыкальный проигрыватель: Останов
}
{$Include Defines.inc}

unit sgeEventMusicPlayerStop;

{$mode ObjFPC}{$H+}

interface

uses
  sgeEventBase;


const
  Event_MusicPlayerStop = 'MusicPlayer.Stop';


type
  TsgeEventMusicPlayerStop = class(TsgeEventBase)
  protected
    function GetName: ShortString; override;
  public
    function Copy: TsgeEventBase; override;
  end;


implementation


function TsgeEventMusicPlayerStop.GetName: ShortString;
begin
  Result := Event_MusicPlayerStop;
end;


function TsgeEventMusicPlayerStop.Copy: TsgeEventBase;
begin
  Result := TsgeEventMusicPlayerStop.Create;
end;



end.

