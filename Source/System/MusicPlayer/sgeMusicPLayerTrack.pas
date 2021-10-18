{
Пакет             Simple Game Engine 2
Файл              sgeMusicPLayerTrack.pas
Версия            1.0
Создан            17.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          MusicPlayer: Аудио дорожка
}
{$Include Defines.inc}

unit sgeMusicPLayerTrack;

{$mode objfpc}{$H+}

interface

type
  TsgeMusicPLayerTrack = class
  private
    FName: String;
    FGroup: String;
    FFileName: String;
  public
    constructor Create(Name: String; FileName: String; Group: String = '');

    property Name: String read FName write FName;
    property Group: String read FGroup write FGroup;
    property FileName: String read FFileName write FFileName;
  end;


implementation


constructor TsgeMusicPLayerTrack.Create(Name: String; FileName: String; Group: String);
begin
  FName := Name;
  FFileName := FileName;
  FGroup := Group;
end;


end.

