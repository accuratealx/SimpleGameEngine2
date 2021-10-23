{
Пакет             Simple Game Engine 2
Файл              sgeMusicPLayerTaskBase.pas
Версия            1.0
Создан            21.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Music player: Базовая задача
}
{$Include Defines.inc}

unit sgeMusicPLayerTaskBase;

{$mode objfpc}{$H+}

interface

type
  TsgeMusicPlayerTaskBase = class
  protected
    FMusicPLayer: TObject;
    FDone: Boolean;
  public
    constructor Create(ExtMusicPLayerObj: TObject);

    procedure Work; virtual; abstract;

    property Done: Boolean read FDone;
  end;


implementation


constructor TsgeMusicPlayerTaskBase.Create(ExtMusicPLayerObj: TObject);
begin
  FMusicPLayer := ExtMusicPLayerObj;
  FDone := False;
end;


end.

