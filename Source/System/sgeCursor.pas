{
Пакет             Simple Game Engine 2
Файл              sgeCursor.pas
Версия            1.0
Создан            06.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Графический курсор
}
{$Include Defines.inc}

unit sgeCursor;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicAnimationFrames;


type
  TsgeCursor = class
  private
    FFrames: TsgeGraphicAnimationFrames;
    FHotPoint: TsgeIntPoint;
    FWidth: Integer;
    FHeight: Integer;

  public
    constructor Create(Frames: TsgeGraphicAnimationFrames; Width, Height: Integer; HotPointX: Integer = 0; HotPointY: Integer = 0);

    property HotPoint: TsgeIntPoint read FHotPoint;
    property Frames: TsgeGraphicAnimationFrames read FFrames;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'Cursor';

  Err_EmptyFrames = 'EmptyFrames';


constructor TsgeCursor.Create(Frames: TsgeGraphicAnimationFrames; Width, Height: Integer; HotPointX: Integer; HotPointY: Integer);
begin
  if Frames = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFrames);

  FFrames := Frames;
  FWidth := Width;
  FHeight := Height;
  FHotPoint := sgeGetIntPoint(HotPointX, HotPointY);
end;


end.

