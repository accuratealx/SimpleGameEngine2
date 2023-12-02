{
Пакет             Simple Game Engine 2
Файл              sgeCursor.pas
Версия            1.1
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
  sgeSprite, sgeAnimationFrameList;


type
  TsgeCursor = class
  private
    FSprite: TsgeSprite;
    FFrames: TsgeAnimationFrameList;
    FHotPoint: TsgeIntPoint;
    FWidth: Integer;
    FHeight: Integer;

  public
    constructor Create(Sprite: TsgeSprite; Frames: TsgeAnimationFrameList; Width, Height: Integer; HotPointX: Integer; HotPointY: Integer);

    property Sprite: TsgeSprite read FSprite write FSprite;
    property Frames: TsgeAnimationFrameList read FFrames write FFrames;
    property HotPoint: TsgeIntPoint read FHotPoint write FHotPoint;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'Cursor';

  Err_EmptyFrames = 'EmptyFrames';
  Err_EmptySprite = 'EmptySprite';
  Err_ZeroFrames  = 'ZeroFrames';


constructor TsgeCursor.Create(Sprite: TsgeSprite; Frames: TsgeAnimationFrameList; Width, Height: Integer; HotPointX: Integer; HotPointY: Integer);
begin
  //Проверить спрайт
  if Sprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  //Проверить кадры анимации
  if Frames = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFrames);

  //Проверить количество кадров
  if Frames.Count < 1 then
    raise EsgeException.Create(_UNITNAME, Err_ZeroFrames);

  FSprite := Sprite;
  FFrames := Frames;
  FWidth := Width;
  FHeight := Height;
  FHotPoint := sgeGetIntPoint(HotPointX, HotPointY);
end;


end.

