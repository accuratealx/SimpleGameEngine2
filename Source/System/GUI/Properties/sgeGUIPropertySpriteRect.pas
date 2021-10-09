{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertySpriteRect.pas
Версия            1.1
Создан            01.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Часть спрайта для вывода
}
{$Include Defines.inc}

unit sgeGUIPropertySpriteRect;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeSimpleParameters, sgeGraphicSprite,
  sgeGUIProperty, sgeGUIPropertyIntPoint, sgeGUIPropertyFloatRect;


type
  //Режим
  TsgeGUIPropertySpriteRectMode = (srmFull, srmTile, srmRect);


  TsgeGUIPropertySpriteRect = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertySpriteRectMode;
    FTile: TsgeGUIPropertyIntPointExt;                              //Координаты плитки
    FRect: TsgeGUIPropertyFloatRectExt;                             //Координаты прямоугольника

    procedure SetMode(AMode: TsgeGUIPropertySpriteRectMode);

    function GetTile: TsgeGUIPropertyIntPoint;
    function GetRect: TsgeGUIPropertyFloatRect;
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertySpriteRectMode read FMode write SetMode;
    property Tile: TsgeGUIPropertyIntPoint read GetTile;
    property Rect: TsgeGUIPropertyFloatRect read GetRect;
  end;


  TsgeGUIPropertySpriteRectExt = class(TsgeGUIPropertySpriteRect)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');
    function GetRect(Sprite: TsgeGraphicSprite): TsgeFloatRect;     //Прямоугольник в координатах OpenGL
  end;


implementation

uses
  sgeGraphicUtils;


procedure TsgeGUIPropertySpriteRect.SetMode(AMode: TsgeGUIPropertySpriteRectMode);
begin
  if FMode = AMode then Exit;

  FMode := AMode;
  UpdateParent;
end;


function TsgeGUIPropertySpriteRect.GetTile: TsgeGUIPropertyIntPoint;
begin
  Result := FTile;
end;


function TsgeGUIPropertySpriteRect.GetRect: TsgeGUIPropertyFloatRect;
begin
  Result := FRect
end;


constructor TsgeGUIPropertySpriteRect.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FTile := TsgeGUIPropertyIntPointExt.Create(AOwner);
  FRect := TsgeGUIPropertyFloatRectExt.Create(AOwner);

  FMode := srmFull;
end;


destructor TsgeGUIPropertySpriteRect.Destroy;
begin
  FRect.Free;
  FTile.Free;

  inherited Destroy;
end;


procedure TsgeGUIPropertySpriteRectExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  //Mode
  ParamName := Prefix + 'Mode';
  if Parameters.Exist[ParamName] then
    begin
    s := LowerCase(Parameters.GetValue(ParamName, ''));
    case s of
      'full': FMode := srmFull;
      'tile': FMode := srmTile;
      'rect': FMode := srmRect;
    end;
    end;

  //Tile
  FTile.LoadParameters(Parameters, Prefix + 'Tile.');

  //Rect
  FRect.LoadParameters(Parameters, Prefix + 'Rect.');
end;


function TsgeGUIPropertySpriteRectExt.GetRect(Sprite: TsgeGraphicSprite): TsgeFloatRect;
begin
  case FMode of
    srmFull: Result := sgeGetFloatRect(0, 1, 1, 0);
    srmTile: Result := sgeGetTextureTileRect(Sprite, FTile.X, FTile.Y);
    srmRect: Result := sgeGetTextureRect(Sprite, FRect.Rect);
  end;
end;



end.

