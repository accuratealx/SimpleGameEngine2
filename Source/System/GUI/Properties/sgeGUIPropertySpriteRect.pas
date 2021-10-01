unit sgeGUIPropertySpriteRect;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeGraphicSprite,
  sgeGUIProperty, sgeGUIPropertyIntPoint, sgeGUIPropertyFloatRect;


type
  //Режим
  TsgeGUIPropertySpriteRectMode = (srmFull, srmTile, srmRect);


  TsgeGUIPropertySpriteRect = class(TsgeGUIProperty)
  private
    FMode: TsgeGUIPropertySpriteRectMode;
    FTile: TsgeGUIPropertyIntPoint;                                 //Координаты плитки
    FRect: TsgeGUIPropertyFloatRect;                                //Координаты прямоугольника

    procedure SetMode(AMode: TsgeGUIPropertySpriteRectMode);
  public
    constructor Create(AOwner: TObject); override;
    destructor  Destroy; override;

    property Mode: TsgeGUIPropertySpriteRectMode read FMode write SetMode;
    property Tile: TsgeGUIPropertyIntPoint read FTile;
    property Rect: TsgeGUIPropertyFloatRect read FRect;
  end;


  TsgeGUIPropertySpriteRectExt = class(TsgeGUIPropertySpriteRect)
  public
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


constructor TsgeGUIPropertySpriteRect.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FTile := TsgeGUIPropertyIntPoint.Create(AOwner);
  FRect := TsgeGUIPropertyFloatRect.Create(AOwner);

  FMode := srmFull;
end;


destructor TsgeGUIPropertySpriteRect.Destroy;
begin
  FRect.Free;
  FTile.Free;

  inherited Destroy;
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

