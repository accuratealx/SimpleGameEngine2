{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpritePart.pas
Версия            1.1
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpritePart;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  //Набор измененных параметров
  TsgeDisplayElementItemSpritePartChangeSet = set of (
    deispcsPosition,    //Положение на экране
    deispcsSize,        //Размеры элемента
    deispcsScale,       //Масштаб
    deispcsOrigin,      //Точка поворота
    deispcsAngle,       //Угол в радианах
    deispcsColor,       //Цвет
    deispcsSpriteRect,  //Координаты части спрайта
    deispcsSprite       //Спрайт
  );


  //Настройки отображения
  TsgeDisplayElementItemSptitePartData = record
    Position: TsgeFloatPoint;   //Положение на экране
    Size: TsgeFloatPoint;       //Размеры элемента
    Scale: TsgeFloatPoint;      //Масштаб
    Origin: TsgeFloatPoint;     //Точка поворота
    Angle: Single;              //Угол в радианах
    Color: TsgeColor;           //Цвет
    SpriteRect: TsgeFloatRect;  //Координаты части спрайта
    Sprite: TsgeSprite;         //Спрайт
  end;


  TsgeDisplayElementItemSpritePart = class(TsgeDisplayElementItemBase)
  private
    FData: TsgeDisplayElementItemSptitePartData;
    FChangeSet: TsgeDisplayElementItemSpritePartChangeSet;

    procedure SetPosition(APosition: TsgeFloatPoint);
    procedure SetPositionX(APositionX: Single);
    procedure SetPositionY(APositionY: Single);
    procedure SetWidth(AWidth: Single);
    procedure SetHeight(AHeight: Single);
    procedure SetScale(AScale: Single);
    procedure SetScaleX(AScaleX: Single);
    procedure SetScaleY(AScaleY: Single);
    procedure SetOrigin(AOrigin: TsgeFloatPoint);
    procedure SetOriginX(AOriginX: Single);
    procedure SetOriginY(AOriginY: Single);
    procedure SetAngle(AAngle: Single);
    procedure SetAngleDegree(AAngle: Single);
    function  GetAngleDegree: Single;
    procedure SetColor(AColor: TsgeColor);
    procedure SetColorRed(ARed: Single);
    procedure SetColorGreen(AGreen: Single);
    procedure SetColorBlue(ABlue: Single);
    procedure SetColorAlpha(AAlpha: Single);
    procedure SetSpriteRect(ASpriteRect: TsgeFloatRect);
    procedure SetSpriteRectX1(ASpriteRectX1: Single);
    procedure SetSpriteRectY1(ASpriteRectY1: Single);
    procedure SetSpriteRectX2(ASpriteRectX2: Single);
    procedure SetSpriteRectY2(ASpriteRectY2: Single);
    procedure SetSprite(ASprite: TsgeSprite);

    procedure FillData(X, Y, Width, Height: Single; SpriteRect: TsgeFloatRect);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);

    procedure ResetChangeSet; override;
    function  GetCopy: TsgeDisplayElementItemBase; override;

    property Data: TsgeDisplayElementItemSptitePartData read FData;
    property ChangeSet: TsgeDisplayElementItemSpritePartChangeSet read FChangeSet;

    property Position: TsgeFloatPoint read FData.Position write SetPosition;
    property PositionX: Single read FData.Position.X write SetPositionX;
    property PositionY: Single read FData.Position.Y write SetPositionY;
    property Width: Single read FData.Size.X write SetWidth;
    property Height: Single read FData.Size.Y write SetHeight;
    property Scale: Single read FData.Scale.X write SetScale;
    property ScaleX: Single read FData.Scale.X write SetScaleX;
    property ScaleY: Single read FData.Scale.Y write SetScaleY;
    property Origin: TsgeFloatPoint read FData.Origin write SetOrigin;
    property OriginX: Single read FData.Origin.X write SetOriginX;
    property OriginY: Single read FData.Origin.Y write SetOriginY;
    property Angle: Single read FData.Angle write SetAngle;
    property AngleDegree: Single read GetAngleDegree write SetAngleDegree;
    property Color: TsgeColor read FData.Color write SetColor;
    property ColorR: Single read FData.Color.Red write SetColorRed;
    property ColorG: Single read FData.Color.Green write SetColorGreen;
    property ColorB: Single read FData.Color.Blue write SetColorBlue;
    property ColorA: Single read FData.Color.Alpha write SetColorAlpha;
    property SpriteRect: TsgeFloatRect read FData.SpriteRect write SetSpriteRect;
    property SpriteRectX1: Single read FData.SpriteRect.X1 write SetSpriteRectX1;
    property SpriteRectY1: Single read FData.SpriteRect.Y1 write SetSpriteRectY1;
    property SpriteRectX2: Single read FData.SpriteRect.X2 write SetSpriteRectX2;
    property SpriteRectY2: Single read FData.SpriteRect.Y2 write SetSpriteRectY2;
    property Sprite: TsgeSprite read FData.Sprite write SetSprite;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  sgeDisplayElementItemSpritePartChangeSetAll = [
    deispcsPosition, deispcsSize, deispcsScale, deispcsOrigin, deispcsAngle, deispcsColor, deispcsSpriteRect, deispcsSprite
  ];

  _UNITNAME = 'DisplayElementItemSpritePart';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSpritePart.FillData(X, Y, Width, Height: Single; SpriteRect: TsgeFloatRect);
begin
  FChangeSet := sgeDisplayElementItemSpritePartChangeSetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Size := sgeGetFloatPoint(Width, Height);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;
  FData.SpriteRect := SpriteRect;
end;


procedure TsgeDisplayElementItemSpritePart.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deispcsPosition);
end;


procedure TsgeDisplayElementItemSpritePart.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deispcsPosition);
end;


procedure TsgeDisplayElementItemSpritePart.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deispcsPosition);
end;


procedure TsgeDisplayElementItemSpritePart.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deispcsSize);
end;


procedure TsgeDisplayElementItemSpritePart.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deispcsSize);
end;


procedure TsgeDisplayElementItemSpritePart.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deispcsScale);
end;


procedure TsgeDisplayElementItemSpritePart.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deispcsScale);
end;


procedure TsgeDisplayElementItemSpritePart.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deispcsScale);
end;


procedure TsgeDisplayElementItemSpritePart.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deispcsOrigin);
end;


procedure TsgeDisplayElementItemSpritePart.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deispcsOrigin);
end;


procedure TsgeDisplayElementItemSpritePart.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deispcsOrigin);
end;


procedure TsgeDisplayElementItemSpritePart.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deispcsAngle);
end;


procedure TsgeDisplayElementItemSpritePart.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deispcsAngle);
end;


function TsgeDisplayElementItemSpritePart.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementItemSpritePart.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deispcsColor);
end;


procedure TsgeDisplayElementItemSpritePart.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deispcsColor);
end;


procedure TsgeDisplayElementItemSpritePart.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deispcsColor);
end;


procedure TsgeDisplayElementItemSpritePart.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deispcsColor);
end;


procedure TsgeDisplayElementItemSpritePart.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deispcsColor);
end;


procedure TsgeDisplayElementItemSpritePart.SetSpriteRect(ASpriteRect: TsgeFloatRect);
begin
  FData.SpriteRect := ASpriteRect;
  Include(FChangeSet, deispcsSpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.SetSpriteRectX1(ASpriteRectX1: Single);
begin
  FData.SpriteRect.X1 := ASpriteRectX1;
  Include(FChangeSet, deispcsSpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.SetSpriteRectY1(ASpriteRectY1: Single);
begin
  FData.SpriteRect.Y1 := ASpriteRectY1;
  Include(FChangeSet, deispcsSpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.SetSpriteRectX2(ASpriteRectX2: Single);
begin
  FData.SpriteRect.X2 := ASpriteRectX2;
  Include(FChangeSet, deispcsSpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.SetSpriteRectY2(ASpriteRectY2: Single);
begin
  FData.SpriteRect.Y2 := ASpriteRectY2;
  Include(FChangeSet, deispcsSpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  Include(FChangeSet, deispcsSprite);
  FData.Sprite := ASprite;
end;


constructor TsgeDisplayElementItemSpritePart.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
begin
  SetSprite(Sprite);
  FillData(X, Y, Width, Height, SpriteRect);
end;


constructor TsgeDisplayElementItemSpritePart.Create(X, Y: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
begin
  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height, SpriteRect);
end;


procedure TsgeDisplayElementItemSpritePart.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementItemSpritePart.GetCopy: TsgeDisplayElementItemBase;
begin
  Result := TsgeDisplayElementItemSpritePart.Create(0, 0, 0, 0, Self.FData.Sprite, Self.FData.SpriteRect);
  TsgeDisplayElementItemSpritePart(Result).FData := Self.FData;
end;



end.


