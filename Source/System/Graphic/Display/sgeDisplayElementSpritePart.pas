{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementSpritePart.pas
Версия            1.2
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Часть спрайта
}
{$Include Defines.inc}

unit sgeDisplayElementSpritePart;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementSpritePartChangeSet = set of (
    despcsPosition,     //Положение на экране
    despcsSize,         //Размеры элемента
    despcsScale,        //Масштаб
    despcsOrigin,       //Точка поворота
    despcsAngle,        //Угол в радианах
    despcsColor,        //Цвет
    despcsSpriteRect,   //Координаты части спрайта
    despcsSprite        //Спрайт
  );


  //Настройки отображения
  TsgeDisplayElementSptitePartData = record
    Position: TsgeFloatPoint;   //Положение на экране
    Size: TsgeFloatPoint;       //Размеры элемента
    Scale: TsgeFloatPoint;      //Масштаб
    Origin: TsgeFloatPoint;     //Точка поворота
    Angle: Single;              //Угол в радианах
    Color: TsgeColor;           //Цвет
    SpriteRect: TsgeFloatRect;  //Координаты части спрайта
    Sprite: TsgeSprite;         //Спрайт
  end;


  TsgeDisplayElementSpritePart = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementSptitePartData;
    FChangeSet: TsgeDisplayElementSpritePartChangeSet;

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

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);

    function GetCopy: TsgeDisplayElement; override;

    property Data: TsgeDisplayElementSptitePartData read FData;
    property ChangeSet: TsgeDisplayElementSpritePartChangeSet read FChangeSet;

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
  _UNITNAME = 'DisplayElementSpritePart';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementSpritePart.FillData(X, Y, Width, Height: Single; SpriteRect: TsgeFloatRect);
const
  SetAll = [despcsPosition, despcsSize, despcsScale, despcsOrigin, despcsAngle, despcsColor, despcsSpriteRect, despcsSprite];
begin
  inherited Create;

  FChangeSet := SetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Size := sgeGetFloatPoint(Width, Height);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;
  FData.SpriteRect := SpriteRect;
end;


procedure TsgeDisplayElementSpritePart.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementSpritePart.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementSpritePart.Create;
begin
  //Заглушка
end;


procedure TsgeDisplayElementSpritePart.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, despcsPosition);
end;


procedure TsgeDisplayElementSpritePart.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, despcsPosition);
end;


procedure TsgeDisplayElementSpritePart.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, despcsPosition);
end;


procedure TsgeDisplayElementSpritePart.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, despcsSize);
end;


procedure TsgeDisplayElementSpritePart.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, despcsSize);
end;


procedure TsgeDisplayElementSpritePart.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, despcsScale);
end;


procedure TsgeDisplayElementSpritePart.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, despcsScale);
end;


procedure TsgeDisplayElementSpritePart.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, despcsScale);
end;


procedure TsgeDisplayElementSpritePart.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, despcsOrigin);
end;


procedure TsgeDisplayElementSpritePart.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, despcsOrigin);
end;


procedure TsgeDisplayElementSpritePart.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, despcsOrigin);
end;


procedure TsgeDisplayElementSpritePart.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, despcsAngle);
end;


procedure TsgeDisplayElementSpritePart.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, despcsAngle);
end;


function TsgeDisplayElementSpritePart.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementSpritePart.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, despcsColor);
end;


procedure TsgeDisplayElementSpritePart.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, despcsColor);
end;


procedure TsgeDisplayElementSpritePart.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, despcsColor);
end;


procedure TsgeDisplayElementSpritePart.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, despcsColor);
end;


procedure TsgeDisplayElementSpritePart.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, despcsColor);
end;


procedure TsgeDisplayElementSpritePart.SetSpriteRect(ASpriteRect: TsgeFloatRect);
begin
  FData.SpriteRect := ASpriteRect;
  Include(FChangeSet, despcsSpriteRect);
end;


procedure TsgeDisplayElementSpritePart.SetSpriteRectX1(ASpriteRectX1: Single);
begin
  FData.SpriteRect.X1 := ASpriteRectX1;
  Include(FChangeSet, despcsSpriteRect);
end;


procedure TsgeDisplayElementSpritePart.SetSpriteRectY1(ASpriteRectY1: Single);
begin
  FData.SpriteRect.Y1 := ASpriteRectY1;
  Include(FChangeSet, despcsSpriteRect);
end;


procedure TsgeDisplayElementSpritePart.SetSpriteRectX2(ASpriteRectX2: Single);
begin
  FData.SpriteRect.X2 := ASpriteRectX2;
  Include(FChangeSet, despcsSpriteRect);
end;


procedure TsgeDisplayElementSpritePart.SetSpriteRectY2(ASpriteRectY2: Single);
begin
  FData.SpriteRect.Y2 := ASpriteRectY2;
  Include(FChangeSet, despcsSpriteRect);
end;


procedure TsgeDisplayElementSpritePart.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, despcsSprite);
end;


constructor TsgeDisplayElementSpritePart.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
begin
  SetSprite(Sprite);
  FillData(X, Y, Width, Height, SpriteRect);
end;


constructor TsgeDisplayElementSpritePart.Create(X, Y: Single; Sprite: TsgeSprite; SpriteRect: TsgeFloatRect);
begin
  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height, SpriteRect);
end;


function TsgeDisplayElementSpritePart.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementSpritePart.Create;

  //Заполнить данные
  TsgeDisplayElementSpritePart(Result).FData := Self.FData;
  TsgeDisplayElementSpritePart(Result).FChangeSet := Self.FChangeSet;
end;



end.


