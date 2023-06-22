{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSprite.pas
Версия            1.1
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSprite;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  //Набор измененных параметров
  TsgeDisplayElementItemSpriteChangeSet = set of (
    deiscsPosition, //Положение на экране
    deiscsSize,     //Размеры элемента
    deiscsScale,    //Масштаб
    deiscsOrigin,   //Точка поворота
    deiscsAngle,    //Угол в радианах
    deiscsColor,    //Цвет
    deiscsSprite    //Спрайт
  );


  //Настройки отображения
  TsgeDisplayElementItemSptiteData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
    Sprite: TsgeSprite;       //Спрайт
  end;


  TsgeDisplayElementItemSprite = class(TsgeDisplayElementItemBase)
  private
    FData: TsgeDisplayElementItemSptiteData;
    FChangeSet: TsgeDisplayElementItemSpriteChangeSet;

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
    procedure SetSprite(ASprite: TsgeSprite);

    procedure FillData(X, Y, Width, Height: Single);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
    constructor Create(X, Y: Single; Sprite: TsgeSprite);

    procedure ResetChangeSet; override;
    function  GetCopy: TsgeDisplayElementItemBase; override;

    property Data: TsgeDisplayElementItemSptiteData read FData;
    property ChangeSet: TsgeDisplayElementItemSpriteChangeSet read FChangeSet;

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
    property Sprite: TsgeSprite read FData.Sprite write SetSprite;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  sgeDisplayElementItemSpriteChangeSetAll = [
    deiscsPosition, deiscsSize, deiscsScale, deiscsOrigin, deiscsAngle, deiscsColor, deiscsSprite
  ];

  _UNITNAME = 'DisplayElementItemSprite';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSprite.FillData(X, Y, Width, Height: Single);
begin
  FChangeSet := sgeDisplayElementItemSpriteChangeSetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Size := sgeGetFloatPoint(Width, Height);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;
end;


procedure TsgeDisplayElementItemSprite.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deiscsPosition);
end;


procedure TsgeDisplayElementItemSprite.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deiscsPosition);
end;


procedure TsgeDisplayElementItemSprite.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deiscsPosition);
end;


procedure TsgeDisplayElementItemSprite.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deiscsSize);
end;


procedure TsgeDisplayElementItemSprite.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deiscsSize);
end;


procedure TsgeDisplayElementItemSprite.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deiscsScale);
end;


procedure TsgeDisplayElementItemSprite.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deiscsScale);
end;


procedure TsgeDisplayElementItemSprite.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deiscsScale);
end;


procedure TsgeDisplayElementItemSprite.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deiscsOrigin);
end;


procedure TsgeDisplayElementItemSprite.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deiscsOrigin);
end;


procedure TsgeDisplayElementItemSprite.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deiscsOrigin);
end;


procedure TsgeDisplayElementItemSprite.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deiscsAngle);
end;


procedure TsgeDisplayElementItemSprite.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deiscsAngle);
end;


function TsgeDisplayElementItemSprite.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementItemSprite.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deiscsColor);
end;


procedure TsgeDisplayElementItemSprite.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deiscsColor);
end;


procedure TsgeDisplayElementItemSprite.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deiscsColor);
end;


procedure TsgeDisplayElementItemSprite.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deiscsColor);
end;


procedure TsgeDisplayElementItemSprite.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deiscsColor);
end;


procedure TsgeDisplayElementItemSprite.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  Include(FChangeSet, deiscsSprite);
  FData.Sprite := ASprite;
end;


constructor TsgeDisplayElementItemSprite.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
begin
  SetSprite(Sprite);
  FillData(X, Y, Width, Height);
end;


constructor TsgeDisplayElementItemSprite.Create(X, Y: Single; Sprite: TsgeSprite);
begin
  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height);
end;


procedure TsgeDisplayElementItemSprite.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementItemSprite.GetCopy: TsgeDisplayElementItemBase;
begin
  Result := TsgeDisplayElementItemSprite.Create(0, 0, 0, 0, Self.FData.Sprite);
  TsgeDisplayElementItemSprite(Result).FData := Self.FData;
end;



end.

