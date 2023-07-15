{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementSprite.pas
Версия            1.2
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementSprite;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementSpriteChangeSet = set of (
    descsPosition,  //Положение на экране
    descsSize,      //Размеры элемента
    descsScale,     //Масштаб
    descsOrigin,    //Точка поворота
    descsAngle,     //Угол в радианах
    descsColor,     //Цвет
    descsSprite,    //Спрайт
    descsReflect    //Отражение
  );


  //Настройки отображения
  TsgeDisplayElementSptiteData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
    Sprite: TsgeSprite;       //Спрайт
    Reflect: TsgeReflectSet;  //Отражение
  end;


  TsgeDisplayElementSprite = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementSptiteData;
    FChangeSet: TsgeDisplayElementSpriteChangeSet;

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
    procedure SetReflect(AReflect: TsgeReflectSet);

    procedure FillData(X, Y, Width, Height: Single);

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
    constructor Create(X, Y: Single; Sprite: TsgeSprite);

    function  GetCopy: TsgeDisplayElement; override;

    property Data: TsgeDisplayElementSptiteData read FData;
    property ChangeSet: TsgeDisplayElementSpriteChangeSet read FChangeSet;

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
    property Reflect: TsgeReflectSet read FData.Reflect write SetReflect;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  _UNITNAME = 'DisplayElementSprite';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementSprite.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, descsPosition);
end;


procedure TsgeDisplayElementSprite.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, descsPosition);
end;


procedure TsgeDisplayElementSprite.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, descsPosition);
end;


procedure TsgeDisplayElementSprite.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, descsSize);
end;


procedure TsgeDisplayElementSprite.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, descsSize);
end;


procedure TsgeDisplayElementSprite.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, descsScale);
end;


procedure TsgeDisplayElementSprite.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, descsScale);
end;


procedure TsgeDisplayElementSprite.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, descsScale);
end;


procedure TsgeDisplayElementSprite.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, descsOrigin);
end;


procedure TsgeDisplayElementSprite.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, descsOrigin);
end;


procedure TsgeDisplayElementSprite.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, descsOrigin);
end;


procedure TsgeDisplayElementSprite.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, descsAngle);
end;


procedure TsgeDisplayElementSprite.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, descsAngle);
end;


function TsgeDisplayElementSprite.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementSprite.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, descsColor);
end;


procedure TsgeDisplayElementSprite.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, descsColor);
end;


procedure TsgeDisplayElementSprite.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, descsColor);
end;


procedure TsgeDisplayElementSprite.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, descsColor);
end;


procedure TsgeDisplayElementSprite.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, descsColor);
end;


procedure TsgeDisplayElementSprite.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, descsSprite);
end;


procedure TsgeDisplayElementSprite.SetReflect(AReflect: TsgeReflectSet);
begin
  if FData.Reflect = AReflect then
    Exit;

  FData.Reflect := AReflect;
  Include(FChangeSet, descsReflect);
end;


procedure TsgeDisplayElementSprite.FillData(X, Y, Width, Height: Single);
const
  SetAll = [descsPosition, descsSize, descsScale, descsOrigin, descsAngle, descsColor, descsSprite, descsReflect];
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
  FData.Reflect := [];
end;


procedure TsgeDisplayElementSprite.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementSprite.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementSprite.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementSprite.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
begin
  SetSprite(Sprite);
  FillData(X, Y, Width, Height);
end;


constructor TsgeDisplayElementSprite.Create(X, Y: Single; Sprite: TsgeSprite);
begin
  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height);
end;


function TsgeDisplayElementSprite.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementSprite.Create;

  //Заполнить данные
  TsgeDisplayElementSprite(Result).FData := Self.FData;
  TsgeDisplayElementSprite(Result).FChangeSet := Self.FChangeSet;
end;



end.

