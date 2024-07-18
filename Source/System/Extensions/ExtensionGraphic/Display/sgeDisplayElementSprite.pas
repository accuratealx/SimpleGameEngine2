{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementSprite.pas
Версия            1.3
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
  sgeTypes, sgeSprite, sgeColor,
  sgeDisplayElement;

type
  TsgeDisplayElementSprite = class(TsgeDisplayElement)
  public
    type
      //Типы измененных параметров
      TChange = (
        csPosition, //Положение на экране
        csSize,     //Размеры элемента
        csScale,    //Масштаб
        csOrigin,   //Точка поворота
        csAngle,    //Угол в радианах
        csColor,    //Цвет
        csSprite,   //Спрайт
        csReflect   //Отражение
      );

      //Набор измененных параметров
      TChangeSet = set of TChange;

      //Настройки отображения
      TData = record
        Position: TsgeFloatPoint; //Положение на экране
        Size: TsgeFloatPoint;     //Размеры элемента
        Scale: TsgeFloatPoint;    //Масштаб
        Origin: TsgeFloatPoint;   //Точка поворота
        Angle: Single;            //Угол в радианах
        Color: TsgeColor;         //Цвет
        Sprite: TsgeSprite;       //Спрайт
        Reflect: TsgeReflectSet;  //Отражение
      end;

  private
    FData: TData;
    FChangeSet: TChangeSet;

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

    property Data: TData read FData;
    property ChangeSet: TChangeSet read FChangeSet;

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
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSprite.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSprite.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSprite.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSprite.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSprite.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSprite.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSprite.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSprite.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSprite.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSprite.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSprite.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, csAngle);
end;


procedure TsgeDisplayElementSprite.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, csAngle);
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
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSprite.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSprite.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSprite.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSprite.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSprite.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, csSprite);
end;


procedure TsgeDisplayElementSprite.SetReflect(AReflect: TsgeReflectSet);
begin
  if FData.Reflect = AReflect then
    Exit;

  FData.Reflect := AReflect;
  Include(FChangeSet, csReflect);
end;


procedure TsgeDisplayElementSprite.FillData(X, Y, Width, Height: Single);
var
  i: TChange;
begin
  //Заполнить набор изменений
  for i := Low(TChange) to High(TChange) do
    Include(FChangeSet, i);

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
  inherited Create;

  SetSprite(Sprite);
  FillData(X, Y, Width, Height);
end;


constructor TsgeDisplayElementSprite.Create(X, Y: Single; Sprite: TsgeSprite);
begin
  inherited Create;

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

