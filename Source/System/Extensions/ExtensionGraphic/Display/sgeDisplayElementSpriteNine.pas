{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpriteNine.pas
Версия            1.3
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт 9
}
{$Include Defines.inc}

unit sgeDisplayElementSpriteNine;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeColor,
  sgeDisplayElement;

type
  TsgeDisplayElementSpriteNine = class(TsgeDisplayElement)
  public
    type
      TChange = (
        csPosition, //Положение на экране
        csSize,     //Размеры элемента
        csScale,    //Масштаб
        csOrigin,   //Точка поворота
        csAngle,    //Угол в радианах
        csColor,    //Цвет
        csOffset,   //Смещение от краев спрайта в пикселях
        csSprite    //Спрайт
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
        Offset: TsgeFloatRect;    //Смещение от краев спрайта в пикселях
        Sprite: TsgeSprite;       //Спрайт
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
    procedure SetOffset(AOffset: TsgeFloatRect);
    procedure SetOffsetX1(AOffsetX1: Single);
    procedure SetOffsetY1(AOffsetY1: Single);
    procedure SetOffsetX2(AOffsetX2: Single);
    procedure SetOffsetY2(AOffsetY2: Single);
    procedure SetSprite(ASprite: TsgeSprite);

    procedure FillData(X, Y, Width, Height: Single; Offset: TsgeFloatRect);

  protected
   procedure ResetChangeSet; override;
   function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Offset: TsgeFloatRect);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; Offset: TsgeFloatRect);

    function GetCopy: TsgeDisplayElement; override;

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
    property Offset: TsgeFloatRect read FData.Offset write SetOffset;
    property OffsetX1: Single read FData.Offset.X1 write SetOffsetX1;
    property OffsetY1: Single read FData.Offset.Y1 write SetOffsetY1;
    property OffsetX2: Single read FData.Offset.X2 write SetOffsetX2;
    property OffsetY2: Single read FData.Offset.Y2 write SetOffsetY2;
    property Sprite: TsgeSprite read FData.Sprite write SetSprite;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  _UNITNAME = 'DisplayElementSpriteNine';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementSpriteNine.FillData(X, Y, Width, Height: Single; Offset: TsgeFloatRect);
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
  FData.Offset := Offset;
  //FData.Reflect := [];
end;


procedure TsgeDisplayElementSpriteNine.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementSpriteNine.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementSpriteNine.Create;
begin
  //Заглушка
end;


procedure TsgeDisplayElementSpriteNine.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSpriteNine.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSpriteNine.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementSpriteNine.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSpriteNine.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSpriteNine.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementSpriteNine.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSpriteNine.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSpriteNine.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementSpriteNine.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, csAngle);
end;


procedure TsgeDisplayElementSpriteNine.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, csAngle);
end;


function TsgeDisplayElementSpriteNine.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementSpriteNine.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSpriteNine.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSpriteNine.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSpriteNine.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSpriteNine.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementSpriteNine.SetOffset(AOffset: TsgeFloatRect);
begin
  FData.Offset := AOffset;
  Include(FChangeSet, csOffset);
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetOffsetX1(AOffsetX1: Single);
begin
  FData.Offset.X1 := AOffsetX1;
  Include(FChangeSet, csOffset);
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetOffsetY1(AOffsetY1: Single);
begin
  FData.Offset.Y1 := AOffsetY1;
  Include(FChangeSet, csOffset);
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetOffsetX2(AOffsetX2: Single);
begin
  FData.Offset.X2 := AOffsetX2;
  Include(FChangeSet, csOffset);
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetOffsetY2(AOffsetY2: Single);
begin
  FData.Offset.Y2 := AOffsetY2;
  Include(FChangeSet, csOffset);
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementSpriteNine.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, csSprite);
end;


constructor TsgeDisplayElementSpriteNine.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Offset: TsgeFloatRect);
begin
  inherited Create;

  SetSprite(Sprite);
  FillData(X, Y, Width, Height, Offset);
end;


constructor TsgeDisplayElementSpriteNine.Create(X, Y: Single; Sprite: TsgeSprite; Offset: TsgeFloatRect);
begin
  inherited Create;

  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height, Offset);
end;


function TsgeDisplayElementSpriteNine.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementSpriteNine.Create;

  //Заполнить данные
  TsgeDisplayElementSpriteNine(Result).FData := Self.FData;
  TsgeDisplayElementSpriteNine(Result).FChangeSet := Self.FChangeSet;
end;



end.

