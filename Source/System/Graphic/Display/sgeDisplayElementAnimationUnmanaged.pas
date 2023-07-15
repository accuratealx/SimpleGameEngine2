{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementAnimationUnmanaged.pas
Версия            1.1
Создан            15.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Неуправляемая анимация
}
{$Include Defines.inc}

unit sgeDisplayElementAnimationUnmanaged;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor, sgeAnimationFrameList,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementAnimationUnmanagedChangeSet = set of (
    deaucsPosition,    //Положение на экране
    deaucsSize,        //Размеры элемента
    deaucsScale,       //Масштаб
    deaucsOrigin,      //Точка поворота
    deaucsAngle,       //Угол в радианах
    deaucsColor,       //Цвет
    deaucsSprite,      //Спрайт
    deaucsReflect,     //Отражение
    deaucsFrames       //Кадры анимации
  );


  //Настройки отображения
  TsgeDisplayElementAnimationUnmanagedData = record
    Position: TsgeFloatPoint;       //Положение на экране
    Size: TsgeFloatPoint;           //Размеры элемента
    Scale: TsgeFloatPoint;          //Масштаб
    Origin: TsgeFloatPoint;         //Точка поворота
    Angle: Single;                  //Угол в радианах
    Color: TsgeColor;               //Цвет
    Sprite: TsgeSprite;             //Спрайт
    Reflect: TsgeReflectSet;        //Отражение
    Frames: TsgeAnimationFrameList; //Список кадров анимации
  end;


  TsgeDisplayElementAnimationUnmanaged = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementAnimationUnmanagedData;
    FChangeSet: TsgeDisplayElementAnimationUnmanagedChangeSet;

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
    procedure SetFrameList(AFrameList: TsgeAnimationFrameList);

    procedure FillData(X, Y, Width, Height: Single);

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);

    function GetCopy: TsgeDisplayElement; override;

    property Data: TsgeDisplayElementAnimationUnmanagedData read FData;
    property ChangeSet: TsgeDisplayElementAnimationUnmanagedChangeSet read FChangeSet;

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
    property Frames: TsgeAnimationFrameList read FData.Frames write SetFrameList;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  _UNITNAME = 'DisplayElementAnimation';

  Err_EmptySprite = 'EmptySprite';
  Err_EmptyAnimationFrames = 'EmptyAnimationFrames';


procedure TsgeDisplayElementAnimationUnmanaged.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementAnimationUnmanaged.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deaucsPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deaucsPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deaucsPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, deaucsSize);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deaucsSize);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deaucsScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deaucsScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deaucsScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deaucsOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deaucsOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deaucsOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deaucsAngle);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deaucsAngle);
end;


function TsgeDisplayElementAnimationUnmanaged.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deaucsColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deaucsColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deaucsColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deaucsColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deaucsColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, deaucsSprite);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetReflect(AReflect: TsgeReflectSet);
begin
  if FData.Reflect = AReflect then
    Exit;

  FData.Reflect := AReflect;
  Include(FChangeSet, deaucsReflect);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetFrameList(AFrameList: TsgeAnimationFrameList);
begin
  //Проверить кадры анимации
  if AFrameList = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyAnimationFrames);

  //Сохранить указатель на кадры
  FData.Frames := AFrameList;
  Include(FChangeSet, deaucsFrames);
end;


procedure TsgeDisplayElementAnimationUnmanaged.FillData(X, Y, Width, Height: Single);
const
  SetAll = [deaucsPosition, deaucsSize, deaucsScale, deaucsOrigin, deaucsAngle, deaucsColor,
    deaucsSprite, deaucsReflect, deaucsFrames];
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


constructor TsgeDisplayElementAnimationUnmanaged.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementAnimationUnmanaged.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
begin
  //Установить кадры анимации
  SetFrameList(AnimationFrames);

  //Установить спрайт
  SetSprite(Sprite);

  //Заполнить остальные данные
  FillData(X, Y, Width, Height);
end;


function TsgeDisplayElementAnimationUnmanaged.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementAnimationUnmanaged.Create;

  //Заполнить данные
  TsgeDisplayElementAnimationUnmanaged(Result).FData := Self.FData;
  TsgeDisplayElementAnimationUnmanaged(Result).FChangeSet := Self.FChangeSet;
end;



end.

