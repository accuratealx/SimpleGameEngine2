{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementAnimation.pas
Версия            1.2
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Анимация
}
{$Include Defines.inc}

unit sgeDisplayElementAnimation;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor, sgeAnimation, sgeAnimationFrameList,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementAnimationChangeSet = set of (
    deacsPosition,    //Положение на экране
    deacsSize,        //Размеры элемента
    deacsScale,       //Масштаб
    deacsOrigin,      //Точка поворота
    deacsAngle,       //Угол в радианах
    deacsColor,       //Цвет
    deacsFrameIndex,  //Номер кадра
    deacsSprite,      //Спрайт
    deacsFrames       //Кадры анимации
  );


  //Настройки отображения
  TsgeDisplayElementAnimationData = record
    Position: TsgeFloatPoint;       //Положение на экране
    Size: TsgeFloatPoint;           //Размеры элемента
    Scale: TsgeFloatPoint;          //Масштаб
    Origin: TsgeFloatPoint;         //Точка поворота
    Angle: Single;                  //Угол в радианах
    Color: TsgeColor;               //Цвет
    FrameIndex: Word;               //Номер кадра
    Sprite: TsgeSprite;             //Спрайт
    Frames: TsgeAnimationFrameList; //Список кадров анимации
  end;


  TsgeDisplayElementAnimation = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementAnimationData;
    FChangeSet: TsgeDisplayElementAnimationChangeSet;
    FAnimation: TsgeAnimation;

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
    procedure SetFrameIndex(AFrameIndex: Word);
    procedure SetSprite(ASprite: TsgeSprite);

    procedure FillData(X, Y, Width, Height: Single);

  protected
    procedure ResetChangeSet; override;

  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
    destructor  Destroy; override;

    function GetCopy: TsgeDisplayElement; override;

    procedure UpdateAnimation;

    property Data: TsgeDisplayElementAnimationData read FData;
    property ChangeSet: TsgeDisplayElementAnimationChangeSet read FChangeSet;

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
    property FrameIndex: Word read FData.FrameIndex write SetFrameIndex;
    property Sprite: TsgeSprite read FData.Sprite write SetSprite;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  _UNITNAME = 'DisplayElementAnimation';

  Err_EmptySprite = 'EmptySprite';
  Err_EmptyAnimationFrames = 'EmptyAnimationFrames';


procedure TsgeDisplayElementAnimation.FillData(X, Y, Width, Height: Single);
const
  SetAll = [deacsPosition, deacsSize, deacsScale, deacsOrigin, deacsAngle, deacsColor, deacsFrameIndex, deacsSprite, deacsFrames];
begin
  FChangeSet := SetAll;

  //Сгенерировать уникальный ID
  SetUniqueID;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Size := sgeGetFloatPoint(Width, Height);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;
  FData.FrameIndex := 0;
end;


procedure TsgeDisplayElementAnimation.ResetChangeSet;
begin
  FChangeSet := [];
end;


procedure TsgeDisplayElementAnimation.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deacsPosition);
end;


procedure TsgeDisplayElementAnimation.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deacsPosition);
end;


procedure TsgeDisplayElementAnimation.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deacsPosition);
end;


procedure TsgeDisplayElementAnimation.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deacsSize);
end;


procedure TsgeDisplayElementAnimation.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deacsSize);
end;


procedure TsgeDisplayElementAnimation.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deacsScale);
end;


procedure TsgeDisplayElementAnimation.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deacsScale);
end;


procedure TsgeDisplayElementAnimation.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deacsScale);
end;


procedure TsgeDisplayElementAnimation.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deacsOrigin);
end;


procedure TsgeDisplayElementAnimation.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deacsOrigin);
end;


procedure TsgeDisplayElementAnimation.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deacsOrigin);
end;


procedure TsgeDisplayElementAnimation.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deacsAngle);
end;


procedure TsgeDisplayElementAnimation.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deacsAngle);
end;


function TsgeDisplayElementAnimation.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementAnimation.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deacsColor);
end;


procedure TsgeDisplayElementAnimation.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deacsColor);
end;


procedure TsgeDisplayElementAnimation.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deacsColor);
end;


procedure TsgeDisplayElementAnimation.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deacsColor);
end;


procedure TsgeDisplayElementAnimation.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deacsColor);
end;


procedure TsgeDisplayElementAnimation.SetFrameIndex(AFrameIndex: Word);
begin
  FData.FrameIndex := AFrameIndex;
  Include(FChangeSet, deacsFrameIndex);
end;


procedure TsgeDisplayElementAnimation.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, deacsSprite);
end;


constructor TsgeDisplayElementAnimation.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementAnimation.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
begin
  //Проверить кадры анимации
  if AnimationFrames = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyAnimationFrames);

  //Сохранить указатель на кадры
  FData.Frames := AnimationFrames;

  //Создать анимацию из кадров
  FAnimation := TsgeAnimation.Create(FData.Frames);

  //Установить спрайт
  SetSprite(Sprite);

  //Заполнить остальные данные
  FillData(X, Y, Width, Height);
end;


destructor TsgeDisplayElementAnimation.Destroy;
begin
  FAnimation.Free;
end;


function TsgeDisplayElementAnimation.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementAnimation.Create;

  //Заполнить данные
  TsgeDisplayElementAnimation(Result).FData := Self.FData;
  TsgeDisplayElementAnimation(Result).FChangeSet := Self.FChangeSet;
end;


procedure TsgeDisplayElementAnimation.UpdateAnimation;
begin
  //Проверить не настал ли момент смены кадра
  if FAnimation.IsFrameChanged then
  begin
    //Обновить параметр
    SetFrameIndex(FAnimation.CurrentFrameIndex);

    //Послать события изменения
    Modify;
  end;
end;



end.

