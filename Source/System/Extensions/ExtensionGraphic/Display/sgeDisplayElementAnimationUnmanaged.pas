{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementAnimationUnmanaged.pas
Версия            1.2
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
  sgeTypes, sgeSprite, sgeColor, sgeAnimationFrameList,
  sgeDisplayElement;

type
  TsgeDisplayElementAnimationUnmanaged = class(TsgeDisplayElement)
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
        csReflect,  //Отражение
        csFrames    //Кадры анимации
      );

      //Набор измененных параметров
      TChangeSet = set of TChange;

      //Настройки отображения
      TData = record
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
    procedure SetFrameList(AFrameList: TsgeAnimationFrameList);

    procedure FillData(X, Y, Width, Height: Single);

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);

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
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, csAngle);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, csAngle);
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
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FData.Sprite := ASprite;
  Include(FChangeSet, csSprite);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetReflect(AReflect: TsgeReflectSet);
begin
  if FData.Reflect = AReflect then
    Exit;

  FData.Reflect := AReflect;
  Include(FChangeSet, csReflect);
end;


procedure TsgeDisplayElementAnimationUnmanaged.SetFrameList(AFrameList: TsgeAnimationFrameList);
begin
  //Проверить кадры анимации
  if AFrameList = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyAnimationFrames);

  //Сохранить указатель на кадры
  FData.Frames := AFrameList;
  Include(FChangeSet, csFrames);
end;


procedure TsgeDisplayElementAnimationUnmanaged.FillData(X, Y, Width, Height: Single);
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


constructor TsgeDisplayElementAnimationUnmanaged.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementAnimationUnmanaged.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; AnimationFrames: TsgeAnimationFrameList);
begin
  inherited Create;

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

