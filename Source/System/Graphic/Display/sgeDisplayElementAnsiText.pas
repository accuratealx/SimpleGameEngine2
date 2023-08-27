{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementAnsiText.pas
Версия            1.0
Создан            16.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Текст Ansi
}
{$Include Defines.inc}

unit sgeDisplayElementAnsiText;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeAnsiFont, sgeColor,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementAnsiTextChangeSet = set of (
    deatcsPosition, //Положение на экране
    deatcsScale,    //Масштаб
    deatcsOrigin,   //Точка поворота
    deatcsAngle,    //Угол в радианах
    deatcsColor,    //Цвет
    deatcsText,     //Текст
    deatcsFont      //Шрифт
  );


  //Настройки отображения
  TsgeDisplayElementAnsiTextData = record
    Position: TsgeFloatPoint; //Положение на экране
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
    TextBytes: TsgeByteArray; //Байты для вывода
    Font: TsgeAnsiFont;       //Шрифт
  end;

type
  TsgeDisplayElementAnsiText = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementAnsiTextData;
    FChangeSet: TsgeDisplayElementAnsiTextChangeSet;
    FText: String;

    procedure SetPosition(APosition: TsgeFloatPoint);
    procedure SetPositionX(APositionX: Single);
    procedure SetPositionY(APositionY: Single);
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
    procedure SetFont(AFont: TsgeAnsiFont);
    procedure SetText(AText: String);

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X, Y: Single; Color: TsgeColor; Font: TsgeAnsiFont; Text: String);
    destructor  Destroy; override;

    function  GetCopy: TsgeDisplayElement; override;

    property Data: TsgeDisplayElementAnsiTextData read FData;
    property ChangeSet: TsgeDisplayElementAnsiTextChangeSet read FChangeSet;

    property Position: TsgeFloatPoint read FData.Position write SetPosition;
    property PositionX: Single read FData.Position.X write SetPositionX;
    property PositionY: Single read FData.Position.Y write SetPositionY;
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
    property Font: TsgeAnsiFont read FData.Font write SetFont;
    property Text: String read FText write SetText;
  end;


implementation

uses
  sgeErrors, sgeMathUtils, sgeOSPlatform;

const
  _UNITNAME = 'DisplayElementAnsiText';

  Err_EmptyFont = 'EmptyFont';


procedure TsgeDisplayElementAnsiText.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deatcsPosition);
end;


procedure TsgeDisplayElementAnsiText.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deatcsPosition);
end;


procedure TsgeDisplayElementAnsiText.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deatcsPosition);
end;


procedure TsgeDisplayElementAnsiText.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deatcsScale);
end;


procedure TsgeDisplayElementAnsiText.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deatcsScale);
end;


procedure TsgeDisplayElementAnsiText.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deatcsScale);
end;


procedure TsgeDisplayElementAnsiText.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deatcsOrigin);
end;


procedure TsgeDisplayElementAnsiText.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deatcsOrigin);
end;


procedure TsgeDisplayElementAnsiText.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deatcsOrigin);
end;


procedure TsgeDisplayElementAnsiText.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deatcsAngle);
end;


procedure TsgeDisplayElementAnsiText.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deatcsAngle);
end;


function TsgeDisplayElementAnsiText.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementAnsiText.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deatcsColor);
end;


procedure TsgeDisplayElementAnsiText.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deatcsColor);
end;


procedure TsgeDisplayElementAnsiText.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deatcsColor);
end;


procedure TsgeDisplayElementAnsiText.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deatcsColor);
end;


procedure TsgeDisplayElementAnsiText.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deatcsColor);
end;


procedure TsgeDisplayElementAnsiText.SetFont(AFont: TsgeAnsiFont);
begin
  //Проверить спрайт
  if AFont = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFont);

  FData.Font := AFont;
  Include(FChangeSet, deatcsFont);
end;


procedure TsgeDisplayElementAnsiText.SetText(AText: String);
begin
  if FText = AText then
    Exit;

  //Запомнить текст
  FText := AText;

  //Шрифт не понимает utf-8, только Ansi. Поэтому переведем тут
  FData.TextBytes := sgeUtf8ToAnsiBytes(FText);
  Include(FChangeSet, deatcsText);
end;


procedure TsgeDisplayElementAnsiText.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementAnsiText.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementAnsiText.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementAnsiText.Create(X, Y: Single; Color: TsgeColor; Font: TsgeAnsiFont; Text: String);
const
  SetAll = [deatcsPosition, deatcsScale, deatcsOrigin, deatcsAngle, deatcsColor, deatcsText, deatcsFont];
begin
  inherited Create;

  FChangeSet := SetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;

  SetFont(Font);
  SetText(Text);
end;


destructor TsgeDisplayElementAnsiText.Destroy;
begin
  SetLength(FData.TextBytes, 0);
end;


function TsgeDisplayElementAnsiText.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementAnsiText.Create;

  //Заполнить данные
  TsgeDisplayElementAnsiText(Result).FData := Self.FData;
  TsgeDisplayElementAnsiText(Result).FChangeSet := Self.FChangeSet;
end;



end.


