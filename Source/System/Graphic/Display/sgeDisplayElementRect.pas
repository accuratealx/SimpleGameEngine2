{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementRect.pas
Версия            1.2
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент отображения: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeGraphicColor,
  sgeDisplayElement;

type
  //Набор измененных параметров
  TsgeDisplayElementRectChangeSet = set of (
    dercsPosition,  //Положение на экране
    dercsSize,      //Размеры элемента
    dercsScale,     //Масштаб
    dercsOrigin,    //Точка поворота
    dercsAngle,     //Угол в радианах
    dercsColor      //Цвет
  );


  //Настройки отображения
  TsgeDisplayElementRectData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
  end;


  //Элемент отображения
  TsgeDisplayElementRect = class(TsgeDisplayElement)
  private
    FData: TsgeDisplayElementRectData;
    FChangeSet: TsgeDisplayElementRectChangeSet;

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

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);

    function GetCopy: TsgeDisplayElement; override;

    property Data: TsgeDisplayElementRectData read FData;
    property ChangeSet: TsgeDisplayElementRectChangeSet read FChangeSet;

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
  end;


implementation

uses
  sgeMathUtils;


procedure TsgeDisplayElementRect.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, dercsPosition);
end;

procedure TsgeDisplayElementRect.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, dercsPosition);
end;


procedure TsgeDisplayElementRect.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, dercsPosition);
end;


procedure TsgeDisplayElementRect.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, dercsSize);
end;


procedure TsgeDisplayElementRect.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, dercsSize);
end;


procedure TsgeDisplayElementRect.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, dercsScale);
end;


procedure TsgeDisplayElementRect.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, dercsScale);
end;


procedure TsgeDisplayElementRect.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, dercsScale);
end;


procedure TsgeDisplayElementRect.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, dercsOrigin);
end;


procedure TsgeDisplayElementRect.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, dercsOrigin);
end;


procedure TsgeDisplayElementRect.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, dercsOrigin);
end;


procedure TsgeDisplayElementRect.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, dercsAngle);
end;


procedure TsgeDisplayElementRect.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, dercsAngle);
end;


function TsgeDisplayElementRect.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementRect.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, dercsColor);
end;


procedure TsgeDisplayElementRect.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, dercsColor);
end;


procedure TsgeDisplayElementRect.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, dercsColor);
end;


procedure TsgeDisplayElementRect.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, dercsColor);
end;


procedure TsgeDisplayElementRect.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, dercsColor);
end;


procedure TsgeDisplayElementRect.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementRect.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementRect.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementRect.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);
const
  SetAll = [dercsPosition, dercsSize, dercsScale, dercsOrigin, dercsAngle, dercsColor];
begin
  inherited Create;

  FChangeSet := SetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X1, Y1);
  FData.Size := sgeGetFloatPoint(X2, Y2);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := Color;
end;


function TsgeDisplayElementRect.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementRect.Create;

  //Заполнить данные
  TsgeDisplayElementRect(Result).FData := Self.FData;
  TsgeDisplayElementRect(Result).FChangeSet := Self.FChangeSet;
end;



end.

