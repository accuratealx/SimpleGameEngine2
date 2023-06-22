{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemRect.pas
Версия            1.1
Создан            27.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент отображения: Цветной прямоугольник
}
{$Include Defines.inc}

unit sgeDisplayElementItemRect;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  //Набор измененных параметров
  TsgeDisplayElementItemRectChangeSet = set of (
    deircsPosition, //Положение на экране
    deircsSize,     //Размеры элемента
    deircsScale,    //Масштаб
    deircsOrigin,   //Точка поворота
    deircsAngle,    //Угол в радианах
    deircsColor     //Цвет
  );


  //Настройки отображения
  TsgeDisplayElementItemRectData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
  end;


  //Элемент отображения
  TsgeDisplayElementItemRect = class(TsgeDisplayElementItemBase)
  private
    FData: TsgeDisplayElementItemRectData;
    FChangeSet: TsgeDisplayElementItemRectChangeSet;

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
  public
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);

    procedure ResetChangeSet; override;
    function  GetCopy: TsgeDisplayElementItemBase; override;

    property Data: TsgeDisplayElementItemRectData read FData;
    property ChangeSet: TsgeDisplayElementItemRectChangeSet read FChangeSet;

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

const
  sgeDisplayElementItemRectChangeSetAll = [deircsPosition, deircsSize, deircsScale, deircsOrigin, deircsAngle, deircsColor];


procedure TsgeDisplayElementItemRect.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deircsPosition);
end;

procedure TsgeDisplayElementItemRect.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deircsPosition);
end;


procedure TsgeDisplayElementItemRect.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deircsPosition);
end;


procedure TsgeDisplayElementItemRect.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deircsSize);
end;


procedure TsgeDisplayElementItemRect.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deircsSize);
end;


procedure TsgeDisplayElementItemRect.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deircsScale);
end;


procedure TsgeDisplayElementItemRect.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deircsScale);
end;


procedure TsgeDisplayElementItemRect.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deircsScale);
end;


procedure TsgeDisplayElementItemRect.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deircsOrigin);
end;


procedure TsgeDisplayElementItemRect.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deircsOrigin);
end;


procedure TsgeDisplayElementItemRect.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deircsOrigin);
end;


procedure TsgeDisplayElementItemRect.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deircsAngle);
end;


procedure TsgeDisplayElementItemRect.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deircsAngle);
end;


function TsgeDisplayElementItemRect.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementItemRect.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deircsColor);
end;


procedure TsgeDisplayElementItemRect.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deircsColor);
end;


procedure TsgeDisplayElementItemRect.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deircsColor);
end;


procedure TsgeDisplayElementItemRect.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deircsColor);
end;


procedure TsgeDisplayElementItemRect.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deircsColor);
end;


constructor TsgeDisplayElementItemRect.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor);
begin
  FChangeSet := sgeDisplayElementItemRectChangeSetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X1, Y1);
  FData.Size := sgeGetFloatPoint(X2, Y2);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := Color;
end;


procedure TsgeDisplayElementItemRect.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementItemRect.GetCopy: TsgeDisplayElementItemBase;
begin
  Result := TsgeDisplayElementItemRect.Create(0, 0, 0, 0, cBlack);
  TsgeDisplayElementItemRect(Result).FData := Self.FData;
end;



end.

