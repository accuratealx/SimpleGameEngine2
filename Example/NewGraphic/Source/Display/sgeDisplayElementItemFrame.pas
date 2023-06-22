{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemFrame.pas
Версия            1.1
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Цветная рамка
}
{$Include Defines.inc}

unit sgeDisplayElementItemFrame;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  //Набор измененных параметров
  TsgeDisplayElementItemFrameChangeSet = set of (
    deifcsPosition,     //Положение на экране
    deifcsSize,         //Размеры элемента
    deifcsScale,        //Масштаб
    deifcsOrigin,       //Точка поворота
    deifcsAngle,        //Угол в радианах
    deifcsColor,        //Цвет
    deifcsThickness,    //Толщина линии
    deifcsStipple,      //Тип штриховки
    deifcsStippleScale  //Масштаб штриховки
  );


  //Настройки отображения
  TsgeDisplayElementItemFrameData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
    Thickness: Single;        //Толщина линии
    Stipple: TsgeLineStipple; //Штриховка
    StippleScale: Word;       //Масштаб штриховки
  end;


  TsgeDisplayElementItemFrame = class(TsgeDisplayElementItemBase)
  protected
    FData: TsgeDisplayElementItemFrameData;
    FChangeSet: TsgeDisplayElementItemFrameChangeSet;

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
    procedure SetThickness(AThickness: Single);
    procedure SetStipple(AStipple: TsgeLineStipple);
    procedure SetStippleScale(AStippleScale: Word);
  public
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Thickness: Single = 1);

    procedure ResetChangeSet; override;
    function  GetCopy: TsgeDisplayElementItemBase; override;

    property Data: TsgeDisplayElementItemFrameData read FData;
    property ChangeSet: TsgeDisplayElementItemFrameChangeSet read FChangeSet;

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
    property Thickness: Single read FData.Thickness write SetThickness;
    property Stipple: TsgeLineStipple read FData.Stipple write SetStipple;
    property StippleScale: Word read FData.StippleScale write SetStippleScale;
  end;


implementation

uses
  sgeMathUtils;

const
    sgeDisplayElementItemFrameChangeSetAll = [
    deifcsPosition, deifcsSize, deifcsScale, deifcsOrigin, deifcsAngle, deifcsColor, deifcsThickness,
    deifcsStipple, deifcsStippleScale
  ];


procedure TsgeDisplayElementItemFrame.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deifcsPosition);
end;


procedure TsgeDisplayElementItemFrame.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deifcsPosition);
end;


procedure TsgeDisplayElementItemFrame.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deifcsPosition);
end;


procedure TsgeDisplayElementItemFrame.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deifcsSize);
end;


procedure TsgeDisplayElementItemFrame.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deifcsSize);
end;


procedure TsgeDisplayElementItemFrame.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deifcsScale);
end;


procedure TsgeDisplayElementItemFrame.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deifcsScale);
end;


procedure TsgeDisplayElementItemFrame.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deifcsScale);
end;


procedure TsgeDisplayElementItemFrame.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deifcsOrigin);
end;


procedure TsgeDisplayElementItemFrame.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deifcsOrigin);
end;


procedure TsgeDisplayElementItemFrame.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deifcsOrigin);
end;


procedure TsgeDisplayElementItemFrame.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deifcsAngle);
end;


procedure TsgeDisplayElementItemFrame.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deifcsAngle);
end;


function TsgeDisplayElementItemFrame.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementItemFrame.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deifcsColor);
end;


procedure TsgeDisplayElementItemFrame.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deifcsColor);
end;


procedure TsgeDisplayElementItemFrame.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deifcsColor);
end;


procedure TsgeDisplayElementItemFrame.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deifcsColor);
end;


procedure TsgeDisplayElementItemFrame.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deifcsColor);
end;


procedure TsgeDisplayElementItemFrame.SetThickness(AThickness: Single);
begin
  FData.Thickness := AThickness;
  Include(FChangeSet, deifcsThickness);
end;


procedure TsgeDisplayElementItemFrame.SetStipple(AStipple: TsgeLineStipple);
begin
  FData.Stipple := AStipple;
  Include(FChangeSet, deifcsStipple);
end;


procedure TsgeDisplayElementItemFrame.SetStippleScale(AStippleScale: Word);
begin
  FData.StippleScale := AStippleScale;
  Include(FChangeSet, deifcsStippleScale);
end;


constructor TsgeDisplayElementItemFrame.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Thickness: Single = 1);
begin
  FChangeSet := sgeDisplayElementItemFrameChangeSetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X1, Y1);
  FData.Size := sgeGetFloatPoint(X2, Y2);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := Color;
  FData.Thickness := Thickness;
  FData.Stipple := lsSolid;
  FData.StippleScale := 1;
end;


procedure TsgeDisplayElementItemFrame.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementItemFrame.GetCopy: TsgeDisplayElementItemBase;
begin
  Result := TsgeDisplayElementItemFrame.Create(0, 0, 0, 0, cBlack);
  TsgeDisplayElementItemFrame(Result).FData := Self.FData;
end;



end.

