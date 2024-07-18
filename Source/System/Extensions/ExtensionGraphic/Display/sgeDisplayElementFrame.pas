{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementFrame.pas
Версия            1.3
Создан            11.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Цветная рамка
}
{$Include Defines.inc}

unit sgeDisplayElementFrame;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeColor,
  sgeDisplayElement;

type
  TsgeDisplayElementFrame = class(TsgeDisplayElement)
  public
    type
      //Типы измененных параметров
      TChange = (
        csPosition,     //Положение на экране
        csSize,         //Размеры элемента
        csScale,        //Масштаб
        csOrigin,       //Точка поворота
        csAngle,        //Угол в радианах
        csColor,        //Цвет
        csThickness,    //Толщина линии
        csStipple,      //Тип штриховки
        csStippleScale  //Масштаб штриховки
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
        Thickness: Single;        //Толщина линии
        Stipple: TsgeLineStipple; //Штриховка
        StippleScale: Word;       //Масштаб штриховки
      end;

  protected
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
    procedure SetThickness(AThickness: Single);
    procedure SetStipple(AStipple: TsgeLineStipple);
    procedure SetStippleScale(AStippleScale: Word);

  protected
    procedure ResetChangeSet; override;
    function  IsNeedUpdate: Boolean; override;
  public
    constructor Create;
    constructor Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Thickness: Single = 1);

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
    property Thickness: Single read FData.Thickness write SetThickness;
    property Stipple: TsgeLineStipple read FData.Stipple write SetStipple;
    property StippleScale: Word read FData.StippleScale write SetStippleScale;
  end;


implementation

uses
  sgeMathUtils;


procedure TsgeDisplayElementFrame.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementFrame.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementFrame.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, csPosition);
end;


procedure TsgeDisplayElementFrame.SetWidth(AWidth: Single);
begin
  FData.Size.X := AWidth;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementFrame.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, csSize);
end;


procedure TsgeDisplayElementFrame.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementFrame.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementFrame.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, csScale);
end;


procedure TsgeDisplayElementFrame.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementFrame.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementFrame.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, csOrigin);
end;


procedure TsgeDisplayElementFrame.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, csAngle);
end;


procedure TsgeDisplayElementFrame.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, csAngle);
end;


function TsgeDisplayElementFrame.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementFrame.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementFrame.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementFrame.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementFrame.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementFrame.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, csColor);
end;


procedure TsgeDisplayElementFrame.SetThickness(AThickness: Single);
begin
  FData.Thickness := AThickness;
  Include(FChangeSet, csThickness);
end;


procedure TsgeDisplayElementFrame.SetStipple(AStipple: TsgeLineStipple);
begin
  FData.Stipple := AStipple;
  Include(FChangeSet, csStipple);
end;


procedure TsgeDisplayElementFrame.SetStippleScale(AStippleScale: Word);
begin
  FData.StippleScale := AStippleScale;
  Include(FChangeSet, csStippleScale);
end;


procedure TsgeDisplayElementFrame.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementFrame.IsNeedUpdate: Boolean;
begin
  Result := FChangeSet <> [];
end;


constructor TsgeDisplayElementFrame.Create;
begin
  //Заглушка
end;


constructor TsgeDisplayElementFrame.Create(X1, Y1, X2, Y2: Single; Color: TsgeColor; Thickness: Single = 1);
var
  i: TChange;
begin
  inherited Create;

  //Заполнить набор изменений
  for i := Low(TChange) to High(TChange) do
    Include(FChangeSet, i);

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


function TsgeDisplayElementFrame.GetCopy: TsgeDisplayElement;
begin
  Result := TsgeDisplayElementFrame.Create;

  //Заполнить данные
  TsgeDisplayElementFrame(Result).FData := Self.FData;
  TsgeDisplayElementFrame(Result).FChangeSet := Self.FChangeSet;
end;



end.

