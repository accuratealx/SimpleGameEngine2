{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSpriteTile.pas
Версия            1.1
Создан            15.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Плитка спрайта
}
{$Include Defines.inc}

unit sgeDisplayElementItemSpriteTile;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeGraphicColor,
  sgeDisplayElementItemBase;

type
  //Набор измененных параметров
  TsgeDisplayElementItemSpriteTileChangeSet = set of (
    deistcsPosition,  //Положение на экране
    deistcsSize,      //Размеры элемента
    deistcsScale,     //Масштаб
    deistcsOrigin,    //Точка поворота
    deistcsAngle,     //Угол в радианах
    deistcsColor,     //Цвет
    deistcsTile,      //Координаты тайла
    deistcsSprite     //Спрайт
  );


  //Настройки отображения
  TsgeDisplayElementItemSptiteTileData = record
    Position: TsgeFloatPoint; //Положение на экране
    Size: TsgeFloatPoint;     //Размеры элемента
    Scale: TsgeFloatPoint;    //Масштаб
    Origin: TsgeFloatPoint;   //Точка поворота
    Angle: Single;            //Угол в радианах
    Color: TsgeColor;         //Цвет
    Column: Word;             //Номер столбца плитки
    Row: Word;                //Номер строки плитки
    Sprite: TsgeSprite;       //Спрайт
  end;


  TsgeDisplayElementItemSpriteTile = class(TsgeDisplayElementItemBase)
  private
    FData: TsgeDisplayElementItemSptiteTileData;
    FChangeSet: TsgeDisplayElementItemSpriteTileChangeSet;

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
    procedure SetColum(AColumn: Word);
    procedure SetRow(ARow: Word);
    procedure SetSprite(ASprite: TsgeSprite);

    procedure FillData(X, Y, Width, Height: Single; Column, Row: Word);
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Column, Row: Word);
    constructor Create(X, Y: Single; Sprite: TsgeSprite; Column, Row: Word);

    procedure ResetChangeSet; override;
    function  GetCopy: TsgeDisplayElementItemBase; override;

    property Data: TsgeDisplayElementItemSptiteTileData read FData;
    property ChangeSet: TsgeDisplayElementItemSpriteTileChangeSet read FChangeSet;

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
    property Column: Word read FData.Column write SetColum;
    property Row: Word read FData.Row write SetRow;
    property Sprite: TsgeSprite read FData.Sprite write SetSprite;
  end;


implementation

uses
  sgeErrors, sgeMathUtils;

const
  sgeDisplayElementItemSpriteTileChangeSetAll = [
    deistcsPosition, deistcsSize, deistcsScale, deistcsOrigin, deistcsAngle, deistcsColor, deistcsTile, deistcsSprite
  ];

  _UNITNAME = 'DisplayElementItemSpriteTile';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemSpriteTile.FillData(X, Y, Width, Height: Single; Column, Row: Word);
begin
  FChangeSet := sgeDisplayElementItemSpriteTileChangeSetAll;

  //Записать параметры
  FData.Position := sgeGetFloatPoint(X, Y);
  FData.Size := sgeGetFloatPoint(Width, Height);
  FData.Scale := sgeGetFloatPoint(1, 1);
  FData.Origin := sgeGetFloatPoint(0, 0);
  FData.Angle := 0;
  FData.Color := cWhite;
  FData.Column := Column;
  FData.Row := Row;
end;


procedure TsgeDisplayElementItemSpriteTile.SetPosition(APosition: TsgeFloatPoint);
begin
  FData.Position := APosition;
  Include(FChangeSet, deistcsPosition);
end;


procedure TsgeDisplayElementItemSpriteTile.SetPositionX(APositionX: Single);
begin
  FData.Position.X := APositionX;
  Include(FChangeSet, deistcsPosition);
end;


procedure TsgeDisplayElementItemSpriteTile.SetPositionY(APositionY: Single);
begin
  FData.Position.Y := APositionY;
  Include(FChangeSet, deistcsPosition);
end;


procedure TsgeDisplayElementItemSpriteTile.SetWidth(AWidth: Single);
begin
  FData.Size.Y := AWidth;
  Include(FChangeSet, deistcsSize);
end;


procedure TsgeDisplayElementItemSpriteTile.SetHeight(AHeight: Single);
begin
  FData.Size.Y := AHeight;
  Include(FChangeSet, deistcsSize);
end;


procedure TsgeDisplayElementItemSpriteTile.SetScale(AScale: Single);
begin
  FData.Scale.X := AScale;
  FData.Scale.Y := AScale;
  Include(FChangeSet, deistcsScale);
end;


procedure TsgeDisplayElementItemSpriteTile.SetScaleX(AScaleX: Single);
begin
  FData.Scale.X := AScaleX;
  Include(FChangeSet, deistcsScale);
end;


procedure TsgeDisplayElementItemSpriteTile.SetScaleY(AScaleY: Single);
begin
  FData.Scale.Y := AScaleY;
  Include(FChangeSet, deistcsScale);
end;


procedure TsgeDisplayElementItemSpriteTile.SetOrigin(AOrigin: TsgeFloatPoint);
begin
  FData.Origin := AOrigin;
  Include(FChangeSet, deistcsOrigin);
end;


procedure TsgeDisplayElementItemSpriteTile.SetOriginX(AOriginX: Single);
begin
  FData.Origin.X := AOriginX;
  Include(FChangeSet, deistcsOrigin);
end;


procedure TsgeDisplayElementItemSpriteTile.SetOriginY(AOriginY: Single);
begin
  FData.Origin.Y := AOriginY;
  Include(FChangeSet, deistcsOrigin);
end;


procedure TsgeDisplayElementItemSpriteTile.SetAngle(AAngle: Single);
begin
  FData.Angle := AAngle;
  Include(FChangeSet, deistcsAngle);
end;


procedure TsgeDisplayElementItemSpriteTile.SetAngleDegree(AAngle: Single);
begin
  FData.Angle := sgeDegToRad(AAngle);
  Include(FChangeSet, deistcsAngle);
end;


function TsgeDisplayElementItemSpriteTile.GetAngleDegree: Single;
begin
  Result := sgeRadToDeg(FData.Angle);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColor(AColor: TsgeColor);
begin
  sgeFitToRange(AColor.Red);
  sgeFitToRange(AColor.Green);
  sgeFitToRange(AColor.Blue);
  sgeFitToRange(AColor.Alpha);
  FData.Color := AColor;
  Include(FChangeSet, deistcsColor);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColorRed(ARed: Single);
begin
  sgeFitToRange(ARed);
  FData.Color.Red := ARed;
  Include(FChangeSet, deistcsColor);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColorGreen(AGreen: Single);
begin
  sgeFitToRange(AGreen);
  FData.Color.Green := AGreen;
  Include(FChangeSet, deistcsColor);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColorBlue(ABlue: Single);
begin
  sgeFitToRange(ABlue);
  FData.Color.Blue := ABlue;
  Include(FChangeSet, deistcsColor);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColorAlpha(AAlpha: Single);
begin
  sgeFitToRange(AAlpha);
  FData.Color.Alpha := AAlpha;
  Include(FChangeSet, deistcsColor);
end;


procedure TsgeDisplayElementItemSpriteTile.SetColum(AColumn: Word);
begin
  FData.Column := AColumn;
  Include(FChangeSet, deistcsTile);
end;


procedure TsgeDisplayElementItemSpriteTile.SetRow(ARow: Word);
begin
  FData.Row := ARow;
  Include(FChangeSet, deistcsTile);
end;


procedure TsgeDisplayElementItemSpriteTile.SetSprite(ASprite: TsgeSprite);
begin
  //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  Include(FChangeSet, deistcsSprite);
  FData.Sprite := ASprite;
end;


constructor TsgeDisplayElementItemSpriteTile.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite; Column, Row: Word);
begin
  SetSprite(Sprite);
  FillData(X, Y, Width, Height, Column, Row);
end;


constructor TsgeDisplayElementItemSpriteTile.Create(X, Y: Single; Sprite: TsgeSprite; Column, Row: Word);
begin
  SetSprite(Sprite);
  FillData(X, Y, Sprite.Width, Sprite.Height, Column, Row);
end;


procedure TsgeDisplayElementItemSpriteTile.ResetChangeSet;
begin
  FChangeSet := [];
end;


function TsgeDisplayElementItemSpriteTile.GetCopy: TsgeDisplayElementItemBase;
begin
  Result := TsgeDisplayElementItemSpriteTile.Create(0, 0, 0, 0, Self.FData.Sprite, 0, 0);
  TsgeDisplayElementItemSpriteTile(Result).FData := Self.FData;
end;



end.


