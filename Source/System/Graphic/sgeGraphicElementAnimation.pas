{
Пакет             Simple Game Engine 2
Файл              *.pas
Версия            1.0
Создан            26.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс элемента отрисовки: Анимация
}
{$Include Defines.inc}

unit sgeGraphicElementAnimation;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicElementBase,
  sgeGraphic, sgeGraphicColor, sgeGraphicAnimation, sgeGraphicAnimationFrames;


type
  //Настройки элемента
  TsgeGraphicElementAnimationData = TsgeGraphicDrawOptions;


  TsgeGraphicElementAnimation = class(TsgeGraphicElementBase)
  private
    FAnimation: TsgeGraphicAnimation;
    FData: TsgeGraphicElementAnimationData;
    FNewData: TsgeGraphicElementAnimationData;

    procedure SetAlpha(AAlpha: Single);
    function  GetAlpha: Single;
  public
    constructor Create(X, Y, W, H: Single; Frames: TsgeGraphicAnimationFrames; CoordType: TsgeGraphicCoordinateType = gctNormal);
    destructor  Destroy; override;

    procedure Reset;

    procedure UpdateData; override;
    procedure Draw(Graphic: TsgeGraphic); override;

    property Alpha: Single read GetAlpha write SetAlpha;
    property X: Single read FNewData.Rect.X1 write FNewData.Rect.X1;
    property Y: Single read FNewData.Rect.Y1 write FNewData.Rect.Y1;
    property W: Single read FNewData.Rect.X2 write FNewData.Rect.X2;
    property H: Single read FNewData.Rect.Y2 write FNewData.Rect.Y2;
    property Angle: Single read FNewData.Angle write FNewData.Angle;
    property Anglepoint: TsgeFloatPoint read FNewData.AnglePoint write FNewData.AnglePoint;
    property Scale: TsgeFloatPoint read FNewData.Scale write FNewData.Scale;
    property CoordType: TsgeGraphicCoordinateType read FNewData.CoordinateType write FNewData.CoordinateType;
    property TransparentColor: TsgeColor read FNewData.TransparentColor write FNewData.TransparentColor;
  	property Reflect: TsgeGraphicReflectTypes read FNewData.Reflect write FNewData.Reflect;
  end;


implementation

uses
  sgeErrors, sgeGraphicUtils;

const
  _UNITNAME = 'sgeGraphicElementAnimation';

  Err_FramesIsEmpty = 'FramesIsEmpty';
  Err_FramesNoSprite = 'FramesNoSprite';



procedure TsgeGraphicElementAnimation.SetAlpha(AAlpha: Single);
begin
  FNewData.TransparentColor := sgeChangeColorAlpha(FNewData.TransparentColor, AAlpha);
end;


function TsgeGraphicElementAnimation.GetAlpha: Single;
begin
  Result := FNewData.TransparentColor.Alpha;
end;


constructor TsgeGraphicElementAnimation.Create(X, Y, W, H: Single; Frames: TsgeGraphicAnimationFrames; CoordType: TsgeGraphicCoordinateType);
begin
  try
    //Проверить на пустой указатель кадров
    if Frames = nil then
      raise EsgeException.Create(_UNITNAME, Err_FramesIsEmpty);

    //Проверить на наличие кадров
    if Frames.Count = 0 then
      raise EsgeException.Create(_UNITNAME, Err_FramesNoSprite);

    //Создать анимацию
    FAnimation := TsgeGraphicAnimation.Create(Frames, 0, 0);

    //Подготовить начальные данные
    FData := DefaultDrawOptions;

    FData.CoordinateType := CoordType;
    FData.Rect.X1 := X;
    FData.Rect.Y1 := Y;
    FData.Rect.X2 := W;
    FData.Rect.Y2 := H;

    FNewData := FData;

  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateGraphicElement, '', E.Message);
  end;
end;


destructor TsgeGraphicElementAnimation.Destroy;
begin
  FAnimation.Free;
end;


procedure TsgeGraphicElementAnimation.Reset;
begin
  FAnimation.Reset;
end;


procedure TsgeGraphicElementAnimation.UpdateData;
begin
  FData := FNewData;
end;


procedure TsgeGraphicElementAnimation.Draw(Graphic: TsgeGraphic);
var
  Frame: TsgeGraphicAnimationFrame;
begin
  //Текущий кадр
  Frame := FAnimation.CurrentFrame;

  //Изменить спрайт
  FData.Sprite := Frame.Sprite;

  //Порпавить область вывода плитки
  FData.SpriteRect := sgeGetTextureTileRect(Frame.Sprite, Frame.Col, Frame.Row);

  //Вывод анимации
  Graphic.DrawSprite(FData);
end;



end.

