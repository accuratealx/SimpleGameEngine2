{
Пакет             Simple Game Engine 2
Файл              sgeViewBox.pas
Версия            1.4
Создан            11.11.2018
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расчёта координат вывода карты с масштабом
}
{$Include Defines.inc}

unit sgeViewBox;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes;


type
  //Тип масштабирования (Свободный, ограниченный)
  TsgeViewBoxScaleType = (vbstFree, vbstDependent);


  TsgeViewBox = class
  private
    FScaleType: TsgeViewBoxScaleType;                       //Тип масштабирования
    FScale: Single;                                         //Масштаб
    FMinScale: Single;                                      //Нижняя граница масштаба
    FMaxScale: Single;                                      //Верхняя граница масштаба

    FMapSize: TsgeFloatPoint;                               //Размеры карты

    FScreenOffset: TsgeFloatPoint;                          //Смещение левого-верхнего угла экрана относительно левого-вехнего угла карты
    FScreenCenter: TsgeFloatPoint;                          //Координаты центра экрана относительно карты

    FScreenSize: TsgeFloatPoint;                            //Размеры экрана

    procedure SetScale(AScale: Single);
    procedure SetMinScale(AMin: Single);
    procedure SetMaxScale(AMax: Single);
    procedure SetScaleType(AType: TsgeViewBoxScaleType);
    procedure SetMapWidth(AWidth: Single);
    procedure SetMapHeight(AHeight: Single);

    procedure SetMapSize(ASize: TsgeFloatPoint);

    procedure SetScreenOffsetX(AX: Single);
    procedure SetScreenOffsetY(AY: Single);
    procedure SetScreenOffset(AOffset: TsgeFloatPoint);

    procedure SetScreenCenterX(AX: Single);
    procedure SetScreenCenterY(AY: Single);
    procedure SetScreenCenter(ACenter: TsgeFloatPoint);

    procedure SetScreenWidth(AWidth: Single);
    procedure SetScreenHeight(AHeight: Single);
    procedure SetScreenSize(ASize: TsgeFloatPoint);

    procedure ChangeScale(AScale: Single; Xc, Yc: Single);
    function  GetMapPosByScreenPointX(AX: Single; Safe: Boolean = True): Single;
    function  GetMapPosByScreenPointY(AY: Single; Safe: Boolean = True): Single;
  public
    constructor Create(MapWidth, MapHeight, ScreenWidth, ScreenHeight: Single; ScaleType: TsgeViewBoxScaleType = vbstFree; Scale: Single = 1; MinScale: Single = 0; MaxScale: Single = 2);

    procedure SetScaledCenterPos(Scale: Single; X, Y: Single);
    function  GetMapPosByScreenPoint(X, Y: Single; Safe: Boolean = True): TsgeFloatPoint;
    function  GetMapBounds: TsgeFloatRect;

    property ScaleType: TsgeViewBoxScaleType read FScaleType write SetScaleType;
    property Scale: Single read FScale write SetScale;
    property MinScale: Single read FMinScale write SetMinScale;
    property MaxScale: Single read FMaxScale write SetMaxScale;
    property MapSize: TsgeFloatPoint read FMapSize write SetMapSize;
    property MapWidth: Single read FMapSize.X write SetMapWidth;
    property MapHeight: Single read FMapSize.Y write SetMapHeight;

    property ScreenOffset: TsgeFloatPoint read FScreenOffset write SetScreenOffset;
    property ScreenOffsetX: Single read FScreenOffset.X write SetScreenOffsetX;
    property ScreenOffsetY: Single read FScreenOffset.Y write SetScreenOffsetY;

    property ScreenCenter: TsgeFloatPoint read FScreenCenter write SetScreenCenter;
    property ScreenCenterX: Single read FScreenCenter.X write SetScreenCenterX;
    property ScreenCenterY: Single read FScreenCenter.Y write SetScreenCenterY;

    property ScreenSize: TsgeFloatPoint read FScreenSize write SetScreenSize;
    property ScreenWidth: Single read FScreenSize.X write SetScreenWidth;
    property ScreenHeight: Single read FScreenSize.Y write SetScreenHeight;
  end;



implementation

uses
  Math;


procedure TsgeViewBox.SetScale(AScale: Single);
begin
  ChangeScale(AScale, FScreenCenter.X, FScreenCenter.Y);
end;


procedure TsgeViewBox.SetMinScale(AMin: Single);
begin
  FMinScale := AMin;
  ChangeScale(FScale, FScreenCenter.X, FScreenCenter.Y);
end;


procedure TsgeViewBox.SetMaxScale(AMax: Single);
begin
  FMaxScale := AMax;
  ChangeScale(FScale, FScreenCenter.X, FScreenCenter.Y);
end;


procedure TsgeViewBox.SetScaleType(AType: TsgeViewBoxScaleType);
begin
  if FScaleType = AType then
    Exit;

  FScaleType := AType;
  ChangeScale(FScale, FScreenCenter.X, FScreenCenter.Y);
end;


procedure TsgeViewBox.SetMapWidth(AWidth: Single);
begin
  if AWidth < 1 then
    AWidth := 1;
  FMapSize.X := AWidth;
  SetScreenOffsetX(FScreenOffset.X);
end;


procedure TsgeViewBox.SetMapHeight(AHeight: Single);
begin
  if AHeight < 1 then
    AHeight := 1;
  FMapSize.Y := AHeight;
  SetScreenOffsetY(FScreenOffset.Y);
end;


procedure TsgeViewBox.SetMapSize(ASize: TsgeFloatPoint);
begin
  SetMapWidth(ASize.X);
  SetMapHeight(ASize.Y);
end;


procedure TsgeViewBox.SetScreenOffsetX(AX: Single);
var
  W: Single;
begin
  //Размер карты с учётом масштаба
  W := FMapSize.X * FScale;

  if W > FScreenSize.X then
  begin
    FScreenOffset.X := AX;
    if FScreenOffset.X < 0 then
      FScreenOffset.X := 0;
    if FScreenOffset.X + FScreenSize.X >= W then
      FScreenOffset.X := W - FScreenSize.X;
  end
  else
    FScreenOffset.X := -((FScreenSize.X / 2) - (W / 2));

  //Поправить центр карты
  FScreenCenter.X := FScreenOffset.X + FScreenSize.X / 2;
end;


procedure TsgeViewBox.SetScreenOffsetY(AY: Single);
var
  H: Single;
begin
  //Размер карты с учётом масштаба
  H := FMapSize.Y * FScale;

  if H > FScreenSize.Y then
  begin
    FScreenOffset.Y := AY;
    if FScreenOffset.Y < 0 then
      FScreenOffset.Y := 0;
    if FScreenOffset.Y + FScreenSize.Y >= H then
      FScreenOffset.Y := H - FScreenSize.Y;
  end
  else
    FScreenOffset.Y := -((FScreenSize.Y / 2) - (H / 2));

  //Поправить центр карты
  FScreenCenter.Y := FScreenOffset.Y + FScreenSize.Y / 2;
end;


procedure TsgeViewBox.SetScreenOffset(AOffset: TsgeFloatPoint);
begin
  SetScreenOffsetX(AOffset.X);
  SetScreenOffsetY(AOffset.Y);
end;


procedure TsgeViewBox.SetScreenCenterX(AX: Single);
begin
  SetScreenOffsetX(AX - FScreenSize.X / 2);
end;


procedure TsgeViewBox.SetScreenCenterY(AY: Single);
begin
  SetScreenOffsetY(AY - FScreenSize.Y / 2);
end;


procedure TsgeViewBox.SetScreenCenter(ACenter: TsgeFloatPoint);
begin
  SetScreenCenterX(ACenter.X);
  SetScreenCenterY(ACenter.Y);
end;


procedure TsgeViewBox.SetScreenWidth(AWidth: Single);
begin
  if AWidth < 1 then
    AWidth := 1;
  FScreenSize.X := AWidth;
  SetScreenOffsetX(FScreenOffset.X);
end;


procedure TsgeViewBox.SetScreenHeight(AHeight: Single);
begin
  if AHeight < 1 then
    AHeight := 1;
  FScreenSize.Y := AHeight;
  SetScreenOffsetY(FScreenOffset.Y);
end;


procedure TsgeViewBox.SetScreenSize(ASize: TsgeFloatPoint);
begin
  SetScreenWidth(ASize.X);
  SetScreenHeight(ASize.Y);
end;


procedure TsgeViewBox.ChangeScale(AScale: Single; Xc, Yc: Single);
var
  rScreen, rMap, scl: Single;
begin
  //Поправить диапазон масштаба
  if AScale < FMinScale then
    AScale := FMinScale;
  if AScale > FMaxScale then
    AScale := FMaxScale;

  //Предусмотреть ограниченный масштаб
  if FScaleType = vbstDependent then
  begin
    //Отношение сторон
    rScreen := FScreenSize.X / FScreenSize.Y;
    rMap := FMapSize.X / FMapSize.Y;

    //Найти наименьший масштаб
    if rScreen > rMap then
      scl := FScreenSize.X / FMapSize.X
    else
      scl := FScreenSize.Y / FMapSize.Y;

    //ОСТОРОЖНО! Грязный хак! Из-за слишком длинной мантиссы в масштабе
    //при выводе спрайтов появляются артефакты при округлении. Выглядит
    //это ужасно в виде полосок в 1 пиксель шириной между спрайтами.
    scl := RoundTo(scl, -3);

    //Поправить масштаб
    if AScale <= scl then
      AScale := scl;
  end;

  //Высчитать новые координаты центра через пропроцию
  Xc := Xc * AScale / FScale;
  Yc := Yc * AScale / FScale;

  //Запомнить масштаб
  FScale := AScale;

  //Изменить центр экрана
  SetScreenCenterX(Xc);
  SetScreenCenterY(Yc);
end;


function TsgeViewBox.GetMapPosByScreenPointX(AX: Single; Safe: Boolean): Single;
var
  W: Single;
begin
  //Ширина карты с учётом масштаба
  W := FMapSize.X * FScale;

  //Определить координату X на карте
  if W <= FScreenSize.X then
  begin
    AX := AX - (FScreenSize.X - W) / 2;

    case Safe of
      True:
      begin
        Result := AX / FScale;
        if Result <= 0 then
          Result := 0;
        if Result >= FMapSize.X then
          Result := FMapSize.X;
      end;

      False:
        if (AX >= 0) and (AX <= W) then
          Result := AX / FScale
        else
          Result := -1;
    end;

  end
  else
    Result := (FScreenOffset.X + AX) / FScale;
end;


function TsgeViewBox.GetMapPosByScreenPointY(AY: Single; Safe: Boolean): Single;
var
  H: Single;
begin
  //Ширина карты с учётом масштаба
  H := FMapSize.Y * FScale;

  //Определить координату Y на карте
  if H <= FScreenSize.Y then
  begin
    AY := AY - (FScreenSize.Y - H) / 2;

    case Safe of
      True:
      begin
        Result := AY / FScale;
        if Result <= 0 then
          Result := 0;
        if Result >= FMapSize.Y then
          Result := FMapSize.Y;
      end;

      False:
        if (AY >= 0) and (AY <= H) then
          Result := AY / FScale
        else
          Result := -1;
    end;

  end
  else
    Result := (FScreenOffset.Y + AY) / FScale;
end;


constructor TsgeViewBox.Create(MapWidth, MapHeight, ScreenWidth, ScreenHeight: Single; ScaleType: TsgeViewBoxScaleType; Scale: Single; MinScale: Single; MaxScale: Single);
begin
  if MapWidth < 1 then
    MapWidth := 1;
  if MapHeight < 1 then
    MapHeight := 1;
  FMapSize.X := MapWidth;
  FMapSize.Y := MapHeight;

  if ScreenWidth < 1 then
    ScreenWidth := 1;
  if ScreenHeight < 1 then
    ScreenHeight := 1;
  FScreenSize.X := ScreenWidth;
  FScreenSize.Y := ScreenHeight;

  FScreenOffset.X := 0;
  FScreenOffset.Y := 0;

  FScaleType := ScaleType;
  FMinScale := MinScale;
  FMaxScale := MaxScale;
  FScale := 1;

  ChangeScale(Scale, 0, 0);
end;


procedure TsgeViewBox.SetScaledCenterPos(Scale: Single; X, Y: Single);
var
  Xc, Yc: Single;
  Sc: Single;
begin
  //Данный алгоритм сломал мне голову не один раз, в попытках написать
  //масштабирование к точке. Если бы не светлый ум моего друга
  //Cheery Programmer, то игроки в Дюну испытывали недоумение при
  //приближении карты и некоторое раздражение. За решение столь сложной
  //для меня задачи, ник Веслый программист будет вшито в саму игру с
  //некоторыми бонусами и отличительными характеристиками. Возможно
  //я введу специальную персональную ачивку.

  //Коэффициент изменения маштаба
  Sc := 1 - FScale / Scale;

  //Смещение центра относительно нового масштаба
  Xc := (X - FScreenSize.X / 2) * Sc;
  Yc := (Y - FScreenSize.Y / 2) * Sc;

  //Изменить масштаб
  ChangeScale(Scale, FScreenCenter.X + Xc, FScreenCenter.Y + Yc);
end;


function TsgeViewBox.GetMapPosByScreenPoint(X, Y: Single; Safe: Boolean): TsgeFloatPoint;
begin
  Result.X := GetMapPosByScreenPointX(X, Safe);
  Result.Y := GetMapPosByScreenPointY(Y, Safe);
end;


function TsgeViewBox.GetMapBounds: TsgeFloatRect;
var
  Pt: TsgeFloatPoint;
begin
  Pt := GetMapPosByScreenPoint(0, 0, True);
  Result.X1 := Pt.X;
  Result.Y1 := Pt.Y;

  Pt := GetMapPosByScreenPoint(FScreenSize.X, FScreenSize.Y, True);
  Result.X2 := Pt.X;
  Result.Y2 := Pt.Y;
end;



end.



