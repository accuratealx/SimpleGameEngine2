{
Пакет             Simple Game Engine 2
Файл              sgeGUISpriteScrollbar.pas
Версия            1.0
Создан            28.11.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Спрайтовый ползунок
}
{$Include Defines.inc}

unit sgeGUISpriteScrollbar;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeTypes, sgeSprite, sgeColor,
  sgeGUIElement,
  sgeEventMouseDown, sgeEventMouseUp, sgeEventMouseMove, sgeEventMouseScroll,
  sgeDisplayElementRect, sgeDisplayElementSpriteNine;

type
  //Обработчики изменения ползунка
  TsgeGUIProcScrollbarScroll = procedure(Obj: TsgeGUIElement; OldPosition, NewPosition: Integer) of object;


  TsgeGUISpriteScrollbar = class(TsgeGUIElement)
  private
    type
      TScrollbarClickLocation = (
        sclLeft,    //Слева от ползунка
        sclSlider,  //На ползунке
        sclRight    //Справа от ползунка
      );
  private
    FMoving: Boolean;
    FMoveOffset: Integer;
    FMovePos: TsgeIntPoint;
  protected
    //Выводимые элементы
    FBGRect: TsgeDisplayElementRect;
    FSlider: TsgeDisplayElementSpriteNine;

    //Праметры
    FClipRect: TsgeClipRect;        //Область отсечения
    FOrientation: TsgeOrientation;  //Ориентация
    FPosition: Integer;             //Положение
    FMin: Integer;                  //Наименьшее значение
    FMax: Integer;                  //Наибольшее значение
    FPageStep: Integer;             //Страничный шаг изменения
    FSliderWidth: Integer;          //Размер ползунка
    FSliderMinSize: Integer;        //Минимальный размер слайдера

    //Обработчики
    FOnScroll: TsgeGUIProcScrollbarScroll;

    procedure CorrectSizeAndPosition;
    procedure CorrectSliderSizeAndPosition;
    function  GetValueRange: Integer;         //Получить общий диапазон изменений
    function  GetSliderRange: Integer;        //Получить свободный диапазон перемещения ползунка
    function  GetDeltaX: Single;              //Получить количество пикселей в одном значении
    function  GetSliderPos: Integer;          //Получить положение ползунка
    function  GetSliderRect: TsgeIntRect;     //Получить границы ползунка
    function  GetClickLocation(Pos: TsgeIntPoint): TScrollbarClickLocation; //Определить в каком месте нажали

    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;

    procedure Handler_OnScroll(OldValue, NewValue: Integer);  //Обработчик изменения положения

    procedure Handler_MouseDown(Mouse: TsgeEventMouseDown); override;
    procedure Handler_MouseUp(Mouse: TsgeEventMouseUp); override;
    procedure Handler_MouseMove(Mouse: TsgeEventMouseMove); override;
    procedure Handler_MouseScroll(Mouse: TsgeEventMouseScroll); override;

    procedure SetOrientation(AOrientation: TsgeOrientation);
    procedure SetSpriteOffset(AOffset: TsgeFloatRect);
    function  GetSpriteOffset: TsgeFloatRect;
    procedure SetBGColor(AColor: TsgeColor);
    function  GetBGColor: TsgeColor;
    procedure SetPosition(APosition: Integer);
    procedure SetSliderWidth(AWidth: Integer);
    procedure SetSliderMinSize(AMinSize: Integer);
    procedure SetMin(AMin: Integer);
    procedure SetMax(AMax: Integer);
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; BGColor: TsgeColor; Orientation: TsgeOrientation = oHorizontal; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    property Orientation: TsgeOrientation read FOrientation write SetOrientation;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition;
    property SliderWidth: Integer read FSliderWidth write SetSliderWidth;
    property SliderMinSize: Integer read FSliderMinSize write SetSliderMinSize;

    property SpriteOffset: TsgeFloatRect read GetSpriteOffset write SetSpriteOffset;

    property OnScroll: TsgeGUIProcScrollbarScroll read FOnScroll write FOnScroll;
  end;


implementation

uses
  sgeCorePointerUtils;


procedure TsgeGUISpriteScrollbar.CorrectSizeAndPosition;
var
  SSize: TsgeIntPoint;
begin
  SSize := GetScaleSize;

  FBGRect.Width := SSize.X;
  FBGRect.Height := SSize.Y;
  FBGRect.Update;

  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.CorrectSliderSizeAndPosition;
var
  W, H, X, Y: Integer;
  GPos: TsgeIntPoint;
  SScale: Single;
begin
  GPos := GetGlobalPos;
  SScale := GetScale;

  case FOrientation of
    oHorizontal:
    begin
      W := FSliderWidth;
      H := FHeight;
      X := GPos.X + GetSliderPos - Round(FMin * GetDeltaX);
      Y := GPos.Y;
    end;

    oVertical:
    begin
      W := FWidth;
      H := FSliderWidth;
      X := GPos.X;
      Y := GPos.Y + GetSliderPos - Round(FMin * GetDeltaX);
    end;
  end;

  FSlider.Scale := SScale;
  FSlider.PositionX := X;
  FSlider.PositionY := Y;
  FSlider.Width := W;
  FSlider.Height := H;
  FSlider.Update;
end;


function TsgeGUISpriteScrollbar.GetValueRange: Integer;
begin
  Result := Abs(FMax - FMin);
end;


function TsgeGUISpriteScrollbar.GetSliderRange: Integer;
var
  SSize: TsgeIntPoint;
begin
  SSize := GetScaleSize;

  case FOrientation of
    oHorizontal:
      Result := Round(SSize.X - FSliderWidth * GetScale);
    oVertical:
      Result := Round(SSize.Y - FSliderWidth * GetScale);
  end;
end;


function TsgeGUISpriteScrollbar.GetDeltaX: Single;
var
  ValueRange, SliderRange: Integer;
begin
  ValueRange := GetValueRange;
  SliderRange := GetSliderRange;

  if ValueRange = 0 then
    Result := SliderRange
  else
    Result := GetSliderRange / ValueRange;
end;


function TsgeGUISpriteScrollbar.GetSliderPos: Integer;
begin
  Result := Round(GetDeltaX * FPosition);
end;


function TsgeGUISpriteScrollbar.GetSliderRect: TsgeIntRect;
var
  SScale: Single;
begin
  SScale := GetScale;

  case FOrientation of
    oHorizontal:
    begin
      Result.X1 := Round((GetSliderPos - Round(FMin * GetDeltaX)) / SScale);
      Result.Y1 := 0;
      Result.X2 := Round(Result.X1 + FSliderWidth);
      Result.Y2 := FHeight;
    end;

    oVertical:
    begin
      Result.X1 := 0;
      Result.Y1 := Round((GetSliderPos - Round(FMin * GetDeltaX)) / Sscale);
      Result.X2 := FWidth;
      Result.Y2 := Round(Result.Y1 + FSliderWidth);
    end;
  end;
end;


function TsgeGUISpriteScrollbar.GetClickLocation(Pos: TsgeIntPoint): TScrollbarClickLocation;
var
  SliderRect: TsgeIntRect;
begin
  SliderRect := GetSliderRect;

  case FOrientation of
    oHorizontal:
    begin
      if (Pos.X >= 0) and (Pos.X <= SliderRect.X1) then
        Exit(sclLeft);

      if (Pos.X >= SliderRect.X2) and (Pos.X <= FWidth) then
        Exit(sclRight);

      Exit(sclSlider);
    end;

    oVertical:
    begin
      if (Pos.Y >= 0) and (Pos.Y <= SliderRect.Y1) then
        Exit(sclLeft);

      if (Pos.Y >= SliderRect.Y2) and (Pos.Y <= FHeight) then
        Exit(sclRight);

      Exit(sclSlider);
    end;
  end;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FBGRect.PositionX := RealLeft;
  FBGRect.PositionY := RealTop;
  FBGRect.Update;

  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  CorrectSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FBGRect.Visible := Visible;
  FSlider.Visible := Visible;
end;


function TsgeGUISpriteScrollbar.DisplayElement_GetVisible: Boolean;
begin
  Result := FSlider.Visible;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectClipRect(Rect: TsgeClipRect);
begin
  FClipRect := Rect;
  FBGRect.ClipRect := FClipRect;
  FSlider.ClipRect := FClipRect;
end;


function TsgeGUISpriteScrollbar.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FClipRect;
end;

procedure TsgeGUISpriteScrollbar.Handler_OnScroll(OldValue, NewValue: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, OldValue, NewValue);
end;


procedure TsgeGUISpriteScrollbar.Handler_MouseDown(Mouse: TsgeEventMouseDown);
begin
  if (mbLeft in Mouse.MouseButtons) then
  begin
    case GetClickLocation(Mouse.Pos) of
      sclLeft:
      begin
        Position := Position - FPageStep;
      end;

      sclRight:
      begin
        Position := Position + FPageStep;
      end;

      sclSlider:
      begin
        FMovePos := Mouse.Pos;
        FMoving := True;
        FMoveOffset := Round(GetSliderPos / GetScale);
        sgeCorePointer_GetSGE.ExtGUI.MouseCapture(Self);
      end;
    end;
  end;

  inherited Handler_MouseDown(Mouse);
end;


procedure TsgeGUISpriteScrollbar.Handler_MouseUp(Mouse: TsgeEventMouseUp);
begin
  if FMoving then
  begin
    FMoving := False;

    //Освободить захват мыши
    sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);
  end;

  inherited Handler_MouseUp(Mouse);
end;


procedure TsgeGUISpriteScrollbar.Handler_MouseMove(Mouse: TsgeEventMouseMove);
var
  AOffset: Integer;
begin
  if FMoving then
  begin
    case FOrientation of
      oHorizontal:
        AOffset := FMoveOffset + (Mouse.X - FMovePos.X);

      oVertical:
        AOffset := FMoveOffset + (Mouse.Y - FMovePos.Y);
    end;

    //Изменить положение
    Position := Round(AOffset / GetDeltaX * GetScale);
  end;

  inherited Handler_MouseMove(Mouse);
end;


procedure TsgeGUISpriteScrollbar.Handler_MouseScroll(Mouse: TsgeEventMouseScroll);
begin
  if Mouse.Delta < 0 then
    Position := Position + FPageStep
  else
    Position := Position - FPageStep;

  inherited Handler_MouseScroll(Mouse);
end;


procedure TsgeGUISpriteScrollbar.SetOrientation(AOrientation: TsgeOrientation);
var
  d: Integer;
begin
  if FOrientation = AOrientation then
    Exit;

  //Сохранить риентацию
  FOrientation := AOrientation;

  //Поправить размеры элемента
  d := Width;
  FWidth := FHeight;
  FHeight := d;

  //Перестроить внешний вид
  CorrectSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.SetSpriteOffset(AOffset: TsgeFloatRect);
begin
  FSlider.Offset := AOffset;
  FSlider.Update;
end;


function TsgeGUISpriteScrollbar.GetSpriteOffset: TsgeFloatRect;
begin
  Result := FSlider.Offset;
end;


procedure TsgeGUISpriteScrollbar.SetBGColor(AColor: TsgeColor);
begin
  FBGRect.Color := AColor;
end;


function TsgeGUISpriteScrollbar.GetBGColor: TsgeColor;
begin
  Result := FBGRect.Color;
end;


procedure TsgeGUISpriteScrollbar.SetPosition(APosition: Integer);
begin
  if APosition < FMin then
    APosition := FMin;
  if APosition > FMax then
    APosition := FMax;
  if FPosition = APosition then
    Exit;

  //Пользовательский обработчик
  Handler_OnScroll(FPosition, APosition);

  //Сохранить значение
  FPosition := APosition;

  //Поправить слайдер
  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.SetSliderWidth(AWidth: Integer);
begin
  if AWidth < FSliderMinSize then
    AWidth := FSliderMinSize;
  if AWidth > FWidth then
    AWidth := FWidth;
  if FSliderWidth = AWidth then
    Exit;

  //Сохранить значение
  FSliderWidth := AWidth;

  //Поправить слайдер
  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.SetSliderMinSize(AMinSize: Integer);
begin
  if AMinSize < 1 then
    AMinSize := 1;
  if FSliderMinSize = AMinSize then
    Exit;

  //Сохранить значение
  FSliderMinSize := AMinSize;

  //Поправить слайдер
  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.SetMin(AMin: Integer);
begin
  if AMin > FMax then
    AMin := FMax;
  if AMin = FMin then
    Exit;

  //Сохранить значение
  FMin := AMin;

  //Поправить ползунок
  if FPosition < FMin then
    FPosition := FMin;

  //Поправить слайдер
  CorrectSliderSizeAndPosition;
end;


procedure TsgeGUISpriteScrollbar.SetMax(AMax: Integer);
begin
  if AMax < FMin then
    AMax := FMin;
  if AMax = FMax then
    Exit;

  //Сохранить значение
  FMax := AMax;

  //Поправить ползунок
  if FPosition > FMax then
    FPosition := FMax;

  //Поправить слайдер
  CorrectSliderSizeAndPosition;
end;


constructor TsgeGUISpriteScrollbar.Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; BGColor: TsgeColor; Orientation: TsgeOrientation; Visible: Boolean; Parent: TsgeGUIElement);
begin
  //Фон
  FBGRect := TsgeDisplayElementRect.Create(Left, Top, Left + Width, Top + Height, BGColor);
  FBGRect.Add(Layer_GUI_Name);
  FBGRect.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FBGRect.Clipped := True;

  //Ползунок
  FSlider := TsgeDisplayElementSpriteNine.Create(Left, Top, width div 2, Height div 2, Sprite, sgeGetFloatRect(0, 0, 0, 0));
  FSlider.Add(Layer_GUI_Name);
  FSlider.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FSlider.Clipped := True;

  //Параметры
  FClipRect := sgeGetClipRect(Left, Top, Width, Height);

  FOrientation := Orientation;
  FEnableDoubleClick := False;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FPageStep := 10;
  FSliderMinSize := 1;
  FSliderWidth := Width div 3;

  inherited Create(Name, Left, Top, Width, Height, Visible, Parent);
end;


destructor TsgeGUISpriteScrollbar.Destroy;
begin
  FSlider.Delete;
  FSlider.Free;

  FBGRect.Delete;
  FBGRect.Free;

  inherited Destroy;
end;



end.

