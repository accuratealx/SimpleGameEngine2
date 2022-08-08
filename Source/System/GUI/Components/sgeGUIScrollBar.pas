{
Пакет             Simple Game Engine 2
Файл              sgeGUIScrollBar.pas
Версия            1.1
Создан            30.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Полоса прокрутки
}
{$Include Defines.inc}

unit sgeGUIScrollBar;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeSimpleParameters,
  sgeEventMouse, sgeEventKeyboard,
  sgeGUIPropertyBackground,
  sgeGUIElement;

type
  //Ориентация
  TsgeGUIOrientation = (oHorizontal, oVertical);

  //Обработчики изменения ползунка
  TsgeGUIProcScrollbarScroll = procedure(Obj: TsgeGUIElement; OldPosition, NewPosition: Integer) of object;


  TsgeGUIScrollBar = class(TsgeGUIElement)
  private
    FMoving: Boolean;
    FMoveOffset: Integer;
    FMovePos: TsgeIntPoint;

  protected
    FBackground: TsgeGUIPropertyBackgroundExt;                      //Фон
    FSlider: TsgeGUIPropertyBackgroundExt;                          //Ползунок

    FOrientation: TsgeGUIOrientation;                               //Положение в пространстве
    FPosition: Integer;                                             //Положение
    FMin: Integer;                                                  //Наименьшее значение
    FMax: Integer;                                                  //Наибольшее значение
    FStep: Integer;                                                 //Обычный шаг изменения
    FPageStep: Integer;                                             //Страничный шаг изменнеия
    FSliderSize: Integer;                                           //Размер ползунка
    FSliderMinSize: Integer;                                        //Минимальный размер слайдера

    //Обработчики
    FOnScroll: TsgeGUIProcScrollbarScroll;
    FOnStartScroll: TsgeGUIProcEvent;
    FOnStopScroll: TsgeGUIProcEvent;

    //Обёртки дополнительных обработчиков
    procedure Handler_OnScroll(OldValue, NewValue: Integer);
    procedure Handler_OnStartScroll;
    procedure Handler_OnStopScroll;

    //Вспомогательные методы
    function GetValueRange: Integer;                                //Получить общий диапазон изменений
    function GetSliderRange: Integer;                               //Получить свободный диапазон перемещения ползунка
    function GetDeltaX: Single;                                     //Получить количество пикселей в одном значении
    function GetSliderPos: Integer;                                 //Получить положение ползунка
    function GetSliderRect: TsgeFloatRect;                          //Получить границы ползунка
    function IsMouseInSliderRect(X, Y: Integer): Boolean;           //Проверить нахождение мыши в нутри границ слайдера
    function IsMouseInLeftBySlider(X, Y: Integer): Boolean;         //Проверить нахождение мыши слева от ползунка
    function IsMouseInRightBySlider(X, Y: Integer): Boolean;        //Проверить нахождение мыши справа от ползунка

    //Свойства
    procedure Handler_MouseDown(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseUp(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseMove(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseScroll(Mouse: TsgeEventMouse); override;
    procedure Handler_ButtonDown(Keyboard: TsgeEventKeyboard); override;

    procedure SetOrientation(AOrientation: TsgeGUIOrientation);
    procedure SetPosition(APosition: Integer);
    procedure SetMin(AMin: Integer);
    procedure SetMax(AMax: Integer);
    procedure SetStep(AStep: Integer);
    procedure SetPageStep(ASize: Integer);
    procedure SetSliderSize(ASize: Integer);
    procedure SetSliderMinSize(AMinSize: Integer);
    function  GetBackground: TsgeGUIPropertyBackground;
    function  GetSlider: TsgeGUIPropertyBackground;

    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property Background: TsgeGUIPropertyBackground read GetBackground;
    property Slider: TsgeGUIPropertyBackground read GetSlider;
    property Orientation: TsgeGUIOrientation read FOrientation write SetOrientation;
    property Position: Integer read FPosition write SetPosition;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Step: Integer read FStep write SetStep;
    property PageStep: Integer read FPageStep write SetPageStep;
    property SliderSize: Integer read FSliderSize write SetSliderSize;
    property SliderMinSize: Integer read FSliderMinSize write SetSliderMinSize;

    property OnScroll: TsgeGUIProcScrollbarScroll read FOnScroll write FOnScroll;
    property OnStartScroll: TsgeGUIProcEvent read FOnStartScroll write FOnStartScroll;
    property OnStopScroll: TsgeGUIProcEvent read FOnStopScroll write FOnStopScroll;
  end;


implementation

uses
  sgeCorePointerUtils, sgeKeys, sgeGUIUtils;


procedure TsgeGUIScrollBar.Handler_OnScroll(OldValue, NewValue: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, OldValue, NewValue);
end;


procedure TsgeGUIScrollBar.Handler_OnStartScroll;
begin
  if Assigned(FOnStartScroll) then
    FOnStartScroll(Self);
end;


procedure TsgeGUIScrollBar.Handler_OnStopScroll;
begin
  if Assigned(FOnStopScroll) then
    FOnStopScroll(Self);
end;


function TsgeGUIScrollBar.GetValueRange: Integer;
begin
  Result := Abs(FMax - FMin);
end;


function TsgeGUIScrollBar.GetSliderRange: Integer;
begin
  case FOrientation of
    oHorizontal:
      Result := FWidth - FSliderSize;
    oVertical:
      Result := FHeight - FSliderSize;
  end;
end;


function TsgeGUIScrollBar.GetDeltaX: Single;
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


function TsgeGUIScrollBar.GetSliderPos: Integer;
var
  dx: Single;
begin
  dx := GetDeltaX;
  Result := Round(dx * FPosition);
end;


function TsgeGUIScrollBar.GetSliderRect: TsgeFloatRect;
var
  dx: Single;
begin
  dx := GetDeltaX;

  case FOrientation of
    oHorizontal:
    begin
      Result.X1 := GetSliderPos - Round(FMin * dx);
      Result.Y1 := 0;
      Result.X2 := Result.X1 + FSliderSize;
      Result.Y2 := FHeight;
    end;

    oVertical:
    begin
      Result.X1 := 0;
      Result.Y1 := GetSliderPos - Round(FMin * dx);
      Result.X2 := FWidth;
      Result.Y2 := Result.Y1 + FSliderSize;
    end;
  end;
end;


function TsgeGUIScrollBar.IsMouseInSliderRect(X, Y: Integer): Boolean;
var
  Rct: TsgeFloatRect;
begin
  Rct := GetSliderRect;
  Result := (X >= Rct.X1) and (X <= Rct.X2) and (Y >= Rct.Y1) and (Y <= Rct.Y2);
end;


function TsgeGUIScrollBar.IsMouseInLeftBySlider(X, Y: Integer): Boolean;
var
  Rct: TsgeFloatRect;
begin
  Rct := GetSliderRect;
  case FOrientation of
    oHorizontal:
      Result := (X >= 0) and (X <= Rct.X1) and (Y >= 0) and (Y <= FHeight);

    oVertical:
      Result := (X >= 0) and (X <= FWidth) and (Y >= 0) and (Y <= Rct.Y1);
  end;
end;


function TsgeGUIScrollBar.IsMouseInRightBySlider(X, Y: Integer): Boolean;
var
  Rct: TsgeFloatRect;
begin
  Rct := GetSliderRect;
  case FOrientation of
    oHorizontal:
      Result := (X >= Rct.X2) and (X <= FWidth) and (Y >= 0) and (Y <= FHeight);

    oVertical:
      Result := (X >= 0) and (X <= FWidth) and (Y >= Rct.Y2) and (Y <= FHeight);
  end;
end;


procedure TsgeGUIScrollBar.Handler_MouseDown(Mouse: TsgeEventMouse);
begin
  if (mbLeft in Mouse.MouseButtons) then
  begin
    //Проверить на попадание в элемент
    if IsMouseInSliderRect(Mouse.X, Mouse.Y) then
    begin
      //Сохранить параметры
      FMovePos := Mouse.Pos;
      FMoving := True;
      FMoveOffset := GetSliderPos;
      sgeCorePointer_GetSGE.ExtGUI.MouseCapture(Self);

      //Пользовательский обработчик
      Handler_OnStartScroll;
    end;

    //Слева от ползунка
    if IsMouseInLeftBySlider(Mouse.X, Mouse.Y) then
      Position := Position - FPageStep;

    //Справа от ползунка
    if IsMouseInRightBySlider(Mouse.X, Mouse.Y) then
      Position := Position + FPageStep;
  end;

  inherited Handler_MouseDown(Mouse);
end;


procedure TsgeGUIScrollBar.Handler_MouseUp(Mouse: TsgeEventMouse);
begin
  if FMoving then
  begin
    FMoving := False;

    //Освободить захват мыши
    sgeCorePointer_GetSGE.ExtGUI.ReleaseMouse(Self);

    //Пользовательский обработчик
    Handler_OnStopScroll;
  end;

  inherited Handler_MouseUp(Mouse);
end;


procedure TsgeGUIScrollBar.Handler_MouseMove(Mouse: TsgeEventMouse);
var
  Offset: Integer;
begin
  if FMoving then
  begin
    case FOrientation of
      oHorizontal:
        Offset := FMoveOffset + (Mouse.X - FMovePos.X);

      oVertical:
        Offset := FMoveOffset + (Mouse.Y - FMovePos.Y);
    end;

    //Изменить положение
    Position := Round(Offset / GetDeltaX);
  end;

  inherited Handler_MouseMove(Mouse);
end;


procedure TsgeGUIScrollBar.Handler_MouseScroll(Mouse: TsgeEventMouse);
begin
  if Mouse.Delta < 0 then
    Position := Position + FStep
  else
    Position := Position - FStep;

  inherited Handler_MouseScroll(Mouse);
end;

procedure TsgeGUIScrollBar.Handler_ButtonDown(Keyboard: TsgeEventKeyboard);
begin
  case Keyboard.Key of
    keyLeft:
      Position := Position - 1;

    keyRight:
      Position := Position + 1;

    keyHome:
      Position := FMin;

    keyEnd:
      Position := FMax;

    keyPageUp:
      Position := Position - FPageStep;

    keyPageDown:
      Position := Position + FPageStep;
  end;

  inherited Handler_ButtonDown(Keyboard);
end;


procedure TsgeGUIScrollBar.SetOrientation(AOrientation: TsgeGUIOrientation);
var
  d: Integer;
begin
  if FOrientation = AOrientation then
    Exit;

  //Сохранить значение
  FOrientation := AOrientation;

  //Поправить размеры
  d := Width;
  FWidth := FHeight;
  FHeight := d;

  //Перерисовать
  Resize;
end;


procedure TsgeGUIScrollBar.SetPosition(APosition: Integer);
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

  //Перерисовать
  Repaint;
end;


procedure TsgeGUIScrollBar.SetMin(AMin: Integer);
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

  //Перерисовать
  Repaint;
end;


procedure TsgeGUIScrollBar.SetMax(AMax: Integer);
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

  //Перерисовать
  Repaint;
end;


procedure TsgeGUIScrollBar.SetStep(AStep: Integer);
begin
  if FStep = AStep then
    Exit;

  //Сохранить значение
  FStep := AStep;
end;


procedure TsgeGUIScrollBar.SetPageStep(ASize: Integer);
begin
  if FPageStep = ASize then
    Exit;

  //Сохранить значение
  FPageStep := ASize;
end;


procedure TsgeGUIScrollBar.SetSliderSize(ASize: Integer);
begin
  if ASize < FSliderMinSize then
    ASize := FSliderMinSize;
  case FOrientation of
    oHorizontal:
      if ASize > FWidth then
        ASize := FWidth;

    oVertical:
      if ASize > FHeight then
        ASize := FHeight;
  end;

  if FSliderSize = ASize then
    Exit;

  //Сохранить значение
  FSliderSize := ASize;

  //Перерисовать
  Repaint;
end;


procedure TsgeGUIScrollBar.SetSliderMinSize(AMinSize: Integer);
begin
  if AMinSize < 1 then
    AMinSize := 1;
  if FSliderMinSize = AMinSize then
    Exit;

  //Сохранить значение
  FSliderMinSize := AMinSize;

  //Перерисовать
  Repaint;
end;


function TsgeGUIScrollBar.GetBackground: TsgeGUIPropertyBackground;
begin
  Result := FBackground;
end;


function TsgeGUIScrollBar.GetSlider: TsgeGUIPropertyBackground;
begin
  Result := FSlider;
end;


class function TsgeGUIScrollBar.GetParameterSectionName: String;
begin
  Result := 'ScrollBar';
end;


procedure TsgeGUIScrollBar.LoadData(Data: TsgeSimpleParameters);
var
  ParamName: String;
  s: String;
begin
  inherited LoadData(Data);

  //Orientation
  ParamName := 'Orientation';
  if Data.Exist[ParamName] then
  begin
    s := LowerCase(Data.GetValue(ParamName, ''));
    case s of
      'horizontal':
        SetOrientation(oHorizontal);
      'vertical':
        SetOrientation(oVertical);
    end;
  end;

  //Step
  sgeGUISetValue(Data, 'Step', FStep, 1);

  //PageStep
  sgeGUISetValue(Data, 'PageStep', FPageStep, 10);

  //SliderMinSize
  sgeGUISetValue(Data, 'SliderMinSize', FSliderMinSize, 1);

  //SliderSize
  sgeGUISetValue(Data, 'SliderSize', FSliderMinSize, 32);

  //Position
  sgeGUISetValue(Data, 'Position', FPosition, 0);

  //FMin
  ParamName := 'Min';
  if Data.Exist[ParamName] then
    SetMin(Data.GetValue(ParamName, 0));

  //FMax
  ParamName := 'Max';
  if Data.Exist[ParamName] then
    SetMin(Data.GetValue(ParamName, 100));

  //Background
  FBackground.LoadParameters(Data, 'Background.');

  //Slider
  FSlider.LoadParameters(Data, 'Slider.');
end;


procedure TsgeGUIScrollBar.DrawBefore;
begin
  //Вывод фона
  FBackground.Draw(sgeGetFloatRect(0, 0, FWidth, FHeight));

  //Вывод слайдера
  FSlider.Draw(GetSliderRect);
end;


constructor TsgeGUIScrollBar.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  //Классы
  FBackground := TsgeGUIPropertyBackgroundExt.Create(Self);
  FSlider := TsgeGUIPropertyBackgroundExt.Create(Self);

  //Свойства
  FEnableDoubleClick := False;
  FOrientation := oHorizontal;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FStep := 1;
  FPageStep := 10;
  FSliderMinSize := 1;
  FSliderSize := Width div 2;

  //Перерисовать форму
  Repaint;
end;


destructor TsgeGUIScrollBar.Destroy;
begin
  FSlider.Free;
  FBackground.Free;

  inherited Destroy;
end;


end.

