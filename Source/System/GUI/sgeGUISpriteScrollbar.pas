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
  sgeDisplayElementRect, sgeDisplayElementSpriteNine;

type
  TsgeGUISpriteScrollbar = class(TsgeGUIElement)
  protected
    FBGRect: TsgeDisplayElementRect;
    FSlider: TsgeDisplayElementSpriteNine;

    FOrientation: TsgeOrientation;  //Ориентация
    FPosition: Integer;             //Положение
    FMin: Integer;                  //Наименьшее значение
    FMax: Integer;                  //Наибольшее значение
    FStep: Integer;                 //Обычный шаг изменения
    FPageStep: Integer;             //Страничный шаг изменнеия
    FSliderSize: Integer;           //Размер ползунка
    FSliderMinSize: Integer;        //Минимальный размер слайдера

    procedure CorrectSliderPosition;

    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;


    procedure SetOrientation(AOrientation: TsgeOrientation);
    procedure SetOffset(AOffset: TsgeFloatRect);
    function  GetOffset: TsgeFloatRect;
  public

    constructor Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Orientation: TsgeOrientation = oHorizontal; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    property Orientation: TsgeOrientation read FOrientation write SetOrientation;
    property Offset: TsgeFloatRect read GetOffset write SetOffset;
  end;


implementation


procedure TsgeGUISpriteScrollbar.CorrectSliderPosition;
begin
  FSlider.PositionX := FBGRect.PositionX + 10;
  FSlider.PositionY := FBGRect.PositionY + 10;
  //FSlider.Width := FBGRect.Width;
  //FSlider.Height := FBGRect.Height;
  FSlider.Scale := GetScale;
  FSlider.Update;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FBGRect.PositionX := RealLeft;
  FBGRect.PositionY := RealTop;
  FBGRect.Update;

  CorrectSliderPosition;
end;


procedure TsgeGUISpriteScrollbar.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  FBGRect.Width := Width;
  FBGRect.Height := Height;
  FBGRect.Update;

  CorrectSliderPosition;
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
  FBGRect.ClipRect := Rect;
  FSlider.ClipRect := Rect;
end;


function TsgeGUISpriteScrollbar.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FSlider.ClipRect;
end;


procedure TsgeGUISpriteScrollbar.SetOrientation(AOrientation: TsgeOrientation);
begin
  if FOrientation = AOrientation then
    Exit;

  FOrientation := AOrientation;

  //Перестроить внешний вид
end;


procedure TsgeGUISpriteScrollbar.SetOffset(AOffset: TsgeFloatRect);
begin
  FSlider.Offset := AOffset;
  FSlider.Update;
end;


function TsgeGUISpriteScrollbar.GetOffset: TsgeFloatRect;
begin
  Result := FSlider.Offset;
end;


constructor TsgeGUISpriteScrollbar.Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Orientation: TsgeOrientation; Visible: Boolean; Parent: TsgeGUIElement);
begin
  //Фон
  FBGRect := TsgeDisplayElementRect.Create(Left, Top, Left + Width, Top + Height, cDarkGray);
  FBGRect.Add(Layer_GUI_Name);
  FBGRect.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FBGRect.Clipped := True;

  //Ползунок
  FSlider := TsgeDisplayElementSpriteNine.Create(Left, Top, width div 2, Height div 2, Sprite, sgeGetFloatRect(0, 0, 0, 0));
  FSlider.Add(Layer_GUI_Name);
  FSlider.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FSlider.Clipped := True;

  //Параметры
  FOrientation := Orientation;
  FEnableDoubleClick := False;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FStep := 1;
  FPageStep := 10;
  FSliderMinSize := 1;
  FSliderSize := Width div 2;

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

