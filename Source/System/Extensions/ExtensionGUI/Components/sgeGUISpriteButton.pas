{
Пакет             Simple Game Engine 2
Файл              sgeGUISpriteButton.pas
Версия            1.0
Создан            23.11.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Спрайтовая кнопка
}
{$Include Defines.inc}

unit sgeGUISpriteButton;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeSprite,
  sgeEventMouse,
  sgeGUIElement,
  sgeDisplayElementSpriteTile;

type
  TsgeGUISpriteButton = class(TsgeGUIElement)
  protected
    type
      TButtonState = (bsDisable, bsNormal, bsActive, bsPressed);

  protected
    FDisplayElement: TsgeDisplayElementSpriteTile;
    FButtonState: TButtonState;

    procedure SetButtonState(AState: TButtonState);

    procedure SetSprite(ASprite: TsgeSprite);
    function  GetSprite: TsgeSprite;
  protected
    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;

    //Свойства
    procedure SetEnable(AEnabled: Boolean); override;
    procedure Handler_MouseEnter(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseLeave(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseDown(Mouse: TsgeEventMouse); override;
    procedure Handler_MouseUp(Mouse: TsgeEventMouse); override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    property Sprite: TsgeSprite read GetSprite write SetSprite;
  end;


implementation


procedure TsgeGUISpriteButton.SetButtonState(AState: TButtonState);
begin
  //Запомнить состояние кнопки
  FButtonState := AState;

  FDisplayElement.Row := Ord(FButtonState);
  FDisplayElement.Update;
end;


procedure TsgeGUISpriteButton.SetSprite(ASprite: TsgeSprite);
begin
  FDisplayElement.Sprite := ASprite;
  FDisplayElement.Update;
end;


function TsgeGUISpriteButton.GetSprite: TsgeSprite;
begin
  Result := FDisplayElement.Sprite;
end;


procedure TsgeGUISpriteButton.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FDisplayElement.PositionX := RealLeft;
  FDisplayElement.PositionY := RealTop;
  FDisplayElement.Update;
end;


procedure TsgeGUISpriteButton.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  FDisplayElement.Width := Width;
  FDisplayElement.Height := Height;
  FDisplayElement.Update;
end;


procedure TsgeGUISpriteButton.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FDisplayElement.Visible := Visible;
end;


function TsgeGUISpriteButton.DisplayElement_GetVisible: Boolean;
begin
  Result := FDisplayElement.Visible;
end;


procedure TsgeGUISpriteButton.DisplayElement_CorrectClipRect(Rect: TsgeClipRect);
begin
  FDisplayElement.ClipRect := Rect;
end;


function TsgeGUISpriteButton.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FDisplayElement.ClipRect;
end;


procedure TsgeGUISpriteButton.SetEnable(AEnabled: Boolean);
begin
  if AEnabled then
    SetButtonState(bsNormal)
  else
    SetButtonState(bsDisable);

  inherited SetEnable(AEnabled);
end;


procedure TsgeGUISpriteButton.Handler_MouseEnter(Mouse: TsgeEventMouse);
begin
  SetButtonState(bsActive);
  inherited Handler_MouseEnter(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseLeave(Mouse: TsgeEventMouse);
begin
  SetButtonState(bsNormal);
  inherited Handler_MouseLeave(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseDown(Mouse: TsgeEventMouse);
begin
  SetButtonState(bsPressed);
  inherited Handler_MouseDown(Mouse);
end;


procedure TsgeGUISpriteButton.Handler_MouseUp(Mouse: TsgeEventMouse);
begin
  SetButtonState(bsActive);
  inherited Handler_MouseUp(Mouse);
end;


constructor TsgeGUISpriteButton.Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Visible: Boolean; Parent: TsgeGUIElement);
begin
  FDisplayElement := TsgeDisplayElementSpriteTile.Create(Left, Top, Width, Height, Sprite, 1, 4);
  FDisplayElement.Add(Layer_GUI_Name);
  FDisplayElement.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FDisplayElement.Clipped := True;
  FDisplayElement.Column := 0;
  FDisplayElement.Row := 1;

  FButtonState := bsNormal;
  FEnableDoubleClick := False;
  FClickButton := mbLeft;

  inherited Create(Name, Left, Top, Width, Height, Visible, Parent);
end;


destructor TsgeGUISpriteButton.Destroy;
begin
  FDisplayElement.Delete;
  FDisplayElement.Free;

  inherited Destroy;
end;



end.

