{
Пакет             Simple Game Engine 2
Файл              sgeGUISprite.pas
Версия            1.0
Создан            25.08.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Спрайт
}
{$Include Defines.inc}

unit sgeGUISprite;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeSprite,
  sgeGUIElement,
  sgeGUIPropertySprite,
  sgeDisplayElementSprite;

type
  TsgeGUISprite = class(TsgeGUIElement)
  private
    FDisplayElement: TsgeDisplayElementSprite;
    FSprite: TsgeGUIPropertySpriteEx;

    function  GetSprite: TsgeGUIPropertySprite;
  protected
    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;

    procedure PropertyChanged; override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    property Sprite: TsgeGUIPropertySprite read GetSprite;
  end;


implementation


function TsgeGUISprite.GetSprite: TsgeGUIPropertySprite;
begin
  Result := FSprite;
end;


procedure TsgeGUISprite.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FDisplayElement.PositionX := RealLeft;
  FDisplayElement.PositionY := RealTop;
  FDisplayElement.Update;
end;


procedure TsgeGUISprite.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  FDisplayElement.Width := Width;
  FDisplayElement.Height := Height;
  FDisplayElement.Update;
end;


procedure TsgeGUISprite.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FDisplayElement.Visible := Visible;
end;


function TsgeGUISprite.DisplayElement_GetVisible: Boolean;
begin
  Result := FDisplayElement.Visible;
end;


procedure TsgeGUISprite.DisplayElement_CorrectClipRect(Rect: TsgeClipRect);
begin
  FDisplayElement.ClipRect := Rect;
end;


function TsgeGUISprite.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FDisplayElement.ClipRect;
end;


procedure TsgeGUISprite.PropertyChanged;
var
  Rect: TsgeIntRect;
begin
  //Получить реальное положение и размеры
  Rect := FSprite.GetRealPosAndSize;

  //Поправить положение и размер спрайта
  FDisplayElement.PositionX := Rect.X1;
  FDisplayElement.PositionY := Rect.Y1;
  FDisplayElement.Width := Rect.X2;
  FDisplayElement.Height := Rect.Y2;
  FDisplayElement.Update;
end;


constructor TsgeGUISprite.Create(Name: String; Left, Top, Width, Height: Integer; Sprite: TsgeSprite; Visible: Boolean; Parent: TsgeGUIElement);
begin
  FDisplayElement := TsgeDisplayElementSprite.Create(Left, Top, Width, Height, Sprite);
  FDisplayElement.Add(Layer_GUI_Name);
  FDisplayElement.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FDisplayElement.Clipped := True;

  FSprite := TsgeGUIPropertySpriteEx.Create(Self, FDisplayElement);

  inherited Create(Name, Left, Top, Width, Height, Visible, Parent);
end;


destructor TsgeGUISprite.Destroy;
begin
  FSprite.Free;

  FDisplayElement.Delete;
  FDisplayElement.Free;

  inherited Destroy;
end;



end.

