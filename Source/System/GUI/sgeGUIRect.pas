{
Пакет             Simple Game Engine 2
Файл              sgeGUIRect.pas
Версия            1.0
Создан            27.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Прямоугольник
}
{$Include Defines.inc}

unit sgeGUIRect;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeGraphicColor,
  sgeGUIElement,
  sgeDisplayElementRect;

type
  TsgeGUIRect = class(TsgeGUIElement)
  private
    FDisplayElement: TsgeDisplayElementRect;

    procedure SetColor(AColor: TsgeColor);
    function  GetColor: TsgeColor;
  protected
    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Color: TsgeColor; Visible: Boolean = True; Parent: TsgeGUIElement = nil);
    destructor  Destroy; override;

    property Color: TsgeColor read GetColor write SetColor;
  end;


implementation


procedure TsgeGUIRect.SetColor(AColor: TsgeColor);
begin
  FDisplayElement.Color := AColor;
  FDisplayElement.Update;
end;


function TsgeGUIRect.GetColor: TsgeColor;
begin
  Result := FDisplayElement.Color;
end;


procedure TsgeGUIRect.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin
  FDisplayElement.PositionX := RealLeft;
  FDisplayElement.PositionY := RealTop;
  FDisplayElement.Update;
end;


procedure TsgeGUIRect.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  FDisplayElement.Width := Width;
  FDisplayElement.Height := Height;
  FDisplayElement.Update;
end;


procedure TsgeGUIRect.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FDisplayElement.Visible := Visible;
end;


function TsgeGUIRect.DisplayElement_GetVisible: Boolean;
begin
  Result := FDisplayElement.Visible;
end;


procedure TsgeGUIRect.DisplayElement_CorrectClipRect(Rect: TsgeClipRect);
begin
  FDisplayElement.ClipRect := Rect;
end;


function TsgeGUIRect.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FDisplayElement.ClipRect;
end;


constructor TsgeGUIRect.Create(Name: String; Left, Top, Width, Height: Integer; Color: TsgeColor; Visible: Boolean; Parent: TsgeGUIElement);
begin
  FDisplayElement := TsgeDisplayElementRect.Create(Left, Top, Left + Width, Top + Height, Color);
  FDisplayElement.Add(Layer_GUI_Name);
  FDisplayElement.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FDisplayElement.Clipped := True;

  inherited Create(Name, Left, Top, Width, Height, Visible, Parent);
end;


destructor TsgeGUIRect.Destroy;
begin
  FDisplayElement.Delete;
  FDisplayElement.Free;

  inherited Destroy;
end;



end.

