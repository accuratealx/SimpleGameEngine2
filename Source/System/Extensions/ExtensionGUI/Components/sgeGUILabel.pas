{
Пакет             Simple Game Engine 2
Файл              sgeGUILabel.pas
Версия            1.0
Создан            27.11.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Надпись
}
{$Include Defines.inc}

unit sgeGUILabel;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeAnsiFont, sgeColor,
  sgeDisplayElementAnsiText,
  sgeGUIElement;

type
  TsgeGUILabel = class(TsgeGUIElement)
  protected
    FText: TsgeDisplayElementAnsiText;
    FVerticalAlign: TsgeVerticalAlign;
    FHorizontalAlign: TsgeHorizontalAlign;

    procedure GetTextSize(out Width, Height: Integer);
    procedure CorrectTextPosition;

    function  GetColor: TsgeColor;
    procedure Setcolor(AColor: TsgeColor);
    function  GetFont: TsgeAnsiFont;
    procedure SetFont(AFont: TsgeAnsiFont);
    function  GetText: String;
    procedure SetText(AText: String);
    procedure SetVerticalAlign(AAlign: TsgeVerticalAlign);
    procedure SetHorizontalAlign(AAlign: TsgeHorizontalAlign);

    procedure CalculateAutosize(var NewWidth, NewHeight: Integer); override;
    procedure CheckMinimalSize(var NewWidth, NewHeight: Integer); override;

    procedure DisplayElement_CorrectPosition(RealLeft, RealTop: Integer); override;
    procedure DisplayElement_CorrectSize(Width, Height: Integer); override;
    procedure DisplayElement_CorrectVisible(Visible: Boolean); override;
    function  DisplayElement_GetVisible: Boolean; override;
    procedure DisplayElement_CorrectClipRect(Rect: TsgeClipRect); override;
    function  DisplayElement_GetClipRect: TsgeClipRect; override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; AText: String; Font: TsgeAnsiFont; Color: TsgeColor; Visible: Boolean; Parent: TsgeGUIElement);
    destructor  Destroy; override;

    property VerticalAlign: TsgeVerticalAlign read FVerticalAlign write SetVerticalAlign;
    property HorizontalAlign: TsgeHorizontalAlign read FHorizontalAlign write SetHorizontalAlign;
    property Color: TsgeColor read GetColor write Setcolor;
    property Text: String read GetText write SetText;
    property Font: TsgeAnsiFont read GetFont write SetFont;
  end;


implementation


procedure TsgeGUILabel.GetTextSize(out Width, Height: Integer);
begin
  Width := Round(FText.Font.GetTextWidth(FText.Text) * FText.Scale);
  Height := Round(FText.Font.GetTextHeight(FText.Text) * FText.Scale);
end;


procedure TsgeGUILabel.CorrectTextPosition;
var
  Size, GlobalPos: TsgeIntPoint;
  W, H: Integer;
begin
  //Размеры текста с учетом масштаба
  GetTextSize(W, H);

  //Размер элемента с учетом масштаба
  Size := GetScaleSize;

  //Реальное положение на экране
  GlobalPos := GetGlobalPos;

  //Изменить выводимый элемент
  FText.PositionX := GlobalPos.X + sgeGetHorizontalAlignOffset(FHorizontalAlign, Size.X, W);
  FText.PositionY := GlobalPos.Y + sgeGetVerticaAlignOffset(FVerticalAlign, Size.Y, H);
  FText.Scale := GetScale;
  FText.Update;
end;


function TsgeGUILabel.GetColor: TsgeColor;
begin
  Result := FText.Color;
end;


procedure TsgeGUILabel.Setcolor(AColor: TsgeColor);
begin
  FText.Color := AColor;
  FText.Update;
end;


function TsgeGUILabel.GetFont: TsgeAnsiFont;
begin
  Result := FText.Font;
end;


procedure TsgeGUILabel.SetFont(AFont: TsgeAnsiFont);
begin
  FText.Font := AFont;
  FText.Update;
end;


function TsgeGUILabel.GetText: String;
begin
  Result := FText.Text;
end;


procedure TsgeGUILabel.SetText(AText: String);
begin
  FText.Text := AText;
  FText.Update;
end;


procedure TsgeGUILabel.SetVerticalAlign(AAlign: TsgeVerticalAlign);
begin
  if FVerticalAlign = AAlign then
    Exit;

  FVerticalAlign := AAlign;

  CorrectTextPosition;
end;


procedure TsgeGUILabel.SetHorizontalAlign(AAlign: TsgeHorizontalAlign);
begin
  if FHorizontalAlign = AAlign then
    Exit;

  FHorizontalAlign := AAlign;

  CorrectTextPosition;
end;


procedure TsgeGUILabel.CalculateAutosize(var NewWidth, NewHeight: Integer);
begin
  GetTextSize(NewWidth, NewHeight);
end;


procedure TsgeGUILabel.CheckMinimalSize(var NewWidth, NewHeight: Integer);
var
  W, H: Integer;
begin
  GetTextSize(W, H);

  if NewWidth < W then
    NewWidth := W;

  if NewHeight < H then
    NewHeight := H;
end;


procedure TsgeGUILabel.DisplayElement_CorrectPosition(RealLeft, RealTop: Integer);
begin

  CorrectTextPosition;
end;


procedure TsgeGUILabel.DisplayElement_CorrectSize(Width, Height: Integer);
begin
  CorrectTextPosition;
end;


procedure TsgeGUILabel.DisplayElement_CorrectVisible(Visible: Boolean);
begin
  FText.Visible := Visible;
end;


function TsgeGUILabel.DisplayElement_GetVisible: Boolean;
begin
  Result := FText.Visible;
end;


procedure TsgeGUILabel.DisplayElement_CorrectClipRect(Rect: TsgeClipRect);
begin
  FText.ClipRect := Rect;
end;


function TsgeGUILabel.DisplayElement_GetClipRect: TsgeClipRect;
begin
  Result := FText.ClipRect;
end;


constructor TsgeGUILabel.Create(Name: String; Left, Top, Width, Height: Integer; AText: String; Font: TsgeAnsiFont; Color: TsgeColor; Visible: Boolean; Parent: TsgeGUIElement);
begin
  FVerticalAlign := vaCenter;
  FHorizontalAlign := haCenter;

  //Отображаемый элемент
  FText := TsgeDisplayElementAnsiText.Create(Left, Top, Color, Font, AText);
  FText.Add(Layer_GUI_Name);
  FText.ClipRect := sgeGetClipRect(Left, Top, Width, Height);
  FText.Clipped := True;

  inherited Create(Name, Left, Top, Width, Height, Visible, Parent);
end;


destructor TsgeGUILabel.Destroy;
begin
  FText.Delete;
  FText.Free;

  inherited Destroy;
end;


end.

