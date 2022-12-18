{
Пакет             Simple Game Engine 2
Файл              sgeGUISpriteView.pas
Версия            1.0
Создан            07.08.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Просмотр спрайтов
}
{$Include Defines.inc}

unit sgeGUISpriteView;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes, sgeSimpleParameters, sgeGraphicSprite, sgeTemplateCollection,
  sgeGUIElement, sgeGUIScrollBar, sgeEventMouse,
  sgeGUIPropertyBackground, sgeGUIPropertyIntRect, sgeGUIPropertyIntPoint;

type
  //Элемент
  TsgeGUISpriteViewItem = class
  private
    FDefaultSprite: TsgeGraphicSprite;
  private
    FOwner: TsgeGUIElement;
    FSprite: TsgeGraphicSprite;
    FData: TObject;

    FX: Single;
    FY: Single;
    FVisible: Boolean;

    procedure SetSprite(ASprite: TsgeGraphicSprite);
  public
    constructor Create(Owner: TsgeGUIElement; Sprite: TsgeGraphicSprite; Data: TObject = nil);

    property Sprite: TsgeGraphicSprite read FSprite write SetSprite;
    property Data: TObject read FData write FData;

    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property Visible: Boolean read FVisible write FVisible;
  end;


  //Список элементов
  TsgeGUISpriteViewItemList = class(specialize TsgeTemplateCollection<TsgeGUISpriteViewItem>);


  //
  TsgeGUISpriteView = class(TsgeGUIElement)
  private
    FSlider: TsgeGUIScrollBar;
    FBackground: TsgeGUIPropertyBackgroundExt;
    FSliderWidth: Integer;
    FPadding: TsgeGUIPropertyIntRectExt;
    FItemIndent: TsgeGUIPropertyIntPointExt;
    FItemSize: TsgeGUIPropertyIntPointExt;

    FItemList: TsgeGUISpriteViewItemList;

    procedure SetSliderWidth(AWidth: Integer);
    function  GetBackground: TsgeGUIPropertyBackground;
    function  GetSlider: TsgeGUIPropertyBackground;
    function  GetPadding: TsgeGUIPropertyIntRect;
    function  GetItemIndent: TsgeGUIPropertyIntPoint;
    function  GetItemSize: TsgeGUIPropertyIntPoint;

    function  GetTotalContentHeight: Integer;                       //Посчитать общую высоту элементов с отступами
    procedure ArrangeScrollBar;
    procedure CorrectSliderSize;                                    //Поправить размер ползунка

    function  GetItemAreaWidth: Integer;                            //Ширина вывода спрайтов
    function  GetItemAreaHeight: Integer;                           //Высота вывода спрайтов
    function  GetItemCountPerLine: Integer;                         //Узнать количество элементов в строке
    function  GetItemTotalLines: Integer;                           //Узнать количество строк
    procedure ArrangeItems;                                         //Расставить элементы

    function GetItemCount: Integer;
    function GetItem(Index: Integer): TsgeGUISpriteViewItem;
  protected
    procedure Handler_MouseScroll(Mouse: TsgeEventMouse); override;

    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;

    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor Destroy; override;

    //Элементы
    procedure ItemAdd(Sprite: TsgeGraphicSprite; Data: TObject = nil);
    procedure ItemDelete(Index: Integer);
    procedure ItemClear;

    property Background: TsgeGUIPropertyBackground read GetBackground;
    property Slider: TsgeGUIPropertyBackground read GetSlider;
    property SliderWidth: Integer read FSliderWidth write SetSliderWidth;
    property Padding: TsgeGUIPropertyIntRect read GetPadding;
    property ItemIndent: TsgeGUIPropertyIntPoint read GetItemIndent;
    property ItemSize: TsgeGUIPropertyIntPoint read GetItemSize;

    property ItemCount: Integer read GetItemCount;
    property Item[Index: Integer]: TsgeGUISpriteViewItem read GetItem;
  end;


implementation

uses
  sgeCorePointerUtils, sgeGraphicColor, sgeGraphic, Math;


procedure TsgeGUISpriteViewItem.SetSprite(ASprite: TsgeGraphicSprite);
begin
  if ASprite = nil then
    ASprite := FDefaultSprite;

  FSprite := ASprite;

  FOwner.Repaint;
end;


constructor TsgeGUISpriteViewItem.Create(Owner: TsgeGUIElement; Sprite: TsgeGraphicSprite; Data: TObject);
begin
  //Сделать проверку Owner на nil

  //Найти указатель на дефолтный спрайт
  FDefaultSprite := sgeCorePointer_GetSGE.ExtResourceList.Default.Sprite;

  FOwner := Owner;
  FData := Data;

  FVisible := True;
  FX := 0;
  FY := 0;

  if Sprite = nil then
    Sprite := FDefaultSprite;
  FSprite := Sprite;
end;


procedure TsgeGUISpriteView.SetSliderWidth(AWidth: Integer);
begin
  if AWidth < 1 then
    AWidth := 1;
  if FSlider.Width = AWidth then
    Exit;

  FSliderWidth := AWidth;

  FSlider.Width := FSliderWidth;

  ArrangeScrollBar;

  ArrangeItems;

  Repaint;
end;


function TsgeGUISpriteView.GetBackground: TsgeGUIPropertyBackground;
begin
  Result := FBackground;
end;


function TsgeGUISpriteView.GetSlider: TsgeGUIPropertyBackground;
begin
  Result := FSlider.Slider;
end;


function TsgeGUISpriteView.GetPadding: TsgeGUIPropertyIntRect;
begin
  Result := FPadding;
end;


function TsgeGUISpriteView.GetItemIndent: TsgeGUIPropertyIntPoint;
begin
  Result := FItemIndent;
end;


function TsgeGUISpriteView.GetItemSize: TsgeGUIPropertyIntPoint;
begin
  Result := FItemSize;
end;


function TsgeGUISpriteView.GetTotalContentHeight: Integer;
begin
  Result := (GetItemTotalLines * FItemSize.Y) + ((GetItemTotalLines - 1) * FItemIndent.Y) + FPadding.Top + FPadding.Bottom;
end;


procedure TsgeGUISpriteView.ArrangeScrollBar;
begin
  FSlider.Top := 0;
  FSlider.Left := FWidth - FSlider.Width;
  FSlider.Height := FHeight;
  FSlider.Max := GetTotalContentHeight - FHeight;
end;


procedure TsgeGUISpriteView.CorrectSliderSize;
const
  MinSliderWidth = 0;
var
  Size: Integer;
begin
  Size := Round(((FHeight - MinSliderWidth) / GetTotalContentHeight) * (FHeight - MinSliderWidth) + MinSliderWidth);
  FSlider.SliderSize := Size;
end;


function TsgeGUISpriteView.GetItemAreaWidth: Integer;
begin
  Result := FWidth - FSlider.Width - FPadding.Left - FPadding.Right;
end;


function TsgeGUISpriteView.GetItemAreaHeight: Integer;
begin
  Result := FHeight - FPadding.Top - FPadding.Bottom;
end;


function TsgeGUISpriteView.GetItemCountPerLine: Integer;
var
  i, W, W2, TW, AreaWidth: Integer;
begin
  Result := 1;

  AreaWidth := GetItemAreaWidth;

  for i := 1 to ItemCount - 1 do
  begin
    W := i * FItemSize.X;
    W2 := FItemIndent.X * (i - 1);

    TW := W + W2;

    if TW < AreaWidth then
      Result := i
    else
      Exit;
  end;
end;


function TsgeGUISpriteView.GetItemTotalLines: Integer;
begin
  Result := Ceil(FItemList.Count / GetItemCountPerLine);
end;


procedure TsgeGUISpriteView.ArrangeItems;
var
  AreaW, ItemsPerLine, i, X1, Y1, DX: Integer;
begin
  //Спрайтов на линию
  ItemsPerLine := GetItemCountPerLine;

  //Ширина выводимой области
  AreaW := GetItemAreaWidth;

  //Для выравнивания по центру
  DX := (AreaW - ((ItemsPerLine * FItemSize.X) + ((ItemsPerLine - 1) * FItemIndent.X))) div 2;

  Y1 := FPadding.Top;
  X1 := DX;

  for i := 0 to ItemCount - 1 do
  begin
    FItemList.Item[i].X := FPadding.Left + X1;
    FItemList.Item[i].Y := Y1;
    X1 := X1 + FItemSize.X + FItemIndent.X;

    if (i + 1) mod ItemsPerLine = 0 then
    begin
      X1 := DX;
      Y1 := Y1 + FItemSize.Y + FItemIndent.Y;
    end;
  end;

  //Поправить ползунок
  CorrectSliderSize;

  Repaint;
end;


function TsgeGUISpriteView.GetItemCount: Integer;
begin
  Result := FItemList.Count;
end;


function TsgeGUISpriteView.GetItem(Index: Integer): TsgeGUISpriteViewItem;
begin
  Result := FItemList.Item[Index];
end;


procedure TsgeGUISpriteView.Handler_MouseScroll(Mouse: TsgeEventMouse);
begin
  FSlider.MouseHandler(emetScroll, Mouse);

  inherited Handler_MouseScroll(Mouse);
end;


procedure TsgeGUISpriteView.SetWidth(AWidth: Integer);
begin
  inherited SetWidth(AWidth);

  LockUpdate;
  CorrectSliderSize;
  ArrangeScrollBar;
  UnLockUpdate;
  Repaint;
end;


procedure TsgeGUISpriteView.SetHeight(AHeight: Integer);
begin
  inherited SetHeight(AHeight);

  LockUpdate;
  CorrectSliderSize;
  ArrangeScrollBar;
  UnLockUpdate;
  Repaint;
end;


class function TsgeGUISpriteView.GetParameterSectionName: String;
begin
  Result := 'SpriteView';
end;


procedure TsgeGUISpriteView.LoadData(Data: TsgeSimpleParameters);
var
  ParamName: String;
begin
  inherited LoadData(Data);

  //SliderWidth
  ParamName := 'SliderWidth';
  if Data.Exist[ParamName] then
    SetSliderWidth(Data.GetValue(ParamName, 20));

  //ItemSize
  FItemSize.LoadParameters(Data, 'ItemSize.');

  //ItemIndent
  FItemIndent.LoadParameters(Data, 'ItemIndent.');

  //Padding
  FPadding.LoadParameters(Data, 'Padding.');

  //Background
  FBackground.LoadParameters(Data, 'Background.');

  //Slider
  (FSlider.Slider as TsgeGUIPropertyBackgroundExt).LoadParameters(Data, 'Slider.');

  FSlider.Repaint;
end;


procedure TsgeGUISpriteView.DrawBefore;
var
  i: Integer;
  DrawOpt: TsgeGraphicDrawOptions;
begin
  //Фон
  FBackground.Draw(sgeGetFloatRect(0, 0, FWidth, FHeight));

  //Элементы
  DrawOpt := DefaultDrawOptions;
  DrawOpt.CoordinateType := gctNormal;

  for i := 0 to ItemCount - 1 do
    with sgeCorePointer_GetSGE.ExtGraphic.Graphic do
    begin
      DrawOpt.Sprite := Item[i].Sprite;
      DrawOpt.Rect.X1 := Item[i].X;
      DrawOpt.Rect.Y1 := Item[i].Y - FSlider.Position;
      DrawOpt.Rect.X2 := FItemSize.X;
      DrawOpt.Rect.Y2 := FItemSize.Y;

      DrawSprite(DrawOpt);
    end;
end;


constructor TsgeGUISpriteView.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FItemList := TsgeGUISpriteViewItemList.Create(True);

  LockUpdate;
  FSliderWidth := 20;

  FPadding := TsgeGUIPropertyIntRectExt.Create(Self);
  FPadding.Left := 10;
  FPadding.Top := 10;
  FPadding.Right := 10;
  FPadding.Bottom := 10;

  FItemIndent := TsgeGUIPropertyIntPointExt.Create(Self);
  FItemIndent.X := 10;
  FItemIndent.Y := 30;

  FItemSize := TsgeGUIPropertyIntPointExt.Create(Self);
  FItemSize.X := 120;
  FItemSize.Y := 120;

  FBackground := TsgeGUIPropertyBackgroundExt.Create(Self);

  FSlider := TsgeGUIScrollBar.Create('Slider', 0, 0, 20, 100, Self);
  FSlider.Orientation := oVertical;
  FSlider.Background.Color.Color := cTransparentBlack;
  FSlider.Slider.Color.Color := cWhite;
  FSlider.Step := 30;

  SetSliderWidth(FSliderWidth);

  CorrectSliderSize;
  ArrangeScrollBar;

  UnLockUpdate;

  Repaint;
end;


destructor TsgeGUISpriteView.Destroy;
begin
  FItemList.Free;
  FItemIndent.Free;
  FItemSize.Free;
  FPadding.Free;
  FBackground.Free;
  FSlider.Free;

  inherited Destroy;
end;


procedure TsgeGUISpriteView.ItemAdd(Sprite: TsgeGraphicSprite; Data: TObject);
var
  AItem: TsgeGUISpriteViewItem;
begin
  AItem := TsgeGUISpriteViewItem.Create(Self, Sprite, Data);
  FItemList.Add(AItem);

  ArrangeScrollBar;
  ArrangeItems;
end;


procedure TsgeGUISpriteView.ItemDelete(Index: Integer);
begin
  FItemList.Delete(Index);

  ArrangeScrollBar;
  ArrangeItems;
end;


procedure TsgeGUISpriteView.ItemClear;
begin
  FItemList.Clear;

  ArrangeScrollBar;

  Repaint;
end;


end.

