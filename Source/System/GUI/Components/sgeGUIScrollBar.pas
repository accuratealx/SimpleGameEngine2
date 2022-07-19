{
Пакет             Simple Game Engine 2
Файл              sgeGUIScrollBar.pas
Версия            1.0
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
  sgeGUIPropertyBackground,
  sgeGUIElement;

type
  //Ориентация
  TsgeGUIOrientation = (oHorizontal, oVertical);


  TsgeGUIScrollBar = class(TsgeGUIElement)
  private
    FOrientation: TsgeGUIOrientation;                               //Положение в пространстве
    FPageSize: Integer;
    FPosition: Integer;                                             //Положение
    FMin: Integer;                                                  //Наименьшее значение
    FMax: Integer;                                                  //Наибольшее значение
    FStep: Integer;                                                 //Шаг по щелчку кнопки
    FPageStep: Integer;                                             //Шаг по щелчку между кнопкой и ползунком

    FBackground: TsgeGUIPropertyBackgroundExt;                      //Фон
    FSlider: TsgeGUIPropertyBackgroundExt;                          //Ползунок


    procedure SetOrientation(AOrientation: TsgeGUIOrientation);
    procedure SetPosition(APosition: Integer);                      //Изменить положение
    procedure SetMin(AMin: Integer);                                //Изменить нижний порог
    procedure SetMax(AMax: Integer);                                //Изменить верхний порог
    procedure SetStep(AStep: Integer);                              //Изменить шаг по кнопке
    procedure SetPageSize(ASize: Integer);                          //Изменить шаг страницы

    function GetBackground: TsgeGUIPropertyBackground;
    function GetSlider: TsgeGUIPropertyBackground;
  protected
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
    property PageSize: Integer read FPageSize write SetPageSize;
  end;


implementation

uses
  sgeCorePointerUtils, sgeGraphicColor, sgeGraphic;


procedure TsgeGUIScrollBar.SetOrientation(AOrientation: TsgeGUIOrientation);
var
  d: Integer;
begin
  if FOrientation = AOrientation then
    Exit;

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

end;


procedure TsgeGUIScrollBar.SetMin(AMin: Integer);
begin

end;


procedure TsgeGUIScrollBar.SetMax(AMax: Integer);
begin

end;


procedure TsgeGUIScrollBar.SetStep(AStep: Integer);
begin

end;


procedure TsgeGUIScrollBar.SetPageSize(ASize: Integer);
begin

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
  FSlider.Draw(sgeGetFloatRect(10, 0, 150, FHeight));

  with sgeCorePointer_GetSGE.ExtGraphic do
  begin
    Graphic.Color := cRed;
    Graphic.PoligonMode := gpmLine;
    Graphic.doCoordinateType := gctClassic;
    Graphic.DrawRect(10, 0, 150, FHeight);
    Graphic.PoligonMode := gpmFill;
  end;
end;


constructor TsgeGUIScrollBar.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  //Классы
  FBackground := TsgeGUIPropertyBackgroundExt.Create(Self);
  FSlider := TsgeGUIPropertyBackgroundExt.Create(Self);

  //Свойства
  FOrientation := oHorizontal;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FStep := 1;
  FPageStep := 10;

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

