{
Пакет             Simple Game Engine 2
Файл              sgeGUIRect.pas
Версия            1.0
Создан            16.07.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Прямоугольник
}
{$Include Defines.inc}

unit sgeGUIRect;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeSimpleParameters,
  sgeGUIElement, sgeGUIPropertyColor, sgeGUIPropertyBorder;


type
  TsgeGUIRect = class(TsgeGUIElement)
  private
    FBGColor: TsgeGUIPropertyColorExt;                              //Цвет фона
    FBorder: TsgeGUIPropertyBorderExt;                              //Рамка

    function GetBGColor: TsgeGUIPropertyColor;
    function GetBorder: TsgeGUIPropertyBorder;

  protected
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;

  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property BGColor: TsgeGUIPropertyColor read GetBGColor;
    property Border: TsgeGUIPropertyBorder read GetBorder;
  end;



implementation

uses
  sgeCorePointerUtils;


function TsgeGUIRect.GetBGColor: TsgeGUIPropertyColor;
begin
  Result := FBGColor;
end;


function TsgeGUIRect.GetBorder: TsgeGUIPropertyBorder;
begin
  Result := FBorder;
end;


class function TsgeGUIRect.GetParameterSectionName: String;
begin
  Result := 'Rect';
end;


procedure TsgeGUIRect.LoadData(Data: TsgeSimpleParameters);
begin
  inherited LoadData(Data);

  //BGColor
  FBGColor.LoadParameters(Data, 'BGColor.');

  //Border
  FBorder.LoadParameters(Data, 'Border.');
end;


procedure TsgeGUIRect.DrawBefore;
begin
  with sgeCorePointer_GetSGE.ExtGraphic do
  begin
    //Вывод фона
    Graphic.BGColor := FBGColor.Color;
    Graphic.EraseBG;

    //Вывод рамки
    Graphic.Color := FBorder.Color.Color;
    Graphic.LineStipple := True;
    Graphic.LineWidth := FBorder.Width;
    Graphic.SetLineStipple(FBorder.Stipple.Scale, FBorder.Stipple.Mode);
  end;
end;


constructor TsgeGUIRect.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FBGColor := TsgeGUIPropertyColorExt.Create(Self);
  FBorder := TsgeGUIPropertyBorderExt.Create(Self);

  Repaint;
end;


destructor TsgeGUIRect.Destroy;
begin
  FBorder.Free;
  FBGColor.Free;

  inherited Destroy;
end;



end.

