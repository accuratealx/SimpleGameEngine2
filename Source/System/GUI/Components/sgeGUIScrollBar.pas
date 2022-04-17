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
  sgeSimpleParameters,
  sgeGUITypes, sgeGUIElement;

type
  TsgeGUIScrollBar = class(TsgeGUIElement)
  private
    FOrientation: TsgeGUIOrientation;

    procedure SetOrientation(AOrientation: TsgeGUIOrientation);
  protected
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;
    procedure DrawBefore; override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;
    destructor  Destroy; override;

    property Orientation: TsgeGUIOrientation read FOrientation write SetOrientation;
  end;


implementation


procedure TsgeGUIScrollBar.SetOrientation(AOrientation: TsgeGUIOrientation);
begin
  if FOrientation = AOrientation then
    Exit;

  FOrientation := AOrientation;
  Resize;
end;


class function TsgeGUIScrollBar.GetParameterSectionName: String;
begin
  Result := 'ScrollBar';
end;


procedure TsgeGUIScrollBar.LoadData(Data: TsgeSimpleParameters);
begin
  inherited LoadData(Data);

end;


procedure TsgeGUIScrollBar.DrawBefore;
begin

end;


constructor TsgeGUIScrollBar.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FOrientation := oHorizontal;
end;


destructor TsgeGUIScrollBar.Destroy;
begin
  inherited Destroy;
end;


end.

