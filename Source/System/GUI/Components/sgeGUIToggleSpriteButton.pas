{
Пакет             Simple Game Engine 2
Файл              .pas
Версия            1.0
Создан            16.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Переключаемая спрайтовая кнопка
}
{$Include Defines.inc}

unit sgeGUIToggleSpriteButton;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeSimpleParameters,
  sgeEventMouse, sgeGUIElement, sgeGUISpriteButton;


type
  TsgeGUIToggleSpriteButton = class(TsgeGUISpriteButton)
  private
    FToggle: Boolean;

    procedure SetToggle(AToggle: Boolean);

  protected
    function GetColSpriteIndex: Word; override;
    class function GetParameterSectionName: String; override;
    procedure LoadData(Data: TsgeSimpleParameters); override;

    procedure Handler_MouseClick(Mouse: TsgeEventMouse); override;
  public
    constructor Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement = nil); override;

    property Down: Boolean read FToggle write SetToggle;
  end;


implementation


procedure TsgeGUIToggleSpriteButton.SetToggle(AToggle: Boolean);
begin
  if FToggle = AToggle then Exit;

  FToggle := AToggle;
  Repaint;
end;


function TsgeGUIToggleSpriteButton.GetColSpriteIndex: Word;
begin
  Result := Ord(FToggle);
end;


class function TsgeGUIToggleSpriteButton.GetParameterSectionName: String;
begin
  Result := 'ToggleSpriteButton';
end;


procedure TsgeGUIToggleSpriteButton.LoadData(Data: TsgeSimpleParameters);
var
  ParamName: String;
begin
  inherited LoadData(Data);

  //Toggle
  ParamName := 'Toggle';
  if Data.Exist[ParamName] then
    FToggle := Data.GetValue(ParamName, False);
end;


procedure TsgeGUIToggleSpriteButton.Handler_MouseClick(Mouse: TsgeEventMouse);
begin
  FToggle := not FToggle;

  inherited Handler_MouseClick(Mouse);
end;


constructor TsgeGUIToggleSpriteButton.Create(Name: String; Left, Top, Width, Height: Integer; Parent: TsgeGUIElement);
begin
  inherited Create(Name, Left, Top, Width, Height, Parent);

  FColCount := 2;
end;


end.

