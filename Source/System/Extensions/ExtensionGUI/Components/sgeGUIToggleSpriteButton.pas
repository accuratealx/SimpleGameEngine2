{
Пакет             Simple Game Engine 2
Файл              sgeGUIToggleSpriteButton.pas
Версия            1.0
Создан            27.11.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Переключаемая спрайтовая кнопка
}
{$Include Defines.inc}

unit sgeGUIToggleSpriteButton;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventMouseDown,
  sgeGUISpriteButton;

type
  TsgeGUIToggleSpriteButton = class(TsgeGUISpriteButton)
  protected
    FToggle: Boolean;

    procedure Handler_MouseClick(Mouse: TsgeEventMouseDown); override;
  public

    property Toggle: Boolean read FToggle;
  end;


implementation


procedure TsgeGUIToggleSpriteButton.Handler_MouseClick(Mouse: TsgeEventMouseDown);
begin
  FToggle := not FToggle;
  FDisplayElement.Column := Ord(FToggle);
  FDisplayElement.Update;
  inherited Handler_MouseClick(Mouse);
end;



end.

