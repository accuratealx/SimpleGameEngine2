{
Пакет             Simple Game Engine 2
Файл              sgeKeyCommandJoystickInfo.pas
Версия            1.0
Создан            05.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команды на кнопках: Информация о джойстике
}
{$Include Defines.inc}

unit sgeKeyCommandJoystickInfo;

{$mode objfpc}{$H+}

interface

uses
  sgeKeyCommandJoystickButtonInfo, sgeKeyCommandJoystickPadInfo, sgeKeyCommandJoystickAxisInfo;


type
  TsgeKeyCommandJoystickInfo = class
  private
    FButtons: TsgeKeyCommandJoystickButtonsInfo;
    FPad: TsgeKeyCommandJoystickPadInfo;
    FAxis: TsgeKeyCommandJoystickAxisInfo;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    property Buttons: TsgeKeyCommandJoystickButtonsInfo read FButtons;
    property Pad: TsgeKeyCommandJoystickPadInfo read FPad;
    property Axis: TsgeKeyCommandJoystickAxisInfo read FAxis;
  end;


implementation


constructor TsgeKeyCommandJoystickInfo.Create;
begin
  FButtons := TsgeKeyCommandJoystickButtonsInfo.Create;
  FPad := TsgeKeyCommandJoystickPadInfo.Create;
  FAxis := TsgeKeyCommandJoystickAxisInfo.Create;
end;


destructor TsgeKeyCommandJoystickInfo.Destroy;
begin
  FAxis.Free;
  FPad.Free;
  FButtons.Free;
end;


procedure TsgeKeyCommandJoystickInfo.Clear;
begin
  FButtons.Clear;
  FPad.Clear;
  FAxis.Clear;
end;



end.

