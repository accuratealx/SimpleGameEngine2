{
Пакет             Simple Game Engine 2
Файл              sgeEventMouse.pas
Версия            1.3
Создан            14.09.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Мышь
}
{$Include Defines.inc}

unit sgeEventMouse;

{$mode objfpc}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeTypes,
  sgeEventBase;


type
  TsgeEventMouse = class(TsgeEventBase)
  private
    function GetPos: TsgeIntPoint;
    function GetFloatPos: TsgeFloatPoint;
  protected
    FX: Integer;
    FY: Integer;
    FMouseButtons: TsgeMouseButtons;
    FKeyboardButtons: TsgeKeyboardButtons;

    function GetName: ShortString; override;
  public
    constructor Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons);

    function Copy: TsgeEventBase; override;

    property Pos: TsgeIntPoint read GetPos;
    property FloatPos: TsgeFloatPoint read GetFloatPos;
    property X: Integer read FX;
    property Y: Integer read FY;
    property MouseButtons: TsgeMouseButtons read FMouseButtons;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;



implementation



function TsgeEventMouse.GetPos: TsgeIntPoint;
begin
  Result.X := FX;
  Result.Y := FY;
end;


function TsgeEventMouse.GetFloatPos: TsgeFloatPoint;
begin
  Result.X := FX;
  Result.Y := FY;
end;


function TsgeEventMouse.GetName: ShortString;
begin
  Result := 'Mouse';
end;


constructor TsgeEventMouse.Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons);
begin
  FX := X;
  FY := Y;
  FMouseButtons := MouseButtons;
  FKeyboardButtons := KeyboardButtons;
end;


function TsgeEventMouse.Copy: TsgeEventBase;
begin
  Result := TsgeEventMouse.Create(FX, FY, FMouseButtons, FKeyboardButtons);
end;



end.

