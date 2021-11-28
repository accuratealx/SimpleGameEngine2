{
Пакет             Simple Game Engine 2
Файл              sgeEventMouse.pas
Версия            1.1
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


const
  Event_MouseDown         = 'Mouse.Down';
  Event_MouseUp           = 'Mouse.Up';
  Event_MouseLeave        = 'Mouse.Leave';
  Event_MouseEnter        = 'Mouse.Enter';
  Event_MouseDoubleClick  = 'Mouse.DoubleClick';
  Event_MouseMove         = 'Mouse.Move';
  Event_MouseScroll       = 'Mouse.Scroll';


type
  TsgeEventMouse = class(TsgeEventBase)
  private
    FX: Integer;
    FY: Integer;
    FDelta: Integer;
    FMouseButtons: TsgeMouseButtons;
    FKeyboardButtons: TsgeKeyboardButtons;

    function GetPos: TsgeIntPoint;
    function GetFloatPos: TsgeFloatPoint;
  public
    constructor Create(Name: ShortString; X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer = 0);

    procedure ChangeXY(X, Y: Integer);

    property Pos: TsgeIntPoint read GetPos;
    property FloatPos: TsgeFloatPoint read GetFloatPos;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Delta: Integer read FDelta;
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


constructor TsgeEventMouse.Create(Name: ShortString; X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer);
begin
  inherited Create(Name);

  FX := X;
  FY := Y;
  FDelta := Delta;
  FMouseButtons := MouseButtons;
  FKeyboardButtons := KeyboardButtons;
end;


procedure TsgeEventMouse.ChangeXY(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;



end.

