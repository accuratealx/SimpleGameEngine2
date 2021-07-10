{
Пакет             Simple Game Engine 2
Файл              sgeEventWindow.pas
Версия            1.2
Создан            02.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно
}
{$Include Defines.inc}

unit sgeEventWindow;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeEventBase;


const
  //Имена событий окна
  Event_WindowSize              = 'Window.Size';
  Event_WindowMaximize          = 'Window.Maximize';
  Event_WindowMinimize          = 'Window.Minimize';
  Event_WindowRestore           = 'Window.Restore';
  Event_WindowClose             = 'Window.Close';
  Event_WindowSetFocus          = 'Window.SetFocus';
  Event_WindowLostFocus         = 'Window.LostFocus';
  Event_WindowShow              = 'Window.Show';
  Event_WindowHide              = 'Window.Hide';
  Event_WindowActivate          = 'Window.Activate';
  Event_WindowDeActivate        = 'Window.DeActivate';
  Event_WindowChar              = 'Window.Char';
  Event_WindowKeyDown           = 'Window.KeyDown';
  Event_WindowKeyUp             = 'Window.KeyUp';
  Event_WindowMouseDown         = 'Window.MouseDown';
  Event_WindowMouseUp           = 'Window.MouseUp';
  Event_WindowMouseLeave        = 'Window.MouseLeave';
  Event_WindowMouseEnter        = 'Window.MouseEnter';
  Event_WindowMouseDoubleClick  = 'Window.MouseDoubleClick';
  Event_WindowMouseMove         = 'Window.MouseMove';
  Event_WindowMouseScroll       = 'Window.MouseScroll';


type
  //Размеры
  TsgeEventWindowSize = class(TsgeEventBase)
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(Width, Height: Integer);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;



  //Символы
  TsgeEventWindowChar = class(TsgeEventBase)
  private
    FChar: Char;
    FKeyboardButtons: TsgeKeyboardButtons;
  public
    constructor Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);

    property Char: Char read FChar;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;



  //Кнопки
  TsgeEventWindowKeyboard = class(TsgeEventBase)
  private
    FKey: Byte;
    FKeyboardButtons: TsgeKeyboardButtons;
  public
    constructor Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);

    property Key: Byte read FKey;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;



  //Кнопки мыши
  TsgeEventWindowMouse = class(TsgeEventBase)
  private
    FX: Integer;
    FY: Integer;
    FDelta: Integer;
    FMouseButtons: TsgeMouseButtons;
    FKeyboardButtons: TsgeKeyboardButtons;

    function GetPos: TsgeIntPoint;
  public
    constructor Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer = 0);

    property Pos: TsgeIntPoint read GetPos;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Delta: Integer read FDelta;
    property MouseButtons: TsgeMouseButtons read FMouseButtons;
    property KeyboardButtons: TsgeKeyboardButtons read FKeyboardButtons;
  end;


implementation


constructor TsgeEventWindowSize.Create(Width, Height: Integer);
begin
  FWidth := Width;
  FHeight := Height;
end;



constructor TsgeEventWindowChar.Create(Char: Char; KeyboardButtons: TsgeKeyboardButtons);
begin
  FChar := Char;
  FKeyboardButtons := KeyboardButtons;
end;



constructor TsgeEventWindowKeyboard.Create(Key: Byte; KeyboardButtons: TsgeKeyboardButtons);
begin
  FKey := Key;
  FKeyboardButtons := KeyboardButtons;
end;


function TsgeEventWindowMouse.GetPos: TsgeIntPoint;
begin
  Result.X := FX;
  Result.Y := FY;
end;


constructor TsgeEventWindowMouse.Create(X, Y: Integer; MouseButtons: TsgeMouseButtons; KeyboardButtons: TsgeKeyboardButtons; Delta: Integer);
begin
  FX := X;
  FY := Y;
  FDelta := Delta;
  FMouseButtons := MouseButtons;
  FKeyboardButtons := KeyboardButtons;
end;




end.

