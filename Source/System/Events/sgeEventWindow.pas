{
Пакет             Simple Game Engine 2
Файл              sgeEventWindow.pas
Версия            1.4
Создан            02.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно
}
{$Include Defines.inc}

unit sgeEventWindow;

{$mode objfpc}{$H+}

interface

uses
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



type
  TsgeEventWindowSize = class(TsgeEventBase)
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(Width, Height: Integer);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;


implementation


constructor TsgeEventWindowSize.Create(Width, Height: Integer);
begin
  FWidth := Width;
  FHeight := Height;
end;




end.

