{
Пакет             Simple Game Engine 2
Файл              sgeEventWindowSize.pas
Версия            1.0
Создан            04.05.2024
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Классы событий: Окно: Изменение размеров
}
{$Include Defines.inc}

unit sgeEventWindowSize;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals+}

interface

uses
  sgeEventBase;


const
  Event_WindowSize = 'Window.Size';


type
  TsgeEventWindowSize = class(TsgeEventBase)
  private
    FWidth: Integer;
    FHeight: Integer;
  protected
    function GetName: ShortString; override;
  public
    constructor Create(Width, Height: Integer);

    function Copy: TsgeEventBase; override;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;


implementation


function TsgeEventWindowSize.GetName: ShortString;
begin
  Result := Event_WindowSize;
end;


constructor TsgeEventWindowSize.Create(Width, Height: Integer);
begin
  FWidth := Width;
  FHeight := Height;
end;


function TsgeEventWindowSize.Copy: TsgeEventBase;
begin
  Result := TsgeEventWindowSize.Create(FWidth, FHeight);
end;



end.

