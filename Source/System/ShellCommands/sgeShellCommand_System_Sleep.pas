{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Sleep.pas
Версия            1.0
Создан            12.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Sleep;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Заснуть на несколько миллисекунд
  Синтаксис:
    System.Sleep [Time]
  Параметры:
    Time - Время засыпания
  }
  TsgeShellCommand_System_Sleep = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  sgeSystemUtils, sgeOSPlatform;



constructor TsgeShellCommand_System_Sleep.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Sleep', Group_System);

  //Добавить параметры
  FParameters.AddInteger('Time', True);
end;


function TsgeShellCommand_System_Sleep.Execute(Command: TsgeSimpleCommand): String;
var
  Time: Integer;
begin
  Result := inherited Execute(Command);

  if not sgeTryStrToInt(Command.GetTail(1), Time) then Time := 0;
  sgeSleep(Time);
end;






end.

