{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Screenshot.pas
Версия            1.1
Создан            14.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Screenshot;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Сделать снимок экрана
  Синтаксис:
    System.Screenshot <Name>
  Параметры:
    Name - Имя скриншота
  }
  TsgeShellCommand_System_Screenshot = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_System_Screenshot';

  Err_CantCreateScreenshot  = 'CantCreateScreenshot';


constructor TsgeShellCommand_System_Screenshot.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Screenshot', Group_System);

  FParameters.AddString('Name', False);
end;


function TsgeShellCommand_System_Screenshot.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try
    SGE.ScreenShot(Command.GetPartSafe(1, ''));
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantCreateScreenshot, '', E.Message);
  end;
end;






end.

