{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Stop.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Stop;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Остановить работу системы
  Синтаксис:
    System.Stop
  }
  TsgeShellCommand_System_Stop = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_System_Stop.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Stop', Group_System);

  //Добавить параметры
end;


function TsgeShellCommand_System_Stop.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.Stop;
end;






end.

