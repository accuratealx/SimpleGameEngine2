{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Write.pas
Версия            1.1
Создан            13.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Write;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Вывести строку в оболочку
  Синтаксис:
    System.Write <Message>
  Параметры:
    Message - Текст сообщения
  }
  TsgeShellCommand_System_Write = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;


constructor TsgeShellCommand_System_Write.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Write', Group_System);
  FParameters.AddString('Message', False);
end;


function TsgeShellCommand_System_Write.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.ExtShell.LogMessage(Command.GetTail(1));

  RepaintShell;
end;



end.

