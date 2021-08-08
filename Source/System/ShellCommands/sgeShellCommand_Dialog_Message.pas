{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Dialog_Message.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Dialog_Message;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Вывести на экран диалоговое окно
  Синтаксис:
    Dialog.Message <Message>
  Параметры:
    Message - Текст сообщения
  }
  TsgeShellCommand_Dialog_Message = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_Dialog_Message.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Message', Group_Dialog);

  //Добавить параметры
  FParameters.AddString('Message', False);
end;


function TsgeShellCommand_Dialog_Message.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Вывести диалог
  SGE.ErrorManager.LogMessage(Command.GetTail(1));
end;






end.

