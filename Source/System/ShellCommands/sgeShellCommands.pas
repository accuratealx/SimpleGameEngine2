{
Пакет             Simple Game Engine 2
Файл              sgeShellFunctions.pas
Версия            1.1
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Методы оболочки
}
{$Include Defines.inc}

unit sgeShellCommands;

{$mode objfpc}{$H+}

interface


procedure sgeShellCommands_Init(SGEObject: TObject);


implementation

uses
  sgeShellCommand_System_Stop,
  sgeShellCommand_System_Read,
  sgeShellCommand_System_ReadLn,
  sgeShellCommand_System_ReadKey,

  sgeShellCommand_Variable_Set,
  sgeShellCommand_Variable_Delete,
  sgeShellCommand_Variable_Clear,

  sgeShellCommand_Dialog_Message;



procedure sgeShellCommands_Init(SGEObject: TObject);
begin
  TsgeShellCommand_System_Stop.Create(SGEObject);
  TsgeShellCommand_System_Read.Create(SGEObject);
  TsgeShellCommand_System_ReadLn.Create(SGEObject);
  TsgeShellCommand_System_ReadKey.Create(SGEObject);

  TsgeShellCommand_Variable_Set.Create(SGEObject);
  TsgeShellCommand_Variable_Delete.Create(SGEObject);
  TsgeShellCommand_Variable_Clear.Create(SGEObject);

  TsgeShellCommand_Dialog_Message.Create(SGEObject);
end;





end.

