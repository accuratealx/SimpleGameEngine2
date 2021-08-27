{
Пакет             Simple Game Engine 2
Файл              sgeShellFunctions.pas
Версия            1.3
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
  sgeShellCommand_System_Sleep,
  sgeShellCommand_System_Write,
  sgeShellCommand_System_Screenshot,
  sgeShellCommand_System_Label,
  sgeShellCommand_System_Return,
  sgeShellCommand_System_Exit,
  sgeShellCommand_System_Procedure,
  sgeShellCommand_System_Goto,
  sgeShellCommand_System_Run,
  sgeShellCommand_System_Call,

  sgeShellCommand_Variable_Set,
  sgeShellCommand_Variable_Delete,
  sgeShellCommand_Variable_Clear,

  sgeShellCommand_Script_Load,

  sgeShellCommand_Dialog_Message;



procedure sgeShellCommands_Init(SGEObject: TObject);
begin
  TsgeShellCommand_System_Stop.Create(SGEObject);
  TsgeShellCommand_System_Read.Create(SGEObject);
  TsgeShellCommand_System_ReadLn.Create(SGEObject);
  TsgeShellCommand_System_ReadKey.Create(SGEObject);
  TsgeShellCommand_System_Sleep.Create(SGEObject);
  TsgeShellCommand_System_Write.Create(SGEObject);
  TsgeShellCommand_System_Screenshot.Create(SGEObject);
  TsgeShellCommand_System_Label.Create(SGEObject);
  TsgeShellCommand_System_Return.Create(SGEObject);
  TsgeShellCommand_System_Exit.Create(SGEObject);
  TsgeShellCommand_System_Procedure.Create(SGEObject);
  TsgeShellCommand_System_Goto.Create(SGEObject);
  TsgeShellCommand_System_Run.Create(SGEObject);
  TsgeShellCommand_System_Call.Create(SGEObject);

  TsgeShellCommand_Variable_Set.Create(SGEObject);
  TsgeShellCommand_Variable_Delete.Create(SGEObject);
  TsgeShellCommand_Variable_Clear.Create(SGEObject);

  TsgeShellCommand_Script_Load.Create(SGEObject);

  TsgeShellCommand_Dialog_Message.Create(SGEObject);
end;





end.

