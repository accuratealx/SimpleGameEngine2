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
  SimpleGameEngine,

  //Shell
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

  //Variable
  sgeShellCommand_Variable_Set,
  sgeShellCommand_Variable_Delete,
  sgeShellCommand_Variable_Clear,

  //Script
  sgeShellCommand_Script_Load,
  sgeShellCommand_Script_Delete,
  sgeShellCommand_Script_Clear,

  //Music
  sgeShellCommand_Music_Play,
  sgeShellCommand_Music_Stop,
  sgeShellCommand_Music_Next,
  sgeShellCommand_Music_Prev,
  sgeShellCommand_Music_Random,

  //WinDialogs
  sgeShellCommand_Dialog_Message;



procedure sgeShellCommands_Init(SGEObject: TObject);
begin
  //System
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

  //Variables
  TsgeShellCommand_Variable_Set.Create(SGEObject);
  TsgeShellCommand_Variable_Delete.Create(SGEObject);
  TsgeShellCommand_Variable_Clear.Create(SGEObject);

  //Script
  TsgeShellCommand_Script_Load.Create(SGEObject);
  TsgeShellCommand_Script_Delete.Create(SGEObject);
  TsgeShellCommand_Script_Clear.Create(SGEObject);

  //Music
  if TSimpleGameEngine(SGEObject).ExtMusicPlayer <> nil then
    begin
    TsgeShellCommand_Music_Play.Create(SGEObject);
    TsgeShellCommand_Music_Stop.Create(SGEObject);
    TsgeShellCommand_Music_Next.Create(SGEObject);
    TsgeShellCommand_Music_Prev.Create(SGEObject);
    TsgeShellCommand_Music_Random.Create(SGEObject);
    end;

  //WinDialogs
  TsgeShellCommand_Dialog_Message.Create(SGEObject);
end;





end.

