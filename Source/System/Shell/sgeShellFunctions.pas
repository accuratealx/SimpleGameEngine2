{
Пакет             Simple Game Engine 2
Файл              sgeShellFunctions.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Методы оболочки
}
{$Include Defines.inc}

unit sgeShellFunctions;

{$mode objfpc}{$H+}

interface


procedure sgeShellFunctions_Init(SGEObject: TObject);


implementation

uses
  sgeErrors, sgeSystemEvent, sgeSystemUtils, sgeOSPlatform, sgeSimpleCommand,
  SimpleGameEngine, sgeExtensionShell;


const
  //Ошибки
  _UNITNAME = 'ShellCommand';
  Err_VariableIsReadOnly = 'VariableIsReadOnly';


  //Группы
  Group_System    = 'System';
  Group_Shell     = 'Shell';
  Group_Variables = 'Variables';
  Group_Dialog    = 'Dialog';

type
  TsgeExtensionShellHack = class(TsgeExtensionShell);

var
  SGE: TSimpleGameEngine; //Указатель на главный класс



{$Region sgeSystem_Stop}
{
Описание:
  Остановить работу системы
Синтаксис:
  Stop
}
function sgeSystem_Stop(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  SGE.Stop;
end;
{$EndRegion}

{$Region sgeSystem_Read}
{
Описание:
  Скопировать значение строки ввода в переменную
Синтаксис:
  Read <VariableName>
Параметры:
  VariableName - Имя переменной для записи результата
}
function sgeSystem_Read(Command: TsgeSimpleCommand): String;
var
  VarName, VarValue: ShortString;
begin
  Result := '';

  //Изменить флажок в оболочке, что бы при вводе поднять флаг события
  TsgeExtensionShellHack(SGE.ExtShell).FreadMode := True;

  //Ждать пока пользователь не нажмет на Enter
  TsgeExtensionShellHack(SGE.ExtShell).FEvent.Wait;

  //Определить нужно ли записывать значение в переменную
  if Command.Count > 1 then
    begin
    //Имя переменной
    VarName := Command.Part[1];

    //Значение переменной
    VarValue := sgetrim(SGE.ExtShell.Editor.Line);

    //Изменить значение переменной
    SGE.ExtVariables.SetString(VarName, VarValue);
    end;
end;
{$EndRegion}

{$Region sgeSystem_ReadLn}
{
Описание:
  Вырезать значение строки ввода в переменную
Синтаксис:
  ReadLn <VariableName>
Параметры:
  VariableName - Имя переменной для записи результата
}
function sgeSystem_ReadLn(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  //Записать результат
  sgeSystem_Read(Command);

  //Стереть строку ввода
  SGE.ExtShell.Editor.Line := '';
end;
{$EndRegion}

{$Region sgeSystem_ReadKey}
{
Описание:
  Скопировать код символа из строки ввода в переменную
Синтаксис:
  ReadKey [VariableName] <Timeout>
Параметры:
  VariableName - Имя переменной для записи результата
  Timeout - Время ожидания нажатия, при отсутсвии ждёт бесконечно
}
function sgeSystem_ReadKey(Command: TsgeSimpleCommand): String;
var
  VarName: ShortString;
  VarValue: Integer;
  Timeout: LongInt;
begin
  Result := '';

  //Определить время ожидания
  Timeout := INFINITE;
  if Command.Count > 3 then
    sgeTryStrToInt(Command.Part[2], Timeout);

  //Поднять флаг чтения символа
  TsgeExtensionShellHack(SGE.ExtShell).FReadKeyMode := True;

  //Ждать пока пользователь не нажмет на Enter
  case TsgeExtensionShellHack(SGE.ExtShell).FEvent.Wait(Timeout) of

    //Выход по нажатию кнопки
    sewrEvent:
      begin
      VarName := Command.Part[1];                                     //Имя переменной
      VarValue := TsgeExtensionShellHack(SGE.ExtShell).FreadKeyChar;  //Значение переменной
      SGE.ExtVariables.SetInteger(VarName, VarValue);                 //Изменить значение переменной
      end;

    //Не дождался нажатия
    sewrTimeOut:
      begin
      TsgeExtensionShellHack(SGE.ExtShell).FReadKeyMode := False;     //Опустить флаг чтения символа
      end;
  end;
end;
{$EndRegion}

{$Region sgeVariable_Set}
{
Описание:
  Изменить значение переменной
Синтаксис:
  Variable.Set [Name] [Value]
Параметры:
  Name - Имя переменной
  Value - Новое значение
Заметка:
  Если переменная не существовала, то будет создана новая
}
function sgeVariable_Set(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  try
    SGE.ExtVariables.SetString(Command.Part[1], Command.Part[2]);
  except
    Result := sgeCreateErrorString(_UNITNAME, Err_VariableIsReadOnly, Command.Part[1]);
  end;
end;
{$EndRegion}

{$Region sgeVariable_Delete}
{
Описание:
  Удалить переменную
Синтаксис:
  Variable.Delete [Name]
Параметры:
  Name - Имя переменной
}
function sgeVariable_Delete(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  SGE.ExtVariables.Variables.Delete(Command.Part[1]);
end;
{$EndRegion}

{$Region sgeVariable_Clear}
{
Описание:
  Удалить все переменные
Синтаксис:
  Variable.Clear
}
function sgeVariable_Clear(Command: TsgeSimpleCommand): String;
begin
  Result := '';

  SGE.ExtVariables.Variables.Clear;
end;
{$EndRegion}





{
Описание:
  Вывести на экран диалоговое окно
Синтаксис:
  ShowMessage <MessageText>
Параметры:
  MessageText - Текст сообщения
}
function sgeDialogs_ShowMessage(Command: TsgeSimpleCommand): String;
var
  s: String;
begin
  Result := '';

  //Сообщение
  s := Command.GetTail(1);

  //Вывести диалог
  SGE.ErrorManager.LogMessage(s);
end;




/////////////////////////////////////////////////////////////
//                  Регистрация команд                     //
/////////////////////////////////////////////////////////////
procedure sgeShellFunctions_Init(SGEObject: TObject);
begin
  //Запомнить указатель
  SGE := TSimpleGameEngine(SGEObject);

  //Зарегестрировать функции
  with SGE.ExtShell.CommandList do
    begin
    //Система
    Add('Stop', @sgeSystem_Stop, 0, Group_System);
    Add('Read', @sgeSystem_Read, 0, Group_System);
    Add('ReadLn', @sgeSystem_ReadLn, 0, Group_System);
    Add('ReadKey', @sgeSystem_ReadKey, 1, Group_System);

    //Переменные
    Add('Variable.Set', @sgeVariable_Set, 2, Group_Variables);
    Add('Variable.Delete', @sgeVariable_Delete, 1, Group_Variables);
    Add('Variable.Clear', @sgeVariable_Clear, 0, Group_Variables);



    //////////Пересмотреть
    Add('ShowMessage', @sgeDialogs_ShowMessage, 0, Group_Dialog);
    end;
end;





end.

