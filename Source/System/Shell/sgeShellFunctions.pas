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
  SimpleGameEngine, sgeSimpleCommand,
  sgeExtensionShell, sgeSystemUtils;


const
  Group_System = 'System';
  Group_Dialog = 'Dialog';

type
  TsgeExtensionShellHack = class(TsgeExtensionShell);

var
  SGE: TSimpleGameEngine; //Указатель на главный класс



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


{
Описание:
  Записать значение строки ввода в переменную
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
  TsgeExtensionShellHack(SGE.ExtShell).SetReadMode;

  //Ждать пока пользователь не нажмет на Enter
  TsgeExtensionShellHack(SGE.ExtShell).MyEvent.Wait;

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


{
Описание:
  Записать значение строки ввода в переменную и удалить ввод
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
    Add('Stop', @sgeSystem_Stop, 0, Group_System);
    Add('Read', @sgeSystem_Read, 0, Group_System);
    Add('ReadLn', @sgeSystem_ReadLn, 0, Group_System);

    Add('ShowMessage', @sgeDialogs_ShowMessage, 0, Group_Dialog);
    end;
end;





end.

