{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_ReadKey.pas
Версия            1.1
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_ReadKey;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Скопировать код символа из строки ввода в переменную
  Синтаксис:
    System.ReadKey [VariableName] <Timeout>
  Параметры:
    VariableName - Имя переменной для записи результата
    Timeout - Время ожидания нажатия, при отсутсвии ждёт бесконечно
  }
  TsgeShellCommand_System_ReadKey = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeSystemUtils, sgeOSPlatform,  sgeSystemEvent, sgeExtensionShell;

type
  TsgeExtensionShellExt = class(TsgeExtensionShell);



constructor TsgeShellCommand_System_ReadKey.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'ReadKey', Group_System);

  FParameters.AddString('VariableName', True);
  FParameters.AddInteger('Timeout', False);
end;


function TsgeShellCommand_System_ReadKey.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  VarName: ShortString;
  VarValue: Integer;
  Timeout: LongInt;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Определить время ожидания
  Timeout := LongInt(INFINITE);
  if Command.Count > 2 then
    sgeTryStrToInt(Command.Part[2], Timeout);

  //Поднять флаг чтения символа
  TsgeExtensionShellExt(SGE.ExtShell).FReadKeyMode := True;

  //Перерисовать оболочку
  RepaintShell;

  //Ждать пока пользователь не нажмет на Enter
  case TsgeExtensionShellExt(SGE.ExtShell).FEvent.Wait(Timeout) of

    //Выход по нажатию кнопки
    sewrEvent:
      begin
      VarName := Command.Part[1];                                     //Имя переменной
      VarValue := TsgeExtensionShellExt(SGE.ExtShell).FreadKeyChar;  //Значение переменной
      SGE.ExtVariables.SetInteger(VarName, VarValue);                 //Изменить значение переменной
      end;

    //Не дождался нажатия
    sewrTimeOut:
      TsgeExtensionShellExt(SGE.ExtShell).FReadKeyMode := False;     //Опустить флаг чтения символа
  end;

  //Перерисовать оболочку
  RepaintShell;
end;






end.

