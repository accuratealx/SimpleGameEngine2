{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_ReadLn.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_ReadLn;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Вырезать значение строки ввода в переменную
  Синтаксис:
    System.ReadLn <VariableName>
  Параметры:
    VariableName - Имя переменной для записи результата
  }
  TsgeShellCommand_System_ReadLn = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeSystemUtils, sgeExtensionShell;

type
  TsgeExtensionShellHack = class(TsgeExtensionShell);


constructor TsgeShellCommand_System_ReadLn.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'ReadLn', Group_System);

  FParameters.AddString('VariableName', False);
end;


function TsgeShellCommand_System_ReadLn.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  VarName, VarValue: ShortString;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Изменить флажок в оболочке, что бы при вводе поднять флаг события
  TsgeExtensionShellHack(SGE.ExtShell).FreadMode := True;

  //Перерисовать оболочку
  TsgeExtensionShellHack(SGE.ExtShell).RepaintThread;

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

  //Удалить строку редактора
  SGE.ExtShell.Editor.Line := '';

  //Перерисовать оболочку
  TsgeExtensionShellHack(SGE.ExtShell).RepaintThread;
end;






end.

