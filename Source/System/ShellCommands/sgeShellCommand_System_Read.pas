{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Read.pas
Версия            1.0
Создан            31.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Read;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Скопировать значение строки ввода в переменную
  Синтаксис:
    System.Read <VariableName>
  Параметры:
    VariableName - Имя переменной для записи результата
  }
  TsgeShellCommand_System_Read = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeExtensionShell, sgeSystemUtils;

type
  TsgeExtensionShellHack = class(TsgeExtensionShell);



constructor TsgeShellCommand_System_Read.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Read', Group_System);

  FParameters.AddString('VariableName', False);
end;


function TsgeShellCommand_System_Read.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  VarName, VarValue: ShortString;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

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






end.

