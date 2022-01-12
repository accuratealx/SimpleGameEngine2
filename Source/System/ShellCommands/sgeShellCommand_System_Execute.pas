{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Execute.pas
Версия            1.0
Создан            09.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Execute;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Загрузить скрипт из файла и выполнить
  Синтаксис:
    System.execute [FileName]
  Параметры:
    FileName - Имя файла
  }
  TsgeShellCommand_System_Execute = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors, sgeFileUtils;

const
  _UNITNAME = 'ShellCommand_System_Execute';



constructor TsgeShellCommand_System_Execute.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Execute', Group_System);

  FParameters.AddString('FileName', True);
end;


function TsgeShellCommand_System_Execute.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  FnLines, Fn, SName: String;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Имя файла
  fn := Command.Part[1];

  //Имя скрипта
  SName := sgeChangeFileExt(sgeExtractFileName(Fn), '');

  //Прочитать файл
  try
    FnLines := SGE.ExtFileSystem.ReadFile(Fn);
  except
    on E: EsgeException do
      Exit(sgeCreateErrorString(_UNITNAME, Err_LoadError, Fn, E.Message));
  end;

  //Добавить скрипт в список
  SGE.ExtShell.ScriptList.Add(SName, FnLines);

  //Добавить новый элемент стека вызовов
  SGE.ExtShell.CallStack.Add(SName, 0);
end;






end.

