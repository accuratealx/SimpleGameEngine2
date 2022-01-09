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
  SimpleGameEngine, sgeErrors, sgeMemoryStream, sgeFileUtils, sgeOSPlatform;

const
  _UNITNAME = 'ShellCommand_System_Execute';

  Err_FileNotFound  = 'FileNotFound';
  Err_CantReadFile  = 'CantReadFile';


constructor TsgeShellCommand_System_Execute.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Execute', Group_System);

  //Добавить параметры
  FParameters.AddString('FileName', True);
end;


function TsgeShellCommand_System_Execute.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Fn, SName: String;
  MS: TsgeMemoryStream;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Имя файла
  fn := Command.Part[1];

  //Имя скрипта
  SName := sgeChangeFileExt(sgeExtractFileName(Fn), '');

  //Загрузить
  MS := TsgeMemoryStream.Create;
  try

    //Определить тип пути
    case sgeIsFullPath(Fn) of
      //Полный путь
      True:
        begin
        if not sgeFileExists(Fn) then
          Exit(sgeCreateErrorString(_UNITNAME, Err_FileNotFound, Fn));

        try
          MS.LoadFromFile(Fn);
        except
          Exit(sgeCreateErrorString(_UNITNAME, Err_CantReadFile, Fn));
        end;
        end;

      //Короткий путь
      False:
        begin
        if not SGE.ExtFileSystem.FileExists(Fn) then
          Exit(sgeCreateErrorString(_UNITNAME, Err_FileNotFound, Fn));

        try
          SGE.ExtFileSystem.ReadFile(Fn, MS);
        except
          Exit(sgeCreateErrorString(_UNITNAME, Err_CantReadFile, Fn));
        end;
        end;
    end;

    //Добавить скрипт в список
    SGE.ExtShell.ScriptList.Add(SName, MS.ToString);

    //Добавить новый элемент стека вызовов
    SGE.ExtShell.CallStack.Add(SName, 0);

  finally
    MS.Free;
  end;
end;






end.

