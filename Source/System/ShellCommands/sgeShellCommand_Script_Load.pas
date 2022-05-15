{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Script_Load.pas
Версия            1.2
Создан            26.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_Script_Load;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Загрузить скрипт из файла в список
  Синтаксис:
    Script.Load [FileName] <Name>
  Параметры:
    FileName - Имя файла
    Name - Имя скрипта
  Заметка:
    Если не указано Имя, то оно берётся из имени файла без расширения
  }
  TsgeShellCommand_Script_Load = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors, sgeFileUtils, sgeOSPlatform;

const
  _UNITNAME = 'ShellCommand_Script_Load';



constructor TsgeShellCommand_Script_Load.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Load', Group_Script);
  FParameters.AddString('FileName', True);
  FParameters.AddString('Name', False);
end;


function TsgeShellCommand_Script_Load.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  FnLines, Fn, SName: String;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Имя файла
  fn := Command.Part[1];

  //Определить имя скрипта
  if Command.Count > 2 then
    SName := Command.Part[2]
  else
    SName := sgeChangeFileExt(sgeExtractFileName(Fn), '');

  //Прочитать файл
  try
    FnLines := SGE.ExtFileSystem.ReadFile(Fn);
  except
    on E: EsgeException do
      Exit(sgeCreateErrorString(_UNITNAME, Err_LoadError, '', E.Message));
  end;

  //Добавить скрипт в список
  SGE.ExtShell.ScriptList.Add(SName, FnLines);
end;



end.

