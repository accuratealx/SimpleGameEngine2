{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Script_Load.pas
Версия            1.0
Создан            26.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
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
  SimpleGameEngine, sgeErrors, sgeMemoryStream, sgeFileUtils, sgeOSPlatform;

const
  _UNITNAME = 'ShellCommand_Script_Load';

  Err_FileNotFound  = 'FileNotFound';
  Err_CantReadFile  = 'CantReadFile';
  Err_CantLoadFile  = 'CantLoadFile';


constructor TsgeShellCommand_Script_Load.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Load', Group_Script);

  //Добавить параметры
  FParameters.AddString('FileName', True);
  FParameters.AddString('Name', False);
end;


function TsgeShellCommand_Script_Load.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Fn, S: String;
  MS: TsgeMemoryStream;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  { TODO : Добавить проверку на существование скрипта или сделать обновление }

  //Имя файла
  fn := Command.Part[1];

  //Определить имя скрипта
  if Command.Count > 2 then S := Command.Part[2] else S := sgeChangeFileExt(sgeExtractFileName(Fn), '');

  //Загрузить
  MS := TsgeMemoryStream.Create;
  try
  //Определить тип пути
  case sgeIsFullPath(Fn) of
    //Полный путь
    True:
      begin
      if not sgeFileExists(Fn) then
        begin
        Result := sgeCreateErrorString(_UNITNAME, Err_FileNotFound, Fn);
        Exit;
        end;

      try
        MS.LoadFromFile(Fn);
      except
        Result := sgeCreateErrorString(_UNITNAME, Err_CantReadFile, Fn);
        Exit;
      end;
      end;

    //Короткий путь
    False:
      begin
      if not SGE.ExtFileSystem.FileExists(Fn) then
        begin
        Result := sgeCreateErrorString(_UNITNAME, Err_FileNotFound, Fn);
        Exit;
        end;

      try
        SGE.ExtFileSystem.ReadFile(Fn, MS);
      except
        Result := sgeCreateErrorString(_UNITNAME, Err_CantReadFile, Fn);
        Exit;
      end;
      end;
  end;


  //Добавить скрипт в список
  try
    SGE.ExtShell.ScriptList.Add(S, MS.ToString);
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantLoadFile, Fn, E.Message);
  end;


  finally
    MS.Free;
  end;
end;






end.

