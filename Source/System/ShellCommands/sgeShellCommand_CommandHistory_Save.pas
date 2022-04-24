{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_CommandHistory_Save.pas
Версия            1.0
Создан            12.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_CommandHistory_Save;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Сохранить историю введённых команд в файл
  Синтаксис:
    CommandHistory.Save [FileName]
  Параметры:
    FileName - Имя файла
  }
  TsgeShellCommand_CommandHistory_Save = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_CommandHistory_Save';



constructor TsgeShellCommand_CommandHistory_Save.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Save', Group_CommandHistory);

  FParameters.AddString('FileName', True);
end;


function TsgeShellCommand_CommandHistory_Save.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Fn: String;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Определить имя файла
  Fn := Command.Part[1];

  //Сохранить в файл
  try
    SGE.ExtFileSystem.WriteFile(Fn, SGE.ExtShell.CommandHistory.ToString);
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_SaveError, '', E.Message);
  end;
end;



end.

