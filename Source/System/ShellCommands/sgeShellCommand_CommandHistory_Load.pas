{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_CommandHistory_Load.pas
Версия            1.1
Создан            12.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_CommandHistory_Load;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Загрузить историю введённых команд из файла
  Синтаксис:
    CommandHistory.Load [FileName]
  Параметры:
    FileName - Имя файла
  }
  TsgeShellCommand_CommandHistory_Load = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_CommandHistory_Load';



constructor TsgeShellCommand_CommandHistory_Load.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Load', Group_CommandHistory);
  FParameters.AddString('FileName', True);
end;


function TsgeShellCommand_CommandHistory_Load.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
  Fn, s: String;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  //Определить имя файла
  Fn := Command.Part[1];

  //Прочитать из файла
  try
    s := SGE.ExtFileSystem.ReadFile(Fn);
  except
    on E: EsgeException do
      Exit(sgeCreateErrorString(_UNITNAME, Err_LoadError, '', E.Message));
  end;

  //Изменить историю
  SGE.ExtShell.CommandHistory.FromString(s);
end;



end.

