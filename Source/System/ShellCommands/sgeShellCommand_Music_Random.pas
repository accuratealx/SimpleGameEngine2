{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Music_Random.pas
Версия            1.0
Создан            18.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Music_Random;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Переключить на случайную дорожку
  Синтаксис:
    Music.Random
  }
  TsgeShellCommand_Music_Random = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_Music_Random';


constructor TsgeShellCommand_Music_Random.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Random', Group_Music);
end;


function TsgeShellCommand_Music_Random.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try

    SGE.ExtMusicPlayer.Random;

  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantPlayMusic, '', E.Message);
  end;
end;



end.

