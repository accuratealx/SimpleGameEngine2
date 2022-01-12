{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Music_Play.pas
Версия            1.0
Создан            18.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Music_Play;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Запустить проигрывание дорожки
  Синтаксис:
    Music.Play <TrackName>
  Параметры:
    TrackName - Имя дорожки
  }
  TsgeShellCommand_Music_Play = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_Music_Play';


constructor TsgeShellCommand_Music_Play.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Play', Group_Music);

  FParameters.AddString('TrackName', False);
end;


function TsgeShellCommand_Music_Play.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try
    if Command.Count > 1 then SGE.ExtMusicPlayer.Play(Command.Part[1])
      else SGE.ExtMusicPlayer.Play;
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantPlayMusic, '', E.Message);
  end;
end;



end.

