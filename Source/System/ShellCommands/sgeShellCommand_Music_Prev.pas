{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Music_Prev.pas
Версия            1.0
Создан            18.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_Music_Prev;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Переключить на предыдущую дорожку
  Синтаксис:
    Music.Prev
  }
  TsgeShellCommand_Music_Prev = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_Music_Prev';



constructor TsgeShellCommand_Music_Prev.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Prev', Group_Music);
end;


function TsgeShellCommand_Music_Prev.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try
    SGE.ExtMusicPlayer.Prev;
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantPlayMusic, '', E.Message);
  end;
end;



end.

