{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Music_Next.pas
Версия            1.0
Создан            18.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_Music_Next;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Переключить на следующую дорожку
  Синтаксис:
    Music.Next
  }
  TsgeShellCommand_Music_Next = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine, sgeErrors;

const
  _UNITNAME = 'ShellCommand_Music_Next';



constructor TsgeShellCommand_Music_Next.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Next', Group_Music);
end;


function TsgeShellCommand_Music_Next.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  try
    SGE.ExtMusicPlayer.Next;
  except
    on E: EsgeException do
      Result := sgeCreateErrorString(_UNITNAME, Err_CantPlayMusic, '', E.Message);
  end;
end;



end.

