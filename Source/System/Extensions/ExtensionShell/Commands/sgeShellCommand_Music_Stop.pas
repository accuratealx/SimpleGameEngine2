{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_Music_Stop.pas
Версия            1.0
Создан            18.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда оболочки
}
{$Include Defines.inc}

unit sgeShellCommand_Music_Stop;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommand;


type
  {
  Описание:
    Остановить работу проигрывателя
  Синтаксис:
    Music.Stop
  }
  TsgeShellCommand_Music_Stop = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);

    function Execute(Command: TsgeSimpleCommand): String; override;
  end;


implementation

uses
  SimpleGameEngine;



constructor TsgeShellCommand_Music_Stop.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Stop', Group_Music);
end;


function TsgeShellCommand_Music_Stop.Execute(Command: TsgeSimpleCommand): String;
var
  SGE: TSimpleGameEngine;
begin
  Result := inherited Execute(Command);
  SGE := TSimpleGameEngine(FSGE);

  SGE.ExtMusicPlayer.Stop;
end;



end.

