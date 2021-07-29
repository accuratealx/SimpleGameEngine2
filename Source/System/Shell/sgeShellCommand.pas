{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandBase.pas
Версия            1.0
Создан            30.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс команды оболочки
}
{$Include Defines.inc}

unit sgeShellCommand;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand;


type
  //Функция вызова команды
  TsgeShellCommandProc = function(Command: TsgeSimpleCommand): String;


  //Класс команды
  TsgeShellCommand = class
  private
    FName: ShortString;
    FGroup: ShortString;
    FMinParamCount: Word;
    FProc: TsgeShellCommandProc;
  public
    constructor Create(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word = 0; Group: ShortString = '');

    property Name: ShortString read FName;
    property Group: ShortString read FGroup;
    property MinParamCount: Word read FMinParamCount;
    property Proc: TsgeShellCommandProc read FProc;
  end;



implementation


constructor TsgeShellCommand.Create(Name: ShortString; Proc: TsgeShellCommandProc; MinParamCount: Word; Group: ShortString);
begin
  FName := Name;
  FGroup := Group;
  FProc := Proc;
  FMinParamCount := MinParamCount;
end;


end.

