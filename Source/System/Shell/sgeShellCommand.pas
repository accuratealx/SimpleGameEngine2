{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandBase.pas
Версия            1.2
Создан            30.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Базовый класс команды оболочки
}
{$Include Defines.inc}

unit sgeShellCommand;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleCommand, sgeShellCommandParameterList;

const
  //Группы
  Group_System    = 'System';
  Group_Music     = 'Music';
  Group_Shell     = 'Shell';
  Group_Variables = 'Variable';
  Group_Script    = 'Script';
  Group_Dialog    = 'Dialog';

  //Ошибки
  Err_CantChangeValue       = 'CantChangeValue';
  Err_CantCreateScreenshot  = 'CantCreateScreenshot';
  Err_CantFindProcedureEnd  = 'CantFindProcedureEnd';
  Err_CantPlayMusic         = 'CantPlayMusic';
  Err_LabelNotFound         = 'LabelNotFound';
  Err_LoadError             = 'LoadError';
  Err_ProcedureNotFound     = 'ProcedureNotFound';
  Err_ScriptNotFound        = 'ScriptNotFound';
  Err_VariableIsReadOnly    = 'VariableIsReadOnly';


type
  //Класс команды
  TsgeShellCommand = class
  protected
    FSGE: TObject;

    //Классы
    FParameters: TsgeShellCommandParameterList;

    //Параметры
    FName: ShortString;
    FGroup: ShortString;
    FMinParamCount: Byte;

    procedure RepaintShell;
  public
    constructor Create(SGEObject: TObject; Name: ShortString; Group: ShortString = '');
    destructor  Destroy; override;

    procedure AfterConstruction; override;

    function GetFullName: String;
    function Execute(Command: TsgeSimpleCommand): String; virtual;

    property Name: ShortString read FName;
    property Group: ShortString read FGroup;
    property MinParamCount: Byte read FMinParamCount;
    property Parameters: TsgeShellCommandParameterList read FParameters;
  end;



implementation

uses
  SimpleGameEngine, sgeExtensionShell;

type
  TsgeExtensionShellExt = class(TsgeExtensionShell);


procedure TsgeShellCommand.RepaintShell;
begin
  TsgeExtensionShellExt(TSimpleGameEngine(FSGE).ExtShell).RepaintThread;
end;


constructor TsgeShellCommand.Create(SGEObject: TObject; Name: ShortString; Group: ShortString);
var
  SGE: TSimpleGameEngine absolute SGEObject;
begin
  //Сохранить указатель
  FSGE := SGEObject;

  //Создать список параметров
  FParameters := TsgeShellCommandParameterList.Create(True);

  //Задать параметры
  FName := Name;
  FGroup := Group;

  //Зарегестрировать команду
  SGE.ExtShell.CommandList.Add(Self);
end;


destructor TsgeShellCommand.Destroy;
begin
  //Удалить список параметров
  FParameters.Free;
end;


procedure TsgeShellCommand.AfterConstruction;
var
  i: Integer;
begin
  //Посчитать минимальное количество параметров
  for i := 0 to FParameters.Count - 1 do
    if FParameters.Item[i].Required then Inc(FMinParamCount);
end;


function TsgeShellCommand.GetFullName: String;
const
  SEPARATOR = '.';
begin
  if FGroup <> '' then Result := FGroup + SEPARATOR + FName else Result := FName;
end;


function TsgeShellCommand.Execute(Command: TsgeSimpleCommand): String;
begin
  //Заглушка
  Result := '';
end;



end.

