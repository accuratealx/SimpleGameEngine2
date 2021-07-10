{
Пакет             Simple Game Engine 2
Файл              sgeErrorManager.pas
Версия            1.0
Создан            25.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс: Обработчик ошибок
}
{$Include Defines.inc}

unit sgeErrorManager;

{$mode objfpc}{$H+}

interface

uses
  sgeCriticalSection, sgeSystemConsole,
  Windows, SysUtils;


const
  Object_ErrorManager = 'ErrorManager';


type
  TsgeErrorManager = class
  private
    FCS: TsgeCriticalSection;
    FConsole: TsgeSystemConsole;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    procedure ShowMessage(Message: String);       //Показать диалоговое окно

    procedure LogMessage(Message: String);        //Обработка сообщения
    procedure LogError(Error: String);            //Обработка ошибки с разбором строк
  end;



implementation

uses
  sgeOSPlatform;



constructor TsgeErrorManager.Create;
begin
  FCS := sgeCriticalSection.TsgeCriticalSection.Create;

  FConsole := TsgeSystemConsole.Create;
  FConsole.InputCodePage := CP_Windows1251;
  FConsole.OutputCodePage := CP_Windows1251;
end;


destructor TsgeErrorManager.Destroy;
begin
  FConsole.Free;

  FCS.Free;
end;


procedure TsgeErrorManager.Clear;
begin
  FConsole.Clear();
end;


procedure TsgeErrorManager.ShowMessage(Message: String);
begin
  sgeShowMessage(Message, 'Error', mtInfo);
end;


procedure TsgeErrorManager.LogMessage(Message: String);
begin
  FCS.Enter;

  FConsole.TextColor := sccWhite;
  FConsole.WriteLn(Message);

  FCS.Leave;
end;


procedure TsgeErrorManager.LogError(Error: String);
begin
  FCS.Enter;

  FConsole.TextColor := sccRed;
  FConsole.WriteLn(Error);

  FCS.Leave;
end;



end.

