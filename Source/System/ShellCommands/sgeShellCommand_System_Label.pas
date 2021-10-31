{
Пакет             Simple Game Engine 2
Файл              sgeShellCommand_System_Label.pas
Версия            1.0
Создан            26.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Команда одолочки
}
{$Include Defines.inc}

unit sgeShellCommand_System_Label;

{$mode objfpc}{$H+}

interface

uses
  sgeShellCommand;


type
  {
  Описание:
    Метка
  Синтаксис:
    System.Label [Name]
  Параметры:
    Name - Имя метки
  }
  TsgeShellCommand_System_Label = class(TsgeShellCommand)
  public
    constructor Create(SGEObject: TObject);
  end;


implementation


constructor TsgeShellCommand_System_Label.Create(SGEObject: TObject);
begin
  inherited Create(SGEObject, 'Label', Group_System);

  //Добавить параметры
  FParameters.AddString('Name', True);
end;



end.

