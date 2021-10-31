{
Пакет             Simple Game Engine 2
Файл              sgeShellCommandQueue.pas
Версия            1.1
Создан            01.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс очереди команд на выполнение
}
{$Include Defines.inc}

unit sgeShellCommandQueue;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateThreadSafeList;


type
  TsgeShellCommandQueue = class(specialize TsgeTemplateThreadSafeList<String>)
  public
    function PullFirstCommand: String;
  end;



implementation


function TsgeShellCommandQueue.PullFirstCommand: String;
begin
  FCS.Enter;
  try

    if FCount > 0 then
      begin
      //Вернуть результат
      Result := GetItem(0);

      //Удалить первую команду
      inherited Delete(0);
      end;

  finally
    FCS.Leave;
  end;
end;


end.

