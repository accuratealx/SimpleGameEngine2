{
Пакет             Simple Game Engine 2
Файл              sgeCriticalSection.pas
Версия            1.2
Создан            24.02.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс системной критической секции
}
{$Include Defines.inc}

unit sgeCriticalSection;

{$mode objfpc}{$H+}

interface

uses
  sgeOSPlatform;


type
  TsgeCriticalSection = class
  private
    FSection: sgeOSPlatform.TsgeCriticalSection;

  public
    constructor Create;
    destructor  Destroy; override;

    function  TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;


implementation


constructor TsgeCriticalSection.Create;
begin
  sgeInitCriticalSection(FSection);
end;


destructor TsgeCriticalSection.Destroy;
begin
  sgeDeleteCriticalSection(FSection);
end;


function TsgeCriticalSection.TryEnter: Boolean;
begin
  Result := sgeTryEnterCriticalSection(FSection);
end;


procedure TsgeCriticalSection.Enter;
begin
  sgeEnterCriticalSection(FSection);
end;


procedure TsgeCriticalSection.Leave;
begin
  sgeLeaveCriticalSection(FSection);
end;


end.

