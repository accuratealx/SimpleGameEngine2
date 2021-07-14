{
Пакет             Simple Game Engine 2
Файл              sgeEventSubscriberGroup.pas
Версия            1.0
Создан            12.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Кдасс группы подписчиков
}
{$Include Defines.inc}

unit sgeEventSubscriberGroup;

{$mode objfpc}{$H+}

interface

uses
  sgeEventSubscriberList;


type
  TsgeEventSubscriberGroup = class
  private
    FName: ShortString;                                             //Имя события
    FSubscribers: TsgeEventSubscriberList;                          //Список подписчиков

  public
    constructor Create(Name: ShortString);
    destructor  Destroy; override;

    property Name: ShortString read FName;
    property Subscribers: TsgeEventSubscriberList read FSubscribers;
  end;


implementation


constructor TsgeEventSubscriberGroup.Create(Name: ShortString);
begin
  FName := Name;
  FSubscribers := TsgeEventSubscriberList.Create;
end;


destructor TsgeEventSubscriberGroup.Destroy;
begin
  FSubscribers.Free;
end;



end.

