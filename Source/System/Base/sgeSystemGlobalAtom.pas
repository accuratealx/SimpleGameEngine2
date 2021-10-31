{
Пакет             Simple Game Engine 2
Файл              sgeSystemGlobalAtom.pas
Версия            1.0
Создан            18.10.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс глобального атома
}
{$Include Defines.inc}

unit sgeSystemGlobalAtom;

{$mode objfpc}{$H+}

interface


type
  TsgeSystemGlobalAtom = class
  private
    FAtom: Word;
    FName: ShortString;
  public
    constructor Create(Name: ShortString);
    destructor  Destroy; override;

    property Atom: Word read FAtom;
    property Name: ShortString read FName;
  end;


implementation

uses
  sgeOSPlatform;


constructor TsgeSystemGlobalAtom.Create(Name: ShortString);
begin
  FAtom := sgeGlobalAddAtom(Name);

  FName := Name;
end;


destructor TsgeSystemGlobalAtom.Destroy;
begin
  sgeGlobalDeleteAtom(FAtom);
end;



end.

