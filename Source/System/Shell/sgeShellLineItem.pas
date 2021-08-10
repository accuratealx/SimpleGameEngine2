{
Пакет             Simple Game Engine 2
Файл              sgeShellLineItem.pas
Версия            1.0
Создан            10.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс одного элемента строки оболочки
}
{$Include Defines.inc}

unit sgeShellLineItem;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphicColor;


type
  TsgeShellLineItem = class
  private
    FColor: TsgeColor;
    FBGColor: TsgeColor;
    FText: String;
  public
    constructor Create(Text: String; Color: TsgeColor; BGColor: TsgeColor);

    property Color: TsgeColor read FColor write FColor;
    property BGColor: TsgeColor read FBGColor write FColor;
    property Text: String read FText write FText;
  end;


implementation


constructor TsgeShellLineItem.Create(Text: String; Color: TsgeColor; BGColor: TsgeColor);
begin
  FText := Text;
  FColor := Color;
  FBGColor := BGColor;
end;


end.

