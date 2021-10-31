{
Пакет             Simple Game Engine 2
Файл              sgeShellLine.pas
Версия            1.1
Создан            10.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс линии строки оболочки
}
{$Include Defines.inc}

unit sgeShellLine;

{$mode objfpc}{$H+}

interface

uses
  sgeTemplateCollection,
  sgeGraphicColor,
  sgeShellLineItem;


type
  TsgeShellLine = class(specialize TsgeTemplateCollection<TsgeShellLineItem>)
  public
    constructor Create;

    function Add(Text: String; Color: TsgeColor): TsgeShellLineItem;
    function Add(Text: String; Color: TsgeColor; BGColor: TsgeColor): TsgeShellLineItem;
  end;


implementation


constructor TsgeShellLine.Create;
begin
  inherited Create(True);
end;


function TsgeShellLine.Add(Text: String; Color: TsgeColor): TsgeShellLineItem;
begin
  Result := TsgeShellLineItem.Create(Text, Color, cTransparentBlack);
  inherited Add(Result);
end;


function TsgeShellLine.Add(Text: String; Color: TsgeColor; BGColor: TsgeColor): TsgeShellLineItem;
begin
  Result := TsgeShellLineItem.Create(Text, Color, BGColor);
  inherited Add(Result);
end;




end.

