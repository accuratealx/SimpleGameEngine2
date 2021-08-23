{
Пакет             Simple Game Engine 2
Файл              sgeGraphicFPS.pas
Версия            1.3
Создан            05.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Графика: Настройки вывода FPS
}
{$Include Defines.inc}

unit sgeGraphicFPS;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicColor, sgeGraphicFont;


type
  TsgeGraphicFPS = class
  private
    FEnable: Boolean;
    FHorizontalAlign: TsgeHorizontalAlign;
    FVerticalAlign: TsgeVerticalAlign;
    FColor: TsgeColor;
    FXOffset: Integer;
    FYOffset: Integer;
    FFont: TsgeGraphicFont;
  public
    constructor Create;
    destructor  Destroy; override;

    property Enable: Boolean read FEnable write FEnable;
    property Font: TsgeGraphicFont read FFont;
    property Color: TsgeColor read FColor write FColor;
    property XOffset: Integer read FXOffset write FXOffset;
    property YOffset: Integer read FYOffset write FYOffset;
    property HorizontalAlign: TsgeHorizontalAlign read FHorizontalAlign write FHorizontalAlign;
    property VerticalAlign: TsgeVerticalAlign read FVerticalAlign write FVerticalAlign;
  end;


implementation


constructor TsgeGraphicFPS.Create;
begin
  FFont := TsgeGraphicFont.Create('Lucida Console', 14, []);
  FHorizontalAlign := haRight;
  FVerticalAlign := vaTop;
  FColor := cWhite;
  FEnable := False;
  FXOffset := -5;
  FYOffset := 5;
end;


destructor TsgeGraphicFPS.Destroy;
begin
  FFont.Free;
end;

end.

