{
Пакет             Simple Game Engine 2
Файл              sgeGraphic.pas
Версия            1.0
Создан            06.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Интерфейс рендерера
}
{$Include Defines.inc}

unit sgeGraphic;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeSprite, sgeGraphicColor;


type
  IsgeGraphic = interface ['{B88F5A15-CFA6-491F-8025-00DB31844C86}']
    function  GetWidth: Integer;
    function  GetHeight: Integer;

    procedure SetScale(X, Y: Single);
    procedure SetScale(Pos: TsgeFloatPoint);
    procedure SetRotate(Angle: Single);
    procedure SetPos(X, Y: Single);
    procedure SetPos(Pos: TsgeFloatPoint);
    procedure SaveState;
    procedure LoadState;
    procedure SetColor(Color: TsgeColor);

    procedure DrawSprite(X, Y: Single; Sprite: TsgeSprite);
  end;



implementation



end.

