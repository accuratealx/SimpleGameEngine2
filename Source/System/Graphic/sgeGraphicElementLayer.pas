{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementLayer.pas
Версия            1.2
Создан            14.07.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс слоя графических элементов
}
{$Include Defines.inc}

unit sgeGraphicElementLayer;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes,
  sgeGraphicElementList;


type
  TsgeGraphicElementLayer = class
  private
    FElementList: TsgeGraphicElementList;                           //Список объектов

    FName: ShortString;                                             //Имя
    FVisible: Boolean;                                              //Видимость
    FIndex: Word;                                                   //Индекс приоритета
    FOffset: TsgeFloatPoint;
    FScale: Single;

  public
    constructor Create(Name: ShortString; Index: Word = 0; Visible: Boolean = True);
    destructor  Destroy; override;

    property Name: ShortString read FName;
    property Index: Word read FIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Offset: TsgeFloatPoint read FOffset write FOffset;
    property Scale: Single read FScale write FScale;

    property Elements: TsgeGraphicElementList read FElementList;
  end;


implementation


constructor TsgeGraphicElementLayer.Create(Name: ShortString; Index: Word; Visible: Boolean);
begin
  FElementList := TsgeGraphicElementList.Create(True);

  FName := Name;
  FIndex := Index;
  FVisible := Visible;
  FScale := 1;
end;


destructor TsgeGraphicElementLayer.Destroy;
begin
  FElementList.Free;
end;


end.

