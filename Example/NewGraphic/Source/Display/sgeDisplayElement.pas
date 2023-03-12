{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElement.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс отображаемого элемента. Содержит список действий отрисовки
}
{$Include Defines.inc}

unit sgeDisplayElement;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSprite,
  sgeDisplayElementItemList;

type
  TsgeDisplayElement = class
  private
    FItems: TsgeDisplayElementItemList;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure DrawSprite(X, Y, Width, Height: Single; Sprite: TsgeSprite);
    procedure DrawSprite(X, Y, Width, Height: Single; Col, Row: Word; Sprite: TsgeSprite);

    property Items: TsgeDisplayElementItemList read FItems;
  end;


implementation

uses
  sgeDisplayElementItemBase{,
  sgeDisplayElementItemSprite};


constructor TsgeDisplayElement.Create;
begin
  //Создать список
  FItems := TsgeDisplayElementItemList.Create(True);
end;


destructor TsgeDisplayElement.Destroy;
begin
  FItems.Free;
end;


procedure TsgeDisplayElement.Clear;
begin
  FItems.Clear;
end;


procedure TsgeDisplayElement.DrawSprite(X, Y, Width, Height: Single; Sprite: TsgeSprite);
{var
  Item: TsgeDisplayElementItemBase;}
begin
  {Item := TsgeDisplayElementItemSprite.Create(X, Y, Width, Height, Sprite);
  FItems.Add(Item);}
end;


procedure TsgeDisplayElement.DrawSprite(X, Y, Width, Height: Single; Col, Row: Word; Sprite: TsgeSprite);
begin

end;



end.

