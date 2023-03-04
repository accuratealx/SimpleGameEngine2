{
Пакет             Simple Game Engine 2
Файл              sgeFontGlyph.pas
Версия            1.0
Создан            22.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Глиф шрифта
}
{$Include Defines.inc}

unit sgeFontGlyph;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeFontGlyph = class
  private
    FCaption: String;                                               //Заголовок глифа
    FSpriteRect: TsgeFloatRect;                                     //Координаты спрайта
    FBaseLine: Word;                                                //Высота базовой линии от нижней части

    function  GetWidth: Integer;
    function  GetHeight: Integer;
  public
    constructor Create;
    constructor Create(Caption: String; BaseLine: Word; SpriteRect: TsgeFloatRect);

    property Caption: String read FCaption write FCaption;
    property SpriteRect: TsgeFloatRect read FSpriteRect write FSpriteRect;
    property BaseLine: Word read FBaseLine write FBaseLine;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property X1: Single read FSpriteRect.X1 write FSpriteRect.X1;
    property Y1: Single read FSpriteRect.Y1 write FSpriteRect.Y1;
    property X2: Single read FSpriteRect.X2 write FSpriteRect.X2;
    property Y2: Single read FSpriteRect.Y2 write FSpriteRect.Y2;
  end;


implementation



function TsgeFontGlyph.GetWidth: Integer;
begin

end;


function TsgeFontGlyph.GetHeight: Integer;
begin

end;


constructor TsgeFontGlyph.Create;
begin
  FCaption := '???';
end;


constructor TsgeFontGlyph.Create(Caption: String; BaseLine: Word; SpriteRect: TsgeFloatRect);
begin
  //Сохранить параметры
  FCaption := Caption;
  FSpriteRect := SpriteRect;
end;



end.

