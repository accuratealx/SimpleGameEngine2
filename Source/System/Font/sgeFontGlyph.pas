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
    FSpriteRect: TsgeIntRect;                                       //Координаты спрайта
    FWidth: Word;                                                   //Ширина символа
    FHeight: Word;                                                  //Высота символа
    FBaseLine: Word;                                                //Высота базовой линии от нижней части

    procedure SetBaseLine(ABaseLine: Word);
    procedure SetHeight(AHeight: Word);
  public
    constructor Create;
    constructor Create(Caption: String; Width, Height, BaseLine: Word; SpriteRect: TsgeIntRect);

    property Caption: String read FCaption write FCaption;
    property SpriteRect: TsgeIntRect read FSpriteRect write FSpriteRect;
    property Width: Word read FWidth write FWidth;
    property Height: Word read FHeight write SetHeight;
    property BaseLine: Word read FBaseLine write SetBaseLine;
  end;


implementation


procedure TsgeFontGlyph.SetBaseLine(ABaseLine: Word);
begin
  //Базовая лини не может быть больше высоты
  if ABaseLine > FHeight then
    ABaseLine := FHeight;

  FBaseLine := ABaseLine;
end;


procedure TsgeFontGlyph.SetHeight(AHeight: Word);
begin
  //Запомнить высоту
  FHeight := AHeight;

  //Поправить базовую линию
  if FBaseLine > FHeight then
    FBaseLine := FHeight;
end;


constructor TsgeFontGlyph.Create;
begin
  FCaption := '???';
end;


constructor TsgeFontGlyph.Create(Caption: String; Width, Height, BaseLine: Word; SpriteRect: TsgeIntRect);
begin
  //Сохранить параметры
  FCaption := Caption;
  FWidth := Width;
  FHeight := Height;
  FSpriteRect := SpriteRect;

  //Изменить базовую линию
  SetBaseLine(BaseLine)
end;



end.

