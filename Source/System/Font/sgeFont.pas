{
Пакет             Simple Game Engine 2
Файл              sgeFont.pas
Версия            1.0
Создан            22.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Шрифт
}
{$Include Defines.inc}

unit sgeFont;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSprite,
  sgeFontGlyph;


type
  //Атрибуты шрифта (Жирный, Наклонный, Подчеркнутый, Перечёркнутый)
  TsgeFontAttributes = set of (
    faBold,                                                         //Жирный
    faItalic,                                                       //Наклонный
    faUnderline,                                                    //Подчеркнутый
    faStrikeOut                                                     //Перечеркнутый
  );


  //Список глифов
  TsgeFontGlyphList = array[0..$FF] of TsgeFontGlyph;


  //Шрифт
  TsgeFont = class
  private
    FGlyphList: TsgeFontGlyphList;                                  //Список глифов
    FSprite: TsgeSprite;                                            //Спрайт с изображениями глифов

    FName: String;                                                  //Имя шрифта
    FAttributes: TsgeFontAttributes;                                //Атрибуты шрифта
    FLineSpace: Word;                                               //Расстояние между строками
    FGlyphSpace: Word;                                              //Расстояние между глифами
    FHeight: Word;                                                  //Высота шрифта

    procedure CreateGlyphList;
    procedure DestroyGlyphList;
  public
    constructor Create(Name: String);
    destructor  Destroy; override;

    property GlyphList: TsgeFontGlyphList read FGlyphList;
    property Sprite: TsgeSprite read FSprite;
    property Attributes: TsgeFontAttributes read FAttributes;
    property Name: String read FName write FName;
    property LineSpace: Word read FLineSpace write FLineSpace;
    property GlyphSpace: Word read FGlyphSpace write FGlyphSpace;
    property Height: Word read FHeight write FHeight;
  end;


implementation

uses
  sgeSystemUtils;


procedure TsgeFont.CreateGlyphList;
var
  i: Integer;
begin
  for i := 0 to $FF do
  begin
    FGlyphList[i] := TsgeFontGlyph.Create;
    FGlyphList[i].Caption := 'Symbol [' + sgeIntToStr(i) + ']';
  end;
end;


procedure TsgeFont.DestroyGlyphList;
var
  i: Integer;
begin
  for i := 0 to $FF do
  begin
    FGlyphList[i].Free;
    FGlyphList[i] := nil;
  end;
end;


constructor TsgeFont.Create(Name: String);
begin
  //Создать список глифов
  CreateGlyphList;

  //Создать спрайт
  FSprite := TsgeSprite.Create(0, 0, 16, 16);

  //Установить параметры
  FAttributes := [];
  FName := Name;
  FHeight := 10;
  FLineSpace := 1;
  FGlyphSpace := 1;
end;


destructor TsgeFont.Destroy;
begin
  FSprite.Free;
  DestroyGlyphList;
end;



end.

