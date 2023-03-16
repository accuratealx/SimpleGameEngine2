{
Пакет             Simple Game Engine 2
Файл              sgeFontGlyph.pas
Версия            1.0
Создан            22.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Глиф шрифта
}
{$Include Defines.inc}

unit sgeAnsiFontGlyph;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes;

type
  TsgeAnsiFontGlyph = class
  private
    FSpriteRect: TsgeFloatRect; //Координаты спрайта
    FBaseLine: Integer;         //Смещение прямоугольника от нижней части до базовой линии
    FWidth: Single;             //Ширина
    FHeight: Single;            //Высота

    procedure CorrectSizes;
    procedure SetX1(AX1: Single);
    procedure SetY1(AY1: Single);
    procedure SetX2(AX2: Single);
    procedure SetY2(AY2: Single);
    procedure SetSpriteRect(Arect: TsgeFloatRect);
  public
    constructor Create(BaseLine: Integer; SpriteRect: TsgeFloatRect);

    procedure FromString(Str: String);
    function  ToString: String; reintroduce;

    property SpriteRect: TsgeFloatRect read FSpriteRect write SetSpriteRect;
    property BaseLine: Integer read FBaseLine write FBaseLine;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property X1: Single read FSpriteRect.X1 write SetX1;
    property Y1: Single read FSpriteRect.Y1 write SetY1;
    property X2: Single read FSpriteRect.X2 write SetX2;
    property Y2: Single read FSpriteRect.Y2 write SetY2;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeStringList;

const
  SEPARATOR = ';';

  _UNITNAME = 'FontGlyph';

  Err_NotEnoughPart = 'NotEnoughPart';
  Err_CantConvertValue = 'CantConvertValue';


procedure TsgeAnsiFontGlyph.CorrectSizes;
begin
  FWidth := FSpriteRect.X2 - FSpriteRect.X1;
  FHeight := FSpriteRect.Y2 - FSpriteRect.Y1;
end;


procedure TsgeAnsiFontGlyph.SetX1(AX1: Single);
begin
  FSpriteRect.X1 := AX1;
  CorrectSizes;
end;


procedure TsgeAnsiFontGlyph.SetY1(AY1: Single);
begin
  FSpriteRect.Y1 := AY1;
  CorrectSizes;
end;


procedure TsgeAnsiFontGlyph.SetX2(AX2: Single);
begin
  FSpriteRect.X2 := AX2;
  CorrectSizes;
end;


procedure TsgeAnsiFontGlyph.SetY2(AY2: Single);
begin
  FSpriteRect.Y2 := AY2;
  CorrectSizes;
end;


procedure TsgeAnsiFontGlyph.SetSpriteRect(Arect: TsgeFloatRect);
begin
  FSpriteRect:= Arect;
  CorrectSizes;
end;


constructor TsgeAnsiFontGlyph.Create(BaseLine: Integer; SpriteRect: TsgeFloatRect);
begin
  //Сохранить параметры
  FBaseLine := BaseLine;
  FSpriteRect := SpriteRect;
  CorrectSizes;
end;


procedure TsgeAnsiFontGlyph.FromString(Str: String);
var
  List: TsgeStringList;
begin
  List := TsgeStringList.Create;
  try
    //Разобрать строку на части
    List.Separator := SEPARATOR;
    List.FromString(Str);

    //Количество секций
    if List.Count < 5 then
      raise EsgeException.Create(_UNITNAME, Err_NotEnoughPart, sgeIntToStr(List.Count));

    //X1
    if not sgeTryStrToFloat(List.Part[0], FSpriteRect.X1) then
      raise EsgeException.Create(_UNITNAME, Err_CantConvertValue, List.Part[0]);

    //Y1
    if not sgeTryStrToFloat(List.Part[1], FSpriteRect.Y1) then
      raise EsgeException.Create(_UNITNAME, Err_CantConvertValue, List.Part[1]);

    //X2
    if not sgeTryStrToFloat(List.Part[2], FSpriteRect.X2) then
      raise EsgeException.Create(_UNITNAME, Err_CantConvertValue, List.Part[2]);

    //Y2
    if not sgeTryStrToFloat(List.Part[3], FSpriteRect.Y2) then
      raise EsgeException.Create(_UNITNAME, Err_CantConvertValue, List.Part[3]);

    //Y1
    if not sgeTryStrToInt(List.Part[4], FBaseLine) then
      raise EsgeException.Create(_UNITNAME, Err_CantConvertValue, List.Part[4]);

    //Поправить размеры
    CorrectSizes;

  finally
    List.Free;
  end;
end;


function TsgeAnsiFontGlyph.ToString: String;
begin
  Result := sgeFloatToStr(FSpriteRect.X1) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.Y1) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.X2) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.Y2) + SEPARATOR +
            sgeIntToStr(FBaseLine);
end;



end.

