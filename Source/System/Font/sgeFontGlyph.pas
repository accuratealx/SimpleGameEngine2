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
    FSpriteRect: TsgeFloatRect;                                     //Координаты спрайта
    FBaseLine: Integer;                                             //Смещение прямоугольника от нижней части до базовой линии

    function  GetWidth: Integer;
    function  GetHeight: Integer;
  public
    constructor Create(BaseLine: Integer; SpriteRect: TsgeFloatRect);

    procedure FromString(Str: String);
    function  ToString: String; reintroduce;

    property SpriteRect: TsgeFloatRect read FSpriteRect write FSpriteRect;
    property BaseLine: Integer read FBaseLine write FBaseLine;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property X1: Single read FSpriteRect.X1 write FSpriteRect.X1;
    property Y1: Single read FSpriteRect.Y1 write FSpriteRect.Y1;
    property X2: Single read FSpriteRect.X2 write FSpriteRect.X2;
    property Y2: Single read FSpriteRect.Y2 write FSpriteRect.Y2;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils, sgeStringList;

const
  SEPARATOR = ';';

  _UNITNAME = 'FontGlyph';

  Err_NotEnoughPart = 'NotEnoughPart';
  Err_CantConvertValue = 'CantConvertValue';


function TsgeFontGlyph.GetWidth: Integer;
begin

end;


function TsgeFontGlyph.GetHeight: Integer;
begin

end;


constructor TsgeFontGlyph.Create(BaseLine: Integer; SpriteRect: TsgeFloatRect);
begin
  //Сохранить параметры
  FBaseLine := BaseLine;
  FSpriteRect := SpriteRect;
end;


procedure TsgeFontGlyph.FromString(Str: String);
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

  finally
    List.Free;
  end;
end;


function TsgeFontGlyph.ToString: String;
begin
  Result := sgeFloatToStr(FSpriteRect.X1) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.Y1) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.X2) + SEPARATOR +
            sgeFloatToStr(FSpriteRect.Y2) + SEPARATOR +
            sgeIntToStr(FBaseLine);
end;



end.

