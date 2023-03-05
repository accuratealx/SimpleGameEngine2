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
  sgeSprite, sgeMemoryStream,
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
    FLineSpace: Word;                                               //Расстояние между строками
    FGlyphSpace: Word;                                              //Расстояние между глифами
    FBaseLine: Word;                                                //Базовая линия от нижней границы символа
    FHeight: Word;                                                  //Высота шрифта

    procedure CreateGlyphList;
    procedure DestroyGlyphList;

    procedure PreCreate;
  public
    constructor Create(Name: String);
    constructor Create(Stream: TsgeMemoryStream);
    destructor  Destroy; override;

    function  ToString: String; reintroduce;
    procedure FromString(Str: String);
    procedure ToMemoryStream(Stream: TsgeMemoryStream);
    procedure FromMemoryStream(Stream: TsgeMemoryStream);

    property GlyphList: TsgeFontGlyphList read FGlyphList;
    property Sprite: TsgeSprite read FSprite;

    property Name: String read FName write FName;
    property LineSpace: Word read FLineSpace write FLineSpace;
    property GlyphSpace: Word read FGlyphSpace write FGlyphSpace;
    property BaseLine: Word read FBaseLine write FBaseLine;
    property Height: Word read FHeight write FHeight;
  end;


implementation

uses
  base64,
  sgeTypes, sgeErrors, sgeSystemUtils, sgeStringList, sgeSimpleContainer, sgeSimpleParameters;

const
  SECTION_INFO = 'Info';
  SECTION_GLYPH = 'Glyph';
  SECTION_SPRITE = 'Sprite';

  PARAM_NAME = 'Name';
  PARAM_LINES_PACE = 'LineSpace';
  PARAM_GLYPHS_PACE = 'GlyphSpace';
  PARAM_BASE_LINE = 'BaseLine';
  PARAM_HEIGHT = 'Height';
  PARAM_WIDTH = 'Width';
  PARAM_DATA = 'Data';

  _UNITNAME = 'Font';

  Err_CorruptData = 'CorruptData';
  Err_CantLoad = 'Cantload';


procedure TsgeFont.CreateGlyphList;
var
  i: Integer;
begin
  for i := 0 to $FF do
    FGlyphList[i] := TsgeFontGlyph.Create(0, sgeGetFloatRect(0, 0, 0, 0));
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


procedure TsgeFont.PreCreate;
begin
  //Создать список глифов
  CreateGlyphList;

  //Создать спрайт
  FSprite := TsgeSprite.Create(0, 0, 0, 0);
end;


constructor TsgeFont.Create(Name: String);
begin
  //Создать объекты
  PreCreate;

  //Установить параметры
  FName := Name;
  FHeight := 10;
  FLineSpace := 1;
  FGlyphSpace := 1;
  FBaseLine := 0;
end;


constructor TsgeFont.Create(Stream: TsgeMemoryStream);
begin
  //Создать объекты
  PreCreate;

  //Загрузить из потока
  FromMemoryStream(Stream);
end;


destructor TsgeFont.Destroy;
begin
  FSprite.Free;
  DestroyGlyphList;
end;


function TsgeFont.ToString: String;
var
  Container: TsgeSimpleContainer;
  Params: TsgeSimpleParameters;
  List: TsgeStringList;
  i: Integer;
  s: String;
begin
  Result := '';

  Container := TsgeSimpleContainer.Create;
  Params := TsgeSimpleParameters.Create;
  List := TsgeStringList.Create;
  try
    //Info
    Params.SetValue(PARAM_NAME, FName);
    Params.SetValue(PARAM_HEIGHT, FHeight);
    Params.SetValue(PARAM_LINES_PACE, FLineSpace);
    Params.SetValue(PARAM_GLYPHS_PACE, FGlyphSpace);
    Params.SetValue(PARAM_BASE_LINE, FBaseLine);
    Container.Add(SECTION_INFO, Params.ToString);


    //Glyph
    Params.Clear;
    for i := 0 to $FF do
      List.Add(FGlyphList[i].ToString);
    Container.Add(SECTION_GLYPH, List.ToString);


    //Sprite
    s := '';
    Params.Clear;
    Params.SetValue(PARAM_WIDTH, FSprite.Width);
    Params.SetValue(PARAM_HEIGHT, FSprite.Height);
    SetLength(s, FSprite.Size);
    Move(FSprite.Data^, s[1], FSprite.Size);
    s := EncodeStringBase64(s);
    Params.SetValue(PARAM_DATA, s);
    Container.Add(SECTION_SPRITE, Params.ToString);


    //Результат
    Result := Container.ToString;

  finally
    List.Free;
    Params.Free;
    Container.Free;
  end;
end;


procedure TsgeFont.FromString(Str: String);
var
  Container: TsgeSimpleContainer;
  Params: TsgeSimpleParameters;
  List: TsgeStringList;
  i, W, H, Size: Integer;
  Data: String;
begin
  Container := TsgeSimpleContainer.Create;
  Params := TsgeSimpleParameters.Create;
  List := TsgeStringList.Create;
  try

    try
      Container.FromString(Str);

      //Info
      Container.GetSectionParameters(SECTION_INFO, Params);
      FName := sgeTrim(Params.GetStringValue(PARAM_NAME));
      FHeight := Params.GetIntegerValue(PARAM_HEIGHT);
      FLineSpace := Params.GetIntegerValue(PARAM_LINES_PACE);
      FGlyphSpace := Params.GetIntegerValue(PARAM_GLYPHS_PACE);
      FBaseLine := Params.GetIntegerValue(PARAM_BASE_LINE);


      //Glyph
      Container.GetSectionList(SECTION_GLYPH, List);
      if List.Count < $FF then
        raise EsgeException.Create(_UNITNAME, Err_CorruptData, SECTION_GLYPH);
      for i := 0 to $FF do
        FGlyphList[i].FromString(List.Part[i]);


      //Sprite
      Params.Clear;
      Container.GetSectionParameters(SECTION_SPRITE, Params);
      W := Params.GetIntegerValue(PARAM_WIDTH);
      H := Params.GetIntegerValue(PARAM_HEIGHT);
      Data := sgeTrim(Params.GetStringValue(PARAM_DATA));
      Data := DecodeStringBase64(Data);

      Size := W * H * 4;
      if Size <> Length(Data) then
        raise EsgeException.Create(_UNITNAME, Err_CorruptData, PARAM_DATA);

      FSprite.SetSize(W, H);
      Move(Data[1], FSprite.Data^, Size);

    except
      on E: EsgeException do
        raise EsgeException.Create(_UNITNAME, Err_CantLoad, '', E.Message);
    end;

  finally
    List.Free;
    Params.Free;
    Container.Free;
  end;
end;


procedure TsgeFont.ToMemoryStream(Stream: TsgeMemoryStream);
begin
  Stream.FromString(ToString);
end;


procedure TsgeFont.FromMemoryStream(Stream: TsgeMemoryStream);
begin
  FromString(Stream.ToString);
end;



end.

