{
Пакет             Simple Game Engine 2
Файл              sgeGUIPropertyFont.pas
Версия            1.0
Создан            22.11.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          GUI: Свойство: Шрифт
}
{$Include Defines.inc}

unit sgeGUIPropertyFont;

{$mode objfpc}{$H+}

interface

uses
  sgeSimpleParameters,
  sgeGraphicFont,
  sgeGUIProperty;

type
  TsgeGUIPropertyFont = class(TsgeGUIProperty)
  private
    FName: String;                                                  //Имя шрифта в таблице ресурсов
    FFont: TsgeGraphicFont;                                         //Указатель на объект шрифта

    procedure SetName(AName: String);
    function  GetHeight: Word;
    function  GetSize: Word;
    function  GetAttrib: TsgeGraphicFontAttrib;
  public
    constructor Create(AOwner: TObject); override;

    function GetTextWidth(const Txt: String): Integer;
    function GetTextHeight(const Txt: String): Integer;

    property Name: String read FName write SetName;
    property Height: Word read GetHeight;
    property Size: Word read GetSize;
    property Attrib: TsgeGraphicFontAttrib read GetAttrib;
  end;


  TsgeGUIPropertyFontExt = class(TsgeGUIPropertyFont)
  public
    procedure LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String = '');

    property Font: TsgeGraphicFont read FFont;
  end;


implementation

uses
  sgeVars;



procedure TsgeGUIPropertyFont.SetName(AName: String);
begin
  //Найти шрифт с именем и установить
  FFont := SGE.ExtResourceList.GetFont(AName);

  //Перерисовать родителя
  UpdateParent;
end;


function TsgeGUIPropertyFont.GetHeight: Word;
begin
  Result := FFont.Height;
end;


function TsgeGUIPropertyFont.GetSize: Word;
begin
  Result := FFont.Size;
end;


function TsgeGUIPropertyFont.GetAttrib: TsgeGraphicFontAttrib;
begin
  Result := FFont.Attrib;
end;


constructor TsgeGUIPropertyFont.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  //Шрифт по умолчанию
  FFont := SGE.ExtResourceList.Default.Font;
end;


function TsgeGUIPropertyFont.GetTextWidth(const Txt: String): Integer;
begin
  Result := FFont.GetStringWidth(Txt);
end;


function TsgeGUIPropertyFont.GetTextHeight(const Txt: String): Integer;
begin
  Result := FFont.GetStringHeight(Txt);
end;


procedure TsgeGUIPropertyFontExt.LoadParameters(Parameters: TsgeSimpleParameters; Prefix: String);
var
  ParamName, s: String;
begin
  ParamName := Prefix + 'Name';
  if Parameters.Exist[ParamName] then
  begin
    s := Parameters.GetValue(ParamName, '');
    FFont := SGE.ExtResourceList.GetFont(s);
    FName := s;
  end;
end;


end.

