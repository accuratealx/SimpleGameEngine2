{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemText.pas
Версия            1.0
Создан            16.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Текст
}
{$Include Defines.inc}

unit sgeDisplayElementItemText;

{$mode ObjFPC}{$H+}

interface

uses
  sgeGraphicColor, sgeFont,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyColor, sgeDisplayElementItemPropertyScale,
  sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemText = class(TsgeDisplayElementItemBase)
  private
    FPoint: TsgeDisplayElementItemPropertyFloatPoint;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FFont: TsgeFont;
    FText: String;

    procedure SetFont(AFont: TsgeFont);
    procedure SetText(AText: String);
  public
    constructor Create(X, Y: Single; Color: TsgeColor; Font: TsgeFont; Text: String);
    destructor  Destroy; override;

    property Point: TsgeDisplayElementItemPropertyFloatPoint read FPoint;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Font: TsgeFont read FFont write SetFont;
    property Text: String read FText write SetText;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'sgeDisplayElementItemRect';

  Err_EmptyFont = 'EmptyFont';


procedure TsgeDisplayElementItemText.SetFont(AFont: TsgeFont);
begin
  //Проверить спрайт
  if AFont = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFont);

  FFont := AFont;
end;


procedure TsgeDisplayElementItemText.SetText(AText: String);
begin
  //Шрифт не понимает utf-8, только Ansi. Поэтому переведем тут,
  //что бы не напрягать поток графики
  FText := Utf8ToAnsi(AText);
end;


constructor TsgeDisplayElementItemText.Create(X, Y: Single; Color: TsgeColor; Font: TsgeFont; Text: String);
begin
  SetFont(Font);
  FPoint := TsgeDisplayElementItemPropertyFloatPoint.Create(X, Y);
  FColor := TsgeDisplayElementItemPropertyColor.Create(Color);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  SetText(Text);
end;


destructor TsgeDisplayElementItemText.Destroy;
begin
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FPoint.Free;
end;



end.


