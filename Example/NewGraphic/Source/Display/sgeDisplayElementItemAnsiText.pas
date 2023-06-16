{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemAnsiText.pas
Версия            1.0
Создан            16.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Текст Ansi
}
{$Include Defines.inc}

unit sgeDisplayElementItemAnsiText;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes, sgeGraphicColor, sgeAnsiFont,
  sgeDisplayElementItemBase, sgeDisplayElementItemPropertyColor, sgeDisplayElementItemPropertyScale,
  sgeDisplayElementItemPropertyRotate, sgeDisplayElementItemPropertyFloatPoint;

type
  TsgeDisplayElementItemAnsiText = class(TsgeDisplayElementItemBase)
  private
    FPoint: TsgeDisplayElementItemPropertyFloatPoint;
    FColor: TsgeDisplayElementItemPropertyColor;
    FScale: TsgeDisplayElementItemPropertyScale;
    FOrigin: TsgeDisplayElementItemPropertyFloatPoint;
    FRotate: TsgeDisplayElementItemPropertyRotate;
    FFont: TsgeAnsiFont;
    FText: String;
    FTextBytes: TsgeByteArray;

    procedure SetFont(AFont: TsgeAnsiFont);
    procedure SetText(AText: String);
  public
    constructor Create(X, Y: Single; Color: TsgeColor; Font: TsgeAnsiFont; Text: String);
    destructor  Destroy; override;

    property Point: TsgeDisplayElementItemPropertyFloatPoint read FPoint;
    property Color: TsgeDisplayElementItemPropertyColor read FColor;
    property Scale: TsgeDisplayElementItemPropertyScale read FScale;
    property Origin: TsgeDisplayElementItemPropertyFloatPoint read FOrigin;
    property Rotate: TsgeDisplayElementItemPropertyRotate read FRotate;
    property Font: TsgeAnsiFont read FFont write SetFont;
    property Text: String read FText write SetText;
    property TextBytes: TsgeByteArray read FTextBytes;
  end;


implementation

uses
  sgeErrors, sgeOSPlatform;

const
  _UNITNAME = 'DisplayElementItemRect';

  Err_EmptyFont = 'EmptyFont';


procedure TsgeDisplayElementItemAnsiText.SetFont(AFont: TsgeAnsiFont);
begin
  //Проверить спрайт
  if AFont = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyFont);

  FFont := AFont;
end;


procedure TsgeDisplayElementItemAnsiText.SetText(AText: String);
begin
  //Запомнить текст
  FText := AText;

  //Шрифт не понимает utf-8, только Ansi. Поэтому переведем тут в байты,
  //что бы не напрягать поток графики
  FTextBytes := sgeUtf8ToAnsiBytes(FText);
end;


constructor TsgeDisplayElementItemAnsiText.Create(X, Y: Single; Color: TsgeColor; Font: TsgeAnsiFont; Text: String);
begin
  SetFont(Font);
  FPoint := TsgeDisplayElementItemPropertyFloatPoint.Create(X, Y);
  FColor := TsgeDisplayElementItemPropertyColor.Create(Color);
  FScale := TsgeDisplayElementItemPropertyScale.Create;
  FOrigin := TsgeDisplayElementItemPropertyFloatPoint.Create;
  FRotate := TsgeDisplayElementItemPropertyRotate.Create;
  SetText(Text);
end;


destructor TsgeDisplayElementItemAnsiText.Destroy;
begin
  SetLength(FText, 0);
  FRotate.Free;
  FOrigin.Free;
  FScale.Free;
  FColor.Free;
  FPoint.Free;
end;



end.


