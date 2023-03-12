{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemPropertySprite.pas
Версия            1.0
Создан            09.03.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Свойство элемента отображения: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemPropertySprite;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSprite,
  sgeDisplayElementItemPropertyFloatRect;

type
  TsgeDisplayElementItemPropertySprite = class
  private
    FSprite: TsgeSprite;
    FRect: TsgeDisplayElementItemPropertyFloatRect;

    procedure SetSprite(ASprite: TsgeSprite);
  public
    constructor Create(Sprite: TsgeSprite);
    constructor Create(Sprite: TsgeSprite; X1, Y1, X2, Y2: Single);
    destructor  Destroy; override;

    property Sprite: TsgeSprite read FSprite write SetSprite;
    property Rect: TsgeDisplayElementItemPropertyFloatRect read FRect;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'sgeDisplayElementItemPropertySprite';

  Err_EmptySprite = 'EmptySprite';


procedure TsgeDisplayElementItemPropertySprite.SetSprite(ASprite: TsgeSprite);
begin
    //Проверить спрайт
  if ASprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  FSprite := ASprite;
end;


constructor TsgeDisplayElementItemPropertySprite.Create(Sprite: TsgeSprite);
begin
  SetSprite(Sprite);
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(0, 0, FSprite.Width, FSprite.Height);
end;


constructor TsgeDisplayElementItemPropertySprite.Create(Sprite: TsgeSprite; X1, Y1, X2, Y2: Single);
begin
  SetSprite(Sprite);
  FRect := TsgeDisplayElementItemPropertyFloatRect.Create(X1, Y1, X2, Y2);
end;


destructor TsgeDisplayElementItemPropertySprite.Destroy;
begin
  FRect.Free;
end;



end.

