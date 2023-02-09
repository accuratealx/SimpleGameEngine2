{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemSprite.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент рисования: Спрайт
}
{$Include Defines.inc}

unit sgeDisplayElementItemSprite;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  sgeSprite,
  sgeDisplayElementItemBase;

type
  TsgeDisplayElementItemSprite = class(TsgeDisplayElementItemBase)
  private
    FSprite: TsgeSprite;
  public
    constructor Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);

    property Sprite: TsgeSprite read FSprite write FSprite;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'DisplayElementItemSprite';

  Err_EmptySprite = 'EmptySprite';


constructor TsgeDisplayElementItemSprite.Create(X, Y, Width, Height: Single; Sprite: TsgeSprite);
begin
  inherited Create(X, Y, Width, Height);

  //Проверить спрайт
  if Sprite = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptySprite);

  //Сохранить спрайт
  FSprite := Sprite;
end;



end.

