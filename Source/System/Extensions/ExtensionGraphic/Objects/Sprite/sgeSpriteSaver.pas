{
Пакет             Simple Game Engine 2
Файл              sgeSpriteSaver.pas
Версия            1.0
Создан            17.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс сохранения спрайтов: Базовый
}
{$Include Defines.inc}

unit sgeSpriteSaver;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream,
  sgeSprite;


type
  TsgeSpriteSaver = class
  public
    procedure ToMemoryStream(Stream: TsgeMemoryStream; Sprite: TsgeSprite); virtual abstract;
  end;


implementation



end.

