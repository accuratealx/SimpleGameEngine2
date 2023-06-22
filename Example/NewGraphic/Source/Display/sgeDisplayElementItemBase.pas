{
Пакет             Simple Game Engine 2
Файл              sgeDisplayElementItemBase.pas
Версия            1.0
Создан            16.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Элемент отображения: Базовый
}
{$Include Defines.inc}

unit sgeDisplayElementItemBase;

{$mode ObjFPC}{$H+}

interface


type
  TsgeDisplayElementItemBase = class
  protected
  public
    procedure ResetChangeSet; virtual; abstract;
    function  GetCopy: TsgeDisplayElementItemBase; virtual; abstract;
  end;


implementation



end.

