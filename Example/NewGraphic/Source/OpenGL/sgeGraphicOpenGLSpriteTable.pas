{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLSpriteTable.pas
Версия            1.0
Создан            29.01.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Хэш-таблица спрайтов
}
{$Include Defines.inc}

unit sgeGraphicOpenGLSpriteTable;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs,
  sgeSprite, sgeGraphicOpenGLSprite;

type
  TsgeGraphicOpenGLSpriteTable = class
  private
    FTable: TFPHashObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    function  Add(Sprite: TsgeSprite): TsgeGraphicOpenGLSprite;
    procedure Delete(Sprite: TsgeSprite);
  end;


implementation

type
  TSpriteTableItem = class
  public
    Count: Word;
    Sprite: TsgeGraphicOpenGLSprite;
  end;


constructor TsgeGraphicOpenGLSpriteTable.Create;
begin
  FTable := TFPHashObjectList.Create(True);
end;


destructor TsgeGraphicOpenGLSpriteTable.Destroy;
begin
  FTable.Free;
end;


procedure TsgeGraphicOpenGLSpriteTable.Clear;
begin

end;


function TsgeGraphicOpenGLSpriteTable.Add(Sprite: TsgeSprite): TsgeGraphicOpenGLSprite;
begin

end;


procedure TsgeGraphicOpenGLSpriteTable.Delete(Sprite: TsgeSprite);
begin

end;



end.

