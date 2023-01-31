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

    function SpriteToStringName(Sprite: TsgeSprite): ShortString;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    function  Add(Sprite: TsgeSprite): TsgeGraphicOpenGLSprite;
    procedure Delete(Sprite: TsgeSprite);
  end;


var
  OpenGLSpriteTable: TsgeGraphicOpenGLSpriteTable;


implementation

uses
  sgeSystemUtils;

type
  TSpriteTableItem = class
  public
    Count: Integer;
    Sprite: TsgeGraphicOpenGLSprite;
  end;


function TsgeGraphicOpenGLSpriteTable.SpriteToStringName(Sprite: TsgeSprite): ShortString;
var
  Addr: Pointer;
  I: Cardinal;
begin
  Addr := @Sprite;
  I := UIntPtr(Addr);
  Result := sgeIntToHEX(I, 16);
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
var
  i: Integer;
begin
  for i := FTable.Count - 1 downto 0 do
    TSpriteTableItem(FTable.Items[i]).Sprite.Free;

  FTable.Clear;
end;


function TsgeGraphicOpenGLSpriteTable.Add(Sprite: TsgeSprite): TsgeGraphicOpenGLSprite;
var
  SpriteName: ShortString;
  Data: TSpriteTableItem;
begin
  //Имя спрайта
  SpriteName := SpriteToStringName(Sprite);

  //Проверить существование спрайта в таблице
  Data := TSpriteTableItem(FTable.Find(SpriteName));

  //Если нет спрайта, то загрузить
  if Data = nil then
  begin
    Data := TSpriteTableItem.Create;
    Data.Count := 1;
    Data.Sprite := TsgeGraphicOpenGLSprite.Create(Sprite);
    FTable.Add(SpriteName, Data);
  end
  else
    Data.Count := Data.Count + 1;

  Result := Data.Sprite;
end;


procedure TsgeGraphicOpenGLSpriteTable.Delete(Sprite: TsgeSprite);
var
  SpriteName: ShortString;
  Data: TSpriteTableItem;
begin
  //Имя спрайта
  SpriteName := SpriteToStringName(Sprite);

  //Проверить существование спрайта в таблице
  Data := TSpriteTableItem(FTable.Find(SpriteName));

  //Если спрайт есть, то удалить
  if Data <> nil then
  begin
    //Уменьшить Count
    Data.Count := Data.Count - 1;

    //Удалить если больше не используется
    if Data.Count <= 0 then
    begin
      Data.Sprite.Free;
      FTable.Delete(FTable.IndexOf(Data));
    end;
  end;
end;


initialization
begin
  OpenGLSpriteTable := TsgeGraphicOpenGLSpriteTable.Create;
end;


finalization
begin
  OpenGLSpriteTable.Free;
end;


end.

