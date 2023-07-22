{
Пакет             Simple Game Engine 2
Файл              sgeSpriteSaverBmp.pas
Версия            1.0
Создан            17.07.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс сохранения спрайтов: Базовый
}
{$Include Defines.inc}

unit sgeSpriteSaverBmp;

{$mode ObjFPC}{$H+}

interface

uses
  sgeMemoryStream,
  sgeSprite,
  sgeSpriteSaver;

type
  TsgeSpriteSaverBmp = class(TsgeSpriteSaver)
  public
    procedure ToMemoryStream(Stream: TsgeMemoryStream; Sprite: TsgeSprite); override;
  end;


implementation

uses
  windows;

procedure TsgeSpriteSaverBmp.ToMemoryStream(Stream: TsgeMemoryStream; Sprite: TsgeSprite);
var
  BFH: TBitmapFileHeader;
  BIH: TBITMAPINFOHEADER;
  BytesPerLine, Trash, Size, i, j, szFileHeader, szInfoHeader: Integer;
  Data: array of Byte;
  SpritePtr, DataPtr: Pointer;
begin
  //Байтов в одной строке
  BytesPerLine := Sprite.Width * 3;

  //Определить количество байт для выравнивания линии кранто 4 байтам
  Trash := BytesPerLine mod 4;
  if Trash <> 0 then
    Trash := 4 - Trash;

  //Размер данных
  Size := (BytesPerLine + Trash) * Sprite.Height;

  //Выделить память под данные
  SetLength(DATA, Size);


  //Заполнить данные
  for i := 0 to Sprite.Height - 1 do
  begin
    //Указатель на строку спрайта
    SpritePtr := Sprite.Data + i * Sprite.Width * 4;

    //Указатель на строку данных
    DataPtr := @Data[0] + i * (BytesPerLine + Trash);

    for j := 0 to Sprite.Width - 1 do
    begin
      Move(SpritePtr^, DataPtr^, 3);

      SpritePtr := SpritePtr + 4;
      DataPtr := DataPtr + 3;
    end;
  end;

  //Определить размеры структур
  szFileHeader := SizeOf(TBitmapFileHeader);
  szInfoHeader := SizeOf(TBITMAPINFOHEADER);

  //Описатель BMP файла
  BFH.bfType := $4D42;                                      //Волшебное слово от микрософта - BM
  BFH.bfReserved1 := 0;
  BFH.bfReserved2 := 0;
  BFH.bfOffBits := szFileHeader + szInfoHeader;             //Смещение от начала файла до самих данных
  BFH.bfSize := BFH.bfOffBits + Size;                       //Размер файла целиком со структурами и мусором

  //Описатель BMP
  BIH.biSize := szInfoHeader;                               //Размер этой структуры. Интересно зачем
  BIH.biWidth := Sprite.Width;                              //Ширина битмапа
  BIH.biHeight := Sprite.Height;                            //Высота битмапа
  BIH.biPlanes := 1;                                        //Сколько слоёв
  BIH.biBitCount := 24;                                     //Бит на пиксель
  BIH.biCompression := BI_RGB;                              //Без сжатия
  BIH.biSizeImage := 0;                                     //Не используется без сжатия
  BIH.biXPelsPerMeter := 0;                                 //Разрешение по X
  BIH.biYPelsPerMeter := 0;                                 //Разрешение по Y
  BIH.biClrUsed := 0;                                       //Сколько цветов в таблице индексов
  BIH.biClrImportant := 0;                                  //0 - все индексы цветов доступны

  //Записать в поток
  Stream.Size := 0;                                         //Обнулить память
  Stream.Write(BFH, 0, SizeOf(BFH));                        //Заголовок файла
  Stream.Write(BIH, szFileHeader, SizeOf(BIH));             //Описание битмапа
  Stream.Write(DATA[0], szFileHeader + szInfoHeader, Size); //Записать данные

  //Очистить память
  SetLength(DATA, 0);
end;



end.

