{
Пакет             Simple Game Engine 2
Файл              sgeSpriteLoaderGDIP.pas
Версия            1.0
Создан            05.01.2022
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс загрузчика спрайтов: GDIPlus
                  форматы: .bmp, .jpeg, .gif, .emf, .wmf, .tif, .png, .ico
}
{$Include Defines.inc}

unit sgeSpriteLoaderGDIP;

{$mode objfpc}{$H+}

interface

uses
  sgeMemoryStream,
  sgeSpriteLoader;


type
  TsgeSpriteLoaderGDIP = class(TsgeSpriteLoader)
  public
    procedure FromMemoryStream(Stream: TsgeMemoryStream); override;
  end;


implementation

uses
  sgeErrors, sgeSystemUtils,
  GDIPAPI, Windows, ActiveX;

const
  _UNITNAME = 'SpriteLoaderGDIP';

  Err_CantAllocMemory             = 'CantAllocMemory';
  Err_CantLockMemory              = 'CantLockMemory';
  Err_CantAccessMemory            = 'CantAccessMemory';
  Err_CantCreateIStream           = 'CantCreateIStream';
  Err_CantCreateBitmapFromIStream = 'CantCreateBitmapFromIStream';
  Err_CantGetWidth                = 'CantGetWidth';
  Err_CantGetHeight               = 'CantGetHeight';



procedure TsgeSpriteLoaderGDIP.FromMemoryStream(Stream: TsgeMemoryStream);
var
  i, BytesPerLine, IdxBmp, IdxData: Integer;
  HBuf: HGLOBAL;
  PBuf: Pointer;
  PStream: IStream;
  Bmp: GpBitmap;
  BmpData: TBitmapData;
  Rct: TGPRect;
begin
  //Выделить память для данных
  HBuf := GlobalAlloc(GMEM_MOVEABLE, Stream.Size);

  //Проверить выделение памяти
  if HBuf = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantAllocMemory, sgeIntToStr(Stream.Size));

  try
    //Заблокировать память
    PBuf := GlobalLock(HBuf);

    //Проверить на доступ к памяти
    if PBuf = nil then
      raise EsgeException.Create(_UNITNAME, Err_CantLockMemory);

    //Скопировать в буфер Stream
    CopyMemory(PBuf, Stream.Data, Stream.Size);

    //Создать IStream
    if not CreateStreamOnHGlobal(HBuf, False, PStream) = S_OK then
      raise EsgeException.Create(_UNITNAME, Err_CantCreateIStream);

    //Создать битмап
    if GdipCreateBitmapFromStream(PStream, Bmp) <> Ok then
      raise EsgeException.Create(_UNITNAME, Err_CantCreateBitmapFromIStream);

    //Прочитать ширину
    FWidth := 0;
    if GdipGetImageWidth(Bmp, FWidth) <> Ok then
      raise EsgeException.Create(_UNITNAME, Err_CantGetWidth);

    //Прочитать высоту
    FHeight := 0;
    if GdipGetImageHeight(Bmp, FHeight) <> Ok then
      raise EsgeException.Create(_UNITNAME, Err_CantGetHeight);

    //Заблокировать память
    Rct := MakeRect(0, 0, FWidth, FHeight);
    if GdipBitmapLockBits(Bmp, @Rct, ImageLockModeRead, PixelFormat32bppARGB, @BmpData) <> Ok then
      raise EsgeException.Create(_UNITNAME, Err_CantAccessMemory);

    //Подготовка буфера
    BytesPerLine := FWidth * 4;                                     //Байтов в строке
    FSize := BytesPerLine * FHeight;                                //Всего данных
    FData := AllocMem(FSize);                                       //Выделить память

    //Переворачивание рисунка
    for i := 0 to FHeight - 1 do
    begin
      IdxBmp := i * BytesPerLine;                                   //Смещение линии GPBitmap
      IdxData := FSize - (i * BytesPerLine) - BytesPerLine;         //Смещение буфера для OpenGL
      Move(Pointer(BmpData.Scan0 + IdxBmp)^, Pointer(FData + IdxData)^, BytesPerLine);  //Копирование из GPBitmap в буфер
    end;

    //Разблокировать память
    GdipBitmapUnlockBits(Bmp, @BmpData);

  finally
    GdipDisposeImage(Bmp);
    GlobalUnlock(HBuf);
    GlobalFree(HBuf);
    PStream := nil;
  end;
end;




/////////////////////////////////////////////////////////////////////////
var
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: Cardinal;

initialization
begin
  //Заполнение полей рекорда инициализации GDI+
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;

  //Запуск GDI+
  if GdiplusStartup(gdiplusToken, @StartupInput, nil) <> Ok then
  begin
    MessageBox(0, 'Cant initialize GDI+', 'Fatal error', MB_ICONERROR or MB_OK);
    halt;
  end;
end;


finalization
begin
  //Удалить GDI+
  GdiplusShutdown(gdiplusToken);
end;

end.

