{
Пакет             Simple Game Engine 2
Файл              sgeTypes.pas
Версия            1.3
Создан            22.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Общие типы
}
{$Include Defines.inc}

unit sgeTypes;

{$mode objfpc}{$H+}

interface


const
  //Приоритет слоев GUI
  Graphic_LayerIndex_Cursor = $FFFF;
  Graphic_LayerIndex_Shell = $FFFE;
  Graphic_LayerIndex_GUI = $FFFD;


type
  //Кнопки мыши
  TsgeMouseButton = (mbLeft, mbMiddle, mbRight, mbExtra1, mbExtra2);
  TsgeMouseButtons = set of TsgeMouseButton;


  //Специальные кнопки клавиатуры
  TsgeKeyboardButton = (kbLeftAlt, kbRightAlt, kbLeftCtrl, kbRightCtrl, kbLeftShift, kbRightShift,
                        kbAlt, kbCtrl, kbShift, kbCapsLock, kbNumLock, kbScrollLock, kbInsert);
  TsgeKeyboardButtons = set of TsgeKeyboardButton;


  //Модификаторы клавиш
  TsgeKeyboardShifts = set of (ksLeftAlt, ksRightAlt, ksLeftCtrl, ksRightCtrl, ksLeftShift, ksRightShift);


  //Горизонтальное выравнивание
  TsgeHorizontalAlign = (haLeft, haCenter, haRight);


  //Вертикальное выравнивание
  TsgeVerticalAlign = (vaTop, vaCenter, vaBottom);


  //Модификаторы поиска
  TsgeSearchOptions = set of (soUnique, soCaseSensivity);


  //Способы сортировки
  TsgeSortMode = (smBubble);


  //Направление сортировки
  TsgeDirection = (dForward, dBackward);


  //Метод обрезания :)
  TsgeTrimSide = (tsLeft, tsRight, tsBoth);


  //Тип прошедшего времени
  TsgePassedTime = (ptBegin, ptMiddle, ptEnd);


  TsgeIntPoint = record
    X: Integer;
    Y: Integer;
  end;
  PsgeIntPoint = ^TsgeIntPoint;


  TsgeFloatPoint = record
    X: Single;
    Y: Single;
  end;
  PsgeFloatPoint = ^TsgeFloatPoint;


  TsgeSmallPoint = record
    X: SmallInt;
    Y: SmallInt;
  end;
  PsgeSmallPoint = ^TsgeSmallPoint;


  TsgeIntRect = record
    X1: Integer;
    Y1: Integer;
    X2: Integer;
    Y2: Integer;
  end;
  PsgeIntRect = ^TsgeIntRect;


  TsgeFloatRect = record
    X1: Single;
    Y1: Single;
    X2: Single;
    Y2: Single;
  end;
  PsgeFloatRect = ^TsgeFloatRect;


function  sgeGetIntPoint(X, Y: Integer): TsgeIntPoint;
function  sgeGetFloatPoint(X, Y: Single): TsgeFloatPoint;
function  sgeGetSmallPoint(X, Y: SmallInt): TsgeSmallPoint;
function  sgeGetIntRect(X1, Y1, X2, Y2: Integer): TsgeIntRect;
function  sgeGetFloatRect(X1, Y1, X2, Y2: Single): TsgeFloatRect;

function  sgeFitRectIn(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;
function  sgeFitRectOut(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;

function  sgeGetKeyboardShiftsFromKeboardButtons(KeyboardButtons: TsgeKeyboardButtons): TsgeKeyboardShifts;

implementation


function sgeGetIntPoint(X, Y: Integer): TsgeIntPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function sgeGetFloatPoint(X, Y: Single): TsgeFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function sgeGetSmallPoint(X, Y: SmallInt): TsgeSmallPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;


function sgeGetIntRect(X1, Y1, X2, Y2: Integer): TsgeIntRect;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;


function sgeGetFloatRect(X1, Y1, X2, Y2: Single): TsgeFloatRect;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;


function sgeFitRectIn(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;
var
  k: Single;
begin
  if (BaseW * RectH < RectW * BaseH) then k := BaseW / RectW else k := BaseH / RectH;
  Result.X := Round(k * RectW);
  Result.Y := Round(k * RectH);
end;


function sgeFitRectOut(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;
var
  k: Single;
begin
  if (BaseW * RectH > RectW * BaseH) then k := BaseW / RectW else k := BaseH / RectH;
  Result.X := Round(k * RectW);
  Result.Y := Round(k * RectH);
end;


function sgeGetKeyboardShiftsFromKeboardButtons(KeyboardButtons: TsgeKeyboardButtons): TsgeKeyboardShifts;
begin
  Result := [];
  if (kbLeftAlt in KeyboardButtons) then Include(Result, ksLeftAlt);
  if (kbRightAlt in KeyboardButtons) then Include(Result, ksRightAlt);
  if (kbLeftCtrl in KeyboardButtons) then Include(Result, ksLeftCtrl);
  if (kbRightCtrl in KeyboardButtons) then Include(Result, ksRightCtrl);
  if (kbLeftShift in KeyboardButtons) then Include(Result, ksLeftShift);
  if (kbRightShift in KeyboardButtons) then Include(Result, ksRightShift);
end;



end.

