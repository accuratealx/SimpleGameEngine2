{
Пакет             Simple Game Engine 2
Файл              sgeTypes.pas
Версия            1.5
Создан            22.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Общие типы
}
{$Include Defines.inc}

unit sgeTypes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

type
  //Массив байтов
  TsgeByteArray = array of Byte;


  //Кнопки мыши
  TsgeMouseButton = (
    mbLeft,   //Левая
    mbMiddle, //Средняя
    mbRight,  //Правая
    mbExtra1, //Дополнительная 1
    mbExtra2  //Дополнительная 2
  );
  TsgeMouseButtons = set of TsgeMouseButton;


  //Специальные кнопки клавиатуры
  TsgeKeyboardButton = (
    kbLeftAlt,    //Левый альт
    kbRightAlt,   //Правый альт
    kbLeftCtrl,   //Левый контрол
    kbRightCtrl,  //Правый контрол
    kbLeftShift,  //Левый шифт
    kbRightShift, //Правый шифт
    kbAlt,        //Альт
    kbCtrl,       //Контрол
    kbShift,      //Шифт
    kbCapsLock,   //Верхний регистр
    kbNumLock,    //Дополнительная цифровая клавиатура
    kbScrollLock, //
    kbInsert      //Вставка
  );
  TsgeKeyboardButtons = set of TsgeKeyboardButton;


  //Модификаторы клавиш
  TsgeKeyboardShifts = set of (
    ksLeftAlt,    //Левый альт
    ksRightAlt,   //Правый альт
    ksLeftCtrl,   //Левый контрол
    ksRightCtrl,  //Правый контрол
    ksLeftShift,  //Левый шифт
    ksRightShift  //Правый шифт
  );


  //Ориантация
  TsgeOrientation = (
    oHorizontal,  //Горизонтальная
    oVertical     //Вертикальная
  );


  //Горизонтальное выравнивание
  TsgeHorizontalAlign = (
    haLeft,   //По левому краю
    haCenter, //По центру
    haRight   //По правлму краю
  );


  //Вертикальное выравнивание
  TsgeVerticalAlign = (
    vaTop,    //По верху
    vaCenter, //По центру
    vaBottom  //По низу
  );


  //Модификаторы поиска
  TsgeSearchOptions = set of (
    soUnique,       //Уникальный
    soCaseSensivity //Регистрозависимый
  );


  //Способы сортировки
  TsgeSortMode = (
    smBubble  //Пузырьком
  );


  //Направление сортировки
  TsgeDirection = (
    dForward, //Вперед
    dBackward //Назад
  );


  //Метод обрезания :)
  TsgeTrimSide = (
    tsLeft,   //Слева
    tsRight,  //Справа
    tsBoth    //Вокруг
  );


  //Виды затемнения
  TsgeFadeMode = (
    fmNormalToColor,          //Экран - цвет
    fmColorToNormal,          //Цвет - Экран
    fmNormalToColorToNormal,  //Экран - Цвет - Экран
    fmColorToNormalToColor    //Цвет - Экран - Цвет
  );


  //Тип прошедшего времени
  TsgePassedTime = (
    ptBegin,  //Начало
    ptMiddle, //Середина
    ptEnd     //Конец
  );


  //Штриховка линий
  TsgeLineStipple = (
    lsSolid,      //Сплошная
    lsDash,       //Тире
    lsNarrowDash, //Узкое тире
    lsWideDash,   //Широкое тире
    lsDot,        //Точки
    lsDashDot,    //Тире-точка
    lsDashDotDot  //Тире-точка-точка
  );


  //Тип отражения
  TsgeReflect = (
    rHorizontal,  //По горизонтали
    rVertical     //По вертикали
  );
  TsgeReflectSet = set of TsgeReflect;


  //Ограничивающий вывод прямоугольник
  TsgeClipRect = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;

    class operator = (A, B: TsgeClipRect): Boolean;
  end;


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


  TsgeFloatTriple = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
  PsgeFloatTriple = ^TsgeFloatTriple;


  TsgeIntTriple = record
    X: Integer;
    Y: Integer;
    Z: Integer;
  end;
  PsgeIntTriple = ^TsgeIntTriple;


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


function sgeGetClipRect(X, Y, Width, Height: Integer): TsgeClipRect;
function sgeRestrictClipRect(Source, Parent: TsgeClipRect): TsgeClipRect;

function sgeGetIntPoint(X, Y: Integer): TsgeIntPoint;
function sgeGetFloatPoint(X, Y: Single): TsgeFloatPoint;
function sgeGetSmallPoint(X, Y: SmallInt): TsgeSmallPoint;

function sgeGetFloatTriple(X, Y, Z: Single): TsgeFloatTriple;
function sgeGetIntTriple(X, Y, Z: Integer): TsgeIntTriple;

function sgeGetIntRect(X1, Y1, X2, Y2: Integer): TsgeIntRect;
function sgeGetFloatRect(X1, Y1, X2, Y2: Single): TsgeFloatRect;

function sgeFitRectIn(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;
function sgeFitRectOut(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;

function sgeGetKeyboardShiftsFromKeboardButtons(KeyboardButtons: TsgeKeyboardButtons): TsgeKeyboardShifts;

function sgeGetVerticaAlignOffset(Mode: TsgeVerticalAlign; BaseHeight, ElementHeight: Integer): Integer;
function sgeGetHorizontalAlignOffset(Mode: TsgeHorizontalAlign; BaseWidth, ElementWidth: Integer): Integer;

implementation


class operator TsgeClipRect. = (A, B: TsgeClipRect): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Width = B.Width) and (A.Height = B.Height);
end;


function sgeGetClipRect(X, Y, Width, Height: Integer): TsgeClipRect;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;


function sgeRestrictClipRect(Source, Parent: TsgeClipRect): TsgeClipRect;
begin
  Result := Source;

  if Result.X < Parent.X then
    Result.X := Parent.X;

  if Result.Y < Parent.Y then
    Result.Y := Parent.Y;

  if Result.X + Result.Width > Parent.X + Parent.Width then
    Result.Width := Parent.Width - (Source.X - Parent.X);

  if Result.Y + Result.Height > Parent.Y + Parent.Height then
    Result.Height := Parent.Height - (Source.Y - Parent.Y);
end;


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


function sgeGetFloatTriple(X, Y, Z: Single): TsgeFloatTriple;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


function sgeGetIntTriple(X, Y, Z: Integer): TsgeIntTriple;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
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
  if (BaseW * RectH < RectW * BaseH) then
    k := BaseW / RectW
  else
    k := BaseH / RectH;

  Result.X := Round(k * RectW);
  Result.Y := Round(k * RectH);
end;


function sgeFitRectOut(BaseW, BaseH, RectW, RectH: Integer): TsgeIntPoint;
var
  k: Single;
begin
  if (BaseW * RectH > RectW * BaseH) then
    k := BaseW / RectW
  else
    k := BaseH / RectH;

  Result.X := Round(k * RectW);
  Result.Y := Round(k * RectH);
end;


function sgeGetKeyboardShiftsFromKeboardButtons(KeyboardButtons: TsgeKeyboardButtons): TsgeKeyboardShifts;
begin
  Result := [];
  if (kbLeftAlt in KeyboardButtons) then
    Include(Result, ksLeftAlt);

  if (kbRightAlt in KeyboardButtons) then
    Include(Result, ksRightAlt);

  if (kbLeftCtrl in KeyboardButtons) then
    Include(Result, ksLeftCtrl);

  if (kbRightCtrl in KeyboardButtons) then
    Include(Result, ksRightCtrl);

  if (kbLeftShift in KeyboardButtons) then
    Include(Result, ksLeftShift);

  if (kbRightShift in KeyboardButtons) then
    Include(Result, ksRightShift);
end;


function sgeGetVerticaAlignOffset(Mode: TsgeVerticalAlign; BaseHeight, ElementHeight: Integer): Integer;
begin
  case Mode of
    vaTop:
      Result := 0;

    vaCenter:
      Result := Round((BaseHeight - ElementHeight) / 2);

    vaBottom:
      Result := BaseHeight - ElementHeight;
  end;
end;


function sgeGetHorizontalAlignOffset(Mode: TsgeHorizontalAlign; BaseWidth, ElementWidth: Integer): Integer;
begin
  case Mode of
    haLeft:
      Result := 0;

    haCenter:
      Result := Round((BaseWidth - ElementWidth) / 2);

    haRight:
      Result := BaseWidth - ElementWidth;
  end;
end;



end.

