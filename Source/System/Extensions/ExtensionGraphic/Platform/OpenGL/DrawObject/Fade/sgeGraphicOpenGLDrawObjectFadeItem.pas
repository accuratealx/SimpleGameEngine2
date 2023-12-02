{
Пакет             Simple Game Engine 2
Файл              sgeGraphicOpenGLDrawObjectFadeItem.pas
Версия            1.0
Создан            13.02.2023
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          OpenGL: Элемент отрисовки: Затемнение экрана: Элемент затемнения
}
{$Include Defines.inc}

unit sgeGraphicOpenGLDrawObjectFadeItem;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTypes,
  sgeColor;

type
  //Функция обработчик прошедшего времени
  TsgeFadeProc = procedure(Time: TsgePassedTime; ID: Integer) of Object;


  //Состояние затемнения
  TsgeFadeStatus = (
    fsPrepared,   //Готово к запуску
    fsRun,        //Работает
    fsCompleted   //Завершено
  );


  //Элемент очереди
  TsgeGraphicOpenGLDrawObjectFadeItem = class
  private
    FStatus: TsgeFadeStatus;    //Состояние задачи
    FMode: TsgeFadeMode;        //Режим перехода
    FColor: TsgeColor;          //Цвет затемнения
    FTime: Cardinal;            //Время перехода в мс
    FID: Integer;               //Идентификатор затемнения
    FTimeProc: TsgeFadeProc;    //Функция-обработчик прошедшего времени

    FValues: array of Single;   //Массив прозрачностей
    FStartTime: Int64;          //Начало затемнения
    FTimeMiddle: Boolean;       //Флаг середины перехода

    procedure ValuesClear;
    procedure ValuesAdd(aValue: Single);
    function  ValuesGetValue(Pos: Single): Single;
    procedure ValuesPrepare;

    procedure DoCallBack(Time: TsgePassedTime);
  public
    constructor Create(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer = 0; TimeProc: TsgeFadeProc = nil);
    destructor  Destroy; override;

    function GetColor: TsgeColor;

    property Status: TsgeFadeStatus read FStatus;
  end;


implementation

uses
  sgeOSPlatform;


procedure TsgeGraphicOpenGLDrawObjectFadeItem.ValuesClear;
begin
  SetLength(FValues, 0);
end;


procedure TsgeGraphicOpenGLDrawObjectFadeItem.ValuesAdd(aValue: Single);
var
  Idx: Integer;
begin
  Idx := Length(FValues);
  SetLength(FValues, Idx + 1);
  FValues[Idx] := aValue;
end;


function TsgeGraphicOpenGLDrawObjectFadeItem.ValuesGetValue(Pos: Single): Single;
var
  I, N: Integer;
  X1, X2: Single;
begin
  //Поправить диапазон
  if Pos <= 0 then
    Pos := 0;
  if Pos >= 1 then
    Pos := 1;

  //Высчитать положение
  N := Length(FValues);
  I := 0;
  X1 := 0;
  X2 := 1;

  while I < N do
  begin
    X1 := I / (N - 1);
    X2 := (I + 1) / (N - 1);
    if (Pos >= X1) and (Pos <= X2) then
      Break;
    Inc(I);
  end;

  Result := FValues[I] + (FValues[I + 1] - FValues[I]) * ((Pos - X1) / (X2 - X1));
end;


procedure TsgeGraphicOpenGLDrawObjectFadeItem.ValuesPrepare;
begin
  ValuesClear;

  case FMode of
    fmNormalToColor:
    begin
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
    end;

    fmColorToNormal:
    begin
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
    end;

    fmNormalToColorToNormal:
    begin
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
    end;

    fmColorToNormalToColor:
    begin
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
    end;
  end;
end;


procedure TsgeGraphicOpenGLDrawObjectFadeItem.DoCallBack(Time: TsgePassedTime);
begin
  if Assigned(FTimeProc) then
    FTimeProc(Time, FID);
end;


constructor TsgeGraphicOpenGLDrawObjectFadeItem.Create(Mode: TsgeFadeMode; Color: TsgeColor; Time: Cardinal; ID: Integer; TimeProc: TsgeFadeProc);
begin
  if Time < 1 then
    Time := 1;

  //Сохранить параметры
  FMode := Mode;
  FColor := Color;
  FTime := Time;
  FID := ID;
  FTimeProc := TimeProc;

  //Подготовить массив прозрачностей
  ValuesPrepare;

  //Установить статус
  FStatus := fsPrepared;
end;


destructor TsgeGraphicOpenGLDrawObjectFadeItem.Destroy;
begin
  //Почистить массив прозрачностей
  ValuesClear;
end;


function TsgeGraphicOpenGLDrawObjectFadeItem.GetColor: TsgeColor;
var
  cTime: Int64;
  Pos: Single;
begin
  //Цвет по умолчанию прозрачный
  Result := cTransparentBlack;

  //Запуск задачи
  if FStatus = fsPrepared then
  begin
    FStartTime := sgeGetTickCount;                                  //Запомнить время запуска
    FStatus := fsRun;                                               //Переключить статус
    DoCallBack(ptBegin);                                            //Вызвать функцию начала периода
  end;

  //Процесс
  if FStatus = fsRun then
  begin
    //Сколько времени прошло с момента запуска
    cTime := sgeGetTickCount - FStartTime;
    if cTime <= FTime then
    begin
      //Положение градиента
      Pos := cTime / FTime;

      //Время середины
      if not FTimeMiddle and (Pos >= 0.5) then
      begin
        FTimeMiddle := True;                                        //Запомнить что наступила середина
        DoCallBack(ptMiddle);                                       //Вызвать функцию середины периода
      end;

      //Вернуть цвет
      Result := FColor;
      Result.Alpha := ValuesGetValue(Pos);                          //Изменить цвет альфы
    end
    else
    begin
      DoCallBack(ptEnd);                                            //Вызвать функцию конца периода
      FStatus := fsCompleted;                                       //Сменить статус на выполнено
    end;
  end;
end;



end.

