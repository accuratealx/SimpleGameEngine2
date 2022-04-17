{
Пакет             Simple Game Engine 2
Файл              sgeScreenFade.pas
Версия            1.0
Создан            06.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс для затемнения экрана
}
{$Include Defines.inc}

unit sgeScreenFade;

{$mode objfpc}{$H+}

interface

uses
  sgeTypes, sgeGraphicColor;


type
  //Виды затемнения
  TsgeScreenFadeMode = (sfmNormalToColor, sfmColorToNormal, sfmNormalToColorToNormal, sfmColorToNormalToColor);


  //Функция обработчик прошедшего времени
  TsgeScreenFadeProc = procedure(Time: TsgePassedTime) of Object;


  TsgeScreenFade = class
  private
    FValues: array of Single;                                       //Массив прозрачностей для градиента
    FEnable: Boolean;                                               //Активность
    FStartTime: Int64;                                              //Время запуска
    FTime: Cardinal;                                                //Длительность в мс
    FColor: TsgeColor;                                              //Промежуточный цвет
    FCallBackProc: TsgeScreenFadeProc;                              //Обработчик прошедшего времени

    FTimeBegin: Boolean;                                            //Флаг срабатывания начала
    FTimeMiddle: Boolean;                                           //Флаг срабатывания середины

    procedure ValuesClear;
    procedure ValuesAdd(aValue: Single);
    function  ValuesGradient(Pos: Single): Single;
    procedure ValuesPrepare(Mode: TsgeScreenFadeMode; Color: TsgeColor);

    procedure DoCallBack(Time: TsgePassedTime);
  public
    destructor Destroy; override;

    procedure Start(Mode: TsgeScreenFadeMode; Color: TsgeColor; Time: Cardinal; Proc: TsgeScreenFadeProc = nil);
    function  GetColor: TsgeColor;

    property Enable: Boolean read FEnable;
  end;



implementation

uses
  sgeOSPlatform;


procedure TsgeScreenFade.ValuesClear;
begin
  SetLength(FValues, 0);
end;


procedure TsgeScreenFade.ValuesAdd(aValue: Single);
var
  Idx: Integer;
begin
  Idx := Length(FValues);
  SetLength(FValues, Idx + 1);
  FValues[Idx] := aValue;
end;


function TsgeScreenFade.ValuesGradient(Pos: Single): Single;
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
    if (Pos >= X1) and (Pos <= X2) then Break;
    Inc(I);
  end;

  Result := FValues[I] + (FValues[I + 1] - FValues[I]) * ((Pos - X1) / (X2 - X1));
end;


procedure TsgeScreenFade.ValuesPrepare(Mode: TsgeScreenFadeMode; Color: TsgeColor);
begin
  //Заполнить массив прозрачностей
  ValuesClear;
  case Mode of
    sfmNormalToColor:
    begin
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
    end;

    sfmColorToNormal:
    begin
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
    end;

    sfmNormalToColorToNormal:
    begin
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
    end;

    sfmColorToNormalToColor:
    begin
      ValuesAdd(FColor.Alpha);
      ValuesAdd(0);
      ValuesAdd(FColor.Alpha);
    end;
  end;
end;


procedure TsgeScreenFade.DoCallBack(Time: TsgePassedTime);
begin
  if Assigned(FCallBackProc) then
    FCallBackProc(Time);
end;


destructor TsgeScreenFade.Destroy;
begin
  ValuesClear;
end;


procedure TsgeScreenFade.Start(Mode: TsgeScreenFadeMode; Color: TsgeColor; Time: Cardinal; Proc: TsgeScreenFadeProc);
begin
  //Восстановить флажки
  FTimeBegin := False;
  FTimeMiddle := False;

  //Запомнить параметры
  if Time < 1 then
    Time := 1;
  FTime := Time;
  FCallBackProc := Proc;
  FEnable := True;
  FColor := Color;
  FStartTime := sgeGetTickCount;

  ValuesPrepare(Mode, FColor);
end;


function TsgeScreenFade.GetColor: TsgeColor;
var
  cTime: Int64;
  Pos: Single;
begin
  Result := sgeChangeColorAlpha(FColor, 0);

  //Сколько времени прошло с момента запуска
  cTime := sgeGetTickCount - FStartTime;
  if cTime <= FTime then
  begin
    //Положение градиента
    Pos := cTime / FTime;

    //Время начала
    if not FTimeBegin and (Pos >= 0) then
    begin
      FTimeBegin := True;
      DoCallBack(ptBegin);
    end;

    //Время середины
    if not FTimeMiddle and (Pos >= 0.5) then
    begin
      FTimeMiddle := True;
      DoCallBack(ptMiddle);
    end;

    Result := sgeChangeColorAlpha(FColor, ValuesGradient(cTime / FTime))
  end
  else
  begin
    DoCallBack(ptEnd);
    FEnable := False;
  end;
end;




end.

