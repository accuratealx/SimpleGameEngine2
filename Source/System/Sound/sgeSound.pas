{
Пакет             Simple Game Engine 2
Файл              sgeSound.pas
Версия            1.0
Создан            18.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс звука
}
{$Include Defines.inc}

unit sgeSound;

{$mode objfpc}{$H+}

interface

uses
  OpenAL;


const

{$IfDef WINDOWS}
  {$IfDef WIN64}
    OpenAlLibName = 'OpenAL x64.dll';
  {$Else}
    OpenAlLibName = 'OpenAL x32.dll';
  {$EndIf}
{$EndIf}



type
  //Способ расчёта затухания источников
  TsgeSoundDistanceModel = (sdmNone, sdmInverse, sdmInverseClamped, sdmLinear, sdmLinearClamped, sdmExponent, sdmExponentClamped);


  TsgeSound = class
  private
    FDevice: TALCdevice;                  //Указатель на устройство
    FContext: TALCcontext;                //Указатель на контекст
    FPosition: array[0..2] of Single;     //Положение в пространстве
    FOrientation: array[0..5] of Single;  //2 вектора ориентации

    procedure SetDistanceModel(ADistance: TsgeSoundDistanceModel);
    function  GetDistanceModel: TsgeSoundDistanceModel;
    procedure SetGain(AGain: Single);
    function  GetGain: Single;
    procedure SetPosX(X: Single);
    procedure SetPosY(Y: Single);
    procedure SetPosZ(Z: Single);
  public
    constructor Create(LibFile: String = OpenAlLibName);
    destructor  Destroy; override;

    property DistanceModel: TsgeSoundDistanceModel read GetDistanceModel write SetDistanceModel;
    property Gain: Single read GetGain write SetGain;
    property PosX: Single read FPosition[0] write SetPosX;
    property PosY: Single read FPosition[1] write SetPosY;
    property PosZ: Single read FPosition[2] write SetPosZ;
  end;


implementation

uses
  sgeErrors;

const
  _UNITNAME = 'Sound';

  Err_CantLoadOpenALLib   = 'CantLoadOpenALLib';
  Err_CantOpenDevice      = 'CantOpenDevice';
  Err_CantCreateContext   = 'CantCreateContext';
  Err_CantActivateContext = 'CantActivateContext';


procedure TsgeSound.SetDistanceModel(ADistance: TsgeSoundDistanceModel);
var
  Mdl: TALenum;
begin
  //Определить модель
  case ADistance of
    sdmNone           : Mdl := AL_NONE;
    sdmInverse        : Mdl := AL_INVERSE_DISTANCE;
    sdmInverseClamped : Mdl := AL_INVERSE_DISTANCE_CLAMPED;
    sdmLinear         : Mdl := AL_LINEAR_DISTANCE;
    sdmLinearClamped  : Mdl := AL_LINEAR_DISTANCE_CLAMPED;
    sdmExponent       : Mdl := AL_EXPONENT_DISTANCE;
    sdmExponentClamped: Mdl := AL_EXPONENT_DISTANCE_CLAMPED;
  end;

  //Изменить модель расчёта расстояния
  alDistanceModel(Mdl);
end;


function TsgeSound.GetDistanceModel: TsgeSoundDistanceModel;
var
  Mdl: TALenum;
begin
  //Прочитать модель
  alGetIntegerv(AL_DISTANCE_MODEL, @Mdl);

  //Определить результат
  case Mdl of
    AL_INVERSE_DISTANCE         : Result := sdmInverse;
    AL_INVERSE_DISTANCE_CLAMPED : Result := sdmInverseClamped;
    AL_LINEAR_DISTANCE          : Result := sdmLinear;
    AL_LINEAR_DISTANCE_CLAMPED  : Result := sdmLinearClamped;
    AL_EXPONENT_DISTANCE        : Result := sdmExponent;
    AL_EXPONENT_DISTANCE_CLAMPED: Result := sdmExponentClamped;
    else Result := sdmNone;
  end;
end;


procedure TsgeSound.SetGain(AGain: Single);
begin
  if AGain <= 0 then AGain := 0;  //Нижний порог усиления 0
  if AGain >= 1 then AGain := 1;  //Верхний порог усиления 1
  alListenerf(AL_GAIN, AGain);    //Изменить скалярный множитель
end;


function TsgeSound.GetGain: Single;
begin
  alGetListenerfv(AL_GAIN, @Result);
end;


procedure TsgeSound.SetPosX(X: Single);
begin
  FPosition[0] := X;
  alListenerfv(AL_POSITION, @FPosition);
end;


procedure TsgeSound.SetPosY(Y: Single);
begin
  FPosition[1] := y;
  alListenerfv(AL_POSITION, @FPosition);
end;


procedure TsgeSound.SetPosZ(Z: Single);
begin
  FPosition[2] := Z;
  alListenerfv(AL_POSITION, @FPosition);
end;


constructor TsgeSound.Create(LibFile: String);
begin
  //Загрузить библиотеку
  if not InitOpenAL(LibFile) then
    raise EsgeException.Create(_UNITNAME, Err_CantLoadOpenALLib, LibFile);

  //Открыть устройство
  FDevice := alcOpenDevice(nil);
  if FDevice = nil then
    raise EsgeException.Create(_UNITNAME, Err_CantOpenDevice);

  //Создать контекст
  FContext := alcCreateContext(FDevice, nil);
  if FContext = nil then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateContext);

  //Выбрать контекст
  if alcMakeContextCurrent(FContext) = ALC_FALSE then
    raise EsgeException.Create(_UNITNAME, Err_CantActivateContext);

  //Обнулить ошибки
  alGetError;

  //Задать вектор направления и вектор верха
  FOrientation[0] := 0;   //X
  FOrientation[1] := 0;   //Y
  FOrientation[2] := 1;   //Z
  FOrientation[3] := 0;   //X
  FOrientation[4] := -1;  //Y
  FOrientation[5] := 0;   //Z
  alListenerfv(AL_ORIENTATION, @FOrientation);  //Изменить ориентацию

  //Прочитать текущие координаты
  alGetListenerfv(AL_POSITION, @FPosition);
end;


destructor TsgeSound.Destroy;
begin
  if not Assigned(alcMakeContextCurrent) then Exit;

  alcMakeContextCurrent(nil);                       //Отменить выбор контекста
  alcDestroyContext(FContext);                      //Удалить контекст
  alcCloseDevice(FDevice);                          //Закрыть устройство
end;




end.

