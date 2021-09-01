{
Пакет             Simple Game Engine 2
Файл              sgeThread.pas
Версия            1.4
Создан            10.04.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс потока
}
{$Include Defines.inc}

unit sgeThread;

{$mode objfpc}{$H+}

interface

uses
  sgeSystemEvent, sgeErrors;


type
  //Приоритет потока
  TsgeThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest, tpTimeCritical);

  //Модификатор окончания выполнения метода (Нет, Заснуть, Уничтожить)
  TsgeThreadProcEndModifier = (tpemNone, tpemSuspend, tpemDestroy);

  //Метод для обработки
  TsgeThreadProc = procedure of object;


  TsgeThread = class
  private
    //Классы
    FWaitEvent: TsgeSystemEvent;                  //Синхронизатор
    FException: EsgeException;                    //Последнее исключение

    //Поля
    FHandle: THandle;                             //Указатель потока
    FID: DWORD;                                   //ID потока в системе
    FFreeOnTerminate: Boolean;                    //Удалять объект по завершении работы
    FTerminated: Boolean;                         //Флаг работы бесконечного цикла
    FSuspended: Boolean;                          //Флаг остановки
    FFinished: Boolean;                           //Флаг завершения работы

    //Вспомогательные поля
    FEndProcModifier: TsgeThreadProcEndModifier;  //Модификатор окончания работы потока
    FRunOnceProcWait: TsgeThreadProc;             //Указатель на метод однократной работы с ожиданием
    FRunOnceProc: TsgeThreadProc;                 //Указатель на метод однократной работы
    FRunLoopProc: TsgeThreadProc;                 //Указатель на метод бесконечной работы

    function  GetException: EsgeException;        //Вернуть ошибку и обнулить указатель
    function  GetPriority: TsgeThreadPriority;
    procedure SetPriority(AValue: TsgeThreadPriority);
    function  GetProcessorID: DWORD;
    procedure SetProcessorID(AValue: DWORD);
  public
    constructor Create(Proc: TsgeThreadProc = nil; Suspended: Boolean = True; FreeOnTerminate: Boolean = False; StackSize: Integer = DefaultStackSize);
    destructor  Destroy; override;

    procedure RunProc(Proc: TsgeThreadProc; Modifier: TsgeThreadProcEndModifier = tpemNone);
    procedure RunProcAndWait(Proc: TsgeThreadProc; Modifier: TsgeThreadProcEndModifier = tpemNone);

    procedure Terminate(Wait: Boolean = False);
    procedure Suspend;
    procedure Resume;

    property Handle: THandle read FHandle;
    property ID: DWORD read FID;
    property Priority: TsgeThreadPriority read GetPriority write SetPriority;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Finished: Boolean read FFinished;
    property ProcessorID: DWORD read GetProcessorID write SetProcessorID;
    property LoopProc: TsgeThreadProc read FRunLoopProc write FRunLoopProc;
    property Exception: EsgeException read GetException;
  end;


implementation

uses
  sgeOSPlatform;

const
  _UNITNAME = 'Thread';

  Err_CantCreateThread          = 'CantCreateThread';
  Err_CantSuspendThread         = 'CantSuspendThread';
  Err_CantResumeThread          = 'CantResumeThread';
  Err_CantGetThreadProcessorID  = 'CantGetThreadProcessorID';
  Err_CantSetThreadProcessorID  = 'CantSetThreadProcessorID';
  Err_CantSetThreadPriority     = 'CantSetThreadPriority';
  Err_CantGetThreadPriority     = 'CantGetThreadPriority';
  Err_EmptyMethodPointer        = 'EmptyMethodPointer';


  //Константы приоритетов
  Priorities: array [TsgeThreadPriority] of Integer = (
    THREAD_PRIORITY_IDLE,
    THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL,
    THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST,
    THREAD_PRIORITY_TIME_CRITICAL);



//Потоковая функция
function ThreadProc(ThreadObjPtr: Pointer): PtrInt; stdcall;
var
  Thread: TsgeThread absolute ThreadObjPtr;
  Modifier: TsgeThreadProcEndModifier;
begin
  //Бесконечный цикл
  while not Thread.FTerminated do
    begin

    //Проверить указатель на одноразовый метод с ожиданием
    if Thread.FRunOnceProcWait <> nil then
      begin
      //Выполнить метод
      try
        Thread.FRunOnceProcWait();
      except
        on E: EsgeException do
          Thread.FException := E;
      end;

      //Стереть указатель
      Thread.FRunOnceProcWait := nil;

      //Поднять флаг окончания работы метода
      Thread.FWaitEvent.Up;

      //Запомнить текущий модификатор
      Modifier := Thread.FEndProcModifier;

      //Обнулить модификатор
      Thread.FEndProcModifier := tpemNone;

      //Обработать модификатор
      case Modifier of
        tpemSuspend: Thread.Suspend;
        tpemDestroy:
          begin
          Thread.FFreeOnTerminate := True;
          Thread.FTerminated := True;
          Continue;
          end;
      end;
      end;


    //Проверить указатель анонимной функции
    if Thread.FRunOnceProc <> nil then
      begin
      //Выполнить метод
      try
        Thread.FRunOnceProc();
      except
        on E: EsgeException do
          Thread.FException := E;
      end;

      //Стереть указатель
      Thread.FRunOnceProc := nil;

      //Запомнить текущий модификатор
      Modifier := Thread.FEndProcModifier;

      //Обнулить модификатор
      Thread.FEndProcModifier := tpemNone;

      //Обработать модификатор
      case Modifier of
        tpemSuspend: Thread.Suspend;
        tpemDestroy:
          begin
          Thread.FFreeOnTerminate := True;
          Thread.FTerminated := True;
          Continue;
          end;
      end;
      end;



    //Проверить указатель бесконечного метода
    if Thread.FRunLoopProc <> nil then
        try
          Thread.FRunLoopProc();
        except
          on E: EsgeException do
            begin
            Thread.FException := E;       //Запомнить исключение
            Thread.FTerminated := True;   //Остановить поток
            end;
        end;

    end;  // while not Terminate



  //Флаг окончания работы потока
  Thread.FFinished := True;

  //Уничтожить поток если установлен флаг
  if Thread.FFreeOnTerminate then Thread.Free;

  //Результат
  Result := 0;
end;


function TsgeThread.GetException: EsgeException;
begin
  Result := FException;
  FException := nil;
end;


function TsgeThread.GetPriority: TsgeThreadPriority;
var
  Res: Integer;
  I: TsgeThreadPriority;
begin
  Result := tpNormal;

  if not sgeGetThreadPriority(FHandle, Res) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetThreadPriority);

  for I := Low(TsgeThreadPriority) to High(TsgeThreadPriority) do
    if Priorities[I] = Res then Result := I;
end;


procedure TsgeThread.SetPriority(AValue: TsgeThreadPriority);
begin
  if not sgeSetThreadPriority(FHandle, Priorities[AValue]) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetThreadPriority);
end;


function TsgeThread.GetProcessorID: DWORD;
begin
  if not sgeGetThreadIdealProcessor(FHandle, Result) then
    raise EsgeException.Create(_UNITNAME, Err_CantGetThreadProcessorID);
end;


procedure TsgeThread.SetProcessorID(AValue: DWORD);
var
  Max: Word;
begin
  //Поправить наибольшее значение
  Max := GetProcessorCount - 1;
  if AValue > Max then AValue := Max;

  if not sgeSetThreadIdealProcessor(FHandle, AValue) then
    raise EsgeException.Create(_UNITNAME, Err_CantSetThreadProcessorID);
end;


constructor TsgeThread.Create(Proc: TsgeThreadProc; Suspended: Boolean; FreeOnTerminate: Boolean; StackSize: Integer);
begin
  //Поля
  FFreeOnTerminate := FreeOnTerminate;
  FTerminated := False;
  FSuspended := True;
  FFinished := False;
  FEndProcModifier := tpemNone;
  FRunOnceProc := nil;
  FRunOnceProcWait := nil;
  FRunLoopProc := Proc;

  //Создать синхронизатор
  FWaitEvent := TsgeSystemEvent.Create(True, False);

  //Создать поток
  FHandle := sgeCreateThread(StackSize, @ThreadProc, Pointer(Self), FID);
  if FHandle = 0 then
    raise EsgeException.Create(_UNITNAME, Err_CantCreateThread);

  //Запуск потока
  if not Suspended then Resume;
end;


destructor TsgeThread.Destroy;
begin
  //Если нет потока, то ничего не делать
  if FHandle <> 0 then
    begin
    //Почистить указатели
    FRunLoopProc := nil;
    FRunOnceProc := nil;
    FRunOnceProcWait := nil;

    //Если поток не завершил свою работу, то прибить
    if not FFinished then Terminate(True);

    //Обнулить переменные
    FHandle := 0;
    FID := 0;
    end;

  //Удалить синхронизатор
  FWaitEvent.Free;

  //Удалить поток из системы
  sgeDeleteThread(FHandle);
end;


procedure TsgeThread.RunProc(Proc: TsgeThreadProc; Modifier: TsgeThreadProcEndModifier);
begin
  if Proc = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyMethodPointer);

  //Запомнить модификатор
  FEndProcModifier := Modifier;

  //Запомнить указатель
  FRunOnceProc := Proc;

  //Разбудить поток
  Resume;
end;


procedure TsgeThread.RunProcAndWait(Proc: TsgeThreadProc; Modifier: TsgeThreadProcEndModifier);
begin
  if Proc = nil then
    raise EsgeException.Create(_UNITNAME, Err_EmptyMethodPointer);

  //Запомнить модификатор
  FEndProcModifier := Modifier;

  //Изменить указатель одноразового метода
  FRunOnceProcWait := Proc;

  //Вывести из сна
  Resume;

  //Ждать сигнала окончания работы метода
  FWaitEvent.Wait;
end;


procedure TsgeThread.Terminate(Wait: Boolean);
begin
  //Переключить флаг работы
  FTerminated := True;

  //Вывести из сна
  Resume;

  //Подождать завершения
  if Wait and not FFinished then sgeWaitForSingleObject(FHandle, INFINITE);
end;


procedure TsgeThread.Suspend;
begin
  if FSuspended then Exit;

  FSuspended := True;

  if not sgeSuspendThread(FHandle) then
    raise EsgeException.Create(_UNITNAME, Err_CantSuspendThread);
end;


procedure TsgeThread.Resume;
begin
  if not FSuspended then Exit;

  FSuspended := False;

  if not sgeResumeThread(FHandle) then
    raise EsgeException.Create(_UNITNAME, Err_CantResumeThread);
end;



end.

