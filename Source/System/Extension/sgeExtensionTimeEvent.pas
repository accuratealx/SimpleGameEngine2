{
Пакет             Simple Game Engine 2
Файл              sgeExtensionTimeEvent.pas
Версия            1.0
Создан            29.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Таймерные события
}
{$Include Defines.inc}

unit sgeExtensionTimeEvent;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeThread, sgeTimeEventList, sgeTimeEventItem, sgeEventTimeEvent;


const
  Extension_TimeEvent = 'TimeEvent';


type
  TsgeExtensionTimeEvent = class(TsgeExtensionBase)
  private
    FThread: TsgeThread;
    FTimeEventList: TsgeTimeEventList;                              //Список таймеров

    FDelay: Cardinal;                                               //Задержка между опросами элементов

    procedure ThreadProc;

  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

    function  Add(Proc: TsgeTimeEventProc; Delay: Cardinal = 0; Times: Integer = -1; AutoDelete: Boolean = True; StartDelay: Cardinal = 0; Enable: Boolean = True): TsgeTimeEventItem;
    procedure Delete(Proc: TsgeTimeEventProc);

    property TimeEventList: TsgeTimeEventList read FTimeEventList;

    property Delay: Cardinal read FDelay write FDelay;
  end;



implementation

uses
  sgeErrors, sgeOSPlatform;

const
  _UNITNAME = 'ExtensionTimeEvent';



procedure TsgeExtensionTimeEvent.ThreadProc;
var
  Idx: Integer;
  El: TsgeTimeEventItem;
begin
  //Заблокировать список
  FTimeEventList.Lock;


  //Цикл по элементам
  Idx := 0;
  while Idx < FTimeEventList.Count do
    begin
    //Проверка на разрушение объекта
    if FDestroying then Break;

    //Ссылка на элемент
    El := FTimeEventList.Item[Idx];

    //Проверить на удаление
    if El.NeedDelete then
      begin
      FTimeEventList.Delete(Idx);
      Continue;
      end;

    //Проверить на активность
    if not El.Enable then Continue;

    //Проверить, прошло время для выполнения или нет
    if El.IsTimePassed then
      begin
      //Увеличить счётчик срабатываний
      El.IncTimes;

      //Событие срабатывания таймера
      EventManager.Publish(Event_TimeEvent, TsgeEventTimeEvent.Create(El.Proc));

      //Проверить на предел по количеству выполнений
      if (El.Times <> -1) and (El.TimesCount >= El.Times) then
        begin
        //Остановить выполнение
        El.Enable := False;

        //Проверить на автоудаление
        if El.AutoDelete then El.Delete;
        end;
      end;

    Inc(Idx);
    end;


  //Разблокировать список
  FTimeEventList.Unlock;

  //Заснуть
  sgeSleep(FDelay);
end;


class function TsgeExtensionTimeEvent.GetName: String;
begin
  Result := Extension_TimeEvent;
end;


constructor TsgeExtensionTimeEvent.Create(ObjectList: TObject);
begin
  try
    inherited Create(ObjectList);

    //Создать объекты
    FThread := TsgeThread.Create(@ThreadProc, True, False);
    FTimeEventList := TsgeTimeEventList.Create;

    //Параметры
    FDelay := 1;

    //Запустить обработку
    FThread.Resume;
  except
    on E: EsgeException do
      raise EsgeException.Create(_UNITNAME, Err_CantCreateExtension, '', E.Message);
  end;
end;


destructor TsgeExtensionTimeEvent.Destroy;
begin
  FDestroying := True;

  FThread.Free;
  FTimeEventList.Free;

  inherited Destroy;
end;


function TsgeExtensionTimeEvent.Add(Proc: TsgeTimeEventProc; Delay: Cardinal; Times: Integer; AutoDelete: Boolean; StartDelay: Cardinal; Enable: Boolean): TsgeTimeEventItem;
begin
  //Создать таймер
  Result := TsgeTimeEventItem.Create(Proc, Delay, Times, AutoDelete, StartDelay, Enable);

  //Добавить в список
  FTimeEventList.Add(Result);
end;


procedure TsgeExtensionTimeEvent.Delete(Proc: TsgeTimeEventProc);
var
  Idx: Integer;
begin
  //Найти индекс таймера
  Idx := FTimeEventList.IndexOf(Proc);

  //Удалить
  if Idx <> -1 then FTimeEventList.Delete(Idx);
end;




end.

