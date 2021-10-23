{
Пакет             Simple Game Engine 2
Файл              sgeExtensionTimeEvent.pas
Версия            1.3
Создан            29.08.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс расширения: Таймерные события
}
{$Include Defines.inc}

unit sgeExtensionTimeEvent;

{$mode objfpc}{$H+}

interface

uses
  sgeExtensionBase, sgeThread,
  sgeTimeEventList, sgeTimeEventItem, sgeEventTimeEvent;


const
  Extension_TimeEvent = 'TimeEvent';


type
  TsgeExtensionTimeEvent = class(TsgeExtensionBase)
  private
    FThread: TsgeThread;
    FTimeEventList: TsgeTimeEventList;                              //Список таймеров

    FDelay: Cardinal;                                               //Задержка между опросами элементов

    //Функции потока
    procedure ThreadProc;                                           //Функция опроса таймеров

    //Вспомогательные функции
    procedure ClearEventList;                                       //Освободить память объектов
  protected
    class function GetName: String; override;

  public
    constructor Create(ObjectList: TObject); override;
    destructor  Destroy; override;

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
  try
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

      //Проверить на активность
      if not El.Enable then
        begin
        Inc(Idx);
        Continue;
        end;

      //Проверить, прошло время для выполнения или нет
      if El.IsTimePassed then
        begin
        //Увеличить счётчик срабатываний
        El.IncTimes;

        //Событие срабатывания таймера
        EventManager.Publish(TsgeEventTimeEvent.Create(Event_TimeEvent, El.Proc));

        //Проверить на предел по количеству выполнений
        if (El.Times <> -1) and (El.TimesCount >= El.Times) then
          begin
          //Остановить выполнение
          El.Enable := False;

          //Проверить на автоудаление
          if El.AutoDelete then
            begin
            El.Free;
            Dec(Idx);
            end;
          end;

        end;

      Inc(Idx);
      end;



  finally
    //Разблокировать список
    FTimeEventList.Unlock;
  end;

  //Заснуть
  sgeSleep(FDelay);
end;


procedure TsgeExtensionTimeEvent.ClearEventList;
begin
  FTimeEventList.Lock;
  try

    if FTimeEventList.Count = 0 then Exit;

    //Удалить объекты
    while FTimeEventList.Count > 0 do
      FTimeEventList.Item[0].Free;

  finally
    FTimeEventList.Unlock;
  end;
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
    FTimeEventList := TsgeTimeEventList.Create(False);

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
  //Флаг уничтожения
  FDestroying := True;

  //Убить поток
  FThread.Free;

  //Очистить список таймеров
  ClearEventList;

  //Удалить список
  FTimeEventList.Free;

  inherited Destroy;
end;



end.

