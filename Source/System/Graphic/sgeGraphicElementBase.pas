{
Пакет             Simple Game Engine 2
Файл              sgeGraphicElementBase.pas
Версия            1.1
Создан            09.06.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Класс базового элемента вывода
}
{$Include Defines.inc}

unit sgeGraphicElementBase;

{$mode objfpc}{$H+}

interface

uses
  sgeGraphic;


const
  Err_CantCreateGraphicElement = 'CantCreateGraphicElement';


type
  TsgeGraphicElementBase = class
  private
    FVisible: Boolean;                                              //Флаг видимости
    FNeedUpdate: Boolean;                                           //Флаг обновления данных
    FNeedDelete: Boolean;                                           //Флаг удаления

  protected
    procedure UpdateData; virtual; abstract;                        //Обновление данных
    procedure ApplySettings;                                        //Применение изменений

  public
    constructor Create;

    procedure Update;
    procedure Delete;

    //Пользовательские функции
    procedure Draw(Graphic: TsgeGraphic); virtual; abstract;

    property Visible: Boolean read FVisible write FVisible;
    property NeedUpdate: Boolean read FNeedUpdate;
    property NeedDelete: Boolean read FNeedDelete;
  end;


implementation


procedure TsgeGraphicElementBase.ApplySettings;
begin
  UpdateData;
  FNeedUpdate := False;
end;


constructor TsgeGraphicElementBase.Create;
begin
  FVisible := True;
end;


procedure TsgeGraphicElementBase.Update;
begin
  FNeedUpdate := True;
end;


procedure TsgeGraphicElementBase.Delete;
begin
  FNeedDelete := True;
end;



end.

